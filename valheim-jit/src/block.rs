use valheim_asm::isa::rv32::RV32Instr;
use valheim_asm::isa::rv64::RV64Instr;
use valheim_asm::isa::typed::Instr;
use valheim_core::cpu::irq::Exception;
use valheim_core::cpu::mmu::{AccessType, TranslationTarget, PAGE_SIZE};
use valheim_core::cpu::RV64Cpu;
use valheim_core::memory::VirtAddr;

pub const MAX_BLOCK_LEN: usize = 32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GuestInst {
  pub pc: u64,
  pub raw: u32,
  pub len: u8,
  pub decoded: Instr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GuestBlock {
  pub start_pc: u64,
  pub instructions: Vec<GuestInst>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FallbackKind {
  System,
  FloatingPoint,
  Other,
}

impl FallbackKind {
  fn from_raw(raw: u32, len: u8) -> Self {
    if len == 4 {
      return match raw & 0x7f {
        0x73 => Self::System,
        0x0f if (raw >> 12) & 0x7 == 1 => Self::System,
        0x07 | 0x27 | 0x43 | 0x47 | 0x4b | 0x4f | 0x53 => Self::FloatingPoint,
        _ => Self::Other,
      };
    }

    let quadrant = raw & 0x3;
    let funct3 = (raw >> 13) & 0x7;
    if matches!((quadrant, funct3), (0, 1 | 5) | (2, 1 | 5)) {
      Self::FloatingPoint
    } else {
      Self::Other
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlockBuild {
  Block(GuestBlock),
  InterpretOne {
    kind: FallbackKind,
    inst: GuestInst,
  },
  Fault {
    raw: Option<u32>,
    exception: Exception,
  },
}

#[inline(always)]
fn fetch_from_host_page(host_page: *const u8, page_offset: usize) -> u32 {
  debug_assert!(page_offset <= PAGE_SIZE as usize - std::mem::size_of::<u32>());
  // `translate_to_host` only supplies this pointer for a complete DRAM page, and the bound above
  // keeps both 16-bit parcels inside that page. The backing Memory allocation is stable for the
  // CPU's lifetime.
  let low = unsafe {
    u16::from_le(host_page.add(page_offset).cast::<u16>().read_unaligned())
  };
  if low & 0b11 != 0b11 {
    return low as u32;
  }
  let high = unsafe {
    u16::from_le(
      host_page
        .add(page_offset + std::mem::size_of::<u16>())
        .cast::<u16>()
        .read_unaligned(),
    )
  };
  (low as u32) | ((high as u32) << 16)
}

#[inline(always)]
fn fetch_for_block(
  cpu: &mut RV64Cpu,
  pc: VirtAddr,
  cached_host_page: &mut Option<*const u8>,
) -> Result<u32, Exception> {
  let page_offset = (pc.0 & (PAGE_SIZE - 1)) as usize;
  if pc.0 & 1 != 0 || page_offset > PAGE_SIZE as usize - std::mem::size_of::<u32>() {
    return cpu.fetch_mem(pc);
  }

  if let Some(host_page) = *cached_host_page {
    return Ok(fetch_from_host_page(host_page, page_offset));
  }

  match cpu.translate_to_host(pc, AccessType::Fetch)? {
    TranslationTarget::Dram {
      paddr,
      host_page,
      phys_page,
      ..
    } => {
      debug_assert_eq!(paddr.0.wrapping_sub(phys_page), page_offset as u64);
      let host_page = host_page.cast_const();
      *cached_host_page = Some(host_page);
      Ok(fetch_from_host_page(host_page, page_offset))
    }
    // Device-backed code is rare and may have read side effects. Keep it on the common fetch path
    // instead of caching or duplicating MMIO endpoint semantics here.
    TranslationTarget::Mmio { .. } => cpu.fetch_mem(pc),
  }
}

impl GuestBlock {
  pub fn translate(cpu: &mut RV64Cpu, max_len: usize) -> BlockBuild {
    let start_pc = cpu.read_pc().0;
    let code_page = start_pc & !(PAGE_SIZE - 1);
    let mut pc = start_pc;
    let mut instructions = Vec::with_capacity(max_len.min(MAX_BLOCK_LEN));
    let mut cached_host_page = None;

    while instructions.len() < max_len.min(MAX_BLOCK_LEN) {
      let crossed_page = (pc & !(PAGE_SIZE - 1)) != code_page;
      let crosses_page_if_uncompressed = (pc & (PAGE_SIZE - 1)) > PAGE_SIZE - 4;
      if crossed_page || (crosses_page_if_uncompressed && !instructions.is_empty()) {
        return BlockBuild::Block(GuestBlock {
          start_pc,
          instructions,
        });
      }

      let raw = match fetch_for_block(cpu, VirtAddr(pc), &mut cached_host_page) {
        Ok(raw) => raw,
        Err(exception) if instructions.is_empty() => {
          return BlockBuild::Fault {
            raw: None,
            exception,
          };
        }
        Err(_) => break,
      };
      let (decoded, len) = match Instr::decode16(raw as u16) {
        Some(decoded) => (decoded, 2),
        None => match Instr::decode32(raw) {
          Some(decoded) => (decoded, 4),
          None if instructions.is_empty() => {
            return BlockBuild::Fault {
              raw: Some(raw),
              exception: Exception::IllegalInstruction,
            };
          }
          None => break,
        },
      };
      let inst = GuestInst {
        pc,
        raw,
        len,
        decoded,
      };

      if crosses_page_if_uncompressed || !is_baseline_native(decoded) {
        return if instructions.is_empty() {
          BlockBuild::InterpretOne {
            kind: FallbackKind::from_raw(raw, len),
            inst,
          }
        } else {
          BlockBuild::Block(GuestBlock {
            start_pc,
            instructions,
          })
        };
      }

      instructions.push(inst);
      if is_terminator(decoded) {
        break;
      }
      pc = pc.wrapping_add(len as u64);
    }

    BlockBuild::Block(GuestBlock {
      start_pc,
      instructions,
    })
  }
}

pub fn is_baseline_native(instr: Instr) -> bool {
  match instr {
    Instr::NOP => true,
    Instr::RV32(
      RV32Instr::LUI(..)
      | RV32Instr::AUIPC(..)
      | RV32Instr::JAL(..)
      | RV32Instr::JALR(..)
      | RV32Instr::BEQ(..)
      | RV32Instr::BNE(..)
      | RV32Instr::BLT(..)
      | RV32Instr::BGE(..)
      | RV32Instr::BLTU(..)
      | RV32Instr::BGEU(..)
      | RV32Instr::ADDI(..)
      | RV32Instr::SLTI(..)
      | RV32Instr::SLTIU(..)
      | RV32Instr::XORI(..)
      | RV32Instr::ORI(..)
      | RV32Instr::ANDI(..)
      | RV32Instr::SLLI(..)
      | RV32Instr::SRLI(..)
      | RV32Instr::SRAI(..)
      | RV32Instr::ADD(..)
      | RV32Instr::SUB(..)
      | RV32Instr::SLL(..)
      | RV32Instr::SLT(..)
      | RV32Instr::SLTU(..)
      | RV32Instr::XOR(..)
      | RV32Instr::SRL(..)
      | RV32Instr::SRA(..)
      | RV32Instr::OR(..)
      | RV32Instr::AND(..)
      | RV32Instr::FENCE(..)
      | RV32Instr::FENCE_TSO
      | RV32Instr::LB(..)
      | RV32Instr::LH(..)
      | RV32Instr::LW(..)
      | RV32Instr::LBU(..)
      | RV32Instr::LHU(..)
      | RV32Instr::SB(..)
      | RV32Instr::SH(..)
      | RV32Instr::SW(..)
      | RV32Instr::MUL(..)
      | RV32Instr::MULH(..)
      | RV32Instr::MULHSU(..)
      | RV32Instr::MULHU(..)
      | RV32Instr::DIV(..)
      | RV32Instr::DIVU(..)
      | RV32Instr::REM(..)
      | RV32Instr::REMU(..)
      | RV32Instr::LR_W(..)
      | RV32Instr::SC_W(..)
      | RV32Instr::AMOSWAP_W(..)
      | RV32Instr::AMOADD_W(..)
      | RV32Instr::AMOXOR_W(..)
      | RV32Instr::AMOAND_W(..)
      | RV32Instr::AMOOR_W(..)
      | RV32Instr::AMOMIN_W(..)
      | RV32Instr::AMOMAX_W(..)
      | RV32Instr::AMOMINU_W(..)
      | RV32Instr::AMOMAXU_W(..),
    ) => true,
    Instr::RV64(
      RV64Instr::SLLI(..)
      | RV64Instr::SRLI(..)
      | RV64Instr::SRAI(..)
      | RV64Instr::ADDIW(..)
      | RV64Instr::SLLIW(..)
      | RV64Instr::SRLIW(..)
      | RV64Instr::SRAIW(..)
      | RV64Instr::ADDW(..)
      | RV64Instr::SUBW(..)
      | RV64Instr::SLLW(..)
      | RV64Instr::SRLW(..)
      | RV64Instr::SRAW(..)
      | RV64Instr::LWU(..)
      | RV64Instr::LD(..)
      | RV64Instr::SD(..)
      | RV64Instr::MULW(..)
      | RV64Instr::DIVW(..)
      | RV64Instr::DIVUW(..)
      | RV64Instr::REMW(..)
      | RV64Instr::REMUW(..)
      | RV64Instr::LR_D(..)
      | RV64Instr::SC_D(..)
      | RV64Instr::AMOSWAP_D(..)
      | RV64Instr::AMOADD_D(..)
      | RV64Instr::AMOXOR_D(..)
      | RV64Instr::AMOAND_D(..)
      | RV64Instr::AMOOR_D(..)
      | RV64Instr::AMOMIN_D(..)
      | RV64Instr::AMOMAX_D(..)
      | RV64Instr::AMOMINU_D(..)
      | RV64Instr::AMOMAXU_D(..),
    ) => true,
    _ => false,
  }
}

pub fn is_terminator(instr: Instr) -> bool {
  is_atomic(instr)
    || matches!(
    instr,
    Instr::RV32(
      RV32Instr::JAL(..)
        | RV32Instr::JALR(..)
        | RV32Instr::BEQ(..)
        | RV32Instr::BNE(..)
        | RV32Instr::BLT(..)
        | RV32Instr::BGE(..)
        | RV32Instr::BLTU(..)
        | RV32Instr::BGEU(..)
    )
    )
}

pub fn is_atomic(instr: Instr) -> bool {
  matches!(
    instr,
    Instr::RV32(
      RV32Instr::LR_W(..)
        | RV32Instr::SC_W(..)
        | RV32Instr::AMOSWAP_W(..)
        | RV32Instr::AMOADD_W(..)
        | RV32Instr::AMOXOR_W(..)
        | RV32Instr::AMOAND_W(..)
        | RV32Instr::AMOOR_W(..)
        | RV32Instr::AMOMIN_W(..)
        | RV32Instr::AMOMAX_W(..)
        | RV32Instr::AMOMINU_W(..)
        | RV32Instr::AMOMAXU_W(..)
    ) | Instr::RV64(
      RV64Instr::LR_D(..)
        | RV64Instr::SC_D(..)
        | RV64Instr::AMOSWAP_D(..)
        | RV64Instr::AMOADD_D(..)
        | RV64Instr::AMOXOR_D(..)
        | RV64Instr::AMOAND_D(..)
        | RV64Instr::AMOOR_D(..)
        | RV64Instr::AMOMIN_D(..)
        | RV64Instr::AMOMAX_D(..)
        | RV64Instr::AMOMINU_D(..)
        | RV64Instr::AMOMAXU_D(..)
    )
  )
}

#[cfg(test)]
mod tests {
  use super::*;
  use valheim_core::cpu::bus::{RV64_MEMORY_BASE, RV64_MEMORY_END, VIRT_MROM_BASE};
  use valheim_core::cpu::csr::CSRMap::SATP;
  use valheim_core::cpu::mmu::{
    PAGE_SHIFT, PTE_A, PTE_R, PTE_V, PTE_X, SATP64_MODE_SHIFT, VMMode,
  };
  use valheim_core::cpu::PrivilegeMode;

  const SV39_ROOT_PAGE: u64 = RV64_MEMORY_BASE + 0x1000;
  const SV39_LEVEL1_PAGE: u64 = RV64_MEMORY_BASE + 0x2000;
  const SV39_LEVEL0_PAGE: u64 = RV64_MEMORY_BASE + 0x3000;

  fn install_sv39_mapping(
    cpu: &mut RV64Cpu,
    vaddr: VirtAddr,
    paddr: VirtAddr,
    flags: u64,
  ) -> VirtAddr {
    let vpn0 = (vaddr.0 >> 12) & 0x1ff;
    let vpn1 = (vaddr.0 >> 21) & 0x1ff;
    let vpn2 = (vaddr.0 >> 30) & 0x1ff;
    let pointer = |page: u64| ((page >> PAGE_SHIFT) << 10) | (1 << PTE_V);

    cpu
      .bus
      .write::<u64>(VirtAddr(SV39_ROOT_PAGE + vpn2 * 8), pointer(SV39_LEVEL1_PAGE))
      .unwrap();
    cpu
      .bus
      .write::<u64>(VirtAddr(SV39_LEVEL1_PAGE + vpn1 * 8), pointer(SV39_LEVEL0_PAGE))
      .unwrap();
    let leaf = VirtAddr(SV39_LEVEL0_PAGE + vpn0 * 8);
    cpu
      .bus
      .write::<u64>(leaf, ((paddr.0 >> PAGE_SHIFT) << 10) | flags)
      .unwrap();
    cpu.mode = PrivilegeMode::Supervisor;
    cpu
      .csrs
      .write_unchecked(
        SATP,
        ((VMMode::SV39 as u64) << SATP64_MODE_SHIFT) | (SV39_ROOT_PAGE >> PAGE_SHIFT),
      )
      .unwrap();
    cpu.sync_pagetable();
    leaf
  }

  fn install_adjacent_sv39_leaf(
    cpu: &mut RV64Cpu,
    first_leaf: VirtAddr,
    paddr: VirtAddr,
    flags: u64,
  ) -> VirtAddr {
    let leaf = first_leaf + VirtAddr(std::mem::size_of::<u64>() as u64);
    cpu
      .bus
      .write::<u64>(leaf, ((paddr.0 >> PAGE_SHIFT) << 10) | flags)
      .unwrap();
    leaf
  }

  #[test]
  fn translates_a_straight_line_ending_in_a_branch() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.write_pc(pc);
    cpu.bus.write::<u32>(pc, 0x0010_0093).unwrap(); // addi x1, x0, 1
    cpu.bus.write::<u32>(pc + VirtAddr(4), 0x0000_0063).unwrap(); // beq x0, x0, 0

    let BlockBuild::Block(block) = GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN) else {
      panic!("expected a translated block");
    };
    assert_eq!(block.instructions.len(), 2);
    assert_eq!(block.instructions[0].len, 4);
    assert!(is_terminator(block.instructions[1].decoded));
  }

  #[test]
  fn retains_compressed_instruction_length() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.write_pc(pc);
    cpu.bus.write::<u32>(pc, 0x0010_0001).unwrap(); // c.nop; c.addi x0, 4

    let BlockBuild::Block(block) = GuestBlock::translate(&mut cpu, 2) else {
      panic!("expected a translated block");
    };
    assert_eq!(block.instructions[0].len, 2);
    assert_eq!(block.instructions[0].pc, RV64_MEMORY_BASE);
    assert_eq!(block.instructions[1].pc, RV64_MEMORY_BASE + 2);
  }

  #[test]
  fn cached_host_page_fetch_is_unaligned_and_little_endian() {
    let mut bytes = [0xff_u8; 8];
    bytes[1..3].copy_from_slice(&0x0001_u16.to_le_bytes());
    assert_eq!(fetch_from_host_page(bytes[1..].as_ptr(), 0), 0x0001);

    bytes[1..5].copy_from_slice(&0x0010_0093_u32.to_le_bytes());
    assert_eq!(fetch_from_host_page(bytes[1..].as_ptr(), 0), 0x0010_0093);
  }

  #[test]
  fn unsupported_first_instruction_uses_single_step_fallback() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.write_pc(pc);
    cpu.bus.write::<u32>(pc, 0x0000_0073).unwrap(); // ecall

    assert_eq!(
      GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN),
      BlockBuild::InterpretOne {
        kind: FallbackKind::System,
        inst: GuestInst {
          pc: pc.0,
          raw: 0x0000_0073,
          len: 4,
          decoded: Instr::RV32(RV32Instr::ECALL),
        },
      }
    );
  }

  #[test]
  fn page_tail_fallback_caches_only_a_successfully_fetched_instruction() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_END - 2);
    cpu.write_pc(pc);
    cpu.bus.write::<u16>(pc, 0x0001).unwrap(); // c.nop

    let BlockBuild::InterpretOne { kind, inst } =
      GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN)
    else {
      panic!("expected a page-tail fallback");
    };
    assert_eq!(kind, FallbackKind::Other);
    assert_eq!(inst.pc, pc.0);
    assert_eq!(inst.raw, 0x0001);
    assert_eq!(inst.len, 2);
    assert_eq!(inst.decoded, Instr::NOP);

    cpu.bus.write::<u16>(pc, 0x0073).unwrap(); // low parcel of ecall
    assert_eq!(
      GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN),
      BlockBuild::Fault {
        raw: None,
        exception: Exception::InstructionAccessFault(VirtAddr(RV64_MEMORY_END)),
      }
    );
  }

  #[test]
  fn translated_dram_page_fetches_a_block_and_updates_accessed_bit() {
    let mut cpu = RV64Cpu::new(None);
    let virtual_pc = VirtAddr(0x4000_0100);
    let physical_pc = VirtAddr(RV64_MEMORY_BASE + 0x8100);
    cpu.bus.write::<u32>(physical_pc, 0x0010_0093).unwrap(); // addi x1, x0, 1
    cpu
      .bus
      .write::<u32>(physical_pc + VirtAddr(4), 0x0000_0063)
      .unwrap(); // beq x0, x0, 0
    let flags = (1 << PTE_V) | (1 << PTE_X);
    let leaf = install_sv39_mapping(
      &mut cpu,
      virtual_pc,
      VirtAddr(RV64_MEMORY_BASE + 0x8000),
      flags,
    );
    cpu.write_pc(virtual_pc);

    let BlockBuild::Block(block) = GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN) else {
      panic!("expected an Sv39 translated block");
    };
    assert_eq!(block.instructions.len(), 2);
    assert_eq!(block.instructions[0].raw, 0x0010_0093);
    assert_eq!(block.instructions[1].raw, 0x0000_0063);
    assert_ne!(cpu.bus.read::<u64>(leaf).unwrap() & (1 << PTE_A), 0);
  }

  #[test]
  fn translated_fetch_faults_keep_the_original_guest_address() {
    let mut cpu = RV64Cpu::new(None);
    let virtual_pc = VirtAddr(0x4000_0100);
    let flags = (1 << PTE_V) | (1 << PTE_R);
    let leaf = install_sv39_mapping(
      &mut cpu,
      virtual_pc,
      VirtAddr(RV64_MEMORY_BASE + 0x8000),
      flags,
    );
    cpu.write_pc(virtual_pc);
    assert_eq!(
      GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN),
      BlockBuild::Fault {
        raw: None,
        exception: Exception::InstructionPageFault(virtual_pc),
      }
    );
    assert_eq!(cpu.bus.read::<u64>(leaf).unwrap() & (1 << PTE_A), 0);

    let mut cpu = RV64Cpu::new(None);
    let flags = (1 << PTE_V) | (1 << PTE_X);
    install_sv39_mapping(
      &mut cpu,
      virtual_pc,
      VirtAddr(RV64_MEMORY_END + PAGE_SIZE),
      flags,
    );
    cpu.write_pc(virtual_pc);
    assert_eq!(
      GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN),
      BlockBuild::Fault {
        raw: None,
        exception: Exception::InstructionAccessFault(virtual_pc),
      }
    );
  }

  #[test]
  fn device_backed_fetch_stays_on_the_common_bus_path() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(VIRT_MROM_BASE);
    cpu.bus.device_tree.write::<u32>(pc, 0x0000_006f).unwrap(); // jal x0, 0
    cpu.write_pc(pc);

    let BlockBuild::Block(block) = GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN) else {
      panic!("expected a block fetched through MROM");
    };
    assert_eq!(block.instructions.len(), 1);
    assert_eq!(block.instructions[0].raw, 0x0000_006f);
  }

  #[test]
  fn page_tail_fallback_translates_the_second_parcel_separately() {
    let mut cpu = RV64Cpu::new(None);
    let virtual_pc = VirtAddr(0x4000_0ffe);
    let first_physical_page = VirtAddr(RV64_MEMORY_BASE + 0x10_000);
    let second_physical_page = VirtAddr(RV64_MEMORY_BASE + 0x20_000);
    cpu
      .bus
      .write::<u16>(first_physical_page + VirtAddr(PAGE_SIZE - 2), 0x0073)
      .unwrap();
    cpu
      .bus
      .write::<u16>(second_physical_page, 0x0000)
      .unwrap();
    let executable = (1 << PTE_V) | (1 << PTE_X);
    let first_leaf = install_sv39_mapping(
      &mut cpu,
      virtual_pc,
      first_physical_page,
      executable,
    );
    let second_leaf =
      install_adjacent_sv39_leaf(&mut cpu, first_leaf, second_physical_page, executable);
    cpu.write_pc(virtual_pc);

    let BlockBuild::InterpretOne { kind, inst } =
      GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN)
    else {
      panic!("expected a cross-page fallback");
    };
    assert_eq!(kind, FallbackKind::System);
    assert_eq!(inst.pc, virtual_pc.0);
    assert_eq!(inst.raw, 0x0000_0073);
    assert_eq!(inst.len, 4);

    cpu
      .bus
      .write::<u64>(
        second_leaf,
        ((second_physical_page.0 >> PAGE_SHIFT) << 10) | (1 << PTE_V) | (1 << PTE_R),
      )
      .unwrap();
    assert_eq!(
      GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN),
      BlockBuild::Fault {
        raw: None,
        exception: Exception::InstructionPageFault(virtual_pc + VirtAddr(2)),
      }
    );
  }
}
