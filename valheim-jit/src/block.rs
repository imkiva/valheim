use valheim_asm::isa::rv32::RV32Instr;
use valheim_asm::isa::rv64::RV64Instr;
use valheim_asm::isa::typed::Instr;
use valheim_core::cpu::irq::Exception;
use valheim_core::cpu::mmu::PAGE_SIZE;
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
  InterpretOne(FallbackKind),
  Fault {
    raw: Option<u32>,
    exception: Exception,
  },
}

impl GuestBlock {
  pub fn translate(cpu: &mut RV64Cpu, max_len: usize) -> BlockBuild {
    let start_pc = cpu.read_pc().0;
    let code_page = start_pc & !(PAGE_SIZE - 1);
    let mut pc = start_pc;
    let mut instructions = Vec::with_capacity(max_len.min(MAX_BLOCK_LEN));

    while instructions.len() < max_len.min(MAX_BLOCK_LEN) {
      if (pc & !(PAGE_SIZE - 1)) != code_page || (pc & (PAGE_SIZE - 1)) > PAGE_SIZE - 4 {
        return if instructions.is_empty() {
          BlockBuild::InterpretOne(FallbackKind::Other)
        } else {
          BlockBuild::Block(GuestBlock {
            start_pc,
            instructions,
          })
        };
      }

      let raw = match cpu.fetch_mem(VirtAddr(pc)) {
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

      if !is_baseline_native(decoded) {
        return if instructions.is_empty() {
          BlockBuild::InterpretOne(FallbackKind::from_raw(raw, len))
        } else {
          BlockBuild::Block(GuestBlock {
            start_pc,
            instructions,
          })
        };
      }

      instructions.push(GuestInst {
        pc,
        raw,
        len,
        decoded,
      });
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
  matches!(
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

#[cfg(test)]
mod tests {
  use super::*;
  use valheim_core::cpu::bus::RV64_MEMORY_BASE;

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
  fn unsupported_first_instruction_uses_single_step_fallback() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.write_pc(pc);
    cpu.bus.write::<u32>(pc, 0x0000_0073).unwrap(); // ecall

    assert_eq!(
      GuestBlock::translate(&mut cpu, MAX_BLOCK_LEN),
      BlockBuild::InterpretOne(FallbackKind::System)
    );
  }
}
