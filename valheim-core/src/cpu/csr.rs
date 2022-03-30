use crate::cpu::csr::CSRMap::{MEIP_MASK, MIE, MIP, MISA, MSIP_MASK, MSTATUS, MTIP_MASK, SEIP_MASK, SSIP_MASK, SSTATUS, SSTATUS_MASK, STIP_MASK};
use crate::cpu::irq::Exception;
use crate::cpu::mmu::{SATP64_MODE_MASK, SATP64_MODE_SHIFT, VM_V20211203_SV64};
use crate::cpu::PrivilegeMode;
use crate::isa::rv64::CSRAddr;

pub const MXLEN: usize = 64;
pub const CSR_MAX: usize = 1 << 12;

pub const VALHEIM_MISA: u64 = (2 << 62) // MXL[1:0]=2 (XLEN is 64)
  | (1 << 20) // Extensions[20] (User mode implemented)
  | (1 << 18) // Extensions[18] (Supervisor mode implemented)
  | (1 << 8)  // Extensions[8]  (I Base Instruction Set)
  | (1 << 12) // Extensions[12] (M extension)
  | (1 << 0)  // Extensions[0]  (A extension)
  | (1 << 5)  // Extensions[5]  (F extension)
  | (1 << 3)  // Extensions[3]  (D extension)
  | (1 << 2)  // Extensions[2]  (C extension)
;

// WARL (Write Any Read Legal) restrictions on CSRs
// https://github.com/qemu/qemu/blob/master/target/riscv/csr.c
const M_MODE_INTERRUPTS: u64 = MSIP_MASK | MTIP_MASK | MEIP_MASK;
const S_MODE_INTERRUPTS: u64 = SSIP_MASK | STIP_MASK | SEIP_MASK;
const DELEGABLE_INTERRUPTS: u64 = S_MODE_INTERRUPTS;

#[allow(non_snake_case)]
#[allow(non_upper_case_globals)]
pub mod CSRMap {
  // Unprivileged CSR addresses

  // User floating-point CSRs.
  /// Flating-point accrued exceptions.
  pub const FFLAGS: u16 = 0x001;
  /// Floating-point dynamic rounding mode.
  pub const FRB: u16 = 0x002;
  /// Floating-point control and status register (frm + fflags).
  pub const FCSR: u16 = 0x003;

  // User Counter/Timers.
  /// Cycle counter for RDCYCLE instruction.
  pub const CYCLE: u16 = 0xc00;
  /// Timer for RDTIME instruction.
  pub const TIME: u16 = 0xc01;

  // Floating-point divide-by-zero
  pub const FCSR_DZ_MASK: u64 = 1 << 3;

  // Supervisor-level CSR addresses

  // Supervisor trap setup.
  /// Supervisor status register.
  pub const SSTATUS: u16 = 0x100;
  /// Supervisor interrupt-enable register.
  pub const SIE: u16 = 0x104;
  /// Supervisor trap handler base address.
  pub const STVEC: u16 = 0x105;
  /// Supervisor counter enable
  pub const SCOUNTEREN: u16 = 0x106;

  // Supervisor configuration.
  /// Supervisor environment configuration register.
  pub const SENVCFG: u16 = 0x10a;

  // Supervisor trap handling.
  /// Scratch register for supervisor trap handlers.
  pub const SSCRATCH: u16 = 0x140;
  /// Supervisor exception program counter.
  pub const SEPC: u16 = 0x141;
  /// Supervisor trap cause.
  pub const SCAUSE: u16 = 0x142;
  /// Supervisor bad address or instruction.
  pub const STVAL: u16 = 0x143;
  /// Supervisor interrupt pending.
  pub const SIP: u16 = 0x144;

  // Supervisor protection and translation.

  /// Supervisor address translation and protection.
  pub const SATP: u16 = 0x180;

  // Debug/Trace registers
  pub const SCONTEXT: u16 = 0x5a8;

  // SSTATUS fields.
  pub const SSTATUS_SIE: u64 = 0x2;
  // sstatus[1]
  pub const SSTATUS_SPIE: u64 = 0x20;
  // sstatus[5]
  pub const SSTATUS_UBE: u64 = 0x40;
  // sstatus[6]
  pub const SSTATUS_SPP: u64 = 0x100;
  // sstatus[8]
  pub const SSTATUS_FS: u64 = 0x6000;
  // sstatus[14:13]
  pub const SSTATUS_XS: u64 = 0x18000;
  // sstatus[16:15]
  pub const SSTATUS_SUM: u64 = 0x40000;
  // sstatus[18]
  pub const SSTATUS_MXR: u64 = 0x80000;
  // sstatus[19]
  pub const SSTATUS_UXL: u64 = 0x3_00000000;
  // sstatus[33:32]
  pub const SSTATUS_SD: u64 = 0x80000000_00000000;
  // sstatus[63]
  pub const SSTATUS_MASK: u64 = SSTATUS_SIE
    | SSTATUS_SPIE
    | SSTATUS_UBE
    | SSTATUS_SPP
    | SSTATUS_FS
    | SSTATUS_XS
    | SSTATUS_SUM
    | SSTATUS_MXR
    | SSTATUS_UXL
    | SSTATUS_SD;

  // Machine-level CSR addresses

  // Machine information registers.

  /// Vendor ID.
  pub const MVENDORID: u16 = 0xf11;
  /// Architecture ID.
  pub const MARCHID: u16 = 0xf12;
  /// Implementation ID.
  pub const MIMPID: u16 = 0xf13;
  /// Hardware thread ID.
  pub const MHARTID: u16 = 0xf14;

  // Machine trap setup.

  /// Machine status register.
  pub const MSTATUS: u16 = 0x300;
  /// ISA and extensions.
  pub const MISA: u16 = 0x301;
  /// Machine exception delegate register.
  pub const MEDELEG: u16 = 0x302;
  /// Machine interrupt delegate register.
  pub const MIDELEG: u16 = 0x303;
  /// Machine interrupt-enable register.
  pub const MIE: u16 = 0x304;
  /// Machine trap-handler base address.
  pub const MTVEC: u16 = 0x305;
  /// Machine counter enable.
  pub const MCOUNTEREN: u16 = 0x306;

  // Machine trap handling.
  /// Scratch register for machine trap handlers.
  pub const MSCRATCH: u16 = 0x340;
  /// Machine exception program counter.
  pub const MEPC: u16 = 0x341;
  /// Machine trap cause.
  pub const MCAUSE: u16 = 0x342;
  /// Machine bad address or instruction.
  pub const MTVAL: u16 = 0x343;
  /// Machine interrupt pending.
  pub const MIP: u16 = 0x344;
  /// Machine trap instruction (transformed)
  pub const MTINST: u16 = 0x34a;
  /// Machine bad guest physical address.
  pub const MTVAL2: u16 = 0x34b;

  // Machine configuration.

  /// Machine environment configuration register.
  pub const MENVCFG: u16 = 0x30a;
  /// Machine secure configuration register.
  pub const MSECCFG: u16 = 0x747;

  // Machine memory protection.

  /// Physical memory protection configuration. includes PMPCFG0, 2, 4, 6, ... 14 for RV64
  pub const PMPCFG0: u16 = 0x3a0;
  /// Physical memory protection address register. from PMPADDR1 to PMPADDR63
  pub const PMPADDR0: u16 = 0x3b0;

  // Machine Counter/Timers

  /// Machine cycle counter
  pub const MCYCLE: u16 = 0xb00;

  // Standard portion of mip
  /// Supervisor software interrupt.
  pub const SSIP_MASK: u64 = 1 << 1;
  /// Machine software interrupt.
  pub const MSIP_MASK: u64 = 1 << 3;
  /// Supervisor timer interrupt.
  pub const STIP_MASK: u64 = 1 << 5;
  /// Machine timer interrupt.
  pub const MTIP_MASK: u64 = 1 << 7;
  /// Supervisor external interrupt.
  pub const SEIP_MASK: u64 = 1 << 9;
  /// Machine external interrupt.
  pub const MEIP_MASK: u64 = 1 << 11;

  // Standard portion of mie
  /// Supervisor software interrupt.
  pub const SSIE_MASK: u64 = 1 << 1;
  /// Machine software interrupt.
  pub const MSIE_MASK: u64 = 1 << 3;
  /// Supervisor timer interrupt.
  pub const STIE_MASK: u64 = 1 << 5;
  /// Machine timer interrupt.
  pub const MTIE_MASK: u64 = 1 << 7;
  /// Supervisor external interrupt.
  pub const SEIE_MASK: u64 = 1 << 9;
  /// Machine external interrupt.
  pub const MEIE_MASK: u64 = 1 << 11;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CSRRegs {
  pub csrs: [u64; CSR_MAX],
}

impl CSRRegs {
  pub fn new() -> Self {
    let mut csrs = [0; CSR_MAX];
    csrs[MISA as usize] = VALHEIM_MISA;
    Self { csrs }
  }

  pub fn read(&self, addr: CSRAddr) -> u64 {
    self.read_unchecked(addr.value())
  }

  pub fn write(&mut self, addr: CSRAddr, val: u64) -> Result<(), Exception> {
    self.write_unchecked(addr.value(), val)
  }

  pub fn read_unchecked(&self, addr: u16) -> u64 {
    use CSRMap::*;
    match addr {
      // 4.1.1 Supervisor Status Register (sstatus)
      // The sstatus register is a subset of the mstatus register.
      // In a straightforward implementation, reading or writing any field in sstatus
      // is equivalent to reading or writing the homonymous field in mstatus.
      SSTATUS => self.csrs[MSTATUS as usize] & SSTATUS_MASK,
      // 4.1.3 Supervisor Interrupt Registers (sip and sie)
      // The sip and sie registers are subsets of the mip and mie registers.
      // Reading any implemented field, or writing any writable field, of sip/sie
      // effects a read or write of the homonymous field of mip/mie.
      // 3.1.9 Machine Interrupt Registers (mip and mie)
      // If an interrupt is delegated to S-mode by setting a bit in the mideleg register,
      // it becomes visible in the sip register and is maskable using the sie register.
      // Otherwise, the corresponding bits in sip and sie are read-only zero.
      SIE => self.csrs[MIE as usize] & self.csrs[MIDELEG as usize],
      SIP => self.csrs[MIP as usize] & self.csrs[MIDELEG as usize],
      addr => self.csrs[addr as usize],
    }
  }

  pub fn write_unchecked(&mut self, addr: u16, val: u64) -> Result<(), Exception> {
    use CSRMap::*;
    match addr {
      MVENDORID => {}
      MARCHID => {}
      MIMPID => {}
      MHARTID => {}
      MIDELEG => {
        // Some interrupts cannot be delegated to S-mode like Machine Timer Interrupt, etc.
        // https://github.com/qemu/qemu/blob/1d60bb4b14601e38ed17384277aa4c30c57925d3/target/riscv/csr.c#L828
        // https://github.com/riscv/riscv-isa-manual/issues/7
        let mask = DELEGABLE_INTERRUPTS;
        let mideleg = self.csrs[MIDELEG as usize];
        let mideleg = (mideleg & !mask) | (val & mask);
        self.csrs[MIDELEG as usize] = mideleg;
      }
      MCYCLE => return Err(Exception::IllegalInstruction),
      // 4.1.1 Supervisor Status Register (sstatus)
      // The sstatus register is a subset of the mstatus register.
      // In a straightforward implementation, reading or writing any field in sstatus
      // is equivalent to reading or writing the homonymous field in mstatus.
      SSTATUS => {
        self.csrs[MSTATUS as usize] =
          (self.csrs[MSTATUS as usize] & !SSTATUS_MASK) | (val & SSTATUS_MASK);
      }
      // 4.1.3 Supervisor Interrupt Registers (sip and sie)
      // The sip and sie registers are subsets of the mip and mie registers.
      // Reading any implemented field, or writing any writable field, of sip/sie
      // effects a read or write of the homonymous field of mip/mie.
      // 3.1.9 Machine Interrupt Registers (mip and mie)
      // If an interrupt is delegated to S-mode by setting a bit in the mideleg register,
      // it becomes visible in the sip register and is maskable using the sie register.
      // Otherwise, the corresponding bits in sip and sie are read-only zero.
      SIE => {
        self.csrs[MIE as usize] = (self.csrs[MIE as usize] & !self.csrs[MIDELEG as usize])
          | (val & self.csrs[MIDELEG as usize]);
      }
      SIP => {
        let mask = SSIP_MASK & self.csrs[MIDELEG as usize];
        self.csrs[MIP as usize] = (self.csrs[MIP as usize] & !mask) | (val & mask);
      }
      SATP => {
        // 4.1.11 Supervisor Address Translation and Protection (satp) Register
        // Implementations are not required to support all MODE settings, and if satp is written
        // with an unsupported MODE, the entire write has no effect; no fields in satp are modified.
        let mode = (val & SATP64_MODE_MASK) >> SATP64_MODE_SHIFT;
        if mode as u8 != VM_V20211203_SV64 {
          // SV64 is not supported because spec does not say anything about it.
          self.csrs[SATP as usize] = val;
        }
      }
      addr => self.csrs[addr as usize] = val,
    }
    Ok(())
  }

  pub fn write_bit(&mut self, addr: u16, bit: usize, val: bool) -> Result<(), Exception> {
    match val {
      true => self.write_unchecked(addr, self.read_unchecked(addr) | 1 << bit),
      false => self.write_unchecked(addr, self.read_unchecked(addr) & !(1 << bit)),
    }
  }

  pub fn read_bit(&self, addr: u16, bit: usize) -> bool {
    (self.read_unchecked(addr) & (1 << bit)) != 0
  }

  #[allow(non_snake_case)]
  pub fn read_mstatus_MPP(&self) -> PrivilegeMode {
    let mpp0 = self.read_bit(MSTATUS, 11);
    let mpp1 = self.read_bit(MSTATUS, 12);
    match (mpp1, mpp0) {
      (false, false) => PrivilegeMode::User,
      (false, true) => PrivilegeMode::Supervisor,
      (true, true) => PrivilegeMode::Machine,
      _ => panic!("invalid privilege mode in MPP (mstatus[11:12]) = {}{}", mpp1 as i32, mpp0 as i32),
    }
  }

  #[allow(non_snake_case)]
  pub fn read_sstatus_SPP(&self) -> PrivilegeMode {
    match self.read_bit(SSTATUS, 8) {
      false => PrivilegeMode::User,
      true => PrivilegeMode::Supervisor,
    }
  }

  #[allow(non_snake_case)]
  pub fn write_mstatus_MPP(&mut self, mode: PrivilegeMode) {
    match mode {
      PrivilegeMode::User => {
        // mstatus[12:11] = 0b00
        let _ = self.write_bit(MSTATUS, 11, false);
        let _ = self.write_bit(MSTATUS, 12, false);
      }
      PrivilegeMode::Supervisor => {
        // mstatus[12:11] = 0b01
        let _ = self.write_bit(MSTATUS, 11, true);
        let _ = self.write_bit(MSTATUS, 12, false);
      }
      PrivilegeMode::Machine => {
        // mstatus[12:11] = 0b11
        let _ = self.write_bit(MSTATUS, 11, true);
        let _ = self.write_bit(MSTATUS, 12, true);
      }
    }
  }

  #[allow(non_snake_case)]
  pub fn write_sstatus_SPP(&mut self, mode: PrivilegeMode) {
    let _ = match mode {
      PrivilegeMode::User => self.write_bit(SSTATUS, 8, false),
      PrivilegeMode::Supervisor => self.write_bit(SSTATUS, 8, true),
      PrivilegeMode::Machine => panic!("SPP cannot be Machine"),
    };
  }

  #[allow(non_snake_case)]
  pub fn read_mstatus_MPRV(&self) -> bool {
    self.read_bit(MSTATUS, 17)
  }

  #[allow(non_snake_case)]
  pub fn write_mstatus_MPRV(&mut self, val: bool) {
    let _ = self.write_bit(MSTATUS, 17, val);
  }

  #[allow(non_snake_case)]
  pub fn read_mstatus_SUM(&self) -> bool {
    self.read_bit(MSTATUS, 18)
  }

  #[allow(non_snake_case)]
  pub fn write_mstatus_SUM(&mut self, val: bool) {
    let _ = self.write_bit(MSTATUS, 18, val);
  }

  #[allow(non_snake_case)]
  pub fn read_mstatus_MXR(&self) -> bool {
    self.read_bit(MSTATUS, 19)
  }

  #[allow(non_snake_case)]
  pub fn write_mstatus_MXR(&mut self, val: bool) {
    let _ = self.write_bit(MSTATUS, 19, val);
  }

  #[allow(non_snake_case)]
  pub fn read_mstatus_MIE(&self) -> bool {
    self.read_bit(MSTATUS, 3)
  }

  #[allow(non_snake_case)]
  pub fn write_mstatus_MIE(&mut self, val: bool) {
    let _ = self.write_bit(MSTATUS, 3, val);
  }

  #[allow(non_snake_case)]
  pub fn read_mstatus_MPIE(&self) -> bool {
    self.read_bit(MSTATUS, 7)
  }

  #[allow(non_snake_case)]
  pub fn write_mstatus_MPIE(&mut self, val: bool) {
    let _ = self.write_bit(MSTATUS, 7, val);
  }

  #[allow(non_snake_case)]
  pub fn read_sstatus_SIE(&self) -> bool {
    self.read_bit(SSTATUS, 1)
  }

  #[allow(non_snake_case)]
  pub fn write_sstatus_SIE(&mut self, val: bool) {
    let _ = self.write_bit(SSTATUS, 1, val);
  }

  #[allow(non_snake_case)]
  pub fn read_sstatus_SPIE(&self) -> bool {
    self.read_bit(SSTATUS, 5)
  }

  #[allow(non_snake_case)]
  pub fn write_sstatus_SPIE(&mut self, val: bool) {
    let _ = self.write_bit(SSTATUS, 5, val);
  }
}
