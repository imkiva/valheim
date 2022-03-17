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

#[allow(non_snake_case)]
#[allow(non_upper_case_globals)]
pub mod CSRMap {
  //////////////////////////////
  // User-level CSR addresses //
  //////////////////////////////
  // User trap setup.
  /// User status register.
  pub const USTATUS: u16 = 0x000;
  /// User trap handler base address.
  pub const UTVEC: u16 = 0x005;

  // User trap handling.
  /// User exception program counter.
  pub const UEPC: u16 = 0x041;
  /// User trap cause.
  pub const UCAUSE: u16 = 0x042;
  /// User bad address or instruction.
  #[allow(dead_code)]
  pub const UTVAL: u16 = 0x043;

  // User floating-point CSRs.
  /// Flating-point accrued exceptions.
  pub const FFLAGS: u16 = 0x001;
  /// Floating-point dynamic rounding mode.
  pub const FRB: u16 = 0x002;
  /// Floating-point control and status register (frm + fflags).
  pub const FCSR: u16 = 0x003;

  // User Counter/Timers.
  /// Timer for RDTIME instruction.
  pub const TIME: u16 = 0xc01;

  /////////////////////////////////////
  // Supervisor-level CSR addresses //
  ////////////////////////////////////
  // Supervisor trap setup.
  /// Supervisor status register.
  pub const SSTATUS: u16 = 0x100;
  /// Supervisor exception delegation register.
  pub const SEDELEG: u16 = 0x102;
  /// Supervisor interrupt delegation register.
  pub const SIDELEG: u16 = 0x103;
  /// Supervisor interrupt-enable register.
  pub const SIE: u16 = 0x104;
  /// Supervisor trap handler base address.
  pub const STVEC: u16 = 0x105;

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

  /////////////////////////////////
  // Machine-level CSR addresses //
  /////////////////////////////////
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
  /// Machine exception delefation register.
  pub const MEDELEG: u16 = 0x302;
  /// Machine interrupt delefation register.
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

  // Machine memory protection.
  /// Physical memory protection configuration.
  pub const PMPCFG0: u16 = 0x3a0;
  /// Physical memory protection address register.
  pub const PMPADDR0: u16 = 0x3b0;

  // MIP fields.
  /// Supervisor software interrupt.
  pub const SSIP_BIT: u64 = 1 << 1;
  /// Machine software interrupt.
  pub const MSIP_BIT: u64 = 1 << 3;
  /// Supervisor timer interrupt.
  pub const STIP_BIT: u64 = 1 << 5;
  /// Machine timer interrupt.
  pub const MTIP_BIT: u64 = 1 << 7;
  /// Supervisor external interrupt.
  pub const SEIP_BIT: u64 = 1 << 9;
  /// Machine external interrupt.
  pub const MEIP_BIT: u64 = 1 << 11;
}

/// The state to contains all the CSRs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CSRRegs {
  pub csrs: [u64; CSR_MAX],
}

impl CSRRegs {
  pub fn new() -> Self {
    let mut csrs = [0; CSR_MAX];
    csrs[CSRMap::MISA as usize] = VALHEIM_MISA;
    Self { csrs }
  }

  pub fn read(&self, addr: CSRAddr) -> u64 {
    self.read_unchecked(addr.value())
  }

  pub fn write(&mut self, addr: CSRAddr, val: u64) {
    self.write_unchecked(addr.value(), val);
  }

  pub fn read_unchecked(&self, addr: u16) -> u64 {
    use CSRMap::*;
    match addr {
      SSTATUS => self.csrs[MSTATUS as usize] & SSTATUS_MASK,
      SIE => self.csrs[MIE as usize] & self.csrs[MIDELEG as usize],
      SIP => self.csrs[MIP as usize] & self.csrs[MIDELEG as usize],
      addr => self.csrs[addr as usize],
    }
  }

  pub fn write_unchecked(&mut self, addr: u16, val: u64) {
    use CSRMap::*;
    match addr {
      MVENDORID => {}
      MARCHID => {}
      MIMPID => {}
      MHARTID => {}
      SSTATUS => {
        self.csrs[MSTATUS as usize] =
          (self.csrs[MSTATUS as usize] & !SSTATUS_MASK) | (val & SSTATUS_MASK);
      }
      SIE => {
        self.csrs[MIE as usize] = (self.csrs[MIE as usize] & !self.csrs[MIDELEG as usize])
          | (val & self.csrs[MIDELEG as usize]);
      }
      SIP => {
        let mask = SSIP_BIT & self.csrs[MIDELEG as usize];
        self.csrs[MIP as usize] = (self.csrs[MIP as usize] & !mask) | (val & mask);
      }
      addr => self.csrs[addr as usize] = val,
    }
  }

  pub fn read_bit(&self, addr: CSRAddr, bit: usize) -> bool {
    self.read_bit_unchecked(addr.value(), bit)
  }

  pub fn write_bit(&mut self, addr: CSRAddr, bit: usize, val: bool) {
    self.write_bit_unchecked(addr.value(), bit, val);
  }

  pub fn read_bit_unchecked(&self, addr: u16, bit: usize) -> bool {
    (self.read_unchecked(addr) & (1 << bit)) != 0
  }

  pub fn write_bit_unchecked(&mut self, addr: u16, bit: usize, val: bool) {
    match val {
      true => self.write_unchecked(addr, self.read_unchecked(addr) | 1 << bit),
      false =>  self.write_unchecked(addr, self.read_unchecked(addr) & !(1 << bit)),
    }
  }
}
