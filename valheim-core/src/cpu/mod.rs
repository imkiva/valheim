use std::cell::RefCell;
use std::fmt::Debug;

use valheim_asm::isa::typed::Reg;

use crate::cpu::bus::{RV64_MEMORY_BASE, RV64_MEMORY_SIZE};
use crate::cpu::mmu::VMMode;
use crate::debug::trace::{Journal, RegTrace, Trace};
use crate::memory::VirtAddr;

pub mod regs;
pub mod execute;
pub mod bus;
pub mod csr;
pub mod data;
pub mod irq;
pub mod mmu;
pub mod test;

#[derive(Debug)]
pub struct RV64Cpu {
  pub regs: regs::Regs,
  pub csrs: csr::CSRRegs,
  pub mode: PrivilegeMode,
  pub bus: bus::Bus,
  /// used by LR/SC
  pub reserved: Vec<VirtAddr>,
  /// used by wfi instruction
  pub wfi: bool,
  /// Virtual memory translation mode
  pub vmmode: VMMode,
  /// physical page number used in virtual memory translation
  pub vmppn: u64,
  /// current cycle instruction
  pub instr: u64,

  // used for debugging
  pub journal: Journal,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Copy, Clone)]
pub enum PrivilegeMode {
  User = 0b00,
  Supervisor = 0b01,
  Machine = 0b11,
}

impl RV64Cpu {
  pub fn new(trace: Option<String>) -> RV64Cpu {
    let regs = regs::Regs::new();
    let csrs = csr::CSRRegs::new();
    RV64Cpu {
      regs,
      csrs,
      mode: PrivilegeMode::Machine,
      bus: bus::Bus::new().expect("Failed to create Bus"),
      reserved: Vec::new(),
      wfi: false,
      vmppn: 0,
      vmmode: VMMode::MBARE,
      journal: Journal {
        init_regs: regs,
        init_mem_base: VirtAddr(RV64_MEMORY_BASE),
        init_mem_size: RV64_MEMORY_SIZE as usize,
        traces: RefCell::new(Vec::with_capacity(1024)),
        max_recent_traces: if trace.is_some() { 1024 } else { 0 },
        trace_file: trace,
      },
      instr: 0,
    }
  }

  #[inline(always)]
  pub fn read_reg(&self, reg: Reg) -> Option<u64> {
    let val = self.regs.read(reg);
    self.journal.trace(|| Trace::Reg(RegTrace::Read(reg, val)));
    val
  }

  #[inline(always)]
  pub fn read_reg_fp(&self, reg: Reg) -> Option<f64> {
    let val = self.regs.read_fp(reg);
    self.journal.trace(|| Trace::Reg(RegTrace::ReadFp(reg, val)));
    val
  }

  #[inline(always)]
  pub fn write_reg(&mut self, reg: Reg, val: u64) -> Option<()> {
    let res = self.regs.write(reg, val);
    self.journal.trace(|| Trace::Reg(RegTrace::Write(reg, val, res.is_some())));
    res
  }

  #[inline(always)]
  pub fn write_reg_fp(&mut self, reg: Reg, val: f64) -> Option<()> {
    let res = self.regs.write_fp(reg, val);
    self.journal.trace(|| Trace::Reg(RegTrace::WriteFp(reg, val, res.is_some())));
    res
  }

  /// internal fast-path
  #[inline(always)]
  pub fn read_pc(&self) -> VirtAddr {
    self.journal.trace(|| Trace::Reg(RegTrace::Read(Reg::PC, Some(self.regs.pc.0))));
    self.regs.pc
  }

  /// internal fast-path
  #[inline(always)]
  pub fn write_pc(&mut self, pc: VirtAddr) {
    self.regs.pc = pc;
    self.journal.trace(|| Trace::Reg(RegTrace::Write(Reg::PC, pc.0, true)));
  }
}
