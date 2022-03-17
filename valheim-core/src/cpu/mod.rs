use std::fmt::Debug;
use std::cell::RefCell;
use crate::cpu::exception::Exception;
use crate::debug::trace::{Journal, MemTrace, RegTrace, Trace};
use crate::isa::typed::Reg;
use crate::memory::{CanIO, VirtAddr};

pub mod regs;
pub mod execute;
pub mod exception;
pub mod bus;
pub mod csr;
pub mod data;
pub mod trap;

#[derive(Debug)]
pub struct RV64Cpu {
  pub regs: regs::Regs,
  pub csrs: csr::CSRRegs,
  pub mode: PrivilegeMode,
  pub bus: bus::Bus,
  /// used by LR/SC
  pub reserved: Vec<VirtAddr>,
  pub journal: Journal,
  pub previous_instr: u64,
}

#[derive(Debug, PartialEq, PartialOrd, Eq, Copy, Clone)]
pub enum PrivilegeMode {
  User = 0b00,
  Supervisor = 0b01,
  Machine = 0b11,
}

impl RV64Cpu {
  pub fn new(memory_size: usize, trace: Option<String>) -> RV64Cpu {
    let regs = regs::Regs::new();
    let csrs = csr::CSRRegs::new();
    RV64Cpu {
      regs,
      csrs,
      mode: PrivilegeMode::Machine,
      bus: bus::Bus::new(memory_size)
        .expect("Failed to create Bus"),
      reserved: Vec::new(),
      journal: Journal {
        init_regs: regs,
        init_mem_base: VirtAddr(0),
        init_mem_size: memory_size,
        traces: RefCell::new(Vec::with_capacity(1024)),
        max_recent_traces: if trace.is_some() { 1024 } else { 0 },
        trace_file: trace,
      },
      previous_instr: 0,
    }
  }

  #[inline(always)]
  pub fn read_mem<T: CanIO + Debug>(&self, addr: VirtAddr) -> Result<T, Exception> {
    let val = self.bus.read(addr);
    self.journal.trace(|| Trace::Mem(MemTrace::Read(addr, std::mem::size_of::<T>(), format!("{:?}", val))));
    val
  }

  #[inline(always)]
  pub fn write_mem<T: CanIO + Debug>(&mut self, addr: VirtAddr, val: T) -> Result<(), Exception> {
    let res = self.bus.write(addr, val);
    self.journal.trace(|| Trace::Mem(MemTrace::Write(addr, std::mem::size_of::<T>(), format!("{:?}", val))));
    res
  }

  #[inline(always)]
  pub fn read_reg(&self, reg: Reg) -> Option<u64> {
    let val = self.regs.read(reg);
    self.journal.trace(|| Trace::Reg(RegTrace::Read(reg, val)));
    val
  }

  #[inline(always)]
  pub fn write_reg(&mut self, reg: Reg, val: u64) -> Option<()> {
    let res = self.regs.write(reg, val);
    self.journal.trace(|| Trace::Reg(RegTrace::Write(reg, val, res.is_some())));
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
