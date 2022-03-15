use std::fmt::Debug;
use std::cell::RefCell;
use crate::debug::trace::{Journal, MemTrace, RegTrace, Trace};
use crate::interp::RV64Interpreter;
use crate::isa::typed::Reg;
use crate::memory::VirtAddr;

pub mod regs;
pub mod execute;
pub mod exception;
pub mod bus;
pub mod csr;
pub mod data;

const RV64_MEMORY_BASE: u64 = 0x80000000;
const RV64_CPU_RESET_OFFSET: u64 = 0x0;
const VALHEIM_MEMORY_SIZE: usize = 4 * 1024 * 1024 * 1024; // 4GiB

#[derive(Debug)]
pub struct RV64Cpu {
  regs: regs::Regs,
  pub bus: bus::Bus,
  pub cpu_reset_pc: VirtAddr,
  pub journal: Journal,
}

impl RV64Cpu {
  pub fn new(trace: Option<String>) -> RV64Cpu {
    let reset_pc = VirtAddr(RV64_MEMORY_BASE + RV64_CPU_RESET_OFFSET);
    let regs = regs::Regs::new(reset_pc);
    RV64Cpu {
      regs,
      bus: bus::Bus::new(RV64_MEMORY_BASE, VALHEIM_MEMORY_SIZE)
        .expect("Failed to create Bus"),
      cpu_reset_pc: reset_pc,
      journal: Journal {
        init_regs: regs,
        init_mem_base: VirtAddr(RV64_MEMORY_BASE),
        init_mem_size: VALHEIM_MEMORY_SIZE,
        traces: RefCell::new(Vec::with_capacity(1024)),
        max_recent_traces: if trace.is_some() { 1024 } else { 0 },
        trace_file: trace,
      },
    }
  }

  #[inline(always)]
  pub fn read_mem<T: Copy + Sized + Debug>(&self, addr: VirtAddr) -> Option<T> {
    let val = self.bus.read(addr);
    self.journal.trace(|| Trace::Mem(MemTrace::Read(addr, std::mem::size_of::<T>(), format!("{:?}", val))));
    val
  }

  #[inline(always)]
  pub fn write_mem<T: Copy + Sized + Debug>(&mut self, addr: VirtAddr, val: T) -> Option<()> {
    let res = self.bus.write(addr, val);
    self.journal.trace(|| Trace::Mem(MemTrace::Write(addr, std::mem::size_of::<T>(), format!("{:?}", val), res.is_some())));
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
  fn read_pc(&self) -> VirtAddr {
    self.journal.trace(|| Trace::Reg(RegTrace::Read(Reg::PC, Some(self.regs.pc.0))));
    self.regs.pc
  }

  /// internal fast-path
  #[inline(always)]
  fn write_pc(&mut self, pc: VirtAddr) {
    self.regs.pc = pc;
    self.journal.trace(|| Trace::Reg(RegTrace::Write(Reg::PC, pc.0, true)));
  }

  pub fn run(&mut self, int: &dyn RV64Interpreter) {
    int.interp(self)
  }

  pub fn reset<T: Sized>(&mut self, mem: &[T]) {
    self.bus.reset_dram(mem);
    self.write_pc(self.cpu_reset_pc);
  }

  pub fn halt(&mut self) {
    self.bus.halt();
    println!("CPU halt with registers: {:?}", self.regs);
  }
}
