use crate::cpu::regs::Regs;
use crate::isa::typed::{Instr, Reg};
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Journal {
  pub init_regs: Regs,
  pub init_mem_base: VirtAddr,
  pub init_mem_size: usize,
  pub trace: Vec<Trace>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Trace {
  Reg(RegTrace),
  Mem(MemTrace),
  Instr(InstrTrace),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstrTrace {
  Fetched(Bytecode),
  Decoded(Bytecode, Instr),
  Executed(Instr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegTrace {
  Read(Reg, u64),
  Write(Reg, u64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemTrace {
  Read(VirtAddr, usize, u64),
  Write(VirtAddr, usize, u64),
}

impl Journal {
  #[inline(always)]
  pub fn trace<F: Fn() -> Trace>(&mut self, f: F) {
    #[cfg(feature = "trace")]
    self.trace.push(f());
  }
}
