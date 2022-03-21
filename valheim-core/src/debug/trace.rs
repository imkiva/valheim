use std::cell::RefCell;
use crate::cpu::regs::Regs;
use crate::isa::compressed::untyped::Bytecode16;
use crate::isa::typed::{Instr, Reg};
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;

#[derive(Debug, Clone)]
pub struct Journal {
  pub init_regs: Regs,
  pub init_mem_base: VirtAddr,
  pub init_mem_size: usize,
  pub traces: RefCell<Vec<Trace>>,
  pub max_recent_traces: usize,
  pub trace_file: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Trace {
  Reg(RegTrace),
  Mem(MemTrace),
  Instr(InstrTrace),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstrTrace {
  Fetched(VirtAddr, Bytecode, Bytecode16),
  Decoded(VirtAddr, Bytecode, Instr),
  DecodedCompressed(VirtAddr, Bytecode16, Instr),
  PrepareExecute(VirtAddr, Instr),
  Executed(VirtAddr, Instr),
  ExecutedCompressed(VirtAddr, Instr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RegTrace {
  Read(Reg, Option<u64>),
  ReadFp(Reg, Option<f64>),
  Write(Reg, u64, bool),
  WriteFp(Reg, f64, bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemTrace {
  Read(VirtAddr, VirtAddr, usize, String),
  Write(VirtAddr, VirtAddr, usize, String),
}

impl Journal {
  #[inline(always)]
  pub fn trace<F: Fn() -> Trace>(&self, f: F) {
    #[cfg(feature = "trace")]
    self.write_traces(f());
  }

  #[cfg(feature = "trace")]
  fn write_traces(&self, trace: Trace) {
    if self.max_recent_traces == 0 { return; }
    let mut traces = self.traces.borrow_mut();
    traces.push(trace);
    if traces.len() > self.max_recent_traces {
      self.flush();
    }
  }

  pub fn flush(&self) {
    #[cfg(feature = "trace")] {
      self.trace_file.as_ref()
        .and_then(|filename| std::fs::OpenOptions::new().create(true).write(true).append(true).open(filename).ok())
        .map(|mut file| {
          self.traces.borrow_mut().iter().for_each(|t| {
            let _ = writeln!(file, "{:?}", t);
          });
          let _ = file.flush();
          let _ = file.sync_all();
          std::mem::drop(file);
        });
      self.traces.borrow_mut().clear();
    }
  }
}
