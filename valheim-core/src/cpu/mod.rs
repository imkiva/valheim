use crate::debug::trace::Journal;
use crate::interp::RV64Interpreter;
use crate::memory;
use crate::memory::VirtAddr;

pub mod regs;

const RV64_MEMORY_BASE: u64 = 0x80000000;
const RV64_CPU_RESET_OFFSET: u64 = 0x0;
const VALHEIM_MEMORY_SIZE: usize = 4 * 1024 * 1024 * 1024; // 4GiB

#[derive(Debug)]
pub struct RV64Cpu {
  pub regs: regs::Regs,
  pub mem: memory::Memory,
  pub cpu_reset_pc: VirtAddr,
  pub journal: Journal,
}

impl RV64Cpu {
  pub fn new() -> RV64Cpu {
    let reset_pc = VirtAddr(RV64_MEMORY_BASE + RV64_CPU_RESET_OFFSET);
    let regs = regs::Regs::new(reset_pc);
    RV64Cpu {
      regs,
      mem: memory::Memory::new(RV64_MEMORY_BASE, VALHEIM_MEMORY_SIZE)
        .expect("Failed to create memory"),
      cpu_reset_pc: reset_pc,
      journal: Journal {
        init_regs: regs,
        init_mem_base: VirtAddr(RV64_MEMORY_BASE),
        init_mem_size: VALHEIM_MEMORY_SIZE,
        trace: Vec::with_capacity(32),
      },
    }
  }

  pub fn run(&mut self, int: &dyn RV64Interpreter) {
    int.interp(self)
  }
}
