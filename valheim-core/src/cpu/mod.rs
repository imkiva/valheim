use crate::interp::Rv64Interpreter;
use crate::memory;
use crate::memory::VirtAddr;

pub mod regs;

const RV64_MEMORY_BASE: u64 = 0x80000000;
const RV64_CPU_RESET_OFFSET: u64 = 0x0;
const VALHEIM_MEMORY_SIZE: usize = 4 * 1024 * 1024 * 1024; // 4GiB

pub struct Rv64Cpu {
  pub regs: regs::Regs,
  pub mem: memory::Memory,
  pub cpu_reset_pc: VirtAddr,
}

impl Rv64Cpu {
  pub fn new() -> Rv64Cpu {
    let reset_pc = VirtAddr(RV64_MEMORY_BASE + RV64_CPU_RESET_OFFSET);
    Rv64Cpu {
      regs: regs::Regs::new(reset_pc),
      mem: memory::Memory::new(RV64_MEMORY_BASE, VALHEIM_MEMORY_SIZE)
        .expect("Failed to create memory"),
      cpu_reset_pc: reset_pc,
    }
  }

  pub fn load_mem(&mut self, program: &[u8]) {
    self.mem.copy_from_slice(program);
  }

  pub fn run(&mut self, int: &mut dyn Rv64Interpreter) {
    int.interp(self)
  }
}
