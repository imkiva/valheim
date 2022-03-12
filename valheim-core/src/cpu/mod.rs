use crate::debug::trace::{Journal, Trace};
use crate::interp::RV64Interpreter;
use crate::isa::rv32::RV32Instr;
use crate::isa::typed::{Instr, Reg};
use crate::isa::untyped::Bytecode;
use crate::memory;
use crate::memory::VirtAddr;

pub mod regs;

const RV64_MEMORY_BASE: u64 = 0x80000000;
const RV64_CPU_RESET_OFFSET: u64 = 0x0;
const VALHEIM_MEMORY_SIZE: usize = 4 * 1024 * 1024 * 1024; // 4GiB

#[derive(Debug)]
pub struct RV64Cpu {
  regs: regs::Regs,
  mem: memory::Memory,
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

  pub fn read_mem<T: Copy + Sized>(&self, addr: VirtAddr) -> Option<T> {
    self.mem.read(addr)
  }

  pub fn write_mem<T: Copy + Sized>(&mut self, addr: VirtAddr, val: T) -> Option<()> {
    self.mem.write(addr, val)
  }

  pub fn read_reg(&self, reg: Reg) -> Option<u64> {
    self.regs.read(reg)
  }

  pub fn write_reg(&mut self, reg: Reg, val: u64) -> Option<()> {
    self.regs.write(reg, val)
  }

  /// internal fast-path
  #[inline(always)]
  fn read_pc(&self) -> VirtAddr {
    self.regs.pc
  }

  /// internal fast-path
  #[inline(always)]
  fn write_pc(&mut self, pc: VirtAddr) {
    self.regs.pc = pc
  }

  pub fn fetch(&mut self) -> Option<(VirtAddr, Bytecode)> {
    let pc = self.read_pc();
    let instr = self.read_mem(pc);
    instr.map(|instr| (pc, instr))
  }

  pub fn decode(&self, _: VirtAddr, untyped: Bytecode) -> Option<Instr> {
    Instr::try_from(untyped)
  }

  pub fn execute(&mut self, pc: VirtAddr, instr: Instr) -> Option<()> {
    match instr {
      Instr::RV32(RV32Instr::EBREAK) => return None,
      _ => (),
    }
    let new_pc = VirtAddr(pc.0 + std::mem::size_of::<Bytecode>() as u64);
    self.write_pc(new_pc);
    Some(())
  }

  pub fn run(&mut self, int: &dyn RV64Interpreter) {
    int.interp(self)
  }

  pub fn reset<T: Sized>(&mut self, mem: &[T]) {
    self.mem.reset(mem);
    self.write_pc(self.cpu_reset_pc);
  }

  pub fn halt(&self) {
    println!("CPU halt with registers: {:?}", self.regs);
  }
}
