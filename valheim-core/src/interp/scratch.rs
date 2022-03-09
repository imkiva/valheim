use std::intrinsics::size_of;
use crate::cpu::Rv64Cpu;
use crate::interp::Rv64Interpreter;
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;

pub struct ScratchInterpreter;

impl ScratchInterpreter {
  pub fn new() -> ScratchInterpreter {
    ScratchInterpreter {}
  }
}

impl ScratchInterpreter {
  fn fetch(&mut self, cpu: &mut Rv64Cpu) -> Option<(VirtAddr, Bytecode)> {
    let pc = cpu.regs.pc;
    let instr = cpu.mem.read(pc);
    cpu.regs.pc += VirtAddr(size_of::<Bytecode>() as u64);
    instr.map(|instr| (pc, instr))
  }
}

impl Rv64Interpreter for ScratchInterpreter {
  fn interp(&mut self, cpu: &mut Rv64Cpu) {
    while let Some((pc, instr)) = self.fetch(cpu) {
      println!("pc = {:x}, opcode = {:x}, instr = {:x}", pc.0, instr.opcode(), instr.repr());
    }
  }
}
