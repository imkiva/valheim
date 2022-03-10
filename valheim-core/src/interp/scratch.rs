use std::intrinsics::size_of;
use crate::cpu::RV64Cpu;
use crate::interp::Rv64Interpreter;
use crate::isa::rv32::RV32Instr;
use crate::isa::typed::Instr;
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;

pub struct ScratchInterpreter;

impl ScratchInterpreter {
  pub fn new() -> ScratchInterpreter {
    ScratchInterpreter {}
  }
}

impl ScratchInterpreter {
  fn fetch(&self, cpu: &mut RV64Cpu) -> Option<(VirtAddr, Bytecode)> {
    let pc = cpu.regs.pc;
    let instr = cpu.mem.read(pc);
    instr.map(|instr| (pc, instr))
  }

  fn decode(&self, bytecode: Bytecode) -> Option<Instr> {
    Instr::try_from(bytecode)
  }
}

impl Rv64Interpreter for ScratchInterpreter {
  fn interp(&self, cpu: &mut RV64Cpu) {
    while let Some((pc, instr)) = self.fetch(cpu) {
      if let Some(decoded) = self.decode(instr) {
        println!("pc = {:#010x}, instr = {:#010x}, decoded = {:?}", pc.0, instr.repr(), decoded);
        match decoded {
          Instr::RV32(RV32Instr::EBREAK) => break,
          _ => (),
        }
        cpu.regs.pc += VirtAddr(size_of::<Bytecode>() as u64);
      } else {
        // TODO: invalid instruction interrupt?
      }
    }
    println!("CPU halt with registers: {:?}", cpu.regs);
  }
}
