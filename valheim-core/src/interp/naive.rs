use crate::cpu::RV64Cpu;
use crate::interp::RV64Interpreter;

pub struct NaiveInterpreter;

impl NaiveInterpreter {
  pub fn new() -> NaiveInterpreter {
    NaiveInterpreter {}
  }
}

impl RV64Interpreter for NaiveInterpreter {
  fn interp(&self, cpu: &mut RV64Cpu) {
    while let Some((pc, instr)) = cpu.fetch() {
      if let Some(decoded) = cpu.decode(pc, instr) {
        println!("pc = {:#010x}, instr = {:#010x}, decoded = {:?}", pc.0, instr.repr(), decoded);
        if cpu.execute(pc, decoded).is_none() { break; }
      } else {
        // TODO: invalid instruction interrupt?
        println!("pc = {:#010x}, instr = {:#010x}, which is an invalid instruction", pc.0, instr.repr());
      }
    }
    cpu.halt();
  }
}
