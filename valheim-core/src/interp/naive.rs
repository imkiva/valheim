use crate::cpu::data::Either;
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
    while let Some((pc, untyped, compressed)) = cpu.fetch() {
      if let Some((from, decoded)) = cpu.decode(pc, untyped, compressed) {
        let (is_compressed, repr) = match from {
          Either::Left(untyped) => (false, untyped.repr()),
          Either::Right(compressed) => (true, compressed.repr() as u32),
        };
        println!("pc = {:#010x}, instr = {:#010x}, RVC = {}, decoded = {:?}", pc.0, repr, is_compressed, decoded);
        if cpu.execute(pc, decoded, is_compressed).is_none() { break; }
      } else {
        // TODO: invalid instruction interrupt?
        println!("pc = {:#010x}, instr = {:#010x} (maybe compressed: {:#010x}), which is an invalid instruction",
                 pc.0, untyped.repr(), compressed.repr());
      }
    }
    cpu.halt();
  }
}
