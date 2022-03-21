use crate::cpu::data::Either;
use crate::cpu::irq::Exception;
use crate::cpu::RV64Cpu;
use crate::interp::RV64Interpreter;

pub struct NaiveInterpreter;

impl NaiveInterpreter {
  pub fn new() -> NaiveInterpreter {
    NaiveInterpreter {}
  }
}

impl RV64Interpreter for NaiveInterpreter {
  fn interp(&self, cpu: &mut RV64Cpu) -> Result<(), Exception> {
    if cpu.wfi {
      return Ok(());
    }
    let (pc, untyped, compressed) = cpu.fetch()?;
    let (from, decoded) = cpu.decode(pc, untyped, compressed)?;
    let (is_compressed, repr) = match from {
      Either::Left(untyped) => (false, untyped.repr()),
      Either::Right(compressed) => (true, compressed.repr() as u32),
    };
    // always put this before execute so it can be used to detect illegal instructions
    // println!("pc = {:x}, RVC = {}, instr = {:?}", pc.0, is_compressed, decoded);
    cpu.previous_instr = repr as u64;
    cpu.execute(pc, decoded, is_compressed)
  }
}
