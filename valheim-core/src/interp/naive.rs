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
    cpu.instr = untyped.repr() as u64; // used as mtval when illegal instruction
    let (from, decoded) = cpu.decode(pc, untyped, compressed)?;
    let is_compressed = match from {
      Either::Left(_) => false,
      Either::Right(_) => true,
    };
    // println!("pc = {:x}, RVC = {}, instr = {:?}", pc.0, is_compressed, decoded);
    cpu.execute(pc, decoded, is_compressed)
  }
}
