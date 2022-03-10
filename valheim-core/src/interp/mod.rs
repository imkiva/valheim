pub mod scratch;

use crate::cpu::RV64Cpu;

pub trait Rv64Interpreter {
  fn interp(&self, cpu: &mut RV64Cpu);
}
