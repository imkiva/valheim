pub mod naive;

use crate::cpu::RV64Cpu;

pub trait RV64Interpreter {
  fn interp(&self, cpu: &mut RV64Cpu);
}
