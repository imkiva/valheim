use crate::cpu::irq::Exception;
use crate::cpu::RV64Cpu;

pub mod naive;

pub trait RV64Interpreter {
  fn interp(&self, cpu: &mut RV64Cpu) -> Result<(), Exception>;
}
