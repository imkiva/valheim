pub mod scratch;

use crate::cpu::Rv64Cpu;

pub trait Rv64Interpreter {
  fn interp(&mut self, cpu: &mut Rv64Cpu);
}
