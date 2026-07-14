use crate::cpu::irq::Exception;
use crate::cpu::RV64Cpu;

pub mod naive;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExecOutcome {
  /// Number of guest instructions attempted, including a faulting instruction.
  /// An executor that observes an already waiting hart reports zero.
  pub attempted: u32,
  pub result: Result<(), Exception>,
}

impl ExecOutcome {
  pub fn new(attempted: u32, result: Result<(), Exception>) -> Self {
    Self { attempted, result }
  }
}

pub trait RV64Executor {
  /// Execute at most `budget` guest instructions.
  fn execute(&mut self, cpu: &mut RV64Cpu, budget: u32) -> ExecOutcome;

  fn diagnostics(&self) -> Option<String> {
    None
  }
}
