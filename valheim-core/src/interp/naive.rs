use crate::cpu::data::Either;
use crate::cpu::RV64Cpu;
use crate::interp::{ExecOutcome, RV64Executor};

pub struct NaiveInterpreter;

impl NaiveInterpreter {
  pub fn new() -> NaiveInterpreter {
    NaiveInterpreter {}
  }
}

impl RV64Executor for NaiveInterpreter {
  fn execute(&mut self, cpu: &mut RV64Cpu, budget: u32) -> ExecOutcome {
    if budget == 0 || cpu.wfi {
      return ExecOutcome::new(0, Ok(()));
    }

    let result = (|| {
      let (pc, untyped, compressed) = cpu.fetch()?;
      cpu.instr = untyped.repr() as u64; // used as mtval when illegal instruction
      let (from, decoded) = cpu.decode(pc, untyped, compressed)?;
      let is_compressed = match from {
        Either::Left(_) => false,
        Either::Right(_) => true,
      };
      // println!("pc = {:x}, RVC = {}, instr = {:?}", pc.0, is_compressed, decoded);
      cpu.execute(pc, decoded, is_compressed)
    })();

    ExecOutcome::new(1, result)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::cpu::bus::RV64_MEMORY_BASE;
  use crate::cpu::irq::Exception;
  use crate::memory::VirtAddr;

  #[test]
  fn naive_executor_attempts_exactly_one_instruction() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.write_pc(pc);
    cpu.bus.write::<u32>(pc, 0x0000_0013).unwrap(); // addi x0, x0, 0

    let mut executor = NaiveInterpreter::new();
    let outcome = executor.execute(&mut cpu, 32);

    assert_eq!(outcome, ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), pc + VirtAddr(4));
  }

  #[test]
  fn naive_executor_counts_faulting_instruction_as_attempted() {
    let mut cpu = RV64Cpu::new(None);
    cpu.write_pc(VirtAddr(0));

    let mut executor = NaiveInterpreter::new();
    let outcome = executor.execute(&mut cpu, 32);

    assert_eq!(outcome.attempted, 1);
    assert_eq!(
      outcome.result,
      Err(Exception::InstructionAccessFault(VirtAddr(0))),
    );
    assert_eq!(cpu.read_pc(), VirtAddr(0));
  }

  #[test]
  fn naive_executor_does_not_attempt_an_instruction_while_waiting() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.write_pc(pc);
    cpu.wfi = true;

    let mut executor = NaiveInterpreter::new();
    let outcome = executor.execute(&mut cpu, 32);

    assert_eq!(outcome, ExecOutcome::new(0, Ok(())));
    assert_eq!(cpu.read_pc(), pc);
  }
}
