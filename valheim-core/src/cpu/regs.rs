use crate::isa::typed::Reg;
use crate::memory::VirtAddr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Regs {
  pub x: [u64; 32],
  pub pc: VirtAddr,
}

impl Regs {
  pub fn new(pc: VirtAddr) -> Regs {
    Regs {
      x: [0; 32],
      pc,
    }
  }

  pub fn read(&self, reg: Reg) -> Option<u64> {
    match reg {
      Reg::ZERO => Some(0),
      Reg::X(x) if x.value() == 0 => Some(0),
      Reg::X(x) => Some(self.x[x.value() as usize]),
      Reg::PC => Some(self.pc.0),
      _ => None,
    }
  }

  pub fn write(&mut self, reg: Reg, value: u64) -> Option<()> {
    match reg {
      Reg::ZERO => (),
      Reg::X(x) if x.value() == 0 => (),
      Reg::X(x) => self.x[x.value() as usize] = value,
      Reg::PC => self.pc = VirtAddr(value),
      _ => return None,
    }
    Some(())
  }
}
