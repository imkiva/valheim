use crate::isa::typed::Reg;
use crate::memory::VirtAddr;

#[derive(Debug, Clone, Copy)]
pub struct Regs {
  pub x: [u64; 32],
  pub f: [f64; 32],
  pub pc: VirtAddr,
}

impl Regs {
  pub fn new() -> Regs {
    Regs {
      x: [0; 32],
      f: [0.0; 32],
      pc: VirtAddr(0),
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

  pub fn read_fp(&self, reg: Reg) -> Option<f64> {
    match reg {
      Reg::F(f) => Some(self.f[f.value() as usize]),
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

  pub fn write_fp(&mut self, reg: Reg, value: f64) -> Option<()> {
    match reg {
      Reg::F(f) => self.f[f.value() as usize] = value,
      _ => return None,
    }
    Some(())
  }
}
