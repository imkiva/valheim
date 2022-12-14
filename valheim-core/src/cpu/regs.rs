use valheim_asm::isa::typed::Reg;

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

impl Regs {
  pub fn show_status(&self) {
    println!("General purpose registers:");
    println!("  pc : {:#x}", self.pc.0);
    for i in 0..31 {
      self.show_one_status(i);
    }
  }

  pub fn show_one_status(&self, i: usize) {
    let xabi = [
      "zero", " ra ", " sp ", " gp ", " tp ", " t0 ", " t1 ", " t2 ", " s0 ", " s1 ", " a0 ",
      " a1 ", " a2 ", " a3 ", " a4 ", " a5 ", " a6 ", " a7 ", " s2 ", " s3 ", " s4 ", " s5 ",
      " s6 ", " s7 ", " s8 ", " s9 ", " s10", " s11", " t3 ", " t4 ", " t5 ", " t6 ",
    ];
    let fabi = [
      " ft0", " ft1", " ft2", " ft3", " ft4", " ft5", " ft6", " ft7",
      " fs0", " fs1",
      " fa0", " fa1",
      " fa2", " fa3", " fa4", " fa5", " fa6", " fa7",
      " fs2", " fs3", " fs4", " fs5", " fs6", " fs7", " fs8", " fs9", "fs10", "fs11",
      " ft8", " ft9", "ft10", "ft11",
    ];
    println!("  x{:<2} ({}): {:<#18x}    f{:<2} ({}): {:}", i, xabi[i], self.x[i], i, fabi[i], self.f[i]);
  }
}
