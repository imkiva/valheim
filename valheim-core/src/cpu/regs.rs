use crate::memory::VirtAddr;

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
}
