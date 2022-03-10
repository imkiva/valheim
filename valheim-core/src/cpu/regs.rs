use crate::memory::VirtAddr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Regs {
  pub x: [u64; 32],
  pub csr: u64,
  pub pc: VirtAddr,
}

impl Regs {
  pub fn new(pc: VirtAddr) -> Regs {
    Regs {
      x: [0; 32],
      csr: 0,
      pc,
    }
  }
}
