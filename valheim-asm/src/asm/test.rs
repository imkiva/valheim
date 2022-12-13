#[cfg(test)]
mod masm {
  use crate::asm::{a0, Assembler, t0};
  #[test]
  pub fn sum() {
    let mut asm = Assembler::new(0);
    // int sum = 0;
    // int i = 0;
    // while (i <= 100)
    //   sum = sum + i;
    let sum = a0;
    let i = t0;
  }
}
