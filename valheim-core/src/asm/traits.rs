use crate::isa::typed::{Imm32, Rd, Reg, Rs1, Rs2, Rs3};

macro_rules! reg_into {
  ($ty:ident) => {
    impl Into<$ty> for Reg {
      fn into(self) -> $ty {
        $ty(self)
      }
    }
  };
}

reg_into!(Rd);
reg_into!(Rs1);
reg_into!(Rs2);
reg_into!(Rs3);

impl<const HIGH_BIT: usize, const LOW_BIT: usize> Into<Imm32<HIGH_BIT, LOW_BIT>> for u32 {
  fn into(self) -> Imm32<HIGH_BIT, LOW_BIT> {
    Imm32::from(self)
  }
}

impl<const HIGH_BIT: usize, const LOW_BIT: usize> Into<Imm32<HIGH_BIT, LOW_BIT>> for i32 {
  fn into(self) -> Imm32<HIGH_BIT, LOW_BIT> {
    (self as u32).into()
  }
}

#[cfg(test)]
mod test {
  use crate::isa::typed::Imm32;

  #[test]
  fn imm32() {
    let i: Imm32<31, 12> = 0b1111_1110_1101_1011_0111.into();
    assert_eq!(i.decode(), 0b1111_1110_1101_1011_0111_000000000000);
  }
}
