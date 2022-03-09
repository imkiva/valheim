use crate::isa::rv32::RV32Instr;
use crate::isa::rv64::RV64Instr;

/// decoded instruction
pub enum Instr {
  RV32(RV32Instr),
  RV64(RV64Instr),
}

/// This is used to represent the immediate value,
/// which is written as `imm[HIGH_BIT:LOW_BIT]` in the risc-v specification.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Imm32<const HIGH_BIT: usize, const LOW_BIT: usize> {
  pub underlying: u32,
}

impl<const HIGH_BIT: usize, const LOW_BIT: usize> Imm32<HIGH_BIT, LOW_BIT> {
  pub fn from(underlying: u32) -> Self {
    Self { underlying }
  }

  pub fn valid_bits(&self) -> usize {
    HIGH_BIT - LOW_BIT + 1
  }

  /// Decode the immediate value by placing its valid bits at the range of `[HIGH_BIT, LOW_BIT]`
  /// according to the risc-v specification.
  pub fn decode(self) -> u32 {
    let mask = (1 << self.valid_bits()) - 1;
    (self.underlying & mask) << LOW_BIT
  }
}

struct If<const B: bool>;
trait True {}
impl True for If<true> {}

impl<
  const LHS_HIGH_BIT: usize,
  const LHS_LOW_BIT: usize,
  const RHS_HIGH_BIT: usize,
  const RHS_LOW_BIT: usize
> std::ops::BitOr<Imm32<RHS_HIGH_BIT, RHS_LOW_BIT>> for Imm32<LHS_HIGH_BIT, LHS_LOW_BIT>
  where If<{ LHS_HIGH_BIT >= LHS_LOW_BIT }>: True,
        If<{ RHS_HIGH_BIT >= RHS_LOW_BIT }>: True,
        If<{ LHS_LOW_BIT - 1 == RHS_HIGH_BIT }>: True,
{
  type Output = Imm32<LHS_HIGH_BIT, RHS_LOW_BIT>;

  fn bitor(self, rhs: Imm32<RHS_HIGH_BIT, RHS_LOW_BIT>) -> Self::Output {
    Self::Output::from((self.decode() | rhs.decode()) >> RHS_LOW_BIT)
  }
}

#[cfg(test)]
mod tests {
  use std::ops::BitOr;
  use crate::isa::typed::Imm32;
  use crate::isa::untyped::JType;
  #[test]
  fn test_imm_encode() {
    // jal x0, -6*4
    let instr_asm: u32 = 0b_1_1111110100_1_11111111_00000_1101111;
    let instr = JType::from_bytes(instr_asm.to_le_bytes());

    let imm19_12 = Imm32::<19, 12>::from(instr.imm19_12() as u32);
    let imm11 = Imm32::<11, 11>::from(instr.imm11() as u32);
    let imm10_1 = Imm32::<10, 1>::from(instr.imm10_1() as u32);
    let imm20 = Imm32::<20, 20>::from(instr.imm20() as u32);
    let all = imm20.bitor(imm19_12).bitor(imm11).bitor(imm10_1);

    assert_eq!(imm19_12.decode(), 0b00000000000011111111000000000000);
    assert_eq!(imm11.decode(), 0b00000000000000000000100000000000);
    assert_eq!(imm10_1.decode(), 0b00000000000000000000011111101000);
    assert_eq!(imm20.decode(), 0b00000000000100000000000000000000);

    let all_u32 = imm20.decode() | imm19_12.decode() | imm11.decode() | imm10_1.decode();
    assert_eq!(all.decode(), all_u32);
  }
}
