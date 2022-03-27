use std::fmt::{Debug, Formatter};

use crate::isa::data::{Fin, workaround};
use crate::isa::rv32::RV32Instr;
use crate::isa::rv64::RV64Instr;

/// decoded instruction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instr {
  RV32(RV32Instr),
  RV64(RV64Instr),
  NOP,
}

/// Destination register
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rd(pub Reg);
/// Source register
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rs1(pub Reg);
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rs2(pub Reg);
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Rs3(pub Reg);

/// Atomic instruction flag: Acquire
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct AQ(pub bool);
/// Atomic instruction flag: Release
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct RL(pub bool);

/// Shift-amount
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Shamt(pub u8);

/// typed-registers
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Reg {
  /// Same as `X(Fin(0))`, but for better performance
  ZERO,
  X(Fin<32>),
  F(Fin<32>),
  PC,
  FCSR,
}

/// Floating-point rounding mode
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum RoundingMode {
  /// Round to nearest, ties to even
  RNE = 0b000,
  /// Round towards zero
  RTZ = 0b001,
  /// Round towards -infinity
  RDN = 0b010,
  /// Round towards +infinity
  RUP = 0b011,
  /// Round to nearest, ties to max magnitude
  RMM = 0b100,
  /// In instruction's rm field, select dynamic rounding mode;
  /// In Rounding Mode register, reserved.
  DYN = 0b111,
}

/// This is used to represent lazily-decoded immediate value,
/// which is written as `imm[HIGH_BIT:LOW_BIT]` in the risc-v specification.
#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Imm32<const HIGH_BIT: usize, const LOW_BIT: usize>(pub u32);

impl<const HIGH_BIT: usize, const LOW_BIT: usize> Imm32<HIGH_BIT, LOW_BIT> {
  pub fn from(underlying: u32) -> Self {
    Self(underlying)
  }

  pub fn valid_bits(&self) -> usize {
    HIGH_BIT - LOW_BIT + 1
  }

  /// Decode the immediate value by placing its valid bits at the range of `[HIGH_BIT, LOW_BIT]`
  /// according to the risc-v specification.
  pub fn decode(self) -> u32 {
    let mask = (1 << self.valid_bits()) - 1;
    (self.0 & mask) << LOW_BIT
  }

  pub fn decode_sext(self) -> i32 {
    sign_extend32(self.decode(), HIGH_BIT)
  }
}

impl<const HIGH_BIT: usize, const LOW_BIT: usize> Debug for Imm32<HIGH_BIT, LOW_BIT> {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "Imm({}, sext = {})", self.decode(), self.decode_sext())
  }
}

/// `sign_bit` The sign bit position of the `data`
#[inline(always)]
fn sign_extend32(data: u32, sign_bit: usize) -> i32 {
  ((data << (31 - sign_bit)) as i32) >> (31 - sign_bit)
}

impl<
  const LHS_HIGH_BIT: usize,
  const LHS_LOW_BIT: usize,
  const RHS_HIGH_BIT: usize,
  const RHS_LOW_BIT: usize
> std::ops::BitOr<Imm32<RHS_HIGH_BIT, RHS_LOW_BIT>> for Imm32<LHS_HIGH_BIT, LHS_LOW_BIT>
  where workaround::If<{ LHS_HIGH_BIT >= LHS_LOW_BIT }>: workaround::True,
        workaround::If<{ RHS_HIGH_BIT >= RHS_LOW_BIT }>: workaround::True,
        workaround::If<{ LHS_LOW_BIT - 1 == RHS_HIGH_BIT }>: workaround::True,
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
  fn test_imm_decode() {
    // jal x0, -24
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

  #[test]
  fn test_jal_decode() {
    let instr_asm: u32 = 0x760c30ef;
    let instr = JType::from_bytes(instr_asm.to_le_bytes());

    let imm19_12 = Imm32::<19, 12>::from(instr.imm19_12() as u32);
    let imm11 = Imm32::<11, 11>::from(instr.imm11() as u32);
    let imm10_1 = Imm32::<10, 1>::from(instr.imm10_1() as u32);
    let imm20 = Imm32::<20, 20>::from(instr.imm20() as u32);
    let all = imm20.bitor(imm19_12).bitor(imm11).bitor(imm10_1);

    let all_u32 = imm20.decode() | imm19_12.decode() | imm11.decode() | imm10_1.decode();
    assert_eq!(all.decode(), all_u32);

    // imm[20|10:1|11|19:12] = inst[31|30:21|20|19:12]
    let instr_asm = instr_asm as u64;
    let offset = (((instr_asm & 0x80000000) as i32 as i64 >> 11) as u64) // imm[20]
      | (instr_asm & 0xff000) // imm[19:12]
      | ((instr_asm >> 9) & 0x800) // imm[11]
      | ((instr_asm >> 20) & 0x7fe); // imm[10:1]

    dbg!(all.decode_sext());
    dbg!(all.decode());
    dbg!(offset);
  }
}
