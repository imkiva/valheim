use std::fmt::{Debug, Formatter};

use modular_bitfield::prelude::*;

use crate::memory::CanIO;

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RType {
  pub opcode: B7,
  pub rd: B5,
  pub funct3: B3,
  pub rs1: B5,
  pub rs2: B5,
  pub funct7: B7,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct IType {
  pub opcode: B7,
  pub rd: B5,
  pub funct3: B3,
  pub rs1: B5,
  pub imm11_0: B12,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SType {
  pub opcode: B7,
  pub imm4_0: B5,
  pub funct3: B3,
  pub rs1: B5,
  pub rs2: B5,
  pub imm11_5: B7,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BType {
  pub opcode: B7,
  pub imm11: B1,
  pub imm4_1: B4,
  pub funct3: B3,
  pub rs1: B5,
  pub rs2: B5,
  pub imm10_5: B6,
  pub imm12: B1,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct UType {
  pub opcode: B7,
  pub rd: B5,
  pub imm31_12: B20,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct JType {
  pub opcode: B7,
  pub rd: B5,
  pub imm19_12: B8,
  pub imm11: B1,
  pub imm10_1: B10,
  pub imm20: B1,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct R4Type {
  pub opcode: B7,
  pub rd: B5,
  pub funct3: B3,
  pub rs1: B5,
  pub rs2: B5,
  pub funct2: B2,
  pub rs3: B5,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RShamt32Type {
  pub opcode: B7,
  pub rd: B5,
  pub funct3: B3,
  pub rs1: B5,
  pub shamt: B5,
  pub funct7: B7,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RAType {
  pub opcode: B7,
  pub rd: B5,
  pub funct3: B3,
  pub rs1: B5,
  pub rs2: B5,
  pub rl: bool,
  pub aq: bool,
  pub funct5: B5,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RShamt64Type {
  pub opcode: B7,
  pub rd: B5,
  pub funct3: B3,
  pub rs1: B5,
  pub shamt: B6,
  pub funct6: B6,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FenceType {
  pub opcode: B7,
  pub rd: B5,
  pub funct3: B3,
  pub rs1: B5,
  pub succ: B4,
  pub pred: B4,
  pub fm: B4,
}

#[bitfield(bits = 32)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct OpcodePeek {
  pub opcode: B7,
  pub dummy: B25,
}

/// untyped instruction
#[derive(Clone, Copy)]
#[repr(C)]
pub union Bytecode {
  pub repr: u32,
  pub peek: OpcodePeek,
  pub r: RType,
  pub r4: R4Type,
  pub r_shamt32: RShamt32Type,
  pub r_shamt64: RShamt64Type,
  pub ra: RAType,
  pub i: IType,
  pub s: SType,
  pub b: BType,
  pub u: UType,
  pub j: JType,
  pub fence: FenceType,
}

#[macro_export] macro_rules! unsafe_wrapper {
  ($ident:ident, $ty:ident) => {
    #[inline(always)]
    pub fn $ident(&self) -> $ty {
      unsafe { self.$ident }
    }
  }
}

impl Bytecode {
  #[inline(always)]
  pub fn opcode(&self) -> u8 {
    unsafe { self.peek.opcode() }
  }
  unsafe_wrapper!(repr, u32);
  unsafe_wrapper!(r, RType);
  unsafe_wrapper!(r4, R4Type);
  unsafe_wrapper!(r_shamt32, RShamt32Type);
  unsafe_wrapper!(r_shamt64, RShamt64Type);
  unsafe_wrapper!(ra, RAType);
  unsafe_wrapper!(i, IType);
  unsafe_wrapper!(s, SType);
  unsafe_wrapper!(b, BType);
  unsafe_wrapper!(j, JType);
  unsafe_wrapper!(u, UType);
  unsafe_wrapper!(fence, FenceType);
}

impl CanIO for Bytecode {}

impl Debug for Bytecode {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "Bytecode({:#010b})", self.repr())
  }
}

impl PartialEq for Bytecode {
  fn eq(&self, other: &Self) -> bool {
    self.repr() == other.repr()
  }
}

impl Eq for Bytecode {}

#[cfg(test)]
mod tests {
  use crate::isa::untyped::JType;

  #[test]
  fn test_layout() {
    // jal x0, -6*4
    let instr_asm: u32 = 0b_1_1111110100_1_11111111_00000_1101111;
    let instr = JType::from_bytes(instr_asm.to_le_bytes());
    assert_eq!(instr.opcode(), 0b_1101111);
    assert_eq!(instr.rd(), 0b0);
    assert_eq!(instr.imm19_12(), 0b11111111);
    assert_eq!(instr.imm11(), 0b1);
    assert_eq!(instr.imm10_1(), 0b1111110100);
    assert_eq!(instr.imm20(), 0b1);
  }
}
