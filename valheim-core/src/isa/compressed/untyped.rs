use std::fmt::{Debug, Formatter};
use modular_bitfield::prelude::*;
use crate::memory::CanIO;
use crate::unsafe_wrapper;

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CRType {
  pub op: B2,
  pub rs2: B5,
  pub rd_or_rs1: B5,
  pub funct4: B4,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CIType {
  pub op: B2,
  pub imm_b5: B5,
  pub rd: B5,
  pub imm_b1: B1,
  pub funct3: B3,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CSSType {
  pub op: B2,
  pub rs2: B5,
  pub imm_b6: B6,
  pub funct3: B3,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CIWType {
  pub op: B2,
  pub rd: B3,
  pub imm_b8: B8,
  pub funct3: B3,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CLType {
  pub op: B2,
  pub rd: B3,
  pub imm_b2: B2,
  pub rs1: B3,
  pub imm_b3: B3,
  pub funct3: B3,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CSType {
  pub op: B2,
  pub rs2: B3,
  pub imm_b2: B2,
  pub rs1: B3,
  pub imm_b3: B3,
  pub funct3: B3,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CAType {
  pub op: B2,
  pub rs2: B3,
  pub funct2: B2,
  pub rd_or_rs1: B3,
  pub funct6: B6,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CBType {
  pub op: B2,
  pub imm5: B1,
  pub imm2_1: B2,
  pub imm7_6: B2,
  pub rd_or_rs1: B3,
  pub imm4_3: B2,
  pub imm8: B1,
  pub funct3: B3,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CBIType {
  pub op: B2,
  pub imm4_0: B5,
  pub rd_or_rs1: B3,
  pub funct2: B2,
  pub imm5: B1,
  pub funct3: B3,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CJType {
  pub op: B2,
  pub imm5: B1,
  pub imm3_1: B3,
  pub imm7: B1,
  pub imm6: B1,
  pub imm10: B1,
  pub imm9_8: B2,
  pub imm4: B1,
  pub imm11: B1,
  pub funct3: B3,
}

#[bitfield(bits = 16)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct OpcodePeek16 {
  pub op: B2,
  pub dummy: B11,
  pub funct3: B3,
}

/// untyped instruction
#[derive(Clone, Copy)]
#[repr(C)]
pub union Bytecode16 {
  pub repr: u16,
  pub peek: OpcodePeek16,
  pub cr: CRType,
  pub ci: CIType,
  pub css: CSSType,
  pub ciw: CIWType,
  pub cl: CLType,
  pub cs: CSType,
  pub ca: CAType,
  pub cb: CBType,
  pub cbi: CBIType,
  pub cj: CJType,
}

impl Bytecode16 {
  #[inline(always)]
  pub fn opcode(&self) -> u8 {
    unsafe { self.peek.op() }
  }
  #[inline(always)]
  pub fn funct3(&self) -> u8 {
    unsafe { self.peek.funct3() }
  }
  unsafe_wrapper!(repr, u16);
  unsafe_wrapper!(cr, CRType);
  unsafe_wrapper!(ci, CIType);
  unsafe_wrapper!(css, CSSType);
  unsafe_wrapper!(ciw, CIWType);
  unsafe_wrapper!(cl, CLType);
  unsafe_wrapper!(cs, CSType);
  unsafe_wrapper!(ca, CAType);
  unsafe_wrapper!(cb, CBType);
  unsafe_wrapper!(cbi, CBIType);
  unsafe_wrapper!(cj, CJType);
}

impl CanIO for Bytecode16 {}

impl Debug for Bytecode16 {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "Bytecode16({:#06b})", self.repr())
  }
}

impl PartialEq for Bytecode16 {
  fn eq(&self, other: &Self) -> bool {
    self.repr() == other.repr()
  }
}

impl Eq for Bytecode16 {}
