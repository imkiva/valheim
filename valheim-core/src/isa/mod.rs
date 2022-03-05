pub mod rv32;
pub mod rv64;

use modular_bitfield::prelude::*;

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
  pub imm: B12,
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
pub struct OpcodePeek {
  pub opcode: B7,
  pub dummy: B25,
}

#[derive(Clone, Copy)]
pub union Instr {
  pub repr: u32,
  pub peek: OpcodePeek,
  pub r: RType,
  pub i: IType,
  pub s: SType,
  pub b: BType,
  pub u: UType,
  pub j: JType,
}

impl Instr {
  pub fn opcode(&self) -> u8 {
    unsafe { self.peek.opcode() }
  }

  pub fn repr(&self) -> u32 {
    unsafe { self.repr }
  }
}
