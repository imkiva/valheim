#![allow(dead_code)]

use crate::asm::{Assembler, Compare, ra, zero};
use crate::isa::rv32::RV32Instr::*;
use crate::isa::rv64::RV64Instr::*;
use crate::isa::typed::Reg;

/// Pseudo-instructions
impl Assembler {
  pub fn mov(&mut self, rd: Reg, rs: Reg) {
    self.addi(rd, rs, 0);
  }

  pub fn not(&mut self, rd: Reg, rs: Reg) {
    self.xori(rd, rs, -1);
  }

  pub fn nop(&mut self) {
    self.addi(zero, zero, 0);
  }

  pub fn jump_offset(&mut self, offset: i32) {
    self.jal_rv(zero, offset);
  }

  pub fn jump_reg(&mut self, rs: Reg) {
    self.jalr_rv(zero, rs, 0);
  }

  pub fn ret(&mut self) {
    self.jalr_rv(zero, ra, 0);
  }

  pub fn call_offset(&mut self, offset: i32) {
    self.load_pc(ra, offset); // TODO: use offset in `jalr_rv`, instead of `addi`
    self.call_reg(ra);
  }

  pub fn call_reg(&mut self, rs: Reg) {
    self.jalr_rv(ra, rs, 0);
  }

  pub fn branch(&mut self, cond: Compare, rs1: Reg, rs2: Reg, offset: i32) {
    self.b_rv(cond, rs1, rs2, offset);
  }

  pub fn beq(&mut self, rs1: Reg, rs2: Reg, offset: i32) {
    self.branch(Compare::EQ, rs1, rs2, offset);
  }

  pub fn bne(&mut self, rs1: Reg, rs2: Reg, offset: i32) {
    self.branch(Compare::NE, rs1, rs2, offset);
  }

  pub fn blt(&mut self, rs1: Reg, rs2: Reg, offset: i32) {
    self.branch(Compare::LT, rs1, rs2, offset);
  }

  pub fn bge(&mut self, rs1: Reg, rs2: Reg, offset: i32) {
    self.branch(Compare::GE, rs1, rs2, offset);
  }

  pub fn bltu(&mut self, rs1: Reg, rs2: Reg, offset: i32) {
    self.branch(Compare::LTU, rs1, rs2, offset);
  }

  pub fn bgeu(&mut self, rs1: Reg, rs2: Reg, offset: i32) {
    self.branch(Compare::GEU, rs1, rs2, offset);
  }

  pub fn beqz(&mut self, rs: Reg, offset: i32) {
    self.beq(rs, zero, offset);
  }

  pub fn bnez(&mut self, rs: Reg, offset: i32) {
    self.bne(rs, zero, offset);
  }

  pub fn bltz(&mut self, rs: Reg, offset: i32) {
    self.blt(rs, zero, offset);
  }

  pub fn bgez(&mut self, rs: Reg, offset: i32) {
    self.bge(rs, zero, offset);
  }

  pub fn bltuz(&mut self, rs: Reg, offset: i32) {
    self.bltu(rs, zero, offset);
  }

  pub fn bgeuz(&mut self, rs: Reg, offset: i32) {
    self.bgeu(rs, zero, offset);
  }

  pub fn load_imm(&mut self, rd: Reg, imm: i32) {
    let high = ((imm as u32) & 0xfffff000) >> 12;
    let low = (imm as u32) & 0xfff;
    if high != 0 {
      self.lui_rv(rd, high as i32);
      self.addi(rd, rd, low as i32);
    } else {
      self.addi(rd, zero, low as i32);
    }
  }

  pub fn load_pc(&mut self, rd: Reg, offset: i32) {
    let high = ((offset as u32) & 0xfffff000) >> 12;
    let low = (offset as u32) & 0xfff;
    if high != 0 {
      self.auipc_rv(rd, high as i32);
      self.addi(rd, rd, low as i32);
    } else {
      self.addi(rd, zero, low as i32);
    }
  }

  pub fn inc(&mut self, rd: Reg) {
    self.addi_rv(rd, rd, 1);
  }

  pub fn dec(&mut self, rd: Reg) {
    self.addi_rv(rd, rd, -1);
  }

  pub fn add(&mut self, rd: Reg, rs1: Reg, rs2: Reg) {
    self.add_rv(rd, rs1, rs2);
  }

  pub fn addi(&mut self, rd: Reg, rs: Reg, imm: i32) {
    self.addi_rv(rd, rs, imm);
  }

  pub fn xori(&mut self, rd: Reg, rs: Reg, imm: i32) {
    self.xori_rv(rd, rs, imm);
  }
}

/// 32-bit operands
impl Assembler {
  pub fn sign_extend_32(&mut self, rd: Reg, rs: Reg) {
    self.addi_32(rd, rs, 0);
  }

  pub fn addi_32(&mut self, rd: Reg, rs: Reg, imm: i32) {
    self.addiw_rv(rd, rs, imm);
  }
}

/// Raw instructions, prefixed with `_rv`
impl Assembler {
  pub fn auipc_rv(&mut self, rd: Reg, imm: i32) {
    self.emit32(AUIPC(rd.into(), imm.into()));
  }

  pub fn lui_rv(&mut self, rd: Reg, imm: i32) {
    self.emit32(LUI(rd.into(), imm.into()));
  }

  pub fn jal_rv(&mut self, rd: Reg, offset: i32) {
    self.emit32(JAL(rd.into(), offset.into()));
  }

  pub fn jalr_rv(&mut self, rd: Reg, rs: Reg, offset: i32) {
    self.emit32(JALR(rd.into(), rs.into(), offset.into()));
  }

  pub fn b_rv(&mut self, cmp: Compare, rs1: Reg, rs2: Reg, offset: i32) {
    match cmp {
      Compare::EQ => self.emit32(BEQ(rs1.into(), rs2.into(), offset.into())),
      Compare::NE => self.emit32(BNE(rs1.into(), rs2.into(), offset.into())),
      Compare::LT => self.emit32(BLT(rs1.into(), rs2.into(), offset.into())),
      Compare::GE => self.emit32(BGE(rs1.into(), rs2.into(), offset.into())),
      Compare::LTU => self.emit32(BLTU(rs1.into(), rs2.into(), offset.into())),
      Compare::GEU => self.emit32(BGEU(rs1.into(), rs2.into(), offset.into())),
    }
  }

  pub fn addi_rv(&mut self, rd: Reg, rs: Reg, imm: i32) {
    self.emit32(ADDI(rd.into(), rs.into(), imm.into()));
  }

  pub fn xori_rv(&mut self, rd: Reg, rs: Reg, imm: i32) {
    self.emit32(XORI(rd.into(), rs.into(), imm.into()));
  }

  pub fn addiw_rv(&mut self, rd: Reg, rs: Reg, imm: i32) {
    self.emit32(ADDIW(rd.into(), rs.into(), imm.into()));
  }

  pub fn add_rv(&mut self, rd: Reg, rs1: Reg, rs2: Reg) {
    self.emit32(ADD(rd.into(), rs1.into(), rs2.into()));
  }
}
