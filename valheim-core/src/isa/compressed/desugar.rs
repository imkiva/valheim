use crate::isa::compressed::untyped::Bytecode16;
use crate::isa::data::Fin;
use crate::isa::decode::gp;
use crate::isa::typed::{Imm32, Instr, Rd, Reg, Rs1, Rs2, Shamt};
use crate::isa::rv32::RV32Instr;
use crate::isa::rv64::RV64Instr;
use crate::rv32;
use crate::rv64;

macro_rules! hint {
    () => {Instr::NOP};
}

macro_rules! reserved {
    () => {return None};
}

macro_rules! nzimm_is_0 {
    ($untyped:expr) => {$untyped.ci().imm_b5() == 0 && $untyped.ci().imm_b1() == 0};
}

macro_rules! nzimm_not_0 {
    ($untyped:expr) => {!nzimm_is_0!($untyped)};
}

macro_rules! rd_is {
    ($untyped:expr, $n:expr) => {$untyped.ci().rd() == $n};
}

macro_rules! rd_is_0 {
    ($untyped:expr) => {rd_is!($untyped, 0)};
}

macro_rules! arith {
  ($type:ident, $opcode:ident, $untyped:expr) => {{
    let ca = $untyped.ca();
    let rd = Rd(gp_3(ca.rd_or_rs1()));
    let rs2 = Rs2(gp_3(ca.rs2()));
    $type!($opcode, rd, Rs1(rd.0), rs2)
  }};
}

impl Instr {
  pub fn try_from_compressed(untyped: Bytecode16) -> Option<Instr> {
    let repr = untyped.repr();
    match (repr, repr & 0b11) {
      // A 16-bit instruction with all bits zero is permanently reserved as an illegal instruction.
      (0, _) => None,
      (_, 0) => decode_untyped(untyped),
      (_, 1) => decode_untyped(untyped),
      (_, 2) => decode_untyped(untyped),
      _ => None,
    }
  }
}

fn decode_untyped(untyped: Bytecode16) -> Option<Instr> {
  let inst = untyped.repr();
  let instr = match untyped.opcode() {
    0b00 => match untyped.funct3() {
      // C.ADDI4SPN (RES for nzuimm = 0)
      0b000 if untyped.ciw().imm_b8() == 0 => reserved!(),
      0b000 => {
        let ciw = untyped.ciw();
        let rd = Rd(gp_3(ciw.rd()));
        // nzuimm[5:4|9:6|2|3] = inst[12:11|10:7|6|5]
        let nzuimm = ((inst >> 1) & 0x3c0) // nzuimm[9:6]
          | ((inst >> 7) & 0x30) // nzuimm[5:4]
          | ((inst >> 2) & 0x8) // nzuimm[3]
          | ((inst >> 4) & 0x4); // nzuimm[2]
        rv32!(ADDI, rd, Rs1(Reg::X(Fin::new(2))), Imm32::from((nzuimm >> 2) as u32))
      }

      // C.FLD for RV32/64, C.LQ for RV128 (not supported)
      0b001 => todo!("C.FLD"),

      // C.LW
      0b010 => {
        let cl = untyped.cl();
        let rd = Rd(gp_3(cl.rs1()));
        let rs1 = Rs1(gp_3(cl.rs1()));
        // offset[5:3|2|6] = isnt[12:10|6|5]
        let offset = ((inst << 1) & 0x40) // imm[6]
          | ((inst >> 7) & 0x38) // imm[5:3]
          | ((inst >> 4) & 0x4); // imm[2]

        rv32!(LW, rd, rs1, Imm32::from((offset >> 2) as u32))
      }

      // C.LD for RV64/128, C.FLW for RV32 (not supported)
      0b011 => {
        let cl = untyped.cl();
        let rd = Rd(gp_3(cl.rd()));
        let rs1 = Rs1(gp_3(cl.rs1()));
        // offset[5:3|7:6] = isnt[12:10|6:5]
        let offset = ((inst << 1) & 0xc0) // imm[7:6]
          | ((inst >> 7) & 0x38); // imm[5:3]

        rv64!(LD, rd, rs1, Imm32::from((offset >> 3) as u32))
      }

      // Reserved
      0b100 => reserved!(),

      // C.FSD for RV32/64, C.SQ for RV128 (not supported)
      0b101 => todo!("C.FSD"),

      // C.SW
      0b110 => {
        let cs = untyped.cs();
        let rs1 = Rs1(gp_3(cs.rs1()));
        let rs2 = Rs2(gp_3(cs.rs2()));
        // offset[5:3|2|6] = isnt[12:10|6|5]
        let offset = ((inst << 1) & 0x40) // imm[6]
          | ((inst >> 7) & 0x38) // imm[5:3]
          | ((inst >> 4) & 0x4); // imm[2]

        rv32!(SW, rs1, rs2, Imm32::from((offset >> 2) as u32))
      }

      // C.SD for RV64/128, C.FSW for RV32 (not supported)
      0b111 => {
        let cs = untyped.cs();
        let rs1 = Rs1(gp_3(cs.rs1()));
        let rs2 = Rs2(gp_3(cs.rs2()));
        // offset[5:3|7:6] = isnt[12:10|6:5]
        let offset = ((inst << 1) & 0xc0) // imm[7:6]
          | ((inst >> 7) & 0x38); // imm[5:3]

        rv64!(SD, rs1, rs2, Imm32::from((offset >> 3) as u32))
      }
      _ => return None,
    },
    0b01 => match untyped.funct3() {
      // C.NOP
      0b000 if rd_is_0!(untyped) && nzimm_not_0!(untyped) => hint!(),
      0b000 if rd_is_0!(untyped) => Instr::NOP,
      // C.ADDI
      0b000 if nzimm_is_0!(untyped) => hint!(),
      0b000 => {
        let ci = untyped.ci();
        let rd = Rd(gp(ci.rd()));
        // imm[5|4:0] = inst[12|6:2]
        let imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
        rv32!(ADDI, rd, Rs1(rd.0), Imm32::from(sign_extend12(imm, 6) as u32))
      }

      // C.ADDIW for RV64/128 (RES for rd = 0), C.JAL for RV32 (not supported)
      0b001 if rd_is_0!(untyped) => reserved!(),
      0b001 => {
        let ci = untyped.ci();
        let rd = Rd(gp(ci.rd()));
        // imm[5|4:0] = inst[12|6:2]
        let imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
        rv64!(ADDIW, rd, Rs1(rd.0), Imm32::from(sign_extend12(imm, 6) as u32))
      }

      // C.LI
      0b010 => {
        let ci = untyped.ci();
        let rd = Rd(gp(ci.rd()));
        // imm[5|4:0] = inst[12|6:2]
        let imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
        rv32!(ADDI, rd, Rs1(Reg::ZERO), Imm32::from(sign_extend12(imm, 6) as u32))
      }

      // C.ADDI16SP (RES for nzimm = 0)
      0b011 if rd_is!(untyped, 2) && nzimm_is_0!(untyped) => reserved!(),
      0b011 if rd_is!(untyped, 2) => {
        // nzimm[9|4|6|8:7|5] = inst[12|6|5|4:3|2]
        let nzimm = ((inst >> 3) & 0x200) // nzimm[9]
          | ((inst >> 2) & 0x10) // nzimm[4]
          | ((inst << 1) & 0x40) // nzimm[6]
          | ((inst << 4) & 0x180) // nzimm[8:7]
          | ((inst << 3) & 0x20); // nzimm[5]
        rv32!(ADDI, Rd(Reg::X(Fin::new(2))), Rs1(Reg::X(Fin::new(2))), Imm32::from(sign_extend12(nzimm >> 4, 6) as u32))
      }

      // C.LUI (RES for imm = 0; HINT for rd = 0)
      0b011 if nzimm_is_0!(untyped) => reserved!(),
      0b011 if rd_is_0!(untyped) => hint!(),
      0b011 => {
        let ci = untyped.ci();
        let rd = Rd(gp(ci.rd()));
        // imm[17|16:12] = inst[12|6:2]
        let imm = (((inst << 5) & 0x20000) | ((inst << 10) & 0x1f000)) as u32;
        // Sign-extended.
        let imm = match (imm & 0x20000) == 0 {
          true => imm,
          false => (0xfffc0000 | imm) as i32 as u32,
        };
        rv32!(LUI, rd, Imm32::from(imm >> 12))
      }

      0b100 => match untyped.cbi().funct2() {
        // HINT for RV32/64, C.SRLI64 for RV128 (not supported)
        0b00 if nzimm_is_0!(untyped) => hint!(),

        // C.SRLI
        0b00 => {
          let cbi = untyped.cbi();
          let rd = Rd(gp_3(cbi.rd_or_rs1()));
          // shamt[5|4:0] = inst[12|6:2]
          let shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
          rv64!(SRLI, rd, Rs1(rd.0), Shamt(shamt as u8))
        }


        // HINT for RV32/64, C.SRAI64 for RV128 (not supported)
        0b01 if nzimm_is_0!(untyped) => hint!(),
        // C.SRAI
        0b01 => {
          let cbi = untyped.cbi();
          let rd = Rd(gp_3(cbi.rd_or_rs1()));
          // shamt[5|4:0] = inst[12|6:2]
          let shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
          rv64!(SRAI, rd, Rs1(rd.0), Shamt(shamt as u8))
        }

        // C.ANDI
        0b10 => {
          let cbi = untyped.cbi();
          let rd = Rd(gp_3(cbi.rd_or_rs1()));
          // imm[5|4:0] = inst[12|6:2]
          let imm = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
          rv32!(ANDI, rd, Rs1(rd.0), Imm32::from(sign_extend12(imm, 6) as u32))
        }

        0b11 => match (untyped.ca().funct6(), untyped.ca().funct2()) {
          // C.SUB
          (0b100_0_11, 00) => arith!(rv32, SUB, untyped),
          // C.XOR
          (0b100_0_11, 01) => arith!(rv32, XOR, untyped),
          // C.OR
          (0b100_0_11, 10) => arith!(rv32, OR, untyped),
          // C.AND
          (0b100_0_11, 11) => arith!(rv32, AND, untyped),
          // C.SUBW
          (0b100_1_11, 00) => arith!(rv64, SUBW, untyped),
          // C.ADDW
          (0b100_1_11, 01) => arith!(rv64, ADDW, untyped),
          // Reserved
          (0b100_1_11, 10) => reserved!(),
          // Reserved
          (0b100_1_11, 11) => reserved!(),
          _ => return None,
        },

        _ => return None,
      },

      // C.J
      0b101 => {
        // offset[11|4|9:8|10|6|7|3:1|5] = inst[12|11|10:9|8|7|6|5:3|2]
        let offset = ((inst >> 1) & 0x800) // offset[11]
          | ((inst << 2) & 0x400) // offset[10]
          | ((inst >> 1) & 0x300) // offset[9:8]
          | ((inst << 1) & 0x80) // offset[7]
          | ((inst >> 1) & 0x40) // offset[6]
          | ((inst << 3) & 0x20) // offset[5]
          | ((inst >> 7) & 0x10) // offset[4]
          | ((inst >> 2) & 0xe); // offset[3:1]
        // Sign-extended.
        let offset = match (offset & 0x800) == 0 {
          true => offset as u32,
          false => (0xf000 | offset) as i16 as i32 as u32,
        };
        rv32!(JAL, Rd(Reg::ZERO), Imm32::from(offset >> 1))
      }

      // C.BEQZ
      0b110 => {
        let cb = untyped.cb();
        let rs1 = Rs1(gp_3(cb.rd_or_rs1()));
        // offset[8|4:3|7:6|2:1|5] = inst[12|11:10|6:5|4:3|2]
        let offset = ((inst >> 4) & 0x100) // offset[8]
          | ((inst << 1) & 0xc0) // offset[7:6]
          | ((inst << 3) & 0x20) // offset[5]
          | ((inst >> 7) & 0x18) // offset[4:3]
          | ((inst >> 2) & 0x6); // offset[2:1]
        rv32!(BEQ, rs1, Rs2(Reg::ZERO), Imm32::from((sign_extend12(offset >> 1, 8) as u32) >> 1))
      }

      // C.BNEZ
      0b111 => {
        let cb = untyped.cb();
        let rs1 = Rs1(gp_3(cb.rd_or_rs1()));
        // offset[8|4:3|7:6|2:1|5] = inst[12|11:10|6:5|4:3|2]
        let offset = ((inst >> 4) & 0x100) // offset[8]
          | ((inst << 1) & 0xc0) // offset[7:6]
          | ((inst << 3) & 0x20) // offset[5]
          | ((inst >> 7) & 0x18) // offset[4:3]
          | ((inst >> 2) & 0x6); // offset[2:1]
        rv32!(BNE, rs1, Rs2(Reg::ZERO), Imm32::from((sign_extend12(offset >> 1, 8) as u32) >> 1))
      }

      _ => return None,
    },
    0b10 => match untyped.funct3() {
      // C.SLLI
      0b000 if rd_is_0!(untyped) => hint!(),
      0b000 if nzimm_is_0!(untyped) => hint!(),
      0b000 => {
        let cbi = untyped.cbi();
        let rd = Rd(gp_3(cbi.rd_or_rs1()));
        // shamt[5|4:0] = inst[12|6:2]
        let shamt = ((inst >> 7) & 0x20) | ((inst >> 2) & 0x1f);
        rv64!(SLLI, rd, Rs1(rd.0), Shamt(shamt as u8))
      }

      // C.FLDSP for RV32/64, C.LQSP for RV128 (not supported)
      0b001 => todo!("C.FLDSP"),

      // C.LWSP (RES for rd = 0)
      0b010 if rd_is_0!(untyped) => reserved!(),
      0b010 => {
        let ci = untyped.ci();
        let rd = Rd(gp(ci.rd()));
        // offset[5|4:2|7:6] = inst[12|6:4|3:2]
        let offset = ((inst << 4) & 0xc0) // offset[7:6]
          | ((inst >> 7) & 0x20) // offset[5]
          | ((inst >> 2) & 0x1c); // offset[4:2]
        rv32!(LW, rd, Rs1(Reg::X(Fin::new(2))), Imm32::from((offset >> 2) as u32))
      }

      // C.LDSP for RV64/128 (RES for rd = 0), C.FLWSP for RV32 (not supported)
      0b011 if rd_is_0!(untyped) => reserved!(),
      0b011 => {
        let ci = untyped.ci();
        let rd = Rd(gp(ci.rd()));
        // offset[5|4:3|8:6] = inst[12|6:5|4:2]
        let offset = ((inst << 4) & 0x1c0) // offset[8:6]
          | ((inst >> 7) & 0x20) // offset[5]
          | ((inst >> 2) & 0x18); // offset[4:3]
        rv64!(LD, rd, Rs1(Reg::X(Fin::new(2))), Imm32::from((offset >> 3) as u32))
      }

      // C.JR (RES for rs1/rd = 0)
      0b100 if nzimm_is_0!(untyped) && rd_is_0!(untyped) => reserved!(),
      0b100 if nzimm_is_0!(untyped) => {
        let cr = untyped.cr();
        let rs1 = Rs1(gp(cr.rd_or_rs1()));
        rv32!(JALR, Rd(Reg::ZERO), rs1, Imm32::from(0))
      }

      // C.MV
      0b100 if untyped.ci().imm_b1() == 0 && untyped.ci().imm_b5() != 0 && rd_is_0!(untyped) => hint!(),
      0b100 if untyped.ci().imm_b1() == 0 && untyped.ci().imm_b5() != 0 => {
        let cr = untyped.cr();
        let rd = Rd(gp(cr.rd_or_rs1()));
        let rs2 = Rs2(gp(cr.rs2()));
        rv32!(ADD, rd, Rs1(Reg::ZERO), rs2)
      }

      // C.EBREAK
      0b100 if untyped.repr() == 0b100_1_00000_00000_10 => rv32!(EBREAK),

      // C.JALR
      0b100 if untyped.ci().imm_b1() == 1 && untyped.ci().imm_b5() == 0 => {
        let cr = untyped.cr();
        let rs1 = Rs1(gp(cr.rd_or_rs1()));
        rv32!(JALR, Rd(Reg::X(Fin::new(1))), rs1, Imm32::from(0))
      }

      // C.ADD
      0b100 if untyped.ci().imm_b1() == 1 => {
        let cr = untyped.cr();
        let rd = Rd(gp(cr.rd_or_rs1()));
        let rs2 = Rs2(gp(cr.rs2()));
        rv32!(ADD, rd, Rs1(rd.0), rs2)
      }

      // C.FSDSP for RV32/64, C.SQSP for RV128 (not supported)
      0b101 => todo!("C.FSDSP"),

      // C.SWSP
      0b110 => {
        let css = untyped.css();
        let rs2 = Rs2(gp(css.rs2()));
        // offset[5:2|7:6] = inst[12:9|8:7]
        let offset = ((inst >> 1) & 0xc0) // offset[7:6]
          | ((inst >> 7) & 0x3c); // offset[5:2]
        rv32!(SW, Rs1(Reg::X(Fin::new(2))), rs2, Imm32::from((offset >> 2) as u32))
      }

      // C.SDSP for RV64/128, C.FSWSP for RV32 (not supported)
      0b111 => {
        let css = untyped.css();
        let rs2 = Rs2(gp(css.rs2()));
        // offset[5:3|8:6] = isnt[12:10|9:7]
        let offset = ((inst >> 1) & 0x1c0) // offset[8:6]
          | ((inst >> 7) & 0x38); // offset[5:3]
        rv64!(SD, Rs1(Reg::X(Fin::new(2))), rs2, Imm32::from((offset >> 3) as u32))
      }

      _ => return None,
    },
    _ => return None,
  };
  Some(instr)
}

fn fp_3(reg: u8) -> Reg {
  let encoding = reg as u8;
  match encoding {
    0b000 => Reg::F(Fin::new(8)),
    0b001 => Reg::F(Fin::new(9)),
    0b010 => Reg::F(Fin::new(10)),
    0b011 => Reg::F(Fin::new(11)),
    0b100 => Reg::F(Fin::new(12)),
    0b101 => Reg::F(Fin::new(13)),
    0b110 => Reg::F(Fin::new(14)),
    0b111 => Reg::F(Fin::new(15)),
    _ => panic!("Inaccessible register encoding: {:b}", encoding),
  }
}

fn gp_3(reg: u8) -> Reg {
  let encoding = reg as u8;
  match encoding {
    0b000 => Reg::X(Fin::new(8)),
    0b001 => Reg::X(Fin::new(9)),
    0b010 => Reg::X(Fin::new(10)),
    0b011 => Reg::X(Fin::new(11)),
    0b100 => Reg::X(Fin::new(12)),
    0b101 => Reg::X(Fin::new(13)),
    0b110 => Reg::X(Fin::new(14)),
    0b111 => Reg::X(Fin::new(15)),
    _ => panic!("Inaccessible register encoding: {:b}", encoding),
  }
}

#[inline(always)]
fn sign_extend12(data: u16, size: usize) -> i16 {
  ((data << (12 - size)) as i16) >> (12 - size)
}
