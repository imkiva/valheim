use std::ops::BitOr;

use crate::isa::data::Fin;
use crate::isa::rv32::{FenceFm, FencePred, FenceSucc};
use crate::isa::rv32::RV32Instr;
use crate::isa::rv64::{CSRAddr, UImm};
use crate::isa::rv64::RV64Instr;
use crate::isa::typed::{AQ, Imm32, Instr, Rd, Reg, RL, RoundingMode, Rs1, Rs2, Rs3, Shamt};
use crate::isa::untyped::Bytecode;

impl Instr {
  pub fn try_from(untyped: Bytecode) -> Option<Instr> {
    decode_untyped(untyped)
  }

  pub fn decode32(i: u32) -> Option<Instr> {
    Instr::try_from(Bytecode { repr: i })
  }
}

#[macro_export] macro_rules! rv32 {
  ($ident:ident) => { Instr::RV32(RV32Instr::$ident) };
  ($ident:ident, $($t:expr),*) => { Instr::RV32(RV32Instr::$ident($( $t, )*)) };
}
#[macro_export] macro_rules! rv64 {
  ($ident:ident) => { Instr::RV64(RV64Instr::$ident) };
  ($ident:ident, $($t:expr),*) => { Instr::RV64(RV64Instr::$ident($( $t, )*)) };
}
macro_rules! r {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let r = $untyped.r();
    let rd = Rd($reg(r.rd()));
    let rs1 = Rs1($reg(r.rs1()));
    let rs2 = Rs2($reg(r.rs2()));
    $type!($opcode, rd, rs1, rs2)
  }};
}
macro_rules! rrm {
  ($type:ident, $opcode:ident, $untyped:expr, $rd_reg:ident, $rs_reg: ident) => {{
    let r = $untyped.r();
    let rd = Rd($rd_reg(r.rd()));
    let rs1 = Rs1($rs_reg(r.rs1()));
    let rs2 = Rs2($rs_reg(r.rs2()));
    let rm = r.funct3() as u8;
    let rm = match rm {
      const {RoundingMode::RNE as u8} => RoundingMode::RNE,
      const {RoundingMode::RTZ as u8} => RoundingMode::RTZ,
      const {RoundingMode::RDN as u8} => RoundingMode::RDN,
      const {RoundingMode::RUP as u8} => RoundingMode::RUP,
      const {RoundingMode::RMM as u8} => RoundingMode::RMM,
      const {RoundingMode::DYN as u8} => RoundingMode::DYN,
      _ => return None,
    };
    $type!($opcode, rd, rs1, rs2, rm)
  }};
}
macro_rules! rrm_no_rs2 {
  ($type:ident, $opcode:ident, $untyped:expr, $rd_reg:ident, $rs_reg: ident) => {{
    let r = $untyped.r();
    let rd = Rd($rd_reg(r.rd()));
    let rs1 = Rs1($rs_reg(r.rs1()));
    let rm = r.funct3() as u8;
    let rm = match rm {
      const {RoundingMode::RNE as u8} => RoundingMode::RNE,
      const {RoundingMode::RTZ as u8} => RoundingMode::RTZ,
      const {RoundingMode::RDN as u8} => RoundingMode::RDN,
      const {RoundingMode::RUP as u8} => RoundingMode::RUP,
      const {RoundingMode::RMM as u8} => RoundingMode::RMM,
      const {RoundingMode::DYN as u8} => RoundingMode::DYN,
      _ => return None,
    };
    $type!($opcode, rd, rs1, rm)
  }};
}
macro_rules! rrm_no_rs2_rm {
  ($type:ident, $opcode:ident, $untyped:expr, $rd_reg:ident, $rs_reg: ident) => {{
    let r = $untyped.r();
    let rd = Rd($rd_reg(r.rd()));
    let rs1 = Rs1($rs_reg(r.rs1()));
    $type!($opcode, rd, rs1)
  }};
}
macro_rules! rrm_no_rm {
  ($type:ident, $opcode:ident, $untyped:expr, $rd_reg:ident, $rs_reg: ident) => {{
    let r = $untyped.r();
    let rd = Rd($rd_reg(r.rd()));
    let rs1 = Rs1($rs_reg(r.rs1()));
    let rs2 = Rs2($rs_reg(r.rs2()));
    $type!($opcode, rd, rs1, rs2)
  }};
}
macro_rules! r_no_rd {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let r = $untyped.r();
    let rs1 = Rs1($reg(r.rs1()));
    let rs2 = Rs2($reg(r.rs2()));
    $type!($opcode, rs1, rs2)
  }};
}
macro_rules! r4 {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let r4 = $untyped.r4();
    let rd = Rd($reg(r4.rd()));
    let rs1 = Rs1($reg(r4.rs1()));
    let rs2 = Rs2($reg(r4.rs2()));
    let rs3 = Rs3($reg(r4.rs3()));
    let rm = r4.funct3() as u8;
    let rm = match rm {
      const {RoundingMode::RNE as u8} => RoundingMode::RNE,
      const {RoundingMode::RTZ as u8} => RoundingMode::RTZ,
      const {RoundingMode::RDN as u8} => RoundingMode::RDN,
      const {RoundingMode::RUP as u8} => RoundingMode::RUP,
      const {RoundingMode::RMM as u8} => RoundingMode::RMM,
      const {RoundingMode::DYN as u8} => RoundingMode::DYN,
      _ => return None,
    };
    $type!($opcode, rd, rs1, rs2, rs3, rm)
  }};
}
macro_rules! i {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let i = $untyped.i();
    let rd = Rd($reg(i.rd()));
    let rs1 = Rs1($reg(i.rs1()));
    let imm = Imm32::<11, 0>::from(i.imm11_0() as u32);
    $type!($opcode, rd, rs1, imm)
  }};
}
macro_rules! i_load_fp {
  ($type:ident, $opcode:ident, $untyped:expr, $freg:ident, $greg:ident) => {{
    let i = $untyped.i();
    let rd = Rd($freg(i.rd()));
    let rs1 = Rs1($greg(i.rs1()));
    let imm = Imm32::<11, 0>::from(i.imm11_0() as u32);
    $type!($opcode, rd, rs1, imm)
  }};
}
macro_rules! s {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let s = $untyped.s();
    let rs1 = Rs1($reg(s.rs1()));
    let rs2 = Rs2($reg(s.rs2()));
    let imm11_5 = Imm32::<11, 5>::from(s.imm11_5() as u32);
    let imm4_0 = Imm32::<4, 0>::from(s.imm4_0() as u32);
    let imm = imm11_5.bitor(imm4_0);
    $type!($opcode, rs1, rs2, imm)
  }};
}
macro_rules! s_store_fp {
  ($type:ident, $opcode:ident, $untyped:expr, $freg:ident, $greg:ident) => {{
    let s = $untyped.s();
    let rs1 = Rs1($greg(s.rs1()));
    let rs2 = Rs2($freg(s.rs2()));
    let imm11_5 = Imm32::<11, 5>::from(s.imm11_5() as u32);
    let imm4_0 = Imm32::<4, 0>::from(s.imm4_0() as u32);
    let imm = imm11_5.bitor(imm4_0);
    $type!($opcode, rs1, rs2, imm)
  }};
}
macro_rules! b {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let b = $untyped.b();
    let rs1 = Rs1($reg(b.rs1()));
    let rs2 = Rs2($reg(b.rs2()));
    let imm12 = Imm32::<12, 12>::from(b.imm12() as u32);
    let imm10_5 = Imm32::<10, 5>::from(b.imm10_5() as u32);
    let imm11 = Imm32::<11, 11>::from(b.imm11() as u32);
    let imm4_1 = Imm32::<4, 1>::from(b.imm4_1() as u32);
    let imm = imm12.bitor(imm11).bitor(imm10_5).bitor(imm4_1);
    $type!($opcode, rs1, rs2, imm)
  }};
}
macro_rules! u {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let u = $untyped.u();
    let rd = Rd($reg(u.rd()));
    let imm = Imm32::<31, 12>::from(u.imm31_12());
    $type!($opcode, rd, imm)
  }};
}
macro_rules! j {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let j = $untyped.j();
    let rd = Rd($reg(j.rd()));
    let imm20 = Imm32::<20, 20>::from(j.imm20() as u32);
    let imm10_1 = Imm32::<10, 1>::from(j.imm10_1() as u32);
    let imm11 = Imm32::<11, 11>::from(j.imm11() as u32);
    let imm19_12 = Imm32::<19, 12>::from(j.imm19_12() as u32);
    let imm = imm20.bitor(imm19_12).bitor(imm11).bitor(imm10_1);
    $type!($opcode, rd, imm)
  }};
}
macro_rules! r_shamt32 {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let r = $untyped.r_shamt32();
    let rd = Rd($reg(r.rd()));
    let rs1 = Rs1($reg(r.rs1()));
    let shamt = Shamt(r.shamt());
    $type!($opcode, rd, rs1, shamt)
  }};
}
macro_rules! r_shamt64 {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let r = $untyped.r_shamt64();
    let rd = Rd($reg(r.rd()));
    let rs1 = Rs1($reg(r.rs1()));
    let shamt = Shamt(r.shamt());
    $type!($opcode, rd, rs1, shamt)
  }};
}
macro_rules! fence {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let f = $untyped.fence();
    let rd = Rd($reg(f.rd()));
    let rs1 = Rs1($reg(f.rs1()));
    let succ = FenceSucc(f.succ() as u32);
    let pred = FencePred(f.pred() as u32);
    let fm = FenceFm(f.fm() as u32);
    $type!($opcode, rd, rs1, succ, pred, fm)
  }};
}
macro_rules! zicsr_rs1 {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let i = $untyped.i();
    let rd = Rd($reg(i.rd()));
    let rs1 = Rs1($reg(i.rs1()));
    let imm = Imm32::<11, 0>::from(i.imm11_0() as u32);
    let csr = CSRAddr(imm);
    $type!($opcode, rd, rs1, csr)
  }};
}
macro_rules! zicsr_uimm {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let i = $untyped.i();
    let rd = Rd($reg(i.rd()));
    let rs1 = Imm32::<4, 0>::from(i.rs1() as u32);
    let uimm = UImm(rs1);
    let imm = Imm32::<11, 0>::from(i.imm11_0() as u32);
    let csr = CSRAddr(imm);
    $type!($opcode, rd, uimm, csr)
  }};
}
macro_rules! ra {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let r = $untyped.ra();
    let rd = Rd($reg(r.rd()));
    let rs1 = Rs1($reg(r.rs1()));
    let rs2 = Rs2($reg(r.rs2()));
    let aq = AQ(r.aq());
    let rl = RL(r.rl());
    $type!($opcode, rd, rs1, rs2, aq, rl)
  }};
}
macro_rules! ra_only_rs1 {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let r = $untyped.ra();
    let rd = Rd($reg(r.rd()));
    let rs1 = Rs1($reg(r.rs1()));
    assert_eq!(r.rs2() as u8, 0);
    let aq = AQ(r.aq());
    let rl = RL(r.rl());
    $type!($opcode, rd, rs1, aq, rl)
  }};
}

fn decode_untyped(untyped: Bytecode) -> Option<Instr> {
  let opcode = untyped.opcode() >> 2; // stripping away `inst[1:0]=11`
  let instr = match opcode {
    OpcodeMap::LUI => u!(rv32, LUI, untyped, gp),
    OpcodeMap::AUIPC => u!(rv32, AUIPC, untyped, gp),
    OpcodeMap::JAL => j!(rv32, JAL, untyped, gp),
    OpcodeMap::JALR => i!(rv32, JALR, untyped, gp),
    OpcodeMap::BRANCH => match untyped.b().funct3() as u8 {
      0b000 => b!(rv32, BEQ, untyped, gp),
      0b001 => b!(rv32, BNE, untyped, gp),
      0b100 => b!(rv32, BLT, untyped, gp),
      0b101 => b!(rv32, BGE, untyped, gp),
      0b110 => b!(rv32, BLTU, untyped, gp),
      0b111 => b!(rv32, BGEU, untyped, gp),
      _ => return None,
    },
    OpcodeMap::LOAD => match untyped.i().funct3() as u8 {
      0b000 => i!(rv32, LB, untyped, gp),
      0b001 => i!(rv32, LH, untyped, gp),
      0b010 => i!(rv32, LW, untyped, gp),
      0b100 => i!(rv32, LBU, untyped, gp),
      0b101 => i!(rv32, LHU, untyped, gp),
      0b110 => i!(rv64, LWU, untyped, gp),
      0b011 => i!(rv64, LD, untyped, gp),
      _ => return None,
    },
    OpcodeMap::STORE => match untyped.s().funct3() as u8 {
      0b000 => s!(rv32, SB, untyped, gp),
      0b001 => s!(rv32, SH, untyped, gp),
      0b010 => s!(rv32, SW, untyped, gp),
      0b011 => s!(rv64, SD, untyped, gp),
      _ => return None,
    }
    OpcodeMap::OP_IMM => match untyped.i().funct3() as u8 {
      0b000 => i!(rv32, ADDI, untyped, gp),
      0b010 => i!(rv32, SLTI, untyped, gp),
      0b011 => i!(rv32, SLTIU, untyped, gp),
      0b100 => i!(rv32, XORI, untyped, gp),
      0b110 => i!(rv32, ORI, untyped, gp),
      0b111 => i!(rv32, ANDI, untyped, gp),
      // RV64's SLLI, SRLI, SRAI have a 1-bit-more `shamt` field compared to RV32:
      // The `shamt` field in RV32: 5 bits
      // The `shamt` field in RV64: 6 bits
      0b001 => r_shamt64!(rv64, SLLI, untyped, gp),
      0b101 => match untyped.r_shamt64().funct6() as u8 {
        0b000000 => r_shamt64!(rv64, SRLI, untyped, gp),
        0b010000 => r_shamt64!(rv64, SRAI, untyped, gp),
        _ => return None,
      }
      _ => return None,
    }
    OpcodeMap::OP_IMM_32 => match untyped.i().funct3() as u8 {
      0b000 => i!(rv64, ADDIW, untyped, gp),
      0b001 => r_shamt32!(rv64, SLLIW, untyped, gp),
      0b101 => match untyped.r_shamt32().funct7() as u8 {
        0b0000000 => r_shamt32!(rv64, SRLIW, untyped, gp),
        0b0100000 => r_shamt32!(rv64, SRAIW, untyped, gp),
        _ => return None,
      }
      _ => return None,
    },
    OpcodeMap::OP => match untyped.r().funct3() as u8 {
      0b000 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, ADD, untyped, gp),
        0b0100000 => r!(rv32, SUB, untyped, gp),
        0b0000001 => r!(rv32, MUL, untyped, gp),
        _ => return None,
      },
      0b001 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, SLL, untyped, gp),
        0b0000001 => r!(rv32, MULH, untyped, gp),
        _ => return None,
      },
      0b010 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, SLT, untyped, gp),
        0b0000001 => r!(rv32, MULHSU, untyped, gp),
        _ => return None,
      }
      0b011 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, SLTU, untyped, gp),
        0b0000001 => r!(rv32, MULHU, untyped, gp),
        _ => return None,
      },
      0b100 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, XOR, untyped, gp),
        0b0000001 => r!(rv32, DIV, untyped, gp),
        _ => return None,
      },
      0b101 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, SRL, untyped, gp),
        0b0100000 => r!(rv32, SRA, untyped, gp),
        0b0000001 => r!(rv32, DIVU, untyped, gp),
        _ => return None,
      },
      0b110 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, OR, untyped, gp),
        0b0000001 => r!(rv32, REM, untyped, gp),
        _ => return None,
      },
      0b111 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, AND, untyped, gp),
        0b0000001 => r!(rv32, REMU, untyped, gp),
        _ => return None,
      },
      _ => return None,
    }
    OpcodeMap::OP_32 => match untyped.r().funct3() as u8 {
      0b000 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv64, ADDW, untyped, gp),
        0b0100000 => r!(rv64, SUBW, untyped, gp),
        0b0000001 => r!(rv64, MULW, untyped, gp),
        _ => return None,
      },
      0b001 => r!(rv64, SLLW, untyped, gp),
      0b100 => r!(rv64, DIVW, untyped, gp),
      0b101 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv64, SRLW, untyped, gp),
        0b0100000 => r!(rv64, SRAW, untyped, gp),
        0b0000001 => r!(rv64, DIVUW, untyped, gp),
        _ => return None,
      },
      0b110 => r!(rv64, REMW, untyped, gp),
      0b111 => r!(rv64, REMUW, untyped, gp),
      _ => return None,
    }
    OpcodeMap::MISC_MEM => match untyped.i().funct3() as u8 {
      0b000 => match untyped.repr() as u32 {
        0b1000_0011_0011_00000_000_00000_0001111 => rv32!(FENCE_TSO),
        0b0000_0001_0000_00000_000_00000_0001111 => rv32!(PAUSE),
        _fence => fence!(rv32, FENCE, untyped, gp),
      },
      0b001 => i!(rv64, FENCE_I, untyped, gp),
      _ => return None,
    },
    OpcodeMap::SYSTEM => match untyped.i().funct3() as u8 {
      0b000 => match untyped.i().imm11_0() as u8 {
        0b0 => rv32!(ECALL),
        0b1 => rv32!(EBREAK),
        _ => match untyped.r().funct7() as u8 {
          0b0001000 => match untyped.r().rs2() as u8 {
            0b00010 => rv64!(SRET),
            0b00101 => rv64!(WFI),
            _ => return None,
          }
          0b0011000 => rv64!(MRET),
          0b0001001 => r_no_rd!(rv64, SFENCE_VMA, untyped, gp),
          0b0001011 => r_no_rd!(rv64, SINVAL_VMA, untyped, gp),
          0b0001100 => match untyped.r().rs2() as u8 {
            0b0 => rv64!(SFENCE_W_INVAL),
            0b1 => rv64!(SFENCE_INVAL_IR),
            _ => return None,
          }
          _ => todo!("Hypervisor ISA")
        }
      },
      0b001 => zicsr_rs1!(rv64, CSRRW, untyped, gp),
      0b010 => zicsr_rs1!(rv64, CSRRS, untyped, gp),
      0b011 => zicsr_rs1!(rv64, CSRRC, untyped, gp),
      0b101 => zicsr_uimm!(rv64, CSRRWI, untyped, gp),
      0b110 => zicsr_uimm!(rv64, CSRRSI, untyped, gp),
      0b111 => zicsr_uimm!(rv64, CSRRCI, untyped, gp),
      _ => return None,
    },
    OpcodeMap::AMO => match untyped.ra().funct3() as u8 {
      0b010 => match untyped.ra().funct5() as u8 {
        0b00010 => ra_only_rs1!(rv32, LR_W, untyped, gp),
        0b00011 => ra!(rv32, SC_W, untyped, gp),
        0b00001 => ra!(rv32, AMOSWAP_W, untyped, gp),
        0b00000 => ra!(rv32, AMOADD_W, untyped, gp),
        0b00100 => ra!(rv32, AMOXOR_W, untyped, gp),
        0b01100 => ra!(rv32, AMOAND_W, untyped, gp),
        0b01000 => ra!(rv32, AMOOR_W, untyped, gp),
        0b10000 => ra!(rv32, AMOMIN_W, untyped, gp),
        0b10100 => ra!(rv32, AMOMAX_W, untyped, gp),
        0b11000 => ra!(rv32, AMOMINU_W, untyped, gp),
        0b11100 => ra!(rv32, AMOMAXU_W, untyped, gp),
        _ => return None,
      },
      0b011 => match untyped.ra().funct5() as u8 {
        0b00010 => ra_only_rs1!(rv64, LR_D, untyped, gp),
        0b00011 => ra!(rv64, SC_D, untyped, gp),
        0b00001 => ra!(rv64, AMOSWAP_D, untyped, gp),
        0b00000 => ra!(rv64, AMOADD_D, untyped, gp),
        0b00100 => ra!(rv64, AMOXOR_D, untyped, gp),
        0b01100 => ra!(rv64, AMOAND_D, untyped, gp),
        0b01000 => ra!(rv64, AMOOR_D, untyped, gp),
        0b10000 => ra!(rv64, AMOMIN_D, untyped, gp),
        0b10100 => ra!(rv64, AMOMAX_D, untyped, gp),
        0b11000 => ra!(rv64, AMOMINU_D, untyped, gp),
        0b11100 => ra!(rv64, AMOMAXU_D, untyped, gp),
        _ => return None,
      },
      _ => return None,
    }

    // RV32/64 FD
    OpcodeMap::LOAD_FP => match untyped.i().funct3() as u8 {
      0b010 => i_load_fp!(rv32, FLW, untyped, fp, gp),
      0b011 => i_load_fp!(rv32, FLD, untyped, fp, gp),
      _ => return None,
    }
    OpcodeMap::STORE_FP => match untyped.s().funct3() as u8 {
      0b010 => s_store_fp!(rv32, FSW, untyped, fp, gp),
      0b011 => s_store_fp!(rv32, FSD, untyped, fp, gp),
      _ => return None,
    }
    OpcodeMap::MADD => match untyped.r4().funct2() as u8 {
      0b00 => r4!(rv32, FMADD_S, untyped, fp),
      0b01 => r4!(rv32, FMADD_D, untyped, fp),
      _ => return None,
    }
    OpcodeMap::MSUB => match untyped.r4().funct2() as u8 {
      0b00 => r4!(rv32, FMSUB_S, untyped, fp),
      0b01 => r4!(rv32, FMSUB_D, untyped, fp),
      _ => return None,
    }
    OpcodeMap::NMADD => match untyped.r4().funct2() as u8 {
      0b00 => r4!(rv32, FNMADD_S, untyped, fp),
      0b01 => r4!(rv32, FNMADD_D, untyped, fp),
      _ => return None,
    }
    OpcodeMap::NMSUB => match untyped.r4().funct2() as u8 {
      0b00 => r4!(rv32, FNMSUB_S, untyped, fp),
      0b01 => r4!(rv32, FNMSUB_D, untyped, fp),
      _ => return None,
    }
    OpcodeMap::OP_FP => match untyped.r().funct7() as u8 {
      0b0000000 => rrm!(rv32, FADD_S, untyped, fp, fp),
      0b0000001 => rrm!(rv32, FADD_D, untyped, fp, fp),
      0b0000100 => rrm!(rv32, FSUB_S, untyped, fp, fp),
      0b0000101 => rrm!(rv32, FSUB_D, untyped, fp, fp),
      0b0001000 => rrm!(rv32, FMUL_S, untyped, fp, fp),
      0b0001001 => rrm!(rv32, FMUL_D, untyped, fp, fp),
      0b0001100 => rrm!(rv32, FDIV_S, untyped, fp, fp),
      0b0001101 => rrm!(rv32, FDIV_D, untyped, fp, fp),
      0b0101100 => rrm_no_rs2!(rv32, FSQRT_S, untyped, fp, fp),
      0b0101101 => rrm_no_rs2!(rv32, FSQRT_D, untyped, fp, fp),
      0b0010000 => match untyped.r().funct3() as u8 {
        0b000 => rrm_no_rm!(rv32, FSGNJ_S, untyped, fp, fp),
        0b001 => rrm_no_rm!(rv32, FSGNJN_S, untyped, fp, fp),
        0b010 => rrm_no_rm!(rv32, FSGNJX_S, untyped, fp, fp),
        _ => return None,
      }
      0b0010001 => match untyped.r().funct3() as u8 {
        0b000 => rrm_no_rm!(rv32, FSGNJ_D, untyped, fp, fp),
        0b001 => rrm_no_rm!(rv32, FSGNJN_D, untyped, fp, fp),
        0b010 => rrm_no_rm!(rv32, FSGNJX_D, untyped, fp, fp),
        _ => return None,
      }
      0b0010100 => match untyped.r().funct3() as u8 {
        0b000 => rrm_no_rm!(rv32, FMIN_S, untyped, fp, fp),
        0b001 => rrm_no_rm!(rv32, FMAX_S, untyped, fp, fp),
        _ => return None,
      }
      0b0010101 => match untyped.r().funct3() as u8 {
        0b000 => rrm_no_rm!(rv32, FMIN_D, untyped, fp, fp),
        0b001 => rrm_no_rm!(rv32, FMAX_D, untyped, fp, fp),
        _ => return None,
      }
      0b1100000 => match untyped.r().rs2() as u8 {
        0b00000 => rrm_no_rs2!(rv32, FCVT_W_S, untyped, gp, fp),
        0b00001 => rrm_no_rs2!(rv32, FCVT_WU_S, untyped, gp, fp),
        0b00010 => rrm_no_rs2!(rv64, FCVT_L_S, untyped, gp, fp),
        0b00011 => rrm_no_rs2!(rv64, FCVT_LU_S, untyped, gp, fp),
        _ => return None,
      }
      0b1110000 => match untyped.r().rs2() as u8 {
        0b00000 => match untyped.r().funct3() as u8 {
          0b000 => rrm_no_rs2_rm!(rv32, FMV_X_W, untyped, gp, fp),
          0b001 => rrm_no_rs2_rm!(rv32, FCLASS_S, untyped, gp, fp),
          _ => return None,
        }
        _ => return None,
      },
      0b0100000 => match untyped.r().rs2() as u8 {
        0b00001 => rrm_no_rs2!(rv32, FCVT_S_D, untyped, fp, fp),
        _ => return None,
      },
      0b0100001 => match untyped.r().rs2() as u8 {
        0b00000 => rrm_no_rs2!(rv32, FCVT_D_S, untyped, fp, fp),
        _ => return None,
      },
      0b1010000 => match untyped.r().funct3() as u8 {
        0b010 => rrm_no_rm!(rv32, FEQ_S, untyped, gp, fp),
        0b001 => rrm_no_rm!(rv32, FLT_S, untyped, gp, fp),
        0b000 => rrm_no_rm!(rv32, FLE_S, untyped, gp, fp),
        _ => return None,
      },
      0b1010001 => match untyped.r().funct3() as u8 {
        0b010 => rrm_no_rm!(rv32, FEQ_D, untyped, gp, fp),
        0b001 => rrm_no_rm!(rv32, FLT_D, untyped, gp, fp),
        0b000 => rrm_no_rm!(rv32, FLE_D, untyped, gp, fp),
        _ => return None,
      },
      0b1101000 => match untyped.r().rs2() as u8 {
        0b00000 => rrm_no_rs2!(rv32, FCVT_S_W, untyped, fp, gp),
        0b00001 => rrm_no_rs2!(rv32, FCVT_S_WU, untyped, fp, gp),
        0b00010 => rrm_no_rs2!(rv64, FCVT_S_L, untyped, fp, gp),
        0b00011 => rrm_no_rs2!(rv64, FCVT_S_LU, untyped, fp, gp),
        _ => return None,
      }
      0b1111000 => match (untyped.r().rs2() as u8, untyped.r().funct3() as u8) {
        (0b00000, 0b000) => rrm_no_rs2_rm!(rv32, FMV_W_X, untyped, fp, gp),
        _ => return None,
      },
      0b1111001 => match (untyped.r().rs2() as u8, untyped.r().funct3() as u8) {
        (0b00000, 0b000) => rrm_no_rs2_rm!(rv64, FMV_D_X, untyped, fp, gp),
        _ => return None,
      },
      0b1110001 => match (untyped.r().rs2() as u8, untyped.r().funct3() as u8) {
        (0b00000, 0b000) => rrm_no_rs2_rm!(rv64, FMV_X_D, untyped, gp, fp),
        (0b00000, 0b001) => rrm_no_rs2_rm!(rv32, FCLASS_D, untyped, gp, fp),
        _ => return None,
      },
      0b1100001 => match untyped.r().rs2() as u8 {
        0b00000 => rrm_no_rs2!(rv32, FCVT_W_D, untyped, gp, fp),
        0b00001 => rrm_no_rs2!(rv32, FCVT_WU_D, untyped, gp, fp),
        0b00010 => rrm_no_rs2!(rv64, FCVT_L_D, untyped, gp, fp),
        0b00011 => rrm_no_rs2!(rv64, FCVT_LU_D, untyped, gp, fp),
        _ => return None,
      }
      0b1101001 => match untyped.r().rs2() as u8 {
        0b00000 => rrm_no_rs2!(rv32, FCVT_D_W, untyped, fp, gp),
        0b00001 => rrm_no_rs2!(rv32, FCVT_D_WU, untyped, fp, gp),
        0b00010 => rrm_no_rs2!(rv64, FCVT_D_L, untyped, fp, gp),
        0b00011 => rrm_no_rs2!(rv64, FCVT_D_LU, untyped, fp, gp),
        _ => return None,
      }
      _ => return None,
    }

    OpcodeMap::_custom_0 |
    OpcodeMap::_custom_1 |
    OpcodeMap::_custom_2_or_rv128 |
    OpcodeMap::_custom_3_or_rv128 |
    OpcodeMap::_reversed_0 |
    OpcodeMap::_reversed_1 |
    OpcodeMap::_reversed_2 => return None,
    _ => return None,
  };
  Some(instr)
}

pub fn fp(reg: u8) -> Reg {
  let encoding = reg as u8;
  if encoding <= 31 {
    Reg::F(Fin::new(encoding as u32))
  } else {
    panic!("Inaccessible register encoding: {:b}", encoding)
  }
}

pub fn gp(reg: u8) -> Reg {
  let encoding = reg as u8;
  if encoding == 0 {
    Reg::ZERO
  } else if encoding > 0 && encoding <= 31 {
    Reg::X(Fin::new(encoding as u32))
  } else {
    panic!("Inaccessible register encoding: {:b}", encoding)
  }
}

/// Opcode map, with `inst[1:0]=11` stripped away.
/// Why not use enum with repr(u8)? Because const patterns are ugly.
/// see: https://internals.rust-lang.org/t/enum-as-repr-constants-in-match-and-other-patterns/9552
#[allow(non_snake_case)]
#[allow(non_upper_case_globals)]
mod OpcodeMap {
  pub const LOAD: u8 = 0b00000;
  pub const LOAD_FP: u8 = 0b00001;
  pub const _custom_0: u8 = 0b00010;
  pub const MISC_MEM: u8 = 0b00011;
  pub const OP_IMM: u8 = 0b00100;
  pub const AUIPC: u8 = 0b00101;
  pub const OP_IMM_32: u8 = 0b00110;

  pub const STORE: u8 = 0b01000;
  pub const STORE_FP: u8 = 0b01001;
  pub const _custom_1: u8 = 0b01010;
  pub const AMO: u8 = 0b01011;
  pub const OP: u8 = 0b01100;
  pub const LUI: u8 = 0b01101;
  pub const OP_32: u8 = 0b01110;

  pub const MADD: u8 = 0b10000;
  pub const MSUB: u8 = 0b10001;
  pub const NMSUB: u8 = 0b10010;
  pub const NMADD: u8 = 0b10011;
  pub const OP_FP: u8 = 0b10100;
  pub const _reversed_0: u8 = 0b10101;
  pub const _custom_2_or_rv128: u8 = 0b10110;

  pub const BRANCH: u8 = 0b11000;
  pub const JALR: u8 = 0b11001;
  pub const _reversed_1: u8 = 0b11010;
  pub const JAL: u8 = 0b11011;
  pub const SYSTEM: u8 = 0b11100;
  pub const _reversed_2: u8 = 0b11101;
  pub const _custom_3_or_rv128: u8 = 0b11110;
}

#[cfg(test)]
mod tests {
  use crate::isa::typed::Instr;
  use crate::isa::untyped::Bytecode;

  #[test]
  fn test_instr_decode() {
    // jal x0, -6*4
    let instr_asm: u32 = 0b_1_1111110100_1_11111111_00000_1101111;
    let bytecode = Bytecode { repr: instr_asm };
    let instr = Instr::try_from(bytecode).unwrap();
    println!("{:?}", instr);
  }
}
