use std::ops::BitOr;
use crate::isa::rv32::RV32Instr;
use crate::isa::rv64::RV64Instr;
use crate::isa::typed::{Imm32, Instr, Rd, Reg, Rs1, Rs2, Rs3, Shamt};
use crate::isa::rv32::{FenceFm, FenceSucc, FencePred};
use crate::isa::untyped::Bytecode;
use crate::isa::data::Fin;

impl Instr {
  pub fn from(untyped: Bytecode) -> Instr {
    decode_untyped(untyped)
  }
}

macro_rules! rv32 {
  ($ident:ident) => { Instr::RV32(RV32Instr::$ident) };
  ($ident:ident, $($t:expr),*) => { Instr::RV32(RV32Instr::$ident($( $t, )*)) };
}
macro_rules! rv64 {
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
macro_rules! i {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let i = $untyped.i();
    let rd = Rd($reg(i.rd()));
    let rs1 = Rs1($reg(i.rs1()));
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
macro_rules! r_shamt {
  ($type:ident, $opcode:ident, $untyped:expr, $reg:ident) => {{
    let r = $untyped.r_shamt();
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
    let succ = FenceSucc(Fin(f.succ() as u32));
    let pred = FencePred(Fin(f.pred() as u32));
    let fm = FenceFm(Fin(f.fm() as u32));
    $type!($opcode, rd, rs1, succ, pred, fm)
  }};
}

fn decode_untyped(untyped: Bytecode) -> Instr {
  let opcode = untyped.opcode() >> 2; // stripping away `inst[1:0]=11`
  match opcode {
    // RV32I
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
      _ => panic!("illegal branch condition {:b} in {:b}", untyped.b().funct3(), untyped.repr()),
    },
    OpcodeMap::LOAD => match untyped.i().funct3() as u8 {
      0b000 => i!(rv32, LB, untyped, gp),
      0b001 => i!(rv32, LH, untyped, gp),
      0b010 => i!(rv32, LW, untyped, gp),
      0b100 => i!(rv32, LBU, untyped, gp),
      0b101 => i!(rv32, LHU, untyped, gp),
      _ => panic!(),
    },
    OpcodeMap::STORE => match untyped.s().funct3() as u8 {
      0b000 => s!(rv32, SB, untyped, gp),
      0b001 => s!(rv32, SH, untyped, gp),
      0b010 => s!(rv32, SW, untyped, gp),
      _ => panic!(),
    }
    OpcodeMap::OP_IMM => match untyped.i().funct3() as u8 {
      0b000 => i!(rv32, ADDI, untyped, gp),
      0b010 => i!(rv32, SLTI, untyped, gp),
      0b011 => i!(rv32, SLTIU, untyped, gp),
      0b100 => i!(rv32, XORI, untyped, gp),
      0b110 => i!(rv32, ORI, untyped, gp),
      0b111 => i!(rv32, ANDI, untyped, gp),
      0b001 => r_shamt!(rv32, SLLI, untyped, gp),
      0b101 => match untyped.r_shamt().funct7() as u8 {
        0b0000000 => r_shamt!(rv32, SRLI, untyped, gp),
        0b0100000 => r_shamt!(rv32, SRAI, untyped, gp),
        _ => panic!(),
      }
      _ => panic!(),
    }
    OpcodeMap::OP => match untyped.r().funct3() as u8 {
      0b000 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, ADD, untyped, gp),
        0b0100000 => r!(rv32, SUB, untyped, gp),
        0b0000001 => r!(rv32, MUL, untyped, gp),
        _ => panic!(),
      },
      0b001 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, SLL, untyped, gp),
        0b0000001 => r!(rv32, MULH, untyped, gp),
        _ => panic!(),
      },
      0b010 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, SLT, untyped, gp),
        0b0000001 => r!(rv32, MULHSU, untyped, gp),
        _ => panic!(),
      }
      0b011 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, SLTU, untyped, gp),
        0b0000001 => r!(rv32, MULHU, untyped, gp),
        _ => panic!(),
      },
      0b100 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, XOR, untyped, gp),
        0b0000001 => r!(rv32, DIV, untyped, gp),
        _ => panic!(),
      },
      0b101 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, SRL, untyped, gp),
        0b0100000 => r!(rv32, SRA, untyped, gp),
        0b0000001 => r!(rv32, DIVU, untyped, gp),
        _ => panic!(),
      },
      0b110 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, OR, untyped, gp),
        0b0000001 => r!(rv32, REM, untyped, gp),
        _ => panic!(),
      },
      0b111 => match untyped.r().funct7() as u8 {
        0b0000000 => r!(rv32, AND, untyped, gp),
        0b0000001 => r!(rv32, REMU, untyped, gp),
        _ => panic!(),
      },
      _ => panic!(),
    }
    OpcodeMap::MISC_MEM => match untyped.repr() as u32 {
      0b1000_0011_0011_00000_000_00000_0001111 => rv32!(FENCE_TSO),
      0b0000_0001_0000_00000_000_00000_0001111 => rv32!(PAUSE),
      _fence => fence!(rv32, FENCE, untyped, gp),
    },
    OpcodeMap::SYSTEM => match untyped.i().imm11_0() as u8 {
      0b0 => rv32!(ECALL),
      0b1 => rv32!(EBREAK),
      _ => panic!(),
    },



    OpcodeMap::_custom_0 |
    OpcodeMap::_custom_1 |
    OpcodeMap::_custom_2_or_rv128 |
    OpcodeMap::_custom_3_or_rv128 |
    OpcodeMap::_reversed_0 |
    OpcodeMap::_reversed_1 |
    OpcodeMap::_reversed_2 => panic!("No custom instructions implemented yet"),
    _ => panic!("unimplemented opcode: {:b}", untyped.opcode()),
  }
}

fn fp(reg: u8) -> Reg {
  let encoding = reg as u8;
  if encoding <= 31 {
    Reg::F(Fin(encoding as u32))
  } else {
    panic!("Inaccessible register encoding: {:b}", encoding)
  }
}

fn gp(reg: u8) -> Reg {
  let encoding = reg as u8;
  if encoding == 0 {
    Reg::ZERO
  } else if encoding > 0 && encoding <= 31 {
    Reg::X(Fin(encoding as u32))
  } else {
    panic!("Inaccessible register encoding: {:b}", encoding)
  }
}

/// Opcode map, with `inst[1:0]=11` stripped away.
/// Why not use enum with repr(u8)? Because const patterns are ugly.
/// see: https://internals.rust-lang.org/t/enum-as-repr-constants-in-match-and-other-patterns/9552
// #[allow(dead_code)]
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
