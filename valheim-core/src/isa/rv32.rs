use crate::isa::data::Fin;
use crate::isa::typed::{AQ, Imm32, Rd, RL, Rs1, Rs2, Shamt};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FenceFm(pub Fin<16>);
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FencePred(pub Fin<16>);
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FenceSucc(pub Fin<16>);

/// typed RV32 instructions
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[allow(non_camel_case_types)]
pub enum RV32Instr {
  // RV32I
  LUI(Rd, Imm32<31, 12>),
  AUIPC(Rd, Imm32<31, 12>),
  JAL(Rd, Imm32<20, 1>),
  JALR(Rd, Rs1, Imm32<11, 0>),
  BEQ(Rs1, Rs2, Imm32<12, 1>),
  BNE(Rs1, Rs2, Imm32<12, 1>),
  BLT(Rs1, Rs2, Imm32<12, 1>),
  BGE(Rs1, Rs2, Imm32<12, 1>),
  BLTU(Rs1, Rs2, Imm32<12, 1>),
  BGEU(Rs1, Rs2, Imm32<12, 1>),
  LB(Rd, Rs1, Imm32<11, 0>),
  LH(Rd, Rs1, Imm32<11, 0>),
  LW(Rd, Rs1, Imm32<11, 0>),
  LBU(Rd, Rs1, Imm32<11, 0>),
  LHU(Rd, Rs1, Imm32<11, 0>),
  SB(Rs1, Rs2, Imm32<11, 0>),
  SH(Rs1, Rs2, Imm32<11, 0>),
  SW(Rs1, Rs2, Imm32<11, 0>),
  ADDI(Rd, Rs1, Imm32<11, 0>),
  SLTI(Rd, Rs1, Imm32<11, 0>),
  SLTIU(Rd, Rs1, Imm32<11, 0>),
  XORI(Rd, Rs1, Imm32<11, 0>),
  ORI(Rd, Rs1, Imm32<11, 0>),
  ANDI(Rd, Rs1, Imm32<11, 0>),
  SLLI(Rd, Rs1, Shamt),
  SRLI(Rd, Rs1, Shamt),
  SRAI(Rd, Rs1, Shamt),
  ADD(Rd, Rs1, Rs2),
  SUB(Rd, Rs1, Rs2),
  SLL(Rd, Rs1, Rs2),
  SLT(Rd, Rs1, Rs2),
  SLTU(Rd, Rs1, Rs2),
  XOR(Rd, Rs1, Rs2),
  SRL(Rd, Rs1, Rs2),
  SRA(Rd, Rs1, Rs2),
  OR(Rd, Rs1, Rs2),
  AND(Rd, Rs1, Rs2),
  FENCE(Rd, Rs1, FenceFm, FencePred, FenceSucc),
  FENCE_TSO,
  PAUSE,
  ECALL,
  EBREAK,

  // RV32M
  MUL(Rd, Rs1, Rs2),
  MULH(Rd, Rs1, Rs2),
  MULHSU(Rd, Rs1, Rs2),
  MULHU(Rd, Rs1, Rs2),
  DIV(Rd, Rs1, Rs2),
  DIVU(Rd, Rs1, Rs2),
  REM(Rd, Rs1, Rs2),
  REMU(Rd, Rs1, Rs2),

  // RV32A
  LR_W(Rd, Rs1, AQ, RL),
  SC_W(Rd, Rs1, Rs2, AQ, RL),
  AMOSWAP_W(Rd, Rs1, Rs2, AQ, RL),
  AMOADD_W(Rd, Rs1, Rs2, AQ, RL),
  AMOXOR_W(Rd, Rs1, Rs2, AQ, RL),
  AMOAND_W(Rd, Rs1, Rs2, AQ, RL),
  AMOOR_W(Rd, Rs1, Rs2, AQ, RL),
  AMOMIN_W(Rd, Rs1, Rs2, AQ, RL),
  AMOMAX_W(Rd, Rs1, Rs2, AQ, RL),
  AMOMINU_W(Rd, Rs1, Rs2, AQ, RL),
  AMOMAXU_W(Rd, Rs1, Rs2, AQ, RL),
}
