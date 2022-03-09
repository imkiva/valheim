use crate::isa::data::Fin;
use crate::isa::typed::{AQ, Imm32, Rd, RL, RoundingMode, Rs1, Rs2, Rs3, Shamt};

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

  // RV32F
  FLW(Rd, Rs1, Imm32<11, 0>),
  FSW(Rs1, Rs2, Imm32<11, 0>),
  FMADD_S(Rd, Rs1, Rs2, Rs3, RoundingMode),
  FMSUB_S(Rd, Rs1, Rs2, Rs3, RoundingMode),
  FNMSUB_S(Rd, Rs1, Rs2, Rs3, RoundingMode),
  FNMADD_S(Rd, Rs1, Rs2, Rs3, RoundingMode),
  FADD_S(Rd, Rs1, Rs2, RoundingMode),
  FSUB_S(Rd, Rs1, Rs2, RoundingMode),
  FMUL_S(Rd, Rs1, Rs2, RoundingMode),
  FDIV_S(Rd, Rs1, Rs2, RoundingMode),
  FSQRT_S(Rd, Rs1, RoundingMode),
  FSGNJ_S(Rd, Rs1, Rs2),
  FSGNJN_S(Rd, Rs1, Rs2),
  FSGNJX_S(Rd, Rs1, Rs2),
  FMIN_S(Rd, Rs1, Rs2),
  FMAX_S(Rd, Rs1, Rs2),
  FCVT_W_S(Rd, Rs1, RoundingMode),
  FCVT_WU_S(Rd, Rs1, RoundingMode),
  FMV_X_W(Rd, Rs1),
  FEQ_S(Rd, Rs1, Rs2),
  FLT_S(Rd, Rs1, Rs2),
  FLE_S(Rd, Rs1, Rs2),
  FCLASS_S(Rd, Rs1),
  FCVT_S_W(Rd, Rs1, RoundingMode),
  FCVT_S_WU(Rd, Rs1, RoundingMode),
  FMV_W_X(Rd, Rs1),

  // RV32D
  FLD(Rd, Rs1, Imm32<11, 0>),
  FSD(Rs1, Rs2, Imm32<11, 0>),
  FMADD_D(Rd, Rs1, Rs2, Rs3, RoundingMode),
  FMSUB_D(Rd, Rs1, Rs2, Rs3, RoundingMode),
  FNMSUB_D(Rd, Rs1, Rs2, Rs3, RoundingMode),
  FNMADD_D(Rd, Rs1, Rs2, Rs3, RoundingMode),
  FADD_D(Rd, Rs1, Rs2, RoundingMode),
  FSUB_D(Rd, Rs1, Rs2, RoundingMode),
  FMUL_D(Rd, Rs1, Rs2, RoundingMode),
  FDIV_D(Rd, Rs1, Rs2, RoundingMode),
  FSQRT_D(Rd, Rs1, RoundingMode),
  FSGNJ_D(Rd, Rs1, Rs2),
  FSGNJN_D(Rd, Rs1, Rs2),
  FSGNJX_D(Rd, Rs1, Rs2),
  FMIN_D(Rd, Rs1, Rs2),
  FMAX_D(Rd, Rs1, Rs2),
  FCVT_S_D(Rd, Rs1, RoundingMode),
  FCVT_D_S(Rd, Rs1, RoundingMode),
  FEQ_D(Rd, Rs1, Rs2),
  FLT_D(Rd, Rs1, Rs2),
  FLE_D(Rd, Rs1, Rs2),
  FCLASS_D(Rd, Rs1),
  FCVT_W_D(Rd, Rs1, RoundingMode),
  FCVT_WU_D(Rd, Rs1, RoundingMode),
  FCVT_D_W(Rd, Rs1, RoundingMode),
  FCVT_D_WU(Rd, Rs1, RoundingMode),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FenceFm(pub Fin<16>);
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FencePred(pub Fin<16>);
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct FenceSucc(pub Fin<16>);
