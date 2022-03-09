use crate::isa::typed::{AQ, Imm32, Rd, RL, RoundingMode, Rs1, Rs2, Shamt};

/// typed RV64 instructions
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[allow(non_camel_case_types)]
pub enum RV64Instr {
  // RV64I
  LWU(Rd, Rs1, Imm32<11, 0>),
  LD(Rd, Rs1, Imm32<11, 0>),
  SD(Rs1, Rs2, Imm32<11, 0>),
  SLLI(Rd, Rs1, Shamt),
  SRLI(Rd, Rs1, Shamt),
  SRAI(Rd, Rs1, Shamt),
  ADDIW(Rd, Rs1, Imm32<11, 0>),
  SLLIW(Rd, Rs1, Shamt),
  SRLIW(Rd, Rs1, Shamt),
  SRAIW(Rd, Rs1, Shamt),
  ADDW(Rd, Rs1, Rs2),
  SUBW(Rd, Rs1, Rs2),
  SLLW(Rd, Rs1, Rs2),
  SRLW(Rd, Rs1, Rs2),
  SRAW(Rd, Rs1, Rs2),

  // RV64M
  MULW(Rd, Rs1, Rs2),
  DIVW(Rd, Rs1, Rs2),
  DIVUW(Rd, Rs1, Rs2),
  REMW(Rd, Rs1, Rs2),
  REMUW(Rd, Rs1, Rs2),

  // RV64A
  LR_D(Rd, Rs1, AQ, RL),
  SC_D(Rd, Rs1, Rs2, AQ, RL),
  AMOSWAP_D(Rd, Rs1, Rs2, AQ, RL),
  AMOADD_D(Rd, Rs1, Rs2, AQ, RL),
  AMOXOR_D(Rd, Rs1, Rs2, AQ, RL),
  AMOAND_D(Rd, Rs1, Rs2, AQ, RL),
  AMOOR_D(Rd, Rs1, Rs2, AQ, RL),
  AMOMIN_D(Rd, Rs1, Rs2, AQ, RL),
  AMOMAX_D(Rd, Rs1, Rs2, AQ, RL),
  AMOMINU_D(Rd, Rs1, Rs2, AQ, RL),
  AMOMAXU_D(Rd, Rs1, Rs2, AQ, RL),

  // RV64F
  FCVT_L_S(Rd, Rs1, RoundingMode),
  FCVT_LU_S(Rd, Rs1, RoundingMode),
  FCVT_S_L(Rd, Rs1, RoundingMode),
  FCVT_S_LU(Rd, Rs1, RoundingMode),

  // RV64D
  FCVT_L_D(Rd, Rs1, RoundingMode),
  FCVT_LU_D(Rd, Rs1, RoundingMode),
  FMV_X_D(Rd, Rs1),
  FCVT_D_L(Rd, Rs1, RoundingMode),
  FCVT_D_LU(Rd, Rs1, RoundingMode),
  FMV_D_X(Rd, Rs1),
}
