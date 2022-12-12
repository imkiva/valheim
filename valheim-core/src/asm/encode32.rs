use crate::isa::data::Fin;
use crate::isa::rv32::RV32Instr;
use crate::isa::rv64::RV64Instr;
use crate::isa::typed::{Instr, Rd, Reg, RoundingMode, Rs1, Rs2, Rs3};
use crate::isa::typed::Reg::{X, ZERO, F, FCSR, PC};

pub trait Encode32 {
  fn encode32(self) -> u32;
}

impl Encode32 for Instr {
  fn encode32(self) -> u32 {
    match self {
      Instr::RV32(i) => i.encode32(),
      Instr::RV64(i) => i.encode32(),
      Instr::NOP => panic!("No nop instruction in 32-bit length encoding"),
    }
  }
}

impl Encode32 for RV32Instr {
  fn encode32(self) -> u32 {
    match self {
      // RVI
      RV32Instr::LUI(rd, imm) => emit_u_type(0b0110111, rd, imm.decode()),
      RV32Instr::AUIPC(rd, imm) => emit_u_type(0b0010111, rd, imm.decode()),
      RV32Instr::JAL(rd, imm) => emit_j_type(0b1101111, rd, imm.decode_sext()),
      RV32Instr::JALR(rd, rs1, imm) => emit_i_type(0b1100111, 0b000, rd, rs1, imm.decode_sext()),
      RV32Instr::BEQ(rs1, rs2, imm) => emit_b_type(0b1100011, 0b000, rs1, rs2, imm.decode_sext()),
      RV32Instr::BNE(rs1, rs2, imm) => emit_b_type(0b1100011, 0b001, rs1, rs2, imm.decode_sext()),
      RV32Instr::BLT(rs1, rs2, imm) => emit_b_type(0b1100011, 0b100, rs1, rs2, imm.decode_sext()),
      RV32Instr::BGE(rs1, rs2, imm) => emit_b_type(0b1100011, 0b101, rs1, rs2, imm.decode_sext()),
      RV32Instr::BLTU(rs1, rs2, imm) => emit_b_type(0b1100011, 0b110, rs1, rs2, imm.decode_sext()),
      RV32Instr::BGEU(rs1, rs2, imm) => emit_b_type(0b1100011, 0b111, rs1, rs2, imm.decode_sext()),
      RV32Instr::LB(rd, rs1, imm) => emit_i_type(0b0000011, 0b000, rd, rs1, imm.decode_sext()),
      RV32Instr::LH(rd, rs1, imm) => emit_i_type(0b0000011, 0b001, rd, rs1, imm.decode_sext()),
      RV32Instr::LW(rd, rs1, imm) => emit_i_type(0b0000011, 0b010, rd, rs1, imm.decode_sext()),
      RV32Instr::LBU(rd, rs1, imm) => emit_i_type(0b0000011, 0b100, rd, rs1, imm.decode_sext()),
      RV32Instr::LHU(rd, rs1, imm) => emit_i_type(0b0000011, 0b101, rd, rs1, imm.decode_sext()),
      RV32Instr::SB(rs1, rs2, imm) => emit_s_type(0b0100011, 0b000, rs1, rs2, imm.decode_sext()),
      RV32Instr::SH(rs1, rs2, imm) => emit_s_type(0b0100011, 0b001, rs1, rs2, imm.decode_sext()),
      RV32Instr::SW(rs1, rs2, imm) => emit_s_type(0b0100011, 0b010, rs1, rs2, imm.decode_sext()),
      RV32Instr::ADDI(rd, rs1, imm) => emit_i_type(0b0010011, 0b000, rd, rs1, imm.decode_sext()),
      RV32Instr::SLTI(rd, rs1, imm) => emit_i_type(0b0010011, 0b010, rd, rs1, imm.decode_sext()),
      RV32Instr::SLTIU(rd, rs1, imm) => emit_i_type(0b0010011, 0b011, rd, rs1, imm.decode_sext()),
      RV32Instr::XORI(rd, rs1, imm) => emit_i_type(0b0010011, 0b100, rd, rs1, imm.decode_sext()),
      RV32Instr::ORI(rd, rs1, imm) => emit_i_type(0b0010011, 0b110, rd, rs1, imm.decode_sext()),
      RV32Instr::ANDI(rd, rs1, imm) => emit_i_type(0b0010011, 0b111, rd, rs1, imm.decode_sext()),
      RV32Instr::SLLI(rd, rs1, shamt5) => emit_r_type(0b0010011, 0b0000000, 0b001, rd, rs1, Rs2(X(Fin::new(shamt5.0 as u32)))),
      RV32Instr::SRLI(rd, rs1, shamt5) => emit_r_type(0b0010011, 0b0000000, 0b101, rd, rs1, Rs2(X(Fin::new(shamt5.0 as u32)))),
      RV32Instr::SRAI(rd, rs1, shamt5) => emit_r_type(0b0010011, 0b0100000, 0b101, rd, rs1, Rs2(X(Fin::new(shamt5.0 as u32)))),
      RV32Instr::ADD(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000000, 0b000, rd, rs1, rs2),
      RV32Instr::SUB(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0100000, 0b000, rd, rs1, rs2),
      RV32Instr::SLL(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000000, 0b001, rd, rs1, rs2),
      RV32Instr::SLT(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000000, 0b010, rd, rs1, rs2),
      RV32Instr::SLTU(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000000, 0b011, rd, rs1, rs2),
      RV32Instr::XOR(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000000, 0b100, rd, rs1, rs2),
      RV32Instr::SRL(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000000, 0b101, rd, rs1, rs2),
      RV32Instr::SRA(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0100000, 0b101, rd, rs1, rs2),
      RV32Instr::OR(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000000, 0b110, rd, rs1, rs2),
      RV32Instr::AND(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000000, 0b111, rd, rs1, rs2),

      RV32Instr::FENCE(_, _, _, _, _) => todo!(),
      RV32Instr::FENCE_TSO => todo!(),
      RV32Instr::PAUSE => todo!(),
      RV32Instr::ECALL => todo!(),
      RV32Instr::EBREAK => todo!(),
      // RVM
      RV32Instr::MUL(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000001, 0b000, rd, rs1, rs2),
      RV32Instr::MULH(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000001, 0b001, rd, rs1, rs2),
      RV32Instr::MULHSU(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000001, 0b010, rd, rs1, rs2),
      RV32Instr::MULHU(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000001, 0b011, rd, rs1, rs2),
      RV32Instr::DIV(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000001, 0b100, rd, rs1, rs2),
      RV32Instr::DIVU(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000001, 0b101, rd, rs1, rs2),
      RV32Instr::REM(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000001, 0b110, rd, rs1, rs2),
      RV32Instr::REMU(rd, rs1, rs2) => emit_r_type(0b0110011, 0b0000001, 0b111, rd, rs1, rs2),
      // RVA
      RV32Instr::LR_W(_, _, _, _) => todo!(),
      RV32Instr::SC_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOSWAP_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOADD_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOXOR_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOAND_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOOR_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOMIN_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOMAX_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOMINU_W(_, _, _, _, _) => todo!(),
      RV32Instr::AMOMAXU_W(_, _, _, _, _) => todo!(),
      // RVF
      RV32Instr::FLW(rd, rs1, imm) => emit_i_type(0b0000111, 0b010, rd, rs1, imm.decode_sext()),
      RV32Instr::FSW(rs1, rs2, imm) => emit_s_type(0b0100111, 0b010, rs1, rs2, imm.decode_sext()),
      RV32Instr::FMADD_S(rd, rs1, rs2, rs3, rm) => emit_r4_type(0b1000011, 0b00, rm.encode32(), rd, rs1, rs2, rs3),
      RV32Instr::FMSUB_S(rd, rs1, rs2, rs3, rm) => emit_r4_type(0b1000111, 0b00, rm.encode32(), rd, rs1, rs2, rs3),
      RV32Instr::FNMSUB_S(rd, rs1, rs2, rs3, rm) => emit_r4_type(0b1001011, 0b00, rm.encode32(), rd, rs1, rs2, rs3),
      RV32Instr::FNMADD_S(rd, rs1, rs2, rs3, rm) => emit_r4_type(0b1001111, 0b00, rm.encode32(), rd, rs1, rs2, rs3),
      RV32Instr::FADD_S(rd, rs1, rs2, rm) => emit_r_type(0b1010011, 0b0000000, rm.encode32(), rd, rs1, rs2),
      RV32Instr::FSUB_S(rd, rs1, rs2, rm) => emit_r_type(0b1010011, 0b0000100, rm.encode32(), rd, rs1, rs2),
      RV32Instr::FMUL_S(rd, rs1, rs2, rm) => emit_r_type(0b1010011, 0b0001000, rm.encode32(), rd, rs1, rs2),
      RV32Instr::FDIV_S(rd, rs1, rs2, rm) => emit_r_type(0b1010011, 0b0001100, rm.encode32(), rd, rs1, rs2),
      RV32Instr::FSQRT_S(rd, rs1, rm) => emit_r_type(0b1010011, 0b0101100, rm.encode32(), rd, rs1, Rs2(ZERO)),
      RV32Instr::FSGNJ_S(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010000, 0b000, rd, rs1, rs2),
      RV32Instr::FSGNJN_S(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010000, 0b001, rd, rs1, rs2),
      RV32Instr::FSGNJX_S(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010000, 0b010, rd, rs1, rs2),
      RV32Instr::FMIN_S(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010100, 0b000, rd, rs1, rs2),
      RV32Instr::FMAX_S(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010100, 0b001, rd, rs1, rs2),
      RV32Instr::FCVT_W_S(rd, rs1, rm) => emit_r_type(0b1010011, 0b1100000, rm.encode32(), rd, rs1, Rs2(ZERO)),
      RV32Instr::FCVT_WU_S(rd, rs1, rm) => emit_r_type(0b1010011, 0b1100000, rm.encode32(), rd, rs1, Rs2(X(Fin::new(1)))),
      RV32Instr::FMV_X_W(rd, rs1) => emit_r_type(0b1010011, 0b1110000, 0b000, rd, rs1, Rs2(ZERO)),
      RV32Instr::FEQ_S(rd, rs1, rs2) => emit_r_type(0b1010011, 0b1010000, 0b010, rd, rs1, rs2),
      RV32Instr::FLT_S(rd, rs1, rs2) => emit_r_type(0b1010011, 0b1010000, 0b001, rd, rs1, rs2),
      RV32Instr::FLE_S(rd, rs1, rs2) => emit_r_type(0b1010011, 0b1010000, 0b000, rd, rs1, rs2),
      RV32Instr::FCLASS_S(rd, rs1) => emit_r_type(0b1010011, 0b1110000, 0b001, rd, rs1, Rs2(ZERO)),
      RV32Instr::FCVT_S_W(rd, rs1, rm) => emit_r_type(0b1010011, 0b1101000, rm.encode32(), rd, rs1, Rs2(ZERO)),
      RV32Instr::FCVT_S_WU(rd, rs1, rm) => emit_r_type(0b1010011, 0b1101000, rm.encode32(), rd, rs1, Rs2(X(Fin::new(1)))),
      RV32Instr::FMV_W_X(rd, rs1) => emit_r_type(0b1010011, 0b1111000, 0b000, rd, rs1, Rs2(ZERO)),
      // FVD
      RV32Instr::FLD(rd, rs1, imm) => emit_i_type(0b0000111, 0b011, rd, rs1, imm.decode_sext()),
      RV32Instr::FSD(rs1, rs2, imm) => emit_s_type(0b0100111, 0b011, rs1, rs2, imm.decode_sext()),
      RV32Instr::FMADD_D(rd, rs1, r2, rs3, rm) => emit_r4_type(0b1000011, 0b01, rm.encode32(), rd, rs1, r2, rs3),
      RV32Instr::FMSUB_D(rd, rs1, r2, rs3, rm) => emit_r4_type(0b1000111, 0b01, rm.encode32(), rd, rs1, r2, rs3),
      RV32Instr::FNMSUB_D(rd, rs1, r2, rs3, rm) => emit_r4_type(0b1001011, 0b01, rm.encode32(), rd, rs1, r2, rs3),
      RV32Instr::FNMADD_D(rd, rs1, r2, rs3, rm) => emit_r4_type(0b1001111, 0b01, rm.encode32(), rd, rs1, r2, rs3),
      RV32Instr::FADD_D(rd, rs1, rs2, rm) => emit_r_type(0b1010011, 0b0000001, rm.encode32(), rd, rs1, rs2),
      RV32Instr::FSUB_D(rd, rs1, rs2, rm) => emit_r_type(0b1010011, 0b0000101, rm.encode32(), rd, rs1, rs2),
      RV32Instr::FMUL_D(rd, rs1, rs2, rm) => emit_r_type(0b1010011, 0b0001001, rm.encode32(), rd, rs1, rs2),
      RV32Instr::FDIV_D(rd, rs1, rs2, rm) => emit_r_type(0b1010011, 0b0001101, rm.encode32(), rd, rs1, rs2),
      RV32Instr::FSQRT_D(rd, rs1, rm) => emit_r_type(0b1010011, 0b0101101, rm.encode32(), rd, rs1, Rs2(ZERO)),
      RV32Instr::FSGNJ_D(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010001, 0b000, rd, rs1, rs2),
      RV32Instr::FSGNJN_D(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010001, 0b001, rd, rs1, rs2),
      RV32Instr::FSGNJX_D(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010001, 0b010, rd, rs1, rs2),
      RV32Instr::FMIN_D(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010101, 0b000, rd, rs1, rs2),
      RV32Instr::FMAX_D(rd, rs1, rs2) => emit_r_type(0b1010011, 0b0010101, 0b001, rd, rs1, rs2),
      RV32Instr::FCVT_S_D(rd, rs1, rm) => emit_r_type(0b1010011, 0b0100000, rm.encode32(), rd, rs1, Rs2(X(Fin::new(1)))),
      RV32Instr::FCVT_D_S(rd, rs1, rm) => emit_r_type(0b1010011, 0b0100001, rm.encode32(), rd, rs1, Rs2(ZERO)),
      RV32Instr::FEQ_D(rd, rs1, rs2) => emit_r_type(0b1010011, 0b1010001, 0b010, rd, rs1, rs2),
      RV32Instr::FLT_D(rd, rs1, rs2) => emit_r_type(0b1010011, 0b1010001, 0b001, rd, rs1, rs2),
      RV32Instr::FLE_D(rd, rs1, rs2) => emit_r_type(0b1010011, 0b1010001, 0b000, rd, rs1, rs2),
      RV32Instr::FCLASS_D(rd, rs1) => emit_r_type(0b1010011, 0b1110001, 0b001, rd, rs1, Rs2(ZERO)),
      RV32Instr::FCVT_W_D(rd, rs1, rm) => emit_r_type(0b1010011, 0b1100001, rm.encode32(), rd, rs1, Rs2(ZERO)),
      RV32Instr::FCVT_WU_D(rd, rs1, rm) => emit_r_type(0b1010011, 0b1100001, rm.encode32(), rd, rs1, Rs2(X(Fin::new(1)))),
      RV32Instr::FCVT_D_W(rd, rs1, rm) => emit_r_type(0b1010011, 0b1101001, rm.encode32(), rd, rs1, Rs2(ZERO)),
      RV32Instr::FCVT_D_WU(rd, rs1, rm) => emit_r_type(0b1010011, 0b1101001, rm.encode32(), rd, rs1, Rs2(X(Fin::new(1)))),
    }
  }
}

impl Encode32 for RV64Instr {
  fn encode32(self) -> u32 {
    match self {
      // RV64I
      RV64Instr::LWU(rd, rs1, imm) => emit_i_type(0b0000011, 0b110, rd, rs1, imm.decode_sext()),
      RV64Instr::LD(rd, rs1, imm) => emit_i_type(0b0000011, 0b011, rd, rs1, imm.decode_sext()),
      RV64Instr::SD(rs1, rs2, imm) => emit_s_type(0b0100011, 0b011, rs1, rs2, imm.decode_sext()),
      RV64Instr::SLLI(_, _, _) => todo!(),
      RV64Instr::SRLI(_, _, _) => todo!(),
      RV64Instr::SRAI(_, _, _) => todo!(),
      RV64Instr::ADDIW(rd, rs1, imm) => emit_i_type(0b0011011, 0b000, rd, rs1, imm.decode_sext()),
      RV64Instr::SLLIW(rd, rs1, shamt5) => emit_r_type(0b0011011, 0b0000000, 0b001, rd, rs1, Rs2(X(Fin::new(shamt5.0 as u32)))),
      RV64Instr::SRLIW(rd, rs1, shamt5) => emit_r_type(0b0011011, 0b0000000, 0b101, rd, rs1, Rs2(X(Fin::new(shamt5.0 as u32)))),
      RV64Instr::SRAIW(rd, rs1, shamt5) => emit_r_type(0b0011011, 0b0100000, 0b101, rd, rs1, Rs2(X(Fin::new(shamt5.0 as u32)))),
      RV64Instr::ADDW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0000000, 0b000, rd, rs1, rs2),
      RV64Instr::SUBW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0100000, 0b000, rd, rs1, rs2),
      RV64Instr::SLLW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0000000, 0b001, rd, rs1, rs2),
      RV64Instr::SRLW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0000000, 0b101, rd, rs1, rs2),
      RV64Instr::SRAW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0100000, 0b101, rd, rs1, rs2),
      // RV64M
      RV64Instr::MULW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0000001, 0b000, rd, rs1, rs2),
      RV64Instr::DIVW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0000001, 0b100, rd, rs1, rs2),
      RV64Instr::DIVUW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0000001, 0b101, rd, rs1, rs2),
      RV64Instr::REMW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0000001, 0b110, rd, rs1, rs2),
      RV64Instr::REMUW(rd, rs1, rs2) => emit_r_type(0b0111011, 0b0000001, 0b111, rd, rs1, rs2),
      // RV64A
      RV64Instr::LR_D(_, _, _, _) => todo!(),
      RV64Instr::SC_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOSWAP_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOADD_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOXOR_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOAND_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOOR_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOMIN_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOMAX_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOMINU_D(_, _, _, _, _) => todo!(),
      RV64Instr::AMOMAXU_D(_, _, _, _, _) => todo!(),
      // RV64F & D
      RV64Instr::FCVT_L_S(_, _, _) => todo!(),
      RV64Instr::FCVT_LU_S(_, _, _) => todo!(),
      RV64Instr::FCVT_S_L(_, _, _) => todo!(),
      RV64Instr::FCVT_S_LU(_, _, _) => todo!(),
      RV64Instr::FCVT_L_D(_, _, _) => todo!(),
      RV64Instr::FCVT_LU_D(_, _, _) => todo!(),
      RV64Instr::FMV_X_D(_, _) => todo!(),
      RV64Instr::FCVT_D_L(_, _, _) => todo!(),
      RV64Instr::FCVT_D_LU(_, _, _) => todo!(),
      RV64Instr::FMV_D_X(_, _) => todo!(),
      // RV32/RV64 Zicsr
      RV64Instr::CSRRW(_, _, _) => todo!(),
      RV64Instr::CSRRS(_, _, _) => todo!(),
      RV64Instr::CSRRC(_, _, _) => todo!(),
      RV64Instr::CSRRWI(_, _, _) => todo!(),
      RV64Instr::CSRRSI(_, _, _) => todo!(),
      RV64Instr::CSRRCI(_, _, _) => todo!(),
      RV64Instr::FENCE_I(_, _, _) => todo!(),
      RV64Instr::SRET => todo!(),
      RV64Instr::MRET => todo!(),
      RV64Instr::WFI => todo!(),
      RV64Instr::SFENCE_VMA(_, _) => todo!(),
      RV64Instr::SINVAL_VMA(_, _) => todo!(),
      RV64Instr::SFENCE_W_INVAL => todo!(),
      RV64Instr::SFENCE_INVAL_IR => todo!(),
    }
  }
}

impl Encode32 for Reg {
  fn encode32(self) -> u32 {
    match self {
      ZERO => 0,
      X(x) => x.value(),
      F(f) => f.value(),
      PC => panic!("No direct pc access in RISC-V instruction encoding"),
      FCSR => panic!("No direct fcsr access in RISC-V instruction encoding"),
    }
  }
}

impl Encode32 for Rd {
  fn encode32(self) -> u32 {
    self.0.encode32()
  }
}

impl Encode32 for Rs1 {
  fn encode32(self) -> u32 {
    self.0.encode32()
  }
}

impl Encode32 for Rs2 {
  fn encode32(self) -> u32 {
    self.0.encode32()
  }
}

impl Encode32 for Rs3 {
  fn encode32(self) -> u32 {
    self.0.encode32()
  }
}

impl Encode32 for RoundingMode {
  fn encode32(self) -> u32 {
    match self {
      RoundingMode::RNE => 0b000,
      RoundingMode::RTZ => 0b001,
      RoundingMode::RDN => 0b010,
      RoundingMode::RUP => 0b011,
      RoundingMode::RMM => 0b100,
      RoundingMode::DYN => 0b111,
    }
  }
}

// Copilot wrote these `emit_*_type`, thank you for saving my life!

/// Convert RISC-V R-type instruction to u32
fn emit_r_type(opcode: u32, funct7: u32, funct3: u32, rd: Rd, rs1: Rs1, rs2: Rs2) -> u32 {
  // 31:25 = funct7, 24:20 = rs2, 19:15 = rs1, 14:12 = funct3, 11:7 = rd, 6:0 = opcode
  (opcode & 0b1111111)
    | ((rd.encode32() & 0b11111) << 7)
    | ((funct3 & 0b111) << 12)
    | ((rs1.encode32() & 0b11111) << 15)
    | ((rs2.encode32() & 0b11111) << 20)
    | ((funct7 & 0b1111111) << 25)
}

/// Convert RISC-V I-type instruction to u32
fn emit_i_type(opcode: u32, funct3: u32, rd: Rd, rs1: Rs1, imm: i32) -> u32 {
  // 31:20 = imm, 19:15 = rs1, 14:12 = funct3, 11:7 = rd, 6:0 = opcode
  (opcode & 0b1111111)
    | ((rd.encode32() & 0b11111) << 7)
    | ((funct3 & 0b111) << 12)
    | ((rs1.encode32() & 0b11111) << 15)
    | ((imm as u32 & 0b111111111111) << 20)
}

/// Convert RISC-V S-type instruction to u32
fn emit_s_type(opcode: u32, funct3: u32, rs1: Rs1, rs2: Rs2, imm: i32) -> u32 {
  // 31:25 = imm[11:5], 24:20 = rs2, 19:15 = rs1, 14:12 = funct3, 11:7 = imm[4:0], 6:0 = opcode
  (opcode & 0b1111111)
    | ((imm as u32 & 0b11111) << 7)
    | ((funct3 & 0b111) << 12)
    | ((rs1.encode32() & 0b11111) << 15)
    | ((rs2.encode32() & 0b11111) << 20)
    | ((imm as u32 & 0b111111100000) << 20)
}

/// Convert RISC-V R4-type instruction to u32
fn emit_r4_type(opcode: u32, funct2: u32, funct3: u32, rd: Rd, rs1: Rs1, rs2: Rs2, rs3: Rs3) -> u32 {
  // 31:27 = rs3, 26:25 = funct2, 24:20 = rs2, 19:15 = rs1, 14:12 = funct3, 11:7 = rd, 6:0 = opcode
  (opcode & 0b1111111)
    | ((rd.encode32() & 0b11111) << 7)
    | ((funct3 & 0b111) << 12)
    | ((rs1.encode32() & 0b11111) << 15)
    | ((rs2.encode32() & 0b11111) << 20)
    | ((funct2 & 0b11) << 25)
    | ((rs3.encode32() & 0b11111) << 27)
}

/// Convert RISC-V U-type instruction to u32
fn emit_u_type(opcode: u32, rd: Rd, imm: u32) -> u32 {
  // 31:12 = imm, 11:7 = rd, 6:0 = opcode
  (opcode & 0b1111111)
    | ((rd.encode32() & 0b11111) << 7)
    | ((imm as u32 & 0b11111111111111111111) << 12)
}

/// Convert RISC-V B-type instruction to u32
fn emit_b_type(opcode: u32, funct3: u32, rs1: Rs1, rs2: Rs2, imm: i32) -> u32 {
  // 31:31 = imm[12], 30:25 = imm[10:5], 24:20 = rs2, 19:15 = rs1, 14:12 = funct3, 11:8 = imm[4:1], 7:7 = imm[11], 6:0 = opcode
  (opcode & 0b1111111)
    | ((imm as u32 & 0b1) << 7)
    | ((imm as u32 & 0b11110) << 7)
    | ((funct3 & 0b111) << 12)
    | ((rs1.encode32() & 0b11111) << 15)
    | ((rs2.encode32() & 0b11111) << 20)
    | ((imm as u32 & 0b11111100000) << 20)
    | ((imm as u32 & 0b100000000000) << 19)
    | ((imm as u32 & 0b111111000000000000000) << 12)
}

/// Convert RISC-V J-type instruction to u32
fn emit_j_type(opcode: u32, rd: Rd, imm: i32) -> u32 {
  // 31:31 = imm[20], 30:21 = imm[10:1], 20:20 = imm[11], 19:12 = imm[19:12], 11:7 = rd, 6:0 = opcode
  (opcode & 0b1111111)
    | ((rd.encode32() & 0b11111) << 7)
    | ((imm as u32 & 0b11111111) << 12)
    | ((imm as u32 & 0b111111110000000000000) << 12)
    | ((imm as u32 & 0b100000000000000000000) << 11)
    | ((imm as u32 & 0b1000000000000000000000) << 9)
    | ((imm as u32 & 0b11111100000000000000000000000000) >> 11)
}

#[cfg(test)]
mod tests {
  use crate::asm::encode32::Encode32;
  use crate::isa::data::Fin;
  use crate::isa::rv32::RV32Instr;
  use crate::isa::typed::{Instr, Rd, RoundingMode, Rs1, Rs2, Rs3};
  use crate::isa::typed::Reg::{F, X};

  #[test]
  pub fn xlb_step2() {
    let x = vec![
      (RV32Instr::FADD_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2))), RoundingMode::DYN), "FADD_D"),
      (RV32Instr::FSUB_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2))), RoundingMode::DYN), "FSUB_D"),
      (RV32Instr::FMUL_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2))), RoundingMode::DYN), "FMUL_D"),
      (RV32Instr::FDIV_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2))), RoundingMode::DYN), "FDIV_D"),
      (RV32Instr::FSGNJ_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FSGNJ_D"),
      (RV32Instr::FSGNJN_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FSGNJN_D"),
      (RV32Instr::FSGNJX_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FSGNJX_D"),
      (RV32Instr::FSGNJX_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FSGNJX_D"),
      (RV32Instr::FMIN_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FMIN_D"),
      (RV32Instr::FMIN_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FMIN_D"),
      (RV32Instr::FMIN_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FMIN_D"),
      (RV32Instr::FMAX_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FMAX_D"),
      (RV32Instr::FMAX_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FMAX_D"),
      (RV32Instr::FMAX_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FMAX_D"),
      (RV32Instr::FEQ_D(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FEQ_D"),
      (RV32Instr::FEQ_D(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FEQ_D"),
      (RV32Instr::FEQ_D(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FEQ_D"),
      (RV32Instr::FEQ_D(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FEQ_D"),
      (RV32Instr::FLT_D(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FLT_D"),
      (RV32Instr::FLT_D(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FLT_D"),
      (RV32Instr::FLE_D(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FLE_D"),
      (RV32Instr::FLE_D(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FLE_D"),
      (RV32Instr::FMADD_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2))), Rs3(F(Fin::new(8))), RoundingMode::DYN), "FMADD_D"),
      (RV32Instr::FMSUB_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2))), Rs3(F(Fin::new(8))), RoundingMode::DYN), "FMSUB_D"),
      (RV32Instr::FNMSUB_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2))), Rs3(F(Fin::new(8))), RoundingMode::DYN), "FNMSUB_D"),
      (RV32Instr::FNMADD_D(Rd(F(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2))), Rs3(F(Fin::new(8))), RoundingMode::DYN), "FNMADD_D"),
    ];

    for (i, n) in x {
      let asm = i.encode32();
      let ii = Instr::decode32(asm).unwrap();
      assert_eq!(Instr::RV32(i), ii);
    }
  }
}
