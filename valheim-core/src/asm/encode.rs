use crate::isa::data::Fin;
use crate::isa::rv32::RV32Instr;
use crate::isa::rv64::RV64Instr;
use crate::isa::typed::{Instr, Rd, Reg, RoundingMode, Rs1, Rs2, Rs3};
use crate::isa::typed::Reg::{X, ZERO, F, FCSR, PC};

pub trait Encode32 {
  fn encode32(self) -> u32;
}

pub trait Encode16 {
  fn encode16(self) -> u16;
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
      RV32Instr::LUI(_, _) => todo!(),
      RV32Instr::AUIPC(_, _) => todo!(),
      RV32Instr::JAL(_, _) => todo!(),
      RV32Instr::JALR(_, _, _) => todo!(),
      RV32Instr::BEQ(_, _, _) => todo!(),
      RV32Instr::BNE(_, _, _) => todo!(),
      RV32Instr::BLT(_, _, _) => todo!(),
      RV32Instr::BGE(_, _, _) => todo!(),
      RV32Instr::BLTU(_, _, _) => todo!(),
      RV32Instr::BGEU(_, _, _) => todo!(),
      RV32Instr::LB(_, _, _) => todo!(),
      RV32Instr::LH(_, _, _) => todo!(),
      RV32Instr::LW(_, _, _) => todo!(),
      RV32Instr::LBU(_, _, _) => todo!(),
      RV32Instr::LHU(_, _, _) => todo!(),
      RV32Instr::SB(_, _, _) => todo!(),
      RV32Instr::SH(_, _, _) => todo!(),
      RV32Instr::SW(_, _, _) => todo!(),
      RV32Instr::ADDI(_, _, _) => todo!(),
      RV32Instr::SLTI(_, _, _) => todo!(),
      RV32Instr::SLTIU(_, _, _) => todo!(),
      RV32Instr::XORI(_, _, _) => todo!(),
      RV32Instr::ORI(_, _, _) => todo!(),
      RV32Instr::ANDI(_, _, _) => todo!(),
      RV32Instr::SLLI(_, _, _) => todo!(),
      RV32Instr::SRLI(_, _, _) => todo!(),
      RV32Instr::SRAI(_, _, _) => todo!(),
      RV32Instr::ADD(_, _, _) => todo!(),
      RV32Instr::SUB(_, _, _) => todo!(),
      RV32Instr::SLL(_, _, _) => todo!(),
      RV32Instr::SLT(_, _, _) => todo!(),
      RV32Instr::SLTU(_, _, _) => todo!(),
      RV32Instr::XOR(_, _, _) => todo!(),
      RV32Instr::SRL(_, _, _) => todo!(),
      RV32Instr::SRA(_, _, _) => todo!(),
      RV32Instr::OR(_, _, _) => todo!(),
      RV32Instr::AND(_, _, _) => todo!(),
      RV32Instr::FENCE(_, _, _, _, _) => todo!(),
      RV32Instr::FENCE_TSO => todo!(),
      RV32Instr::PAUSE => todo!(),
      RV32Instr::ECALL => todo!(),
      RV32Instr::EBREAK => todo!(),
      // RVM
      RV32Instr::MUL(_, _, _) => todo!(),
      RV32Instr::MULH(_, _, _) => todo!(),
      RV32Instr::MULHSU(_, _, _) => todo!(),
      RV32Instr::MULHU(_, _, _) => todo!(),
      RV32Instr::DIV(_, _, _) => todo!(),
      RV32Instr::DIVU(_, _, _) => todo!(),
      RV32Instr::REM(_, _, _) => todo!(),
      RV32Instr::REMU(_, _, _) => todo!(),
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
      RV64Instr::LWU(_, _, _) => todo!(),
      RV64Instr::LD(_, _, _) => todo!(),
      RV64Instr::SD(_, _, _) => todo!(),
      RV64Instr::SLLI(_, _, _) => todo!(),
      RV64Instr::SRLI(_, _, _) => todo!(),
      RV64Instr::SRAI(_, _, _) => todo!(),
      RV64Instr::ADDIW(_, _, _) => todo!(),
      RV64Instr::SLLIW(_, _, _) => todo!(),
      RV64Instr::SRLIW(_, _, _) => todo!(),
      RV64Instr::SRAIW(_, _, _) => todo!(),
      RV64Instr::ADDW(_, _, _) => todo!(),
      RV64Instr::SUBW(_, _, _) => todo!(),
      RV64Instr::SLLW(_, _, _) => todo!(),
      RV64Instr::SRLW(_, _, _) => todo!(),
      RV64Instr::SRAW(_, _, _) => todo!(),
      RV64Instr::MULW(_, _, _) => todo!(),
      RV64Instr::DIVW(_, _, _) => todo!(),
      RV64Instr::DIVUW(_, _, _) => todo!(),
      RV64Instr::REMW(_, _, _) => todo!(),
      RV64Instr::REMUW(_, _, _) => todo!(),
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
