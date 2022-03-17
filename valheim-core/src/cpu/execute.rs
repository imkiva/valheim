use crate::cpu::data::Either;
use crate::cpu::exception::Exception;
use crate::cpu::RV64Cpu;
use crate::debug::trace::{InstrTrace, Trace};
use crate::isa::compressed::untyped::Bytecode16;
use crate::isa::rv32::RV32Instr;
use crate::isa::typed::{Imm32, Instr, Rd, Reg, Rs1, Rs2, Rs3};
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;
use crate::isa::rv32::RV32Instr::*;
use crate::isa::rv64::RV64Instr;
use crate::isa::rv64::RV64Instr::*;
use crate::isa::typed::Instr::{RV32, RV64};

impl RV64Cpu {
  pub fn fetch(&mut self) -> Result<(VirtAddr, Bytecode, Bytecode16), Exception> {
    let pc = self.read_pc();
    let bytecode: Bytecode = self.read_mem(pc)?;
    let compressed: Bytecode16 = self.read_mem(pc)?;
    self.journal.trace(|| Trace::Instr(InstrTrace::Fetched(pc, bytecode, compressed)));
    Ok((pc, bytecode, compressed))
  }

  pub fn decode(&mut self, pc: VirtAddr, untyped: Bytecode, compressed: Bytecode16) -> Result<(Either<Bytecode, Bytecode16>, Instr), Exception> {
    match Instr::try_from_compressed(compressed) {
      Some(instr) => {
        self.journal.trace(|| Trace::Instr(InstrTrace::DecodedCompressed(pc, compressed, instr)));
        Ok((Either::Right(compressed), instr))
      },
      None => {
        Instr::try_from(untyped).map(|instr| {
          self.journal.trace(|| Trace::Instr(InstrTrace::Decoded(pc, untyped, instr)));
          (Either::Left(untyped), instr)
        }).ok_or(Exception::IllegalInstruction(pc, untyped, compressed))
      },
    }
  }

  pub fn execute(&mut self, pc: VirtAddr, instr: Instr, is_compressed: bool) -> Result<(), Exception> {
    let delta = match is_compressed {
      true => std::mem::size_of::<Bytecode16>() as u64,
      false => std::mem::size_of::<Bytecode>() as u64,
    };
    let mut next_pc = VirtAddr(pc.0.wrapping_add(delta));

    self.journal.trace(|| Trace::Instr(InstrTrace::PrepareExecute(pc, instr)));
    match instr {
      Instr::NOP => (),
      // nop is also encoded as `ADDI x0, x0, 0`
      RV32(ADDI(Rd(Reg::ZERO), Rs1(Reg::ZERO), Imm32(0))) => (),
      RV32(LUI(rd, imm)) => rd.write(self, imm.decode() as i32 as i64 as u64),
      RV32(AUIPC(rd, offset)) => rd.write(self, pc.0.wrapping_add(offset.decode() as i32 as i64 as u64)),
      RV32(JAL(rd, offset)) => {
        rd.write(self, next_pc.0);
        next_pc = VirtAddr(pc.0.wrapping_add(offset.decode_sext() as u64));
      }
      RV32(JALR(rd, rs1, imm)) => {
        let offset = imm.decode_sext() as i64;
        let target = (rs1.read(self) as i64).wrapping_add(offset) & !1;
        rd.write(self, next_pc.0);
        next_pc = VirtAddr(target as u64);
      }
      RV32(BEQ(rs1, rs2, offset)) |
      RV32(BNE(rs1, rs2, offset)) |
      RV32(BLT(rs1, rs2, offset)) |
      RV32(BGE(rs1, rs2, offset)) |
      RV32(BLTU(rs1, rs2, offset)) |
      RV32(BGEU(rs1, rs2, offset)) => {
        let rs1 = rs1.read(self);
        let rs2 = rs2.read(self);
        let compare = match instr {
          RV32(BEQ(_, _, _)) => rs1 == rs2,
          RV32(BNE(_, _, _)) => rs1 != rs2,
          RV32(BLT(_, _, _)) => (rs1 as i64) < (rs2 as i64),
          RV32(BGE(_, _, _)) => (rs1 as i64) >= (rs2 as i64),
          RV32(BLTU(_, _, _)) => rs1 < rs2,
          RV32(BGEU(_, _, _)) => rs1 >= rs2,
          _ => unreachable!()
        };
        if compare {
          next_pc = VirtAddr(pc.0.wrapping_add(offset.decode_sext() as u64));
        }
      }

      RV64(LD(rd, rs1, offset)) => rd.write(self, self.read_mem::<u64>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))?),
      RV32(LW(rd, rs1, offset)) => rd.write(self, self.read_mem::<u32>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as i32 as i64 as u64),
      RV32(LB(rd, rs1, offset)) => rd.write(self, self.read_mem::<u8>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as i8 as i32 as i64 as u64),
      RV32(LH(rd, rs1, offset)) => rd.write(self, self.read_mem::<u16>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as i16 as i32 as i64 as u64),
      RV64(LWU(rd, rs1, offset)) => rd.write(self, self.read_mem::<u32>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as u64),
      RV32(LBU(rd, rs1, offset)) => rd.write(self, self.read_mem::<u8>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as u64),
      RV32(LHU(rd, rs1, offset)) => rd.write(self, self.read_mem::<u16>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as u64),

      RV32(SB(rs1, rs2, offset)) => self.write_mem(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)), rs2.read(self) as u8)?,
      RV32(SH(rs1, rs2, offset)) => self.write_mem(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)), rs2.read(self) as u16)?,
      RV32(SW(rs1, rs2, offset)) => self.write_mem(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)), rs2.read(self) as u32)?,
      RV64(SD(rs1, rs2, offset)) => self.write_mem(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)), rs2.read(self) as u64)?,

      RV32(ADDI(rd, rs1, Imm32(0))) => rd.write(self, rs1.read(self)),
      // sext.w is encoded as `addiw rd, rs1, 0`
      RV64(ADDIW(rd, rs1, Imm32(0))) => rd.write(self, rs1.read(self) as i32 as i64 as u64),
      RV32(ADDI(rd, rs1, imm)) => rd.write(self, (rs1.read(self) as i64).wrapping_add(imm.decode_sext() as i64) as u64),
      RV64(ADDIW(rd, rs1, imm)) => rd.write(self, (rs1.read(self) as i32).wrapping_add(imm.decode_sext() as i32) as i32 as i64 as u64),
      RV32(SLTI(rd, rs1, imm)) => rd.write(self, if (rs1.read(self) as i64) < (imm.decode_sext() as i64) { 1 } else { 0 }),
      RV32(SLTIU(rd, rs1, Imm32(1))) => rd.write(self, if rs1.read(self) == 0 { 1 } else { 0 }),
      RV32(SLTIU(rd, rs1, imm)) => rd.write(self, if rs1.read(self) < (imm.decode_sext() as u64) { 1 } else { 0 }),
      RV32(XORI(rd, rs1, imm)) => rd.write(self, rs1.read(self) ^ (imm.decode_sext() as u64)),
      RV32(ORI(rd, rs1, imm)) => rd.write(self, rs1.read(self) | (imm.decode_sext() as u64)),
      RV32(ANDI(rd, rs1, imm)) => rd.write(self, rs1.read(self) & (imm.decode_sext() as u64)),

      // RV32's SLLI, SRLI, SRAI was not used when decoding, so they never executes
      // but their implementation should be same.
      RV32(RV32Instr::SLLI(rd, rs1, shamt)) => rd.write(self, rs1.read(self) << shamt.0),
      RV32(RV32Instr::SRLI(rd, rs1, shamt)) => rd.write(self, rs1.read(self) >> shamt.0),
      RV32(RV32Instr::SRAI(rd, rs1, shamt)) => rd.write(self, ((rs1.read(self) as i64) >> shamt.0) as u64),
      RV64(RV64Instr::SLLI(rd, rs1, shamt)) => rd.write(self, rs1.read(self) << shamt.0),
      RV64(RV64Instr::SLLIW(rd, rs1, shamt)) => rd.write(self, ((rs1.read(self) as u32) << shamt.0) as i32 as i64 as u64),
      RV64(RV64Instr::SRLI(rd, rs1, shamt)) => rd.write(self, rs1.read(self) >> shamt.0),
      RV64(RV64Instr::SRLIW(rd, rs1, shamt)) => rd.write(self, ((rs1.read(self) as u32) >> shamt.0) as i32 as i64 as u64),
      RV64(RV64Instr::SRAI(rd, rs1, shamt)) => rd.write(self, ((rs1.read(self) as i64) >> shamt.0) as u64),
      RV64(RV64Instr::SRAIW(rd, rs1, shamt)) => rd.write(self, ((rs1.read(self) as i32) >> shamt.0) as i32 as i64 as u64),
      RV32(ADD(rd, rs1, rs2)) => rd.write(self, rs1.read(self).wrapping_add(rs2.read(self))),
      RV64(ADDW(rd, rs1, rs2)) => rd.write(self, rs1.read(self).wrapping_add(rs2.read(self)) as i32 as i64 as u64),
      RV32(SUB(rd, rs1, rs2)) => rd.write(self, rs1.read(self).wrapping_sub(rs2.read(self))),
      RV64(SUBW(rd, rs1, rs2)) => rd.write(self, rs1.read(self).wrapping_sub(rs2.read(self)) as i32 as i64 as u64),
      RV32(SLT(rd, rs1, rs2)) => rd.write(self, if (rs1.read(self) as i64) < (rs2.read(self) as i64) { 1 } else { 0 }),
      RV32(SLTU(rd, Rs1(Reg::ZERO), rs2)) => rd.write(self, if rs2.read(self) != 0 { 1 } else { 0 }),
      RV32(SLTU(rd, rs1, rs2)) => rd.write(self, if rs1.read(self) < rs2.read(self) { 1 } else { 0 }),
      RV32(XOR(rd, rs1, rs2)) => rd.write(self, rs1.read(self) ^ rs2.read(self)),
      RV32(SLL(rd, rs1, rs2)) => rd.write(self, rs1.read(self) << (rs2.read(self) & 0b111111)),
      RV64(SLLW(rd, rs1, rs2)) => rd.write(self, ((rs1.read(self) as u32) << (rs2.read(self) & 0b11111)) as i32 as i64 as u64),
      RV32(SRL(rd, rs1, rs2)) => rd.write(self, rs1.read(self) >> (rs2.read(self) & 0b111111)),
      RV64(SRLW(rd, rs1, rs2)) => rd.write(self, ((rs1.read(self) as u32) >> (rs2.read(self) & 0b11111)) as i32 as i64 as u64),
      RV32(SRA(rd, rs1, rs2)) => rd.write(self, ((rs1.read(self) as i64) >> (rs2.read(self) & 0b111111)) as u64),
      RV64(SRAW(rd, rs1, rs2)) => rd.write(self, ((rs1.read(self) as i32) >> (rs2.read(self) & 0b11111)) as i32 as i64 as u64),
      RV32(OR(rd, rs1, rs2)) => rd.write(self, rs1.read(self) | rs2.read(self)),
      RV32(AND(rd, rs1, rs2)) => rd.write(self, rs1.read(self) & rs2.read(self)),
      RV32(FENCE(_, _, _, _, _)) => (),
      RV32(FENCE_TSO) => (),
      RV32(PAUSE) => unimplemented!(),
      RV32(ECALL) => unimplemented!(),
      RV32(MUL(_, _, _)) => unimplemented!(),
      RV32(MULH(_, _, _)) => unimplemented!(),
      RV32(MULHSU(_, _, _)) => unimplemented!(),
      RV32(MULHU(_, _, _)) => unimplemented!(),
      RV32(DIV(_, _, _)) => unimplemented!(),
      RV32(DIVU(_, _, _)) => unimplemented!(),
      RV32(REM(_, _, _)) => unimplemented!(),
      RV32(REMU(_, _, _)) => unimplemented!(),
      // the valheim trap
      RV32(EBREAK) => return Err(Exception::ValheimEbreak),

      RV32(LR_W(_, _, _, _)) => todo!(),
      RV32(SC_W(_, _, _, _, _)) => todo!(),
      RV32(AMOSWAP_W(_, _, _, _, _)) => todo!(),
      RV32(AMOADD_W(_, _, _, _, _)) => todo!(),
      RV32(AMOXOR_W(_, _, _, _, _)) => todo!(),
      RV32(AMOAND_W(_, _, _, _, _)) => todo!(),
      RV32(AMOOR_W(_, _, _, _, _)) => todo!(),
      RV32(AMOMIN_W(_, _, _, _, _)) => todo!(),
      RV32(AMOMAX_W(_, _, _, _, _)) => todo!(),
      RV32(AMOMINU_W(_, _, _, _, _)) => todo!(),
      RV32(AMOMAXU_W(_, _, _, _, _)) => todo!(),
      RV32(FLW(_, _, _)) => todo!(),
      RV32(FSW(_, _, _)) => todo!(),
      RV32(FMADD_S(_, _, _, _, _)) => todo!(),
      RV32(FMSUB_S(_, _, _, _, _)) => todo!(),
      RV32(FNMSUB_S(_, _, _, _, _)) => todo!(),
      RV32(FNMADD_S(_, _, _, _, _)) => todo!(),
      RV32(FADD_S(_, _, _, _)) => todo!(),
      RV32(FSUB_S(_, _, _, _)) => todo!(),
      RV32(FMUL_S(_, _, _, _)) => todo!(),
      RV32(FDIV_S(_, _, _, _)) => todo!(),
      RV32(FSQRT_S(_, _, _)) => todo!(),
      RV32(FSGNJ_S(_, _, _)) => todo!(),
      RV32(FSGNJN_S(_, _, _)) => todo!(),
      RV32(FSGNJX_S(_, _, _)) => todo!(),
      RV32(FMIN_S(_, _, _)) => todo!(),
      RV32(FMAX_S(_, _, _)) => todo!(),
      RV32(FCVT_W_S(_, _, _)) => todo!(),
      RV32(FCVT_WU_S(_, _, _)) => todo!(),
      RV32(FMV_X_W(_, _)) => todo!(),
      RV32(FEQ_S(_, _, _)) => todo!(),
      RV32(FLT_S(_, _, _)) => todo!(),
      RV32(FLE_S(_, _, _)) => todo!(),
      RV32(FCLASS_S(_, _)) => todo!(),
      RV32(FCVT_S_W(_, _, _)) => todo!(),
      RV32(FCVT_S_WU(_, _, _)) => todo!(),
      RV32(FMV_W_X(_, _)) => todo!(),
      RV32(FLD(_, _, _)) => todo!(),
      RV32(FSD(_, _, _)) => todo!(),
      RV32(FMADD_D(_, _, _, _, _)) => todo!(),
      RV32(FMSUB_D(_, _, _, _, _)) => todo!(),
      RV32(FNMSUB_D(_, _, _, _, _)) => todo!(),
      RV32(FNMADD_D(_, _, _, _, _)) => todo!(),
      RV32(FADD_D(_, _, _, _)) => todo!(),
      RV32(FSUB_D(_, _, _, _)) => todo!(),
      RV32(FMUL_D(_, _, _, _)) => todo!(),
      RV32(FDIV_D(_, _, _, _)) => todo!(),
      RV32(FSQRT_D(_, _, _)) => todo!(),
      RV32(FSGNJ_D(_, _, _)) => todo!(),
      RV32(FSGNJN_D(_, _, _)) => todo!(),
      RV32(FSGNJX_D(_, _, _)) => todo!(),
      RV32(FMIN_D(_, _, _)) => todo!(),
      RV32(FMAX_D(_, _, _)) => todo!(),
      RV32(FCVT_S_D(_, _, _)) => todo!(),
      RV32(FCVT_D_S(_, _, _)) => todo!(),
      RV32(FEQ_D(_, _, _)) => todo!(),
      RV32(FLT_D(_, _, _)) => todo!(),
      RV32(FLE_D(_, _, _)) => todo!(),
      RV32(FCLASS_D(_, _)) => todo!(),
      RV32(FCVT_W_D(_, _, _)) => todo!(),
      RV32(FCVT_WU_D(_, _, _)) => todo!(),
      RV32(FCVT_D_W(_, _, _)) => todo!(),
      RV32(FCVT_D_WU(_, _, _)) => todo!(),

      RV64(FENCE_I(_, _, _)) => (),
      RV64(CSRRW(rd, rs1, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, rs1.read(self));
        rd.write(self, old);
        // TODO: update page table when csr is SATP
      },
      RV64(CSRRS(rd, rs1, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, old | rs1.read(self));
        rd.write(self, old);
        // TODO: update page table when csr is SATP
      },
      RV64(CSRRC(_, _, _)) => todo!("csr"),
      RV64(CSRRWI(_, _, _)) => todo!("csr"),
      RV64(CSRRSI(_, _, _)) => todo!("csr"),
      RV64(CSRRCI(_, _, _)) => todo!("csr"),
      RV64(MULW(_, _, _)) => todo!(),
      RV64(DIVW(_, _, _)) => todo!(),
      RV64(DIVUW(_, _, _)) => todo!(),
      RV64(REMW(_, _, _)) => todo!(),
      RV64(REMUW(_, _, _)) => todo!(),
      RV64(LR_D(_, _, _, _)) => todo!(),
      RV64(SC_D(_, _, _, _, _)) => todo!(),
      RV64(AMOSWAP_D(_, _, _, _, _)) => todo!(),
      RV64(AMOADD_D(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 8 != 0 {
          return Err(Exception::LoadAddressMisaligned);
        }
        let val = self.read_mem::<u64>(VirtAddr(addr))?;
        self.write_mem::<u64>(VirtAddr(addr), val.wrapping_add(rs2.read(self)))?;
        rd.write(self, val);
      }
      RV64(AMOXOR_D(_, _, _, _, _)) => todo!(),
      RV64(AMOAND_D(_, _, _, _, _)) => todo!(),
      RV64(AMOOR_D(_, _, _, _, _)) => todo!(),
      RV64(AMOMIN_D(_, _, _, _, _)) => todo!(),
      RV64(AMOMAX_D(_, _, _, _, _)) => todo!(),
      RV64(AMOMINU_D(_, _, _, _, _)) => todo!(),
      RV64(AMOMAXU_D(_, _, _, _, _)) => todo!(),
      RV64(FCVT_L_S(_, _, _)) => todo!(),
      RV64(FCVT_LU_S(_, _, _)) => todo!(),
      RV64(FCVT_S_L(_, _, _)) => todo!(),
      RV64(FCVT_S_LU(_, _, _)) => todo!(),
      RV64(FCVT_L_D(_, _, _)) => todo!(),
      RV64(FCVT_LU_D(_, _, _)) => todo!(),
      RV64(FMV_X_D(_, _)) => todo!(),
      RV64(FCVT_D_L(_, _, _)) => todo!(),
      RV64(FCVT_D_LU(_, _, _)) => todo!(),
      RV64(FMV_D_X(_, _)) => todo!(),
    };

    self.journal.trace(|| match is_compressed {
      true => Trace::Instr(InstrTrace::ExecutedCompressed(pc, instr)),
      false => Trace::Instr(InstrTrace::Executed(pc, instr))
    });
    self.write_pc(next_pc);
    Ok(())
  }
}

impl Rs1 {
  #[inline(always)]
  fn read(&self, cpu: &RV64Cpu) -> u64 {
    cpu.read_reg(self.0).expect("internal error")
  }
}

impl Rs2 {
  #[inline(always)]
  fn read(&self, cpu: &RV64Cpu) -> u64 {
    cpu.read_reg(self.0).expect("internal error")
  }
}

impl Rs3 {
  #[inline(always)]
  fn read(&self, cpu: &RV64Cpu) -> u64 {
    cpu.read_reg(self.0).expect("internal error")
  }
}

impl Rd {
  #[inline(always)]
  fn write(&self, cpu: &mut RV64Cpu, val: u64) {
    cpu.write_reg(self.0, val);
  }
}
