use crate::cpu::csr::CSRMap::{FCSR, FCSR_DZ_MASK, MEPC, MSTATUS, SATP, SEPC, SSTATUS};
use crate::cpu::data::Either;
use crate::cpu::irq::Exception;
use crate::cpu::{PrivilegeMode, RV64Cpu};
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
    let compressed: Bytecode16 = Bytecode16 { repr: bytecode.repr() as u16 };
    self.journal.trace(|| Trace::Instr(InstrTrace::Fetched(pc, bytecode, compressed)));
    Ok((pc, bytecode, compressed))
  }

  pub fn decode(&mut self, pc: VirtAddr, untyped: Bytecode, compressed: Bytecode16) -> Result<(Either<Bytecode, Bytecode16>, Instr), Exception> {
    match Instr::try_from_compressed(compressed) {
      Some(instr) => {
        self.journal.trace(|| Trace::Instr(InstrTrace::DecodedCompressed(pc, compressed, instr)));
        Ok((Either::Right(compressed), instr))
      }
      None => {
        Instr::try_from(untyped).map(|instr| {
          self.journal.trace(|| Trace::Instr(InstrTrace::Decoded(pc, untyped, instr)));
          (Either::Left(untyped), instr)
        }).ok_or(Exception::IllegalInstruction(pc, untyped, compressed))
      }
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

      RV32(SB(rs1, rs2, offset)) => {
        let addr = rs1.read(self).wrapping_add(offset.decode_sext() as u64);
        let val = rs2.read(self) as u8;
        self.write_mem(VirtAddr(addr), val)?
      }
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
      RV64(FENCE_I(_, _, _)) => (),
      RV32(FENCE_TSO) => (),
      RV32(PAUSE) => panic!("not implemented at PC = {:?}", pc),
      RV32(ECALL) => return match self.mode {
        PrivilegeMode::User => Err(Exception::UserEcall),
        PrivilegeMode::Supervisor => Err(Exception::SupervisorEcall),
        PrivilegeMode::Machine => Err(Exception::MachineEcall),
      },
      RV32(MUL(rd, rs1, rs2)) => rd.write(self, (rs1.read(self) as i64).wrapping_mul(rs2.read(self) as i64) as u64),
      RV32(MULH(rd, rs1, rs2)) => {
        let rs1 = rs1.read(self) as i64 as i128;
        let rs2 = rs2.read(self) as i64 as i128;
        let val = (rs1.wrapping_mul(rs2) >> 64) as u64;
        // signed * signed
        rd.write(self, val)
      }
      RV32(MULHSU(rd, rs1, rs2)) => {
        let rs1 = rs1.read(self) as i64 as i128 as u128;
        let rs2 = rs2.read(self) as u128;
        let val = (rs1.wrapping_mul(rs2) >> 64) as u64;
        // signed × unsigned
        rd.write(self, val)
      }
      RV32(MULHU(rd, rs1, rs2)) => {
        let rs1 = rs1.read(self) as u128;
        let rs2 = rs2.read(self) as u128;
        let val = (rs1.wrapping_mul(rs2) >> 64) as u64;
        // unsigned * unsigned
        rd.write(self, val)
      }
      RV32(DIV(rd, rs1, rs2)) => {
        let dividend = rs1.read(self) as i64;
        let divisor = rs2.read(self) as i64;
        let val = if divisor == 0 {
          self.csrs.write_unchecked(FCSR, self.csrs.read_unchecked(FCSR) | FCSR_DZ_MASK);
          u64::MAX
        } else if dividend == i64::MIN && divisor == -1 {
          dividend as u64
        } else {
          dividend.wrapping_div(divisor) as u64
        };
        rd.write(self, val);
      }
      RV32(DIVU(rd, rs1, rs2)) => {
        let dividend = rs1.read(self);
        let divisor = rs2.read(self);
        let val = if divisor == 0 {
          self.csrs.write_unchecked(FCSR, self.csrs.read_unchecked(FCSR) | FCSR_DZ_MASK);
          u64::MAX
        } else {
          dividend.wrapping_div(divisor)
        };
        rd.write(self, val);
      }
      RV32(REM(rd, rs1, rs2)) => {
        let dividend = rs1.read(self) as i64;
        let divisor = rs2.read(self) as i64;
        let val = if divisor == 0 {
          dividend as u64
        } else if dividend == i64::MIN && divisor == -1 {
          0
        } else {
          dividend.wrapping_rem(divisor) as u64
        };
        rd.write(self, val);
      }
      RV32(REMU(rd, rs1, rs2)) => {
        let dividend = rs1.read(self);
        let divisor = rs2.read(self);
        let val = if divisor == 0 {
          dividend
        } else {
          dividend.wrapping_rem(divisor)
        };
        rd.write(self, val);
      }
      RV64(MULW(rd, rs1, rs2)) => {
        let lhs = rs1.read(self) as i32;
        let rhs = rs2.read(self) as i32;
        let val = lhs.wrapping_mul(rhs);
        rd.write(self, val as i64 as u64);
      }
      RV64(DIVW(rd, rs1, rs2)) => {
        let dividend = rs1.read(self) as i32;
        let divisor = rs2.read(self) as i32;
        let val = if divisor == 0 {
          self.csrs.write_unchecked(FCSR, self.csrs.read_unchecked(FCSR) | FCSR_DZ_MASK);
          u64::MAX
        } else if dividend == i32::MIN && divisor == -1 {
          dividend as i64 as u64
        } else {
          dividend.wrapping_div(divisor) as i64 as u64
        };
        rd.write(self, val);
      }
      RV64(DIVUW(rd, rs1, rs2)) => {
        let dividend = rs1.read(self) as u32;
        let divisor = rs2.read(self) as u32;
        let val = if divisor == 0 {
          self.csrs.write_unchecked(FCSR, self.csrs.read_unchecked(FCSR) | FCSR_DZ_MASK);
          u64::MAX
        } else {
          dividend.wrapping_div(divisor) as i32 as i64 as u64
        };
        rd.write(self, val);
      }
      RV64(REMW(rd, rs1, rs2)) => {
        let dividend = rs1.read(self) as i32;
        let divisor = rs2.read(self) as i32;
        let val = if divisor == 0 {
          dividend as i64 as u64
        } else if dividend == i32::MIN && divisor == -1 {
          0
        } else {
          dividend.wrapping_rem(divisor) as i64 as u64
        };
        rd.write(self, val);
      }
      RV64(REMUW(rd, rs1, rs2)) => {
        let dividend = rs1.read(self) as u32;
        let divisor = rs2.read(self) as u32;
        let val = if divisor == 0 {
          dividend as i32 as i64 as u64
        } else {
          dividend.wrapping_rem(divisor) as i32 as i64 as u64
        };
        rd.write(self, val);
      }

      // the valheim trap
      RV32(EBREAK) => return Err(Exception::Breakpoint),

      RV32(LR_W(rd, rs1, _, _)) => {
        let addr = rs1.read(self);
        if addr % 4 != 0 {
          return Err(Exception::LoadAddressMisaligned(VirtAddr(addr)));
        }
        let addr = VirtAddr(addr);
        let val = self.read_mem::<u32>(addr)?;
        rd.write(self, val as i32 as i64 as u64);
        self.reserved.push(addr);
      }
      RV32(SC_W(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 4 != 0 {
          return Err(Exception::StoreAddressMisaligned(VirtAddr(addr)));
        }
        let addr = VirtAddr(addr);
        if self.reserved.contains(&addr) {
          // "Regardless of success or failure, executing an SC.W instruction
          // invalidates any reservation held by this hart. "
          self.reserved.retain(|&x| x != addr);
          self.write_mem::<u32>(addr, rs2.read(self) as u32)?;
          rd.write(self, 0);
        } else {
          self.reserved.retain(|&x| x != addr);
          rd.write(self, 1);
        };
      }
      RV64(LR_D(rd, rs1, _, _)) => {
        let addr = rs1.read(self);
        if addr % 8 != 0 {
          return Err(Exception::LoadAddressMisaligned(VirtAddr(addr)));
        }
        let val = self.read_mem::<u64>(VirtAddr(addr))?;
        rd.write(self, val);
        self.reserved.push(VirtAddr(addr));
      }
      RV64(SC_D(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 8 != 0 {
          return Err(Exception::StoreAddressMisaligned(VirtAddr(addr)));
        }
        let addr = VirtAddr(addr);
        if self.reserved.contains(&addr) {
          self.reserved.retain(|&x| x != addr);
          self.write_mem::<u64>(addr, rs2.read(self))?;
          rd.write(self, 0);
        } else {
          self.reserved.retain(|&x| x != addr);
          rd.write(self, 1);
        }
      }
      RV32(AMOSWAP_W(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 4 != 0 {
          return Err(Exception::LoadAddressMisaligned(VirtAddr(addr)));
        }
        let val = self.read_mem::<u32>(VirtAddr(addr))?;
        self.write_mem::<u32>(VirtAddr(addr), rs2.read(self) as u32)?;
        rd.write(self, val as i32 as i64 as u64)
      }
      RV32(AMOADD_W(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(AMOXOR_W(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(AMOAND_W(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(AMOOR_W(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(AMOMIN_W(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(AMOMAX_W(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(AMOMINU_W(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(AMOMAXU_W(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOSWAP_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOADD_D(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 8 != 0 {
          return Err(Exception::LoadAddressMisaligned(VirtAddr(addr)));
        }
        let val = self.read_mem::<u64>(VirtAddr(addr))?;
        self.write_mem::<u64>(VirtAddr(addr), val.wrapping_add(rs2.read(self)))?;
        rd.write(self, val);
      }
      RV64(AMOXOR_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOAND_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOOR_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOMIN_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOMAX_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOMINU_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOMAXU_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),

      RV64(CSRRW(rd, rs1, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, rs1.read(self));
        rd.write(self, old);
        if csr.value() == SATP {
          self.sync_pagetable();
        }
      }
      RV64(CSRRS(rd, rs1, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, old | rs1.read(self));
        rd.write(self, old);
        if csr.value() == SATP {
          self.sync_pagetable();
        }
      }
      RV64(CSRRC(_, _, _)) => todo!("csr"),
      RV64(CSRRWI(_, _, _)) => todo!("csr"),
      RV64(CSRRSI(_, _, _)) => todo!("csr"),
      RV64(CSRRCI(_, _, _)) => todo!("csr"),

      RV32(FLW(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSW(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMADD_S(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMSUB_S(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FNMSUB_S(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FNMADD_S(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FADD_S(_, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSUB_S(_, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMUL_S(_, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FDIV_S(_, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSQRT_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSGNJ_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSGNJN_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSGNJX_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMIN_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMAX_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_W_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_WU_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMV_X_W(_, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FEQ_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FLT_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FLE_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCLASS_S(_, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_S_W(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_S_WU(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMV_W_X(_, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FLD(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSD(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMADD_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMSUB_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FNMSUB_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FNMADD_D(_, _, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FADD_D(_, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSUB_D(_, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMUL_D(_, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FDIV_D(_, _, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSQRT_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSGNJ_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSGNJN_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FSGNJX_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMIN_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FMAX_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_S_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_D_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FEQ_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FLT_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FLE_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCLASS_D(_, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_W_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_WU_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_D_W(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCVT_D_WU(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FCVT_L_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FCVT_LU_S(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FCVT_S_L(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FCVT_S_LU(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FCVT_L_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FCVT_LU_D(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FMV_X_D(_, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FCVT_D_L(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FCVT_D_LU(_, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(FMV_D_X(_, _)) => panic!("not implemented at PC = {:?}", pc),

      // Privileged
      RV64(SRET) => {
        let sepc = self.csrs.read_unchecked(SEPC);
        let spie = self.csrs.read_bit(SSTATUS, 5);
        let spp = match self.csrs.read_bit(SSTATUS, 8) {
          false => PrivilegeMode::User,
          true => PrivilegeMode::Supervisor,
        };

        // set pc to MEPC
        next_pc = VirtAddr(sepc);
        // set cpu privilege mode to MPP
        self.mode = spp;
        // set SIE = SPIE
        self.csrs.write_bit(SSTATUS, 1, spie);
        // set SPIE to 1
        self.csrs.write_bit(SSTATUS, 5, true);
        // set SPP to User if User is supported, otherwise to Machine
        self.csrs.write_bit(SSTATUS, 8, false);
      }
      RV64(MRET) => {
        let mepc = self.csrs.read_unchecked(MEPC);
        let mpie = self.csrs.read_bit(MSTATUS, 7);
        let mpp0 = self.csrs.read_bit(MSTATUS, 11);
        let mpp1 = self.csrs.read_bit(MSTATUS, 12);
        let mpp = match (mpp1, mpp0) {
          (false, false) => PrivilegeMode::User,
          (false, true) => PrivilegeMode::Supervisor,
          (true, true) => PrivilegeMode::Machine,
          _ => panic!("invalid privilege mode in MPP (mstatus[11:12]) = {}{}", mpp1 as i32, mpp0 as i32),
        };

        // set pc to MEPC
        next_pc = VirtAddr(mepc);
        // set cpu privilege mode to MPP
        self.mode = mpp;
        // set MIE = MPIE
        self.csrs.write_bit(MSTATUS, 3, mpie);
        // set MPIE to 1
        self.csrs.write_bit(MSTATUS, 7, true);
        // set MPP to User if User is supported, otherwise to Machine
        self.csrs.write_bit(MSTATUS, 11, false);
        self.csrs.write_bit(MSTATUS, 12, false);
      }
      RV64(WFI) => panic!("not implemented at PC = {:?}", pc),

      // 4.1.11 Supervisor Address Translation and Protection (satp) Register
      // The satp register is considered active when the effective privilege mode is S-mode or U-mode.
      // Executions of the address-translation algorithm may only begin using a given value of satp when satp is active.
      // Translations that began while satp was active are not required to complete or terminate
      // when satp is no longer active, unless an SFENCE.VMA instruction matching the address and
      // ASID is executed. The SFENCE.VMA instruction must be used to ensure that updates
      // to the address-translation data structures are observed by subsequent implicit reads
      // to those structures by a hart.
      RV64(SFENCE_VMA(_, _)) => (),

      RV64(SINVAL_VMA(_, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(SFENCE_W_INVAL) => panic!("not implemented at PC = {:?}", pc),
      RV64(SFENCE_INVAL_IR) => panic!("not implemented at PC = {:?}", pc),
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
