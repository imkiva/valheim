use std::cmp::Ordering;

use rustc_apfloat::{Float, Status, StatusAnd};
use rustc_apfloat::ieee::{Double, Single};

use crate::cpu::{PrivilegeMode, RV64Cpu};
use crate::cpu::csr::CSRMap::{FCSR, FCSR_DZ_MASK, FCSR_NV_MASK, FCSR_NX_MASK, FCSR_OF_MASK, FCSR_UF_MASK, FFLAGS, MEPC, MSTATUS, SATP, SEPC, SSTATUS};
use crate::cpu::data::Either;
use crate::cpu::irq::Exception;
use crate::debug::trace::{InstrTrace, Trace};
use crate::isa::compressed::untyped::Bytecode16;
use crate::isa::rv32::RV32Instr;
use crate::isa::rv32::RV32Instr::*;
use crate::isa::rv64::RV64Instr;
use crate::isa::rv64::RV64Instr::*;
use crate::isa::typed::{Imm32, Instr, Rd, Reg, Rs1, Rs2, Rs3};
use crate::isa::typed::Instr::{RV32, RV64};
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;

macro_rules! update_fflags {
  ($self:ident, $status:expr) => {
    let mut fflags = 0;
    if $status & Status::INVALID_OP == Status::INVALID_OP { fflags |= FCSR_NV_MASK; }
    if $status & Status::DIV_BY_ZERO == Status::DIV_BY_ZERO { fflags |= FCSR_DZ_MASK; }
    if $status & Status::OVERFLOW == Status::OVERFLOW { fflags |= FCSR_OF_MASK; }
    if $status & Status::UNDERFLOW == Status::UNDERFLOW { fflags |= FCSR_UF_MASK; }
    if $status & Status::INEXACT == Status::INEXACT { fflags |= FCSR_NX_MASK; }
    $self.csrs.write_unchecked(FFLAGS, fflags);
  };
}

macro_rules! expr {
  ($e:expr) => { $e }
}

macro_rules! pos {
  ($e:expr) => {$e};
}

macro_rules! neg {
  ($e:expr) => {-$e};
}

macro_rules! softfpu_single_op {
  ($self:ident, $rd:ident, $rs1:ident, $rs2:ident, $op:tt) => {{
    let lhs = Single::from_bits(($rs1.read_fp($self) as f32).to_bits() as u128);
    let rhs = Single::from_bits(($rs2.read_fp($self) as f32).to_bits() as u128);
    let StatusAnd { status, value } = expr!(lhs $op rhs);
    let val = f32::from_bits(value.to_bits() as u32);
    update_fflags!($self, status);
    $rd.write_fp($self, val as f64)
  }};
}

macro_rules! softfpu_single_op_fuse {
  ($self:ident, $rd:ident, $rs1sign:ident, $rs1:ident, $rs2:ident, $rs3sign:ident, $rs3:ident, $op:tt) => {{
    let rs1 = Single::from_bits(($rs1.read_fp($self) as f32).to_bits() as u128);
    let rs2 = Single::from_bits(($rs2.read_fp($self) as f32).to_bits() as u128);
    let rs3 = Single::from_bits(($rs3.read_fp($self) as f32).to_bits() as u128);
    let rs1 = $rs1sign!(rs1);
    let rs3 = $rs3sign!(rs3);
    let StatusAnd { status, value } = expr!(rs1.$op(rs2, rs3));
    let val = f32::from_bits(value.to_bits() as u32);
    update_fflags!($self , status);
    $rd.write_fp($self, val as f64)
  }};
}

macro_rules! softfpu_double_op {
  ($self:ident, $rd:ident, $rs1:ident, $rs2:ident, $op:tt) => {{
    let lhs = Double::from_bits($rs1.read_fp($self).to_bits() as u128);
    let rhs = Double::from_bits($rs2.read_fp($self).to_bits() as u128);
    let StatusAnd { status, value } = expr!(lhs $op rhs);
    let val = f64::from_bits(value.to_bits() as u64);
    update_fflags!($self, status);
    $rd.write_fp($self, val)
  }};
}

macro_rules! softfpu_double_op_fuse {
  ($self:ident, $rd:ident, $rs1sign:ident, $rs1:ident, $rs2:ident, $rs3sign:ident, $rs3:ident, $op:tt) => {{
    let rs1 = Double::from_bits($rs1.read_fp($self).to_bits() as u128);
    let rs2 = Double::from_bits($rs2.read_fp($self).to_bits() as u128);
    let rs3 = Double::from_bits($rs3.read_fp($self).to_bits() as u128);
    let rs1 = $rs1sign!(rs1);
    let rs3 = $rs3sign!(rs3);
    let StatusAnd { status, value } = expr!(rs1.$op(rs2, rs3));
    let val = f64::from_bits(value.to_bits() as u64);
    update_fflags!($self , status);
    $rd.write_fp($self, val)
  }};
}

impl RV64Cpu {
  pub fn fetch(&mut self) -> Result<(VirtAddr, Bytecode, Bytecode16), Exception> {
    let pc = self.read_pc();
    let bytecode: Bytecode = Bytecode { repr: self.fetch_mem(pc)? };
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
      None => match Instr::try_from(untyped) {
        Some(instr) => {
          self.journal.trace(|| Trace::Instr(InstrTrace::Decoded(pc, untyped, instr)));
          Ok((Either::Left(untyped), instr))
        }
        None => {
          // println!("Warning: invalid instruction in {:?} mode: {:#x} at {:#x}, at kernel {:?}", self.mode, untyped.repr(), pc.0, self.translate(pc, Reason::Read));
          Err(Exception::IllegalInstruction)
        }
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
      RV32(JAL(rd, imm)) => {
        let offset = imm.decode_sext() as i64;
        let target = (pc.0 as i64).wrapping_add(offset);
        rd.write(self, next_pc.0);
        next_pc = VirtAddr(target as u64);
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

      RV32(LB(rd, rs1, offset)) => {
        let val = self.read_mem::<u8>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as i8 as i32 as i64 as u64;
        rd.write(self, val)
      }
      RV32(LH(rd, rs1, offset)) => {
        let val = self.read_mem::<u16>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as i16 as i32 as i64 as u64;
        rd.write(self, val)
      }
      RV32(LW(rd, rs1, offset)) => {
        let val = self.read_mem::<u32>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as i32 as i64 as u64;
        rd.write(self, val)
      }
      RV64(LD(rd, rs1, offset)) => {
        let val = self.read_mem::<u64>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))?;
        rd.write(self, val)
      }
      RV32(LBU(rd, rs1, offset)) => {
        let val = self.read_mem::<u8>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as u64;
        rd.write(self, val)
      }
      RV32(LHU(rd, rs1, offset)) => {
        let val = self.read_mem::<u16>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as u64;
        rd.write(self, val)
      }
      RV64(LWU(rd, rs1, offset)) => {
        let val = self.read_mem::<u32>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)))? as u64;
        rd.write(self, val)
      }

      RV32(SB(rs1, rs2, offset)) => self.write_mem::<u8>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)), rs2.read(self) as u8)?,
      RV32(SH(rs1, rs2, offset)) => self.write_mem::<u16>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)), rs2.read(self) as u16)?,
      RV32(SW(rs1, rs2, offset)) => self.write_mem::<u32>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)), rs2.read(self) as u32)?,
      RV64(SD(rs1, rs2, offset)) => self.write_mem::<u64>(VirtAddr(rs1.read(self).wrapping_add(offset.decode_sext() as u64)), rs2.read(self) as u64)?,

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

      // RVM
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
          let _ = self.csrs.write_unchecked(FCSR, self.csrs.read_unchecked(FCSR) | FCSR_DZ_MASK);
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
          let _ = self.csrs.write_unchecked(FCSR, self.csrs.read_unchecked(FCSR) | FCSR_DZ_MASK);
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
          let _ = self.csrs.write_unchecked(FCSR, self.csrs.read_unchecked(FCSR) | FCSR_DZ_MASK);
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
          let _ = self.csrs.write_unchecked(FCSR, self.csrs.read_unchecked(FCSR) | FCSR_DZ_MASK);
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

      // RVA
      RV32(LR_W(rd, rs1, _, _)) => {
        let addr = rs1.read(self);
        if addr % 4 != 0 { return Err(Exception::LoadAddressMisaligned(VirtAddr(addr))); }
        let addr = VirtAddr(addr);
        let val = self.read_mem::<u32>(addr)?;
        rd.write(self, val as i32 as i64 as u64);
        self.reserved.push(addr);
      }
      RV32(SC_W(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 4 != 0 { return Err(Exception::StoreAddressMisaligned(VirtAddr(addr))); }
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
        if addr % 8 != 0 { return Err(Exception::LoadAddressMisaligned(VirtAddr(addr))); }
        let val = self.read_mem::<u64>(VirtAddr(addr))?;
        rd.write(self, val);
        self.reserved.push(VirtAddr(addr));
      }
      RV64(SC_D(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 8 != 0 { return Err(Exception::StoreAddressMisaligned(VirtAddr(addr))); }
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

      RV32(AMOADD_W(rd, rs1, rs2, _, _)) => {
        let addr = VirtAddr(rs1.read(self));
        if addr.0 % 4 != 0 { return Err(Exception::LoadAddressMisaligned(addr)); }
        let val = self.read_mem::<u32>(addr)?;
        self.write_mem::<u32>(addr, val.wrapping_add(rs2.read(self) as u32))?;
        rd.write(self, val as i32 as i64 as u64)
      }
      RV64(AMOADD_D(rd, rs1, rs2, _, _)) => {
        let addr = VirtAddr(rs1.read(self));
        if addr.0 % 8 != 0 { return Err(Exception::LoadAddressMisaligned(addr)); }
        let val = self.read_mem::<u64>(addr)?;
        self.write_mem::<u64>(addr, val.wrapping_add(rs2.read(self)))?;
        rd.write(self, val);
      }

      RV32(AMOXOR_W(rd, rs1, rs2, _, _)) => {
        let addr = VirtAddr(rs1.read(self));
        if addr.0 % 4 != 0 { return Err(Exception::LoadAddressMisaligned(addr)); }
        let val = self.read_mem::<u32>(addr)?;
        self.write_mem::<u32>(addr, (val as i32 ^ (rs2.read(self) as i32)) as u32)?;
        rd.write(self, val as i32 as i64 as u64)
      }
      RV64(AMOXOR_D(rd, rs1, rs2, _, _)) => {
        let addr = VirtAddr(rs1.read(self));
        if addr.0 % 8 != 0 { return Err(Exception::LoadAddressMisaligned(addr)); }
        let val = self.read_mem::<u64>(addr)?;
        self.write_mem::<u64>(addr, val ^ rs2.read(self))?;
        rd.write(self, val);
      }

      RV32(AMOAND_W(rd, rs1, rs2, _, _)) => {
        let addr = VirtAddr(rs1.read(self));
        if addr.0 % 4 != 0 { return Err(Exception::LoadAddressMisaligned(addr)); }
        let val = self.read_mem::<u32>(addr)?;
        self.write_mem::<u32>(addr, (val as i32 & (rs2.read(self) as i32)) as u32)?;
        rd.write(self, val as i32 as i64 as u64)
      }
      RV64(AMOAND_D(rd, rs1, rs2, _, _)) => {
        let addr = VirtAddr(rs1.read(self));
        if addr.0 % 8 != 0 { return Err(Exception::LoadAddressMisaligned(addr)); }
        let val = self.read_mem::<u64>(addr)?;
        self.write_mem::<u64>(addr, val & rs2.read(self))?;
        rd.write(self, val);
      }

      RV32(AMOOR_W(rd, rs1, rs2, _, _)) => {
        let addr = VirtAddr(rs1.read(self));
        if addr.0 % 4 != 0 { return Err(Exception::LoadAddressMisaligned(addr)); }
        let val = self.read_mem::<u32>(addr)?;
        self.write_mem::<u32>(addr, (val as i32 | (rs2.read(self) as i32)) as u32)?;
        rd.write(self, val as i32 as i64 as u64)
      }
      RV64(AMOOR_D(rd, rs1, rs2, _, _)) => {
        let addr = VirtAddr(rs1.read(self));
        if addr.0 % 8 != 0 { return Err(Exception::LoadAddressMisaligned(addr)); }
        let val = self.read_mem::<u64>(addr)?;
        self.write_mem::<u64>(addr, val | rs2.read(self))?;
        rd.write(self, val);
      }

      RV32(AMOMIN_W(rd, rs1, rs2, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOMIN_D(rd, rs1, rs2, _, _)) => panic!("not implemented at PC = {:?}", pc),

      RV32(AMOMAX_W(rd, rs1, rs2, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOMAX_D(rd, rs1, rs2, _, _)) => panic!("not implemented at PC = {:?}", pc),

      RV32(AMOMINU_W(rd, rs1, rs2, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOMINU_D(rd, rs1, rs2, _, _)) => panic!("not implemented at PC = {:?}", pc),

      RV32(AMOMAXU_W(rd, rs1, rs2, _, _)) => panic!("not implemented at PC = {:?}", pc),
      RV64(AMOMAXU_D(rd, rs1, rs2, _, _)) => panic!("not implemented at PC = {:?}", pc),

      RV32(AMOSWAP_W(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 4 != 0 { return Err(Exception::LoadAddressMisaligned(VirtAddr(addr))); }
        let val = self.read_mem::<u32>(VirtAddr(addr))?;
        self.write_mem::<u32>(VirtAddr(addr), rs2.read(self) as u32)?;
        rd.write(self, val as i32 as i64 as u64)
      }
      RV64(AMOSWAP_D(rd, rs1, rs2, _, _)) => {
        let addr = rs1.read(self);
        if addr % 8 != 0 { return Err(Exception::LoadAddressMisaligned(VirtAddr(addr))); }
        let val = self.read_mem::<u64>(VirtAddr(addr))?;
        self.write_mem::<u64>(VirtAddr(addr), rs2.read(self))?;
        rd.write(self, val)
      }

      // CSR
      RV64(CSRRW(rd, rs1, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, rs1.read(self))?;
        rd.write(self, old);
        if csr.value() == SATP { self.sync_pagetable(); }
      }
      RV64(CSRRS(rd, rs1, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, old | rs1.read(self))?;
        rd.write(self, old);
        if csr.value() == SATP { self.sync_pagetable(); }
      }
      RV64(CSRRC(rd, rs1, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, old & (!rs1.read(self)))?;
        rd.write(self, old);
        if csr.value() == SATP { self.sync_pagetable(); }
      }
      RV64(CSRRWI(rd, imm, csr)) => {
        rd.write(self, self.csrs.read(csr));
        self.csrs.write(csr, imm.value() as u64)?;
        if csr.value() == SATP { self.sync_pagetable(); }
      }
      RV64(CSRRSI(rd, imm, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, old | imm.value() as u64)?;
        rd.write(self, old);
        if csr.value() == SATP { self.sync_pagetable(); }
      }
      RV64(CSRRCI(rd, imm, csr)) => {
        let old = self.csrs.read(csr);
        self.csrs.write(csr, old & (!imm.value() as u64))?;
        rd.write(self, old);
        if csr.value() == SATP { self.sync_pagetable(); }
      }

      // RVF, RVD
      RV32(FLW(rd, rs1, imm)) => {
        let addr = rs1.read(self).wrapping_add(imm.decode_sext() as u64);
        let val = f32::from_bits(self.read_mem::<u32>(VirtAddr(addr))? as u32);
        rd.write_fp(self, val as f64);
      }
      RV32(FLD(rd, rs1, imm)) => {
        let addr = rs1.read(self).wrapping_add(imm.decode_sext() as u64);
        let val = f64::from_bits(self.read_mem::<u64>(VirtAddr(addr))?);
        rd.write_fp(self, val);
      }
      RV32(FSW(rs1, rs2, imm)) => {
        let addr = rs1.read(self).wrapping_add(imm.decode_sext() as u64);
        let val = rs2.read_fp(self) as f32;
        self.write_mem::<u32>(VirtAddr(addr), val.to_bits())?;
      }
      RV32(FSD(rs1, rs2, imm)) => {
        let addr = rs1.read(self).wrapping_add(imm.decode_sext() as u64);
        let val = rs2.read_fp(self);
        self.write_mem::<u64>(VirtAddr(addr), val.to_bits())?;
      }

      RV32(FMADD_S(rd, rs1, rs2, rs3, _)) => softfpu_single_op_fuse!(self, rd, pos, rs1, rs2, pos, rs3, mul_add),
      RV32(FMADD_D(rd, rs1, rs2, rs3, _)) => softfpu_double_op_fuse!(self, rd, pos, rs1, rs2, pos, rs3, mul_add),
      RV32(FMSUB_S(rd, rs1, rs2, rs3, _)) => softfpu_single_op_fuse!(self, rd, pos, rs1, rs2, neg, rs3, mul_add),
      RV32(FMSUB_D(rd, rs1, rs2, rs3, _)) => softfpu_double_op_fuse!(self, rd, pos, rs1, rs2, neg, rs3, mul_add),
      RV32(FNMADD_S(rd, rs1, rs2, rs3, _)) => softfpu_single_op_fuse!(self, rd, neg, rs1, rs2, neg, rs3, mul_add),
      RV32(FNMADD_D(rd, rs1, rs2, rs3, _)) => softfpu_double_op_fuse!(self, rd, neg, rs1, rs2, neg, rs3, mul_add),
      RV32(FNMSUB_S(rd, rs1, rs2, rs3, _)) => softfpu_single_op_fuse!(self, rd, neg, rs1, rs2, pos, rs3, mul_add),
      RV32(FNMSUB_D(rd, rs1, rs2, rs3, _)) => softfpu_double_op_fuse!(self, rd, neg, rs1, rs2, pos, rs3, mul_add),

      RV32(FADD_S(rd, rs1, rs2, _)) => softfpu_single_op!(self, rd, rs1, rs2, +),
      RV32(FADD_D(rd, rs1, rs2, _)) => softfpu_double_op!(self, rd, rs1, rs2, +),
      RV32(FSUB_S(rd, rs1, rs2, _)) => softfpu_single_op!(self, rd, rs1, rs2, -),
      RV32(FSUB_D(rd, rs1, rs2, _)) => softfpu_double_op!(self, rd, rs1, rs2, -),
      RV32(FMUL_S(rd, rs1, rs2, _)) => softfpu_single_op!(self, rd, rs1, rs2, *),
      RV32(FMUL_D(rd, rs1, rs2, _)) => softfpu_double_op!(self, rd, rs1, rs2, *),
      RV32(FDIV_S(rd, rs1, rs2, _)) => softfpu_single_op!(self, rd, rs1, rs2, /),
      RV32(FDIV_D(rd, rs1, rs2, _)) => softfpu_double_op!(self, rd, rs1, rs2, /),

      RV32(FSQRT_S(rd, rs1, _)) => {
        let rs1 = rs1.read_fp(self) as f32;
        let val = rs1.sqrt();
        if val.is_nan() {
          self.csrs.write_unchecked(FFLAGS, FCSR_NV_MASK);
          rd.write_fp(self, f32::NAN as f64)
        } else {
          if rs1 % 1.0 != 0.0 || val % 1.0 != 0.0 {
            self.csrs.write_unchecked(FFLAGS, FCSR_NX_MASK);
          }
          rd.write_fp(self, val as f64)
        }
      }
      // TODO: replace with softfpu
      RV32(FSQRT_D(rd, rs1, _)) => rd.write_fp(self, (rs1.read_fp(self) as f64).sqrt() as f64),

      RV32(FSGNJ_S(rd, rs1, rs2)) => rd.write_fp(self, rs1.read_fp(self).copysign(rs2.read_fp(self))),
      RV32(FSGNJ_D(rd, rs1, rs2)) => rd.write_fp(self, rs1.read_fp(self).copysign(rs2.read_fp(self))),

      RV32(FSGNJN_S(rd, rs1, rs2)) => rd.write_fp(self, rs1.read_fp(self).copysign(-rs2.read_fp(self))),
      RV32(FSGNJN_D(rd, rs1, rs2)) => rd.write_fp(self, rs1.read_fp(self).copysign(-rs2.read_fp(self))),

      RV32(FSGNJX_S(rd, rs1, rs2)) => {
        let rs1 = rs1.read_fp(self);
        let sign1 = (rs1 as f32).to_bits() & 0x80000000;
        let sign2 = (rs2.read_fp(self) as f32).to_bits() & 0x80000000;
        let other = (rs1 as f32).to_bits() & 0x7fffffff;
        rd.write_fp(self, f32::from_bits((sign1 ^ sign2) | other) as f64);
      }
      RV32(FSGNJX_D(rd, rs1, rs2)) => {
        let rs1 = rs1.read_fp(self);
        let sign1 = rs1.to_bits() & 0x80000000_00000000;
        let sign2 = rs2.read_fp(self).to_bits() & 0x80000000_00000000;
        let other = rs1.to_bits() & 0x7fffffff_ffffffff;
        rd.write_fp(self, f64::from_bits((sign1 ^ sign2) | other));
      }

      RV32(FMIN_S(rd, rs1, rs2)) => rd.write_fp(self, rs1.read_fp(self).min(rs2.read_fp(self))),
      RV32(FMIN_D(rd, rs1, rs2)) => rd.write_fp(self, rs1.read_fp(self).min(rs2.read_fp(self))),
      RV32(FMAX_S(rd, rs1, rs2)) => rd.write_fp(self, rs1.read_fp(self).max(rs2.read_fp(self))),
      RV32(FMAX_D(rd, rs1, rs2)) => rd.write_fp(self, rs1.read_fp(self).max(rs2.read_fp(self))),

      RV32(FEQ_S(rd, rs1, rs2)) |
      RV32(FLT_S(rd, rs1, rs2)) |
      RV32(FLE_S(rd, rs1, rs2)) => {
        let lhs_rawbits = (rs1.read_fp(self) as f32).to_bits();
        let rhs_rawbits = (rs2.read_fp(self) as f32).to_bits();
        let lhs = Single::from_bits(lhs_rawbits as u128);
        let rhs = Single::from_bits(rhs_rawbits as u128);
        if lhs.is_nan() || rhs.is_nan() {
          // 13.8 Single-Precision Floating-Point Compare Instructions
          // FEQ.S performs a quiet comparison: it only sets the invalid operation exception flag if either input is a signaling NaN.
          // FLT.S and FLE.S perform what the IEEE 754-2008 standard refers to as signaling comparisons:
          // that is, they set the invalid operation exception flag if either input is NaN.
          let set_nv = match instr {
            RV32(FEQ_S(..)) => {
              // TODO: possible a bug of rust: converting signaling NaN from f32 to f64 (or vice versa) may lose the signaling characteristic
              //  see: https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=024899edb6ca3820319e45058f838403
              lhs.is_signaling() || rhs.is_signaling()
            }
            RV32(FLT_S(..)) |
            RV32(FLE_S(..)) => true,
            _ => unreachable!(),
          };
          if set_nv { let _ = self.csrs.write_unchecked(FFLAGS, FCSR_NV_MASK); }
          rd.write(self, 0u64);
        } else {
          let cmp = lhs.partial_cmp(&rhs).unwrap();
          match (instr, cmp) {
            (RV32(FEQ_S(..)), Ordering::Equal) => rd.write(self, 1u64),
            (RV32(FLT_S(..)), Ordering::Less) => rd.write(self, 1u64),
            (RV32(FLE_S(..)), Ordering::Equal) => rd.write(self, 1u64),
            (RV32(FLE_S(..)), Ordering::Less) => rd.write(self, 1u64),
            _ => rd.write(self, 0u64),
          }
        }
      }

      // TODO: replace with softfpu
      RV32(FEQ_D(rd, rs1, rs2)) => rd.write(self, (rs1.read_fp(self) == rs2.read_fp(self)) as u64),
      RV32(FLT_D(rd, rs1, rs2)) => rd.write(self, (rs1.read_fp(self) < rs2.read_fp(self)) as u64),
      RV32(FLE_D(rd, rs1, rs2)) => rd.write(self, (rs1.read_fp(self) <= rs2.read_fp(self)) as u64),

      RV32(FCLASS_S(_, _)) => panic!("not implemented at PC = {:?}", pc),
      RV32(FCLASS_D(_, _)) => panic!("not implemented at PC = {:?}", pc),

      RV32(FMV_X_W(rd, rs1)) => {
        let fp = rs1.read_fp(self) as f32;
        let bits = fp.to_bits();
        let val = (bits & 0xffffffff) as i32 as i64 as u64;
        rd.write(self, val)
      }
      RV32(FMV_W_X(rd, rs1)) => {
        let bits = (rs1.read(self) & 0xffffffff) as u32;
        let val = f32::from_bits(bits) as f64;
        rd.write_fp(self, val)
      }

      RV64(FMV_X_D(rd, rs1)) => rd.write(self, rs1.read_fp(self).to_bits()),
      RV64(FMV_D_X(rd, rs1)) => rd.write_fp(self, f64::from_bits(rs1.read(self))),

      // 13.7 Single-Precision Floating-Point Conversion and Move Instructions
      // TODO: If the rounded result is not representable in the destination format,
      //  it is clipped to the nearest value and the invalid flag is set.

      // TODO: All floating-point conversion instructions set the Inexact exception flag
      //  if the rounded result differs from the operand value and the Invalid exception flag is not set.

      RV32(FCVT_W_S(rd, rs1, _)) => rd.write(self, ((rs1.read_fp(self) as f32).round() as i32) as u64),
      RV32(FCVT_S_W(rd, rs1, _)) => rd.write_fp(self, ((rs1.read(self) as i32) as f32) as f64),

      RV32(FCVT_W_D(rd, rs1, _)) => rd.write(self, (rs1.read_fp(self).round() as i32) as u64),
      RV32(FCVT_D_W(rd, rs1, _)) => rd.write_fp(self, (rs1.read(self) as i32) as f64),

      RV32(FCVT_WU_S(rd, rs1, _)) => rd.write(self, (((rs1.read_fp(self) as f32).round() as u32) as i32) as u64),
      RV32(FCVT_S_WU(rd, rs1, _)) => rd.write_fp(self, ((rs1.read(self) as u32) as f32) as f64),

      RV32(FCVT_WU_D(rd, rs1, _)) => rd.write(self, ((rs1.read_fp(self).round() as u32) as i32) as u64),
      RV32(FCVT_D_WU(rd, rs1, _)) => rd.write_fp(self, (rs1.read(self) as u32) as f64),

      RV32(FCVT_S_D(rd, rs1, _)) => rd.write_fp(self, rs1.read_fp(self)),
      RV32(FCVT_D_S(rd, rs1, _)) => rd.write_fp(self, (rs1.read_fp(self) as f32) as f64),

      RV64(FCVT_L_S(rd, rs1, _)) => rd.write(self, (rs1.read_fp(self) as f32).round() as i64 as u64),
      RV64(FCVT_S_L(rd, rs1, _)) => rd.write_fp(self, (rs1.read(self) as i64 as f32) as f64),

      RV64(FCVT_LU_S(rd, rs1, _)) => rd.write(self, (rs1.read_fp(self) as f32).round() as u64),
      RV64(FCVT_S_LU(rd, rs1, _)) => rd.write_fp(self, ((rs1.read(self) as u64) as f32) as f64),

      RV64(FCVT_L_D(rd, rs1, _)) => rd.write(self, rs1.read_fp(self).round() as i64 as u64),
      RV64(FCVT_D_L(rd, rs1, _)) => rd.write_fp(self, rs1.read(self) as i64 as f64),

      RV64(FCVT_LU_D(rd, rs1, _)) => rd.write(self, rs1.read_fp(self).round() as u64),
      RV64(FCVT_D_LU(rd, rs1, _)) => rd.write_fp(self, rs1.read(self) as f64),

      // Privileged
      RV64(SRET) => {
        let sepc = self.csrs.read_unchecked(SEPC);
        let spie = self.csrs.read_sstatus_SPIE();
        let spp = self.csrs.read_sstatus_SPP();

        // set pc to MEPC
        next_pc = VirtAddr(sepc);
        // set cpu privilege mode to MPP
        self.mode = spp;
        // set SIE = SPIE
        self.csrs.write_sstatus_SIE(spie);
        // set SPIE to 1
        self.csrs.write_sstatus_SPIE(true);
        // set SPP to User if User is supported, otherwise to Supervisor
        self.csrs.write_sstatus_SPP(PrivilegeMode::User);

        // 3.1.6.3 Memory Privilege in mstatus Register
        // An MRET or SRET instruction that changes the privilege mode
        // to a mode less privileged than M also sets MPRV=0.
        self.csrs.write_mstatus_MPRV(false);
      }
      RV64(MRET) => {
        let mepc = self.csrs.read_unchecked(MEPC);
        let mpie = self.csrs.read_mstatus_MPIE();
        let mpp = self.csrs.read_mstatus_MPP();

        // set pc to MEPC
        next_pc = VirtAddr(mepc);
        // set cpu privilege mode to MPP
        self.mode = mpp;
        // set MIE = MPIE
        self.csrs.write_mstatus_MIE(mpie);
        // set MPIE to 1
        self.csrs.write_mstatus_MPIE(true);
        // set MPP to User if User is supported, otherwise to Machine
        self.csrs.write_mstatus_MPP(PrivilegeMode::User);

        // 3.1.6.3 Memory Privilege in mstatus Register
        // An MRET or SRET instruction that changes the privilege mode
        // to a mode less privileged than M also sets MPRV=0.
        self.csrs.write_mstatus_MPRV(false);
      }

      RV64(WFI) => {
        // println!("[Valheim] CPU wfi at {:#x} in {:?} mode", pc.0, self.mode);
        // pub fn debug_dump(cpu: &mut RV64Cpu, begin: VirtAddr, ninstr: u64) {
        //   let udump_start = begin - VirtAddr(4 * ninstr);
        //   let udump_end = begin + VirtAddr(4 * ninstr);
        //   unsafe {
        //     let mut udump = udump_start;
        //     while udump != udump_end {
        //       let kdump = cpu.translate(udump, Reason::Read).unwrap();
        //       let ptr = cpu.bus.mem.to_phys(kdump).unwrap().0;
        //       println!("{:#08x} -> {:#08x}: {:02x} {:02x} {:02x} {:02x}  |  {}{}{}{}", udump.0, kdump.0,
        //                *ptr, *(ptr.wrapping_add(1)), *(ptr.wrapping_add(2)), *(ptr.wrapping_add(3)),
        //                *ptr as char, *(ptr.wrapping_add(1)) as char, *(ptr.wrapping_add(2)) as char, *(ptr.wrapping_add(3)) as char);
        //       udump += VirtAddr(4);
        //     }
        //   }
        // }
        // debug_dump(self, pc, 5);
        self.wfi = true
      }

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

macro_rules! impl_read {
  ($ty:ident) => {
    impl $ty {
      #[inline(always)]
      fn read(&self, cpu: &RV64Cpu) -> u64 {
        cpu.read_reg(self.0).expect("internal error")
      }

      #[inline(always)]
      fn read_fp(&self, cpu: &RV64Cpu) -> f64 {
        cpu.read_reg_fp(self.0).expect("internal error")
      }
    }
  };
}

impl_read!(Rs1);
impl_read!(Rs2);
impl_read!(Rs3);

impl Rd {
  #[inline(always)]
  fn write(&self, cpu: &mut RV64Cpu, val: u64) {
    cpu.write_reg(self.0, val);
  }

  fn write_fp(&self, cpu: &mut RV64Cpu, val: f64) {
    cpu.write_reg_fp(self.0, val);
  }
}
