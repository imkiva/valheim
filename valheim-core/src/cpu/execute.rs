use crate::cpu::RV64Cpu;
use crate::isa::rv32::RV32Instr;
use crate::isa::typed::{Imm32, Instr, Rd, Reg, Rs1, Rs2, Rs3};
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;
use crate::isa::rv32::RV32Instr::*;
use crate::isa::rv64::RV64Instr;
use crate::isa::rv64::RV64Instr::*;
use crate::isa::typed::Instr::{RV32, RV64};

impl RV64Cpu {
  pub fn fetch(&mut self) -> Option<(VirtAddr, Bytecode)> {
    let pc = self.read_pc();
    let instr = self.read_mem(pc);
    instr.map(|instr| (pc, instr))
  }

  pub fn decode(&self, _: VirtAddr, untyped: Bytecode) -> Option<Instr> {
    Instr::try_from(untyped)
  }

  pub fn execute(&mut self, pc: VirtAddr, instr: Instr) -> Option<()> {
    let mut next_pc = VirtAddr(pc.0.wrapping_add(std::mem::size_of::<Bytecode>() as u64));
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
        rd.write(self, next_pc.0);
        next_pc = VirtAddr(rs1.read(self).wrapping_add(imm.decode_sext() as u64) & !1);
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
      RV32(XORI(rd, rs1, imm)) => rd.write(self, rs1.read(self) ^ imm.decode() as u64),
      RV32(ORI(rd, rs1, imm)) => rd.write(self, rs1.read(self) | imm.decode() as u64),
      RV32(ANDI(rd, rs1, imm)) => rd.write(self, rs1.read(self) & imm.decode() as u64),

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
      RV32(EBREAK) => return None,

      // TODO: RV32AFD
      RV32(_) => todo!("rv32afd"),

      RV64(FENCE_I(_, _, _)) => (),
      RV64(CSRRW(_, _, _)) => todo!("csr"),
      RV64(CSRRS(rd, rs1, csr)) => {
        // TODO: real csr handler
        if csr.value() == 0xf14 { rd.write(self, 0); }
      },
      RV64(CSRRC(_, _, _)) => todo!("csr"),
      RV64(CSRRWI(_, _, _)) => todo!("csr"),
      RV64(CSRRSI(_, _, _)) => todo!("csr"),
      RV64(CSRRCI(_, _, _)) => todo!("csr"),

      // TODO: RV64MAFD
      RV64(_) => todo!("rv64mafd"),
    };
    self.write_pc(next_pc);
    Some(())
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
