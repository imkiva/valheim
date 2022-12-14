#![allow(unused_imports)]
#![allow(unused_variables)]
#![allow(dead_code)]

#[cfg(test)]
mod fcvt {
  use std::str::FromStr;
  use rustc_apfloat::{Float, FloatConvert};
  use rustc_apfloat::ieee::{Double, Single};
  use valheim_asm::asm::encode32::Encode32;
  use valheim_asm::isa::data::Fin;
  use valheim_asm::isa::rv32::RV32Instr;
  use valheim_asm::isa::rv64::RV64Instr;
  use valheim_asm::isa::typed::{Instr, Reg, RoundingMode};
  use valheim_asm::isa::typed::Instr::{RV32, RV64};

  use crate::cpu::RV64Cpu;
  use crate::memory::VirtAddr;

  pub enum APFloat {
    S(Single),
    D(Double),
  }

  impl APFloat {
    pub fn to_f64(&self) -> f64 {
      match self {
        APFloat::S(s) => f32::from_bits(s.to_bits() as u32) as f64,
        APFloat::D(d) => f64::from_bits(d.to_bits() as u64),
      }
    }
  }

  pub fn they_are_inverse(f: Reg, x: Reg, f2i: Instr, i2f: Instr, data: APFloat) {
    let mut cpu = RV64Cpu::new(None);
    let data = data.to_f64();
    cpu.regs.write_fp(f, data);
    // convert float to int
    println!("Before execution");
    cpu.regs.show_one_status(1);
    cpu.execute(VirtAddr(0), f2i, false).unwrap();
    println!("After execution");
    cpu.regs.show_one_status(1);
    assert_eq!(data as i64, cpu.regs.read(x).unwrap() as i64);
    // convert int back to float (with floating point part removed)
    println!("Before execution");
    cpu.regs.show_one_status(1);
    cpu.execute(VirtAddr(0), i2f, false).unwrap();
    println!("After execution");
    cpu.regs.show_one_status(1);
    assert_eq!(data as i64 as f64, cpu.regs.read_fp(f).unwrap());
  }

  #[test]
  pub fn all_in() {
    let f = Reg::F(Fin::new(1));
    let x = Reg::X(Fin::new(1));
    let tests = vec![
      (RV32(RV32Instr::FCVT_W_S(x.into(), f.into(), RoundingMode::DYN)),
       RV32(RV32Instr::FCVT_S_W(f.into(), x.into(), RoundingMode::DYN)),
       APFloat::S(Single::from_str("114514.1919").unwrap())),
      (RV64(RV64Instr::FCVT_L_D(x.into(), f.into(), RoundingMode::DYN)),
       RV64(RV64Instr::FCVT_D_L(f.into(), x.into(), RoundingMode::DYN)),
       APFloat::D(Double::from_str("114514.1919").unwrap())),
    ];
    for (f2i, i2f, data) in tests {
      println!("f2i: {:?}, asm = {:#10X}", f2i, f2i.encode32());
      println!("i2f: {:?}, asm = {:#10X}", i2f, i2f.encode32());
      // they_are_inverse(f, x, f2i, i2f, data);
      println!("==============================================")
    }
  }
}
