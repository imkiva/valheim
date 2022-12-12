#[cfg(test)]
mod tests {
  use crate::asm::encode::Encode32;
  use crate::isa::data::Fin;
  use crate::isa::rv32::RV32Instr;
  use crate::isa::typed::{Instr, Rd, RoundingMode, Rs1, Rs2, Rs3};
  use crate::isa::typed::Reg::{F, X};

  #[test]
  fn xlb1() {
    let x = Instr::decode32(0xA221A253).unwrap();
    println!("{:?}", x);
  }

  #[test]
  fn xlb2() {
    let x = vec![
      (RV32Instr::FEQ_S(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FEQ_S"),
      (RV32Instr::FEQ_S(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FEQ_S"),
      (RV32Instr::FEQ_S(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FEQ_S"),
      (RV32Instr::FEQ_S(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FEQ_S"),
      (RV32Instr::FLT_S(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FLT_S"),
      (RV32Instr::FLT_S(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FLT_S"),
      (RV32Instr::FLE_S(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FLE_S"),
      (RV32Instr::FLE_S(Rd(X(Fin::new(4))), Rs1(F(Fin::new(3))), Rs2(F(Fin::new(2)))), "FLE_S"),
    ];
    for (instr, name) in x {
      println!("{} = {:#X}", name, instr.encode32());
    }
  }

  #[test]
  fn xlb_step1() {
    let v = vec![
      (0x21F253, "FADD_S"),
      (0x821F253, "FSUB_S"),
      (0x1021F253, "FMUL_S"),
      (0x1821F253, "FDIV_S"),
      (0x20218253, "FSGNJ_S"),
      (0x20219253, "FSGNJN_S"),
      (0x2021A253, "FSGNJX_S"),
      (0x2021A253, "FSGNJX_S"),
      (0x28218253, "FMIN_S"),
      (0x28218253, "FMIN_S"),
      (0x28218253, "FMIN_S"),
      (0x28219253, "FMAX_S"),
      (0x28219253, "FMAX_S"),
      (0x28219253, "FMAX_S"),
      (0xA221A253, "FEQ_S"),
      (0xA221A253, "FEQ_S"),
      (0xA221A253, "FEQ_S"),
      (0xA221A253, "FEQ_S"),
      (0xA2219253, "FLT_S"),
      (0xA2219253, "FLT_S"),
      (0xA2218253, "FLE_S"),
      (0xA2218253, "FLE_S"),
      (0x4021F243, "FMADD_S"),
      (0x4021F247, "FMSUB_S"),
      (0x4021F24B, "FNMSUB_S"),
      (0x4021F24F, "FNMADD_S"),
    ];
    for (_, (raw, name)) in v.into_iter().enumerate() {
      let i = Instr::decode32(raw);
      println!("{} = {:?}", name, i.unwrap());
    }
  }

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
    let v = vec![
      ("0.5", "0.5", "1.0"),
      ("1.0", "0.5", "0.5"),
      ("0.5", "0.5", "0.25"),
      ("0.1", "0.1", "1.0"),
      ("0.5", "0.2", "0.5"),
      ("0.5", "-1.0", "0.5"),
      ("-0.5", "-0.5", "0.5"),
      ("-0.5", "0.5", "-0.5"),
      ("-0.5", "0.5", "-0.5"),
      ("-0.5", "-0.6", "-0.6"),
      ("0.5", "0.6", "0.5"),
      ("-0.5", "-0.6", "-0.5"),
      ("0.5", "0.6", "0.6"),
      ("0.5", "-0.6", "0.5"),
      ("0.5", "0.5", "1"),
      ("0.5", "-0.5", "0"),
      ("-0.5", "0.5", "0"),
      ("0.4", "0.5", "0"),
      ("0.4", "0.5", "1"),
      ("0.5", "0.5", "0"),
      ("0.4", "0.5", "1"),
      ("0.5", "0.5", "1"),
      ("0.5", "0.5", "0.75"),
      ("0.5", "0.5", "-0.25"),
      ("0.5", "0.5", "0.25"),
      ("0.5", "0.5", "-0.75"),
    ];

    let g = x.into_iter().zip(v.into_iter()).collect::<Vec<_>>();
    for ((i, n), (a, b, c)) in g {
      let asm = i.encode32();
      let ii = Instr::decode32(asm).unwrap();
      assert_eq!(Instr::RV32(i), ii);
      println!("{{  {:#X}, \"{}\", {}, {}, {}  }},",
               asm,
               n,
               a, b, c
      );
    }
  }
}
