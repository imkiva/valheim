#[cfg(test)]
mod masm {
  use crate::asm::{a0, Assembler, Compare, t0, t1};
  use crate::isa::typed::Instr;

  pub fn decode_encode(di: u32) {
    let d = Instr::decode32(di);
    let mut a = Assembler::new(0);
    a.emit32(d.unwrap());
    let ei = a.code.read_u32().unwrap();
    println!("di = {:#b}", di);
    println!("ei = {:#b}", ei);
    let e = Instr::decode32(ei);
    println!("d = {:?}", d);
    println!("e = {:?}", e);
    assert_eq!(di, ei);
    assert_eq!(d, e);
  }

  #[test]
  pub fn encode_j_type() {
    decode_encode(0b_1_1111110100_1_11111111_00000_1101111); // jal x0, -6*4
  }

  #[test]
  pub fn encode_b_type() {
    decode_encode(0b11111110011000101000110011100011); // beq, t0, t1, -8
  }

  #[test]
  pub fn encode_u_type() {
    decode_encode(0x00110837); // li, x16, 1114112
  }

  #[test]
  pub fn sum() {
    let mut asm = Assembler::new(0);
    // int sum = 0;
    // int i = 0;
    // while (i <= 100)
    //   sum = sum + i;
    // print(sum);

    // register allocation
    let sum = a0;
    let i = t0;
    let tmp = t1;
    // int sum = 0;
    // int i = 0;
    // int tmp = 101;
    // begin:
    //   if (i >= tmp) goto end;
    //   sum = sum + i;
    //   goto begin;
    // end:
    // print(sum);
    let begin = asm.new_label();
    let end = asm.new_label();
    asm.load_imm(sum, 0);
    asm.load_imm(i, 0);
    asm.load_imm(tmp, 101);
    asm.emit_label(begin);
    asm.branch(Compare::GE, i, tmp, end);
    asm.add(sum, sum, i);
    asm.jump(begin);
    asm.emit_label(end);
    // done, freezing the holes
    asm.freeze();
    // get the code
    asm.code.set_rpos(0);
    let mut pc = 0;
    while let Ok(raw) = asm.code.read_u32() {
      let inst = Instr::decode32(raw);
      println!("{:#010X}  {:#010x}   {:?}", pc, raw, inst.unwrap());
      pc += 4;
    }
  }
}
