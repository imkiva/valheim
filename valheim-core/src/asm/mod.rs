use crate::asm::encode::{Encode, EncodeCompressed};
use crate::cpu::data::Either;

pub mod encode;

pub struct Assembler {
  pub code: Vec<Either<u32, u16>>,
}

impl Assembler {
  pub fn emit32<T: Encode>(&mut self, code: T) {
    self.code.push(Either::Left(code.encode()));
  }

  pub fn emit16<T: EncodeCompressed>(&mut self, code: T) {
    self.code.push(Either::Right(code.encode_compressed()));
  }
}
