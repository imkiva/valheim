use crate::asm::encode::{Encode32, Encode16};
use crate::cpu::data::Either;

pub mod encode;
pub mod test;

pub struct Assembler {
  pub code: Vec<Either<u32, u16>>,
}

impl Assembler {
  pub fn emit32<T: Encode32>(&mut self, code: T) {
    self.code.push(Either::Left(code.encode32()));
  }

  pub fn emit16<T: Encode16>(&mut self, code: T) {
    self.code.push(Either::Right(code.encode16()));
  }
}
