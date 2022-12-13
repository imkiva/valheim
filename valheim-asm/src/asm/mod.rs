#![allow(non_upper_case_globals)]

use std::collections::HashMap;

use bytebuffer::{ByteBuffer, Endian};

use crate::asm::encode16::Encode16;
use crate::asm::encode32::Encode32;
use crate::isa::data::Fin;
use crate::isa::typed::{Instr, Reg};

pub mod encode32;
pub mod encode16;
pub mod traits;
pub mod riscv;
pub mod test;

pub const zero: Reg = Reg::ZERO;
pub const ra: Reg = Reg::X(Fin::new(1));
pub const sp: Reg = Reg::X(Fin::new(2));
pub const gp: Reg = Reg::X(Fin::new(3));
pub const tp: Reg = Reg::X(Fin::new(4));
pub const t0: Reg = Reg::X(Fin::new(5));
pub const t1: Reg = Reg::X(Fin::new(6));
pub const t2: Reg = Reg::X(Fin::new(7));
pub const s0: Reg = Reg::X(Fin::new(8));
pub const s1: Reg = Reg::X(Fin::new(9));
pub const a0: Reg = Reg::X(Fin::new(10));
pub const a1: Reg = Reg::X(Fin::new(11));
pub const a2: Reg = Reg::X(Fin::new(12));
pub const a3: Reg = Reg::X(Fin::new(13));
pub const a4: Reg = Reg::X(Fin::new(14));
pub const a5: Reg = Reg::X(Fin::new(15));
pub const a6: Reg = Reg::X(Fin::new(16));
pub const a7: Reg = Reg::X(Fin::new(17));
pub const s2: Reg = Reg::X(Fin::new(18));
pub const s3: Reg = Reg::X(Fin::new(19));
pub const s4: Reg = Reg::X(Fin::new(20));
pub const s5: Reg = Reg::X(Fin::new(21));
pub const s6: Reg = Reg::X(Fin::new(22));
pub const s7: Reg = Reg::X(Fin::new(23));
pub const s8: Reg = Reg::X(Fin::new(24));
pub const s9: Reg = Reg::X(Fin::new(25));
pub const s10: Reg = Reg::X(Fin::new(26));
pub const s11: Reg = Reg::X(Fin::new(27));
pub const t3: Reg = Reg::X(Fin::new(28));
pub const t4: Reg = Reg::X(Fin::new(29));
pub const t5: Reg = Reg::X(Fin::new(30));
pub const t6: Reg = Reg::X(Fin::new(31));

pub struct Assembler {
  pub base: usize,
  pub code: ByteBuffer,
  pub label_id: usize,
  pub labels: HashMap<LabelId, Label>,
  pub backfills: HashMap<LabelId, (Current, Box<dyn FnOnce(Label, Current) -> Instr>)>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Label {
  /// The label position
  pub position: isize,
}

#[derive(Debug, Copy, Clone)]
pub struct Current {
  /// Current position
  pub position: isize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct LabelId {
  pub idx: usize,
}

#[derive(Debug, Copy, Clone)]
pub enum Compare {
  EQ,
  NE,
  LT,
  GE,
  LTU,
  GEU,
}

/// Common assembler
impl Assembler {
  pub fn new(base: usize) -> Assembler {
    let mut asm = Assembler {
      base,
      code: ByteBuffer::new(),
      backfills: HashMap::new(),
      labels: HashMap::new(),
      label_id: 0,
    };
    asm.code.set_endian(Endian::LittleEndian);
    asm
  }

  pub fn current(&self) -> Current {
    Current { position: self.code.get_wpos() as isize }
  }

  pub fn new_label(&mut self) -> LabelId {
    let id = LabelId { idx: self.label_id };
    self.label_id += 1;
    id
  }

  pub fn emit_label(&mut self, id: LabelId) {
    let label = Label { position: self.current().position };
    self.labels.insert(id, label);
  }

  pub fn backfill(&mut self, id: LabelId, compute: Box<dyn FnOnce(Label, Current) -> Instr>) {
    self.backfills.insert(id, (self.current(), compute));
    self.code.write_u32(0); // the placeholder
  }

  pub fn freeze(&mut self) {
    for (id, label) in self.labels.iter() {
      if let Some((their, compute)) = self.backfills.remove(id) {
        let instr = compute(label.clone(), their);
        let current = self.current();
        self.code.set_wpos(their.position as usize);
        self.code.write_u32(instr.encode32());
        self.code.set_wpos(current.position as usize);
      }
    }
  }

  pub fn emit32<T: Encode32>(&mut self, code: T) {
    self.code.write_u32(code.encode32());
  }

  pub fn emit16<T: Encode16>(&mut self, code: T) {
    self.code.write_u16(code.encode16());
  }
}
