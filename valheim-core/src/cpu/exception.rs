use crate::isa::untyped::Bytecode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Exception {
  IllegalInstruction(Bytecode),
}
