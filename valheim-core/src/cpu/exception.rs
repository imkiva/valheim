use crate::isa::compressed::untyped::Bytecode16;
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Exception {
  IllegalInstruction(VirtAddr, Bytecode, Bytecode16),
  LoadAccessFault(VirtAddr),
  StoreAccessFault(VirtAddr),
  LoadAddressMisaligned(VirtAddr),
  // TODO: remove this hack
  ValheimEbreak,
}
