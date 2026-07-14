#[cfg(not(all(target_arch = "x86_64", target_os = "linux")))]
compile_error!("valheim-jit supports only Linux x86_64 with the System V ABI");

mod atomic;
mod block;
mod cranelift;
mod memory;
mod runtime;

pub use block::{BlockBuild, FallbackKind, GuestBlock, GuestInst, MAX_BLOCK_LEN};
pub use runtime::{JitExecutor, JitStats};
