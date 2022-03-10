#![feature(core_intrinsics)]
#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(inline_const_pat)]

pub mod cpu;
pub mod isa;
pub mod memory;
pub mod interp;

#[macro_use]
extern crate derive_more;
extern crate core;
