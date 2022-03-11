#![allow(incomplete_features)]

#![feature(core_intrinsics)]
#![feature(generic_const_exprs)]
#![feature(inline_const_pat)]

pub mod cpu;
pub mod isa;
pub mod memory;
pub mod interp;

pub mod debug;

#[macro_use]
extern crate derive_more;
extern crate core;
