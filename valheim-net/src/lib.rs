//! Replaceable host networking backends for Valheim.
//!
//! The emulator-facing traits live in `valheim-core`. This crate owns host-side policy and
//! implementations so a future native Rust NAT can replace passt without changing virtio-net.

mod config;

pub use config::{
  Ipv4Subnet, NetworkConfig, SubnetParseError, DEFAULT_GUEST_MAC, DEFAULT_MTU, DEFAULT_SUBNET_CIDR,
};

#[cfg(target_os = "linux")]
mod passt;
#[cfg(target_os = "linux")]
pub use passt::PasstBackendFactory;
