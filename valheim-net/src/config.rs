use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::net::Ipv4Addr;
use std::str::FromStr;

pub const DEFAULT_SUBNET_CIDR: &str = "10.172.0.0/16";
pub const DEFAULT_GUEST_MAC: [u8; 6] = [0x52, 0x54, 0x00, 0x12, 0x34, 0x56];
pub const DEFAULT_MTU: u16 = 1500;

/// A canonical RFC 1918 IPv4 network large enough for Valheim's fixed address layout.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Ipv4Subnet {
  network: Ipv4Addr,
  prefix_len: u8,
}

impl Ipv4Subnet {
  /// The network must be fully contained in `10.0.0.0/8`, `172.16.0.0/12`, or
  /// `192.168.0.0/16`. The `/27` upper bound leaves at least 32 addresses, including the fixed
  /// `network + 15` guest address.
  pub fn new(network: Ipv4Addr, prefix_len: u8) -> Result<Self, SubnetParseError> {
    if prefix_len > 27 {
      return Err(SubnetParseError::PrefixTooLong(prefix_len));
    }

    let address = u32::from(network);
    let mask = prefix_mask(prefix_len);
    if address & mask != address {
      return Err(SubnetParseError::NonCanonicalNetwork {
        supplied: network,
        canonical: Ipv4Addr::from(address & mask),
        prefix_len,
      });
    }

    if !is_private_network(address, prefix_len) {
      return Err(SubnetParseError::OutsidePrivateRange {
        network,
        prefix_len,
      });
    }

    Ok(Self {
      network,
      prefix_len,
    })
  }

  pub fn network(self) -> Ipv4Addr {
    self.network
  }

  pub fn prefix_len(self) -> u8 {
    self.prefix_len
  }

  pub fn address_at(self, offset: u32) -> Option<Ipv4Addr> {
    let host_count = 1u64 << (32 - self.prefix_len);
    if u64::from(offset) >= host_count {
      return None;
    }
    u32::from(self.network)
      .checked_add(offset)
      .map(Ipv4Addr::from)
  }
}

impl Default for Ipv4Subnet {
  fn default() -> Self {
    DEFAULT_SUBNET_CIDR
      .parse()
      .expect("the built-in Valheim subnet must be valid")
  }
}

impl Display for Ipv4Subnet {
  fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
    write!(formatter, "{}/{}", self.network, self.prefix_len)
  }
}

impl FromStr for Ipv4Subnet {
  type Err = SubnetParseError;

  fn from_str(value: &str) -> Result<Self, Self::Err> {
    let (address, prefix) = value
      .split_once('/')
      .ok_or(SubnetParseError::MissingPrefix)?;
    if address.is_empty() || prefix.is_empty() || prefix.contains('/') {
      return Err(SubnetParseError::InvalidCidr);
    }
    let address = address
      .parse::<Ipv4Addr>()
      .map_err(|_| SubnetParseError::InvalidAddress)?;
    let prefix = prefix
      .parse::<u8>()
      .map_err(|_| SubnetParseError::InvalidPrefix)?;
    Self::new(address, prefix)
  }
}

fn prefix_mask(prefix_len: u8) -> u32 {
  if prefix_len == 0 {
    0
  } else {
    u32::MAX << (32 - prefix_len)
  }
}

fn is_private_network(address: u32, prefix_len: u8) -> bool {
  const PRIVATE_10: u32 = 0x0a00_0000;
  const PRIVATE_172: u32 = 0xac10_0000;
  const PRIVATE_192: u32 = 0xc0a8_0000;

  (prefix_len >= 8 && address & prefix_mask(8) == PRIVATE_10)
    || (prefix_len >= 12 && address & prefix_mask(12) == PRIVATE_172)
    || (prefix_len >= 16 && address & prefix_mask(16) == PRIVATE_192)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SubnetParseError {
  MissingPrefix,
  InvalidCidr,
  InvalidAddress,
  InvalidPrefix,
  PrefixTooLong(u8),
  NonCanonicalNetwork {
    supplied: Ipv4Addr,
    canonical: Ipv4Addr,
    prefix_len: u8,
  },
  OutsidePrivateRange {
    network: Ipv4Addr,
    prefix_len: u8,
  },
}

impl Display for SubnetParseError {
  fn fmt(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::MissingPrefix => write!(formatter, "IPv4 subnet must use CIDR notation"),
      Self::InvalidCidr => write!(formatter, "invalid IPv4 CIDR subnet"),
      Self::InvalidAddress => write!(formatter, "invalid IPv4 network address"),
      Self::InvalidPrefix => write!(formatter, "invalid IPv4 prefix length"),
      Self::PrefixTooLong(prefix) => write!(
        formatter,
        "IPv4 prefix /{prefix} is too long; the maximum is /27"
      ),
      Self::NonCanonicalNetwork {
        supplied,
        canonical,
        prefix_len,
      } => write!(
        formatter,
        "{supplied}/{prefix_len} is not a network address; use {canonical}/{prefix_len}"
      ),
      Self::OutsidePrivateRange {
        network,
        prefix_len,
      } => write!(
        formatter,
        "{network}/{prefix_len} is not fully contained in an RFC 1918 private network"
      ),
    }
  }
}

impl Error for SubnetParseError {}

/// Guest-visible IPv4 configuration advertised by the selected backend.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NetworkConfig {
  subnet: Ipv4Subnet,
  gateway: Ipv4Addr,
  dns: Ipv4Addr,
  guest: Ipv4Addr,
  guest_mac: [u8; 6],
  mtu: u16,
}

impl NetworkConfig {
  pub fn new(subnet: Ipv4Subnet) -> Self {
    Self {
      subnet,
      gateway: subnet
        .address_at(2)
        .expect("a valid Valheim subnet contains network + 2"),
      dns: subnet
        .address_at(3)
        .expect("a valid Valheim subnet contains network + 3"),
      guest: subnet
        .address_at(15)
        .expect("a valid Valheim subnet contains network + 15"),
      guest_mac: DEFAULT_GUEST_MAC,
      mtu: DEFAULT_MTU,
    }
  }

  pub fn subnet(self) -> Ipv4Subnet {
    self.subnet
  }

  pub fn gateway(self) -> Ipv4Addr {
    self.gateway
  }

  pub fn dns(self) -> Ipv4Addr {
    self.dns
  }

  pub fn guest(self) -> Ipv4Addr {
    self.guest
  }

  pub fn guest_mac(self) -> [u8; 6] {
    self.guest_mac
  }

  pub fn mtu(self) -> u16 {
    self.mtu
  }
}

impl Default for NetworkConfig {
  fn default() -> Self {
    Self::new(Ipv4Subnet::default())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn default_layout_uses_requested_network() {
    let config = NetworkConfig::default();
    assert_eq!(config.subnet.to_string(), DEFAULT_SUBNET_CIDR);
    assert_eq!(config.gateway, Ipv4Addr::new(10, 172, 0, 2));
    assert_eq!(config.dns, Ipv4Addr::new(10, 172, 0, 3));
    assert_eq!(config.guest, Ipv4Addr::new(10, 172, 0, 15));
    assert_eq!(config.guest_mac, DEFAULT_GUEST_MAC);
    assert_eq!(config.mtu, 1500);
  }

  #[test]
  fn custom_subnet_derives_addresses_from_network() {
    let subnet: Ipv4Subnet = "192.168.64.0/24".parse().unwrap();
    let config = NetworkConfig::new(subnet);
    assert_eq!(config.gateway, Ipv4Addr::new(192, 168, 64, 2));
    assert_eq!(config.dns, Ipv4Addr::new(192, 168, 64, 3));
    assert_eq!(config.guest, Ipv4Addr::new(192, 168, 64, 15));
  }

  #[test]
  fn slash_27_still_contains_all_fixed_addresses() {
    let subnet: Ipv4Subnet = "172.20.4.32/27".parse().unwrap();
    assert_eq!(subnet.address_at(31), Some(Ipv4Addr::new(172, 20, 4, 63)));
    assert_eq!(subnet.address_at(32), None);
    assert_eq!(
      NetworkConfig::new(subnet).guest,
      Ipv4Addr::new(172, 20, 4, 47)
    );
  }

  #[test]
  fn accepts_rfc1918_boundaries() {
    for subnet in [
      "10.0.0.0/8",
      "10.255.255.224/27",
      "172.16.0.0/12",
      "172.31.255.224/27",
      "192.168.0.0/16",
      "192.168.255.224/27",
    ] {
      assert!(subnet.parse::<Ipv4Subnet>().is_ok(), "rejected {subnet}");
    }
  }

  #[test]
  fn rejects_networks_not_fully_contained_in_rfc1918_space() {
    for subnet in [
      "0.0.0.0/0",
      "10.0.0.0/7",
      "100.64.0.0/10",
      "127.0.0.0/8",
      "172.0.0.0/11",
      "192.0.0.0/15",
      "192.169.0.0/16",
      "224.0.0.0/4",
    ] {
      assert!(
        matches!(
          subnet.parse::<Ipv4Subnet>(),
          Err(SubnetParseError::OutsidePrivateRange { .. })
        ),
        "accepted {subnet}"
      );
    }
  }

  #[test]
  fn rejects_noncanonical_network_address() {
    assert_eq!(
      "10.172.1.5/16".parse::<Ipv4Subnet>(),
      Err(SubnetParseError::NonCanonicalNetwork {
        supplied: Ipv4Addr::new(10, 172, 1, 5),
        canonical: Ipv4Addr::new(10, 172, 0, 0),
        prefix_len: 16,
      })
    );
  }

  #[test]
  fn rejects_prefixes_that_cannot_hold_fixed_layout() {
    assert_eq!(
      "10.0.0.0/28".parse::<Ipv4Subnet>(),
      Err(SubnetParseError::PrefixTooLong(28))
    );
  }

  #[test]
  fn rejects_malformed_cidr() {
    assert_eq!(
      "10.0.0.0".parse::<Ipv4Subnet>(),
      Err(SubnetParseError::MissingPrefix)
    );
    assert_eq!(
      "10.0.0.0/24/1".parse::<Ipv4Subnet>(),
      Err(SubnetParseError::InvalidCidr)
    );
    assert_eq!(
      "not-an-ip/24".parse::<Ipv4Subnet>(),
      Err(SubnetParseError::InvalidAddress)
    );
    assert_eq!(
      "10.0.0.0/nope".parse::<Ipv4Subnet>(),
      Err(SubnetParseError::InvalidPrefix)
    );
  }
}
