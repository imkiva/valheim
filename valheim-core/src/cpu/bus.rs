use std::collections::BTreeMap;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

use crate::cpu::irq::Exception;
use crate::device::clint::Clint;
use crate::device::Device;
use crate::device::plic::Plic;
use crate::device::virtio::Virtio;
use crate::memory::{CanIO, Memory, VirtAddr};

pub const RV64_MEMORY_BASE: u64 = 0x80000000;
pub const RV64_MEMORY_SIZE: u64 = 256 * 1024 * 1024;
pub const RV64_MEMORY_END: u64 = RV64_MEMORY_BASE + RV64_MEMORY_SIZE;

pub const VIRT_MROM_BASE: u64 = 0x1000;
pub const VIRT_MROM_SIZE: u64 = 0xf000;
pub const VIRT_MROM_END: u64 = VIRT_MROM_BASE + VIRT_MROM_SIZE;

pub const CLINT_BASE: u64 = 0x2000000;
pub const CLINT_SIZE: u64 = 0x10000;
pub const CLINT_END: u64 = CLINT_BASE + CLINT_SIZE;

pub const PLIC_BASE: u64 = 0xc00_0000;
pub const PLIC_SIZE: u64 = 0x208000;
pub const PLIC_END: u64 = PLIC_BASE + PLIC_SIZE;

/// The address which virtio starts.
pub const VIRTIO_BASE: u64 = 0x1000_1000;
pub const VIRTIO_SIZE: u64 = 0x1000;
pub const VIRTIO_END: u64 = VIRTIO_BASE + VIRTIO_SIZE;

const HOST_PAGE_SIZE: usize = 4096;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DramHostPage {
  pub host_page: *mut u8,
  pub phys_page: u64,
}

/// System Bus, which handles DRAM access and memory-mapped IO.
/// https://github.com/qemu/qemu/blob/master/hw/riscv/virt.c
/// Builtin IO maps:
/// - 0x1000      - 0x1000 + 0xf000       ==== Virt_MROM, like device trees
/// - 0x0x2000000 - 0x2000000 + 0x10000   ==== CLINT
pub struct Bus {
  pub mem: Memory,
  pub devices: Vec<Arc<dyn Device>>,
  pub io_map: BTreeMap<(VirtAddr, VirtAddr), usize>,

  // Builtin IO devices
  pub device_tree: Memory,
  pub clint: Clint,
  pub plic: Plic,
  pub virtio: Virtio,
}

impl Debug for Bus {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "Bus {{ mem: {:?}, devices: {:?} }}",
      self.mem,
      self.devices.iter().map(|dev| dev.name()).collect::<Vec<_>>(),
    )
  }
}

impl Bus {
  pub fn new() -> Result<Bus, std::io::Error> {
    Ok(Bus {
      mem: Memory::new(RV64_MEMORY_BASE, RV64_MEMORY_SIZE as usize)?,
      devices: Vec::with_capacity(8),
      io_map: BTreeMap::new(),
      device_tree: Memory::new(VIRT_MROM_BASE, VIRT_MROM_SIZE as usize)?,
      clint: Clint::new(),
      plic: Plic::new(),
      virtio: Virtio::new(0),
    })
  }

  pub unsafe fn add_device(&mut self, device: Arc<dyn Device>) -> Result<(), ()> {
    let ranges = device.init()?;
    let idx = self.devices.len();
    self.devices.push(device);
    for range in ranges {
      self.io_map.insert(range, idx);
    }
    Ok(())
  }

  pub fn halt(&mut self) {
    self.devices.iter().for_each(|dev| match dev.destroy() {
      Ok(_) => (),
      Err(_) => eprintln!("Error destroying device: {}", (*dev).name()),
    });
  }

  /// Returns the host page backing a DRAM address. Device and unmapped addresses never receive a
  /// direct pointer and must continue through `read`/`write` so their side effects are preserved.
  #[inline(always)]
  pub fn dram_host_page(&self, addr: VirtAddr) -> Option<DramHostPage> {
    let phys_page = addr.0 & !((HOST_PAGE_SIZE as u64) - 1);
    let host_page = self
      .mem
      .to_phys_with_width(VirtAddr(phys_page), HOST_PAGE_SIZE)?
      .0;
    Some(DramHostPage {
      host_page,
      phys_page,
    })
  }

  /// Checks whether a write would be routed by the bus without performing it.
  ///
  /// This validates mapped ranges and the access widths accepted by the bus-level endpoints. It
  /// intentionally does not call a device, so probing cannot trigger MMIO side effects.
  pub(crate) fn probe_write(&self, addr: VirtAddr, width: usize) -> Result<(), Exception> {
    if self.mem.contains(addr, width) || self.device_tree.contains(addr, width) {
      return Ok(());
    }
    if Bus::range_contains(CLINT_BASE, CLINT_END, addr, width) {
      return Ok(());
    }
    if Bus::range_contains(PLIC_BASE, PLIC_END, addr, width) {
      return if width == 4 {
        Ok(())
      } else {
        Err(Exception::StoreAccessFault(addr))
      };
    }
    if Bus::range_contains(VIRTIO_BASE, VIRTIO_END, addr, width) {
      return if matches!(width, 1 | 2 | 4) {
        Ok(())
      } else {
        Err(Exception::StoreAccessFault(addr))
      };
    }

    let mapped_device = self.io_map.iter().any(|((base, end), dev_id)| {
      self.devices.get(*dev_id).is_some() && Bus::range_contains(base.0, end.0, addr, width)
    });
    if mapped_device && matches!(width, 1 | 2 | 4 | 8) {
      Ok(())
    } else {
      Err(Exception::StoreAccessFault(addr))
    }
  }

  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Result<T, Exception> {
    let width = std::mem::size_of::<T>();
    if let Some(value) = self.mem.read::<T>(addr) {
      return Ok(value);
    }
    if let Some(value) = self.device_tree.read::<T>(addr) {
      return Ok(value);
    }
    if Bus::range_contains(CLINT_BASE, CLINT_END, addr, width) {
      return Ok(Bus::safe_reinterpret_as_T(self.clint.read::<T>(addr)?));
    }
    if Bus::range_contains(PLIC_BASE, PLIC_END, addr, width) {
      if width != 4 {
        return Err(Exception::LoadAccessFault(addr));
      }
      let val = self.plic.read(addr)?;
      return Ok(Bus::safe_reinterpret_as_T(val as u64));
    }
    if Bus::range_contains(VIRTIO_BASE, VIRTIO_END, addr, width) {
      return Ok(Bus::safe_reinterpret_as_T(
        self.virtio.read::<T>(addr)? as u64
      ));
    }

    match self.select_device_for_read(addr, width) {
      Some(dev) => match width {
        1 => Ok(Bus::safe_reinterpret_as_T(
          dev.read(addr).ok_or(Exception::LoadAccessFault(addr))? as u64,
        )),
        2 => Ok(Bus::safe_reinterpret_as_T(
          dev.read16(addr).ok_or(Exception::LoadAccessFault(addr))? as u64,
        )),
        4 => Ok(Bus::safe_reinterpret_as_T(
          dev.read32(addr).ok_or(Exception::LoadAccessFault(addr))? as u64,
        )),
        8 => Ok(Bus::safe_reinterpret_as_T(
          dev.read64(addr).ok_or(Exception::LoadAccessFault(addr))? as u64,
        )),
        _ => Err(Exception::LoadAccessFault(addr)),
      },
      None => Err(Exception::LoadAccessFault(addr)),
    }
  }

  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, val: T) -> Result<(), Exception> {
    let width = std::mem::size_of::<T>();
    if self.mem.write::<T>(addr, val).is_some() {
      return Ok(());
    }
    if self.device_tree.write::<T>(addr, val).is_some() {
      return Ok(());
    }
    if Bus::range_contains(CLINT_BASE, CLINT_END, addr, width) {
      return self
        .clint
        .write::<T>(addr, Bus::safe_reinterpret_as_u64(val));
    }
    if Bus::range_contains(PLIC_BASE, PLIC_END, addr, width) {
      if width != 4 {
        return Err(Exception::StoreAccessFault(addr));
      }
      let val = Bus::safe_reinterpret_as_u64(val) as u32;
      return self.plic.write(addr, val);
    }
    if Bus::range_contains(VIRTIO_BASE, VIRTIO_END, addr, width) {
      return self
        .virtio
        .write::<T>(addr, Bus::safe_reinterpret_as_u64(val) as u32);
    }

    match self.select_device_for_write(addr, width) {
      Some(dev) => match width {
        1 => dev
          .write(addr, Bus::safe_reinterpret_as_u64(val) as u8)
          .map_err(|_| Exception::StoreAccessFault(addr)),
        2 => dev
          .write16(addr, Bus::safe_reinterpret_as_u64(val) as u16)
          .map_err(|_| Exception::StoreAccessFault(addr)),
        4 => dev
          .write32(addr, Bus::safe_reinterpret_as_u64(val) as u32)
          .map_err(|_| Exception::StoreAccessFault(addr)),
        8 => dev
          .write64(addr, Bus::safe_reinterpret_as_u64(val) as u64)
          .map_err(|_| Exception::StoreAccessFault(addr)),
        _ => Err(Exception::StoreAccessFault(addr)),
      },
      None => Err(Exception::StoreAccessFault(addr)),
    }
  }

  fn range_contains(base: u64, end: u64, addr: VirtAddr, width: usize) -> bool {
    let Some(offset) = addr.0.checked_sub(base) else {
      return false;
    };
    let Some(range_size) = end.checked_sub(base) else {
      return false;
    };
    let Ok(width) = u64::try_from(width) else {
      return false;
    };
    offset <= range_size && width <= range_size - offset
  }

  #[allow(non_snake_case)]
  fn safe_reinterpret_as_T<T: CanIO>(val: u64) -> T {
    // CanIO trait guarantees that the transmute is safe
    debug_assert!(std::mem::size_of::<T>() <= std::mem::size_of::<u64>());
    unsafe { *std::mem::transmute::<*const u64, *const T>(&val as *const u64) }
  }

  fn safe_reinterpret_as_u64<T: CanIO>(val: T) -> u64 {
    // CanIO trait guarantees that the transmute is safe
    match std::mem::size_of::<T>() {
      1 => (unsafe { *std::mem::transmute::<*const T, *const u8>(&val as *const T) }) as u64,
      2 => (unsafe { *std::mem::transmute::<*const T, *const u16>(&val as *const T) }) as u64,
      4 => (unsafe { *std::mem::transmute::<*const T, *const u32>(&val as *const T) }) as u64,
      8 => (unsafe { *std::mem::transmute::<*const T, *const u64>(&val as *const T) }),
      _ => panic!("Invalid size for CanIO trait"),
    }
  }

  fn select_device_for_read(&self, addr: VirtAddr, width: usize) -> Option<Arc<dyn Device>> {
    for ((base, end), dev_id) in self.io_map.iter() {
      if Bus::range_contains(base.0, end.0, addr, width) {
        return match self.devices.get(*dev_id) {
          Some(dev) => Some(dev.clone()),
          None => None,
        };
      }
    }
    None
  }

  fn select_device_for_write(&mut self, addr: VirtAddr, width: usize) -> Option<Arc<dyn Device>> {
    for ((base, end), dev_id) in self.io_map.iter_mut() {
      if Bus::range_contains(base.0, end.0, addr, width) {
        return match self.devices.get(*dev_id) {
          Some(dev) => Some(dev.clone()),
          None => None,
        };
      }
    }
    None
  }
}

#[cfg(test)]
mod tests {
  use super::{Bus, RV64_MEMORY_BASE, RV64_MEMORY_END};
  use crate::cpu::irq::Exception;
  use crate::memory::VirtAddr;

  #[test]
  fn dram_accesses_use_half_open_width_checked_ranges() {
    let mut bus = Bus::new().unwrap();
    let last = VirtAddr(RV64_MEMORY_END - 1);

    assert_eq!(bus.write(last, 0x5a_u8), Ok(()));
    assert_eq!(bus.read::<u8>(last), Ok(0x5a));
    assert_eq!(bus.read::<u16>(last), Err(Exception::LoadAccessFault(last)));
    assert_eq!(
      bus.write(last, 0x1234_u16),
      Err(Exception::StoreAccessFault(last))
    );
    assert_eq!(
      bus.read::<u8>(VirtAddr(RV64_MEMORY_END)),
      Err(Exception::LoadAccessFault(VirtAddr(RV64_MEMORY_END))),
    );
  }

  #[test]
  fn only_dram_addresses_get_direct_host_pages() {
    let bus = Bus::new().unwrap();
    let addr = VirtAddr(RV64_MEMORY_BASE + 0x1234);
    let mapping = bus.dram_host_page(addr).unwrap();

    assert_eq!(mapping.phys_page, RV64_MEMORY_BASE + 0x1000);
    assert_eq!(
      mapping.host_page,
      bus
        .mem
        .to_phys(VirtAddr(RV64_MEMORY_BASE + 0x1000))
        .unwrap()
        .0,
    );
    assert!(bus.dram_host_page(VirtAddr(0x1000)).is_none());
    assert!(bus.dram_host_page(VirtAddr(RV64_MEMORY_END)).is_none());
  }

  #[test]
  fn write_probe_is_side_effect_free_and_rejects_unmapped_ranges() {
    let mut bus = Bus::new().unwrap();
    let last = VirtAddr(RV64_MEMORY_END - 1);
    bus.write(last, 0x5a_u8).unwrap();

    assert_eq!(bus.probe_write(last, 1), Ok(()));
    assert_eq!(bus.read::<u8>(last), Ok(0x5a));
    assert_eq!(
      bus.probe_write(VirtAddr(RV64_MEMORY_END), 1),
      Err(Exception::StoreAccessFault(VirtAddr(RV64_MEMORY_END))),
    );
  }
}
