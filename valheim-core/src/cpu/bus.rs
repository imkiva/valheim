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
pub const RV64_MEMORY_SIZE: u64 = 4 * 1024 * 1024 * 1024;
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

  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Result<T, Exception> {
    // fast-path for builtin io devices
    match addr.0 {
      RV64_MEMORY_BASE..=RV64_MEMORY_END => self.mem.read::<T>(addr).ok_or(Exception::LoadAccessFault(addr)),
      VIRT_MROM_BASE..=VIRT_MROM_END => self.device_tree.read::<T>(addr).ok_or(Exception::LoadAccessFault(addr)),
      CLINT_BASE..=CLINT_END => Ok(Bus::safe_reinterpret_as_T(self.clint.read::<T>(addr)?)),
      PLIC_BASE..=PLIC_END => {
        assert_eq!(std::mem::size_of::<T>(), 4);
        let val = self.plic.read(addr)?;
        Ok(Bus::safe_reinterpret_as_T(val as u64))
      }
      VIRTIO_BASE..=VIRTIO_END => Ok(Bus::safe_reinterpret_as_T(self.virtio.read::<T>(addr)? as u64)),

      _ => match self.select_device_for_read(addr) {
        Some(dev) => match std::mem::size_of::<T>() {
          1 => Ok(Bus::safe_reinterpret_as_T(dev.read(addr).ok_or(Exception::LoadAccessFault(addr))? as u64)),
          2 => Ok(Bus::safe_reinterpret_as_T(dev.read16(addr).ok_or(Exception::LoadAccessFault(addr))? as u64)),
          4 => Ok(Bus::safe_reinterpret_as_T(dev.read32(addr).ok_or(Exception::LoadAccessFault(addr))? as u64)),
          8 => Ok(Bus::safe_reinterpret_as_T(dev.read64(addr).ok_or(Exception::LoadAccessFault(addr))? as u64)),
          _ => Err(Exception::LoadAccessFault(addr)),
        }
        None => Err(Exception::LoadAccessFault(addr)),
      },
    }
  }

  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, val: T) -> Result<(), Exception> {
    // fast-path for builtin io devices
    match addr.0 {
      RV64_MEMORY_BASE..=RV64_MEMORY_END => self.mem.write::<T>(addr, val).ok_or(Exception::StoreAccessFault(addr)),
      VIRT_MROM_BASE..=VIRT_MROM_END => self.device_tree.write::<T>(addr, val).ok_or(Exception::StoreAccessFault(addr)),
      CLINT_BASE..=CLINT_END => self.clint.write::<T>(addr, Bus::safe_reinterpret_as_u64(val)),
      PLIC_BASE..=PLIC_END => {
        assert_eq!(std::mem::size_of::<T>(), 4);
        let val = Bus::safe_reinterpret_as_u64(val) as u32;
        self.plic.write(addr, val)
      }
      VIRTIO_BASE..=VIRTIO_END => self.virtio.write::<T>(addr, Bus::safe_reinterpret_as_u64(val) as u32),

      _ => match self.select_device_for_write(addr) {
        Some(dev) => match std::mem::size_of::<T>() {
          1 => dev.write(addr, Bus::safe_reinterpret_as_u64(val) as u8).map_err(|_| Exception::StoreAccessFault(addr)),
          2 => dev.write16(addr, Bus::safe_reinterpret_as_u64(val) as u16).map_err(|_| Exception::StoreAccessFault(addr)),
          4 => dev.write32(addr, Bus::safe_reinterpret_as_u64(val) as u32).map_err(|_| Exception::StoreAccessFault(addr)),
          8 => dev.write64(addr, Bus::safe_reinterpret_as_u64(val) as u64).map_err(|_| Exception::StoreAccessFault(addr)),
          _ => Err(Exception::StoreAccessFault(addr)),
        }
        None => Err(Exception::StoreAccessFault(addr)),
      },
    }
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

  fn select_device_for_read(&self, addr: VirtAddr) -> Option<Arc<dyn Device>> {
    for ((base, end), dev_id) in self.io_map.iter() {
      if addr >= *base && addr < *end {
        return match self.devices.get(*dev_id) {
          Some(dev) => Some(dev.clone()),
          None => None,
        };
      }
    }
    None
  }

  fn select_device_for_write(&mut self, addr: VirtAddr) -> Option<Arc<dyn Device>> {
    for ((base, end), dev_id) in self.io_map.iter_mut() {
      if addr >= *base && addr <= *end {
        return match self.devices.get(*dev_id) {
          Some(dev) => Some(dev.clone()),
          None => None,
        };
      }
    }
    None
  }
}
