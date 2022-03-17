use std::collections::BTreeMap;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;
use crate::cpu::exception::Exception;
use crate::device::Device;
use crate::memory::{CanIO, Memory, VirtAddr};

const RV64_MEMORY_BASE: u64 = 0x80000000;

const VIRT_MROM_BASE: u64 = 0x1000;
const VIRT_MROM_END : u64 = VIRT_MROM_BASE + 0xf000;

/// System Bus, which handles DRAM access and memory-mapped IO.
/// https://github.com/qemu/qemu/blob/master/hw/riscv/virt.c
/// Builtin IO maps:
/// - 0x1000 - 0x1000 + 0xf000 : Virt_MROM, like device trees
pub struct Bus {
  pub mem: Memory,
  pub devices: Vec<Arc<dyn Device>>,
  pub io_map: BTreeMap<(VirtAddr, VirtAddr), usize>,
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
  pub fn new(memory_size: usize) -> Result<Bus, std::io::Error> {
    let mem = Memory::new(RV64_MEMORY_BASE, memory_size)?;
    Ok(Bus {
      mem,
      devices: Vec::with_capacity(8),
      io_map: BTreeMap::new(),
    })
  }

  pub unsafe fn add_device(&mut self, device: Arc<dyn Device>) -> Result<(), ()> {
    let ranges =  device.init()?;
    let idx = self.devices.len();
    self.devices.push(device);
    for range in ranges {
      self.io_map.insert(range, idx);
    }
    Ok(())
  }

  pub fn halt(&mut self) {
    self.devices.iter().for_each(|dev| match unsafe { dev.destroy() } {
      Ok(_) => (),
      Err(_) => eprintln!("Error destroying device: {}", { unsafe { (*dev).name() } }),
    });
  }

  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Result<T, Exception> {
    match self.select_device_for_read(addr) {
      // CanIO trait guarantees that the transmute is safe
      Some(dev) => match std::mem::size_of::<T>() {
        1 => Ok(unsafe { *std::mem::transmute::<*const u8, *const T>(&dev.read(addr).ok_or(Exception::StoreAccessFault(addr))? as *const u8) }),
        2 => Ok(unsafe { *std::mem::transmute::<*const u16, *const T>(&dev.read16(addr).ok_or(Exception::StoreAccessFault(addr))? as *const u16) }),
        4 => Ok(unsafe { *std::mem::transmute::<*const u32, *const T>(&dev.read32(addr).ok_or(Exception::StoreAccessFault(addr))? as *const u32) }),
        8 => Ok(unsafe { *std::mem::transmute::<*const u64, *const T>(&dev.read64(addr).ok_or(Exception::StoreAccessFault(addr))? as *const u64) }),
        _ => Err(Exception::LoadAccessFault(addr)),
      }
      None => self.mem.read(addr).ok_or(Exception::StoreAccessFault(addr)),
    }
  }

  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, val: T) -> Result<(), Exception> {
    match self.select_device_for_write(addr) {
      // CanIO trait guarantees that the transmute is safe
      Some(dev) => match std::mem::size_of::<T>() {
        1 => unsafe { dev.write(addr, *std::mem::transmute::<*const T, *const u8>(&val as *const T)) }.map_err(|_| Exception::StoreAccessFault(addr)),
        2 => unsafe { dev.write16(addr, *std::mem::transmute::<*const T, *const u16>(&val as *const T)) }.map_err(|_| Exception::StoreAccessFault(addr)),
        4 => unsafe { dev.write32(addr, *std::mem::transmute::<*const T, *const u32>(&val as *const T)) }.map_err(|_| Exception::StoreAccessFault(addr)),
        8 => unsafe { dev.write64(addr, *std::mem::transmute::<*const T, *const u64>(&val as *const T)) }.map_err(|_| Exception::StoreAccessFault(addr)),
        _ => Err(Exception::StoreAccessFault(addr)),
      }
      None => self.mem.write(addr, val).ok_or(Exception::StoreAccessFault(addr)),
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

  pub fn load<T: CanIO>(&mut self, mem: &[T], offset: usize) {
    self.mem.load(mem, offset);
  }
}


