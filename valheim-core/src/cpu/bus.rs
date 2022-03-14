use std::collections::BTreeMap;
use std::fmt::{Debug, Formatter};
use crate::device::Device;
use crate::memory::{Memory, VirtAddr};

/// System Bus, which handles DRAM access and memory-mapped IO.
pub struct Bus {
  mem: Memory,
  devices: BTreeMap<(VirtAddr, VirtAddr), Box<dyn Device>>,
}

impl Debug for Bus {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "Bus {{ mem: {:?}, devices: {:?} }}",
      self.mem,
      self.devices.iter().map(|(_, dev)| dev.name()).collect::<Vec<_>>(),
    )
  }
}

impl Bus {
  pub fn new(memory_base: u64, memory_size: usize) -> Result<Bus, std::io::Error> {
    let mem = Memory::new(memory_base, memory_size)?;
    Ok(Bus {
      mem,
      devices: BTreeMap::new(),
    })
  }

  pub fn add_device(&mut self, mut device: Box<dyn Device>) -> Result<(), ()> {
    let (base, end) = device.init()?;
    self.devices.insert((base, end), device);
    Ok(())
  }

  pub fn read<T: Copy + Sized>(&self, addr: VirtAddr) -> Option<T> {
    match self.select_device_for_read(addr) {
      Some(dev) => match std::mem::size_of::<T>() {
        1 => Some(unsafe { *std::mem::transmute::<* const u8, *const T>(&dev.read8(addr)? as *const u8) }),
        2 => Some(unsafe { *std::mem::transmute::<* const u16, *const T>(&dev.read16(addr)? as *const u16) }),
        4 => Some(unsafe { *std::mem::transmute::<* const u32, *const T>(&dev.read32(addr)? as *const u32) }),
        8 => Some(unsafe { *std::mem::transmute::<* const u64, *const T>(&dev.read64(addr)? as *const u64) }),
        _ => None,
      }
      None => self.mem.read(addr),
    }
  }

  pub fn write<T: Copy + Sized>(&mut self, addr: VirtAddr, val: T) -> Option<()> {
    match self.select_device_for_write(addr) {
      Some(dev) => match std::mem::size_of::<T>() {
        1 => dev.write8(addr, unsafe { *std::mem::transmute::<* const T, *const u8>(&val as *const T) }).ok(),
        2 => dev.write16(addr, unsafe { *std::mem::transmute::<* const T, *const u16>(&val as *const T) }).ok(),
        4 => dev.write32(addr, unsafe { *std::mem::transmute::<* const T, *const u32>(&val as *const T) }).ok(),
        8 => dev.write64(addr, unsafe { *std::mem::transmute::<* const T, *const u64>(&val as *const T) }).ok(),
        _ => None,
      }
      None => self.mem.write(addr, val),
    }
  }

  fn select_device_for_read(&self, addr: VirtAddr) -> Option<&Box<dyn Device>> {
    for ((base, end), dev) in self.devices.iter() {
      if addr >= *base && addr < *end {
        return Some(dev);
      }
    }
    None
  }

  fn select_device_for_write(&mut self, addr: VirtAddr) -> Option<&mut Box<dyn Device>> {
    for ((base, end), dev) in self.devices.iter_mut() {
      if addr >= *base && addr <= *end {
        return Some(dev);
      }
    }
    None
  }

  pub(crate) fn reset_dram<T: Sized>(&mut self, p0: &[T]) {
    self.mem.reset(p0);
  }
}
