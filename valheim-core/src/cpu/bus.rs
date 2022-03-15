use std::collections::BTreeMap;
use std::fmt::{Debug, Formatter};
use crate::device::Device;
use crate::memory::{CanIO, Memory, VirtAddr};

/// System Bus, which handles DRAM access and memory-mapped IO.
pub struct Bus {
  mem: Memory,
  devices: Vec<*mut dyn Device>,
  io_map: BTreeMap<(VirtAddr, VirtAddr), usize>,
}

impl Debug for Bus {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "Bus {{ mem: {:?}, devices: {:?} }}",
      self.mem,
      self.devices.iter().map(|&dev| unsafe { (*dev).name() }).collect::<Vec<_>>(),
    )
  }
}

impl Bus {
  pub fn new(memory_base: u64, memory_size: usize) -> Result<Bus, std::io::Error> {
    let mem = Memory::new(memory_base, memory_size)?;
    Ok(Bus {
      mem,
      devices: Vec::with_capacity(8),
      io_map: BTreeMap::new(),
    })
  }

  pub unsafe fn add_device(&mut self, device: *mut dyn Device) -> Result<(), ()> {
    let ranges = (*device).init()?;
    let idx = self.devices.len();
    self.devices.push(device);
    for range in ranges {
      self.io_map.insert(range, idx);
    }
    Ok(())
  }

  pub fn halt(&mut self) {
    self.devices.iter().for_each(|&dev| match unsafe { (*dev).destroy() } {
      Ok(_) => (),
      Err(_) => eprintln!("Error destroying device: {}", { unsafe { (*dev).name() } }),
    });
  }

  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Option<T> {
    match self.select_device_for_read(addr) {
      // CanIO trait guarantees that the transmute is safe
      Some(dev) => match std::mem::size_of::<T>() {
        1 => Some(unsafe { *std::mem::transmute::<*const u8, *const T>((*dev).read(addr)? as *const u8) }),
        2 => Some(unsafe { *std::mem::transmute::<*const u16, *const T>((*dev).read16(addr)? as *const u16) }),
        4 => Some(unsafe { *std::mem::transmute::<*const u32, *const T>((*dev).read32(addr)? as *const u32) }),
        8 => Some(unsafe { *std::mem::transmute::<*const u64, *const T>((*dev).read64(addr)? as *const u64) }),
        _ => None,
      }
      None => self.mem.read(addr),
    }
  }

  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, val: T) -> Option<()> {
    match self.select_device_for_write(addr) {
      // CanIO trait guarantees that the transmute is safe
      Some(dev) => match std::mem::size_of::<T>() {
        1 => unsafe { (*dev).write(addr, *std::mem::transmute::<*const T, *const u8>(&val as *const T)) }.ok(),
        2 => unsafe { (*dev).write16(addr, *std::mem::transmute::<*const T, *const u16>(&val as *const T)) }.ok(),
        4 => unsafe { (*dev).write32(addr, *std::mem::transmute::<*const T, *const u32>(&val as *const T)) }.ok(),
        8 => unsafe { (*dev).write64(addr, *std::mem::transmute::<*const T, *const u64>(&val as *const T)) }.ok(),
        _ => None,
      }
      None => self.mem.write(addr, val),
    }
  }

  fn select_device_for_read(&self, addr: VirtAddr) -> Option<*const dyn Device> {
    for ((base, end), dev_id) in self.io_map.iter() {
      if addr >= *base && addr < *end {
        return match self.devices.get(*dev_id) {
          Some(&dev) => Some(dev),
          None => None,
        };
      }
    }
    None
  }

  fn select_device_for_write(&mut self, addr: VirtAddr) -> Option<*mut dyn Device> {
    for ((base, end), dev_id) in self.io_map.iter_mut() {
      if addr >= *base && addr <= *end {
        return match self.devices.get(*dev_id) {
          Some(&dev) => Some(dev),
          None => None,
        };
      }
    }
    None
  }

  pub fn load_kernel<T: CanIO>(&mut self, mem: &[T], offset: usize) {
    self.mem.load_kernel(mem, offset);
  }
}
