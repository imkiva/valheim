use memmap2::MmapMut;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Add, Sub, AddAssign, SubAssign)]
pub struct VirtAddr(pub u64);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct PhysAddr(pub *const u8);

pub struct Memory {
  memory_base: VirtAddr,
  memory_size: usize,
  memory: MmapMut,
}

impl Memory {
  pub fn new(memory_base: u64, memory_size: usize) -> Result<Memory, std::io::Error> {
    let memory = MmapMut::map_anon(memory_size)?;
    Ok(Memory {
      memory_base: VirtAddr(memory_base),
      memory_size,
      memory,
    })
  }

  pub fn to_phys(&self, virt: VirtAddr) -> Option<PhysAddr> {
    if cfg!(debug_assertions) && !self.check_virt_bounds(virt) { return None; }
    let offset = virt - self.memory_base;
    let ptr = unsafe { self.memory.as_ptr().offset(offset.0 as isize) };
    Some(PhysAddr(ptr))
  }

  pub fn to_virt(&self, phys: PhysAddr) -> Option<VirtAddr> {
    if cfg!(debug_assertions) && !self.check_phys_bounds(phys) { return None; }
    let offset = phys.0 as usize - self.memory.as_ptr() as usize;
    Some(self.memory_base + VirtAddr(offset as u64))
  }

  fn check_virt_bounds(&self, virt: VirtAddr) -> bool {
    virt >= self.memory_base && virt < self.memory_base + VirtAddr(self.memory_size as u64)
  }

  fn check_phys_bounds(&self, phys: PhysAddr) -> bool {
    let ptr = self.memory.as_ptr() as usize;
    (phys.0 as usize) >= ptr && (phys.0 as usize) < ptr + self.memory_size
  }

  pub fn get_mut<T: Sized>(&mut self, virt: VirtAddr) -> Option<&mut T> {
    let phys = self.to_phys(virt)?;
    unsafe {
      let ptr = std::mem::transmute::<*const u8, *mut T>(phys.0);
      Some(&mut *ptr)
    }
  }

  pub fn get<T: Sized>(&self, virt: VirtAddr) -> Option<&T> {
    let phys = self.to_phys(virt)?;
    unsafe {
      let ptr = std::mem::transmute::<*const u8, *const T>(phys.0);
      Some(&*ptr)
    }
  }

  pub fn read<T: Copy + Sized>(&self, addr: VirtAddr) -> Option<T> {
    self.get(addr).map(|v| *v)
  }

  pub fn write<T: Copy + Sized>(&mut self, addr: VirtAddr, value: T) -> Option<()> {
    self.get_mut(addr).map(|v| *v = value)
  }

  pub fn reset<T: Sized>(&mut self, mem: &[T]) {
    unsafe {
      std::ptr::copy_nonoverlapping(
        mem.as_ptr() as *const u8,
        self.memory.as_mut_ptr(),
        mem.len() * std::mem::size_of::<T>()
      );
    }
  }
}

#[cfg(test)]
mod test {
  use crate::memory::{Memory, VirtAddr};

  #[test]
  fn memory_init() {
    let mem = Memory::new(0x1000, 0x1000).unwrap();
    std::mem::drop(mem);
  }

  #[test]
  pub fn memory_read() {
    let mut mem = Memory::new(0x1000, 0x1000).unwrap();
    let addr = mem.memory_base + VirtAddr(0x4);
    let value = 0xCAFEBABE_DEADBEEF as u64;
    mem.write(addr, value);
    assert_eq!(mem.read::<u64>(addr), Some(value));

    let low_addr = mem.memory_base + VirtAddr(0x4);
    let low_value = 0xDEADBEEF as u32;
    assert_eq!(mem.read::<u32>(low_addr), Some(low_value));

    let high_addr = mem.memory_base + VirtAddr(0x8);
    let high_value = 0xCAFEBABE as u32;
    assert_eq!(mem.read::<u32>(high_addr), Some(high_value));

    let invalid_addr1 = mem.memory_base + VirtAddr(0xffff);
    let invalid_addr2 = VirtAddr(0);
    assert_eq!(mem.read::<u32>(invalid_addr1), None);
    assert_eq!(mem.read::<u32>(invalid_addr2), None);
  }
}
