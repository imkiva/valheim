use std::fmt::Debug;

use memmap2::MmapMut;

/// `transmutable` data types
pub trait CanIO: Copy + Sized {}
impl CanIO for u8 {}
impl CanIO for u16 {}
impl CanIO for u32 {}
impl CanIO for u64 {}
impl CanIO for i8 {}
impl CanIO for i16 {}
impl CanIO for i32 {}
impl CanIO for i64 {}

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Add, Sub, AddAssign, SubAssign)]
pub struct VirtAddr(pub u64);

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct PhysAddr(pub *mut u8);

impl Debug for VirtAddr {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "VirtAddr({:#x})", self.0)
  }
}

impl Debug for PhysAddr {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "PhysAddr({:#x})", self.0 as usize)
  }
}

#[derive(Debug)]
pub struct Memory {
  pub memory_base: VirtAddr,
  pub memory_size: usize,
  pub memory: MmapMut,
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

  #[inline(always)]
  pub fn to_phys(&self, virt: VirtAddr) -> Option<PhysAddr> {
    self.to_phys_with_width(virt, 1)
  }

  /// Returns a host pointer only when the complete `[virt, virt + width)` range is mapped.
  ///
  /// The pointer remains valid while this `Memory` and its backing mapping are alive. Callers must
  /// still use unaligned accesses unless they establish a stronger alignment invariant themselves.
  #[inline(always)]
  pub fn to_phys_with_width(&self, virt: VirtAddr, width: usize) -> Option<PhysAddr> {
    let offset = self.offset_for_range(virt, width)?;
    let ptr = unsafe { self.memory.as_ptr().add(offset) as *mut u8 };
    Some(PhysAddr(ptr))
  }

  pub fn to_virt(&self, phys: PhysAddr) -> Option<VirtAddr> {
    if !self.check_phys_bounds(phys, 1) {
      return None;
    }
    let offset = (phys.0 as usize).checked_sub(self.memory.as_ptr() as usize)?;
    Some(self.memory_base + VirtAddr(offset as u64))
  }

  /// Tests whether the complete `[addr, addr + width)` range belongs to this mapping.
  #[inline(always)]
  pub fn contains(&self, addr: VirtAddr, width: usize) -> bool {
    self.offset_for_range(addr, width).is_some()
  }

  #[inline(always)]
  fn offset_for_range(&self, addr: VirtAddr, width: usize) -> Option<usize> {
    let Some(offset) = addr.0.checked_sub(self.memory_base.0) else {
      return None;
    };
    let Ok(offset) = usize::try_from(offset) else {
      return None;
    };
    if offset > self.memory_size || width > self.memory_size - offset {
      return None;
    }
    Some(offset)
  }

  fn check_phys_bounds(&self, phys: PhysAddr, width: usize) -> bool {
    let base = self.memory.as_ptr() as usize;
    let Some(offset) = (phys.0 as usize).checked_sub(base) else {
      return false;
    };
    offset <= self.memory_size && width <= self.memory_size - offset
  }

  pub fn get_mut<T: CanIO>(&mut self, virt: VirtAddr) -> Option<&mut T> {
    let phys = self.to_phys_with_width(virt, std::mem::size_of::<T>())?;
    if (phys.0 as usize) % std::mem::align_of::<T>() != 0 {
      return None;
    }
    unsafe {
      // CanIO trait guarantees that the transmute is safe
      let ptr = phys.0.cast::<T>();
      Some(&mut *ptr)
    }
  }

  pub fn get<T: CanIO>(&self, virt: VirtAddr) -> Option<&T> {
    let phys = self.to_phys_with_width(virt, std::mem::size_of::<T>())?;
    if (phys.0 as usize) % std::mem::align_of::<T>() != 0 {
      return None;
    }
    unsafe {
      // CanIO trait guarantees that the transmute is safe
      let ptr = phys.0.cast::<T>();
      Some(&*ptr)
    }
  }

  #[inline(always)]
  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Option<T> {
    let phys = self.to_phys_with_width(addr, std::mem::size_of::<T>())?;
    Some(unsafe { phys.0.cast::<T>().read_unaligned() })
  }

  #[inline(always)]
  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, value: T) -> Option<()> {
    let phys = self.to_phys_with_width(addr, std::mem::size_of::<T>())?;
    unsafe { phys.0.cast::<T>().write_unaligned(value) };
    Some(())
  }

  /// Copies bytes from DRAM into `dst` after validating the complete guest-physical range.
  pub fn read_bytes(&self, addr: VirtAddr, dst: &mut [u8]) -> Option<()> {
    if dst.is_empty() {
      self.to_phys_with_width(addr, 0)?;
      return Some(());
    }
    let phys = self.to_phys_with_width(addr, dst.len())?;
    unsafe {
      std::ptr::copy_nonoverlapping(phys.0.cast_const(), dst.as_mut_ptr(), dst.len());
    }
    Some(())
  }

  /// Borrows a byte slice only when the complete guest-physical range belongs to DRAM.
  pub fn slice(&self, addr: VirtAddr, width: usize) -> Option<&[u8]> {
    let offset = self.offset_for_range(addr, width)?;
    self.memory.get(offset..offset + width)
  }

  /// Copies `src` into DRAM after validating the complete guest-physical range.
  pub fn write_bytes(&mut self, addr: VirtAddr, src: &[u8]) -> Option<()> {
    if src.is_empty() {
      self.to_phys_with_width(addr, 0)?;
      return Some(());
    }
    let phys = self.to_phys_with_width(addr, src.len())?;
    unsafe {
      std::ptr::copy_nonoverlapping(src.as_ptr(), phys.0, src.len());
    }
    Some(())
  }

  pub fn load<T: CanIO>(&mut self, offset: usize, mem: &[T]) -> Option<()> {
    let byte_len = mem.len().checked_mul(std::mem::size_of::<T>())?;
    if byte_len == 0 {
      return Some(());
    }
    let addr = VirtAddr(u64::try_from(offset).ok()?);
    let phys = self.to_phys_with_width(addr, byte_len)?;
    unsafe {
      std::ptr::copy_nonoverlapping(mem.as_ptr() as *const u8, phys.0, byte_len);
    }
    Some(())
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
    assert_eq!(mem.write(addr, value), Some(()));
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

  #[test]
  fn memory_checks_the_complete_access_width() {
    let mut mem = Memory::new(0x1000, 0x1000).unwrap();
    let last = VirtAddr(0x1fff);

    assert!(mem.contains(last, 1));
    assert!(!mem.contains(last, 2));
    assert!(!mem.contains(VirtAddr(0x2000), 1));
    assert_eq!(mem.write(last, 0xaa_u8), Some(()));
    assert_eq!(mem.read::<u8>(last), Some(0xaa));
    assert_eq!(mem.read::<u16>(last), None);
    assert_eq!(mem.write(last, 0xbbcc_u16), None);
  }

  #[test]
  fn memory_supports_unaligned_values_without_references() {
    let mut mem = Memory::new(0x1000, 0x1000).unwrap();
    let unaligned = VirtAddr(0x1003);
    let value = 0x0123_4567_89ab_cdef_u64;

    assert_eq!(mem.write(unaligned, value), Some(()));
    assert_eq!(mem.read::<u64>(unaligned), Some(value));
    assert!(mem.get::<u64>(unaligned).is_none());
    assert!(mem.get_mut::<u64>(unaligned).is_none());
  }

  #[test]
  fn memory_load_checks_the_whole_slice() {
    let mut mem = Memory::new(0x1000, 8).unwrap();

    assert_eq!(mem.load(0x1004, &[1_u8, 2, 3, 4]), Some(()));
    assert_eq!(mem.read::<u32>(VirtAddr(0x1004)), Some(0x0403_0201));
    assert_eq!(mem.load(0x1005, &[1_u8, 2, 3, 4]), None);
  }

  #[test]
  fn memory_bulk_copy_checks_the_whole_slice() {
    let mut mem = Memory::new(0x1000, 8).unwrap();
    let source = [1_u8, 2, 3, 4];
    let mut destination = [0_u8; 4];

    assert_eq!(mem.write_bytes(VirtAddr(0x1004), &source), Some(()));
    assert_eq!(mem.read_bytes(VirtAddr(0x1004), &mut destination), Some(()));
    assert_eq!(destination, source);
    assert_eq!(mem.write_bytes(VirtAddr(0x1005), &source), None);
    assert_eq!(mem.read_bytes(VirtAddr(0x0fff), &mut destination), None);
  }

  #[test]
  fn memory_slices_check_the_complete_range() {
    let mut mem = Memory::new(0x1000, 8).unwrap();
    mem.write_bytes(VirtAddr(0x1002), &[1, 2, 3, 4]).unwrap();

    assert_eq!(mem.slice(VirtAddr(0x1002), 4), Some(&[1, 2, 3, 4][..]));
    assert_eq!(mem.slice(VirtAddr(0x1008), 0), Some(&[][..]));
    assert!(mem.slice(VirtAddr(0x1007), 2).is_none());
    assert!(mem.slice(VirtAddr(0x0fff), 1).is_none());
    assert!(mem.slice(VirtAddr(u64::MAX), 2).is_none());
  }
}
