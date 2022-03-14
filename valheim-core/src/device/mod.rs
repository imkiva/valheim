use crate::memory::VirtAddr;

pub trait Device {
  fn name(&self) -> &str;
  fn init(&mut self) -> Result<(VirtAddr, VirtAddr), ()>;
  fn destroy(&mut self) -> Result<(), ()>;
  fn read(&self, addr: VirtAddr) -> Option<u8>;
  fn write(&mut self, addr: VirtAddr, data: u8) -> Result<(), ()>;

  fn read8(&self, addr: VirtAddr) -> Option<u8> {
    self.read(addr)
  }

  fn read16(&self, addr: VirtAddr) -> Option<u16> {
    let lo = self.read(addr)?;
    let hi = self.read(addr + VirtAddr(1))?;
    Some((hi as u16) << 8 | lo as u16)
  }

  fn read32(&self, addr: VirtAddr) -> Option<u32> {
    let lo = self.read16(addr)?;
    let hi = self.read16(addr + VirtAddr(2))?;
    Some((hi as u32) << 8 | lo as u32)
  }

  fn read64(&self, addr: VirtAddr) -> Option<u64> {
    let lo = self.read32(addr)?;
    let hi = self.read32(addr + VirtAddr(4))?;
    Some((hi as u64) << 32 | lo as u64)
  }

  fn write8(&mut self, addr: VirtAddr, data: u8) -> Result<(), ()> {
    self.write(addr, data)
  }

  fn write16(&mut self, addr: VirtAddr, data: u16) -> Result<(), ()> {
    self.write(addr, data as u8)?;
    self.write(addr + VirtAddr(1), (data >> 8) as u8)
  }

  fn write32(&mut self, addr: VirtAddr, data: u32) -> Result<(), ()> {
    self.write16(addr, data as u16)?;
    self.write16(addr + VirtAddr(2), (data >> 16) as u16)
  }

  fn write64(&mut self, addr: VirtAddr, data: u64) -> Result<(), ()> {
    self.write32(addr, data as u32)?;
    self.write32(addr + VirtAddr(4), (data >> 32) as u32)
  }
}
