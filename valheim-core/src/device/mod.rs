use crate::memory::{Memory, VirtAddr};

pub trait Device {
  fn name(&self) -> &'static str;
  fn vendor_id(&self) -> u16;
  fn device_id(&self) -> u16;
  fn init(&'static mut self) -> Result<Vec<(VirtAddr, VirtAddr)>, ()>;
  fn destroy(&mut self) -> Result<(), ()>;
  fn dma_read(&self, addr: VirtAddr) -> Option<&Memory>;
  fn dma_write(&mut self, addr: VirtAddr) -> Option<&mut Memory>;
  fn mmio_read(&self, addr: VirtAddr) -> Option<u8>;
  fn mmio_write(&mut self, addr: VirtAddr, val: u8) -> Result<(), ()>;

  fn read(&self, addr: VirtAddr) -> Option<u8> {
    match self.dma_read(addr) {
      Some(mem) => mem.read(addr),
      None => self.mmio_read(addr),
    }
  }

  fn write(&mut self, addr: VirtAddr, data: u8) -> Result<(), ()> {
    match self.dma_write(addr) {
      Some(mem) => mem.write(addr, data).ok_or(()),
      None => self.mmio_write(addr, data),
    }
  }

  fn read16(&self, addr: VirtAddr) -> Option<u16> {
    match self.dma_read(addr) {
      Some(mem) => mem.read(addr),
      None => {
        let lo = self.mmio_read(addr)?;
        let hi = self.mmio_read(addr + VirtAddr(1))?;
        Some((hi as u16) << 8 | lo as u16)
      }
    }
  }

  fn read32(&self, addr: VirtAddr) -> Option<u32> {
    match self.dma_read(addr) {
      Some(mem) => mem.read(addr),
      None => {
        let lo = self.mmio_read(addr)?;
        let hi = self.mmio_read(addr + VirtAddr(1))?;
        let lo2 = self.mmio_read(addr + VirtAddr(2))?;
        let hi2 = self.mmio_read(addr + VirtAddr(3))?;
        Some((hi2 as u32) << 24 | (lo2 as u32) << 16 | (hi as u32) << 8 | lo as u32)
      }
    }
  }

  fn read64(&self, addr: VirtAddr) -> Option<u64> {
    match self.dma_read(addr) {
      Some(mem) => mem.read(addr),
      None => {
        let lo = self.mmio_read(addr)?;
        let hi = self.mmio_read(addr + VirtAddr(1))?;
        let lo2 = self.mmio_read(addr + VirtAddr(2))?;
        let hi2 = self.mmio_read(addr + VirtAddr(3))?;
        let lo3 = self.mmio_read(addr + VirtAddr(4))?;
        let hi3 = self.mmio_read(addr + VirtAddr(5))?;
        let lo4 = self.mmio_read(addr + VirtAddr(6))?;
        let hi4 = self.mmio_read(addr + VirtAddr(7))?;
        Some((hi4 as u64) << 56 | (lo4 as u64) << 48 | (hi3 as u64) << 40 | (lo3 as u64) << 32
          | (hi2 as u64) << 24 | (lo2 as u64) << 16 | (hi as u64) << 8 | lo as u64)
      }
    }
  }

  fn write16(&mut self, addr: VirtAddr, data: u16) -> Result<(), ()> {
    match self.dma_write(addr) {
      Some(mem) => mem.write(addr, data).ok_or(()),
      None => {
        self.mmio_write(addr, data as u8)?;
        self.mmio_write(addr + VirtAddr(1), (data >> 8) as u8)
      }
    }
  }

  fn write32(&mut self, addr: VirtAddr, data: u32) -> Result<(), ()> {
    match self.dma_write(addr) {
      Some(mem) => mem.write(addr, data).ok_or(()),
      None => {
        self.mmio_write(addr, data as u8)?;
        self.mmio_write(addr + VirtAddr(1), (data >> 8) as u8)?;
        self.mmio_write(addr + VirtAddr(2), (data >> 16) as u8)?;
        self.mmio_write(addr + VirtAddr(3), (data >> 24) as u8)
      }
    }
  }

  fn write64(&mut self, addr: VirtAddr, data: u64) -> Result<(), ()> {
    match self.dma_write(addr) {
      Some(mem) => mem.write(addr, data).ok_or(()),
      None => {
        self.mmio_write(addr, data as u8)?;
        self.mmio_write(addr + VirtAddr(1), (data >> 8) as u8)?;
        self.mmio_write(addr + VirtAddr(2), (data >> 16) as u8)?;
        self.mmio_write(addr + VirtAddr(3), (data >> 24) as u8)?;
        self.mmio_write(addr + VirtAddr(4), (data >> 32) as u8)?;
        self.mmio_write(addr + VirtAddr(5), (data >> 40) as u8)?;
        self.mmio_write(addr + VirtAddr(6), (data >> 48) as u8)?;
        self.mmio_write(addr + VirtAddr(7), (data >> 56) as u8)
      }
    }
  }
}
