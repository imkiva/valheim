use std::io::{self, ErrorKind};

use memmap2::MmapMut;

use crate::cpu::bus::VIRTIO_BASE;
use crate::cpu::irq::Exception;
use crate::memory::{CanIO, Memory, VirtAddr};

/// The PLIC source used by the VirtIO block device.
pub const VIRTIO_IRQ: u64 = 1;

const VRING_DESC_SIZE: u64 = 16;
const QUEUE_SIZE: u16 = 128;
const SECTOR_SIZE: u64 = 512;
const COPY_CHUNK_SIZE: usize = 64 * 1024;

const VIRTQ_DESC_F_NEXT: u16 = 1;
const VIRTQ_DESC_F_WRITE: u16 = 2;
const VIRTQ_DESC_F_INDIRECT: u16 = 4;
const VIRTQ_AVAIL_F_NO_INTERRUPT: u16 = 1;

const VIRTIO_BLK_F_SEG_MAX: u32 = 1 << 2;
const VIRTIO_BLK_F_FLUSH: u32 = 1 << 9;
const VIRTIO_BLK_T_IN: u32 = 0;
const VIRTIO_BLK_T_OUT: u32 = 1;
const VIRTIO_BLK_T_FLUSH: u32 = 4;
const VIRTIO_BLK_S_OK: u8 = 0;
const VIRTIO_BLK_S_IOERR: u8 = 1;
const VIRTIO_BLK_S_UNSUPP: u8 = 2;

const VIRTIO_STATUS_DRIVER_OK: u32 = 4;
const VIRTIO_MMIO_INT_VRING: u32 = 1;

const MAGIC: u64 = VIRTIO_BASE;
const MAGIC_END: u64 = VIRTIO_BASE + 0x3;
const VERSION: u64 = VIRTIO_BASE + 0x4;
const VERSION_END: u64 = VIRTIO_BASE + 0x7;
const DEVICE_ID: u64 = VIRTIO_BASE + 0x8;
const DEVICE_ID_END: u64 = VIRTIO_BASE + 0xb;
const VENDOR_ID: u64 = VIRTIO_BASE + 0xc;
const VENDOR_ID_END: u64 = VIRTIO_BASE + 0xf;
const DEVICE_FEATURES: u64 = VIRTIO_BASE + 0x10;
const DEVICE_FEATURES_END: u64 = VIRTIO_BASE + 0x13;
const DEVICE_FEATURES_SEL: u64 = VIRTIO_BASE + 0x14;
const DEVICE_FEATURES_SEL_END: u64 = VIRTIO_BASE + 0x17;
const DRIVER_FEATURES: u64 = VIRTIO_BASE + 0x20;
const DRIVER_FEATURES_END: u64 = VIRTIO_BASE + 0x23;
const DRIVER_FEATURES_SEL: u64 = VIRTIO_BASE + 0x24;
const DRIVER_FEATURES_SEL_END: u64 = VIRTIO_BASE + 0x27;
const GUEST_PAGE_SIZE: u64 = VIRTIO_BASE + 0x28;
const GUEST_PAGE_SIZE_END: u64 = VIRTIO_BASE + 0x2b;
const QUEUE_SEL: u64 = VIRTIO_BASE + 0x30;
const QUEUE_SEL_END: u64 = VIRTIO_BASE + 0x33;
const QUEUE_NUM_MAX: u64 = VIRTIO_BASE + 0x34;
const QUEUE_NUM_MAX_END: u64 = VIRTIO_BASE + 0x37;
const QUEUE_NUM: u64 = VIRTIO_BASE + 0x38;
const QUEUE_NUM_END: u64 = VIRTIO_BASE + 0x3b;
const QUEUE_ALIGN: u64 = VIRTIO_BASE + 0x3c;
const QUEUE_ALIGN_END: u64 = VIRTIO_BASE + 0x3f;
const QUEUE_PFN: u64 = VIRTIO_BASE + 0x40;
const QUEUE_PFN_END: u64 = VIRTIO_BASE + 0x43;
const QUEUE_NOTIFY: u64 = VIRTIO_BASE + 0x50;
const QUEUE_NOTIFY_END: u64 = VIRTIO_BASE + 0x53;
const INTERRUPT_STATUS: u64 = VIRTIO_BASE + 0x60;
const INTERRUPT_STATUS_END: u64 = VIRTIO_BASE + 0x63;
const INTERRUPT_ACK: u64 = VIRTIO_BASE + 0x64;
const INTERRUPT_ACK_END: u64 = VIRTIO_BASE + 0x67;
const STATUS: u64 = VIRTIO_BASE + 0x70;
const STATUS_END: u64 = VIRTIO_BASE + 0x73;
const CONFIG: u64 = VIRTIO_BASE + 0x100;
const CONFIG_END: u64 = VIRTIO_BASE + 0x10f;

/// Storage operations required by the VirtIO block transport.
pub trait BlockBackend {
  fn len(&self) -> u64;
  /// Borrows a fully checked backend range when direct reads are supported. A successful slice
  /// must have exactly `width` bytes.
  fn read_slice(&self, _offset: u64, _width: usize) -> Option<io::Result<&[u8]>> {
    None
  }
  /// Fills the complete buffer on success. Partial reads must return an error.
  fn read_at(&self, offset: u64, buffer: &mut [u8]) -> io::Result<()>;
  /// Writes the complete buffer on success. Partial writes must return an error.
  fn write_at(&mut self, offset: u64, buffer: &[u8]) -> io::Result<()>;
  fn flush(&mut self) -> io::Result<()>;
}

fn checked_backend_range(
  length: usize,
  offset: u64,
  width: usize,
) -> io::Result<std::ops::Range<usize>> {
  let start = usize::try_from(offset)
    .map_err(|_| io::Error::new(ErrorKind::UnexpectedEof, "block offset does not fit usize"))?;
  let end = start
    .checked_add(width)
    .filter(|end| *end <= length)
    .ok_or_else(|| io::Error::new(ErrorKind::UnexpectedEof, "block access is out of range"))?;
  Ok(start..end)
}

impl BlockBackend for MmapMut {
  fn len(&self) -> u64 {
    AsRef::<[u8]>::as_ref(self).len() as u64
  }

  fn read_at(&self, offset: u64, buffer: &mut [u8]) -> io::Result<()> {
    let range = checked_backend_range(AsRef::<[u8]>::as_ref(self).len(), offset, buffer.len())?;
    buffer.copy_from_slice(&self[range]);
    Ok(())
  }

  fn read_slice(&self, offset: u64, width: usize) -> Option<io::Result<&[u8]>> {
    Some(
      checked_backend_range(AsRef::<[u8]>::as_ref(self).len(), offset, width)
        .map(|range| &self[range]),
    )
  }

  fn write_at(&mut self, offset: u64, buffer: &[u8]) -> io::Result<()> {
    let range = checked_backend_range(AsRef::<[u8]>::as_ref(self).len(), offset, buffer.len())?;
    self[range].copy_from_slice(buffer);
    Ok(())
  }

  fn flush(&mut self) -> io::Result<()> {
    MmapMut::flush(self)
  }
}

#[derive(Debug, Copy, Clone)]
struct VirtqueueAddr {
  desc_addr: u64,
  avail_addr: u64,
  used_addr: u64,
}

impl VirtqueueAddr {
  fn from_device(virtio: &Virtio) -> Option<Self> {
    let size = u64::from(virtio.queue_num);
    let page_size = u64::from(virtio.guest_page_size);
    let align = u64::from(virtio.queue_align);
    if virtio.queue_pfn == 0
      || size == 0
      || size > u64::from(QUEUE_SIZE)
      || !size.is_power_of_two()
      || page_size == 0
      || !page_size.is_power_of_two()
      || align == 0
      || !align.is_power_of_two()
    {
      return None;
    }

    let desc_addr = u64::from(virtio.queue_pfn).checked_mul(page_size)?;
    let avail_addr = desc_addr.checked_add(VRING_DESC_SIZE.checked_mul(size)?)?;
    let avail_end = avail_addr.checked_add(6_u64.checked_add(2_u64.checked_mul(size)?)?)?;
    let used_addr = avail_end.checked_add(align - 1)? & !(align - 1);
    Some(Self {
      desc_addr,
      avail_addr,
      used_addr,
    })
  }

  fn fully_mapped(self, memory: &Memory, queue_num: u16) -> bool {
    let size = usize::from(queue_num);
    let Some(desc_len) = size.checked_mul(VRING_DESC_SIZE as usize) else {
      return false;
    };
    let Some(avail_len) = size.checked_mul(2).and_then(|length| length.checked_add(6)) else {
      return false;
    };
    let Some(used_len) = size.checked_mul(8).and_then(|length| length.checked_add(6)) else {
      return false;
    };
    memory.contains(VirtAddr(self.desc_addr), desc_len)
      && memory.contains(VirtAddr(self.avail_addr), avail_len)
      && memory.contains(VirtAddr(self.used_addr), used_len)
  }
}

#[derive(Debug, Copy, Clone)]
struct VirtqDesc {
  addr: u64,
  len: u32,
  flags: u16,
  next: u16,
}

impl VirtqDesc {
  fn read(memory: &Memory, queue: VirtqueueAddr, index: u16) -> Option<Self> {
    let offset = VRING_DESC_SIZE.checked_mul(u64::from(index))?;
    let address = queue.desc_addr.checked_add(offset)?;
    let mut bytes = [0_u8; VRING_DESC_SIZE as usize];
    memory.read_bytes(VirtAddr(address), &mut bytes)?;
    Some(Self {
      addr: u64::from_le_bytes(bytes[0..8].try_into().ok()?),
      len: u32::from_le_bytes(bytes[8..12].try_into().ok()?),
      flags: u16::from_le_bytes(bytes[12..14].try_into().ok()?),
      next: u16::from_le_bytes(bytes[14..16].try_into().ok()?),
    })
  }
}

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VirtioServiceResult {
  /// Number of requests published to the used ring during this call.
  pub completed: u16,
  /// Current level of the VirtIO interrupt-status line, including older unacknowledged work.
  pub interrupt_asserted: bool,
  /// At least one byte was written to guest DRAM by device DMA in this call, including request
  /// status and used-ring updates.
  pub dma_write: bool,
}

struct RequestResult {
  status: u8,
  written: u32,
  dma_write: bool,
}

/// Legacy VirtIO-MMIO version 1 block device with a single split virtqueue.
pub struct Virtio {
  device_features: [u32; 2],
  device_features_sel: u32,
  driver_features: [u32; 2],
  driver_features_sel: u32,
  guest_page_size: u32,
  queue_sel: u32,
  queue_num: u32,
  queue_align: u32,
  queue_pfn: u32,
  notify_pending: bool,
  interrupt_status: u32,
  status: u32,
  capacity: u64,
  last_avail_idx: u16,
  used_idx: u16,
  backend: Option<Box<dyn BlockBackend>>,
  descriptor_scratch: Vec<VirtqDesc>,
  transfer_scratch: Vec<u8>,
}

impl Virtio {
  pub fn new(capacity: u64) -> Self {
    Self {
      device_features: [VIRTIO_BLK_F_SEG_MAX | VIRTIO_BLK_F_FLUSH, 0],
      device_features_sel: 0,
      driver_features: [0; 2],
      driver_features_sel: 0,
      guest_page_size: 0,
      queue_sel: 0,
      queue_num: 0,
      queue_align: 0x1000,
      queue_pfn: 0,
      notify_pending: false,
      interrupt_status: 0,
      status: 0,
      capacity,
      last_avail_idx: 0,
      used_idx: 0,
      backend: None,
      descriptor_scratch: Vec::new(),
      transfer_scratch: Vec::new(),
    }
  }

  /// Installs a writable mmap backend. Empty and partial-sector images are rejected.
  pub fn set_image(&mut self, image: MmapMut) -> io::Result<()> {
    self.set_backend(image)
  }

  pub fn set_backend<B: BlockBackend + 'static>(&mut self, backend: B) -> io::Result<()> {
    let length = backend.len();
    if length == 0 || length % SECTOR_SIZE != 0 {
      return Err(io::Error::new(
        ErrorKind::InvalidInput,
        "VirtIO block image must be non-empty and 512-byte aligned",
      ));
    }
    self.capacity = length / SECTOR_SIZE;
    self.transfer_scratch.resize(COPY_CHUNK_SIZE, 0);
    self.backend = Some(Box::new(backend));
    self.reset_transport();
    Ok(())
  }

  pub fn interrupt_asserted(&self) -> bool {
    self.interrupt_status != 0
  }

  fn reset_transport(&mut self) {
    self.device_features_sel = 0;
    self.driver_features = [0; 2];
    self.driver_features_sel = 0;
    // Linux writes the legacy GuestPageSize during transport probe, before virtio core resets the
    // device status. Real virtio-mmio transports retain this transport property across that reset;
    // clearing it here makes the first queue notification impossible to locate.
    self.queue_sel = 0;
    self.queue_num = 0;
    self.queue_align = 0x1000;
    self.queue_pfn = 0;
    self.notify_pending = false;
    self.interrupt_status = 0;
    self.status = 0;
    self.last_avail_idx = 0;
    self.used_idx = 0;
    self.descriptor_scratch.clear();
  }

  fn reset_queue_indices(&mut self) {
    self.last_avail_idx = 0;
    self.used_idx = 0;
    self.notify_pending = false;
  }

  fn selected_feature(features: &[u32; 2], selector: u32) -> u32 {
    usize::try_from(selector)
      .ok()
      .and_then(|index| features.get(index))
      .copied()
      .unwrap_or(0)
  }

  fn io_width<T: CanIO>(addr: u64, base: u64, register_width: u64) -> Option<(u64, u32)> {
    let width = std::mem::size_of::<T>() as u64;
    if !matches!(width, 1 | 2 | 4) {
      return None;
    }
    let offset = addr.checked_sub(base)?;
    if offset.checked_add(width)? > register_width {
      return None;
    }
    Some((offset, width as u32))
  }

  fn read_register<T: CanIO>(addr: u64, base: u64, value: u32) -> Option<u32> {
    let (offset, width) = Self::io_width::<T>(addr, base, 4)?;
    let mask = match width {
      1 => 0xff,
      2 => 0xffff,
      4 => u32::MAX,
      _ => return None,
    };
    Some((value >> (offset * 8)) & mask)
  }

  fn merge_register_write<T: CanIO>(addr: u64, base: u64, old: u32, value: u32) -> Option<u32> {
    let (offset, width) = Self::io_width::<T>(addr, base, 4)?;
    let value_mask = match width {
      1 => 0xff,
      2 => 0xffff,
      4 => u32::MAX,
      _ => return None,
    };
    let shift = (offset * 8) as u32;
    let mask = value_mask << shift;
    Some((old & !mask) | ((value & value_mask) << shift))
  }

  fn written_register_bits<T: CanIO>(addr: u64, base: u64, value: u32) -> Option<u32> {
    Self::merge_register_write::<T>(addr, base, 0, value)
  }

  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Result<u32, Exception> {
    let address = addr.0;
    let value = match address {
      MAGIC..=MAGIC_END => Self::read_register::<T>(address, MAGIC, 0x7472_6976),
      VERSION..=VERSION_END => Self::read_register::<T>(address, VERSION, 1),
      DEVICE_ID..=DEVICE_ID_END => Self::read_register::<T>(
        address,
        DEVICE_ID,
        if self.backend.is_some() { 2 } else { 0 },
      ),
      VENDOR_ID..=VENDOR_ID_END => {
        Self::read_register::<T>(address, VENDOR_ID, 0x554d_4551)
      }
      DEVICE_FEATURES..=DEVICE_FEATURES_END => Self::read_register::<T>(
        address,
        DEVICE_FEATURES,
        Self::selected_feature(&self.device_features, self.device_features_sel),
      ),
      QUEUE_NUM_MAX..=QUEUE_NUM_MAX_END => Self::read_register::<T>(
        address,
        QUEUE_NUM_MAX,
        if self.queue_sel == 0 { u32::from(QUEUE_SIZE) } else { 0 },
      ),
      QUEUE_PFN..=QUEUE_PFN_END => Self::read_register::<T>(
        address,
        QUEUE_PFN,
        if self.queue_sel == 0 { self.queue_pfn } else { 0 },
      ),
      INTERRUPT_STATUS..=INTERRUPT_STATUS_END => {
        Self::read_register::<T>(address, INTERRUPT_STATUS, self.interrupt_status)
      }
      STATUS..=STATUS_END => Self::read_register::<T>(address, STATUS, self.status),
      CONFIG..=CONFIG_END => {
        let Some((offset, width)) = Self::io_width::<T>(address, CONFIG, 16) else {
          return Err(Exception::LoadAccessFault(addr));
        };
        let mut bytes = [0_u8; 16];
        bytes[0..8].copy_from_slice(&self.capacity.to_le_bytes());
        bytes[12..16].copy_from_slice(&u32::from(QUEUE_SIZE - 2).to_le_bytes());
        let mut result = 0_u32;
        for index in 0..width {
          result |= u32::from(bytes[(offset + u64::from(index)) as usize]) << (index * 8);
        }
        Some(result)
      }
      _ => None,
    };
    value.ok_or(Exception::LoadAccessFault(addr))
  }

  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, value: u32) -> Result<(), Exception> {
    let address = addr.0;
    match address {
      DEVICE_FEATURES_SEL..=DEVICE_FEATURES_SEL_END => {
        self.device_features_sel = Self::merge_register_write::<T>(
          address,
          DEVICE_FEATURES_SEL,
          self.device_features_sel,
          value,
        )
        .ok_or(Exception::StoreAccessFault(addr))?;
      }
      DRIVER_FEATURES..=DRIVER_FEATURES_END => {
        let selector = usize::try_from(self.driver_features_sel).ok();
        let old = selector
          .and_then(|index| self.driver_features.get(index))
          .copied()
          .unwrap_or(0);
        let new = Self::merge_register_write::<T>(address, DRIVER_FEATURES, old, value)
          .ok_or(Exception::StoreAccessFault(addr))?;
        if let Some(feature) = selector.and_then(|index| self.driver_features.get_mut(index)) {
          *feature = new;
        }
      }
      DRIVER_FEATURES_SEL..=DRIVER_FEATURES_SEL_END => {
        self.driver_features_sel = Self::merge_register_write::<T>(
          address,
          DRIVER_FEATURES_SEL,
          self.driver_features_sel,
          value,
        )
        .ok_or(Exception::StoreAccessFault(addr))?;
      }
      GUEST_PAGE_SIZE..=GUEST_PAGE_SIZE_END => {
        self.guest_page_size = Self::merge_register_write::<T>(
          address,
          GUEST_PAGE_SIZE,
          self.guest_page_size,
          value,
        )
        .ok_or(Exception::StoreAccessFault(addr))?;
        self.reset_queue_indices();
      }
      QUEUE_SEL..=QUEUE_SEL_END => {
        self.queue_sel = Self::merge_register_write::<T>(
          address,
          QUEUE_SEL,
          self.queue_sel,
          value,
        )
        .ok_or(Exception::StoreAccessFault(addr))?;
      }
      QUEUE_NUM..=QUEUE_NUM_END => {
        let old = if self.queue_sel == 0 { self.queue_num } else { 0 };
        let new = Self::merge_register_write::<T>(address, QUEUE_NUM, old, value)
          .ok_or(Exception::StoreAccessFault(addr))?;
        if self.queue_sel == 0 {
          self.queue_num = new;
          self.reset_queue_indices();
        }
      }
      QUEUE_ALIGN..=QUEUE_ALIGN_END => {
        let old = if self.queue_sel == 0 { self.queue_align } else { 0 };
        let new = Self::merge_register_write::<T>(address, QUEUE_ALIGN, old, value)
          .ok_or(Exception::StoreAccessFault(addr))?;
        if self.queue_sel == 0 {
          self.queue_align = new;
          self.reset_queue_indices();
        }
      }
      QUEUE_PFN..=QUEUE_PFN_END => {
        let old = if self.queue_sel == 0 { self.queue_pfn } else { 0 };
        let new = Self::merge_register_write::<T>(address, QUEUE_PFN, old, value)
          .ok_or(Exception::StoreAccessFault(addr))?;
        if self.queue_sel == 0 {
          self.queue_pfn = new;
          self.reset_queue_indices();
        }
      }
      QUEUE_NOTIFY..=QUEUE_NOTIFY_END => {
        let queue = Self::merge_register_write::<T>(address, QUEUE_NOTIFY, 0, value)
          .ok_or(Exception::StoreAccessFault(addr))?;
        if queue == 0 {
          self.notify_pending = true;
        }
      }
      INTERRUPT_ACK..=INTERRUPT_ACK_END => {
        let acknowledged = Self::written_register_bits::<T>(address, INTERRUPT_ACK, value)
          .ok_or(Exception::StoreAccessFault(addr))?;
        self.interrupt_status &= !acknowledged;
      }
      STATUS..=STATUS_END => {
        let new = Self::merge_register_write::<T>(address, STATUS, self.status, value)
          .ok_or(Exception::StoreAccessFault(addr))?;
        if new == 0 {
          self.reset_transport();
        } else {
          self.status = new;
        }
      }
      _ => return Err(Exception::StoreAccessFault(addr)),
    }
    Ok(())
  }

  fn read_u16(memory: &Memory, address: u64) -> Option<u16> {
    let mut bytes = [0_u8; 2];
    memory.read_bytes(VirtAddr(address), &mut bytes)?;
    Some(u16::from_le_bytes(bytes))
  }

  fn write_u16(memory: &mut Memory, address: u64, value: u16) -> bool {
    memory
      .write_bytes(VirtAddr(address), &value.to_le_bytes())
      .is_some()
  }

  fn descriptor_chain(
    memory: &Memory,
    queue: VirtqueueAddr,
    queue_num: u16,
    head: u16,
    descriptors: &mut Vec<VirtqDesc>,
  ) -> bool {
    descriptors.clear();
    if head >= queue_num {
      return false;
    }
    let queue_num = usize::from(queue_num);
    if descriptors.capacity() < queue_num {
      descriptors.reserve(queue_num);
    }
    let mut index = head;
    for _ in 0..queue_num {
      if usize::from(index) >= queue_num {
        return false;
      }
      let Some(descriptor) = VirtqDesc::read(memory, queue, index) else {
        return false;
      };
      if descriptor.flags & VIRTQ_DESC_F_INDIRECT != 0 {
        return false;
      }
      descriptors.push(descriptor);
      if descriptor.flags & VIRTQ_DESC_F_NEXT == 0 {
        return true;
      }
      index = descriptor.next;
    }
    false
  }

  fn block_range(&self, sector: u64, length: u64) -> Option<u64> {
    let start = sector.checked_mul(SECTOR_SIZE)?;
    let end = start.checked_add(length)?;
    (end <= self.backend.as_ref()?.len()).then_some(start)
  }

  fn validate_data_ranges(memory: &Memory, descriptors: &[VirtqDesc]) -> Option<u64> {
    let mut total = 0_u64;
    for descriptor in descriptors {
      let length = usize::try_from(descriptor.len).ok()?;
      if !memory.contains(VirtAddr(descriptor.addr), length) {
        return None;
      }
      total = total.checked_add(u64::from(descriptor.len))?;
    }
    Some(total)
  }

  fn transfer_in(
    &mut self,
    memory: &mut Memory,
    sector: u64,
    descriptors: &[VirtqDesc],
  ) -> RequestResult {
    if descriptors
      .iter()
      .any(|descriptor| descriptor.flags & VIRTQ_DESC_F_WRITE == 0)
    {
      return RequestResult::io_error();
    }
    let Some(total) = Self::validate_data_ranges(memory, descriptors) else {
      return RequestResult::io_error();
    };
    let Some(start) = self.block_range(sector, total) else {
      return RequestResult::io_error();
    };

    let mut result = RequestResult::success();
    let (Some(backend), buffer) = (self.backend.as_ref(), &mut self.transfer_scratch) else {
      return RequestResult::io_error();
    };
    let mut disk_offset = start;
    for descriptor in descriptors {
      let mut descriptor_offset = 0_usize;
      let descriptor_len = descriptor.len as usize;
      while descriptor_offset < descriptor_len {
        let chunk_len = COPY_CHUNK_SIZE.min(descriptor_len - descriptor_offset);
        let guest_addr = VirtAddr(descriptor.addr + descriptor_offset as u64);
        let copied = match backend.read_slice(disk_offset, chunk_len) {
          Some(Ok(chunk)) if chunk.len() == chunk_len => {
            memory.write_bytes(guest_addr, chunk).is_some()
          }
          Some(Ok(_)) => false,
          Some(Err(_)) => false,
          None => {
            let chunk = &mut buffer[..chunk_len];
            backend.read_at(disk_offset, chunk).is_ok()
              && memory.write_bytes(guest_addr, chunk).is_some()
          }
        };
        if !copied {
          result.status = VIRTIO_BLK_S_IOERR;
          return result;
        }
        result.written = result.written.saturating_add(chunk_len as u32);
        result.dma_write = true;
        descriptor_offset += chunk_len;
        disk_offset += chunk_len as u64;
      }
    }
    result
  }

  fn transfer_out(
    &mut self,
    memory: &Memory,
    sector: u64,
    descriptors: &[VirtqDesc],
  ) -> RequestResult {
    if descriptors
      .iter()
      .any(|descriptor| descriptor.flags & VIRTQ_DESC_F_WRITE != 0)
    {
      return RequestResult::io_error();
    }
    let Some(total) = Self::validate_data_ranges(memory, descriptors) else {
      return RequestResult::io_error();
    };
    let Some(start) = self.block_range(sector, total) else {
      return RequestResult::io_error();
    };

    let Some(backend) = self.backend.as_mut() else {
      return RequestResult::io_error();
    };
    let mut disk_offset = start;
    for descriptor in descriptors {
      let mut descriptor_offset = 0_usize;
      let descriptor_len = descriptor.len as usize;
      while descriptor_offset < descriptor_len {
        let chunk_len = COPY_CHUNK_SIZE.min(descriptor_len - descriptor_offset);
        let guest_addr = VirtAddr(descriptor.addr + descriptor_offset as u64);
        let Some(chunk) = memory.slice(guest_addr, chunk_len) else {
          return RequestResult::io_error();
        };
        if backend.write_at(disk_offset, chunk).is_err() {
          return RequestResult::io_error();
        }
        descriptor_offset += chunk_len;
        disk_offset += chunk_len as u64;
      }
    }
    RequestResult::success()
  }

  fn process_request(
    &mut self,
    memory: &mut Memory,
    descriptors: &[VirtqDesc],
  ) -> RequestResult {
    if descriptors.len() < 2 {
      return RequestResult::io_error();
    }
    let header = descriptors[0];
    let status_descriptor = descriptors[descriptors.len() - 1];
    if status_descriptor.flags & VIRTQ_DESC_F_WRITE == 0
      || status_descriptor.len == 0
      || !memory.contains(VirtAddr(status_descriptor.addr), 1)
    {
      return RequestResult::io_error();
    }

    let mut header_bytes = [0_u8; 16];
    let mut result = if header.flags & VIRTQ_DESC_F_WRITE != 0
      || header.len < 16
      || memory
        .read_bytes(VirtAddr(header.addr), &mut header_bytes)
        .is_none()
    {
      RequestResult::io_error()
    } else {
      let request_type = u32::from_le_bytes([
        header_bytes[0],
        header_bytes[1],
        header_bytes[2],
        header_bytes[3],
      ]);
      let sector = u64::from_le_bytes([
        header_bytes[8],
        header_bytes[9],
        header_bytes[10],
        header_bytes[11],
        header_bytes[12],
        header_bytes[13],
        header_bytes[14],
        header_bytes[15],
      ]);
      let data = &descriptors[1..descriptors.len() - 1];
      match request_type {
        VIRTIO_BLK_T_IN => self.transfer_in(memory, sector, data),
        VIRTIO_BLK_T_OUT => self.transfer_out(memory, sector, data),
        VIRTIO_BLK_T_FLUSH if data.is_empty() => {
          if self
            .backend
            .as_mut()
            .map_or(false, |backend| backend.flush().is_ok())
          {
            RequestResult::success()
          } else {
            RequestResult::io_error()
          }
        }
        VIRTIO_BLK_T_FLUSH => RequestResult::io_error(),
        _ => RequestResult::unsupported(),
      }
    };

    if memory
      .write_bytes(VirtAddr(status_descriptor.addr), &[result.status])
      .is_some()
    {
      result.written = result.written.saturating_add(1);
      result.dma_write = true;
    } else {
      result.status = VIRTIO_BLK_S_IOERR;
    }
    result
  }

  /// Writes one used-ring element and advances the private index without publishing the index.
  fn write_used_element(
    &mut self,
    memory: &mut Memory,
    queue: VirtqueueAddr,
    head: u16,
    length: u32,
  ) -> bool {
    let slot = self.used_idx % self.queue_num as u16;
    let Some(element_address) = queue
      .used_addr
      .checked_add(4)
      .and_then(|address| address.checked_add(u64::from(slot) * 8))
    else {
      return false;
    };
    let mut element = [0_u8; 8];
    element[0..4].copy_from_slice(&u32::from(head).to_le_bytes());
    element[4..8].copy_from_slice(&length.to_le_bytes());
    if memory
      .write_bytes(VirtAddr(element_address), &element)
      .is_none()
    {
      return false;
    }
    self.used_idx = self.used_idx.wrapping_add(1);
    true
  }

  /// Drains every descriptor chain published before the current available index.
  ///
  /// Guest queue mistakes and backend failures are represented in request status bytes and never
  /// escape as CPU exceptions. Calling this without a pending queue notification is a no-op.
  pub fn service_queue(&mut self, memory: &mut Memory) -> VirtioServiceResult {
    let mut result = VirtioServiceResult {
      interrupt_asserted: self.interrupt_asserted(),
      ..VirtioServiceResult::default()
    };
    if !self.notify_pending {
      return result;
    }
    self.notify_pending = false;
    if self.backend.is_none() || self.status & VIRTIO_STATUS_DRIVER_OK == 0 {
      return result;
    }
    let Some(queue) = VirtqueueAddr::from_device(self) else {
      return result;
    };
    let queue_num = self.queue_num as u16;
    if !queue.fully_mapped(memory, queue_num) {
      return result;
    }
    let Some(avail_flags) = Self::read_u16(memory, queue.avail_addr) else {
      return result;
    };
    let Some(avail_idx) = Self::read_u16(memory, queue.avail_addr + 2) else {
      return result;
    };
    let available = avail_idx.wrapping_sub(self.last_avail_idx);
    if available > queue_num {
      self.last_avail_idx = avail_idx;
      return result;
    }

    let mut descriptors = std::mem::take(&mut self.descriptor_scratch);
    let mut pending_completions = 0_u16;
    for _ in 0..available {
      let slot = self.last_avail_idx % queue_num;
      let Some(ring_address) = queue
        .avail_addr
        .checked_add(4)
        .and_then(|address| address.checked_add(u64::from(slot) * 2))
      else {
        self.last_avail_idx = self.last_avail_idx.wrapping_add(1);
        continue;
      };
      let head = Self::read_u16(memory, ring_address).unwrap_or(queue_num);
      self.last_avail_idx = self.last_avail_idx.wrapping_add(1);
      let request = if Self::descriptor_chain(memory, queue, queue_num, head, &mut descriptors) {
        self.process_request(memory, &descriptors)
      } else {
        RequestResult::io_error()
      };
      if self.write_used_element(memory, queue, head, request.written) {
        pending_completions = pending_completions.saturating_add(1);
      }
      result.dma_write |= request.dma_write;
    }
    self.descriptor_scratch = descriptors;

    // Publish the whole batch only after every visible used-ring element has been written.
    if pending_completions != 0
      && Self::write_u16(memory, queue.used_addr + 2, self.used_idx)
    {
      result.completed = pending_completions;
      result.dma_write = true;
    }

    if result.completed != 0 && avail_flags & VIRTQ_AVAIL_F_NO_INTERRUPT == 0 {
      self.interrupt_status |= VIRTIO_MMIO_INT_VRING;
    }
    result.interrupt_asserted = self.interrupt_asserted();
    result
  }
}

impl RequestResult {
  fn success() -> Self {
    Self {
      status: VIRTIO_BLK_S_OK,
      written: 0,
      dma_write: false,
    }
  }

  fn io_error() -> Self {
    Self {
      status: VIRTIO_BLK_S_IOERR,
      written: 0,
      dma_write: false,
    }
  }

  fn unsupported() -> Self {
    Self {
      status: VIRTIO_BLK_S_UNSUPP,
      written: 0,
      dma_write: false,
    }
  }
}

#[cfg(test)]
mod tests {
  use std::cell::RefCell;
  use std::fs::{remove_file, File, OpenOptions};
  use std::io::{Read, Seek, SeekFrom};
  use std::rc::Rc;
  use std::time::{SystemTime, UNIX_EPOCH};

  use super::*;

  const MEMORY_BASE: u64 = 0x8000_0000;
  const QUEUE_BASE: u64 = MEMORY_BASE + 0x1000;
  const BUFFER_BASE: u64 = MEMORY_BASE + 0x4000;

  #[derive(Default)]
  struct BackendState {
    bytes: Vec<u8>,
    flushes: usize,
    fail_read_after_partial_fill: bool,
    fail_flush: bool,
  }

  #[derive(Clone)]
  struct TestBackend(Rc<RefCell<BackendState>>);

  impl TestBackend {
    fn new(size: usize) -> Self {
      Self(Rc::new(RefCell::new(BackendState {
        bytes: vec![0; size],
        ..BackendState::default()
      })))
    }
  }

  impl BlockBackend for TestBackend {
    fn len(&self) -> u64 {
      self.0.borrow().bytes.len() as u64
    }

    fn read_at(&self, offset: u64, buffer: &mut [u8]) -> io::Result<()> {
      let state = self.0.borrow();
      let range = checked_backend_range(state.bytes.len(), offset, buffer.len())?;
      if state.fail_read_after_partial_fill {
        if let Some(first) = buffer.first_mut() {
          *first = 0xff;
        }
        return Err(io::Error::new(
          ErrorKind::Other,
          "injected partial read failure",
        ));
      }
      buffer.copy_from_slice(&state.bytes[range]);
      Ok(())
    }

    fn write_at(&mut self, offset: u64, buffer: &[u8]) -> io::Result<()> {
      let mut state = self.0.borrow_mut();
      let range = checked_backend_range(state.bytes.len(), offset, buffer.len())?;
      state.bytes[range].copy_from_slice(buffer);
      Ok(())
    }

    fn flush(&mut self) -> io::Result<()> {
      let mut state = self.0.borrow_mut();
      state.flushes += 1;
      if state.fail_flush {
        Err(io::Error::new(ErrorKind::Other, "injected flush failure"))
      } else {
        Ok(())
      }
    }
  }

  fn write_u16(memory: &mut Memory, address: u64, value: u16) {
    memory
      .write_bytes(VirtAddr(address), &value.to_le_bytes())
      .unwrap();
  }

  fn read_u16(memory: &Memory, address: u64) -> u16 {
    let mut bytes = [0; 2];
    memory.read_bytes(VirtAddr(address), &mut bytes).unwrap();
    u16::from_le_bytes(bytes)
  }

  fn read_u32(memory: &Memory, address: u64) -> u32 {
    let mut bytes = [0; 4];
    memory.read_bytes(VirtAddr(address), &mut bytes).unwrap();
    u32::from_le_bytes(bytes)
  }

  fn configured_with_queue_size<B: BlockBackend + 'static>(
    backend: B,
    queue_size: u16,
  ) -> (Virtio, Memory, VirtqueueAddr) {
    let mut virtio = Virtio::new(0);
    virtio.set_backend(backend).unwrap();
    let memory = Memory::new(MEMORY_BASE, 0x20_000).unwrap();
    virtio
      .write::<u32>(VirtAddr(GUEST_PAGE_SIZE), 0x1000)
      .unwrap();
    virtio
      .write::<u32>(VirtAddr(QUEUE_NUM), u32::from(queue_size))
      .unwrap();
    virtio
      .write::<u32>(VirtAddr(QUEUE_ALIGN), 0x1000)
      .unwrap();
    virtio
      .write::<u32>(VirtAddr(QUEUE_PFN), (QUEUE_BASE / 0x1000) as u32)
      .unwrap();
    virtio
      .write::<u32>(VirtAddr(STATUS), VIRTIO_STATUS_DRIVER_OK)
      .unwrap();
    let queue = VirtqueueAddr::from_device(&virtio).unwrap();
    (virtio, memory, queue)
  }

  fn configured(backend: TestBackend) -> (Virtio, Memory, VirtqueueAddr) {
    configured_with_queue_size(backend, QUEUE_SIZE)
  }

  fn write_desc(
    memory: &mut Memory,
    queue: VirtqueueAddr,
    index: u16,
    addr: u64,
    len: u32,
    flags: u16,
    next: u16,
  ) {
    let mut bytes = [0_u8; 16];
    bytes[0..8].copy_from_slice(&addr.to_le_bytes());
    bytes[8..12].copy_from_slice(&len.to_le_bytes());
    bytes[12..14].copy_from_slice(&flags.to_le_bytes());
    bytes[14..16].copy_from_slice(&next.to_le_bytes());
    memory
      .write_bytes(
        VirtAddr(queue.desc_addr + u64::from(index) * VRING_DESC_SIZE),
        &bytes,
      )
      .unwrap();
  }

  fn write_header(memory: &mut Memory, address: u64, request_type: u32, sector: u64) {
    let mut bytes = [0_u8; 16];
    bytes[0..4].copy_from_slice(&request_type.to_le_bytes());
    bytes[8..16].copy_from_slice(&sector.to_le_bytes());
    memory.write_bytes(VirtAddr(address), &bytes).unwrap();
  }

  fn publish(
    memory: &mut Memory,
    queue: VirtqueueAddr,
    queue_size: u16,
    index: u16,
    head: u16,
  ) {
    write_u16(
      memory,
      queue.avail_addr + 4 + u64::from(index % queue_size) * 2,
      head,
    );
    write_u16(memory, queue.avail_addr + 2, index.wrapping_add(1));
  }

  fn notify(virtio: &mut Virtio) {
    virtio.write::<u32>(VirtAddr(QUEUE_NOTIFY), 0).unwrap();
  }

  #[test]
  fn backend_validation_and_device_discovery() {
    let mut virtio = Virtio::new(99);
    assert_eq!(virtio.read::<u32>(VirtAddr(DEVICE_ID)).unwrap(), 0);
    assert_eq!(
      virtio.read::<u32>(VirtAddr(QUEUE_NUM_MAX)).unwrap(),
      u32::from(QUEUE_SIZE)
    );
    assert_eq!(
      virtio.read::<u32>(VirtAddr(DEVICE_FEATURES)).unwrap(),
      VIRTIO_BLK_F_SEG_MAX | VIRTIO_BLK_F_FLUSH
    );
    assert_eq!(
      virtio.set_backend(TestBackend::new(0)).unwrap_err().kind(),
      ErrorKind::InvalidInput
    );
    assert_eq!(
      virtio.set_backend(TestBackend::new(513)).unwrap_err().kind(),
      ErrorKind::InvalidInput
    );
    virtio.set_backend(TestBackend::new(1024)).unwrap();
    assert_eq!(virtio.read::<u32>(VirtAddr(DEVICE_ID)).unwrap(), 2);
    assert_eq!(virtio.read::<u32>(VirtAddr(CONFIG)).unwrap(), 2);
    assert_eq!(virtio.read::<u32>(VirtAddr(CONFIG + 8)).unwrap(), 0);
    assert_eq!(
      virtio.read::<u32>(VirtAddr(CONFIG + 12)).unwrap(),
      u32::from(QUEUE_SIZE - 2)
    );
    for (offset, byte) in u32::from(QUEUE_SIZE - 2)
      .to_le_bytes()
      .into_iter()
      .enumerate()
    {
      assert_eq!(
        virtio
          .read::<u8>(VirtAddr(CONFIG + 12 + offset as u64))
          .unwrap(),
        u32::from(byte)
      );
    }
    assert!(virtio.read::<u8>(VirtAddr(CONFIG + 16)).is_err());
    assert!(virtio.read::<u32>(VirtAddr(CONFIG + 13)).is_err());
  }

  #[test]
  fn mmap_backend_reads_writes_and_flushes() {
    let unique = SystemTime::now()
      .duration_since(UNIX_EPOCH)
      .unwrap()
      .as_nanos();
    let path = std::env::temp_dir().join(format!(
      "valheim-virtio-mmap-{}-{unique}.img",
      std::process::id()
    ));
    let file = OpenOptions::new()
      .read(true)
      .write(true)
      .create_new(true)
      .open(&path)
      .unwrap();
    file.set_len(512).unwrap();
    let mut mmap = unsafe { MmapMut::map_mut(&file) }.unwrap();
    BlockBackend::write_at(&mut mmap, 7, &[1, 2, 3]).unwrap();
    let mut bytes = [0_u8; 3];
    BlockBackend::read_at(&mmap, 7, &mut bytes).unwrap();
    assert_eq!(bytes, [1, 2, 3]);
    assert_eq!(
      BlockBackend::read_slice(&mmap, 7, 3).unwrap().unwrap(),
      &[1, 2, 3]
    );
    assert!(BlockBackend::read_slice(&mmap, 511, 3)
      .unwrap()
      .is_err());
    BlockBackend::flush(&mut mmap).unwrap();
    assert!(BlockBackend::read_at(&mmap, 511, &mut bytes).is_err());
    drop(mmap);
    drop(file);

    let mut file = File::open(&path).unwrap();
    file.seek(SeekFrom::Start(7)).unwrap();
    file.read_exact(&mut bytes).unwrap();
    assert_eq!(bytes, [1, 2, 3]);
    remove_file(path).unwrap();
  }

  #[test]
  fn mmap_backend_copies_directly_to_and_from_guest_dram() {
    let mut mmap = MmapMut::map_anon(4096).unwrap();
    for (index, byte) in mmap[0..512].iter_mut().enumerate() {
      *byte = index as u8;
    }
    let (mut virtio, mut memory, queue) = configured_with_queue_size(mmap, QUEUE_SIZE);
    let header = BUFFER_BASE;
    let first_data = BUFFER_BASE + 0x100;
    let second_data = BUFFER_BASE + 0x400;
    let status = BUFFER_BASE + 0x700;

    write_header(&mut memory, header, VIRTIO_BLK_T_IN, 0);
    write_desc(&mut memory, queue, 0, header, 16, VIRTQ_DESC_F_NEXT, 1);
    write_desc(
      &mut memory,
      queue,
      1,
      first_data,
      512,
      VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE,
      2,
    );
    write_desc(&mut memory, queue, 2, status, 1, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, queue, QUEUE_SIZE, 0, 0);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    let first = memory.slice(VirtAddr(first_data), 512).unwrap();
    assert_eq!(first[0], 0);
    assert_eq!(first[255], 255);

    memory
      .write_bytes(VirtAddr(first_data), &[0xa5; 512])
      .unwrap();
    write_header(&mut memory, header, VIRTIO_BLK_T_OUT, 1);
    write_desc(&mut memory, queue, 1, first_data, 512, VIRTQ_DESC_F_NEXT, 2);
    publish(&mut memory, queue, QUEUE_SIZE, 1, 0);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 1);

    write_header(&mut memory, header, VIRTIO_BLK_T_IN, 1);
    write_desc(
      &mut memory,
      queue,
      1,
      second_data,
      512,
      VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE,
      2,
    );
    publish(&mut memory, queue, QUEUE_SIZE, 2, 0);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    assert!(memory
      .slice(VirtAddr(second_data), 512)
      .unwrap()
      .iter()
      .all(|byte| *byte == 0xa5));
  }

  #[test]
  fn in_request_supports_nonzero_head_and_publishes_used_length() {
    let backend = TestBackend::new(4096);
    for (index, byte) in backend.0.borrow_mut().bytes[512..1024].iter_mut().enumerate() {
      *byte = index as u8;
    }
    let (mut virtio, mut memory, queue) = configured(backend);
    let header = BUFFER_BASE;
    let data = BUFFER_BASE + 0x100;
    let status = BUFFER_BASE + 0x400;
    write_header(&mut memory, header, VIRTIO_BLK_T_IN, 1);
    write_desc(&mut memory, queue, 5, header, 16, VIRTQ_DESC_F_NEXT, 6);
    write_desc(
      &mut memory,
      queue,
      6,
      data,
      512,
      VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE,
      7,
    );
    write_desc(&mut memory, queue, 7, status, 1, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, queue, QUEUE_SIZE, 0, 5);
    notify(&mut virtio);

    let result = virtio.service_queue(&mut memory);
    assert_eq!(result.completed, 1);
    assert!(result.interrupt_asserted);
    assert!(result.dma_write);
    let mut bytes = [0_u8; 512];
    memory.read_bytes(VirtAddr(data), &mut bytes).unwrap();
    assert_eq!(bytes[0], 0);
    assert_eq!(bytes[255], 255);
    assert_eq!(memory.read::<u8>(VirtAddr(status)), Some(VIRTIO_BLK_S_OK));
    assert_eq!(read_u32(&memory, queue.used_addr + 4), 5);
    assert_eq!(read_u32(&memory, queue.used_addr + 8), 513);
    assert_eq!(read_u16(&memory, queue.used_addr + 2), 1);
  }

  #[test]
  fn advertised_segment_limit_accepts_multiple_data_descriptors() {
    let backend = TestBackend::new(4096);
    for (index, byte) in backend.0.borrow_mut().bytes[0..512].iter_mut().enumerate() {
      *byte = index as u8;
    }
    let (mut virtio, mut memory, queue) = configured(backend);
    let header = BUFFER_BASE;
    let first_data = BUFFER_BASE + 0x100;
    let second_data = BUFFER_BASE + 0x300;
    let status = BUFFER_BASE + 0x500;
    write_header(&mut memory, header, VIRTIO_BLK_T_IN, 0);
    write_desc(&mut memory, queue, 0, header, 16, VIRTQ_DESC_F_NEXT, 1);
    write_desc(
      &mut memory,
      queue,
      1,
      first_data,
      256,
      VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE,
      2,
    );
    write_desc(
      &mut memory,
      queue,
      2,
      second_data,
      256,
      VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE,
      3,
    );
    write_desc(&mut memory, queue, 3, status, 1, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, queue, QUEUE_SIZE, 0, 0);
    notify(&mut virtio);

    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    let mut first = [0_u8; 256];
    let mut second = [0_u8; 256];
    memory.read_bytes(VirtAddr(first_data), &mut first).unwrap();
    memory
      .read_bytes(VirtAddr(second_data), &mut second)
      .unwrap();
    assert_eq!(first[0], 0);
    assert_eq!(first[255], 255);
    assert_eq!(second[0], 0);
    assert_eq!(second[255], 255);
    assert_eq!(memory.read::<u8>(VirtAddr(status)), Some(VIRTIO_BLK_S_OK));
    assert_eq!(read_u32(&memory, queue.used_addr + 8), 513);
  }

  #[test]
  fn failed_backend_read_does_not_copy_partial_scratch_into_guest() {
    let backend = TestBackend::new(4096);
    backend.0.borrow_mut().fail_read_after_partial_fill = true;
    let (mut virtio, mut memory, queue) = configured(backend);
    let header = BUFFER_BASE;
    let data = BUFFER_BASE + 0x100;
    let status = BUFFER_BASE + 0x400;
    memory
      .write_bytes(VirtAddr(data), &[0x5a; 512])
      .unwrap();
    write_header(&mut memory, header, VIRTIO_BLK_T_IN, 0);
    write_desc(&mut memory, queue, 0, header, 16, VIRTQ_DESC_F_NEXT, 1);
    write_desc(
      &mut memory,
      queue,
      1,
      data,
      512,
      VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE,
      2,
    );
    write_desc(&mut memory, queue, 2, status, 1, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, queue, QUEUE_SIZE, 0, 0);
    notify(&mut virtio);

    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    let mut guest_data = [0_u8; 512];
    memory
      .read_bytes(VirtAddr(data), &mut guest_data)
      .unwrap();
    assert!(guest_data.iter().all(|byte| *byte == 0x5a));
    assert_eq!(memory.read::<u8>(VirtAddr(status)), Some(VIRTIO_BLK_S_IOERR));
    assert_eq!(read_u32(&memory, queue.used_addr + 8), 1);
  }

  #[test]
  fn advertised_segment_limit_is_fully_serviceable() {
    let backend = TestBackend::new(128 * 512);
    for (index, byte) in backend.0.borrow_mut().bytes.iter_mut().enumerate() {
      *byte = index as u8;
    }
    let (mut virtio, mut memory, queue) = configured(backend);
    let header = BUFFER_BASE;
    let data = BUFFER_BASE + 0x100;
    let segment_count = QUEUE_SIZE - 2;
    let status = data + u64::from(segment_count) * 512;
    write_header(&mut memory, header, VIRTIO_BLK_T_IN, 0);
    write_desc(&mut memory, queue, 0, header, 16, VIRTQ_DESC_F_NEXT, 1);
    for index in 0..segment_count {
      let descriptor_index = index + 1;
      write_desc(
        &mut memory,
        queue,
        descriptor_index,
        data + u64::from(index) * 512,
        512,
        VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE,
        descriptor_index + 1,
      );
    }
    write_desc(
      &mut memory,
      queue,
      QUEUE_SIZE - 1,
      status,
      1,
      VIRTQ_DESC_F_WRITE,
      0,
    );
    publish(&mut memory, queue, QUEUE_SIZE, 0, 0);
    notify(&mut virtio);

    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    assert_eq!(memory.read::<u8>(VirtAddr(data)), Some(0));
    assert_eq!(
      memory.read::<u8>(VirtAddr(status - 1)),
      Some(((u64::from(segment_count) * 512 - 1) & 0xff) as u8)
    );
    assert_eq!(memory.read::<u8>(VirtAddr(status)), Some(VIRTIO_BLK_S_OK));
    assert_eq!(
      read_u32(&memory, queue.used_addr + 8),
      u32::from(segment_count) * 512 + 1
    );
  }

  #[test]
  fn one_notification_batches_used_index_for_multiple_out_requests() {
    let backend = TestBackend::new(4096);
    let observer = backend.clone();
    let (mut virtio, mut memory, queue) = configured(backend);
    let requests = [
      (1_u16, 1_u64, 0x55_u8, BUFFER_BASE),
      (4, 2, 0xaa, BUFFER_BASE + 0x800),
    ];
    for (head, sector, fill, base) in requests {
      write_header(&mut memory, base, VIRTIO_BLK_T_OUT, sector);
      memory
        .write_bytes(VirtAddr(base + 0x100), &vec![fill; 512])
        .unwrap();
      write_desc(&mut memory, queue, head, base, 16, VIRTQ_DESC_F_NEXT, head + 1);
      write_desc(
        &mut memory,
        queue,
        head + 1,
        base + 0x100,
        512,
        VIRTQ_DESC_F_NEXT,
        head + 2,
      );
      write_desc(
        &mut memory,
        queue,
        head + 2,
        base + 0x400,
        1,
        VIRTQ_DESC_F_WRITE,
        0,
      );
    }
    publish(&mut memory, queue, QUEUE_SIZE, 0, 1);
    publish(&mut memory, queue, QUEUE_SIZE, 1, 4);
    notify(&mut virtio);

    let result = virtio.service_queue(&mut memory);
    assert_eq!(result.completed, 2);
    assert!(result.dma_write);
    assert_eq!(read_u16(&memory, queue.used_addr + 2), 2);
    assert_eq!(read_u32(&memory, queue.used_addr + 4), 1);
    assert_eq!(read_u32(&memory, queue.used_addr + 12), 4);
    let state = observer.0.borrow();
    assert!(state.bytes[512..1024].iter().all(|byte| *byte == 0x55));
    assert!(state.bytes[1024..1536].iter().all(|byte| *byte == 0xaa));
    drop(state);

    assert!(virtio.descriptor_scratch.capacity() >= 3);
    let scratch_pointer = virtio.descriptor_scratch.as_ptr();
    publish(&mut memory, queue, QUEUE_SIZE, 2, 1);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    assert_eq!(virtio.descriptor_scratch.as_ptr(), scratch_pointer);
    assert_eq!(read_u16(&memory, queue.used_addr + 2), 3);
    assert_eq!(read_u32(&memory, queue.used_addr + 20), 1);
  }

  #[test]
  fn batched_used_index_includes_malformed_request_completions() {
    let backend = TestBackend::new(4096);
    let observer = backend.clone();
    let queue_size = 8;
    let (mut virtio, mut memory, queue) = configured_with_queue_size(backend, queue_size);

    write_desc(
      &mut memory,
      queue,
      0,
      BUFFER_BASE,
      16,
      VIRTQ_DESC_F_NEXT,
      0,
    );
    let header = BUFFER_BASE + 0x200;
    let status = BUFFER_BASE + 0x300;
    write_header(&mut memory, header, VIRTIO_BLK_T_FLUSH, 0);
    write_desc(&mut memory, queue, 1, header, 16, VIRTQ_DESC_F_NEXT, 2);
    write_desc(&mut memory, queue, 2, status, 1, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, queue, queue_size, 0, 0);
    publish(&mut memory, queue, queue_size, 1, 1);
    notify(&mut virtio);

    let result = virtio.service_queue(&mut memory);
    assert_eq!(result.completed, 2);
    assert!(result.dma_write);
    assert!(result.interrupt_asserted);
    assert_eq!(read_u16(&memory, queue.used_addr + 2), 2);
    assert_eq!(read_u32(&memory, queue.used_addr + 4), 0);
    assert_eq!(read_u32(&memory, queue.used_addr + 8), 0);
    assert_eq!(read_u32(&memory, queue.used_addr + 12), 1);
    assert_eq!(read_u32(&memory, queue.used_addr + 16), 1);
    assert_eq!(memory.read::<u8>(VirtAddr(status)), Some(VIRTIO_BLK_S_OK));
    assert_eq!(observer.0.borrow().flushes, 1);
  }

  #[test]
  fn flush_is_supported_and_interrupt_ack_is_write_one_to_clear() {
    let backend = TestBackend::new(4096);
    let observer = backend.clone();
    let (mut virtio, mut memory, queue) = configured(backend);
    write_header(&mut memory, BUFFER_BASE, VIRTIO_BLK_T_FLUSH, 0);
    write_desc(
      &mut memory,
      queue,
      2,
      BUFFER_BASE,
      16,
      VIRTQ_DESC_F_NEXT,
      3,
    );
    write_desc(
      &mut memory,
      queue,
      3,
      BUFFER_BASE + 0x100,
      1,
      VIRTQ_DESC_F_WRITE,
      0,
    );
    publish(&mut memory, queue, QUEUE_SIZE, 0, 2);
    notify(&mut virtio);
    let result = virtio.service_queue(&mut memory);
    assert_eq!(result.completed, 1);
    assert!(result.dma_write);
    assert_eq!(observer.0.borrow().flushes, 1);
    assert!(virtio.interrupt_asserted());

    observer.0.borrow_mut().fail_flush = true;
    memory
      .write::<u8>(VirtAddr(BUFFER_BASE + 0x100), 0xff)
      .unwrap();
    publish(&mut memory, queue, QUEUE_SIZE, 1, 2);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    assert_eq!(observer.0.borrow().flushes, 2);
    assert_eq!(
      memory.read::<u8>(VirtAddr(BUFFER_BASE + 0x100)),
      Some(VIRTIO_BLK_S_IOERR)
    );

    virtio.interrupt_status |= 2;
    virtio.write::<u8>(VirtAddr(INTERRUPT_ACK), 1).unwrap();
    assert_eq!(virtio.interrupt_status, 2);
    virtio.write::<u8>(VirtAddr(INTERRUPT_ACK), 2).unwrap();
    assert!(!virtio.interrupt_asserted());
  }

  #[test]
  fn batched_available_and_used_indices_wrap() {
    let backend = TestBackend::new(4096);
    let queue_size = 8;
    let (mut virtio, mut memory, queue) = configured_with_queue_size(backend, queue_size);
    for (head, base) in [(0_u16, BUFFER_BASE), (2_u16, BUFFER_BASE + 0x200)] {
      write_header(&mut memory, base, VIRTIO_BLK_T_FLUSH, 0);
      write_desc(&mut memory, queue, head, base, 16, VIRTQ_DESC_F_NEXT, head + 1);
      write_desc(
        &mut memory,
        queue,
        head + 1,
        base + 0x100,
        1,
        VIRTQ_DESC_F_WRITE,
        0,
      );
    }
    virtio.last_avail_idx = u16::MAX;
    virtio.used_idx = u16::MAX;
    write_u16(&mut memory, queue.used_addr + 2, u16::MAX);
    let wrapped_slot = u64::from(u16::MAX % queue_size);
    write_u16(
      &mut memory,
      queue.avail_addr + 4 + wrapped_slot * 2,
      0,
    );
    write_u16(&mut memory, queue.avail_addr + 4, 2);
    write_u16(&mut memory, queue.avail_addr + 2, 1);
    notify(&mut virtio);

    let result = virtio.service_queue(&mut memory);
    assert_eq!(result.completed, 2);
    assert!(result.dma_write);
    assert!(result.interrupt_asserted);
    assert_eq!(virtio.last_avail_idx, 1);
    assert_eq!(virtio.used_idx, 1);
    assert_eq!(
      read_u32(&memory, queue.used_addr + 4 + wrapped_slot * 8),
      0
    );
    assert_eq!(
      read_u32(&memory, queue.used_addr + 8 + wrapped_slot * 8),
      1
    );
    assert_eq!(read_u32(&memory, queue.used_addr + 4), 2);
    assert_eq!(read_u32(&memory, queue.used_addr + 8), 1);
    assert_eq!(read_u16(&memory, queue.used_addr + 2), 1);
  }

  #[test]
  fn bad_chains_and_out_of_range_requests_do_not_panic() {
    let backend = TestBackend::new(1024);
    let (mut virtio, mut memory, queue) = configured(backend);
    write_desc(
      &mut memory,
      queue,
      0,
      BUFFER_BASE,
      16,
      VIRTQ_DESC_F_NEXT,
      0,
    );
    publish(&mut memory, queue, QUEUE_SIZE, 0, 0);
    notify(&mut virtio);
    let result = virtio.service_queue(&mut memory);
    assert_eq!(result.completed, 1);
    assert!(result.dma_write);
    assert_eq!(read_u32(&memory, queue.used_addr + 8), 0);

    write_header(&mut memory, BUFFER_BASE, VIRTIO_BLK_T_OUT, 2);
    write_desc(&mut memory, queue, 1, BUFFER_BASE, 16, VIRTQ_DESC_F_NEXT, 2);
    write_desc(
      &mut memory,
      queue,
      2,
      BUFFER_BASE + 0x100,
      512,
      VIRTQ_DESC_F_NEXT,
      3,
    );
    write_desc(
      &mut memory,
      queue,
      3,
      BUFFER_BASE + 0x400,
      1,
      VIRTQ_DESC_F_WRITE,
      0,
    );
    publish(&mut memory, queue, QUEUE_SIZE, 1, 1);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    assert_eq!(
      memory.read::<u8>(VirtAddr(BUFFER_BASE + 0x400)),
      Some(VIRTIO_BLK_S_IOERR)
    );

    write_header(&mut memory, BUFFER_BASE, VIRTIO_BLK_T_IN, 0);
    write_desc(&mut memory, queue, 4, BUFFER_BASE, 16, VIRTQ_DESC_F_NEXT, 5);
    write_desc(
      &mut memory,
      queue,
      5,
      MEMORY_BASE + 0x20_000,
      512,
      VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE,
      6,
    );
    write_desc(
      &mut memory,
      queue,
      6,
      BUFFER_BASE + 0x500,
      1,
      VIRTQ_DESC_F_WRITE,
      0,
    );
    publish(&mut memory, queue, QUEUE_SIZE, 2, 4);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    assert_eq!(
      memory.read::<u8>(VirtAddr(BUFFER_BASE + 0x500)),
      Some(VIRTIO_BLK_S_IOERR)
    );
  }

  #[test]
  fn unsupported_opcode_gets_unsupp_status() {
    let backend = TestBackend::new(1024);
    let (mut virtio, mut memory, queue) = configured(backend);
    write_header(&mut memory, BUFFER_BASE, 99, 0);
    write_desc(&mut memory, queue, 0, BUFFER_BASE, 16, VIRTQ_DESC_F_NEXT, 1);
    write_desc(
      &mut memory,
      queue,
      1,
      BUFFER_BASE + 0x100,
      1,
      VIRTQ_DESC_F_WRITE,
      0,
    );
    publish(&mut memory, queue, QUEUE_SIZE, 0, 0);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 1);
    assert_eq!(
      memory.read::<u8>(VirtAddr(BUFFER_BASE + 0x100)),
      Some(VIRTIO_BLK_S_UNSUPP)
    );
  }

  #[test]
  fn service_without_notify_is_idempotent_and_reset_clears_transport() {
    let backend = TestBackend::new(1024);
    let (mut virtio, mut memory, _) = configured(backend);
    assert_eq!(virtio.service_queue(&mut memory), VirtioServiceResult::default());
    virtio.interrupt_status = 3;
    virtio.notify_pending = true;
    virtio.last_avail_idx = 7;
    virtio.used_idx = 9;
    let guest_page_size = virtio.guest_page_size;
    virtio.write::<u32>(VirtAddr(STATUS), 0).unwrap();
    assert_eq!(virtio.status, 0);
    assert_eq!(virtio.interrupt_status, 0);
    assert!(!virtio.notify_pending);
    assert_eq!(virtio.queue_pfn, 0);
    assert_eq!(virtio.last_avail_idx, 0);
    assert_eq!(virtio.used_idx, 0);
    assert_eq!(virtio.guest_page_size, guest_page_size);
    assert_eq!(virtio.read::<u32>(VirtAddr(DEVICE_ID)).unwrap(), 2);

    virtio.write::<u32>(VirtAddr(QUEUE_SEL), 1).unwrap();
    assert_eq!(virtio.read::<u32>(VirtAddr(QUEUE_NUM_MAX)).unwrap(), 0);
    virtio.write::<u32>(VirtAddr(QUEUE_PFN), 123).unwrap();
    virtio.write::<u32>(VirtAddr(QUEUE_SEL), 0).unwrap();
    assert_eq!(virtio.read::<u32>(VirtAddr(QUEUE_PFN)).unwrap(), 0);
    virtio.write::<u32>(VirtAddr(DEVICE_FEATURES_SEL), 99).unwrap();
    assert_eq!(virtio.read::<u32>(VirtAddr(DEVICE_FEATURES)).unwrap(), 0);
    virtio.write::<u32>(VirtAddr(STATUS), 128).unwrap();
    assert_eq!(virtio.read::<u32>(VirtAddr(STATUS)).unwrap(), 128);
  }

  #[test]
  fn overrun_available_ring_is_resynchronized_without_io() {
    let backend = TestBackend::new(1024);
    let observer = backend.clone();
    let (mut virtio, mut memory, queue) = configured(backend);
    write_u16(&mut memory, queue.avail_addr + 2, QUEUE_SIZE + 1);
    notify(&mut virtio);
    assert_eq!(virtio.service_queue(&mut memory).completed, 0);
    assert_eq!(virtio.last_avail_idx, QUEUE_SIZE + 1);
    assert!(observer.0.borrow().bytes.iter().all(|byte| *byte == 0));
  }
}
