use std::collections::VecDeque;
use std::io;

use crate::cpu::irq::Exception;
use crate::memory::{CanIO, Memory, VirtAddr};

use super::{
  descriptor_chain, read_u16, write_u16, write_used_element, EthernetBackend,
  EthernetBackendFactory, LegacyTransport, NetworkWake, VirtioServiceResult, VirtqDesc,
  VirtqueueAddr, VIRTIO_F_RING_INDIRECT_DESC, VIRTQ_AVAIL_F_NO_INTERRUPT,
  VIRTQ_DESC_F_WRITE,
};

/// The PLIC source reserved for the VirtIO network device.
pub const VIRTIO_NET_IRQ: u64 = 2;

pub const VIRTIO_NET_RX_QUEUE: usize = 0;
pub const VIRTIO_NET_TX_QUEUE: usize = 1;

const VIRTIO_NET_DEVICE_ID: u32 = 1;
const VIRTIO_NET_F_MAC: u32 = 1 << 5;
const VIRTIO_NET_HDR_LEN: usize = 10;
const MIN_ETHERNET_FRAME_LEN: usize = 14;
// MTU 1500 plus an Ethernet header and one 802.1Q VLAN tag. Linux 5.17 sizes its non-mergeable
// receive buffers for this boundary even when no VLAN offload feature is negotiated.
const MAX_ETHERNET_FRAME_LEN: usize = 1518;
const RX_BACKLOG_LIMIT: usize = 256;
const MAX_BACKEND_FRAMES_PER_SERVICE: usize = 256;

#[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
pub struct VirtioNetStats {
  pub rx_frames: u64,
  pub tx_frames: u64,
  pub rx_dropped: u64,
  pub tx_dropped: u64,
  pub malformed_rx_buffers: u64,
  pub malformed_tx_packets: u64,
  pub backend_errors: u64,
}

/// Legacy VirtIO-MMIO version 1 network device with one RX and one TX split virtqueue.
///
/// Host workers communicate only through [`EthernetBackend`]. Guest memory is accessed solely by
/// [`VirtioNet::service_queues`] on the machine thread.
pub struct VirtioNet {
  transport: LegacyTransport,
  mac: [u8; 6],
  backend: Option<Box<dyn EthernetBackend>>,
  rx_backlog: VecDeque<Vec<u8>>,
  rx_descriptors: Vec<VirtqDesc>,
  tx_descriptors: Vec<VirtqDesc>,
  rx_scratch: Vec<u8>,
  tx_scratch: Vec<u8>,
  stats: VirtioNetStats,
  backend_failed: bool,
  backend_error: Option<io::Error>,
}

impl VirtioNet {
  pub fn new(base: u64, mac: [u8; 6]) -> Self {
    Self {
      transport: LegacyTransport::new(
        base,
        [VIRTIO_NET_F_MAC | VIRTIO_F_RING_INDIRECT_DESC, 0],
        2,
      ),
      mac,
      backend: None,
      rx_backlog: VecDeque::with_capacity(RX_BACKLOG_LIMIT),
      rx_descriptors: Vec::new(),
      tx_descriptors: Vec::new(),
      rx_scratch: Vec::new(),
      tx_scratch: Vec::new(),
      stats: VirtioNetStats::default(),
      backend_failed: false,
      backend_error: None,
    }
  }

  pub fn base(&self) -> u64 {
    self.transport.base
  }

  pub fn mac(&self) -> [u8; 6] {
    self.mac
  }

  pub fn backend_attached(&self) -> bool {
    self.backend.is_some()
  }

  pub fn attach_backend(&mut self, backend: Box<dyn EthernetBackend>) -> io::Result<()> {
    self.shutdown()?;
    self.backend = Some(backend);
    self.transport.reset();
    self.clear_device_buffers();
    Ok(())
  }

  pub fn set_backend<B: EthernetBackend + 'static>(&mut self, backend: B) -> io::Result<()> {
    self.attach_backend(Box::new(backend))
  }

  pub fn start_backend(
    &mut self,
    factory: Box<dyn EthernetBackendFactory>,
    wake: NetworkWake,
  ) -> io::Result<()> {
    let backend = factory.start(wake)?;
    self.attach_backend(backend)
  }

  /// Stops and removes the backend. Repeated calls are harmless.
  pub fn shutdown(&mut self) -> io::Result<()> {
    let result = self
      .backend
      .as_mut()
      .map_or(Ok(()), |backend| backend.shutdown());
    self.backend = None;
    self.transport.reset();
    self.clear_device_buffers();
    result
  }

  pub fn interrupt_asserted(&self) -> bool {
    self.transport.interrupt_asserted()
  }

  pub fn stats(&self) -> VirtioNetStats {
    self.stats
  }

  pub fn take_backend_error(&mut self) -> Option<io::Error> {
    self.backend_error.take()
  }

  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Result<u32, Exception> {
    self.transport.read::<T>(
      addr,
      if self.backend.is_some() {
        VIRTIO_NET_DEVICE_ID
      } else {
        0
      },
      &self.mac,
    )
  }

  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, value: u32) -> Result<(), Exception> {
    self.transport.write::<T>(addr, value)?;
    let status_start = self.transport.base + super::STATUS_OFFSET;
    if addr.0 >= status_start
      && addr.0 < status_start + 4
      && self.transport.status == 0
    {
      self.clear_device_buffers();
    }
    Ok(())
  }

  /// Polls the nonblocking backend and services both network queues.
  pub fn service_queues(&mut self, memory: &mut Memory) -> VirtioServiceResult {
    self.poll_backend();
    let mut result = VirtioServiceResult {
      interrupt_asserted: self.interrupt_asserted(),
      ..VirtioServiceResult::default()
    };
    if self.backend.is_none() || !self.transport.driver_ok() {
      return result;
    }

    self.service_tx(memory, &mut result);
    self.service_rx(memory, &mut result);
    result.interrupt_asserted = self.interrupt_asserted();
    result
  }

  /// Alias matching the block frontend's service method.
  pub fn service_queue(&mut self, memory: &mut Memory) -> VirtioServiceResult {
    self.service_queues(memory)
  }

  fn clear_device_buffers(&mut self) {
    self.rx_backlog.clear();
    self.rx_descriptors.clear();
    self.tx_descriptors.clear();
    self.rx_scratch.clear();
    self.tx_scratch.clear();
    self.backend_failed = false;
    self.backend_error = None;
  }

  fn record_backend_error(&mut self, error: io::Error) {
    self.stats.backend_errors = self.stats.backend_errors.saturating_add(1);
    self.backend_failed = true;
    self.backend_error = Some(error);
  }

  fn poll_backend(&mut self) {
    if self.backend_failed || self.rx_backlog.len() >= RX_BACKLOG_LIMIT {
      return;
    }
    for _ in 0..MAX_BACKEND_FRAMES_PER_SERVICE {
      if self.rx_backlog.len() >= RX_BACKLOG_LIMIT {
        break;
      }
      let received = match self.backend.as_mut() {
        Some(backend) => backend.try_recv(),
        None => return,
      };
      match received {
        Ok(Some(frame)) if valid_frame_len(frame.len()) => self.rx_backlog.push_back(frame),
        Ok(Some(_)) => {
          self.stats.rx_dropped = self.stats.rx_dropped.saturating_add(1);
        }
        Ok(None) => break,
        Err(error) => {
          self.record_backend_error(error);
          break;
        }
      }
    }
  }

  fn service_tx(&mut self, memory: &mut Memory, result: &mut VirtioServiceResult) {
    let Some(state) = self.transport.queue_mut(VIRTIO_NET_TX_QUEUE) else {
      return;
    };
    if !state.notify_pending {
      return;
    }
    state.notify_pending = false;

    let Some(queue) = VirtqueueAddr::from_transport(&self.transport, VIRTIO_NET_TX_QUEUE) else {
      return;
    };
    let queue_num = self.transport.queues[VIRTIO_NET_TX_QUEUE].num as u16;
    if !queue.fully_mapped(memory, queue_num) {
      return;
    }
    let Some(avail_flags) = read_u16(memory, queue.avail_addr) else {
      return;
    };
    let Some(avail_idx) = read_u16(memory, queue.avail_addr + 2) else {
      return;
    };
    let mut last_avail_idx = self.transport.queues[VIRTIO_NET_TX_QUEUE].last_avail_idx;
    let mut used_idx = self.transport.queues[VIRTIO_NET_TX_QUEUE].used_idx;
    let available = avail_idx.wrapping_sub(last_avail_idx);
    if available > queue_num {
      self.transport.queues[VIRTIO_NET_TX_QUEUE].last_avail_idx = avail_idx;
      return;
    }

    let indirect_enabled = self.transport.indirect_enabled();
    let mut descriptors = std::mem::take(&mut self.tx_descriptors);
    let mut completed = 0_u16;
    for _ in 0..available {
      let slot = last_avail_idx % queue_num;
      let head = queue
        .avail_addr
        .checked_add(4)
        .and_then(|address| address.checked_add(u64::from(slot) * 2))
        .and_then(|address| read_u16(memory, address))
        .unwrap_or(queue_num);
      last_avail_idx = last_avail_idx.wrapping_add(1);

      if descriptor_chain(
        memory,
        queue,
        queue_num,
        head,
        indirect_enabled,
        &mut descriptors,
      ) {
        self.transmit(memory, &descriptors);
      } else {
        self.stats.malformed_tx_packets = self.stats.malformed_tx_packets.saturating_add(1);
        self.stats.tx_dropped = self.stats.tx_dropped.saturating_add(1);
      }
      if write_used_element(memory, queue, queue_num, &mut used_idx, head, 0) {
        completed = completed.saturating_add(1);
      }
    }
    self.tx_descriptors = descriptors;
    self.transport.queues[VIRTIO_NET_TX_QUEUE].last_avail_idx = last_avail_idx;
    self.transport.queues[VIRTIO_NET_TX_QUEUE].used_idx = used_idx;
    publish_completions(
      &mut self.transport,
      memory,
      queue,
      used_idx,
      completed,
      avail_flags,
      result,
    );
  }

  fn transmit(&mut self, memory: &Memory, descriptors: &[VirtqDesc]) {
    let Some(total) = descriptor_capacity(memory, descriptors, false) else {
      self.malformed_tx();
      return;
    };
    if total < VIRTIO_NET_HDR_LEN + MIN_ETHERNET_FRAME_LEN
      || total > VIRTIO_NET_HDR_LEN + MAX_ETHERNET_FRAME_LEN
    {
      self.malformed_tx();
      return;
    }

    self.tx_scratch.resize(total, 0);
    let mut copied = 0_usize;
    for descriptor in descriptors {
      let length = descriptor.len as usize;
      let Some(bytes) = memory.slice(VirtAddr(descriptor.addr), length) else {
        self.malformed_tx();
        return;
      };
      self.tx_scratch[copied..copied + length].copy_from_slice(bytes);
      copied += length;
    }
    let frame = &self.tx_scratch[VIRTIO_NET_HDR_LEN..];
    let sent = if self.backend_failed {
      Ok(false)
    } else {
      match self.backend.as_mut() {
        Some(backend) => backend.try_send(frame),
        None => Ok(false),
      }
    };
    match sent {
      Ok(true) => self.stats.tx_frames = self.stats.tx_frames.saturating_add(1),
      Ok(false) => self.stats.tx_dropped = self.stats.tx_dropped.saturating_add(1),
      Err(error) => {
        self.stats.tx_dropped = self.stats.tx_dropped.saturating_add(1);
        self.record_backend_error(error);
      }
    }
  }

  fn malformed_tx(&mut self) {
    self.stats.malformed_tx_packets = self.stats.malformed_tx_packets.saturating_add(1);
    self.stats.tx_dropped = self.stats.tx_dropped.saturating_add(1);
  }

  fn service_rx(&mut self, memory: &mut Memory, result: &mut VirtioServiceResult) {
    let queue_was_notified = self
      .transport
      .queue(VIRTIO_NET_RX_QUEUE)
      .map_or(false, |queue| queue.notify_pending);
    if self.rx_backlog.is_empty() && !queue_was_notified {
      return;
    }
    if let Some(queue) = self.transport.queue_mut(VIRTIO_NET_RX_QUEUE) {
      queue.notify_pending = false;
    }
    if self.rx_backlog.is_empty() {
      return;
    }

    let Some(queue) = VirtqueueAddr::from_transport(&self.transport, VIRTIO_NET_RX_QUEUE) else {
      return;
    };
    let queue_num = self.transport.queues[VIRTIO_NET_RX_QUEUE].num as u16;
    if !queue.fully_mapped(memory, queue_num) {
      return;
    }
    let Some(avail_flags) = read_u16(memory, queue.avail_addr) else {
      return;
    };
    let Some(avail_idx) = read_u16(memory, queue.avail_addr + 2) else {
      return;
    };
    let mut last_avail_idx = self.transport.queues[VIRTIO_NET_RX_QUEUE].last_avail_idx;
    let mut used_idx = self.transport.queues[VIRTIO_NET_RX_QUEUE].used_idx;
    let available = avail_idx.wrapping_sub(last_avail_idx);
    if available > queue_num {
      self.transport.queues[VIRTIO_NET_RX_QUEUE].last_avail_idx = avail_idx;
      return;
    }

    let indirect_enabled = self.transport.indirect_enabled();
    let mut descriptors = std::mem::take(&mut self.rx_descriptors);
    let mut completed = 0_u16;
    for _ in 0..available {
      let Some(frame) = self.rx_backlog.front() else {
        break;
      };
      let slot = last_avail_idx % queue_num;
      let head = queue
        .avail_addr
        .checked_add(4)
        .and_then(|address| address.checked_add(u64::from(slot) * 2))
        .and_then(|address| read_u16(memory, address))
        .unwrap_or(queue_num);
      last_avail_idx = last_avail_idx.wrapping_add(1);

      let chain_valid = descriptor_chain(
        memory,
        queue,
        queue_num,
        head,
        indirect_enabled,
        &mut descriptors,
      );
      let written = if chain_valid {
        receive_into(memory, &descriptors, frame, &mut self.rx_scratch)
      } else {
        None
      };
      let length = match written {
        Some(length) => {
          self.rx_backlog.pop_front();
          self.stats.rx_frames = self.stats.rx_frames.saturating_add(1);
          result.dma_write = true;
          length
        }
        None => {
          self.stats.malformed_rx_buffers = self.stats.malformed_rx_buffers.saturating_add(1);
          0
        }
      };
      if write_used_element(memory, queue, queue_num, &mut used_idx, head, length) {
        completed = completed.saturating_add(1);
      }
    }
    self.rx_descriptors = descriptors;
    self.transport.queues[VIRTIO_NET_RX_QUEUE].last_avail_idx = last_avail_idx;
    self.transport.queues[VIRTIO_NET_RX_QUEUE].used_idx = used_idx;
    publish_completions(
      &mut self.transport,
      memory,
      queue,
      used_idx,
      completed,
      avail_flags,
      result,
    );
  }
}

impl Drop for VirtioNet {
  fn drop(&mut self) {
    if let Some(backend) = self.backend.as_mut() {
      let _ = backend.shutdown();
    }
  }
}

fn valid_frame_len(length: usize) -> bool {
  (MIN_ETHERNET_FRAME_LEN..=MAX_ETHERNET_FRAME_LEN).contains(&length)
}

fn descriptor_capacity(
  memory: &Memory,
  descriptors: &[VirtqDesc],
  writable: bool,
) -> Option<usize> {
  let mut total = 0_usize;
  for descriptor in descriptors {
    if (descriptor.flags & VIRTQ_DESC_F_WRITE != 0) != writable {
      return None;
    }
    let length = descriptor.len as usize;
    if !memory.contains(VirtAddr(descriptor.addr), length) {
      return None;
    }
    total = total.checked_add(length)?;
  }
  Some(total)
}

fn receive_into(
  memory: &mut Memory,
  descriptors: &[VirtqDesc],
  frame: &[u8],
  scratch: &mut Vec<u8>,
) -> Option<u32> {
  let required = VIRTIO_NET_HDR_LEN.checked_add(frame.len())?;
  if descriptor_capacity(memory, descriptors, true)? < required {
    return None;
  }
  scratch.clear();
  scratch.resize(VIRTIO_NET_HDR_LEN, 0);
  scratch.extend_from_slice(frame);

  let mut copied = 0_usize;
  for descriptor in descriptors {
    if copied == required {
      break;
    }
    let length = (descriptor.len as usize).min(required - copied);
    memory.write_bytes(
      VirtAddr(descriptor.addr),
      &scratch[copied..copied + length],
    )?;
    copied += length;
  }
  (copied == required).then(|| required as u32)
}

fn publish_completions(
  transport: &mut LegacyTransport,
  memory: &mut Memory,
  queue: VirtqueueAddr,
  used_idx: u16,
  completed: u16,
  avail_flags: u16,
  result: &mut VirtioServiceResult,
) {
  if completed == 0 || !write_u16(memory, queue.used_addr + 2, used_idx) {
    return;
  }
  result.completed = result.completed.saturating_add(completed);
  result.dma_write = true;
  if avail_flags & VIRTQ_AVAIL_F_NO_INTERRUPT == 0 {
    transport.raise_vring_interrupt();
  }
}

#[cfg(test)]
mod tests {
  use std::collections::VecDeque;
  use std::io::{self, ErrorKind};
  use std::sync::{Arc, Mutex};

  use super::*;
  use crate::cpu::bus::VIRTIO_BASE;
  use crate::device::virtio::{
    CONFIG_OFFSET, QUEUE_SIZE, VIRTIO_STATUS_DRIVER_OK, VIRTQ_DESC_F_INDIRECT,
    VIRTQ_DESC_F_NEXT,
  };

  const NET_BASE: u64 = VIRTIO_BASE + 0x1000;
  const MEMORY_BASE: u64 = 0x8000_0000;
  const RX_QUEUE_BASE: u64 = MEMORY_BASE + 0x1000;
  const TX_QUEUE_BASE: u64 = MEMORY_BASE + 0x3000;
  const BUFFER_BASE: u64 = MEMORY_BASE + 0x6000;
  const MAC: [u8; 6] = [0x52, 0x54, 0, 0x12, 0x34, 0x56];

  #[derive(Default)]
  struct BackendState {
    rx: VecDeque<Vec<u8>>,
    tx: Vec<Vec<u8>>,
    accept_tx: bool,
    fail_send: bool,
    fail_recv: bool,
    shutdowns: usize,
  }

  struct TestBackend(Arc<Mutex<BackendState>>);

  impl EthernetBackend for TestBackend {
    fn try_send(&mut self, frame: &[u8]) -> io::Result<bool> {
      let mut state = self.0.lock().unwrap();
      if state.fail_send {
        return Err(io::Error::new(ErrorKind::BrokenPipe, "send failed"));
      }
      if state.accept_tx {
        state.tx.push(frame.to_vec());
      }
      Ok(state.accept_tx)
    }

    fn try_recv(&mut self) -> io::Result<Option<Vec<u8>>> {
      let mut state = self.0.lock().unwrap();
      if state.fail_recv {
        return Err(io::Error::new(ErrorKind::BrokenPipe, "receive failed"));
      }
      Ok(state.rx.pop_front())
    }

    fn shutdown(&mut self) -> io::Result<()> {
      self.0.lock().unwrap().shutdowns += 1;
      Ok(())
    }
  }

  fn write_u16(memory: &mut Memory, address: u64, value: u16) {
    memory
      .write_bytes(VirtAddr(address), &value.to_le_bytes())
      .unwrap();
  }

  fn read_u32(memory: &Memory, address: u64) -> u32 {
    let mut bytes = [0; 4];
    memory.read_bytes(VirtAddr(address), &mut bytes).unwrap();
    u32::from_le_bytes(bytes)
  }

  fn write_desc_at(
    memory: &mut Memory,
    table_addr: u64,
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
      .write_bytes(VirtAddr(table_addr + u64::from(index) * 16), &bytes)
      .unwrap();
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
    write_desc_at(memory, queue.desc_addr, index, addr, len, flags, next);
  }

  fn publish(memory: &mut Memory, queue: VirtqueueAddr, index: u16, head: u16) {
    write_u16(
      memory,
      queue.avail_addr + 4 + u64::from(index % QUEUE_SIZE) * 2,
      head,
    );
    write_u16(memory, queue.avail_addr + 2, index.wrapping_add(1));
  }

  fn select_queue(net: &mut VirtioNet, queue: usize, queue_base: u64) {
    net
      .write::<u32>(VirtAddr(NET_BASE + super::super::QUEUE_SEL_OFFSET), queue as u32)
      .unwrap();
    net
      .write::<u32>(VirtAddr(NET_BASE + super::super::QUEUE_NUM_OFFSET), QUEUE_SIZE.into())
      .unwrap();
    net
      .write::<u32>(VirtAddr(NET_BASE + super::super::QUEUE_ALIGN_OFFSET), 0x1000)
      .unwrap();
    net
      .write::<u32>(
        VirtAddr(NET_BASE + super::super::QUEUE_PFN_OFFSET),
        (queue_base / 0x1000) as u32,
      )
      .unwrap();
  }

  fn configured() -> (
    VirtioNet,
    Memory,
    VirtqueueAddr,
    VirtqueueAddr,
    Arc<Mutex<BackendState>>,
  ) {
    let state = Arc::new(Mutex::new(BackendState {
      accept_tx: true,
      ..BackendState::default()
    }));
    let mut net = VirtioNet::new(NET_BASE, MAC);
    net
      .set_backend(TestBackend(Arc::clone(&state)))
      .unwrap();
    let memory = Memory::new(MEMORY_BASE, 0x20_000).unwrap();
    net
      .write::<u32>(VirtAddr(NET_BASE + super::super::GUEST_PAGE_SIZE_OFFSET), 0x1000)
      .unwrap();
    select_queue(&mut net, VIRTIO_NET_RX_QUEUE, RX_QUEUE_BASE);
    let rx_queue = VirtqueueAddr::from_transport(&net.transport, VIRTIO_NET_RX_QUEUE).unwrap();
    select_queue(&mut net, VIRTIO_NET_TX_QUEUE, TX_QUEUE_BASE);
    let tx_queue = VirtqueueAddr::from_transport(&net.transport, VIRTIO_NET_TX_QUEUE).unwrap();
    net
      .write::<u32>(VirtAddr(NET_BASE + super::super::DRIVER_FEATURES_OFFSET), VIRTIO_F_RING_INDIRECT_DESC)
      .unwrap();
    net
      .write::<u32>(VirtAddr(NET_BASE + super::super::STATUS_OFFSET), VIRTIO_STATUS_DRIVER_OK)
      .unwrap();
    (net, memory, rx_queue, tx_queue, state)
  }

  fn notify(net: &mut VirtioNet, queue: usize) {
    net
      .write::<u32>(VirtAddr(NET_BASE + super::super::QUEUE_NOTIFY_OFFSET), queue as u32)
      .unwrap();
  }

  fn ethernet_frame(fill: u8, length: usize) -> Vec<u8> {
    let mut frame = vec![fill; length];
    frame[0..6].copy_from_slice(&MAC);
    frame
  }

  #[test]
  fn discovery_features_mac_and_independent_queue_state() {
    let mut net = VirtioNet::new(NET_BASE, MAC);
    assert_eq!(net.read::<u32>(VirtAddr(NET_BASE + 8)).unwrap(), 0);
    assert_eq!(net.read::<u32>(VirtAddr(NET_BASE + 0x10)).unwrap(), VIRTIO_NET_F_MAC | VIRTIO_F_RING_INDIRECT_DESC);
    assert_eq!(net.read::<u32>(VirtAddr(NET_BASE + CONFIG_OFFSET)).unwrap(), 0x1200_5452);
    assert_eq!(net.read::<u16>(VirtAddr(NET_BASE + CONFIG_OFFSET + 4)).unwrap(), 0x5634);
    assert!(net.read::<u8>(VirtAddr(NET_BASE + CONFIG_OFFSET + 6)).is_err());

    let state = Arc::new(Mutex::new(BackendState::default()));
    net.set_backend(TestBackend(state)).unwrap();
    assert_eq!(net.read::<u32>(VirtAddr(NET_BASE + 8)).unwrap(), 1);
    net.write::<u32>(VirtAddr(NET_BASE + 0x28), 0x1000).unwrap();
    select_queue(&mut net, 0, RX_QUEUE_BASE);
    select_queue(&mut net, 1, TX_QUEUE_BASE);
    net.write::<u32>(VirtAddr(NET_BASE + 0x30), 0).unwrap();
    assert_eq!(net.read::<u32>(VirtAddr(NET_BASE + 0x40)).unwrap(), (RX_QUEUE_BASE / 0x1000) as u32);
    net.write::<u32>(VirtAddr(NET_BASE + 0x30), 1).unwrap();
    assert_eq!(net.read::<u32>(VirtAddr(NET_BASE + 0x40)).unwrap(), (TX_QUEUE_BASE / 0x1000) as u32);
    net.write::<u32>(VirtAddr(NET_BASE + 0x30), 2).unwrap();
    assert_eq!(net.read::<u32>(VirtAddr(NET_BASE + 0x34)).unwrap(), 0);
  }

  #[test]
  fn tx_scatter_gather_strips_header_and_publishes_zero_length() {
    let (mut net, mut memory, _rx, tx, state) = configured();
    let frame = ethernet_frame(0xa5, 60);
    memory.write_bytes(VirtAddr(BUFFER_BASE), &[0; 10]).unwrap();
    memory
      .write_bytes(VirtAddr(BUFFER_BASE + 0x100), &frame[..17])
      .unwrap();
    memory
      .write_bytes(VirtAddr(BUFFER_BASE + 0x200), &frame[17..])
      .unwrap();
    write_desc(&mut memory, tx, 3, BUFFER_BASE, 10, VIRTQ_DESC_F_NEXT, 4);
    write_desc(&mut memory, tx, 4, BUFFER_BASE + 0x100, 17, VIRTQ_DESC_F_NEXT, 5);
    write_desc(&mut memory, tx, 5, BUFFER_BASE + 0x200, 43, 0, 0);
    publish(&mut memory, tx, 0, 3);
    notify(&mut net, VIRTIO_NET_TX_QUEUE);

    let result = net.service_queues(&mut memory);
    assert_eq!(result.completed, 1);
    assert!(result.dma_write);
    assert!(result.interrupt_asserted);
    assert_eq!(state.lock().unwrap().tx, vec![frame]);
    assert_eq!(read_u32(&memory, tx.used_addr + 4), 3);
    assert_eq!(read_u32(&memory, tx.used_addr + 8), 0);
  }

  #[test]
  fn vlan_sized_frames_fit_the_mtu_1500_linux_buffer_boundary() {
    let (mut net, mut memory, rx, tx, state) = configured();
    let frame = ethernet_frame(0x7a, MAX_ETHERNET_FRAME_LEN);
    let mut packet = vec![0; VIRTIO_NET_HDR_LEN];
    packet.extend_from_slice(&frame);
    memory.write_bytes(VirtAddr(BUFFER_BASE), &packet).unwrap();
    write_desc(
      &mut memory,
      tx,
      0,
      BUFFER_BASE,
      packet.len() as u32,
      0,
      0,
    );
    publish(&mut memory, tx, 0, 0);
    notify(&mut net, VIRTIO_NET_TX_QUEUE);
    assert_eq!(net.service_queues(&mut memory).completed, 1);
    assert_eq!(state.lock().unwrap().tx, vec![frame.clone()]);

    state.lock().unwrap().rx.push_back(frame.clone());
    let rx_buffer = BUFFER_BASE + 0x1000;
    write_desc(
      &mut memory,
      rx,
      0,
      rx_buffer,
      packet.len() as u32,
      VIRTQ_DESC_F_WRITE,
      0,
    );
    publish(&mut memory, rx, 0, 0);
    let result = net.service_queues(&mut memory);
    assert_eq!(result.completed, 1);
    let mut received = vec![0; packet.len()];
    memory.read_bytes(VirtAddr(rx_buffer), &mut received).unwrap();
    assert_eq!(&received[..VIRTIO_NET_HDR_LEN], &[0; VIRTIO_NET_HDR_LEN]);
    assert_eq!(&received[VIRTIO_NET_HDR_LEN..], frame);
  }

  #[test]
  fn tx_indirect_and_malformed_directions_are_bounded() {
    let (mut net, mut memory, _rx, tx, state) = configured();
    let table = BUFFER_BASE;
    let packet = BUFFER_BASE + 0x200;
    let frame = ethernet_frame(0x33, 60);
    let mut bytes = vec![0; 10];
    bytes.extend_from_slice(&frame);
    memory.write_bytes(VirtAddr(packet), &bytes).unwrap();
    write_desc_at(&mut memory, table, 0, packet, bytes.len() as u32, 0, 0);
    write_desc(&mut memory, tx, 0, table, 16, VIRTQ_DESC_F_INDIRECT, 0);
    publish(&mut memory, tx, 0, 0);
    notify(&mut net, VIRTIO_NET_TX_QUEUE);
    assert_eq!(net.service_queues(&mut memory).completed, 1);
    assert_eq!(state.lock().unwrap().tx, vec![frame]);

    write_desc(&mut memory, tx, 1, packet, bytes.len() as u32, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, tx, 1, 1);
    notify(&mut net, VIRTIO_NET_TX_QUEUE);
    assert_eq!(net.service_queues(&mut memory).completed, 1);
    assert_eq!(state.lock().unwrap().tx.len(), 1);
    assert_eq!(net.stats().malformed_tx_packets, 1);
  }

  #[test]
  fn rx_waits_for_both_frame_and_buffer_then_writes_zero_header() {
    let (mut net, mut memory, rx, _tx, state) = configured();
    let frame = ethernet_frame(0x5a, 64);
    state.lock().unwrap().rx.push_back(frame.clone());
    assert_eq!(net.service_queues(&mut memory).completed, 0);
    assert_eq!(net.rx_backlog.len(), 1);

    write_desc(&mut memory, rx, 2, BUFFER_BASE, 20, VIRTQ_DESC_F_NEXT | VIRTQ_DESC_F_WRITE, 3);
    write_desc(&mut memory, rx, 3, BUFFER_BASE + 0x100, 128, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, rx, 0, 2);
    notify(&mut net, VIRTIO_NET_RX_QUEUE);
    let result = net.service_queues(&mut memory);
    assert_eq!(result.completed, 1);
    assert!(result.dma_write);
    assert!(memory.slice(VirtAddr(BUFFER_BASE), 10).unwrap().iter().all(|byte| *byte == 0));
    let first = memory.slice(VirtAddr(BUFFER_BASE + 10), 10).unwrap();
    let second = memory.slice(VirtAddr(BUFFER_BASE + 0x100), frame.len() - 10).unwrap();
    assert_eq!([first, second].concat(), frame);
    assert_eq!(read_u32(&memory, rx.used_addr + 4), 2);
    assert_eq!(read_u32(&memory, rx.used_addr + 8), 74);
  }

  #[test]
  fn rx_does_not_consume_available_buffer_without_frame() {
    let (mut net, mut memory, rx, _tx, state) = configured();
    write_desc(&mut memory, rx, 0, BUFFER_BASE, 128, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, rx, 0, 0);
    notify(&mut net, VIRTIO_NET_RX_QUEUE);
    assert_eq!(net.service_queues(&mut memory).completed, 0);
    assert_eq!(net.transport.queues[VIRTIO_NET_RX_QUEUE].last_avail_idx, 0);

    state.lock().unwrap().rx.push_back(ethernet_frame(0x7e, 60));
    assert_eq!(net.service_queues(&mut memory).completed, 1);
    assert_eq!(net.transport.queues[VIRTIO_NET_RX_QUEUE].last_avail_idx, 1);
  }

  #[test]
  fn bad_rx_buffer_is_completed_without_losing_frame() {
    let (mut net, mut memory, rx, _tx, state) = configured();
    let frame = ethernet_frame(0x44, 60);
    state.lock().unwrap().rx.push_back(frame.clone());
    write_desc(&mut memory, rx, 0, BUFFER_BASE, 8, VIRTQ_DESC_F_WRITE, 0);
    write_desc(&mut memory, rx, 1, BUFFER_BASE + 0x100, 128, VIRTQ_DESC_F_WRITE, 0);
    publish(&mut memory, rx, 0, 0);
    publish(&mut memory, rx, 1, 1);
    notify(&mut net, VIRTIO_NET_RX_QUEUE);

    assert_eq!(net.service_queues(&mut memory).completed, 2);
    assert_eq!(read_u32(&memory, rx.used_addr + 8), 0);
    assert_eq!(read_u32(&memory, rx.used_addr + 16), 70);
    assert_eq!(net.stats().malformed_rx_buffers, 1);
    assert_eq!(net.stats().rx_frames, 1);
  }

  #[test]
  fn no_interrupt_flag_reset_w1c_and_backend_errors() {
    let (mut net, mut memory, _rx, tx, state) = configured();
    let frame = ethernet_frame(0x22, 60);
    let mut bytes = vec![0; 10];
    bytes.extend_from_slice(&frame);
    memory.write_bytes(VirtAddr(BUFFER_BASE), &bytes).unwrap();
    write_desc(&mut memory, tx, 0, BUFFER_BASE, bytes.len() as u32, 0, 0);
    write_u16(&mut memory, tx.avail_addr, VIRTQ_AVAIL_F_NO_INTERRUPT);
    publish(&mut memory, tx, 0, 0);
    notify(&mut net, VIRTIO_NET_TX_QUEUE);
    assert_eq!(net.service_queues(&mut memory).completed, 1);
    assert!(!net.interrupt_asserted());

    state.lock().unwrap().fail_send = true;
    write_u16(&mut memory, tx.avail_addr, 0);
    publish(&mut memory, tx, 1, 0);
    notify(&mut net, VIRTIO_NET_TX_QUEUE);
    assert_eq!(net.service_queues(&mut memory).completed, 1);
    assert!(net.interrupt_asserted());
    assert_eq!(net.take_backend_error().unwrap().kind(), ErrorKind::BrokenPipe);
    net.write::<u8>(VirtAddr(NET_BASE + super::super::INTERRUPT_ACK_OFFSET), 1).unwrap();
    assert!(!net.interrupt_asserted());

    net.write::<u32>(VirtAddr(NET_BASE + super::super::STATUS_OFFSET), 0).unwrap();
    assert_eq!(net.transport.queues[0].pfn, 0);
    assert_eq!(net.transport.queues[1].pfn, 0);
    assert!(net.backend_attached());
    assert_eq!(net.read::<u32>(VirtAddr(NET_BASE + 8)).unwrap(), 1);
  }
}
