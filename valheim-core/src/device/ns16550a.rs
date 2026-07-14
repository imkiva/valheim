use std::io;
use std::io::prelude::*;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::device::Device;
use crate::memory::{Memory, VirtAddr};

pub const UART_BASE: u64 = 0x1000_0000;
pub const UART_SIZE: u64 = 0x100;
const UART_END: u64 = UART_BASE + 0x100;

/// The interrupt request of UART.
pub const UART_IRQ: u64 = 10;

/// Receive holding register (for input bytes).
pub const UART_RHR: u64 = UART_BASE + 0;

/// Transmit holding register (for output bytes).
pub const UART_THR: u64 = UART_BASE + 0;

/// Interrupt enable register.
pub const UART_IER: u64 = UART_BASE + 1;

/// FIFO control register.
pub const UART_FCR: u64 = UART_BASE + 2;

/// Interrupt status register.
/// ISR[0] = 0: an interrupt is pending and the ISR contents may be used as a pointer to the appropriate interrupt service routine.
/// ISR[0] = 1: no interrupt is pending.
pub const UART_ISR: u64 = UART_BASE + 2;

/// Line control register.
pub const UART_LCR: u64 = UART_BASE + 3;
/// Divisor latch access bit in the line control register.
const UART_LCR_DLAB: u8 = 1 << 7;

/// Line status register.
/// LSR[0] = 0: no data in receive holding register or FIFO.
/// LSR[0] = 1: data has been receive and saved in the receive holding register or FIFO.
/// LSR[5] = 0: transmit holding register is full. 16550 will not accept any data for transmission.
/// LSR[5] = 1: transmitter hold register (or FIFO) is empty. CPU can load the next character.
pub const UART_LSR: u64 = UART_BASE + 5;
/// LSR[0] mask
pub const UART_LSR_RX: u8 = 1;
/// LSR[5] mask
pub const UART_LSR_TX: u8 = 1 << 5;
/// LSR[6] mask
const UART_LSR_TEMT: u8 = 1 << 6;

/// Enable received-data-available interrupts.
const UART_IER_RDI: u8 = 1;
/// Enable transmitter-holding-register-empty interrupts.
const UART_IER_THRI: u8 = 1 << 1;

/// FIFO enable bit in the FIFO control register.
const UART_FCR_ENABLE_FIFO: u8 = 1;
/// Clear receive FIFO bit in the FIFO control register.
const UART_FCR_CLEAR_RCVR: u8 = 1 << 1;

struct UartState {
  registers: [u8; UART_SIZE as usize],
  divisor_latch_low: u8,
  divisor_latch_high: u8,
  receive_interrupt_asserted: bool,
  // THRE is a latched interrupt cause. Keep the cause visible to an IIR
  // read, but pulse Valheim's edge-like PLIC input only once per event.
  // This avoids repeatedly injecting the same TX interrupt into guests such
  // as xv6, which does not read IIR in its UART interrupt handler.
  thre_interrupt_pending: bool,
  thre_interrupt_asserted: bool,
}

impl UartState {
  fn new() -> Self {
    let mut registers = [0; UART_SIZE as usize];
    // Both the transmitter holding register and transmitter are empty.
    registers[(UART_LSR - UART_BASE) as usize] = UART_LSR_TX | UART_LSR_TEMT;
    // Report carrier, data-set-ready, and clear-to-send to the serial core.
    registers[6] = 0xb0;
    Self {
      registers,
      divisor_latch_low: 0,
      divisor_latch_high: 0,
      receive_interrupt_asserted: false,
      thre_interrupt_pending: false,
      thre_interrupt_asserted: false,
    }
  }

  fn dlab(&self) -> bool {
    self.registers[(UART_LCR - UART_BASE) as usize] & UART_LCR_DLAB != 0
  }

  fn interrupt_identification(&self) -> u8 {
    let ier = self.registers[(UART_IER - UART_BASE) as usize];
    let lsr = self.registers[(UART_LSR - UART_BASE) as usize];
    let fcr = self.registers[(UART_FCR - UART_BASE) as usize];
    let fifo = if fcr & UART_FCR_ENABLE_FIFO != 0 { 0xc0 } else { 0 };

    if ier & UART_IER_RDI != 0 && lsr & UART_LSR_RX != 0 {
      fifo | 0x04
    } else if ier & UART_IER_THRI != 0 && self.thre_interrupt_pending {
      fifo | 0x02
    } else {
      fifo | 0x01
    }
  }

  fn acknowledge_interrupt(&mut self) -> u8 {
    let identification = self.interrupt_identification();
    if identification & 0x0f == 0x02 {
      self.clear_thre_interrupt();
    }
    identification
  }

  fn set_interrupt_enable(&mut self, value: u8) {
    let ier_index = (UART_IER - UART_BASE) as usize;
    let old_ier = self.registers[ier_index];
    let new_ier = value & 0x0f;
    self.registers[ier_index] = new_ier;

    if new_ier & UART_IER_RDI == 0 {
      self.receive_interrupt_asserted = false;
    } else if old_ier & UART_IER_RDI == 0 &&
      self.registers[(UART_LSR - UART_BASE) as usize] & UART_LSR_RX != 0 {
      self.receive_interrupt_asserted = true;
    }

    if new_ier & UART_IER_THRI == 0 {
      self.clear_thre_interrupt();
    } else if old_ier & UART_IER_THRI == 0 {
      self.raise_thre_interrupt();
    }
  }

  fn raise_thre_interrupt(&mut self) {
    self.thre_interrupt_pending = true;
    self.thre_interrupt_asserted = true;
  }

  fn clear_thre_interrupt(&mut self) {
    self.thre_interrupt_pending = false;
    self.thre_interrupt_asserted = false;
  }

  fn raise_receive_interrupt(&mut self) {
    if self.registers[(UART_IER - UART_BASE) as usize] & UART_IER_RDI != 0 {
      self.receive_interrupt_asserted = true;
    }
  }

  fn clear_receive_interrupt(&mut self) {
    self.receive_interrupt_asserted = false;
  }

  fn take_interrupt(&mut self) -> bool {
    let ier = self.registers[(UART_IER - UART_BASE) as usize];
    let lsr = self.registers[(UART_LSR - UART_BASE) as usize];
    if ier & UART_IER_RDI != 0 && lsr & UART_LSR_RX != 0 &&
      self.receive_interrupt_asserted {
      self.receive_interrupt_asserted = false;
      true
    } else if ier & UART_IER_THRI != 0 && self.thre_interrupt_asserted {
      self.thre_interrupt_asserted = false;
      true
    } else {
      false
    }
  }

  fn has_interrupt_to_deliver(&self) -> bool {
    let ier = self.registers[(UART_IER - UART_BASE) as usize];
    let lsr = self.registers[(UART_LSR - UART_BASE) as usize];
    (ier & UART_IER_RDI != 0 && lsr & UART_LSR_RX != 0 && self.receive_interrupt_asserted) ||
      (ier & UART_IER_THRI != 0 && self.thre_interrupt_asserted)
  }
}

struct SharedUartState {
  state: Mutex<UartState>,
  input_consumed: Condvar,
  // This is only a fast-path hint. UartState remains the source of truth and
  // all changes to an interrupt pulse are serialized by `state`.
  interrupt_to_deliver: AtomicBool,
}

impl SharedUartState {
  fn new() -> Self {
    Self {
      state: Mutex::new(UartState::new()),
      input_consumed: Condvar::new(),
      interrupt_to_deliver: AtomicBool::new(false),
    }
  }

  fn publish_interrupt_state(&self, state: &UartState) {
    // Producers publish while holding `state`. The interrupt consumer also
    // updates the hint before releasing that mutex, so a producer that races
    // with consumption necessarily publishes its newer value afterwards.
    self
      .interrupt_to_deliver
      .store(state.has_interrupt_to_deliver(), Ordering::Release);
  }
}

pub struct Uart16550a {
  state: Arc<SharedUartState>,
}

impl Uart16550a {
  pub fn new() -> Self {
    let state = Arc::new(SharedUartState::new());

    {
      let state = state.clone();
      std::thread::spawn(move || loop {
        let mut buffer = [0; 1];
        match io::stdin().read(&mut buffer) {
          Ok(0) => return,
          Ok(_) => {
            let mut uart = state.state.lock().expect("cannot lock uart state");
            // we can only write to the register if there's no previous data.
            // we achieve this by checking the bit 0 of LSR:
            // - 0: no data in receive holding register.
            // - 1: means data has been receive and saved in the receive holding register.
            while uart.registers[(UART_LSR - UART_BASE) as usize] & UART_LSR_RX != 0 {
              uart = state
                .input_consumed
                .wait(uart)
                .expect("cannot wait on uart state");
            }
            uart.registers[(UART_RHR - UART_BASE) as usize] = buffer[0];
            uart.registers[(UART_LSR - UART_BASE) as usize] |= UART_LSR_RX;
            uart.raise_receive_interrupt();
            state.publish_interrupt_state(&uart);
          }
          Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
          Err(e) => {
            eprintln!("[Valheim] uart input error: {}", e);
            return;
          }
        }
      });
    }

    Self { state }
  }
}

impl Device for Uart16550a {
  fn name(&self) -> &'static str {
    // TODO: name
    "UART16550A"
  }

  fn vendor_id(&self) -> u16 {
    // TODO: vendor id
    0x0000
  }

  fn device_id(&self) -> u16 {
    // TODO: device id
    0x0000
  }

  fn init(&self) -> Result<Vec<(VirtAddr, VirtAddr)>, ()> {
    Ok(vec![(VirtAddr(UART_BASE), VirtAddr(UART_END))])
  }

  fn destroy(&self) -> Result<(), ()> {
    Ok(())
  }

  fn dma_read(&self, _addr: VirtAddr) -> Option<&Memory> {
    None
  }

  fn dma_write(&self, _addr: VirtAddr) -> Option<&mut Memory> {
    None
  }

  fn mmio_read(&self, addr: VirtAddr) -> Option<u8> {
    let mut uart = self.state.state.lock().expect("cannot lock uart state");
    let value = match addr.0 {
      UART_RHR => {
        if uart.dlab() {
          uart.divisor_latch_low
        } else {
          let val = uart.registers[(UART_RHR - UART_BASE) as usize];
          uart.registers[(UART_LSR - UART_BASE) as usize] &= !UART_LSR_RX;
          uart.clear_receive_interrupt();
          self.state.input_consumed.notify_one();
          val
        }
      }
      UART_IER => {
        if uart.dlab() {
          uart.divisor_latch_high
        } else {
          uart.registers[(UART_IER - UART_BASE) as usize]
        }
      }
      UART_ISR => uart.acknowledge_interrupt(),
      addr => uart.registers[(addr - UART_BASE) as usize],
    };
    self.state.publish_interrupt_state(&uart);
    Some(value)
  }

  fn mmio_write(&self, addr: VirtAddr, val: u8) -> Result<(), ()> {
    let mut uart = self.state.state.lock().expect("cannot lock uart state");
    match addr.0 {
      UART_THR => {
        if uart.dlab() {
          uart.divisor_latch_low = val;
        } else {
          uart.clear_thre_interrupt();
          io::stdout().write_all(&[val]).expect("cannot write to stdout");
          io::stdout().flush().expect("cannot flush stdout");
          // Valheim writes the byte synchronously, so THR is empty again as
          // soon as this MMIO operation completes.
          if uart.registers[(UART_IER - UART_BASE) as usize] & UART_IER_THRI != 0 {
            uart.raise_thre_interrupt();
          }
        }
      }
      UART_IER => {
        if uart.dlab() {
          uart.divisor_latch_high = val;
        } else {
          uart.set_interrupt_enable(val);
        }
      }
      UART_FCR => {
        uart.registers[(UART_FCR - UART_BASE) as usize] = val;
        if val & UART_FCR_CLEAR_RCVR != 0 {
          uart.registers[(UART_LSR - UART_BASE) as usize] &= !UART_LSR_RX;
          uart.clear_receive_interrupt();
          self.state.input_consumed.notify_one();
        }
      }
      addr => {
        uart.registers[(addr - UART_BASE) as usize] = val;
      }
    }
    self.state.publish_interrupt_state(&uart);
    Ok(())
  }

  fn is_interrupting(&self) -> Option<u64> {
    if !self.state.interrupt_to_deliver.load(Ordering::Acquire) {
      return None;
    }

    let mut uart = self.state.state.lock().expect("cannot lock uart state");
    let interrupting = uart.take_interrupt();
    self.state.publish_interrupt_state(&uart);
    interrupting.then_some(UART_IRQ)
  }
}

#[cfg(test)]
mod tests {
  use std::sync::{mpsc, Barrier};
  use std::time::Duration;

  use super::*;

  fn uart_without_input_thread() -> Uart16550a {
    Uart16550a { state: Arc::new(SharedUartState::new()) }
  }

  #[test]
  fn interrupt_poll_without_an_event_does_not_lock_uart_state() {
    let uart = uart_without_input_thread();
    let shared = uart.state.clone();
    let guard = shared.state.lock().expect("cannot lock uart state");
    let (sender, receiver) = mpsc::channel();

    let poller = std::thread::spawn(move || sender.send(uart.is_interrupting()).unwrap());

    // Keep the state mutex held while the other thread polls. The no-event
    // path must finish from the atomic hint instead of waiting for this lock.
    assert_eq!(receiver.recv_timeout(Duration::from_secs(2)), Ok(None));
    drop(guard);
    poller.join().unwrap();
  }

  #[test]
  fn consuming_one_cause_republishes_another_pending_pulse() {
    let uart = uart_without_input_thread();
    {
      let mut state = uart.state.state.lock().expect("cannot lock uart state");
      state.set_interrupt_enable(UART_IER_RDI | UART_IER_THRI);
      state.registers[(UART_LSR - UART_BASE) as usize] |= UART_LSR_RX;
      state.raise_receive_interrupt();
      uart.state.publish_interrupt_state(&state);
    }

    // Receive has priority, and consuming it must leave the THRE event in the
    // atomic fast-path hint for the next PLIC poll.
    assert_eq!(uart.is_interrupting(), Some(UART_IRQ));
    assert!(uart.state.interrupt_to_deliver.load(Ordering::Acquire));
    assert_eq!(uart.is_interrupting(), Some(UART_IRQ));
    assert!(!uart.state.interrupt_to_deliver.load(Ordering::Acquire));
    assert_eq!(uart.is_interrupting(), None);
  }

  #[test]
  fn racing_producer_cannot_lose_its_atomic_interrupt_hint() {
    let uart = Arc::new(uart_without_input_thread());
    uart.mmio_write(VirtAddr(UART_IER), UART_IER_THRI).unwrap();

    let barrier = Arc::new(Barrier::new(2));
    let producer_uart = uart.clone();
    let producer_barrier = barrier.clone();
    let producer = std::thread::spawn(move || {
      producer_barrier.wait();
      let mut state = producer_uart
        .state
        .state
        .lock()
        .expect("cannot lock uart state");
      state.registers[(UART_LSR - UART_BASE) as usize] |= UART_LSR_RX;
      state.set_interrupt_enable(UART_IER_RDI | UART_IER_THRI);
      producer_uart.state.publish_interrupt_state(&state);
    });

    barrier.wait();
    assert_eq!(uart.is_interrupting(), Some(UART_IRQ));
    producer.join().unwrap();

    // If the consumer won the mutex, this is the newly published RX pulse. If
    // the producer won, RX was consumed first and this is the older THRE
    // pulse. In either ordering, publishing while holding the mutex ensures
    // that the remaining event cannot be overwritten with a stale `false`.
    assert!(uart.state.interrupt_to_deliver.load(Ordering::Acquire));
    assert_eq!(uart.is_interrupting(), Some(UART_IRQ));
  }

  #[test]
  fn thre_enable_edge_rearms_atomic_interrupt_hint() {
    let uart = uart_without_input_thread();

    uart.mmio_write(VirtAddr(UART_IER), UART_IER_THRI).unwrap();
    assert_eq!(uart.is_interrupting(), Some(UART_IRQ));
    assert_eq!(uart.is_interrupting(), None);

    // Rewriting an already-enabled IER is not a new THRE event.
    uart.mmio_write(VirtAddr(UART_IER), UART_IER_THRI).unwrap();
    assert_eq!(uart.is_interrupting(), None);

    uart.mmio_write(VirtAddr(UART_IER), 0).unwrap();
    uart.mmio_write(VirtAddr(UART_IER), UART_IER_THRI).unwrap();
    assert_eq!(uart.is_interrupting(), Some(UART_IRQ));
    assert_eq!(uart.is_interrupting(), None);
  }

  #[test]
  fn iir_acknowledgement_clears_atomic_thre_hint() {
    let uart = uart_without_input_thread();

    uart.mmio_write(VirtAddr(UART_IER), UART_IER_THRI).unwrap();
    assert!(uart.state.interrupt_to_deliver.load(Ordering::Acquire));
    assert_eq!(uart.mmio_read(VirtAddr(UART_ISR)).unwrap() & 0x0f, 0x02);
    assert!(!uart.state.interrupt_to_deliver.load(Ordering::Acquire));
    assert_eq!(uart.is_interrupting(), None);
  }

  #[test]
  fn thre_interrupt_is_a_single_pulse_until_rearmed() {
    let mut state = UartState::new();
    state.set_interrupt_enable(UART_IER_THRI);

    assert_eq!(state.interrupt_identification() & 0x0f, 0x02);
    assert!(state.take_interrupt());
    assert!(!state.take_interrupt());

    // The cause remains available for the driver's IIR read even after the
    // PLIC pulse has been delivered.
    assert_eq!(state.acknowledge_interrupt() & 0x0f, 0x02);
    assert_eq!(state.interrupt_identification() & 0x0f, 0x01);

    state.raise_thre_interrupt();
    assert!(state.take_interrupt());
  }

  #[test]
  fn receive_interrupt_is_a_single_pulse_until_the_next_byte() {
    let mut state = UartState::new();
    state.set_interrupt_enable(UART_IER_RDI);
    state.registers[(UART_LSR - UART_BASE) as usize] |= UART_LSR_RX;
    state.raise_receive_interrupt();

    assert_eq!(state.interrupt_identification() & 0x0f, 0x04);
    assert!(state.take_interrupt());
    assert!(!state.take_interrupt());

    state.registers[(UART_LSR - UART_BASE) as usize] &= !UART_LSR_RX;
    assert!(!state.take_interrupt());

    state.registers[(UART_LSR - UART_BASE) as usize] |= UART_LSR_RX;
    state.raise_receive_interrupt();
    assert!(state.take_interrupt());
  }
}
