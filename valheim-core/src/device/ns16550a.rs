use std::io;
use std::io::prelude::*;
use std::sync::{
  Arc,
  atomic::{AtomicBool, Ordering}, Condvar, Mutex,
};

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

pub struct Uart16550a {
  buffer: Arc<(Mutex<[u8; UART_SIZE as usize]>, Condvar)>,
  interrupting: Arc<AtomicBool>,
}

impl Uart16550a {
  pub fn new() -> Self {
    let uart = Arc::new((Mutex::new([0; UART_SIZE as usize]), Condvar::new()));
    let interrupting = Arc::new(AtomicBool::new(false));
    {
      let (uart, _) = uart.as_ref();
      let mut uart = uart.lock().expect("cannot lock uart buffer");
      // Transmitter hold register is empty. It allows input anytime.
      uart[(UART_LSR - UART_BASE) as usize] |= UART_LSR_TX;
    }

    {
      let uart = uart.clone();
      let interrupting = interrupting.clone();
      std::thread::spawn(move || loop {
        let mut buffer = [0; 1];
        match io::stdin().read(&mut buffer) {
          Ok(_) => {
            let (uart, cond) = uart.as_ref();
            let mut uart = uart.lock().expect("cannot lock uart buffer");
            // we can only write to the register if there's no previous data.
            // we achieve this by checking the bit 0 of LSR:
            // - 0: no data in receive holding register.
            // - 1: means data has been receive and saved in the receive holding register.
            while (uart[(UART_LSR - UART_BASE) as usize] & UART_LSR_RX) == 1 {
              uart = cond.wait(uart).expect("cannot wait on uart buffer");
            }
            uart[0] = buffer[0];
            interrupting.store(true, Ordering::Release);
            uart[(UART_LSR - UART_BASE) as usize] |= UART_LSR_RX;
          }
          Err(e) => {
            println!("[Valheim] uart input error: {}", e);
          }
        }
      });
    }

    Self { buffer: uart, interrupting }
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
    let (uart, cond) = &*self.buffer;
    let mut uart = uart.lock().expect("cannot lock uart buffer");
    match addr.0 {
      UART_RHR => {
        let val = uart[(UART_RHR - UART_BASE) as usize];
        uart[(UART_LSR - UART_BASE) as usize] &= !UART_LSR_RX;
        cond.notify_one();
        Some(val)
      }
      addr => Some(uart[(addr - UART_BASE) as usize]),
    }
  }

  fn mmio_write(&self, addr: VirtAddr, val: u8) -> Result<(), ()> {
    let (uart, _) = &*self.buffer;
    let mut uart = uart.lock().expect("cannot lock uart buffer");
    match addr.0 {
      UART_THR => {
        print!("{}", val as char);
        io::stdout().flush().expect("cannot flush stdout");
      }
      addr => {
        uart[(addr - UART_BASE) as usize] = val;
      }
    }
    Ok(())
  }

  fn is_interrupting(&self) -> Option<u64> {
    let irq = self.interrupting.swap(false, Ordering::Acquire);
    match irq {
      true => Some(UART_IRQ),
      false => None,
    }
  }
}
