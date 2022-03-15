use std::fs::File;
use std::io::Read;
use std::sync::mpsc::{channel, Receiver, Sender, TryRecvError};
use std::thread::JoinHandle;
use clap::Parser;
use valheim_core::cpu::RV64Cpu;
use valheim_core::device::Device;
use valheim_core::interp::naive::NaiveInterpreter;
use valheim_core::memory::{Memory, VirtAddr};

mod repl;

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(short, long)]
  pub kernel: String,
  #[clap(long)]
  pub trace: Option<String>,
}

pub struct XLBDevice {
  pub mem: Memory,
  pub worker: Option<JoinHandle<()>>,
  pub sender: Option<Sender<()>>,
}

impl XLBDevice {
  pub fn new() -> Self {
    XLBDevice {
      mem: Memory::new(0xb8000, 1024 * 1024).expect("Failed to create memory"),
      worker: None,
      sender: None,
    }
  }
}

impl Device for XLBDevice {
  fn name(&self) -> &'static str {
    "XLBDevice"
  }

  fn vendor_id(&self) -> u16 {
    0xCAFE
  }

  fn device_id(&self) -> u16 {
    0x0001
  }

  fn init(&'static mut self) -> Result<Vec<(VirtAddr, VirtAddr)>, ()> {
    let (tx, rx) = channel();
    let mem = &self.mem;
    self.sender = Some(tx);
    self.worker = Some(std::thread::spawn(move || {
      let mut old = mem.read(VirtAddr(0xb8000)).unwrap();
      loop {
        match rx.try_recv() {
          Ok(_) | Err(TryRecvError::Disconnected) => break,
          Err(TryRecvError::Empty) => {}
        }
        match mem.read::<i32>(VirtAddr(0xb8000)) {
          Some(result) if result != old => {
            println!("XLBDevice: data received: {}", result);
            old = result;
          }
          _ => continue,
        }
      }
    }));
    Ok(vec![(VirtAddr(0xb8000), VirtAddr(0xb8000 + 1024 * 1024))])
  }

  fn destroy(&mut self) -> Result<(), ()> {
    match &self.sender {
      Some(tx) => tx.send(()).map_err(|_| ()),
      _ => Ok(()),
    }
  }

  fn dma_read(&self, addr: VirtAddr) -> Option<&Memory> {
    if addr.0 >= 0xb8000 && addr.0 <= 0xb8000 + 1024 * 1024 {
      return Some(&self.mem);
    }
    None
  }

  fn dma_write(&mut self, addr: VirtAddr) -> Option<&mut Memory> {
    if addr.0 >= 0xb8000 && addr.0 <= 0xb8000 + 1024 * 1024 {
      return Some(&mut self.mem);
    }
    None
  }

  fn mmio_read(&self, addr: VirtAddr) -> Option<u8> {
    unreachable!()
  }

  fn mmio_write(&mut self, addr: VirtAddr, val: u8) -> Result<(), ()> {
    unreachable!()
  }
}

fn main() {
  let args = Args::parse();
  let mut file = File::open(&args.kernel).expect("Failed to open kernel");
  let mut bytes = vec![];
  file.read_to_end(&mut bytes).expect("Failed to read kernel");

  let mut cpu = RV64Cpu::new(args.trace);
  cpu.load_kernel(bytes.as_slice());
  let interp = NaiveInterpreter::new();
  cpu.run(&interp);
}
