use std::fs::File;
use std::io::Read;

use clap::Parser;

use valheim_core::machine::Machine;

mod repl;

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(short, long)]
  pub kernel: String,
  #[clap(short, long)]
  pub bios: Option<String>,
  #[clap(short, long)]
  pub cmdline: Option<String>,
  #[clap(short, long)]
  pub disk: Option<String>,
  #[clap(long)]
  pub trace: Option<String>,
}

fn main() {
  let args = Args::parse();
  let mut kernel = File::open(&args.kernel).expect("Failed to open kernel");
  let mut kernel_bytes = vec![];
  kernel.read_to_end(&mut kernel_bytes).expect("Failed to read kernel");

  let bios_bytes = args.bios.map(|bios| {
    let mut file = File::open(&bios).expect("Failed to open bios");
    let mut bytes = vec![];
    file.read_to_end(&mut bytes).expect("Failed to read kernel");
    bytes
  });

  let disk_bytes = args.disk.map(|bios| {
    let mut file = File::open(&bios).expect("Failed to open disk");
    let mut bytes = vec![];
    file.read_to_end(&mut bytes).expect("Failed to read disk");
    bytes
  });

  let mut machine = Machine::new(args.cmdline, args.trace);
  match bios_bytes {
    Some(bios_bytes) => {
      machine.load_memory(0x80000000, bios_bytes.as_slice());
      machine.load_memory(0x80200000, kernel_bytes.as_slice());
    }
    None => {
      machine.load_memory(0x80000000, kernel_bytes.as_slice());
    }
  }
  match disk_bytes {
    Some(disk_bytes) => {
      machine.load_disk(disk_bytes);
    }
    None => {}
  }

  machine.run();
}
