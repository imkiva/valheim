mod repl;

use std::fs::File;
use std::io::Read;
use clap::Parser;
use valheim_core::machine::Machine;

const VALHEIM_MEMORY_SIZE: usize = 4 * 1024 * 1024 * 1024; // 4GiB

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(short, long)]
  pub kernel: String,
  #[clap(long)]
  pub trace: Option<String>,
}

fn main() {
  let args = Args::parse();
  let mut file = File::open(&args.kernel).expect("Failed to open kernel");
  let mut bytes = vec![];
  file.read_to_end(&mut bytes).expect("Failed to read kernel");

  let mut machine = Machine::new(VALHEIM_MEMORY_SIZE);
  machine.load_kernel(bytes.as_slice());
  machine.run();
}
