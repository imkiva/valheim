use std::fs::File;
use std::io::Read;
use clap::Parser;
use valheim_core::cpu::RV64Cpu;
use valheim_core::interp::naive::NaiveInterpreter;

mod repl;

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

  let mut cpu = RV64Cpu::new(args.trace);
  cpu.reset(bytes.as_slice());
  let interp = NaiveInterpreter::new();
  cpu.run(&interp);
}
