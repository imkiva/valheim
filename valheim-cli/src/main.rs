use std::fs::File;
use std::io::Read;

use clap::Parser;

use valheim_core::machine::Machine;

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
  #[clap(long)]
  pub test: bool,
}

fn main() -> Result<(), std::io::Error> {
  let args = Args::parse();
  let mut machine = Machine::new(args.cmdline, args.trace);

  let kernel = read_image(&args.kernel)?;
  let bios = args.bios.and_then(|bios| read_image(&bios).ok());

  match bios {
    Some(bios) => {
      machine.load_memory(0x80000000, bios.as_slice());
      machine.load_memory(0x80200000, kernel.as_slice());
    }
    None => {
      machine.load_memory(0x80000000, kernel.as_slice());
    }
  }

  if let Some(disk_file) = args.disk {
    machine.load_disk_file(disk_file)?;
  }

  match args.test {
    true => std::process::exit(machine.run_for_test()),
    false => machine.run(),
  }
  Ok(())
}

fn read_image(image: &str) -> Result<Vec<u8>, std::io::Error> {
  let mut file = match File::open(&image) {
    Ok(file) => file,
    Err(err) => {
      eprintln!("Error opening image file: {}", err);
      return Err(err);
    }
  };
  let mut bytes = vec![];
  file.read_to_end(&mut bytes).expect("Failed to read image file");
  Ok(bytes)
}
