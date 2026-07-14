use std::fs::File;
use std::io::Read;

use clap::Parser;

use valheim_core::interp::naive::NaiveInterpreter;
use valheim_core::interp::RV64Executor;
use valheim_core::machine::Machine;
use valheim_core::TRACE_ENABLED;
use valheim_jit::JitExecutor;

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
  #[clap(long)]
  pub test_name: Option<String>,
  #[clap(long, default_value = "naive")]
  pub engine: String,
  #[clap(long, default_value_t = 500)]
  pub jit_hot_threshold: u32,
  #[clap(long, default_value_t = 32)]
  pub jit_max_block_len: usize,
  #[clap(long, default_value_t = 4096)]
  pub jit_max_compiled_blocks: usize,
  #[clap(long, default_value_t = 134_217_728)]
  pub jit_max_code_bytes: u64,
  #[clap(long)]
  pub jit_stats: bool,
  #[clap(long, default_value_t = 1_000_000)]
  pub jit_stats_interval: u64,
}

fn main() -> Result<(), std::io::Error> {
  let args = Args::parse();
  let executor: Box<dyn RV64Executor> = match args.engine.as_str() {
    "naive" => Box::new(NaiveInterpreter::new()),
    "jit" if args.trace.is_some() || TRACE_ENABLED => {
      eprintln!("JIT does not yet support instruction tracing; using the naive executor");
      Box::new(NaiveInterpreter::new())
    }
    "jit" => Box::new(
      JitExecutor::new()
        .map_err(|error| std::io::Error::new(std::io::ErrorKind::Other, error))?
        .with_hot_threshold(args.jit_hot_threshold)
        .with_max_block_len(args.jit_max_block_len)
        .with_max_compiled_blocks(args.jit_max_compiled_blocks)
        .with_max_live_code_bytes(args.jit_max_code_bytes)
        .with_stats_enabled(args.jit_stats)
        .with_stats_interval(args.jit_stats.then_some(args.jit_stats_interval)),
    ),
    engine => {
      return Err(std::io::Error::new(
        std::io::ErrorKind::InvalidInput,
        format!("unknown execution engine {engine:?}; expected naive or jit"),
      ));
    }
  };
  let mut machine = Machine::new_with_executor(args.cmdline, args.trace, executor);

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

  if args.test {
    let exit_code = machine.run_for_test(args.test_name.unwrap_or("<unknown-test>".to_string()));
    if args.jit_stats {
      report_executor_diagnostics(&machine);
    }
    std::process::exit(exit_code);
  } else {
    machine.run();
    if args.jit_stats {
      report_executor_diagnostics(&machine);
    }
  }
  Ok(())
}

fn report_executor_diagnostics(machine: &Machine) {
  if let Some(diagnostics) = machine.executor.diagnostics() {
    eprintln!("[valheim-jit] {diagnostics}");
  }
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
