use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use clap::Parser;

use valheim_core::interp::naive::NaiveInterpreter;
use valheim_core::interp::RV64Executor;
use valheim_core::machine::Machine;
use valheim_core::TRACE_ENABLED;
use valheim_jit::JitExecutor;
use valheim_net::{NetworkConfig, PasstBackendFactory, DEFAULT_SUBNET_CIDR};

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
  /// Guest networking mode. Networking is absent unless NAT is explicitly enabled.
  #[clap(long, default_value = "none", possible_values = &["none", "nat"])]
  pub net: String,
  /// Canonical RFC 1918 IPv4 subnet used by NAT (default: 10.172.0.0/16).
  #[clap(long, value_name = "CIDR")]
  pub net_subnet: Option<String>,
  /// passt executable used by NAT (default: resolve `passt` through PATH).
  #[clap(long, value_name = "PATH")]
  pub passt: Option<PathBuf>,
  #[clap(long)]
  pub trace: Option<String>,
  #[clap(long)]
  pub test: bool,
  #[clap(long)]
  pub test_name: Option<String>,
  #[clap(long, default_value = "naive")]
  pub engine: String,
  #[clap(long, default_value_t = 750)]
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
  let network = network_configuration(&args)?;
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

  if let Some((config, passt)) = network {
    machine.enable_network(Box::new(PasstBackendFactory::new(passt, config)))?;
    eprintln!(
      "Enabling IPv4 NAT: guest {}/{}, gateway {}, DNS {}",
      config.guest(),
      config.subnet().prefix_len(),
      config.gateway(),
      config.dns(),
    );
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

fn network_configuration(args: &Args) -> Result<Option<(NetworkConfig, PathBuf)>, std::io::Error> {
  match args.net.as_str() {
    "none" => {
      if args.net_subnet.is_some() || args.passt.is_some() {
        return Err(std::io::Error::new(
          std::io::ErrorKind::InvalidInput,
          "--net-subnet and --passt require --net nat",
        ));
      }
      Ok(None)
    }
    "nat" => {
      let subnet = args
        .net_subnet
        .as_deref()
        .unwrap_or(DEFAULT_SUBNET_CIDR)
        .parse()
        .map_err(|error| {
          std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("invalid --net-subnet: {error}"),
          )
        })?;
      let passt = args
        .passt
        .clone()
        .unwrap_or_else(|| PathBuf::from("passt"));
      if passt.as_os_str().is_empty() {
        return Err(std::io::Error::new(
          std::io::ErrorKind::InvalidInput,
          "--passt must not be empty",
        ));
      }
      Ok(Some((NetworkConfig::new(subnet), passt)))
    }
    _ => unreachable!("clap validates --net"),
  }
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

#[cfg(test)]
mod tests {
  use super::*;

  fn args(extra: &[&str]) -> Args {
    let mut arguments = vec!["valheim-cli", "--kernel", "kernel.bin"];
    arguments.extend_from_slice(extra);
    Args::try_parse_from(arguments).unwrap()
  }

  #[test]
  fn networking_is_off_by_default() {
    assert!(network_configuration(&args(&[])).unwrap().is_none());
  }

  #[test]
  fn nat_uses_the_default_subnet_and_passt_from_path() {
    let (config, passt) = network_configuration(&args(&["--net", "nat"]))
      .unwrap()
      .unwrap();
    assert_eq!(config.subnet().to_string(), DEFAULT_SUBNET_CIDR);
    assert_eq!(config.guest().to_string(), "10.172.0.15");
    assert_eq!(passt, PathBuf::from("passt"));
  }

  #[test]
  fn nat_accepts_a_custom_canonical_subnet() {
    let (config, passt) = network_configuration(&args(&[
      "--net=nat",
      "--net-subnet",
      "192.168.64.0/24",
      "--passt",
      "/opt/passt",
    ]))
    .unwrap()
    .unwrap();
    assert_eq!(config.guest().to_string(), "192.168.64.15");
    assert_eq!(config.gateway().to_string(), "192.168.64.2");
    assert_eq!(passt, PathBuf::from("/opt/passt"));
  }

  #[test]
  fn network_specific_options_are_rejected_while_offline() {
    let error = network_configuration(&args(&["--net-subnet", "10.1.0.0/16"]))
      .unwrap_err();
    assert_eq!(error.kind(), std::io::ErrorKind::InvalidInput);
  }

  #[test]
  fn malformed_or_noncanonical_subnets_are_rejected() {
    for subnet in ["10.0.0.1/24", "10.0.0.0/28", "not-a-subnet"] {
      let error = network_configuration(&args(&["--net", "nat", "--net-subnet", subnet]))
        .unwrap_err();
      assert_eq!(error.kind(), std::io::ErrorKind::InvalidInput);
    }
  }
}
