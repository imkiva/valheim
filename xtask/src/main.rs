#![allow(dead_code)]

use std::path::{Path, PathBuf};
use std::process::exit;

use xshell::{cmd, Error, Shell};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutionEngine {
  Naive,
  Jit,
}

impl ExecutionEngine {
  fn parse(value: &str) -> Result<Self, String> {
    match value {
      "naive" => Ok(Self::Naive),
      "jit" => Ok(Self::Jit),
      _ => Err(format!("invalid engine {value:?}; expected naive or jit")),
    }
  }

  fn as_str(self) -> &'static str {
    match self {
      Self::Naive => "naive",
      Self::Jit => "jit",
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TestOptions {
  engine: ExecutionEngine,
}

fn main() -> Result<(), Error> {
  let argv = std::env::args().skip(1).collect::<Vec<_>>();
  match argv.first().map(|s| s.as_str()) {
    Some("test") => {
      let options = match parse_test_options(&argv[1..]) {
        Ok(options) => options,
        Err(error) => {
          eprintln!("error: {error}");
          eprintln!("Usage: xtask test [--engine naive|jit]");
          exit(2);
        }
      };
      run_riscv_tests(options)
    }
    _ => {
      eprintln!("Usage: xtask <command>");
      eprintln!("Available <command>s:");
      eprintln!("  test [--engine naive|jit]: run riscv-tests");
      exit(1);
    }
  }
}

fn parse_test_options(argv: &[String]) -> Result<TestOptions, String> {
  let mut engine = None;
  let mut args = argv.iter().filter(|arg| arg.as_str() != "--");

  while let Some(arg) = args.next() {
    let value = if arg == "--engine" {
      args
        .next()
        .ok_or_else(|| "--engine requires a value".to_string())?
        .as_str()
    } else if let Some(value) = arg.strip_prefix("--engine=") {
      value
    } else {
      return Err(format!("unknown test option {arg:?}"));
    };

    if engine.is_some() {
      return Err("--engine may only be specified once".to_string());
    }
    engine = Some(ExecutionEngine::parse(value)?);
  }

  Ok(TestOptions {
    engine: engine.unwrap_or(ExecutionEngine::Naive),
  })
}

fn run_riscv_tests(options: TestOptions) -> Result<(), Error> {
  let sh = Shell::new()?;
  let testing_dir = project_root().join("valheim-testing");
  let test_source_dir = testing_dir.join("riscv-tests");
  let test_target_dir = testing_dir.join("target");
  let nproc = cmd!(sh, "nproc").read().unwrap_or("1".to_string());

  println!(":: Compiling debug variant of Valheim emulator");
  sh.change_dir(project_root());
  cmd!(sh, "cargo make-debug").run()?;

  println!(":: Compiling riscv-tests with {} threads...", nproc);
  sh.change_dir(test_source_dir);
  cmd!(sh, "./configure --prefix={test_target_dir}").run()?;
  cmd!(sh, "make -j{nproc}").run()?;
  cmd!(sh, "make install").run()?;

  println!(":: Running riscv-tests with Valheim emulator...");
  let enabled_tests = include_str!("../../valheim-testing/enabled-tests.txt")
    .lines()
    .filter(|l| !l.is_empty())
    .filter(|l| !l.starts_with("#"));
  for test in enabled_tests {
    let elf = isa(test);
    run_one(test, elf, options.engine)?;
  }
  Ok(())
}

fn run_one(test_name: &str, elf: PathBuf, engine: ExecutionEngine) -> Result<(), Error> {
  let sh = Shell::new()?;
  let emulator = binary_dir("debug").join("valheim-cli");
  let jit_args = match engine {
    ExecutionEngine::Naive => Vec::new(),
    ExecutionEngine::Jit => vec!["--jit-hot-threshold", "1"],
  };
  let engine = engine.as_str();
  let bin = elf.parent().unwrap().join(format!(
    "{}.bin",
    elf.file_name().unwrap().to_str().unwrap()
  ));
  cmd!(sh, "riscv64-unknown-elf-objcopy -O binary {elf} {bin}")
    .quiet()
    .run()?;
  cmd!(
    sh,
    "{emulator} --engine {engine} {jit_args...} --test --test-name {test_name} --kernel {bin}"
  )
  .quiet()
  .run()?;
  Ok(())
}

fn riscv_tests_install_dir() -> PathBuf {
  project_root().join("valheim-testing").join("target")
    .join("share").join("riscv-tests")
}

fn benchmark(name: &str) -> PathBuf {
  riscv_tests_install_dir().join("benchmarks").join(format!("{}.riscv", name))
}

fn isa(name: &str) -> PathBuf {
  riscv_tests_install_dir().join("isa").join(name)
}

// copied from https://github.com/rustsbi/rustsbi-qemu/blob/main/xtask/src/main.rs
fn project_root() -> PathBuf {
  Path::new(&env!("CARGO_MANIFEST_DIR"))
    .ancestors()
    .nth(1)
    .unwrap()
    .to_path_buf()
}

fn binary_dir(mode: &str) -> PathBuf {
  project_root().join("target").join(mode)
}

#[cfg(test)]
mod tests {
  use super::*;

  fn parse(args: &[&str]) -> Result<TestOptions, String> {
    parse_test_options(&args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>())
  }

  #[test]
  fn test_engine_defaults_to_naive() {
    assert_eq!(parse(&[]).unwrap().engine, ExecutionEngine::Naive);
    assert_eq!(parse(&["--"]).unwrap().engine, ExecutionEngine::Naive);
  }

  #[test]
  fn test_engine_accepts_separate_and_equals_forms() {
    assert_eq!(
      parse(&["--engine", "jit"]).unwrap().engine,
      ExecutionEngine::Jit,
    );
    assert_eq!(
      parse(&["--", "--engine=naive"]).unwrap().engine,
      ExecutionEngine::Naive,
    );
  }

  #[test]
  fn test_engine_rejects_invalid_or_missing_values() {
    assert!(parse(&["--engine", "invalid"])
      .unwrap_err()
      .contains("invalid engine"));
    assert!(parse(&["--engine"])
      .unwrap_err()
      .contains("requires a value"));
  }

  #[test]
  fn test_engine_rejects_unknown_and_duplicate_options() {
    assert!(parse(&["--wat"])
      .unwrap_err()
      .contains("unknown test option"));
    assert!(parse(&["--engine", "naive", "--engine", "jit"])
      .unwrap_err()
      .contains("only be specified once"),);
  }
}
