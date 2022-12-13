#![allow(dead_code)]

use std::path::{Path, PathBuf};
use std::process::exit;

use xshell::{cmd, Error, Shell};

fn main() -> Result<(), Error> {
  let argv = std::env::args().skip(1).collect::<Vec<_>>();
  match argv.first().map(|s| s.as_str()) {
    Some("test") => run_riscv_tests(argv.into_iter().skip(1).collect()),
    _ => {
      eprintln!("Usage: xtask <command>");
      eprintln!("Available <command>s:");
      eprintln!("  test: run riscv-tests");
      exit(1);
    }
  }
}

fn run_riscv_tests(_argv: Vec<String>) -> Result<(), Error> {
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
    run_one(test, elf)?;
  }
  Ok(())
}

fn run_one(test_name: &str, elf: PathBuf) -> Result<(), Error> {
  let sh = Shell::new()?;
  let emulator = binary_dir("debug").join("valheim-cli");
  let bin = elf.parent().unwrap().join(format!("{}.bin", elf.file_name().unwrap().to_str().unwrap()));
  cmd!(sh, "riscv64-unknown-elf-objcopy -O binary {elf} {bin}").quiet().run()?;
  cmd!(sh, "{emulator} --test --test-name {test_name} --kernel {bin}").quiet().run()?;
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
