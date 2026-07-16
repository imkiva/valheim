use std::io::{Read, Write};
use std::process::{Command, Stdio};

use crate::device::clint::TIMEBASE_FREQUENCY;

const DTS_TEMPLATE: &str = include_str!("../../../dts/valheim.dts.template");
const VALHEIM_VERSION: &str = env!("CARGO_PKG_VERSION");
const VALHEIM_REPOSITORY: &str = env!("CARGO_PKG_REPOSITORY");

pub fn generate_device_tree_rom(
  cmdline: String,
  memory_base: u64,
  memory_size: u64,
) -> Result<Vec<u8>, std::io::Error> {
  let instantiated = instantiate_device_tree(&cmdline, memory_base, memory_size);
  let mut dtb_bytes = call_compiler(instantiated)?;
  let mut rom = vec![0; 32];
  rom.append(&mut dtb_bytes);
  let align = 0x1000;
  rom.resize((rom.len() + align - 1) / align * align, 0);
  Ok(rom)
}

fn instantiate_device_tree(cmdline: &str, memory_base: u64, memory_size: u64) -> String {
  let memory_reg = format!(
    "{:#x} {:#x} {:#x} {:#x}",
    (memory_base >> 32),
    (memory_base & (u32::MAX as u64)),
    (memory_size >> 32),
    (memory_size & (u32::MAX as u64)),
  );
  DTS_TEMPLATE
    .to_string()
    .replace("${VALHEIM_BOOTARGS}", cmdline)
    .replace("${VALHEIM_MEMORY_REG}", &memory_reg)
    .replace(
      "${VALHEIM_TIMEBASE_FREQUENCY}",
      &format!("{:#x}", TIMEBASE_FREQUENCY),
    )
    .replace("${VALHEIM_VERSION}", VALHEIM_VERSION)
    .replace("${VALHEIM_REPOSITORY}", VALHEIM_REPOSITORY)
}

fn call_compiler(dts: String) -> Result<Vec<u8>, std::io::Error> {
  // TODO: self-made device-tree-compiler. crates.io have only readers, not writers.
  let compiler = match Command::new("dtc")
    .args(["-I", "dts", "-O", "dtb", "-o", "-", "-"])
    .stdin(Stdio::piped())
    .stdout(Stdio::piped())
    .stderr(Stdio::null())
    .spawn()
  {
    Ok(p) => p,
    Err(e) => {
      eprintln!(
        "Cannot find device tree compiler command `dtc` on this machine ({}).",
        e
      );
      eprintln!("Try installing `dtc` by:");
      eprintln!("  macOS : brew install dtc");
      eprintln!("  Ubuntu: sudo apt install device-tree-compiler");
      eprintln!("In the near future, Valheim should comes with a builtin device tree compiler.");
      return Err(e);
    }
  };

  compiler
    .stdin
    .expect("device tree compiler unavailable")
    .write_all(dts.as_bytes())?;
  let mut dtb_bytes = Vec::with_capacity(32);
  compiler
    .stdout
    .expect("device tree compiler unavailable")
    .read_to_end(&mut dtb_bytes)?;
  Ok(dtb_bytes)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn device_tree_identifies_valheim_build() {
    const ROM_HEADER_SIZE: usize = 32;
    const FDT_MAGIC: u32 = 0xd00d_feed;

    let rom =
      generate_device_tree_rom("console=ttyS0".to_string(), 0x8000_0000, 0x1000_0000).unwrap();
    let fdt = &rom[ROM_HEADER_SIZE..];
    assert_eq!(u32::from_be_bytes(fdt[0..4].try_into().unwrap()), FDT_MAGIC);
    let fdt_size = u32::from_be_bytes(fdt[4..8].try_into().unwrap()) as usize;

    let mut decompiler = Command::new("dtc")
      .args(["-q", "-I", "dtb", "-O", "dts", "-o", "-", "-"])
      .stdin(Stdio::piped())
      .stdout(Stdio::piped())
      .spawn()
      .unwrap();
    decompiler
      .stdin
      .as_mut()
      .unwrap()
      .write_all(&fdt[..fdt_size])
      .unwrap();
    let output = decompiler.wait_with_output().unwrap();
    assert!(output.status.success());
    let dts = String::from_utf8(output.stdout).unwrap();

    assert!(dts.contains("compatible = \"valheim,virt\\0riscv-virtio\";"));
    assert!(dts.contains("model = \"Valheim RISC-V virtual machine\";"));
    assert!(dts.contains(&format!("valheim,version = \"{VALHEIM_VERSION}\";")));
    assert!(dts.contains(&format!("valheim,repository = \"{VALHEIM_REPOSITORY}\";")));
    assert!(dts.contains(&format!(
      "valheim,release = \"Valheim {VALHEIM_VERSION}\\nGitHub: {VALHEIM_REPOSITORY}\\n\";"
    )));
    assert!(dts.contains("virtio_mmio@10001000"));
    assert!(dts.contains("reg = <0x00 0x10001000 0x00 0x1000>;"));
    assert!(dts.contains("virtio_mmio@10002000"));
    assert!(dts.contains("reg = <0x00 0x10002000 0x00 0x1000>;"));
    assert!(dts.contains("interrupts = <0x02>;"));
    assert!(dts.contains("rtc@101000"));
    assert!(dts.contains("compatible = \"google,goldfish-rtc\";"));
    assert!(dts.contains("reg = <0x00 0x101000 0x00 0x1000>;"));
    assert!(dts.contains("interrupts = <0x0b>;"));
  }
}
