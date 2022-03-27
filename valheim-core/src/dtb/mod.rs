use std::io::{Read, Write};
use std::process::{Command, Stdio};

const DTS_TEMPLATE: &str = include_str!("../../../dts/valheim.dts.template");

pub fn generate_device_tree_rom(cmdline: String) -> Result<Vec<u8>, std::io::Error> {
  let instantiated: String = DTS_TEMPLATE.to_string()
    .replace("${VALHEIM_BOOTARGS}", cmdline.as_ref());

  let mut dtb_bytes = call_compiler(instantiated)?;
  let mut rom = vec![0; 32];
  rom.append(&mut dtb_bytes);
  let align = 0x1000;
  rom.resize((rom.len() + align - 1) / align * align, 0);
  Ok(rom)
}

fn call_compiler(dts: String) -> Result<Vec<u8>, std::io::Error> {
  // TODO: self-made device-tree-compiler. crates.io have only readers, not writers.
  let compiler = match Command::new("dtc")
    .args(["-I", "dts", "-O", "dtb", "-o", "-", "-"])
    .stdin(Stdio::piped())
    .stdout(Stdio::piped())
    .stderr(Stdio::null())
    .spawn() {
    Ok(p) => p,
    Err(e) => {
      eprintln!("Cannot find device tree compiler command `dtc` on this machine ({}).", e);
      eprintln!("Try installing `dtc` by:");
      eprintln!("  macOS : brew install dtc");
      eprintln!("  Ubuntu: sudo apt install device-tree-compiler");
      eprintln!("In the near future, Valheim should comes with a builtin device tree compiler.");
      return Err(e);
    }
  };

  compiler.stdin.expect("device tree compiler unavailable").write_all(dts.as_bytes())?;
  let mut dtb_bytes = Vec::with_capacity(32);
  compiler.stdout.expect("device tree compiler unavailable").read_to_end(&mut dtb_bytes)?;
  Ok(dtb_bytes)
}
