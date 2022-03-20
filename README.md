# valheim
Learning purpose riscv64 (RV64GC) emulator.
This project is built for [一生一芯](https://ysyx.org/) as a reference implementation.

### Highlights
- [Type-safe instructions](./valheim-core/src/isa/typed.rs) which makes the decoding [less error-prone](./valheim-core/src/isa/decode.rs).
- Full emulation trace (registers, memory, etc.) like persistent data structures, which is useful for debugging the real hardware.
- [MISA]() = `RV64ACDFIMSU`
  - RV64G (IMAFD_Zicsr_Zifencei) instruction set
  - RVC extension
  - Supervisor mode extension
  - User mode extension

### Amazing Moments

#### Running [xv6 for RISC-V](https://github.com/mit-pdos/xv6-riscv)
With the following command:
```shell
$(CROSS)objcopy -O binary xv6/kernel xv6/kernel.bin
cargo run --release -- --kernel xv6/kernel.bin --disk xv6/fs.img
```

![xv6-booting](./pictures/xv6-booting.png)

#### Running [RustSBI-QEMU with its test kernel](https://github.com/rustsbi/rustsbi-qemu)
With the following command:
```shell
cargo run --release -- --kernel tests/test-kernel.bin --bios tests/rustsbi-qemu.bin
```

![rustsbi-booting](./pictures/rustsbi-booting.jpg)

