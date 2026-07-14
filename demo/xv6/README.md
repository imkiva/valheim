# xv6 on Valheim

This demo builds Valheim and the legacy VirtIO-compatible xv6-riscv commit
`a1da53a5a12e21b44a2c79d962a437fa2107627c`, then starts an interactive xv6
shell:

```bash
./demo/xv6/run.sh
```

The first run downloads and SHA-256 verifies the pinned RISC-V GNU toolchain,
installs `nightly-2024-09-05` with rustup's minimal profile, clones xv6, and
builds both projects. Persistent downloads, sources, build output, and the
writable guest disk all stay under `target/demo/`.

Host prerequisites are `bash`, `curl`, `git`, `make`, `rustup`, `sha256sum`,
`tar`, and Device Tree Compiler (`sudo apt install device-tree-compiler`).
Successful boot ends at:

```text
xv6 kernel is booting

init: starting sh
$
```

Press `Ctrl-C` to stop Valheim. To reset guest-side disk changes, run:

```bash
RESET_DISK=1 ./demo/xv6/run.sh
```

Optional overrides are `JOBS`, `VALHEIM_RUST_TOOLCHAIN`, and
`VALHEIM_RISCV_TOOLCHAIN`.
