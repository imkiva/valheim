# RustSBI-QEMU test kernel on Valheim

这个 demo 构建并运行 Valheim 在 2022 年 3 月展示过的 RustSBI-QEMU test
kernel。它是会自行结束的 SBI 测试，不是交互式 shell。

## 一键运行

从仓库根目录或任意工作目录运行：

```bash
./demo/rustsbi/run.sh
```

普通运行默认使用 JIT；需要参考解释器时传入 `--engine naive`。

只准备 RustSBI firmware 和 test kernel、不启动 guest：

```bash
./demo/rustsbi/run.sh --build-only
```

`--build-only` 只能作为第一个参数且不能带其他参数。普通运行允许把额外参数继续传给
`valheim-cli`，显式 `--engine` 会覆盖脚本的 JIT 默认值。脚本以出现以下行为作为成功：

```text
<< Test-kernel: All hart SBI test SUCCESS, shutdown
```

完整输出写入：

```text
target/demo/rustsbi/runtime/last-run.log
```

## 可复现版本

- RustSBI-QEMU commit：`999e3556fcfa1b0900dd797ae2186667af8d2dc6`
- RustSBI-QEMU `0.1.0` / RustSBI `0.2.1` / SBI spec `0.3`
- RustSBI Rust：`nightly-2022-02-14`
- Valheim Rust：`nightly-2024-09-05`
- guest target：`riscv64imac-unknown-none-elf`
- RISC-V GNU toolchain：官方 `2022.03.09` bare-metal 资产，GCC `11.1.0`、
  Binutils `2.37`
- 工具链 archive SHA-256：
  `6ec8ea11558f283aecd47c52a25c61a10c117ed703fee09c5a7dbfde3b522da1`

Valheim 的 RustSBI 截图加入于 2022-03-20；上述 commit 是当时默认分支的
HEAD。旧源码依赖后来从 Rust 删除的 generator API，因此必须使用同年代 nightly。

## 自动安装与产物位置

主机只需预先提供 `curl`、`git`、`tar`、`sha256sum`、`rustup`、`tee` 和
`dtc`。Ubuntu/Debian 上按项目约定安装 `dtc`：

```bash
sudo apt-get update
sudo apt-get install -y device-tree-compiler
```

脚本会按需完成以下工作：

1. 下载并校验固定的 RISC-V GNU toolchain。
2. 用 minimal profile 安装缺失的两个 Rust nightly，并为旧 nightly 安装 guest
   target。
3. clone 固定 RustSBI-QEMU commit，应用三个兼容 patch。
4. 构建 RustSBI、test kernel 和 Valheim，必要时启动并校验 test kernel。

所有下载、源码、缓存和产物都在 Git 忽略的 `target/demo/`：

```text
target/demo/
├── gcc-riscv64-elf-2022.03.09/ # 共享 GNU toolchain
├── rustup/                      # demo 专用 RUSTUP_HOME
├── cargo-home/                  # demo 专用 CARGO_HOME
├── cargo/                       # Valheim CARGO_TARGET_DIR
└── rustsbi/
    ├── source/
    ├── cargo-target/
    ├── artifacts/valheim/
    └── runtime/last-run.log
```

不要运行 `cargo clean`；它会删除整个 `target/`，包括上述下载和产物。

可选覆盖变量：

- `JOBS`
- `RUSTSBI_RUST_TOOLCHAIN`
- `VALHEIM_RUST_TOOLCHAIN`
- `VALHEIM_RISCV_TOOLCHAIN`：复用已安装的兼容 bare-metal GNU 工具链。

## 为什么需要 patch

上游 commit `999e355` 的 test kernel 硬编码了 hart 1–4 的 HSM 流程，而
Valheim 只有 hart 0。未修改版本会在 `Wake hart 1` 后等待。
`single-hart-valheim.patch` 保留 hart 0 上可运行的测试，打印历史 success
marker，然后用 `ebreak` 适配 Valheim 的退出约定。该 marker 不能解释为实际测试了
hart 1–4。

该历史 firmware 在 Valheim 上未打 patch 实跑时，S-mode test kernel 观察到
`a1=0`。`valheim-dtb-pointer.patch` 把 supervisor 入口的 DTB 地址显式设为
`0x87f00000`。这个常量必须与 Valheim `Machine` 中的 `RV64_DTB_ADDR` 保持一致；
这是 Valheim 启动布局兼容措施，不代表已确认的上游通用 bug。

`valheim-time-relay.patch` 修复该历史 firmware 的 supervisor timer 中继首次建立和
后续重装。MachineTimer trap 会设置 STIP 并关闭 MTIE；每次 SBI TIME `set_timer`
必须按“屏蔽 MTIE → 写新 `mtimecmp` → 清旧 STIP → 重开 MTIE”的顺序装载。
未打 patch 时，当前初始化路径不会打开 MTIE，Linux 无法可靠获得该中继。这一路径由
Linux `sleep` 和 `/proc/interrupts` 验收；历史 test kernel 本身不调用 timer 扩展，
不能单独证明 relay 正常。

不要改用旧仓库的 `cargo make`。它会向 GNU objcopy 传入仅适用于
`rust-objcopy` 的 `--binary-architecture=riscv64`，Binutils 2.37 会拒绝该参数。
