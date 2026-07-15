# Debian 13 最小 Linux Demo

这个 demo 在 Valheim 中启动 Linux 5.17，并进入来自 Docker Official Image 的 Debian 13
`trixie-slim` RISC-V 64 位 Bash。根文件系统不是手写的 BusyBox 或伪造的
`os-release`：脚本按 OCI digest 从 Docker Hub 的 `library/debian` 下载官方
`debian:13-slim` riscv64 layer，并校验完整 SHA-256。

根文件系统作为 gzip 压缩的内建 initramfs 放进 kernel `Image`，不使用 Valheim
的 legacy VirtIO 磁盘。官方 OCI layer 保持不变；打包时只拼接一个单独的 overlay：

- `/init`：挂载 devtmpfs、proc、sysfs、tmpfs 和 devpts，然后用 `/bin/bash` 启动交互 shell。
- `/dev/console`、`/dev/null`、`/dev/tty`：让 PID 1 在挂载 devtmpfs 前就能打开控制台。

打包过程在同一个 `fakeroot` 会话中解包并生成 cpio，因此保留官方 layer 中的
UID、GID、权限、符号链接和硬链接，不要求用 root 身份构建，也不会修改下载的
rootfs tarball。

## 一键运行

从仓库根目录执行：

```bash
./demo/linux/run.sh
```

脚本默认使用 JIT；需要显式运行参考解释器时传入 `--engine naive`。

脚本使用自身路径定位仓库，所以也可以从其他工作目录调用。首次运行会依次：

1. 下载并校验固定的 Debian OCI index、riscv64 manifest 和 rootfs layer。
2. 下载并解压固定的 RISC-V Linux GNU 工具链到共享目录
   `target/demo/gcc-riscv64-glibc-2022.03.09/`。
3. 下载 Linux 5.17、生成保留官方元数据的 initramfs，并构建 raw `Image`。
4. 调用 `demo/rustsbi/run.sh --build-only` 构建历史 RustSBI firmware。
5. 构建 release 版 `valheim-cli`，然后直接进入交互式 guest。

下载、源码、构建和运行目录全部位于 `target/demo/linux/`，共享 Linux GNU
工具链位于 `target/demo/gcc-riscv64-glibc-2022.03.09/`。下载和构建产物都有
缓存；再次运行会重新校验固定摘要，复用正确的下载、工具链、initramfs 和增量
kernel 构建。

主机需要 `curl`、`make`、`cc`、`bc`、`bison`、`flex`、`perl`、`fakeroot`、
`cpio`、`gzip`、`xz`、`tar`、`python3`、`sha256sum`、`dtc` 和 Rustup。脚本会自动
安装 RustSBI demo 已固定的 Rust toolchain。`dtc` 按项目约定通过 apt 安装：

```bash
sudo apt-get update
sudo apt-get install -y device-tree-compiler
```

如果共享缓存完全为空，四个固定大型 archive 合计约 1.24 GB：Linux GNU
工具链约 612 MB、RustSBI bare-metal GNU 工具链约 472 MB、Linux source
约 128 MB、Debian rootfs layer 约 28 MB。此外还会下载两个 Rust nightly、guest
target 和 Cargo 依赖；旧 Cargo 首次同步 crates.io Git index 也会占用约 1–2 GiB。
连同解压和构建产物，建议至少预留 8 GiB 可用空间。

可选环境变量：

- `JOBS`：kernel、RustSBI 和 Cargo 的并行任务数；默认使用 `nproc`。
- `VALHEIM_RUST_TOOLCHAIN`：Valheim Rust toolchain；默认 `nightly-2024-09-05`。
- `VALHEIM_LINUX_TOOLCHAIN`：共享 Linux GNU 工具链目录；默认是上面的固定目录。
- `VALHEIM_RISCV_TOOLCHAIN`：RustSBI 的共享 bare-metal GNU 工具链目录。

脚本使用 `RUSTUP_HOME=target/demo/rustup`、`CARGO_HOME=target/demo/cargo-home`
和 `CARGO_TARGET_DIR=target/demo/cargo`；调用 RustSBI demo 时也共享这些目录，
确保 Rust/Cargo 下载和构建产物不落到仓库外。`CARGO_HOME` 只注入
实际 Cargo 子进程，以便继续复用宿主已安装的 rustup 代理。

## 固定来源与摘要

| 内容 | 固定来源 | SHA-256 / OCI digest |
| --- | --- | --- |
| Debian `13-slim` OCI index | `registry-1.docker.io/v2/library/debian` | `020c0d20b9880058cbe785a9db107156c3c75c2ac944a6aa7ab59f2add76a7bd` |
| riscv64 OCI manifest | 同上；index 中 `architecture=riscv64` 的 manifest | `7244fbb388f7b59c9f584bb2bb7ef3a60b23aa1e55f1ad1d0641bd5ec12390f3` |
| Debian rootfs layer | manifest 中唯一的 gzip layer | `3ed37bd5491de4685b6418abd6b83c4b16cc06b7a51e46da7f154c5a149a41a5` |
| Linux 5.17 | `https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-5.17.tar.xz` | `555fef61dddb591a83d62dd04e252792f9af4ba9ef14683f64840e46fa20b1b1` |
| RISC-V GNU/Linux toolchain | `riscv-collab/riscv-gnu-toolchain` release `2022.03.09` 的 Ubuntu 20.04 glibc asset | `02b97cf3502d9542943b62c7470d99f97c0c9148be95e1277df96d4b5c2fdb41` |

固定的 Debian layer 当前内容是 Debian `13.6`，OCI index annotation 标记为
`trixie-slim` 和 `riscv64`。Linux 5.17 与 Debian 版本是两个不同层次：前者是
适配 Valheim/RustSBI 的 guest kernel，后者是官方 Debian 用户空间。Debian 13
glibc 声明的最低 Linux ABI 为 4.15，因此可以在这个 5.17 kernel 上运行。

## 启动与验收

realtime 切换前的历史固定 CPU 16 release 基线是 JIT 4.806 秒、naive
60.926 秒；该时钟会快进 guest 等待。`10cabc6`/`258fdf6` 后的 realtime JIT
三次为 8.903906 / 8.379352 / 8.321334 秒，中位数 8.379352 秒。硬件不同
会变化，且新旧口径不可直接比较。realtime naive 单次验收为
133.996711 秒，不是正式三次中位数。kernel timestamp 来自 host-monotonic 驱动的
10 MHz `mtime`，与宿主 elapsed time 等速推进；CLINT 在 `Machine`/CPU 构造期间建立
anchor，计数包含随后 DTB 生成及 kernel/BIOS 的读取和装载，但不包含此前的 CLI 参数解析和
JIT executor 构造。它不是宿主日历时间，也不能代替进程级外部计时。看到下面提示即启动成功：

```text
Debian 13 (trixie) official slim rootfs on Valheim
...
debian13#
```

在 guest 中可运行：

```bash
cat /etc/os-release
cat /etc/debian_version
uname -a
printf 'BASH=%s\n' "$BASH_VERSION"
id
echo DEBIAN13_SHELL_OK
grep riscv-timer /proc/interrupts
time sleep 1
grep riscv-timer /proc/interrupts
```

`SBI TIME extension detected`、`sched_clock: 64 bits at 10MHz`、`sleep 1` 约一秒且
前后两次 `riscv-timer` IRQ 计数增加，是 realtime/timer-relay 验收的一部分。

固定 layer 应报告 Debian 13/trixie（完整版本 `13.6`），`uname` 应报告 Linux
`5.17.0`、架构 `riscv64`，`id` 应报告 `uid=0(root)`。按宿主终端的 `Ctrl-C`
退出 Valheim。

关键产物：

```text
target/demo/linux/build/debian-13-slim-riscv64.cpio
target/demo/linux/build/kernel/arch/riscv/boot/Image
target/demo/rustsbi/artifacts/valheim/rustsbi-qemu.bin
target/demo/cargo/release/valheim-cli
```

不要对仓库运行 `cargo clean`；它会删除整个 `target/`，包括下载的共享工具链和
全部 demo。
