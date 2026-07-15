# Debian 13 ext4 Linux Demo

这个 demo 在 Valheim 中启动 Linux 5.17，并进入来自 Docker Official Image 的 Debian 13
`trixie-slim` RISC-V 64 位 Bash。根文件系统不是手写的 BusyBox 或伪造的
`os-release`：脚本按 OCI digest 从 Docker Hub 的 `library/debian` 下载官方
`debian:13-slim` riscv64 layer，并校验完整 SHA-256。

根文件系统位于一个无分区表的 256 MiB sparse raw ext4 镜像中，通过 Valheim 的
legacy VirtIO-MMIO block 设备作为 `/dev/vda` 挂载。kernel `Image` 不再内嵌完整
initramfs；VirtIO、VirtIO-MMIO、VirtIO block 和 ext4 都直接编进 kernel，默认命令行为：

```text
root=/dev/vda rootfstype=ext4 rootwait rw init=/init
```

脚本从官方 OCI layer 生成经过校验的只读 base，再复制成 guest 实际使用的可写 runtime：

```text
target/demo/linux/build/debian-13-slim-riscv64.ext4  # verified read-only base
target/demo/linux/runtime/rootfs.ext4                 # writable runtime
```

脚本把 base 设为 `0444`，并拒绝把它（包括 symlink/hardlink）作为可写 `--disk`
传给模拟器。runtime 默认跨运行保留；`RESET_DISK=1` 才会明确从 base 重置。镜像
schema 会覆盖 layer、`/init`、容量、block size、UUID、label 和 feature 集；已有
runtime 的 schema 缺失或变化时脚本会停止并要求用户先备份，不会静默删除 guest 数据。

官方 OCI layer 保持不变；构建时只加入以下启动 overlay：

- `/init`：挂载 devtmpfs、proc、sysfs、tmpfs 和 devpts，确认 `/` 实际是 read-write
  ext4，然后用 `/bin/bash` 启动交互 shell。
- `/dev/console`、`/dev/null`、`/dev/tty`：让 PID 1 在挂载 devtmpfs 前就能打开控制台。

OCI 解包、overlay/device node 创建和 `mke2fs -d` 全部位于同一个 `fakeroot`
会话，因此保留官方 layer 中的 UID、GID、权限、符号链接和硬链接。整个过程不使用
宿主 mount 或 sudo，也不会修改下载的 rootfs tarball。

ext4 使用固定 UUID `3f3434d2-6c1e-4f8b-98e8-4f525649534b`、label
`VALHEIMROOT`、4 KiB block 和明确的 feature 集；`lazy_itable_init` 与
`lazy_journal_init` 均关闭。脚本对 base 运行只读 `e2fsck -fn`，并用 `debugfs`
检查 superblock、`/init`、设备节点、`/etc/shadow` 的 `0:42/0640` 元数据以及
Debian Perl hardlink，避免 fakeroot 元数据在转换时悄悄丢失。

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
3. 下载 Linux 5.17，生成并校验保留官方元数据的 ext4 base；首次运行或显式重置时
   创建独立可写 runtime。
4. 调用 `demo/rustsbi/run.sh --build-only` 构建历史 RustSBI firmware。
5. 构建包含内建 VirtIO block/ext4 驱动的 raw `Image`。
6. 构建 release 版 `valheim-cli`，自动传入 runtime `--disk`，然后进入交互式 guest。

下载、源码、构建和运行目录全部位于 `target/demo/linux/`，共享 Linux GNU
工具链位于 `target/demo/gcc-riscv64-glibc-2022.03.09/`。下载和构建产物都有
缓存；再次运行会重新校验固定摘要和 ext4 base，复用正确的下载、工具链、runtime
以及增量 kernel 构建。

主机需要 `curl`、`make`、`cc`、`bc`、`bison`、`flex`、`perl`、`fakeroot`、
`gzip`、`xz`、`tar`、`python3`、`sha256sum`、`mke2fs`、`e2fsck`、`debugfs`、
`dtc`、`flock` 和 Rustup。三个 ext4 工具由 Debian/Ubuntu 的 `e2fsprogs` 提供，
`flock` 来自 `util-linux`。脚本会自动安装 RustSBI demo 已固定的 Rust toolchain。
在最小 Debian/Ubuntu 宿主上可安装以下系统依赖；Rustup 仍需单独安装：

```bash
sudo apt-get update
sudo apt-get install -y \
  build-essential bc bison curl device-tree-compiler e2fsprogs fakeroot flex \
  gzip perl python3 tar util-linux xz-utils
```

如果共享缓存完全为空，四个固定大型 archive 合计约 1.24 GB：Linux GNU
工具链约 612 MB、RustSBI bare-metal GNU 工具链约 472 MB、Linux source
约 128 MB、Debian rootfs layer 约 28 MB。此外还会下载两个 Rust nightly、guest
target 和 Cargo 依赖；旧 Cargo 首次同步 crates.io Git index 也会占用约 1–2 GiB。
ext4 文件逻辑大小为 256 MiB，但 base 和首次 runtime copy 都保持 sparse（支持时优先
使用 reflink）。连同解压和构建产物，建议至少预留 8 GiB 可用空间。

可选环境变量：

- `JOBS`：kernel、RustSBI 和 Cargo 的并行任务数；默认使用 `nproc`。
- `RESET_DISK=1`：丢弃默认 runtime 的 guest 写入，从已验证的只读 base 创建
  新副本；默认 `0`，会复用既有 runtime。
- `VALHEIM_RUST_TOOLCHAIN`：Valheim Rust toolchain；默认 `nightly-2024-09-05`。
- `VALHEIM_LINUX_TOOLCHAIN`：共享 Linux GNU 工具链目录；默认是上面的固定目录。
- `VALHEIM_RISCV_TOOLCHAIN`：RustSBI 的共享 bare-metal GNU 工具链目录。

脚本使用 `RUSTUP_HOME=target/demo/rustup`、`CARGO_HOME=target/demo/cargo-home`
和 `CARGO_TARGET_DIR=target/demo/cargo`；调用 RustSBI demo 时也共享这些目录，
确保 Rust/Cargo 下载和构建产物不落到仓库外。`CARGO_HOME` 只注入
实际 Cargo 子进程，以便继续复用宿主已安装的 rustup 代理。

额外参数继续透传给 `valheim-cli`。脚本默认注入 `--engine jit`，显式
`--engine naive` 可覆盖。脚本也会自动注入默认 runtime 的 `--disk`；若用户显式提供
`--disk PATH`、`--disk=PATH`、`-d PATH`、`-dPATH` 或 `-d=PATH`，则不会重复注入。
attached `-cCMDLINE`/`-c=CMDLINE` 也会正确覆盖默认 cmdline。`RESET_DISK=1` 与显式
disk 不能同时使用，避免重置了未实际启动的另一份镜像。

脚本对默认 runtime 使用稳定 lock file，并对实际 disk inode 持有 nonblocking
exclusive `flock`；锁定的文件描述符会跨最后的 `exec` 保留到 Valheim 退出。因此两个
`run.sh` 不能同时读写同一镜像，重置也不能覆盖正在使用的默认 runtime。显式 external
disk 同样会锁定；它必须是可写 raw ext4、包含可执行 `/init`，并符合用户提供的 kernel
cmdline。直接调用 `valheim-cli --disk ...` 会绕过这个 advisory lock 约定，调用者必须自行
保证没有另一个进程使用该镜像。

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

## 启动、写入与验收

以下数据来自旧的内建-initramfs demo，不是当前 ext4 block-root 性能基线：realtime
切换前的历史固定 CPU 16 release 基线是 JIT 4.806 秒、naive
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
awk '$2 == "/" { print $1, $3, $4 }' /proc/mounts
grep riscv-timer /proc/interrupts
time sleep 1
grep riscv-timer /proc/interrupts
```

`SBI TIME extension detected`、`sched_clock: 64 bits at 10MHz`、VirtIO block 识别、
ext4 以 read-write 方式挂载、`sleep 1` 约一秒且
前后两次 `riscv-timer` IRQ 计数增加，是 realtime/timer-relay 验收的一部分。

验证 runtime 可写和跨进程持久化：

```bash
echo EXT4_PERSIST_OK >/root/valheim-persist
sync
cat /root/valheim-persist
```

执行 `sync` 后按宿主 `Ctrl-C` 退出，再次运行脚本，文件应仍存在。然后退出并运行：

```bash
RESET_DISK=1 ./demo/linux/run.sh
```

重置后的 guest 中 `/root/valheim-persist` 应不存在。宿主 `Ctrl-C` 等价于突然断电，
所以持久化验收必须先在 guest 执行 `sync`；日志恢复也应在下一次启动时保持文件系统
可挂载。不要在 Valheim 仍持有 mmap 时用宿主 e2fsprogs 检查 runtime。

固定 layer 应报告 Debian 13/trixie（完整版本 `13.6`），`uname` 应报告 Linux
`5.17.0`、架构 `riscv64`，`id` 应报告 `uid=0(root)`。按宿主终端的 `Ctrl-C`
退出 Valheim。

关键产物：

```text
target/demo/linux/build/debian-13-slim-riscv64.ext4
target/demo/linux/runtime/rootfs.ext4
target/demo/linux/build/kernel/arch/riscv/boot/Image
target/demo/rustsbi/artifacts/valheim/rustsbi-qemu.bin
target/demo/cargo/release/valheim-cli
```

旧的 `.cpio` 可能仍留在既有 `target/demo` 缓存中，但当前 kernel 配置要求
`CONFIG_INITRAMFS_SOURCE=""`，不会再将它嵌入 `Image`。

不要对仓库运行 `cargo clean`；它会删除整个 `target/`，包括下载的共享工具链和
全部 demo。
