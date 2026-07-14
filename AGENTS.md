# Valheim 项目与本地 Demo 指南

本文档面向后续在本仓库工作的开发者和自动化 Agent。它记录当前源码结构、运行约定、已经验证的工具版本以及本地 demo 的复现方法。根目录 `README.md` 中部分命令已经随 workspace 结构变化而过时；涉及构建和运行时，以本文档及源码为准。

## 项目概览

Valheim 是一个用 Rust 编写、以学习和参考实现为目的的 RISC-V 64 位系统模拟器，项目版本为 `0.2.0`。它作为宿主普通进程运行，采用解释执行方式运行 guest，目标 ISA 是 RV64GC，并实现机器态、监管态和用户态所需的 CSR、异常、中断、分页以及一组 QEMU `virt` 风格设备。

当前实现的重要边界：

- 单 hart；设备树只声明 hart 0。
- 256 MiB guest RAM；这是为内嵌完整 Debian 13 slim rootfs 的 Linux demo 扩容后的值。
- guest kernel/BIOS 必须是 raw binary，CLI 不解析 ELF。
- VirtIO block 是 legacy VirtIO-MMIO version 1，队列长度为 8；现代要求 version 2 的 guest 驱动不兼容。
- 主执行器是 `NaiveInterpreter`，没有 JIT。
- UART 直接连接宿主标准输入和标准输出，并实现 Linux 8250 驱动需要的 DLAB、IIR、RX/TX 中断和状态位。

## Workspace 与模块职责

根 `Cargo.toml` 包含四个成员，但 `default-members = ["xtask"]`。因此裸 `cargo run` 或 `cargo build` 默认操作的是 `xtask`，不是模拟器 CLI。

| 路径 | 职责 |
| --- | --- |
| `valheim-asm/` | RISC-V 指令数据结构、类型安全的寄存器/立即数表示、16/32 位指令解码与编码，以及相关单元测试。 |
| `valheim-core/` | 模拟器核心：CPU/寄存器、CSR、异常和中断、MMU、指令执行、解释器、内存总线、设备、DTB、运行循环和 trace。 |
| `valheim-core/src/cpu/` | CPU 状态、执行语义、CSR、MMU、异常/中断和系统总线。 |
| `valheim-core/src/interp/` | 解释器接口与当前的朴素解释器实现。 |
| `valheim-core/src/device/` | CLINT、PLIC、NS16550A UART 和 legacy VirtIO block。 |
| `valheim-core/src/machine/` | 将 CPU、解释器、DTB、UART、kernel、BIOS 和磁盘组合成可运行的虚拟机。 |
| `valheim-cli/` | `valheim-cli` 命令行入口，负责参数解析和加载镜像。 |
| `xtask/` | 自动构建、转换并运行上游 `riscv-tests`。 |
| `dts/` | 启动时由 `dtc` 编译的设备树模板。 |
| `valheim-testing/` | 固定的 `riscv-tests` 子模块、启用/禁用测试列表和测试安装目录。 |
| `profiler/` | 旧的 DTrace/FlameGraph 性能分析脚本；当前不能直接开箱运行。 |
| `target/try/` | 本机手动安装的工具链和按 guest 分类的可运行 demo；被 Git 忽略。 |

## Guest 运行模型与硬件布局

| 项目 | 当前约定 |
| --- | --- |
| Reset PC | `0x8000_0000` |
| 无 BIOS | raw kernel 加载到 `0x8000_0000` |
| 有 BIOS | raw BIOS 加载到 `0x8000_0000`，raw kernel 加载到 `0x8020_0000` |
| DTB | 带 32 字节前缀的副本仍位于 MROM `0x1000`；裸 FDT 另复制到 DRAM `0x87f0_0000`，guest 的 `a1/x11` 指向后者 |
| RAM | `256 MiB @ 0x8000_0000` |
| CLINT | `0x0200_0000` |
| PLIC | `0x0c00_0000` |
| UART | `0x1000_0000`，IRQ 10 |
| VirtIO block | `0x1000_1000`，IRQ 1，legacy version 1 |
| 默认 kernel cmdline | `root=/dev/vda ro console=ttyS0` |

`Machine::new` 每次都会根据 `dts/valheim.dts.template` 调用外部 `dtc` 生成 DTB。因此 `dtc` 不只是构建依赖，也是每次运行 CLI 和 ISA 测试时的依赖。DTB 必须复制到普通 DRAM：Linux 建立最终页表后不会继续映射 Valheim 的低地址 MROM，若只传旧地址 `0x1020`，内核会在切换页表后访问异常。

## 已验证的本机工具环境

最后一次完整验证日期：2026-07-14。

- Valheim Rust：`nightly-2024-09-05`。根 `rust-toolchain` 只写了不固定版本的 `nightly`，为了复现不要依赖它解析到的最新版本。
- 历史 RustSBI demo Rust：`nightly-2022-02-14`，已安装 target `riscv64imac-unknown-none-elf`。旧源码使用已从现代 Rust 删除的 generator API，不能改用新 nightly。
- Device Tree Compiler：通过 apt 安装的 `device-tree-compiler 1.6.1-1`。
- RISC-V bare-metal GNU toolchain：
  `target/try/gcc-riscv64-elf-2022.03.09/`
  - GCC `11.1.0 (g5964b5cd727)`
  - Binutils/objcopy `2.37`
  - 可执行文件目录：`target/try/gcc-riscv64-elf-2022.03.09/riscv/bin`
  - 安装包 SHA-256：`6ec8ea11558f283aecd47c52a25c61a10c117ed703fee09c5a7dbfde3b522da1`
- RISC-V Linux GNU toolchain：
  `target/try/gcc-riscv64-glibc-2022.03.09/`
  - GCC `11.1.0`，Binutils `2.37`
  - target prefix：`riscv64-unknown-linux-gnu-`
  - 可执行文件目录：`target/try/gcc-riscv64-glibc-2022.03.09/riscv/bin`
  - 安装包 SHA-256：`02b97cf3502d9542943b62c7470d99f97c0c9148be95e1277df96d4b5c2fdb41`

工具链来自项目原 CI 使用的官方 2022-03-09 预编译资产：

```text
https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2022.03.09/riscv64-elf-ubuntu-20.04-nightly-2022.03.09-nightly.tar.gz
```

Linux 工具链来自同一批官方资产：

```text
https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2022.03.09/riscv64-glibc-ubuntu-20.04-nightly-2022.03.09-nightly.tar.gz
```

bare-metal 工具链供 xv6、RustSBI 和 `riscv-tests` 使用；Linux 内核必须使用 Linux-target 工具链。不要用 `riscv64-unknown-elf-` 构建 Linux，它的 linker 不支持 vDSO 所需的 `-shared`。

若 `target/try` 被删除，按用户约定手动恢复到共享工具链目录，不要用 apt 安装交叉 GCC，也不要把它放进某个 demo：

```bash
toolchain_dir="$PWD/target/try/gcc-riscv64-elf-2022.03.09"
archive="$toolchain_dir/riscv64-elf-ubuntu-20.04-nightly-2022.03.09-nightly.tar.gz"
mkdir -p "$toolchain_dir"
curl --fail --location \
  'https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2022.03.09/riscv64-elf-ubuntu-20.04-nightly-2022.03.09-nightly.tar.gz' \
  --output "$archive"
printf '%s  %s\n' \
  '6ec8ea11558f283aecd47c52a25c61a10c117ed703fee09c5a7dbfde3b522da1' \
  "$archive" | sha256sum --check
tar -xzf "$archive" -C "$toolchain_dir"
```

在新机器上，`dtc` 可按以下方式安装：

```bash
sudo apt-get update
sudo apt-get install -y device-tree-compiler
```

需要直接调用交叉编译器时：

```bash
export PATH="$PWD/target/try/gcc-riscv64-elf-2022.03.09/riscv/bin:$PATH"
```

构建 Linux 时改用：

```bash
export PATH="$PWD/target/try/gcc-riscv64-glibc-2022.03.09/riscv/bin:$PATH"
export CROSS_COMPILE=riscv64-unknown-linux-gnu-
```

重要：不要随意运行 `cargo clean`。整个 `target/` 被 Git 忽略，而手动安装的数 GiB 工具链、xv6/RustSBI/Linux checkout、rootfs、磁盘镜像和启动脚本也都位于 `target/try/`；`cargo clean` 会把它们一起删除。

## 构建与常用命令

构建已验证的 release CLI：

```bash
cargo +nightly-2024-09-05 build \
  --release --locked --package valheim-cli
```

查看 CLI 帮助：

```bash
./target/release/valheim-cli --help
```

也可以使用仓库的 Cargo alias：

```bash
cargo +nightly-2024-09-05 make
cargo +nightly-2024-09-05 start --kernel path/to/kernel.bin
```

不要用 README 中的旧写法：

```bash
cargo run --release -- --kernel ...
```

由于默认成员是 `xtask`，上述命令不会启动 `valheim-cli`。不使用 alias 时应明确指定包：

```bash
cargo +nightly-2024-09-05 run \
  --release --package valheim-cli -- \
  --kernel path/to/kernel.bin
```

### CLI 参数

- `--kernel` / `-k`：必填的 raw binary。
- `--bios` / `-b`：可选 raw BIOS；存在时 kernel 改为加载到 `0x8020_0000`。
- `--disk` / `-d`：可选 VirtIO block 镜像，以读写方式 mmap；guest 写入会直接修改文件。
- `--cmdline` / `-c`：替换 DTB 中的 bootargs。
- `--trace`：指定 trace 追加输出文件，但只有启用 `valheim-core/trace` feature 才生效。
- `--test`：按 `riscv-tests` 的 ECALL 约定运行并返回测试退出码。
- `--test-name`：测试输出中使用的名称。

直接运行 raw kernel：

```bash
./target/release/valheim-cli --kernel path/to/kernel.bin
```

运行 BIOS + kernel + 磁盘：

```bash
./target/release/valheim-cli \
  --bios path/to/bios.bin \
  --kernel path/to/kernel.bin \
  --disk path/to/rootfs.img \
  --cmdline 'root=/dev/vda ro console=ttyS0'
```

磁盘会被原地修改。手工运行未知 guest 前，先复制镜像，不要直接挂载唯一原件。

## 测试

Rust workspace 单元测试：

```bash
cargo +nightly-2024-09-05 test --workspace --locked
```

该命令已验证为 14 个测试通过、0 个失败：`valheim-asm` 10 个，`valheim-core` 4 个；后者包含 UART RX/TX 单次中断脉冲回归测试。

完整 RISC-V ISA 测试需要交叉工具链和 `riscv-tests` 子模块：

```bash
export PATH="$PWD/target/try/gcc-riscv64-elf-2022.03.09/riscv/bin:$PATH"
cargo +nightly-2024-09-05 run-riscv-tests
```

当前启用 96 个 ISA 测试、禁用 11 个浮点相关测试。`xtask` 会构建 debug CLI、配置并安装 `riscv-tests`、用 `objcopy` 将每个 ELF 转成 raw binary，再逐个运行。该流程没有单项超时，guest 卡死会使整个命令一直等待。

子模块 URL 是 GitHub SSH 地址。没有 GitHub SSH key 时使用 HTTPS 覆盖：

```bash
git -c \
  submodule.valheim-testing/riscv-tests.url=https://github.com/riscv-software-src/riscv-tests.git \
  submodule update --init --recursive
```

## Trace

`valheim-core` 的 `trace` feature 默认关闭。普通构建即使传 `--trace` 也不会记录。正确方式：

```bash
cargo +nightly-2024-09-05 run \
  --release \
  --package valheim-cli \
  --features valheim-core/trace \
  -- \
  --kernel path/to/kernel.bin \
  --trace valheim-trace.txt
```

不要把 `--features` 放在 `cargo start` alias 后；该 alias 已经包含 Cargo 的 `--`，额外参数会被当作 `valheim-cli` 参数。Trace 文件以追加模式打开。

## Demo 目录约定

本地工具链和 demo 必须分离：

```text
target/try/
├── gcc-riscv64-elf-2022.03.09/  # 多个 demo 可共享的工具链
├── gcc-riscv64-glibc-2022.03.09/ # Linux-target 共享工具链
├── linux/                        # Linux 5.17 + 官方 Debian 13 slim shell
│   ├── run.sh
│   ├── README.md
│   ├── downloads/
│   ├── overlay/                  # 只含启动用 /init 和设备节点清单
│   ├── source/                   # Linux 5.17 源码
│   └── build/                    # 官方 rootfs cpio 和 kernel Image
├── rustsbi/                      # 历史 RustSBI-QEMU test-kernel
│   ├── run.sh
│   ├── single-hart-valheim.patch
│   ├── valheim-dtb-pointer.patch
│   ├── source/
│   ├── artifacts/
│   └── runtime/last-run.log
└── xv6/
    ├── run.sh                    # 一键构建并启动
    ├── README.md
    ├── source/                   # 固定提交的 xv6 checkout 和构建产物
    └── runtime/fs.img            # guest 实际读写的磁盘副本
```

今后新增 demo 时，使用 `target/try/<demo-name>/`，在该目录内提供可从任意工作目录调用的 `run.sh`。不要把共享 GCC/Rust 工具链嵌套到某个 demo 目录中。每个脚本至少应：

1. 固定并检查上游版本。
2. 使用相对脚本自身位置计算仓库根目录。
3. 验证依赖，构建所需 guest 和 Valheim。
4. 使用独立的可写运行磁盘，避免修改唯一的原始镜像。
5. 给出明确的启动成功标志和退出方式。

注意：`target/` 在 `.gitignore` 中，因此这些本地 demo 文件不会出现在 `git status`，也不会随普通 Git commit 保存。

## 已验证的 xv6 Demo

从仓库根目录运行：

```bash
./target/try/xv6/run.sh
```

该脚本可从任意目录调用。它会：

1. 检查 `dtc`、Cargo、Git、Make 和固定位置的 RISC-V GNU 工具链。
2. 若 `target/try/xv6/source` 不存在，clone xv6-riscv 并 checkout 固定提交。
3. 构建 `kernel/kernel` 和 `fs.img`。
4. 用 `objcopy -O binary` 生成 `kernel/kernel.bin`。
5. 构建 `valheim-cli` release 版。
6. 使用 `runtime/fs.img` 启动 Valheim，最终进入交互式 xv6 shell。

预期启动末尾：

```text
xv6 kernel is booting

init: starting sh
$
```

2026-07-14 已实际验证：脚本进入 `$` 后，guest 内执行 `echo RUN_SH_OK` 和 `ls` 均成功。按 `Ctrl-C` 退出模拟器。

默认复用 `runtime/fs.img`，保留 guest 写入。需要丢弃运行期改动时：

```bash
RESET_DISK=1 ./target/try/xv6/run.sh
```

可选环境变量：

- `JOBS`：xv6 并行构建任务数。
- `VALHEIM_RUST_TOOLCHAIN`：默认 `nightly-2024-09-05`。
- `VALHEIM_RISCV_TOOLCHAIN`：默认 `target/try/gcc-riscv64-elf-2022.03.09`。

### 为什么固定这个 xv6 版本

固定提交：

```text
a1da53a5a12e21b44a2c79d962a437fa2107627c
```

Valheim 的 xv6 README 演示实际加入于 2022-03-19。当时 xv6-riscv 默认分支仍停在这个 legacy VirtIO 提交；要求 VirtIO-MMIO version 2 的改动直到 2022-08 才合入默认分支。这个提交的 `kernel/virtio_disk.c` 明确要求 version 1，正好匹配 Valheim。

其他兼容点：

- xv6 `_entry` 链接地址和 Valheim 无 BIOS 的加载地址都是 `0x8000_0000`。
- 构建出的 ISA 是 RV64GC，落在 Valheim 实现范围内。
- xv6 Makefile 中 `CPUS := 3` 只影响 QEMU 启动参数，不是编译宏；Valheim 单 hart 可以启动。
- RAM、CLINT、PLIC、UART、VirtIO 地址及 IRQ 与该版 xv6 匹配。

不要随意把 `source/` 更新到 xv6 最新分支；现代 xv6 会因为 VirtIO version 2 检查而无法在当前 Valheim 上启动。

## 已验证的 Debian 13 Linux Demo

从任意目录运行：

```bash
/home/kiva/git/valheim/target/try/linux/run.sh
```

或者在仓库根目录运行：

```bash
./target/try/linux/run.sh
```

脚本会固定并校验所有外部输入，复用 `target/try` 下的两个 GNU 工具链，构建 RustSBI、Linux 和 Valheim，然后进入真实 Debian Bash：

```text
Debian GNU/Linux 13 (trixie)
debian13#
```

可在 guest 中验证：

```bash
cat /etc/debian_version
cat /etc/os-release
uname -a
bash --version
id
```

2026-07-14 已实际验证交互输入输出：`/etc/debian_version` 为 `13.6`，Bash 为 `5.2.37(1)-release`，`coreutils` 为 `9.7-3`，glibc 为 `2.41-12+deb13u3`，`id` 显示 root；超过 80 字节的连续 UART 输出后仍能继续交互。完整验收日志在 `target/try/linux/runtime/last-run.log`。release 解释器在本机进入 shell 通常约需 1–3 分钟；第一次运行还要下载和构建，耗时更长。按宿主 `Ctrl-C` 退出。

固定版本和来源：

- userspace：Docker Official Image 的 `debian:13-slim` / `trixie-slim` riscv64 rootfs；脚本按不可变 OCI digest 从 Docker Hub 的 `library/debian` 下载。
- OCI index：`sha256:020c0d20b9880058cbe785a9db107156c3c75c2ac944a6aa7ab59f2add76a7bd`。
- OCI riscv64 manifest：`sha256:7244fbb388f7b59c9f584bb2bb7ef3a60b23aa1e55f1ad1d0641bd5ec12390f3`。
- rootfs layer：`sha256:3ed37bd5491de4685b6418abd6b83c4b16cc06b7a51e46da7f154c5a149a41a5`，内容为 Debian `13.6`。
- kernel：upstream Linux `v5.17`，tarball SHA-256 `555fef61dddb591a83d62dd04e252792f9af4ba9ef14683f64840e46fa20b1b1`。
- firmware：上文固定的 RustSBI-QEMU 2022-03 版本及 Valheim 适配 patch。

Linux `v5.17` 是根据项目 2022-03 的 demo 时间选择的同年代内核，不应误写成 README 历史 openEuler 录屏的原始内核；历史录屏使用的是 Linux `5.5.19` 和 OpenSBI `0.6`。

这不是手工伪造的 Debian rootfs。脚本原样下载并逐级校验官方 OCI index、riscv64 manifest 和 rootfs layer，在同一个 `fakeroot` 会话中解包和生成 cpio，从而保留层内 UID/GID、权限、符号链接和硬链接。由于 `slim` 容器层本来没有 init 系统且 `/dev` 为空，单独拼接的 initramfs overlay 只增加启动必需的 `/init` 和 `console/null/tty` 设备节点；`/init` 挂载 `devtmpfs`、`proc`、`sysfs`、`tmpfs` 和 `devpts` 后执行 rootfs 自带的 `/bin/bash`。Debian 的 Bash、glibc、coreutils、dpkg 数据库和 `/etc/os-release` 全部来自官方层。

根文件系统内建在 kernel Image 中，不经过当前有兼容限制的 VirtIO block。为了容纳完整官方层，Valheim RAM 已扩大为 256 MiB；为了让 Linux 的最终页表仍能访问设备树，FDT 位于 guest DRAM `0x87f0_0000`；UART 也补齐了 Linux 8250 serial 驱动依赖的寄存器和中断语义。

常用覆盖变量：

- `JOBS`：并行构建任务数。
- `VALHEIM_RUST_TOOLCHAIN`：默认 `nightly-2024-09-05`。
- `VALHEIM_LINUX_TOOLCHAIN`：默认 `target/try/gcc-riscv64-glibc-2022.03.09`。
- `VALHEIM_RISCV_TOOLCHAIN`：RustSBI 使用的 bare-metal GNU 工具链，默认 `target/try/gcc-riscv64-elf-2022.03.09`。

生成物放在 `target/try/linux/`：OCI metadata、rootfs layer 和 Linux tarball 在 `downloads/`，Linux 源码在 `source/`，合成的 cpio 和 kernel 输出在 `build/`。不要用 `cargo clean` 删除它们。

## 已验证的 RustSBI Demo

这项 demo 是有限运行的 SBI test kernel，不是 shell。从任意目录运行：

```bash
./target/try/rustsbi/run.sh
```

脚本会固定 RustSBI-QEMU 提交、应用有记录的单 hart 和 DTB 指针 patch、构建 BIOS 和 test kernel、复用共享 GNU objcopy 转 raw binary、构建 Valheim，然后运行并检查成功行。预期末尾：

```text
>> Hart 0 state return value: 0
<< Test-kernel: test for hart 0 success, wake another hart
<< Test-kernel: All hart SBI test SUCCESS, shutdown
```

完整输出保存在 `target/try/rustsbi/runtime/last-run.log`。2026-07-14 已从项目目录外执行脚本验证：退出码为 0，并检查到上述 success marker。

### RustSBI 历史版本依据

Valheim README 在 2022-03-20 首次加入 RustSBI 截图。当时 RustSBI-QEMU 默认分支 HEAD 是：

```text
999e3556fcfa1b0900dd797ae2186667af8d2dc6
```

它是 `v0.1.0-7-g999e355`：包版本仍为 RustSBI-QEMU 0.1.0，但依赖已经是截图中的 RustSBI 0.2.1，并报告 SBI spec 0.3。BIOS ELF 入口为 `0x8000_0000`，test-kernel ELF 入口为 `0x8020_0000`，与 Valheim 的 BIOS/kernel 加载地址完全一致。

构建固定使用 `nightly-2022-02-14`，并复用 `target/try/gcc-riscv64-elf-2022.03.09` 的 GNU objcopy。不要直接改回上游 `cargo make`：旧 xtask 会向 objcopy 传仅适合 `rust-objcopy` 的 `--binary-architecture=riscv64`，GNU Binutils 2.37 会拒绝它；当前脚本直接构建两个 package，再执行兼容的 `objcopy -O binary`。

### 为什么存在单 hart patch

Valheim 原截图不是未修改的上游 test kernel；原始 binary 也从未提交。上游 `999e355` 硬编码测试 hart 1–4，而 Valheim 只声明 hart 0。未修改版本已经实际诊断运行：BASE、time、非法指令转交和 hart 0 查询成功，随后固定停在：

```text
>> Wake hart 1, sbi return value 0
```

`target/try/rustsbi/single-hart-valheim.patch` 保留可在 hart 0 上运行的测试，并在 success marker 后执行 `ebreak`。这是退出适配：旧 RustSBI 原本通过 QEMU SiFive test finisher `0x0010_0000` 关机，Valheim 没有该设备。输出仍保留历史截图的 `All hart` 文本，但它只表示适用于单 hart 的测试通过，不能解释为实际测试了 hart 1–4。

该历史 firmware 在 Valheim 上未打 patch 实跑时，S-mode test kernel 观察到 `a1=0`。`target/try/rustsbi/valheim-dtb-pointer.patch` 将 supervisor 入口的 DTB 指针显式固定为当前 DRAM 地址 `0x87f0_0000`，因此 test kernel 和 Linux 都能得到有效 FDT。这个常量必须与 `Machine` 中的 `RV64_DTB_ADDR` 同步；它属于 Valheim 单 hart/启动布局适配，不能归因为已确认的上游通用 bug，也不能误报为未修改上游的逐字节复现。

Linux demo 只需要构建 firmware、不需要启动 test kernel，可用：

```bash
./target/try/rustsbi/run.sh --build-only
```

## openEuler Demo 状态

openEuler 演示没有固定的 BIOS、kernel、rootfs、下载脚本或版本 hash，README 还明确记录串口不能作为 `init` console。它只能作为历史启动成果，当前不能称为开箱即用 demo。

## 已知陷阱与维护注意事项

- README 中裸 `cargo run --release -- ...` 已过时，必须指定 `valheim-cli` 包或使用 `cargo start` alias。
- CLI 只读取 raw binary，不能把 ELF 直接传给 `--kernel` 或 `--bios`。
- 指定的 BIOS 文件读取失败时，当前 CLI 会打印错误，但随后把它当成“没有 BIOS”继续运行，并把 kernel 放到 `0x8000_0000`；诊断启动问题时要留意这一点。
- `dtc` 是运行时硬依赖。当前 DTB 生成代码丢弃 stderr 且不检查编译器退出状态，非法 `--cmdline` 可能只表现为损坏的 DTB 或不清楚的启动失败。
- `--disk` 会 mmap 并原地修改文件。
- `--trace` 默认无效，必须显式启用 feature。
- 测试和普通运行循环都没有 watchdog/超时，坏 guest 可能永久循环。
- 当前 VirtIO 是 legacy version 1；必须选择明确支持 legacy VirtIO-MMIO v1 的 guest 驱动，不能只根据 guest 的发布年份判断。
- RustSBI 历史 test kernel 的 success marker 来自明确记录的单 hart patch；不要声称未修改的上游多 hart HSM 测试在 Valheim 上完整通过。
- Debian 13 demo 的 rootfs 必须继续来自已固定并校验的官方 OCI artifact；不要用 BusyBox、手写 `/etc/os-release` 或自制目录树冒充 Debian。
- Linux 内核必须在同一个 `fakeroot` 元数据状态下打包 initramfs，否则 OCI 层中的 `root:shadow` 等属主信息会被宿主用户 UID 污染。
- Linux demo 使用内建 initramfs，成功不代表现代 Linux 的 VirtIO block 路径已兼容；切换到磁盘 rootfs 前必须单独修复和验证 VirtIO。
- UART RX/TX 目前以单次事件脉冲适配 Valheim 的简化 PLIC。xv6/Linux 的正常初始化顺序已验证；若字节恰在 PLIC source 10 被 mask 时到达，简化 PLIC 不会在之后 enable 时从 pending 重算 claim，事件可能暂时卡住。完整 level-triggered 语义需要连同 PLIC/SEIP 路径一起修复。
- DTB 先由 `Machine::new` 放到 `0x87f0_0000`，CLI 随后才加载 BIOS/kernel，当前没有镜像范围与 DTB overlap 检查；现有约 40 MiB Linux Image 安全，但不要传入会延伸到该地址的大型 raw image。
- `Memory::load` 当前只检查复制起点，bus 的 `*_END` 常量又按 exclusive end 计算却用于 inclusive match；不可信或超大镜像可能越界。修复边界检查前，只使用已校验且尺寸明确的 guest artifact。
- `profiler/profile.sh` 仍引用旧 binary 名称和缺失的测试镜像，并依赖 DTrace；使用前必须修正，不能把它作为已验证流程。
- Release profile 保留 debug symbols，这是性能分析和符号化所需的有意配置。

源码格式遵循 `.editorconfig`：UTF-8、LF、2 空格缩进，Rust 目标行宽 100。不要在无关任务中顺手升级 nightly、依赖或 guest 版本；该项目依赖旧 nightly feature 和旧硬件接口，升级需要单独验证。

## 修改后的最低验证要求

普通 Rust 改动至少运行：

```bash
cargo +nightly-2024-09-05 test --workspace --locked
cargo +nightly-2024-09-05 build --release --locked --package valheim-cli
```

修改 CPU、CSR、MMU、异常/中断、总线、UART、PLIC、CLINT、VirtIO、DTB 或 Machine 启动逻辑后，还应运行：

```bash
RESET_DISK=1 ./target/try/xv6/run.sh
./target/try/rustsbi/run.sh
./target/try/linux/run.sh
```

验收不是只看到 kernel banner。xv6 必须进入 `$`，Linux 必须进入 `debian13#`，并分别至少成功执行一个 guest 命令，例如：

```text
echo RUN_SH_OK
cat /etc/debian_version
```
