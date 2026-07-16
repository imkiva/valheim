# Valheim 项目与本地 Demo 指南

本文档面向后续在本仓库工作的开发者和自动化 Agent。它记录当前源码结构、运行约定、已经验证的工具版本以及本地 demo 的复现方法。根目录 `README.md` 中部分命令已经随 workspace 结构变化而过时；涉及构建和运行时，以本文档及源码为准。

## 项目概览

Valheim 是一个用 Rust 编写、以学习和参考实现为目的的 RISC-V 64 位系统模拟器，项目版本为 `0.2.0`。它作为宿主普通进程运行，可使用朴素解释器或分层 JIT 运行 guest，目标 ISA 是 RV64GC，并实现机器态、监管态和用户态所需的 CSR、异常、中断、分页以及一组 QEMU `virt` 风格设备。

当前实现的重要边界：

- 单 hart；设备树只声明 hart 0。
- 256 MiB guest RAM；Debian 13 demo 现从外部 ext4 block rootfs 启动，不再把完整
  rootfs 内嵌进 kernel。默认 rootfs 是固定 Debian 13 NoCloud riscv64 镜像中提取并适配的
  p1，并在首次生成 base 时通过宿主 chroot 从固定 Debian snapshot 预装基础开发环境；
  `--from-oci` 保留原有固定 slim OCI 路径。该内存规模供 Linux、page cache 和真实
  用户空间使用，与磁盘镜像容量无关。
- guest kernel/BIOS 必须是 raw binary，CLI 不解析 ELF。
- VirtIO block 是 legacy VirtIO-MMIO version 1，队列上限为 128，支持 direct 和协商后的
  indirect split-queue descriptor chain、读写、FLUSH 和 `SEG_MAX=126`；Linux 5.17 的
  legacy 驱动已通过 writable ext4 root 验证，要求 VirtIO-MMIO version 2 的 guest 驱动仍不兼容。
- VirtIO network 同样是 legacy VirtIO-MMIO version 1，使用各一个 RX/TX split queue、
  固定 MAC `52:54:00:12:34:56` 和队列上限 128。CLI 默认不接入 backend；显式
  `--net nat` 时由独立的非特权 `passt` 进程提供 IPv4 guest 出站 NAT。当前不支持
  IPv6、host→guest 入站连接或端口转发。
- CLI 默认执行器是 `NaiveInterpreter`；`--engine jit` 启用 decoded-TB + Cranelift 分层
  JIT。system/F/D 等无法 native lower 的指令作为 fallback TB 起点且完整 fetch/decode 后，
  negative cache 会保存完整 `GuestInst`；命中后直接调用共享 CPU 语义执行。native 路径可在
  一个 Machine budget 内批量执行多个 TB，并用 generation-guarded successor cache 连接常见边。
- JIT-enabled CLI 和完整 workspace 明确只支持 Linux x86_64 System V ABI；
  `valheim-core` 等不依赖 `valheim-jit` 的 crate 仍可单独构建。
- UART 直接连接宿主标准输入和标准输出，并实现 Linux 8250 驱动需要的 DLAB、IIR、RX/TX 中断和状态位。
- CLINT `mtime` 由宿主 monotonic clock 以 DTB 声明的 10 MHz 实时驱动；`TIME`
  CSR 是同一计数器的只读视图；MMIO 写 `mtime` 会重设 guest/host anchor，随后继续
  实时推进。完整 `Machine::run`/`run_for_test` 在 WFI 上按
  绝对 timer deadline 或 WakeHub 设备通知阻塞，宿主空闲时不忙轮询；公开的
  `run_next()` 仍是非阻塞单步接口。

## Workspace 与模块职责

根 `Cargo.toml` 包含六个成员，但 `default-members = ["xtask"]`。因此裸 `cargo run`
或 `cargo build` 默认操作的是 `xtask`，不是模拟器 CLI。

| 路径 | 职责 |
| --- | --- |
| `valheim-asm/` | RISC-V 指令数据结构、类型安全的寄存器/立即数表示、16/32 位指令解码与编码，以及相关单元测试。 |
| `valheim-core/` | 模拟器核心：CPU/寄存器、CSR、异常和中断、MMU、指令执行、解释器、内存总线、设备、DTB、运行循环和 trace。 |
| `valheim-core/src/cpu/` | CPU 状态、执行语义、CSR、异常/中断、系统总线，以及解释器/JIT 共用的唯一 `translate_to_host()` 页表与权限逻辑。 |
| `valheim-core/src/interp/` | 共享 `RV64Executor`/`ExecOutcome` 执行器契约与朴素解释器实现。 |
| `valheim-core/src/device/` | CLINT、level-aware PLIC、NS16550A UART、共用 legacy VirtIO-MMIO transport，以及支持 direct/indirect split queue 的 block/network frontend。 |
| `valheim-core/src/machine/` | 将 CPU、可注入执行器、DTB、UART、kernel、BIOS、磁盘和可选网络 backend 组合成可运行的虚拟机。 |
| `valheim-jit/` | decoded TB/cache/runtime、页内 fetch translation cache、Cranelift RV64I/M lowering、A 扩展 helper、software TLB 和 DRAM fast path。 |
| `valheim-cli/` | `valheim-cli` 命令行入口，负责参数解析和加载镜像。 |
| `valheim-net/` | 可替换的宿主网络策略层；当前实现 Linux `passt` backend、IPv4 subnet/address 规则和 helper 生命周期，未来纯 Rust NAT 可在这里替换。 |
| `xtask/` | 自动构建、转换并运行上游 `riscv-tests`。 |
| `dts/` | 启动时由 `dtc` 编译的设备树模板。 |
| `valheim-testing/` | 固定的 `riscv-tests` 子模块、启用/禁用测试列表和测试安装目录。 |
| `profiler/` | 旧的 DTrace/FlameGraph 性能分析脚本；当前不能直接开箱运行。 |
| `demo/` | Git 跟踪的三个最小 demo 入口：启动脚本、说明、RustSBI 适配 patch 和 Linux `/init`。 |
| `target/demo/` | demo 运行时下载或生成的工具链、Rustup/Cargo 状态、guest 源码、构建产物和日志；被 Git 忽略。 |

## Guest 运行模型与硬件布局

| 项目 | 当前约定 |
| --- | --- |
| Reset PC | `0x8000_0000` |
| 无 BIOS | raw kernel 加载到 `0x8000_0000` |
| 有 BIOS | raw BIOS 加载到 `0x8000_0000`，raw kernel 加载到 `0x8020_0000` |
| DTB | 带 32 字节前缀的副本仍位于 MROM `0x1000`；裸 FDT 另复制到 DRAM `0x87f0_0000`，guest 的 `a1/x11` 指向后者 |
| RAM | `256 MiB @ 0x8000_0000` |
| CLINT | `0x0200_0000`；10 MHz host-monotonic realtime `mtime` |
| PLIC | `0x0c00_0000`；M/S 两个 context，claim/complete 和 level re-pend |
| UART | `0x1000_0000`，IRQ 10 |
| VirtIO block | `0x1000_1000`，IRQ 1，legacy version 1 |
| VirtIO network | `0x1000_2000`，IRQ 2，legacy version 1；RX queue 0、TX queue 1，队列上限 128 |
| CLI 默认 kernel cmdline | `root=/dev/vda ro console=ttyS0`；Linux demo 显式覆盖为 ext4 `rw` |

`Machine::new` 每次都会根据 `dts/valheim.dts.template` 调用外部 `dtc` 生成 DTB。因此 `dtc` 不只是构建依赖，也是每次运行 CLI 和 ISA 测试时的依赖。DTB 必须复制到普通 DRAM：Linux 建立最终页表后不会继续映射 Valheim 的低地址 MROM，若只传旧地址 `0x1020`，内核会在切换页表后访问异常。

## 已验证的本机工具环境

最后一次完整验证日期：2026-07-16。

- Valheim Rust：`nightly-2024-09-05`。根 `rust-toolchain` 只写了不固定版本的 `nightly`，为了复现不要依赖它解析到的最新版本。
- 历史 RustSBI demo Rust：`nightly-2022-02-14`，已安装 target `riscv64imac-unknown-none-elf`。旧源码使用已从现代 Rust 删除的 generator API，不能改用新 nightly。
- Device Tree Compiler：通过 apt 安装的 `device-tree-compiler 1.6.1-1`。
- NoCloud host ext4 工具：脚本固定并自动构建 e2fsprogs `1.47.2`，使用源码
  自带的私有 libuuid/libblkid。Ubuntu 22.04 常见的宿主 e2fsprogs `1.46.5` 不
  识别固定 NoCloud p1 的 `orphan_file`/`FEATURE_C12`；不要用它替代脚本固定的
  NoCloud `e2fsck`/`debugfs`。OCI 路径仍使用宿主 e2fsprogs。
- NoCloud chroot 工具：脚本固定下载 Ubuntu
  `qemu-user-static 6.2+dfsg-2ubuntu6.31` 的 amd64 package，只提取并校验其中的
  static qemu-riscv64。base cache miss 时需要 mount/PID namespace、loop mount、
  `chroot` 和 `update-binfmts`，非 root 用户还需要 `sudo`；宿主内核必须支持 p1 的
  `orphan_file` ext4 feature，容器还必须提供 loop device 和 `CAP_SYS_ADMIN`。缓存命中
  与 `--from-oci` 不需要这条特权路径。
- Linux NAT helper：demo 固定 `passt 2026_06_11.a9c61ff`，snapshot URL 为
  `https://passt.top/passt/snapshot/passt-2026_06_11.a9c61ff.tar.xz`，SHA-256 为
  `b94b235cb96ce1b7aeab6552b7e0b4c9a780e5d700ced500c65e429b2d8b8450`。脚本只在
  `--net nat` 且未显式传 `--passt` 时下载并从源码构建；构建同时保留 `passt` 和
  x86_64 runtime dispatcher 需要的 `passt.avx2`，不需要 root、TAP 或 network namespace。
- RISC-V bare-metal GNU toolchain：
  `target/demo/gcc-riscv64-elf-2022.03.09/`
  - GCC `11.1.0 (g5964b5cd727)`
  - Binutils/objcopy `2.37`
  - 可执行文件目录：`target/demo/gcc-riscv64-elf-2022.03.09/riscv/bin`
  - 安装包 SHA-256：`6ec8ea11558f283aecd47c52a25c61a10c117ed703fee09c5a7dbfde3b522da1`
- RISC-V Linux GNU toolchain：
  `target/demo/gcc-riscv64-glibc-2022.03.09/`
  - GCC `11.1.0`，Binutils `2.37`
  - target prefix：`riscv64-unknown-linux-gnu-`
  - 可执行文件目录：`target/demo/gcc-riscv64-glibc-2022.03.09/riscv/bin`
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

demo 脚本会在首次运行时下载并校验上述固定工具链，然后解压到
`target/demo/` 下的共享目录。不要用 apt 安装交叉 GCC，也不要把工具链放进
某个 guest 的状态目录。

在新机器上，`dtc` 可按以下方式安装：

```bash
sudo apt-get update
sudo apt-get install -y device-tree-compiler
```

需要直接调用交叉编译器时：

```bash
export PATH="$PWD/target/demo/gcc-riscv64-elf-2022.03.09/riscv/bin:$PATH"
```

构建 Linux 时改用：

```bash
export PATH="$PWD/target/demo/gcc-riscv64-glibc-2022.03.09/riscv/bin:$PATH"
export CROSS_COMPILE=riscv64-unknown-linux-gnu-
```

重要：不要随意运行 `cargo clean`。整个 `target/` 被 Git 忽略，数 GiB 工具链、
Rustup/Cargo 缓存、xv6/RustSBI/Linux checkout、rootfs 和磁盘镜像都位于
`target/demo/`；`cargo clean` 会把它们一起删除。`demo/` 中被 Git 跟踪的入口文件不受影响。

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

上述命令默认使用 naive；在支持的 Linux x86_64 SysV 宿主上可显式选择 JIT：

```bash
cargo +nightly-2024-09-05 start \
  --engine jit \
  --kernel path/to/kernel.bin
```

### CLI 参数

- `--kernel` / `-k`：必填的 raw binary。
- `--bios` / `-b`：可选 raw BIOS；存在时 kernel 改为加载到 `0x8020_0000`。
- `--disk` / `-d`：可选 VirtIO block 镜像，以读写方式 mmap；guest 写入会直接修改文件。
- `--cmdline` / `-c`：替换 DTB 中的 bootargs。
- `--net none|nat`：选择网络模式，默认 `none`；`nat` 在 Linux 宿主启动一个独立
  `passt` backend，只提供 guest IPv4 出站连接。
- `--net-subnet CIDR`：NAT 的 canonical RFC 1918 IPv4 network，默认
  `10.172.0.0/16`，最窄 `/27`；guest、gateway 和 DNS 固定为 network 加 15、2、3。
  只有 `--net nat` 时可用。
- `--passt PATH`：NAT 使用的 `passt` 可执行文件；默认从 `PATH` 查找 `passt`，只有
  `--net nat` 时可用。
- `--trace`：指定 trace 追加输出文件，但只有启用 `valheim-core/trace` feature 才生效。
- `--test`：按 `riscv-tests` 的 ECALL 约定运行并返回测试退出码。
- `--test-name`：测试输出中使用的名称。
- `--engine naive|jit`：选择执行器，默认 `naive`。
- `--jit-hot-threshold`：TB 在 decoded 层成功执行多少次后编译，默认 750，最小 1。
- `--jit-max-block-len`：每个 TB 的最大 guest 指令数，默认 32，有效范围 1–32。
- `--jit-max-compiled-blocks`：每个 Cranelift module 的函数上限，默认 4096。
- `--jit-max-code-bytes`：所有存活 module 的机器码总上限，默认 134217728
  bytes（128 MiB）；到达上限后在 dispatcher 安全点整体换代。
- `--jit-stats`：启用 JIT 统计输出。
- `--jit-stats-interval`：统计输出间隔，默认 1000000 次 dispatcher，只在
  `--jit-stats` 时生效。

任何 runtime `--trace` 参数或编译期 `valheim-core/trace` feature 都会将请求的 JIT
强制切换为 naive，并在 stderr 给出提示。

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

需要网络的 guest 还必须启用 legacy VirtIO network、IPv4 并自行使用 DHCP 或固定地址；
CLI 只负责接入设备和 backend。使用默认地址布局启动出站 NAT：

```bash
./target/release/valheim-cli \
  --bios path/to/bios.bin \
  --kernel path/to/kernel.bin \
  --disk path/to/rootfs.img \
  --cmdline 'root=/dev/vda rw console=ttyS0 ip=dhcp' \
  --net nat
```

磁盘会被原地修改。手工运行未知 guest 前，先复制镜像，不要直接挂载唯一原件。

## 测试

Rust workspace 单元测试：

```bash
cargo +nightly-2024-09-05 test --workspace --locked
```

该命令已验证为 248 个测试通过、0 个失败：`valheim-asm` 11 个、
`valheim-cli` 5 个、`valheim-core` 129 个、`valheim-net` 28 个，
`valheim-jit` 56 个 unit + 11 个 native/naive differential + 4 个 memory fast-path
integration tests，`xtask` 4 个。额外的 trace 语义回归为：

```bash
cargo +nightly-2024-09-05 test \
  --locked --package valheim-core --features trace
```

该命令已验证 130 个测试通过。

完整 RISC-V ISA 测试需要交叉工具链和 `riscv-tests` 子模块：

```bash
export PATH="$PWD/target/demo/gcc-riscv64-elf-2022.03.09/riscv/bin:$PATH"
cargo +nightly-2024-09-05 run-riscv-tests -- --engine naive
cargo +nightly-2024-09-05 run-riscv-tests -- --engine jit
```

当前启用 96 个 ISA 测试、禁用 11 个浮点相关测试；naive 和 JIT 均已验证
96/96。`xtask` 默认使用 naive；JIT 测试自动传入 hot threshold 1，以确保覆盖
native 编译路径。它会构建 debug CLI、配置并安装 `riscv-tests`、用 `objcopy`
将每个 ELF 转成 raw binary，再逐个运行。该流程没有单项超时，guest 卡死会使
整个命令一直等待。

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

不要把 `--features` 放在 `cargo start` alias 后；该 alias 已经包含 Cargo 的 `--`，额外参数会被当作 `valheim-cli` 参数。Trace 文件以追加模式打开。JIT 暂不支持逐指令 trace；
`--engine jit --trace ...` 或启用 trace feature 的构建会提示并使用 naive。

## Demo 目录约定

可提交的入口和可重建的运行状态必须分离：

```text
demo/                                # Git 跟踪；只放最小静态入口
├── xv6/                           # run.sh + README.md
├── rustsbi/                       # run.sh + README.md + 三个适配 patch
└── linux/                         # run.sh + chroot helper + README.md + init

target/demo/                         # Git 忽略；由 run.sh 创建
├── gcc-riscv64-elf-2022.03.09/   # xv6/RustSBI 共享 GNU 工具链
├── gcc-riscv64-glibc-2022.03.09/ # Linux-target 共享 GNU 工具链
├── rustup/                        # 隔离的 Rustup home
├── cargo-home/                    # 隔离的 Cargo home
├── cargo/                         # Valheim Cargo target 输出
├── xv6/                           # 源码、构建产物和可写磁盘
├── rustsbi/                       # 源码、RustSBI Cargo target、artifact 和日志
└── linux/                         # 下载、源码、host-tools、NoCloud/OCI ext4 和 kernel
```

今后新增 demo 时，使用 `demo/<demo-name>/run.sh` 作为可从任意工作目录调用的
入口，把所有下载和生成内容放到 `target/demo/<demo-name>/` 或 `target/demo/` 下的共享目录。
不要把共享 GCC/Rust 工具链嵌套到某个 guest 状态目录中。每个脚本至少应：

1. 固定并检查上游版本。
2. 使用相对脚本自身位置计算仓库根目录。
3. 验证依赖，构建所需 guest 和 Valheim。
4. 使用独立的可写运行磁盘，避免修改唯一的原始镜像。
5. 给出明确的启动成功标志和退出方式。

注意：`target/` 在 `.gitignore` 中，因此只有 `demo/` 中的静态文件会随 Git commit
保存；脚本的下载与构建产物不应出现在 `git status` 中。

三个 demo 的 `run.sh` 都会把额外参数透传给 `valheim-cli`，无参数时默认注入
`--engine jit`。显式 `--engine naive` 或 `--engine=naive` 可覆盖该默认值；以下形式仍可用于
明确记录 JIT 验证：

```bash
RESET_DISK=1 ./demo/xv6/run.sh --engine jit
./demo/rustsbi/run.sh --engine jit
./demo/linux/run.sh --engine jit
```

2026-07-15 已实际验证三个 demo 的 naive 和 JIT 两种 engine：xv6 进入 `$` 并执行
`echo`，RustSBI 输出 success marker，当时的 Debian slim OCI 路径（现为
`--from-oci`）从 `/dev/vda` 的 read-write ext4 进入 `debian13#`、读取版本
`13.6` 并写入文件。Debian JIT 还验证了 `sync` 后跨进程持久化、日志恢复和
`RESET_DISK=1` 重置。这些是 OCI 模式的历史验收事实，不应自动外推为新默认
NoCloud 模式的验收结果。

## 已验证的 xv6 Demo

从仓库根目录运行：

```bash
./demo/xv6/run.sh
```

该脚本可从任意目录调用。它会：

1. 检查宿主依赖，并按需安装固定的 RISC-V GNU 工具链和 Rust nightly。
2. 若 `target/demo/xv6/source` 不存在，clone xv6-riscv 并 checkout 固定提交。
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
RESET_DISK=1 ./demo/xv6/run.sh
```

可选环境变量：

- `JOBS`：xv6 并行构建任务数。
- `VALHEIM_RUST_TOOLCHAIN`：默认 `nightly-2024-09-05`。
- `VALHEIM_RISCV_TOOLCHAIN`：默认 `target/demo/gcc-riscv64-elf-2022.03.09`。

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

## Debian 13 Linux Demo

从任意目录运行：

```bash
/path/to/valheim/demo/linux/run.sh
```

或者在仓库根目录运行：

```bash
./demo/linux/run.sh
```

默认使用固定的 Debian 13 NoCloud riscv64 cloud image；要运行原有的精简
Docker Official Image 回归口径，传入由 demo 脚本消费、不透传给 CLI 的
`--from-oci`：

```bash
./demo/linux/run.sh --from-oci
```

网络默认关闭；显式启用 IPv4 guest 出站 NAT：

```bash
./demo/linux/run.sh --net nat
./demo/linux/run.sh --net nat --net-subnet 10.173.0.0/16
./demo/linux/run.sh --net nat --passt /absolute/path/to/passt
```

默认 subnet 是 `10.172.0.0/16`，guest、gateway、DNS 分别固定为
`10.172.0.15`、`10.172.0.2`、`10.172.0.3`，MAC 固定为
`52:54:00:12:34:56`。每个 Valheim/`passt` 实例拥有独立的 userspace network
状态，因此并行实例可复用同一组 guest-visible 地址。`--net-subnet` 只接受完全位于
RFC 1918 地址空间的 canonical network，且不能窄于 `/27`；三个固定地址仍分别使用
network 加 15、2、3。`run.sh` 只在使用脚本默认 cmdline 时为 NAT 自动追加
`ip=dhcp`；显式 `--cmdline`/`-c` 后调用者必须自行保留 DHCP 或配置静态网络。

脚本会固定并校验选中来源及其他所有外部输入，复用 `target/demo` 下的两个
GNU 工具链，构建 RustSBI、Linux 和 Valheim，然后进入真实 Debian Bash：

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

2026-07-14 已实际验证当时的 slim OCI 路径（现用 `--from-oci` 选择）的交互输入
输出：`/etc/debian_version` 为 `13.6`，Bash 为 `5.2.37(1)-release`，`coreutils`
为 `9.7-3`，glibc 为 `2.41-12+deb13u3`，`id` 显示 root；超过 80 字节的连续
UART 输出后仍能继续交互。第一次运行还要下载和构建，耗时更长。按宿主
`Ctrl-C` 退出。除非另有新的实际运行记录，不得把这些版本和交互结果改写为
默认 NoCloud 路径的已验证结果。

2026-07-16 已实际验证当前 IPv4 NAT：OCI 路径分别用 release naive/JIT 启动，默认
NoCloud 路径用 release JIT 启动；Linux 5.17 均发现 `virtio1`，DHCP 获得
`10.172.0.15/16`，默认路由和 resolver 分别指向 `10.172.0.2`、`10.172.0.3`，公网
IPv4 DNS、HTTP/TCP 出站与 network IRQ 增长均通过，guest 未启用 IPv6。OCI 还验证了
自定义 `10.173.0.0/16`、默认离线启动；两个来源都验证 `sync` 后跨进程持久化及各自的
`RESET_DISK=1` 清除。NoCloud 保留原有 resolver symlink，并在联网 guest 中再次验证
392-package profile、`dpkg --audit` 和 GCC 编译运行。guest 没有 RTC、启动时间为 1970，
所以未手工校时前 HTTPS 证书时间校验失败不属于网络故障；基础联网验收使用 HTTP。
另以 OCI/NoCloud 两个 guest 并行启动验证：两个隔离实例可以同时复用默认地址，均完成
DNS 与 HTTP/TCP 出站，宿主没有 Valheim/passt listener，退出后没有遗留 helper。

2026-07-15 在开发包扩展前，已实际用默认 NoCloud 路径和 release JIT 验证原始
271-package 适配版：GPT/p1、固定 e2fsprogs 1.47.2、read-write ext4、基础工具、
journal recovery 和 `sync` 后跨进程持久化均通过。

同日又实际完成当前 development profile 的首次构建与 release JIT 验收：宿主
chroot 从固定、签名校验的 Debian snapshots 安装 121 个新包、0 个升级；`dpkg --audit`
为空，APT 模拟升级仍为 0，宿主 qemu chroot 和真实 Valheim guest 都成功用 GCC
编译并运行最小 C 程序。最终 392 个 dpkg 条目全部为 `install ok installed`，排序后的
`package/architecture/version` manifest SHA-256 为
`051c1d9de5b0b1eb38442edcab296835363fcdbbe806f4335888de1e11d6672b`。
guest 中 GCC target 为 `riscv64-linux-gnu`，Git 为 2.47.3，pip 为 25.1.1，且
GCC/G++/make、Git、CMake/Ninja/Meson、Autotools、GDB/strace、jq/rsync 和 Python
开发命令均已验证可执行；第二次启动复用 base/runtime 并在 journal recovery 后进入
prompt。仍没有把 NoCloud naive 或性能数据写成已验证事实。

2026-07-15 第一阶段在同一 release binary 与已构建 artifact 上，从宿主进程启动到真实行末
`debian13# ` 各测三次：naive 中位数 69.603 s，JIT 中位数 13.447 s，加速 5.176×；设计与
第一阶段数据见根 `JIT-PLAN.md`。后续性能提交在固定 CPU 16 上重测
realtime 切换前的性能树：naive 中位数 60.926 s，JIT 中位数 4.806 s，
加速 12.677×。`10cabc6`/`258fdf6` 后 realtime JIT 三次中位数为
8.379352 s；realtime naive 单次验收为 133.996711 s（不是三次中位数）。
这些历史数据都来自内建 initramfs，不能作为任一 ext4 block-root 的性能基线；旧时钟还会
快进 guest 等待，新旧绝对时间也不可直接对比。逐项交错 A/B、profile 和剩余方向见
`JIT-PERF.md`。

既有 slim OCI ext4 direct-root 历史口径固定 CPU 16、显式 threshold 750、关闭
stats，并为每次启动复制 base image 得到全新的可写副本。`ded32a0` 与第一轮
终点 `746684b` 各三次交错结果的中位数
分别为 3.957267 s 和 3.371372 s，第一轮累计缩短 14.806%（1.1738×）。这是本轮 VirtIO、PLIC 和
JIT 第一轮优化的累计结果，不能归因给任一单独 commit。后续独立 A/B 中，共享 core
translation cache 将 prompt 中位数从 2.481824 s 降至 2.215396 s（-10.735%），跳过非
`mtime` 写入的 host clock sample 为 -2.487%；VirtIO indirect descriptors 的 prompt 和
64 MiB 吞吐均为中性。逐项数据、已否决实验和剩余方向见 `JIT-PERF.md`。

固定版本和来源：

- 默认 userspace：Debian 官方 NoCloud riscv64 build `20260712-2537`，下载 URL 为
  `https://cloud.debian.org/images/cloud/trixie/20260712-2537/debian-13-nocloud-riscv64-20260712-2537.tar.xz`，
  SHA-512 为
  `65f4c937175e6f096e697f671b8bbd745f1a6025f610343e1d00bfd4e0bbe475b27a8cc77c0f22f83499242d78be2070769c24da11ee4c73e37072eab8659783`。
  同目录 package metadata
  `debian-13-nocloud-riscv64-20260712-2537.json` 的 SHA-512 为
  `023206bfb347bc1f2c1b64ae1d42f7c1ccc2b42fb48b7f2bdd2e8c7750d06bdee0ffc2248d5df054d40803aefc8abc2d5318098fee42202d41dbfe1eb6b28412`。
  archive 内是带 GPT 的 sparse `disk.raw`；脚本从 p1 取出 root ext4，加入 Valheim
  启动所需的 `/init` 和早期 console 节点，再把该分区作为无分区表的
  `/dev/vda`。不要把整个 `disk.raw` 直接传给 Valheim。
- NoCloud development packages：main/updates 固定为 Debian snapshot
  `20260712T202631Z`，security 固定为 `20260712T194830Z`；只使用 `main`、保留
  Debian archive keyring 签名校验，并为历史 snapshot 设置
  `Check-Valid-Until: no`。显式顶层包列表和完整 392-package manifest 都属于 schema。
- NoCloud chroot static qemu：Ubuntu amd64 package
  `qemu-user-static_6.2+dfsg-2ubuntu6.31_amd64.deb`，SHA-256 为
  `2d22939f98f2ee8b84c5cc53b01082a4a937cfc7b4a8aa432788b9eaf4a14a41`；提取出的
  `qemu-riscv64-static` SHA-256 为
  `ee063e5feaae2475b1eabe82ead98574c94fbbdbf6e0131379ea686ab6e3b437`。
- NAT helper：`passt 2026_06_11.a9c61ff`，snapshot SHA-256 为
  `b94b235cb96ce1b7aeab6552b7e0b4c9a780e5d700ced500c65e429b2d8b8450`；源码和构建
  cache 分别位于 `host-tools/passt-2026_06_11.a9c61ff-source` 和
  `host-tools/passt-2026_06_11.a9c61ff-build`，build schema 是
  `v2:version=2026_06_11.a9c61ff:sha256=b94b235cb96ce1b7aeab6552b7e0b4c9a780e5d700ced500c65e429b2d8b8450:x86_64-avx2-dispatch`。
- 可选 userspace：`--from-oci` 使用 Docker Official Image 的 `debian:13-slim` /
  `trixie-slim` riscv64 rootfs；脚本按不可变 OCI digest 从 Docker Hub 的
  `library/debian` 下载。
- OCI index：`sha256:020c0d20b9880058cbe785a9db107156c3c75c2ac944a6aa7ab59f2add76a7bd`。
- OCI riscv64 manifest：`sha256:7244fbb388f7b59c9f584bb2bb7ef3a60b23aa1e55f1ad1d0641bd5ec12390f3`。
- OCI rootfs layer：`sha256:3ed37bd5491de4685b6418abd6b83c4b16cc06b7a51e46da7f154c5a149a41a5`，
  内容为 Debian `13.6`。
- NoCloud host ext4 工具：e2fsprogs `1.47.2`，tarball URL 为
  `https://cdn.kernel.org/pub/linux/kernel/people/tytso/e2fsprogs/v1.47.2/e2fsprogs-1.47.2.tar.xz`，
  SHA-256 为 `08242e64ca0e8194d9c1caad49762b19209a06318199b63ce74ae4ef2d74e63c`。
- kernel：upstream Linux `v5.17`，tarball SHA-256 `555fef61dddb591a83d62dd04e252792f9af4ba9ef14683f64840e46fa20b1b1`。
- firmware：上文固定的 RustSBI-QEMU 2022-03 版本及 Valheim 适配 patch；
  SBI TIME `set_timer` 会首次建立并在后续重装 machine-timer→STIP 中继。

Linux `v5.17` 是根据项目 2022-03 的 demo 时间选择的同年代内核，不应误写成 README 历史 openEuler 录屏的原始内核；历史录屏使用的是 Linux `5.5.19` 和 OpenSBI `0.6`。

两种模式都直接使用 Debian 官方 artifact，不是 BusyBox、手写
`/etc/os-release` 或自制目录树。NoCloud p1 原本就是 Debian 构建的 ext4；脚本只把
尚未晋升的临时 p1 副本挂入私有 mount namespace，用 qemu-riscv64 binfmt + chroot
安装固定开发包，卸载后再以 `debugfs` 接入仓库 `/init` 和控制台节点，并用固定构建的
e2fsprogs 1.47.2 验收其 `orphan_file` ext4 feature。下载 archive、最终 base 和
runtime 不作为安装目标。OCI 模式则原样下载并逐级校验官方
index、riscv64 manifest 和 rootfs layer，在同一个 `fakeroot` 会话中解包、加入
最小 overlay 并调用 `mke2fs -d`，从而保留层内 UID/GID、权限、符号链接、
硬链接和设备节点。`/init` 确认 `/` 是 read-write ext4，挂载 `devtmpfs`、
`proc`、`sysfs`、`tmpfs` 和 `devpts` 后执行 rootfs 自带的 `/bin/bash`。

NoCloud 官方输入有 271 个 dpkg package；固定 chroot profile 新增 121 个，最终为
392 个。除 `procps`/`psmisc`、Nano/Vim、Python 3、`man`、压缩和文件系统工具外，
默认还包含 build-essential、GCC/G++/make、Binutils、Git、CMake/Ninja/Meson、
Autotools、Bison/Flex、GDB/strace/lsof、jq/rsync、Python headers/pip/venv 等基础
开发环境。
demo kernel 将 `CONFIG_NET`、`CONFIG_PACKET`、`CONFIG_UNIX`、`CONFIG_INET`、
kernel DHCP 和 `CONFIG_VIRTIO_NET` 直接编进 `Image`，并明确关闭 `CONFIG_IPV6`。
脚本默认仍离线启动；显式 `--net nat` 时才构建/启动 `passt` 并在默认 cmdline 加
`ip=dhcp`。此时 NoCloud/OCI 中的 `curl`、wget、Git 和 APT 可使用 IPv4 出站连接，
但实际 package 可用性仍受镜像 sources 和固定 snapshot 状态约束。当前没有
host→guest 入站、端口转发或 IPv6。

两个来源拥有彼此独立的只读 base 和持久化可写 runtime；kernel 不再内嵌
rootfs，而是用 built-in VirtIO block/ext4 驱动直接挂载选中 runtime 为
`/dev/vda`。`RESET_DISK=1` 只会用选中来源的 base 重置它自己的 runtime；
不会删除或覆盖另一来源的 guest 写入。base、runtime 与实际 disk inode 都有
防误用校验或 nonblocking `flock`。DHCP resolver 接入改变了共享 `/init` 的运行语义，
因此 NoCloud schema 已升为 v3、OCI schema 已升为 v2；过去仅为来源中性 banner
变更接受旧 OCI schema 的兼容例外已移除。任一来源的旧 runtime 都不会被静默覆盖，
必须先备份再为同一来源显式 `RESET_DISK=1`。重置只复制已验证 base，不会重复联网
安装。guest `sync` 后写入已在 OCI 历史路径验证可跨进程和 journal recovery 保留。
`/init` 只在 `/proc/net/pnp` 明确报告 DHCP nameserver 时写 resolver：NoCloud 的
`/etc/resolv.conf` 保持 symlink，并创建其固定 target
`/run/systemd/resolve/stub-resolv.conf`；OCI 则更新原有 regular
`/etc/resolv.conf`。离线启动不改写 resolver。FDT 仍位于 guest DRAM
`0x87f0_0000`，UART 继续作为 Linux 8250 console。

常用覆盖变量：

- `JOBS`：并行构建任务数。
- `--from-oci`：使用原 slim OCI 路径；该参数由 `run.sh` 消费，不是
  `valheim-cli` 参数。
- `--net nat`：启用 IPv4 guest 出站 NAT；省略时网络关闭。demo 只在该模式且未覆盖
  helper 时准备固定 `passt`。
- `--net-subnet CIDR`：覆盖默认 `10.172.0.0/16`；只接受 canonical RFC 1918
  network，最窄 `/27`，且要求 `--net nat`。
- `--passt PATH`：用现有 `passt` binary 替代 demo 固定构建，要求 `--net nat`。
- `RESET_DISK=1`：从当前选中来源的只读 base 重建它自己的默认可写
  ext4 runtime；默认复用该来源现有的 guest 写入。
- `VALHEIM_RUST_TOOLCHAIN`：默认 `nightly-2024-09-05`。
- `VALHEIM_LINUX_TOOLCHAIN`：默认 `target/demo/gcc-riscv64-glibc-2022.03.09`。
- `VALHEIM_RISCV_TOOLCHAIN`：RustSBI 使用的 bare-metal GNU 工具链，默认 `target/demo/gcc-riscv64-elf-2022.03.09`。

生成物放在 `target/demo/linux/`：NoCloud archive、OCI metadata/rootfs layer、
e2fsprogs 1.47.2 和 Linux tarball 在 `downloads/`，Linux 源码在 `source/`，NoCloud 专用
e2fsprogs 源码/build 缓存分别在 `host-tools/e2fsprogs-1.47.2-source` 和
`host-tools/e2fsprogs-1.47.2-build`，static qemu 缓存在
`host-tools/qemu-user-static-6.2+dfsg-2ubuntu6.31`，固定 passt 源码/build 缓存分别在
`host-tools/passt-2026_06_11.a9c61ff-source` 和
`host-tools/passt-2026_06_11.a9c61ff-build`。NoCloud/OCI 只读 ext4 base 与 kernel
输出在 `build/`；base 分别为 `build/debian-13-nocloud-riscv64.ext4`
和 `build/debian-13-slim-riscv64.ext4`，两个来源的可写 runtime 分别为
`runtime/nocloud-rootfs.ext4` 和保留旧路径的 OCI `runtime/rootfs.ext4`。archive 内的
整个 sparse
`disk.raw` 只是 p1 抽取的输入，不作为第三份可写 runtime 保留。不要用
`cargo clean` 删除这些缓存。

## 已验证的 RustSBI Demo

这项 demo 是有限运行的 SBI test kernel，不是 shell。从任意目录运行：

```bash
./demo/rustsbi/run.sh
```

脚本会固定 RustSBI-QEMU 提交，应用有记录的单 hart、DTB 指针和 timer
relay 三个 patch，构建 BIOS 和 test kernel，复用共享 GNU objcopy 转 raw binary，
构建 Valheim，然后运行并检查成功行。预期末尾：

```text
>> Hart 0 state return value: 0
<< Test-kernel: test for hart 0 success, wake another hart
<< Test-kernel: All hart SBI test SUCCESS, shutdown
```

完整输出保存在 `target/demo/rustsbi/runtime/last-run.log`。2026-07-15 已在当前
三个 patch 上分别以 naive/JIT 验证：退出码为 0，并检查到上述 success marker。

### RustSBI 历史版本依据

Valheim README 在 2022-03-20 首次加入 RustSBI 截图。当时 RustSBI-QEMU 默认分支 HEAD 是：

```text
999e3556fcfa1b0900dd797ae2186667af8d2dc6
```

它是 `v0.1.0-7-g999e355`：包版本仍为 RustSBI-QEMU 0.1.0，但依赖已经是截图中的 RustSBI 0.2.1，并报告 SBI spec 0.3。BIOS ELF 入口为 `0x8000_0000`，test-kernel ELF 入口为 `0x8020_0000`，与 Valheim 的 BIOS/kernel 加载地址完全一致。

构建固定使用 `nightly-2022-02-14`，并复用 `target/demo/gcc-riscv64-elf-2022.03.09` 的 GNU objcopy。不要直接改回上游 `cargo make`：旧 xtask 会向 objcopy 传仅适合 `rust-objcopy` 的 `--binary-architecture=riscv64`，GNU Binutils 2.37 会拒绝它；当前脚本直接构建两个 package，再执行兼容的 `objcopy -O binary`。

### 为什么需要三个适配 patch

Valheim 原截图不是未修改的上游 test kernel；原始 binary 也从未提交。上游 `999e355` 硬编码测试 hart 1–4，而 Valheim 只声明 hart 0。未修改版本已经实际诊断运行：BASE、time、非法指令转交和 hart 0 查询成功，随后固定停在：

```text
>> Wake hart 1, sbi return value 0
```

`demo/rustsbi/single-hart-valheim.patch` 保留可在 hart 0 上运行的测试，并在 success marker 后执行 `ebreak`。这是退出适配：旧 RustSBI 原本通过 QEMU SiFive test finisher `0x0010_0000` 关机，Valheim 没有该设备。输出仍保留历史截图的 `All hart` 文本，但它只表示适用于单 hart 的测试通过，不能解释为实际测试了 hart 1–4。

该历史 firmware 在 Valheim 上未打 patch 实跑时，S-mode test kernel 观察到 `a1=0`。`demo/rustsbi/valheim-dtb-pointer.patch` 将 supervisor 入口的 DTB 指针显式固定为当前 DRAM 地址 `0x87f0_0000`，因此 test kernel 和 Linux 都能得到有效 FDT。这个常量必须与 `Machine` 中的 `RV64_DTB_ADDR` 同步；它属于 Valheim 单 hart/启动布局适配，不能归因为已确认的上游通用 bug，也不能误报为未修改上游的逐字节复现。

`demo/rustsbi/valheim-time-relay.patch` 修复该历史 firmware 的 SBI TIME 中继首次建立和
后续重装路径：machine-timer trap 会设置 STIP 并关闭 MTIE，每次 `set_timer` 必须在替换
`mtimecmp` 后清 STIP、重新打开 MTIE。去掉该 patch 时，当前初始化路径不会打开 MTIE，
Linux 无法可靠获得 MTIP→STIP timer relay；RustSBI test kernel 本身不覆盖该路径，必须用
Linux `sleep`/`/proc/interrupts` 验收。

Linux demo 只需要构建 firmware、不需要启动 test kernel，可用：

```bash
./demo/rustsbi/run.sh --build-only
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
- JIT 暂不支持完整逐指令 trace；请求 JIT 同时传 `--trace` 或启用 trace feature
  时会强制使用 naive。
- Cranelift IR verifier 在启用 debug assertions 的构建（包括默认 `cargo test`）中开启，
  在所有 release 构建（包括 release tests）中为减少每个 TB 的编译成本而关闭。修改 lowering、
  helper ABI 或 native exit 时，不能只跑默认 debug tests；必须补 release JIT tests、JIT ISA
  和三个 release JIT demo，防止 malformed IR 在 release 中变成 panic 或错误机器码。
- `fetch_mem()` 必须先取首个 16-bit parcel，并且只在确认为 32-bit 指令时取第二个；
  跨页时两个 parcel 必须分别以 Fetch 权限翻译。不能退回“翻译一次再读 u32”，否则会
  绕过第二页映射、X 权限及 `PC+2` fault address。
- 页表 walker 的隐式 PTE read/A-D write 和最终物理 endpoint access fault 必须按原始
  Fetch/Read/Write 类型报告 guest VA；跨页数据访问应报告实际故障 fragment 的 VA，不能
  把页表或 endpoint 的物理地址泄漏进 `mtval/stval`。解释器与 JIT slow path 共用该不变量。
- 测试和普通运行循环都没有 watchdog/超时，坏 guest 可能永久循环。
- 当前 block/network 都是 legacy VirtIO-MMIO version 1，QueueNumMax 为 128；仍必须
  选择明确支持 legacy v1 的 guest 驱动，不能只根据 guest 的发布年份判断。block 公布
  FLUSH、SEG_MAX（126）和 `VIRTIO_F_RING_INDIRECT_DESC`；network 公布固定 MAC 和
  `VIRTIO_F_RING_INDIRECT_DESC`，使用 non-mergeable 10-byte network header、RX queue 0
  和 TX queue 1。两者都接受 direct 以及协商后的 indirect descriptor chain。
- 离线模式仍在 DTB 中保留 `0x1000_2000` 的 VirtIO-MMIO node，但 network frontend 的
  DeviceID 为 0；只有 backend 成功启动后才呈现 DeviceID 1。backend worker 只能通过有界
  channel 传完整 Ethernet frame，并用 WakeHub 唤醒 WFI；不得从 worker 线程直接访问 guest
  memory。frontend 当前只接受 MTU 1500 所需的 frame，额外允许一个 802.1Q tag 后的
  1518-byte Ethernet frame。
- `valheim-net` 当前使用 passt 的 QEMU frame-stream 协议（4-byte big-endian length 加完整
  Ethernet frame）。启动时 passt 会关闭除 `--fd` 外的继承 fd，因此 readiness 不能使用普通
  inherited pipe；现有实现让 passt 通过 `/proc/<valheim-pid>/fd/<memfd>` 写 PID，并校验 PID
  后才认为 backend ready。helper 启动失败、超时、Valheim 正常退出或 panic/drop 路径都必须
  kill/reap 子进程并 join worker，不能遗留 passt。passt build 必须同时保留相邻的
  `passt.avx2`，否则 passt 的 x86_64 runtime dispatcher 会警告或不能选择 AVX2 build。
- passt NAT 启动会读取宿主 `/etc/resolv.conf` 的第一个可用 IPv4 nameserver，显式作为
  `--dns-host`，再把 guest-visible network 加 3 映射为 DNS proxy；找不到 IPv4 nameserver
  时应在启动 guest 前报错。passt 参数固定关闭 TCP/UDP host port forwarding、gateway
  host mapping 和 IPv6，不能把当前模式描述成可从 host 访问 guest。
- network backend 运行期错误当前只记录并打印一次，frontend 不公布 link-status feature、
  不热重启 helper，后续 TX 会丢弃而 guest 仍可能认为 carrier up。guest 写 status 0 时会
  清 frontend queue/backlog，但不会清空 backend channel 中已经排队的 frame；后续若要求
  严格 reset 隔离，需要给 backend 协议加入 generation 或 purge。
- RustSBI 历史 test kernel 的 success marker 来自明确记录的单 hart patch；不要声称未修改的上游多 hart HSM 测试在 Valheim 上完整通过。
- Debian 13 demo 的 rootfs 必须继续来自已固定并校验的 Debian 官方
  NoCloud 或 OCI artifact；不要用 BusyBox、手写 `/etc/os-release` 或自制目录树
  冒充 Debian。NoCloud 默认路径必须校验固定 archive 的 SHA-512、GPT 布局和
  p1 ext4 边界；不得把未校验的整个 `disk.raw` 当成 `/dev/vda`。
- OCI Linux ext4 base 必须在同一个 `fakeroot` 会话中解包 layer、创建
  overlay/device node 并执行 `mke2fs -d`，否则 `root:shadow` 等属主信息会被
  宿主 UID 污染。NoCloud base 应从官方 `disk.raw` 的 p1 提取；开发包安装只能
  mount 尚未晋升的临时 p1 副本，必须在私有 mount namespace 中使用固定 static
  qemu、唯一的临时 F-flag riscv64 binfmt、固定且签名校验的 Debian snapshot 和
  chroot；不得复用、禁用或改写宿主已有的 binfmt entry，退出时也只能删除本次创建的 entry。
  必须用 `policy-rc.d` 阻止 service 启动，保留原 `resolv.conf`/APT sources，并在成功、
  失败和信号退出时撤销本次 binfmt、逆序卸载所有 bind/proc/tmpfs/root mount；不得
  mount 或修改下载 archive、最终 base 或 runtime。卸载后才以 `debugfs` 接入 `/init`
  和必需设备节点。NoCloud 的 `orphan_file`/
  `FEATURE_C12` 必须用脚本固定构建的 e2fsprogs 1.47.2 验收，不得因宿主
  1.46.5 报 unsupported 而跳过 fsck，也不得对 NoCloud base 禁用官方 ext4 feature；
  固定 e2fsprogs 不能弥补宿主内核缺少该 ext4 feature 支持。
  开发包顶层列表、snapshot、qemu/helper hash、392-package count 和完整
  package/architecture/version manifest 都必须纳入 schema 或只读验收。修改任一来源的
  schema、feature 或 `/init` 时必须保留对应版本的只读 fsck/debugfs 元数据验收。
- Linux demo 的 NoCloud 和 OCI runtime 是互相独立的持久化可写磁盘；
  `RESET_DISK=1` 只能重置当前选中来源。guest `sync` 前用宿主 `Ctrl-C` 退出等价于
  突然断电。不要在 Valheim 仍持有 mmap 时对 runtime 运行 e2fsprogs。`run.sh` 的 `flock`
  是 advisory；直接调用 `valheim-cli --disk` 会绕过它，调用者必须避免并发打开同一镜像。
  两个来源仍共享下载、toolchain/source、kernel、RustSBI 和 Cargo 构建产物；这些阶段
  必须继续由 `target/demo/linux/prepare.lock` 串行保护。该全局锁只在准备/构建阶段持有，
  特权 chroot/base 晋升也必须位于该锁内，并在进入 Valheim 前确认没有本次 binfmt 或
  mount 残留后释放；来源 runtime lock 与实际 disk inode lock 则跨 `exec` 持有。
- PLIC 会在 priority/enable/threshold/claim/complete 和 source level 变化后重算各 context
  的 claim；CPU 每轮 interrupt poll 再按 S-context claimability 重建 SEIP。VirtIO block
  IRQ 1 和 network IRQ 2 都使用真正的 level input，ACK 后才 deassert；network RX backend
  还必须通过 WakeHub 解除 WFI，避免 guest 空闲时依赖宿主轮询。UART 仍通过 one-shot pulse
  接口注入；CPU 只在 WFI 或 SEIP 对当前特权全局可投递时轮询 pulse device，避免在常见的
  S-mode 临界区提前消费下一脉冲。若改成完整 NS16550 line-level IRQ，必须继续保持 PLIC
  gateway 的 in-service/coalescing 语义。
- production CLINT 只支持 host-monotonic realtime：`mtime` 以 10 MHz 在 guest 执行、
  WFI 和宿主被抢占期间持续流逝，不得恢复 instruction-tick 或 WFI fast-forward。
  `ClockSource` 注入只用于无 sleep 的可重复测试，不是面向 CLI 的 deterministic/turbo
  mode。WFI 的 timer wait 必须使用绝对宿主 deadline 并与 WakeHub generation
  一起防止 poll-to-wait lost wake。
- DTB 先由 `Machine::new` 放到 `0x87f0_0000`，CLI 随后才加载 BIOS/kernel，当前没有镜像范围与 DTB overlap 检查；现有约 40 MiB Linux Image 安全，但不要传入会延伸到该地址的大型 raw image。
- `Memory` 现已对完整宽度/完整 slice 做 checked 半开区间检查，非对齐读写使用
  unaligned primitives；bus 也使用完整宽度的半开区间匹配。但 `Machine::load_memory`
  仍会丢弃 `Memory::load` 的 `Option`，越界镜像会静默地未被加载。
- `profiler/profile.sh` 仍引用旧 binary 名称和缺失的测试镜像，并依赖 DTrace；使用前必须修正，不能把它作为已验证流程。
- Release profile 保留 debug symbols，这是性能分析和符号化所需的有意配置。

源码格式遵循 `.editorconfig`：UTF-8、LF、2 空格缩进，Rust 目标行宽 100。不要在无关任务中顺手升级 nightly、依赖或 guest 版本；该项目依赖旧 nightly feature 和旧硬件接口，升级需要单独验证。

## 修改后的最低验证要求

普通 Rust 改动至少运行：

```bash
cargo +nightly-2024-09-05 test --workspace --locked
cargo +nightly-2024-09-05 build --release --locked --package valheim-cli
```

修改 `valheim-jit` runtime、Cranelift lowering、helper ABI、cache/chaining 或 native side exit
后，还应让 verifier-off 的 release 路径和真实 guest 通过：

```bash
cargo +nightly-2024-09-05 test \
  --release --locked --package valheim-jit

export PATH="$PWD/target/demo/gcc-riscv64-elf-2022.03.09/riscv/bin:$PATH"
cargo +nightly-2024-09-05 run-riscv-tests -- --engine jit

RESET_DISK=1 ./demo/xv6/run.sh --engine jit
./demo/rustsbi/run.sh --engine jit
./demo/linux/run.sh --engine jit
```

上述 release `valheim-jit` tests 和三个 release demo 覆盖 verifier-off 路径；`xtask`
的 ISA 流程仍构建 debug CLI，以 verifier-on 配置覆盖 96 项 lowering。

修改 CPU、CSR、MMU、异常/中断、总线、UART、PLIC、CLINT、VirtIO、DTB 或 Machine 启动逻辑后，还应运行：

```bash
cargo +nightly-2024-09-05 test \
  --locked --package valheim-core --features trace

export PATH="$PWD/target/demo/gcc-riscv64-elf-2022.03.09/riscv/bin:$PATH"
cargo +nightly-2024-09-05 run-riscv-tests -- --engine naive
cargo +nightly-2024-09-05 run-riscv-tests -- --engine jit

RESET_DISK=1 ./demo/xv6/run.sh --engine naive
./demo/rustsbi/run.sh --engine naive
./demo/linux/run.sh --engine naive

RESET_DISK=1 ./demo/xv6/run.sh --engine jit
./demo/rustsbi/run.sh --engine jit
./demo/linux/run.sh --engine jit
```

验收不是只看到 kernel banner。xv6 必须进入 `$`，Linux 必须进入 `debian13#`，并分别至少成功执行一个 guest 命令，例如：

```text
echo RUN_SH_OK
cat /etc/debian_version
```

修改 VirtIO、PLIC、DMA、磁盘 mmap 或 Linux ext4 demo 时，Linux 还必须确认启动日志识别
`vda`、`/proc/mounts` 中 `/dev/root` 为 `ext4` 且含 `rw`，guest 写文件后执行 `sync`，
重启仍能读取该文件；随后用与该来源匹配的 `RESET_DISK=1` 命令启动并确认
文件消失。修改 rootfs 来源选择、base/runtime 生成、schema 或重置逻辑时，必须分别
验收默认 NoCloud 与 `--from-oci`，确认两个 runtime 的写入和重置互不影响。还应检查
`/proc/interrupts` 的 `virtio0` 计数增长，并验证第二个并发 `run.sh` 不能打开同一 runtime。
修改 NoCloud package/chroot 路径时，还必须从 base cache miss 开始验证固定 snapshot
安装、`dpkg --audit` 为空、392 个 installed package 和 manifest hash；宿主 qemu chroot
及真实 Valheim guest 都要用 GCC 编译并运行最小 C 程序，且退出后不得残留 Valheim
binfmt entry、loop mount 或 chroot bind mount。`--from-oci` 不应触发 sudo/chroot。

修改 VirtIO network、`valheim-net`、passt 参数/构建或 Linux DHCP/resolver 接入时，还必须
保留默认离线启动，并分别验证 `--net none`、默认 `--net nat` 和至少一个自定义 RFC 1918
`--net-subnet`。真实 Linux guest 应确认：发现第二个 legacy VirtIO device；DHCP 得到
network 加 15、default route 指向加 2、`/proc/net/pnp` 和实际 resolver 指向加 3；公网
IPv4 DNS 与 TCP/UDP 出站可用；`/proc/net/if_inet6` 不存在；`/proc/interrupts` 的
`virtio1`（network IRQ 2）在收发时增长。NoCloud symlink resolver 和 OCI regular
resolver 必须分别验收，离线时两者都不得被 `/init` 改写；这项要求不等于可以把只完成
OCI 的网络验收写成 NoCloud 已验证。至少再确认 passt helper 启动失败会阻止 guest 启动，
Valheim 正常退出和 `Ctrl-C` 后都没有遗留 helper/worker，两个并行实例能在逻辑隔离下复用
同一 guest-visible subnet，并且没有 host→guest listener/端口转发。frontend、backend 或
WFI 路径的测试还应覆盖 direct/indirect RX/TX、malformed descriptor、队列 wrap、IRQ
ACK/deassert、backend backpressure/failure、异步 RX 唤醒和 clean shutdown/reap。

修改 CLINT、TIME CSR、WFI 或 RustSBI timer relay 时，Debian 还必须确认启动日志包含
`SBI TIME extension detected` 和 10 MHz `sched_clock`，`time sleep 1` 约为一秒、
`/proc/interrupts` 中 `riscv-timer` 计数持续增长，且宿主 `pidstat` 在 Bash
prompt 空闲窗口不再稳定占用一个 core。
