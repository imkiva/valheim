# Debian 13 NoCloud/OCI ext4 Linux Demo

这个 demo 在 Valheim 中启动 Linux 5.17，默认进入 Debian 官方 NoCloud
riscv64 build `20260712-2537` 的 Bash。它比原有 `debian:13-slim` 容器层更接近
一个可交互使用的基础 Debian 系统。官方 p1 原有 271 个 package；脚本在首次构建
只读 base 时通过宿主联网 chroot 再安装 121 个 package，最终固定为 392 个全部处于
`install ok installed` 状态的 dpkg package。除了原镜像中的工具，还包括：

- `procps`/`psmisc`，因此有 `ps`、`top`、`free` 等基础系统工具。
- `less`、`file`、Nano、Vim、Python 3、`man` 和 manpages。
- `curl`、CA 证书、`screen`、`tar`/`gzip`/`xz`/`zstd`/`cpio`。
- e2fsprogs、dosfstools 等常用文件系统工具。
- `build-essential`、GCC/G++、make、Binutils、Git 和开发用 manpages。
- CMake、Ninja、Meson，以及 Autoconf、Automake、Libtool、Bison、Flex。
- GDB、strace、lsof、jq、rsync、wget、fakeroot、patch、tree 和 zip/unzip。
- Python 3 的 headers、pip 和 venv。

demo kernel 将 IPv4、kernel DHCP 和 VirtIO network 驱动直接编进 `Image`，但脚本
默认不启用网络，guest 离线启动仍不需要宿主网络。显式传入 `--net nat` 后，Valheim
通过独立的 `passt` 进程为 guest 提供 IPv4 出站 NAT；NoCloud 中预装的 `curl`、
`wget`、Git 和 APT 才具备出站连通条件。当前不支持 IPv6、host 到 guest 的入站连接
或端口转发；APT 是否能安装特定 package 还取决于镜像内的 sources 与固定 snapshot
状态。

根文件系统以无分区表的 raw ext4 镜像通过 Valheim legacy VirtIO-MMIO block
设备挂载为 `/dev/vda`。kernel `Image` 不再内嵌完整 initramfs；VirtIO、
VirtIO-MMIO、VirtIO block 和 ext4 都直接编进 kernel，默认命令行为：

```text
root=/dev/vda rootfstype=ext4 rootwait rw init=/init
```

官方 NoCloud `tar.xz` 内不是目录形式的 rootfs，而是一个带 GPT 分区表的
sparse `disk.raw`。脚本校验 archive 的固定 SHA-512 和 GPT p1 边界，从 p1 提取
root ext4。只在 base cache miss 时，脚本把尚未晋升的 p1 临时副本挂进私有 mount/PID
namespace，通过唯一的临时 riscv64 binfmt + chroot 使用固定 Debian snapshot 安装开发包；成功
卸载并通过 fsck 后，才用 `debugfs` 加入仓库的 `/init` 与早期 console 设备节点。
下载的 archive、最终只读 base 和 runtime 都不会被拿去做安装期 mount。提取后的 p1
本身被作为无分区表的 `/dev/vda`；不会把整个 `disk.raw` 交给 Valheim，也不会把它
保留为另一份可写 runtime。

默认 NoCloud 和可选 OCI 来源有各自独立的只读 base 和可写 runtime：

```text
target/demo/linux/build/debian-13-nocloud-riscv64.ext4  # NoCloud verified read-only base
target/demo/linux/runtime/nocloud-rootfs.ext4            # NoCloud writable runtime
target/demo/linux/build/debian-13-slim-riscv64.ext4     # OCI verified read-only base
target/demo/linux/runtime/rootfs.ext4                    # OCI writable runtime (legacy path)
```

脚本把两个 base 设为 `0444`，并拒绝把它们（包括 symlink/hardlink）作为可写
`--disk` 传给模拟器。runtime 默认跨运行保留；`RESET_DISK=1` 只会从当前
选中来源的 base 重置它自己的 runtime，不会改动另一来源中的 guest 数据。
每个 runtime 都有独立 schema；如果 schema 缺失或变化，脚本会停止并要求用户先
备份，不会静默删除 guest 数据。DHCP DNS 接入改变了共享 `/init` 的运行语义，因此
NoCloud schema 已升为 v3、OCI schema 已升为 v2，过去只为 banner 文字迁移保留的
OCI 兼容例外也不再适用。已有任一来源的旧 runtime 时，先备份 guest 数据，再为同一
来源显式使用 `RESET_DISK=1`。脚本只会复制通过当前 schema 验证的 base；如果 base
也因 schema 变化需要重建，NoCloud 路径会再次需要联网 chroot 和相应宿主权限。

两个来源都使用以下最小 Valheim 启动接入：

- `/init`：挂载 devtmpfs、proc、sysfs、tmpfs 和 devpts，确认 `/` 实际是 read-write
  ext4；若 kernel DHCP 在 `/proc/net/pnp` 提供 DNS，则为 NoCloud 的
  `/etc/resolv.conf` symlink 创建 `/run/systemd/resolve/stub-resolv.conf`，或更新 OCI
  的普通 `/etc/resolv.conf`，最后用 `/bin/bash` 启动交互 shell。离线启动不会改写
  resolver 文件。
- `/dev/console`、`/dev/null`、`/dev/tty`：让 PID 1 在挂载 devtmpfs 前就能打开控制台。

NoCloud rootfs 虽然含有 systemd，但这个 demo 通过 kernel cmdline 显式使用上述
`init=/init`，不会启动完整 systemd service graph；“更完整”主要指可用的用户空间
命令、dpkg 内容和原生开发工具链更多。

OCI 解包、overlay/device node 创建和 `mke2fs -d` 全部位于同一个 `fakeroot`
会话，因此保留官方 layer 中的 UID、GID、权限、符号链接和硬链接。NoCloud
保留官方 p1 里的 ext4 目录树和元数据，再做固定开发包安装和上述启动接入。
`--from-oci` 路径仍不使用宿主 mount 或 sudo；NoCloud 首次生成 base 时需要特权
执行 mount/chroot（非 root 用户通过 sudo），缓存命中和后续运行不再需要。安装在
`unshare` 的私有 mount/PID namespace 中进行，只 bind `/dev`、只读 DNS 和独立
`/proc`；临时 `policy-rc.d`
禁止 package maintainer script 启动服务。chroot 不是安全沙箱，因此输入只允许固定、
签名校验的 Debian artifact 和 snapshot。两条路径都不会修改已下载的官方
archive/layer。脚本对适配后的 base 运行只读 `e2fsck -fn`，并用 `debugfs` 检查
superblock、`/init`、设备节点、完整 package manifest 和关键 Debian 元数据。
NoCloud p1 含有 `orphan_file`/`FEATURE_C12` ext4 feature；Ubuntu
22.04 常见的 e2fsprogs 1.46.5 不识别它。为避免把干净的官方文件系统误判为
损坏，脚本会为 NoCloud 自动下载、校验并构建固定 e2fsprogs 1.47.2，使用其
`e2fsck` 和 `debugfs`。该构建启用源码自带的私有 libuuid/libblkid，不需要宿主
`uuid-dev` 或 `libblkid-dev`。固定的用户态工具不能替代内核 ext4 驱动：宿主内核也
必须能读写带 `orphan_file` 的 ext4；在容器中运行还必须开放 loop device 和
`CAP_SYS_ADMIN`。OCI 路径仍使用宿主 e2fsprogs，生成固定 UUID
`3f3434d2-6c1e-4f8b-98e8-4f525649534b`、label `VALHEIMROOT`、4 KiB block 和明确的
feature 集，并保留 `/etc/shadow` 的 `0:42/0640` 与 Debian Perl hardlink 验收。

## 一键运行

从仓库根目录执行：

```bash
./demo/linux/run.sh
```

这会选择默认 NoCloud rootfs。需要使用原 slim OCI rootfs 做精简环境回归时：

```bash
./demo/linux/run.sh --from-oci
```

`--from-oci` 是 `run.sh` 自己的参数，会在启动 `valheim-cli` 前从参数列表删除。
脚本默认使用 JIT；需要显式运行参考解释器时传入 `--engine naive`，两种
rootfs 来源都可与两种 engine 组合：

```bash
./demo/linux/run.sh --engine naive
./demo/linux/run.sh --from-oci --engine naive
```

### IPv4 出站 NAT

网络默认关闭。使用默认 `10.172.0.0/16` 私有网段启动 NAT：

```bash
./demo/linux/run.sh --net nat
```

默认地址为 guest `10.172.0.15`、网关 `10.172.0.2`、DNS proxy
`10.172.0.3`；每个 Valheim/passt 实例相互隔离，因此并行实例可以复用这组
guest-visible 地址。可用 canonical RFC 1918 CIDR（完全位于 `10/8`、`172.16/12`
或 `192.168/16`，且不窄于 `/27`）覆盖网段，guest、网关和 DNS 仍分别使用网络
地址加 15、2 和 3 的偏移：

```bash
./demo/linux/run.sh --net nat --net-subnet 10.173.0.0/16
```

脚本可靠识别 `--net nat` 和 `--net=nat`，只在 NAT 模式下给它自动提供的 kernel
cmdline 追加 `ip=dhcp`。如果同时用 `--cmdline`/`-c` 完全覆盖 cmdline，调用者必须
自行保留 `ip=dhcp` 或在 guest 中手工配置网络；离线模式不会等待 DHCP。

NAT 模式默认下载、校验并构建下文固定版本的 `passt`，然后给 CLI 传入它的绝对路径。
也可以显式提供已有 binary，跳过 demo 自带 passt 的下载和构建：

```bash
./demo/linux/run.sh --net nat --passt /absolute/path/to/passt
```

直接调用 `valheim-cli` 时同样可用 `--passt PATH` 指定 helper。该模式目前只提供 guest
IPv4 出站 TCP、UDP 和 DNS，不提供 IPv6、host→guest 连接或入站端口映射。

脚本使用自身路径定位仓库，所以也可以从其他工作目录调用。首次运行会依次：

1. 下载并解压固定的 RISC-V Linux GNU 工具链到共享目录
   `target/demo/gcc-riscv64-glibc-2022.03.09/`。
2. 下载 Linux 5.17。
3. 按选中模式下载并校验固定的 Debian NoCloud archive/package metadata，或 OCI index、
   riscv64 manifest 和 rootfs layer；NoCloud 还会构建固定的宿主 e2fsprogs 1.47.2，
   下载固定的 static qemu-riscv64，在临时 p1 chroot 中从固定 Debian snapshot 安装
   开发包，然后生成并校验选中来源的 ext4 base。NoCloud 仅在 base cache miss 时需要
   sudo；首次运行或显式重置时创建该来源独立的可写 runtime。
4. 构建包含内建 VirtIO block、VirtIO network、IPv4/kernel DHCP 和 ext4 驱动的
   raw `Image`；只有显式 NAT 模式才启动 DHCP。
5. 调用 `demo/rustsbi/run.sh --build-only` 构建历史 RustSBI firmware。
6. 构建 release 版 `valheim-cli`，自动传入 runtime `--disk`，然后进入交互式 guest。

下载、源码、构建和运行目录全部位于 `target/demo/linux/`，共享 Linux GNU
工具链位于 `target/demo/gcc-riscv64-glibc-2022.03.09/`。下载和构建产物都有
缓存；再次运行会重新校验固定摘要和选中来源的 ext4 base，复用正确的
下载、工具链、该来源的 runtime 以及增量 kernel 构建。
NoCloud 专用 e2fsprogs、static qemu 和 NAT 模式的 passt source/build 缓存在：

```text
target/demo/linux/host-tools/e2fsprogs-1.47.2-source
target/demo/linux/host-tools/e2fsprogs-1.47.2-build
target/demo/linux/host-tools/qemu-user-static-6.2+dfsg-2ubuntu6.31
target/demo/linux/host-tools/passt-2026_06_11.a9c61ff-source
target/demo/linux/host-tools/passt-2026_06_11.a9c61ff-build
```

两条路径共同需要 `curl`、`make`、`cc`、`bc`、`bison`、`flex`、`perl`、
`gzip`、`xz`、`tar`、`python3`、`sha256sum`、`dtc`、`flock` 和 Rustup。
默认 NoCloud 路径另需 `dd`、`sha512sum`、`sfdisk`、`chroot`、`mount`、
`unshare`、`update-binfmts` 和 `dpkg-deb`；它会自行构建并使用上述固定 1.47.2 的
`e2fsck`/`debugfs`，并从固定 Ubuntu package 提取 static qemu-riscv64。非 root
用户还需要 `sudo`；直接以 root 运行时不要求安装它。
`--from-oci` 路径另需 `fakeroot` 及宿主
`mke2fs`/`e2fsck`/`debugfs`（由 Debian/Ubuntu 的 `e2fsprogs` package 提供）。
`sfdisk` 来自 `fdisk` package（source 属于 util-linux），`flock` 来自
`util-linux` package，`update-binfmts` 来自 `binfmt-support`，`dpkg-deb` 来自
`dpkg`；`dd`、`chroot` 和两种 hash 命令来自 `coreutils`。脚本会自动安装 RustSBI
demo 已固定的 Rust toolchain。NoCloud 的 sudo/chroot 需求只发生在首次生成或 schema
变化后重建 base；已有 base 校验通过时不会请求 sudo。NoCloud chroot 还要求
`binfmt_misc` 已挂载并处于 enabled 状态；helper 会在不满足时直接报错。
在最小 Debian/Ubuntu 宿主上可安装以下系统依赖；Rustup 仍需单独安装：

```bash
sudo apt-get update
sudo apt-get install -y \
  build-essential bc binfmt-support bison curl device-tree-compiler dpkg e2fsprogs \
  fakeroot fdisk flex gzip perl python3 sudo tar util-linux xz-utils
```

如果共享缓存完全为空，默认 NoCloud 模式需下载 Linux GNU 工具链约
612 MB、RustSBI bare-metal GNU 工具链约 472 MB、Linux source 约 128 MB，以及
NoCloud archive 约 293 MiB（约 307 MB）。只有首次使用 `--from-oci` 时才会额外下载约 28 MB 的
OCI rootfs layer 和很小的 metadata。NoCloud 首次运行还会下载约 7 MiB 的
e2fsprogs 1.47.2 源码 archive、约 12.4 MiB 的 qemu-user-static package，以及固定
Debian snapshot 中约 163 MB 的 development packages，并生成宿主工具缓存。此外还会
下载两个 Rust nightly、guest target 和 Cargo 依赖；旧 Cargo 首次同步 crates.io Git
index 也会占用约 1–2 GiB。
NoCloud `disk.raw` 逻辑大小为 3 GiB，p1 base/runtime 逻辑大小约为 2.87 GiB，
开发包安装后的 base 实际分配约 1.83 GiB、仍有约 1.35 GiB ext4 free space；宿主
文件系统必须支持 sparse file，副本会在支持时优先使用 reflink。OCI base/runtime 逻辑大小仍为
256 MiB。连同解压、两条 rootfs 路径和构建产物，建议至少预留 12 GiB 可用空间；
不支持 reflink 时会占用更多空间；不支持 sparse 时脚本会拒绝生成不符合约定的 base。

可选环境变量：

- `JOBS`：kernel、RustSBI 和 Cargo 的并行任务数；默认使用 `nproc`。
- `RESET_DISK=1`：丢弃当前选中来源的默认 runtime guest 写入，从该来源已
  验证的只读 base 创建新副本；默认 `0`，会复用该来源已有的 runtime。
- `VALHEIM_RUST_TOOLCHAIN`：Valheim Rust toolchain；默认 `nightly-2024-09-05`。
- `VALHEIM_LINUX_TOOLCHAIN`：共享 Linux GNU 工具链目录；默认是上面的固定目录。
- `VALHEIM_RISCV_TOOLCHAIN`：RustSBI 的共享 bare-metal GNU 工具链目录。

脚本使用 `RUSTUP_HOME=target/demo/rustup`、`CARGO_HOME=target/demo/cargo-home`
和 `CARGO_TARGET_DIR=target/demo/cargo`；调用 RustSBI demo 时也共享这些目录，
确保 Rust/Cargo 下载和构建产物不落到仓库外。`CARGO_HOME` 只注入
实际 Cargo 子进程，以便继续复用宿主已安装的 rustup 代理。

除 `--from-oci` 外的额外参数继续透传给 `valheim-cli`。`--net nat` 会按需注入
固定 passt binary 和默认 cmdline 的 `ip=dhcp`；显式 `--passt` 与显式 cmdline 分别
覆盖这两个默认值。脚本默认注入
`--engine jit`，显式 `--engine naive` 可覆盖。脚本也会自动注入选中来源
runtime 的 `--disk`；若用户显式提供
`--disk PATH`、`--disk=PATH`、`-d PATH`、`-dPATH` 或 `-d=PATH`，则不会重复注入。
attached `-cCMDLINE`/`-c=CMDLINE` 也会正确覆盖默认 cmdline。显式 disk
不需要准备 NoCloud 或 OCI rootfs；`RESET_DISK=1` 与显式 disk 不能同时使用，避免
重置了未实际启动的另一份镜像。

脚本对两个来源的默认 runtime 分别使用稳定 lock file，并对实际 disk inode 持有 nonblocking
exclusive `flock`；锁定的文件描述符会跨最后的 `exec` 保留到 Valheim 退出。因此两个
`run.sh` 不能同时读写同一镜像，重置也不能覆盖正在使用的默认 runtime。显式 external
disk 同样会锁定；它必须是可写 raw ext4、包含可执行 `/init`，并符合用户提供的 kernel
cmdline。直接调用 `valheim-cli --disk ...` 会绕过这个 advisory lock 约定，调用者必须自行
保证没有另一个进程使用该镜像。

共享的下载、toolchain/source 准备、kernel、RustSBI 和 Cargo 构建还由
`target/demo/linux/prepare.lock` 串行保护；另一个正在准备这些公共产物的 Linux demo
会让新进程快速报错。该锁在进入 Valheim 前释放，因此准备完成后，不同 disk 的两个
模拟器进程仍可并行运行；每个 disk 自己的锁会继续持有到对应进程退出。

## 固定来源与摘要

| 内容 | 固定来源 | 校验值 |
| --- | --- | --- |
| Debian 13 NoCloud riscv64 build `20260712-2537` | `https://cloud.debian.org/images/cloud/trixie/20260712-2537/debian-13-nocloud-riscv64-20260712-2537.tar.xz` | SHA-512 `65f4c937175e6f096e697f671b8bbd745f1a6025f610343e1d00bfd4e0bbe475b27a8cc77c0f22f83499242d78be2070769c24da11ee4c73e37072eab8659783` |
| NoCloud build metadata/package list | 同目录的 `debian-13-nocloud-riscv64-20260712-2537.json` | SHA-512 `023206bfb347bc1f2c1b64ae1d42f7c1ccc2b42fb48b7f2bdd2e8c7750d06bdee0ffc2248d5df054d40803aefc8abc2d5318098fee42202d41dbfe1eb6b28412` |
| NoCloud 宿主 ext4 工具 e2fsprogs 1.47.2 | `https://cdn.kernel.org/pub/linux/kernel/people/tytso/e2fsprogs/v1.47.2/e2fsprogs-1.47.2.tar.xz` | SHA-256 `08242e64ca0e8194d9c1caad49762b19209a06318199b63ce74ae4ef2d74e63c` |
| NoCloud chroot 的 Debian main/updates snapshot | `https://snapshot.debian.org/archive/debian/20260712T202631Z/` | Debian archive keyring 签名；`Check-Valid-Until: no` |
| NoCloud chroot 的 Debian security snapshot | `https://snapshot.debian.org/archive/debian-security/20260712T194830Z/` | Debian archive keyring 签名；`Check-Valid-Until: no` |
| NoCloud static qemu-riscv64 | Ubuntu `qemu-user-static_6.2+dfsg-2ubuntu6.31_amd64.deb` | package SHA-256 `2d22939f98f2ee8b84c5cc53b01082a4a937cfc7b4a8aa432788b9eaf4a14a41`；binary SHA-256 `ee063e5feaae2475b1eabe82ead98574c94fbbdbf6e0131379ea686ab6e3b437` |
| NoCloud enriched dpkg manifest | 392 行排序后的 `package<TAB>architecture<TAB>version` | SHA-256 `051c1d9de5b0b1eb38442edcab296835363fcdbbe806f4335888de1e11d6672b` |
| Debian `13-slim` OCI index | `registry-1.docker.io/v2/library/debian` | `020c0d20b9880058cbe785a9db107156c3c75c2ac944a6aa7ab59f2add76a7bd` |
| riscv64 OCI manifest | 同上；index 中 `architecture=riscv64` 的 manifest | `7244fbb388f7b59c9f584bb2bb7ef3a60b23aa1e55f1ad1d0641bd5ec12390f3` |
| Debian rootfs layer | manifest 中唯一的 gzip layer | `3ed37bd5491de4685b6418abd6b83c4b16cc06b7a51e46da7f154c5a149a41a5` |
| passt userspace NAT | `https://passt.top/passt/snapshot/passt-2026_06_11.a9c61ff.tar.xz` | SHA-256 `b94b235cb96ce1b7aeab6552b7e0b4c9a780e5d700ced500c65e429b2d8b8450` |
| Linux 5.17 | `https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-5.17.tar.xz` | `555fef61dddb591a83d62dd04e252792f9af4ba9ef14683f64840e46fa20b1b1` |
| RISC-V GNU/Linux toolchain | `riscv-collab/riscv-gnu-toolchain` release `2022.03.09` 的 Ubuntu 20.04 glibc asset | `02b97cf3502d9542943b62c7470d99f97c0c9148be95e1277df96d4b5c2fdb41` |

NoCloud 的固定 URL 是带日期和 build id 的不变入口；脚本不跟随 `latest/`
重定向。包列表可与同目录的官方
`debian-13-nocloud-riscv64-20260712-2537.json` 交叉检查，但该 JSON 只描述扩展前的
271-package 官方输入；最终 392-package 状态由固定 snapshot、显式顶层 package 列表、
零升级检查和完整 dpkg manifest SHA-256 共同约束。历史 snapshot 必须保留签名校验，
脚本只关闭其过期时间检查，不使用 `trusted=yes` 或 `--allow-unauthenticated`。
固定 OCI layer 的内容是
Debian `13.6`，OCI index annotation 标记为 `trixie-slim` 和 `riscv64`。Linux 5.17
与 Debian 版本是两个不同层次：前者是适配 Valheim/RustSBI 的 guest kernel，
后者是官方 Debian 用户空间。Debian 13 glibc 声明的最低 Linux ABI 为 4.15，
因此可以在这个 5.17 kernel 上运行。

## 启动、写入与验收

2026-07-15 在加入开发包前，已实际以默认 NoCloud + release JIT 启动原始
271-package 适配版：固定 e2fsprogs 1.47.2 的 `e2fsck -fn` 通过，Linux 把提取后的
6027264-sector p1 识别为 2.87 GiB `/dev/vda`，以 read-write ext4 挂载并进入
`debian13#`；基础工具、journal recovery 和 `sync` 后跨进程持久化均通过。

同日又实际完成了当前 development profile 的首次构建与 release JIT 验收：chroot
从两个固定 snapshot 安装 121 个新包、0 个升级；`dpkg --audit` 为空，APT 模拟升级
仍为 0，宿主 qemu chroot 与真实 Valheim guest 中的最小 C 程序都由 GCC 成功编译并
输出成功标志。最终 392 个 dpkg 条目全部 installed，完整 manifest SHA-256 与上表
一致；guest 中确认 GCC target 为 `riscv64-linux-gnu`、Git 为 2.47.3、pip 为 25.1.1，
且 GCC/G++/make、Git、CMake/Ninja/Meson、Autotools、GDB/strace、jq/rsync 和 Python
开发命令均可执行。第二次启动复用了已验证 base/runtime，ext4 journal recovery 后
再次进入 prompt。NoCloud naive 与性能数据尚未按正式口径记录。

以下数据来自旧的内建-initramfs demo，不是任一 ext4 block-root 性能基线：realtime
切换前的历史固定 CPU 16 release 基线是 JIT 4.806 秒、naive
60.926 秒；该时钟会快进 guest 等待。`10cabc6`/`258fdf6` 后的 realtime JIT
三次为 8.903906 / 8.379352 / 8.321334 秒，中位数 8.379352 秒。硬件不同
会变化，且新旧口径不可直接比较。realtime naive 单次验收为
133.996711 秒，不是正式三次中位数。kernel timestamp 来自 host-monotonic 驱动的
10 MHz `mtime`，与宿主 elapsed time 等速推进；CLINT 在 `Machine`/CPU 构造期间建立
anchor，计数包含随后 DTB 生成及 kernel/BIOS 的读取和装载，但不包含此前的 CLI 参数解析和
JIT executor 构造。它不是宿主日历时间，也不能代替进程级外部计时。2026-07-14/15
记录的交互、持久化和 direct-root 性能数据来自原 slim OCI 路径，现在应用
`--from-oci` 复现；不应将它们当作默认 NoCloud 的已验证性能数据。看到
`debian13#` prompt 即表示已进入 guest shell：

```text
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
command -v ps top less file nano vim python3 man
command -v gcc g++ make git pkg-config cmake ninja meson
command -v autoconf automake libtoolize bison flex gdb strace lsof jq rsync
command -v pip3 python3-config
dpkg --audit
printf 'installed='; dpkg-query -W -f='${db:Status-Abbrev}\n' | grep -c '^ii '
printf '#include <stdio.h>\nint main(void){puts("CC_OK");}\n' >/tmp/hello.c
gcc -O2 -Wall -Werror /tmp/hello.c -o /tmp/hello && /tmp/hello
awk '$2 == "/" { print $1, $3, $4 }' /proc/mounts
grep riscv-timer /proc/interrupts
time sleep 1
grep riscv-timer /proc/interrupts
```

`SBI TIME extension detected`、`sched_clock: 64 bits at 10MHz`、VirtIO block 识别、
ext4 以 read-write 方式挂载、`sleep 1` 约一秒且
前后两次 `riscv-timer` IRQ 计数增加，是 realtime/timer-relay 验收的一部分。

显式使用 `--net nat` 时，还应看到 `virtio_net` 绑定第二个 VirtIO-MMIO 设备。默认
网段可用以下命令检查 DHCP 地址、路由、DNS 数据与 IPv4 出站连接：

```bash
ip -4 address show dev eth0
ip -4 route
cat /proc/net/pnp
cat /etc/resolv.conf
getent ahostsv4 debian.org
wget -4 -qO /dev/null http://example.com/
```

地址应包含 `10.172.0.15/16`，default route 应指向 `10.172.0.2`，resolver 应包含
`10.172.0.3`。公共网络检查会受宿主网络策略影响，不应替代后端 frame/queue 单元测试。
Valheim 尚未提供 RTC，guest 初始时间为 1970；未手工校时前 HTTPS 证书时间检查会失败，
因此这里故意用 HTTP 只验收网络路径。

验证 runtime 可写和跨进程持久化：

```bash
echo EXT4_PERSIST_OK >/root/valheim-persist
sync
cat /root/valheim-persist
```

执行 `sync` 后按宿主 `Ctrl-C` 退出，以同一来源再次运行脚本，文件应仍存在。
默认 NoCloud 的重置命令是：

```bash
RESET_DISK=1 ./demo/linux/run.sh
```

要重置 OCI runtime，则必须仍然选中 OCI：

```bash
RESET_DISK=1 ./demo/linux/run.sh --from-oci
```

重置后的选中 guest 中 `/root/valheim-persist` 应不存在，另一来源的 runtime
不会受影响。宿主 `Ctrl-C` 等价于突然断电，
所以持久化验收必须先在 guest 执行 `sync`；日志恢复也应在下一次启动时保持文件系统
可挂载。不要在 Valheim 仍持有 mmap 时用宿主 e2fsprogs 检查 runtime。

两个固定来源都应报告 Debian 13/trixie；OCI layer 的完整版本固定为
`13.6`。`uname` 应报告 Linux `5.17.0`、架构 `riscv64`，`id` 应报告
`uid=0(root)`。NoCloud 模式中上述基础/开发工具的 `command -v`、392-package 计数、
空的 `dpkg --audit` 和原生 GCC smoke test 应全部成功；OCI slim 模式不要用该工具
列表验收。按宿主终端的 `Ctrl-C` 退出 Valheim。

关键产物：

```text
target/demo/linux/build/debian-13-slim-riscv64.ext4
target/demo/linux/build/debian-13-nocloud-riscv64.ext4
target/demo/linux/runtime/nocloud-rootfs.ext4
target/demo/linux/runtime/rootfs.ext4
target/demo/linux/build/kernel/arch/riscv/boot/Image
target/demo/rustsbi/artifacts/valheim/rustsbi-qemu.bin
target/demo/cargo/release/valheim-cli
```

旧的 `.cpio` 可能仍留在既有 `target/demo` 缓存中，但当前 kernel 配置要求
`CONFIG_INITRAMFS_SOURCE=""`，不会再将它嵌入 `Image`。

不要对仓库运行 `cargo clean`；它会删除整个 `target/`，包括下载的共享工具链和
全部 demo。
