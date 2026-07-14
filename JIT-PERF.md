# Valheim JIT 性能记录与后续方向

本文档记录截至 2026-07-15 已经落地的 JIT/运行时性能优化、可复现的 Debian
启动数据，以及下一轮仍值得评估的方向。JIT 的总体架构、语义约束和第一阶段验收见
[`JIT-PLAN.md`](JIT-PLAN.md)。

本轮到此停止：下面“后续候选”均未实现，也不代表已经验证会更快。

## 范围与当前结论

- host 明确只支持 Linux x86_64 System V ABI。
- guest、Linux kernel、RustSBI、Debian rootfs 和未压缩内建 initramfs 均保持不变；本轮性能
  改动只发生在 Valheim。
- 性能主指标仍是宿主进程启动到真实行末 `debian13# ` prompt，包含 JIT 编译时间。
- 初始 JIT 的中位数为 13.447 s；当前最终工作树的中位数为 7.014 s，启动时间再减少
  47.8%，即再快 1.917 倍。
- 相对 69.603 s 的 naive 基线，当前 JIT 约快 9.923 倍，启动时间减少 89.9%。
- 最后几个 6.9–7.0 s checkpoint 的差异处于本机噪声范围，不能归因成某个小改动的独立收益。

当前性能代码终点是 `b7bc41a`；其后单独提交本文档，不混入代码改动。

## 测量口径

测量环境与 `JIT-PLAN.md` 相同：Linux 6.6.87.2 WSL2、AMD Ryzen 9 9950X3D、
32 logical CPUs、固定的 Debian demo artifacts，以及同一 release 构建。外部计时从进程
启动开始，到检测到真实行末 prompt 为止；因此镜像加载和全部运行期 JIT 编译成本都被计入。

计时运行默认关闭 `--jit-stats`，避免统计本身改变热路径。每个正式结果运行三次并取中位数。
开发期累计 checkpoint 如下：

| 累计 checkpoint | Debian prompt 结果 |
| --- | ---: |
| naive | 69.428 / 69.673 / 69.603 s；中位数 69.603 s |
| `b432c42` 初始 JIT | 13.447 / 13.399 / 13.452 s；中位数 13.447 s |
| memory/UART/SATP 等第一批优化 | 中位数 12.077 s |
| runtime cache / stats gating | 中位数 9.777 s |
| native multi-TB batching | 中位数 8.357 s |
| batch context hoist | 中位数约 7.010 s |
| compiled front cache，同一累计树 A/B | 启用约 7.059 s；禁用约 7.265 s |
| MSTATUS translation mask | 中位数 6.922 s |
| SATP/SFENCE 拆分 | 中位数 6.949 s |
| 完成 WFI/CLINT 后的最终树 | 7.086 / 6.987 / 7.014 s；中位数 7.014 s |

这些 checkpoint 在开发工作树上累计测得，之后才按职责拆成独立 Git commit；因此 Git 拓扑
顺序与测量顺序并不一一对应，不能用相邻行之差宣称单个 commit 的独立收益。可以单独引用的
A/B 结果是 compiled front cache；SATP/SFENCE 和 CLINT 对 Debian prompt 基本中性。

CLINT 的确定性收益不体现在这个 prompt 指标中：它把 hart 的长 WFI timer idle wait 从
O(`mtimecmp - mtime`) 次空转降为 O(1) 次 deadline 跳转。

## 已完成并独立提交的优化

`b432c42..b7bc41a` 正好包含以下十个独立提交。

### Core、设备与失效语义

1. `254db75 perf(core): fast-forward WFI timer waits`

   当 hart 处于 WFI、`MTIE` 本地启用且存在未来 `mtimecmp` 时，直接把 CLINT 推进到准确
   deadline，再走统一的 pending-interrupt/trap 路径。实现不会跳过已 pending 的软件中断、
   不会越过初始 tick 已触发的 timer，也不会对未启用的 timer 快进。

2. `a297f53 perf(uart): avoid locking on idle interrupt polls`

   UART 增加 acquire/release atomic IRQ hint。绝大多数无事件的中断轮询只做一次 atomic
   load，不再进入 `Mutex<UartState>`。真实状态仍在 mutex 下消费和重新发布，因此保留 RX
   优先、RX/THRE 单脉冲、IER rearm 和 IIR acknowledge 语义，并覆盖 producer/consumer
   竞态。

3. `5d9f5a2 fix(cpu): preserve zero-source CSR read semantics`

   `CSRRS/CSRRC rs1=x0` 和立即数形式的 `uimm=0` 现在是真正的只读操作。SATP 纯读不再
   伪装成写入，不再同步页表或增加 translation epoch；对只读 CSR 的零源读取也不会错误地产生
   illegal-instruction。它主要是正确性修复，同时去掉了会触发 JIT 失效的伪写入。

4. `254a19c feat(cpu): track explicit SFENCE generations`

   新增只由成功的显式 `SFENCE.VMA` 递增的 `sfence_epoch`。原有 `translation_epoch` 继续覆盖
   SATP 写和 SFENCE，使 JIT 能区分“地址空间切换”和“guest 明确要求丢弃旧翻译”。

### JIT 正确性护栏、代码生成与 dispatcher

5. `74e71d6 fix(jit): terminate translation blocks at atomics`

   LR/SC 和全部 AMO 成为 TB terminator。原子 helper 后不再继续执行同一 TB 的其他 guest
   指令，避免 reservation、fault PC、寄存器提交和后续 multi-TB chaining 发生顺序错误。

6. `7d317cd perf(jit): eliminate disabled statistics overhead`

   CLI 只有显式传入 `--jit-stats` 才收集统计。关闭时，Rust runtime 跳过 dispatcher/cache/
   fallback counters、`Instant::now()` 和 compilation samples；Cranelift 也完全不生成 TLB-hit
   counter RMW。软件 TLB 保留一个有效的 disabled sink，保证“已有 native block 编译后再
   关闭统计”不会让旧机器码解引用空指针。

7. `5c9165f perf(jit): streamline native memory codegen`

   为 frame、xregs/TLB 和 guest DRAM 设置互不混淆的 Cranelift alias region；guest DRAM
   明确允许 RISC-V 合法的非对齐访问。TLB miss、跨页访问和 helper failure 标为 cold；fault
   metadata 与 dirty GPR 只在需要 side exit 时写回；8-bit 访存删除不可能发生的跨页检查。
   16/32/64-bit 页尾访问仍进入共享 CPU slow path，以保留第二段 guest VA 和精确异常类型。

8. `58b3ef4 perf(jit): add indexed compiled-block front cache`

   增加 4096 set、2-way 的 compiled-TB front cache，命中时绕过 `FxHashMap`。authoritative
   map 指向稳定的 block arena index；generation 在 arena 清理前先失效，避免悬空 index。
   live code bytes 也改为增量维护，不再扫描所有 backend。累计树上的直接 A/B 为约
   7.059 s 对 7.265 s。

9. `116ff49 perf(jit): batch native blocks with precise side exits`

   一次 `RV64Executor::execute()` 可以在同一个 Machine budget 内连续运行多个已编译 TB，
   减少每个 TB 都返回顶层 dispatcher 的成本。后续 TB 只允许 native continuation；遇到
   未编译块、system/fallback、atomic、WFI、exception 或 budget 末尾立即停止。若后续 TB
   第一条指令发生 TLB miss、MMIO、跨页或 Sv39 D-bit 更新，则在任何 guest-visible side
   effect 前退出，下一 dispatcher 边界再精确执行。

10. `b7bc41a perf(jit): narrow translation cache invalidation`

    epoch、TLB context 和 code-arena 检查提升到整个 native batch 入口，只检查一次。`TbKey`
    收窄为 PC、privilege 和 SATP；SATP 切换可重用各自 keyed block，不再清空全部 native
    code。MSTATUS context 只保留会影响翻译的 MPP、MPRV、SUM 和 MXR。显式
    `SFENCE.VMA` 与 `FENCE.I` 仍保守地清 compiled cache。

## 当前验证状态

最终代码树已经通过：

- `cargo +nightly-2024-09-05 test --workspace --locked`
  - `valheim-asm` 11 个测试；
  - `valheim-core` 53 个测试；
  - `valheim-jit` 31 个单元测试；
  - 8 个 native/naive differential tests；
  - 4 个 native memory fast-path integration tests；
  - `xtask` 4 个测试。
- `cargo +nightly-2024-09-05 build --release --locked --package valheim-cli`。
- 最终 Debian 树三次到达真实 `debian13#` prompt，结果见上表。

初始 JIT 阶段的 96 个 `riscv-tests`、xv6、RustSBI 和 Debian 双引擎验收记录见
`JIT-PLAN.md`。这十个优化提交拆分后的最终树没有再次完整运行 96 项测试、xv6 和 RustSBI；
下次继续优化前应补跑，合并前也必须满足根 `AGENTS.md` 的完整回归要求。

`cargo fmt --all -- --check` 会要求把仓库既有的 2 空格 Rust 风格整体改成 rustfmt 默认布局，
因此当前不能作为局部改动的有效格式门禁。本轮只运行过只读 `--check`，失败后没有产生文件
修改；`git diff --check` 已通过。

## 尚可评估的优化

以下优先级是基于当前代码结构和已有统计的工程判断，不是性能结论。下一轮应先用 `perf` 和
新的低开销 counters 确认热点，再一次只实现并提交一个方向。

### P0：先补齐可观测性

本机已安装 `rustfilt 0.2.1`。`/usr/bin/perf` 包装器当前找不到与
`6.6.87.2-microsoft-standard-WSL2` 匹配的工具；已有的
`/usr/lib/linux-tools/5.15.0-1103-kvm/perf` 5.15.200 可以运行，但在这个 WSL 环境中
`cycles`、`instructions`、`branches` 和 `branch-misses` 均报告 `<not supported>`。因此下次
要么先验证匹配版本和 WSL hardware counters，要么用 software `cpu-clock` samples；需要可靠
硬件计数时应在 native Linux x86_64 上复测，不能把当前 WSL 的空计数当作结果。

1. 为 Cranelift 生成代码输出 `/tmp/perf-$PID.map` 或 jitdump。

   release profile 已保留 Rust debug symbols，`rustfilt` 可以处理 Rust 符号；但没有 perf-map
   时，`perf record` 无法把 native TB 样本对应到 guest PC。先让每个函数以 guest 起始 PC、
   SATP/privilege 和 code range 注册，才能区分 Rust dispatcher、TLB helper、atomic helper
   与真正的生成代码。

2. 增加只在 `--jit-stats` 下开启的 stop-reason 和 context counters。

   至少记录每次 Machine dispatch 实际运行多少 guest 指令/多少 native TB、front-cache
   hit/miss、停止 batching 的原因、TLB 因 privilege/SATP/MSTATUS/SFENCE 各自失效的次数、
   atomic helper 调用数，以及 SFENCE/FENCE.I 次数。计时基准仍必须关闭统计。

3. 固定 profile 流程。

   使用同一个 prompt harness。在 hardware counters 可用的环境中，先做 `perf stat -r 5` 的
   cycles、instructions、branches、branch-misses、cache-misses 和 context-switches，再用
   `perf record -g` 找累计 CPU 热点；当前 WSL 可先用 wall-clock A/B 和 software
   `cpu-clock` sampling。不要把 stats-on 的运行时间和 stats-off 的运行时间直接比较。

### P1：优先做的小步实验

1. 调整 executor budget，并把 timer deadline 与外部设备轮询边界分别考虑。

   当前 `MAX_EXECUTOR_BUDGET` 固定为 32，即使 multi-TB batching 仍每 32 条 guest 指令返回
   Machine。应独立 sweep 64/128/256，先验证能否继续减少 CLINT、pending-interrupt 和
   dispatcher 开销。timer 已有精确 deadline，可继续限制 budget；风险是 UART/VirtIO 外部
   IRQ 延迟变大，因此不能无界增大，必须测交互延迟和设备回归。

2. 复用 `JitFrame`，并给 compiled block 增加 generation-guarded successor cache。

   当前每个 native TB 都重新构造 frame、间接调用生成函数、写回 PC，再用全局 front cache
   查下一 TB。先在 Rust batching 内复用 frame，并为已知 fallthrough/branch target 缓存一到
   两个 successor arena index，可以在不做机器码 patching 的情况下减少重复 lookup。所有
   successor 都必须受 cache generation、SATP、privilege 和 budget guard 保护。

3. 缩短 software TLB hit path，并避免地址空间往返时清空全部 entry。

   当前 load/store TLB 各为 256-entry direct-mapped table；每个 24-byte entry 分别比较 VPN
   tag 和 generation。可以评估 16-byte packed tag、把 SATP/effective privilege/MPRV/SUM/MXR
   纳入 context tag，以及保留多个近期 context。这样 U/S trap 或进程 SATP 往返时不必丢掉
   另一个 context 的热 entry。任何压缩 tag 都必须证明无 false hit；`SFENCE.VMA` 仍必须让
   受影响的旧翻译不可达。

4. 为 TB builder 增加 page-local fetch translation cache。

   `GuestBlock::translate()` 当前对同一 code page 内的每条指令调用 `fetch_mem()`，每次都可能
   重新走 Sv39。TB 已被限制在单页内，可先通过 core 的 `translate_to_host()` 对该页执行一次
   Fetch 翻译，再从返回的 DRAM host page 解码后续 parcel。不能复制 Sv39 权限逻辑；跨页
   32-bit 指令、MMIO endpoint、A-bit 更新和 fault guest VA 仍由共享接口决定。

5. 用 `perf` 决定是否 inline Linux 热点原子操作。

   当前所有 LR/SC/AMO 都调用 Rust helper 并终止 TB。Linux 锁可能让该 helper 成为热点；若
   profile 证实，应先只对对齐 DRAM fast path 做 x86_64/Cranelift lowering，保留 terminator，
   再单独讨论是否允许 continuation。必须保留 LR/SC reservation、A/D 位、MMIO、异常 PC 和
   guest memory-order 语义，不能仅以“单 hart”为由删除这些边界。

6. 降低 JIT 编译与 finalize 成本。

   第一阶段统计快照中约 5586 个 compiled TB 累计花费 1.604 s，说明编译成本仍足以影响
   7 s 级启动。可比较按 TB 长度/回边/执行次数调整 hot threshold、批量 finalize 多个函数，
   或由独立 backend 后台编译后在 dispatcher 安全点安装。后台编译和批量 finalize 风险较高，
   应在确认 compilation samples 仍占当前最终树的显著比例后再做。

### P2：结构性优化

1. page-local trace/superblock，再评估 direct native block chaining。

   superblock 可以跨常见 branch，把多个 TB 的 GPR 保留在 SSA 中，减少寄存器数组写回、Rust
   间接调用和 PC lookup。它比简单调大 budget 更可能带来结构性收益，但必须让总 attempted
   始终受 Machine budget 限制，并为 exception、MMIO、TLB miss、atomic、system、WFI 和 IRQ
   保留精确 side exit。若进一步 patch native tail jump，还要处理 module rotation、code arena
   回收和所有旧 target 的原子失效，因此应放在 successor cache 之后。

2. context-tagged/superpage-aware TLB 与 page-walk cache。

   现有 TLB 以 4 KiB host page 填充，即使 Sv39 leaf 是 2 MiB/1 GiB 也不能复用更大 span。
   可以让 core 的 `TranslationTarget` 返回经过验证的 page size/span，再由 JIT 填充 superpage
   entry；也可缓存中间页表层级。页表遍历和 PTE 权限/A/D 更新仍只能由
   `translate_to_host()` 的共享逻辑决定，JIT 不得另写一份 Sv39 walker。

3. 精细化 `SFENCE.VMA` 失效。

   当前任意显式 SFENCE 都保守清空全部 decoded/native cache。可让 core 记录 rs1 VA、rs2
   ASID 和 fence generation，按 RISC-V 规则只失效匹配 TLB/TB；代码页依赖也需要记录 guest
   VA 到物理页的关系。该优化很容易制造 stale-code 或 stale-permission bug，只有统计显示
   SFENCE 导致大量重译时才值得做。

4. 只 lower 被证明安全且高频的 system/CSR 指令。

   第一阶段快照有约 63.9 万次 system fallback。可先按 opcode/CSR 细分；若 `cycle/time/instret`
   纯读占主导，可把 dispatcher 起始计数和 instruction offset 显式传给 native code。ECALL、
   xRET、WFI、SATP/MSTATUS 写及 fence 仍应保留边界。时间 CSR 必须与当前“一条 guest 指令
   一个 tick”的模型完全一致。

5. 事件驱动的设备 pending 与无 timer WFI 等待。

   UART 空轮询已经无锁，但 Machine 仍周期性调用每个设备的 `is_interrupting()`。可以把设备
   pending 发布到统一 atomic bitmap/PLIC，并让没有未来 timer 的 WFI 在宿主条件变量或 eventfd
   上等待。它主要降低空闲 CPU，而非当前 Debian prompt；还应先修清 level-triggered PLIC/
   SEIP 语义，避免把现有单脉冲兼容行为固化进优化路径。

### 当前低优先级

- 更复杂的 code LRU：第一阶段峰值 live code 约 3.47 MiB，远低于默认 128 MiB 上限，且没有
  code-cache flush；它不是当前 Debian 启动瓶颈。
- 单独增加 TLB replacement 复杂度：已有快照 hit rate 为 98.885%。应先区分 compulsory、
  direct-map conflict 和 context invalidation miss，再决定 2/4-way 是否值得。
- F/D native lowering：到 prompt 的快照只有约 3276 次 floating-point fallback，对 Debian
  启动的潜在收益很低；它更像 ISA 覆盖工作。
- 为少量 MMIO/cross-page slow path 增加专门机器码：这些路径必须以正确性为先，已有快照只有
  数千次，优先级低于 dispatcher、TLB context 和原子热点。
- persistent code cache 或跨进程复用：需要稳定 relocation、host feature、guest artifact 和
  失效协议，复杂度与当前 7 s 目标不匹配。

## 建议的下一轮顺序

1. 在当前 `b7bc41a` 代码基线上补 perf-map、stop-reason counters 和 stats-off `perf stat` 基线。
2. 先做 executor budget sweep；它改动最小，也能判断顶层 dispatch 是否仍是主要瓶颈。
3. 根据 profile 在 TLB context/packed entry、successor cache/frame reuse、fetch page cache 和
   inline atomic 中只选一个实施；每项单独 commit、单独三次 Debian A/B。
4. 补跑 workspace、96 个 `riscv-tests`、xv6、RustSBI 和 Debian guest 命令。
5. 只有前述小步优化不能继续降低热点时，才进入 superblock/direct chaining 或后台编译。

所有后续实现继续遵守两个不变量：JIT 不复制 Sv39 翻译/权限逻辑；任何 batching/chaining
都不能越过 Machine budget、精确 timer deadline 或 guest-visible side effect。
