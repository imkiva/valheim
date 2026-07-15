# Valheim JIT 性能记录与后续方向

本文档记录截至 2026-07-15 已经落地的 JIT/运行时性能优化、可复现的 Debian
启动数据，以及下一轮仍值得评估的方向。JIT 的总体架构、语义约束和第一阶段验收见
[`JIT-PLAN.md`](JIT-PLAN.md)。

启动性能优化实现到 `0265308`；之后的 WFI 语义与宿主空闲修复实现到 `9348812`。下面
“尚可评估的优化”均未实现，也不代表已经验证会更快。每个已落地优化都先独立验证、再单独
提交；没有收益的实验已经完整回滚。

## 范围与当前结论

- host 明确只支持 Linux x86_64 System V ABI。
- guest、Linux kernel、RustSBI、Debian rootfs 和未压缩内建 initramfs 均保持不变；本轮性能
  改动只发生在 Valheim。
- 性能主指标仍是宿主进程启动到真实行末 `debian13# ` prompt，包含 JIT 编译时间。
- 初始 JIT 的历史中位数为 13.447 s；上一轮 `b7bc41a` 工作树为 7.014 s。
- 当前 `0265308` 在固定 CPU 16 上三次为 4.814 / 4.757 / 4.806 s，中位数 4.806 s。
  这个绝对 checkpoint 与早期未绑核数据的调度条件不同；单项收益应引用下面的交错 A/B，
  不应直接用两个绝对 checkpoint 的差值归因。
- 相同固定 CPU 的 naive 中位数为 60.926 s，因此当前总体对照为 12.677×，启动时间减少
  92.1%；它不是逐项归因依据。旧的 69.603 / 13.447 s 数据继续作为第一阶段历史基线保留。
- Debian JIT prompt 后的 WFI 忙轮询已经修复：修复前稳定占用一个 host core；实测两个
  约 5 秒窗口均为 0 个 user/system tick，同时 UART 唤醒和超过 80 字节的连续输入输出通过。

当前运行时代码终点是 `9348812`；其后单独提交本文档，不混入代码改动。

## 测量口径

测量环境与 `JIT-PLAN.md` 相同：Linux 6.6.87.2 WSL2、AMD Ryzen 9 9950X3D、
32 logical CPUs、固定的 Debian demo artifacts，以及同一 release 构建。外部计时从进程
启动开始，到检测到真实行末 prompt 为止；因此镜像加载和全部运行期 JIT 编译成本都被计入。

计时运行默认关闭 `--jit-stats`，避免统计本身改变热路径。早期累计 checkpoint 如下：

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
| 完成 WFI/CLINT 后的 `b7bc41a` | 7.086 / 6.987 / 7.014 s；中位数 7.014 s |
| 当前 naive，固定 CPU 16 | 66.382 / 60.926 / 60.849 s；中位数 60.926 s |
| 当前 `0265308`，固定 CPU 16 | 4.814 / 4.757 / 4.806 s；中位数 4.806 s |

这些 checkpoint 在开发工作树上累计测得，之后才按职责拆成独立 Git commit；因此 Git 拓扑
顺序与测量顺序并不一一对应，不能用相邻行之差宣称单个 commit 的独立收益。可以单独引用的
A/B 结果是 compiled front cache；SATP/SFENCE 和 CLINT 对 Debian prompt 基本中性。

CLINT 的确定性收益不体现在这个 prompt 指标中：它把 hart 的长 WFI timer idle wait 从
O(`mtimecmp - mtime`) 次空转降为 O(1) 次 deadline 跳转。

prompt 空闲问题使用单独口径诊断。修复前 `perf stat` 的 3 秒窗口为 3001.15 ms task-clock，
即稳定使用 1.000 个 host CPU；JIT stats 中 dispatch 从 2000 万增长到 10.2 亿时 guest
instruction/native execution 完全不变，证明是 WFI dispatcher 空转。`9348812` 后使用
`/proc/<pid>/stat`、`CLK_TCK=100` 测得两个约 5.002 秒窗口均为 0 user/system tick；随后
`echo`、Debian 版本查询和 114 字节 UART 命令/输出均成功返回 prompt。WSL 对应 perf 前端在
最终复测时缺少当前 kernel tools，因此最终 0-tick 结果没有伪装成 perf 数据。

本轮新增优化使用相同 Debian artifacts、固定 CPU 16、关闭 stats，并交错运行旧/新二进制。
各行是各自独立实验，基线取样时间不同，因此不能把相邻行相减，也不能把所有倍数直接相乘：

| 独立优化 | 旧版中位数 | 新版中位数 | 独立结果 |
| --- | ---: | ---: | ---: |
| executor budget 32 → 1024 | 7.042948 s | 6.263692 s | 1.124408×；时间 -11.064% |
| generation-guarded successor cache | 6.321652 s | 5.789340 s | 1.091947×；时间 -8.420% |
| native exit fast path | 5.782143 s | 5.307218 s | 1.089486×；时间 -8.214% |
| batch 内复用 `JitFrame` | 5.117076 s | 5.018599 s | 1.019622×；时间 -1.924% |
| 默认 hot threshold 500 → 750 | 5.103571 s | 4.904941 s | 1.040496×；时间 -3.892% |
| release 关闭 Cranelift IR verifier | 4.928789 s | 4.541883 s | 1.085186×；时间 -7.850% |

最后一行只删除 release JIT 编译期的重复 IR 校验；启用 debug assertions 的构建（包括默认
`cargo test`）仍开启 verifier，所有 release 构建（包括 release tests）则关闭。verifier 本身
不是 guest 安全边界，但 malformed IR 在 release 可能更晚暴露为 panic 或错误机器码；因此
debug verifier、release verifier-off tests、ISA tests 和 demo 是不可省略的回归门禁。

当前 stats-on 快照记录 4496 个 compiled TB、1.813 s 累计编译时间、4.11 MB 生成代码、
277,803,697 次 native TB 执行、273,461,839 次 successor hit 和 3,305,991 次 miss，且
compile failure 为 0。TLB 为 384,604,333 hit / 552,731 miss（约 99.86% hit）；统计开启会
显著改变运行时间，这些数值只用于热点排序，不能与 stats-off 的 4.806 s 直接比较。

## 已完成并独立提交的优化

`b432c42..0265308` 的启动性能代码包含以下十六个独立提交；中间的 `d980f5e` 仅是上一版
性能文档，不属于代码优化。随后两个提交修复 WFI 正确性和 prompt 空闲 CPU。

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

### 第二轮 dispatcher 与编译期开销

11. `b1b1b15 perf(core): amortize executor dispatch overhead`

    Machine 的 executor ceiling 从 32 提高到 1024，减少 Machine/JIT dispatcher、CLINT 和
    pending-interrupt 检查次数。未来 timer deadline 仍会精确缩短 budget；上限继续约束 UART、
    VirtIO 等异步设备的最坏轮询延迟。

12. `d1d0a6f perf(jit): cache native block successors`

    每个 compiled block 增加 2-way successor cache，以完整 `TbKey` 和 cache generation
    保护 arena index。native batch 的常见边不再反复查询全局 front cache；跨 module 的 Rust
    索引链接仍由 module 生命周期和整体 generation 失效保护。stats-on 快照的 successor hit
    rate 约为 98.8%。

13. `32fd4a7 perf(jit): streamline native execution exits`

    `FAULT_NONE` 成为内联热路径，异常、MMIO、TLB miss 等处理移到 cold/out-of-line 函数；
    pending slow-memory 只传播 block 内 index，不再复制完整 `GuestInst`。精确 fault PC、raw
    instruction、attempted 计数和 guest exception 类型保持不变。

14. `c0d84ec perf(jit): reuse native frames within batches`

    一个 executor batch 只构造一次 `JitFrame`，successor 之间仅重置 block-local exit 字段。
    CPU、xregs、software TLB 和 stats 指针在该 batch 内保持稳定；任何可能替换这些 allocation
    或 cache generation 的路径都会先结束 batch。

15. `89ad510 perf(jit): retune default hot threshold`

    在完整 Debian 上 sweep 250/500/750/1000/1500/2000/3000 后，把统一默认阈值从 500 调为
    750；CLI 和 `JitExecutor` 默认同步更新，最小值仍为 1。正式无显式 threshold 的旧/新
    binary A/B 为 5.103571 s 对 4.904941 s。

16. `0265308 perf(jit): skip IR verification in release builds`

    Cranelift 0.112.3 默认会在一次 TB 编译的多个阶段重复运行 IR verifier。所有 release
    构建（包括 release tests）关闭这些编译期检查；启用 debug assertions 的构建（包括默认
    tests）仍显式开启，由此保留 lowering 开发期的早失败并移除 release 启动成本。

### WFI 正确性与宿主空闲

17. `8fd5057 fix(cpu): separate WFI wake from interrupt traps`

    locally-enabled pending interrupt 现在可以在全局 xIE 关闭时退出 WFI，但只有现有 global
    eligibility 检查通过时才进入 trap。wake-only 不清 MIP，individual enable 关闭也不会唤醒；
    Machine 和真实 JIT timer 用例同时覆盖 global-on trap 与 global-off wake-only。

18. `9348812 perf(core): block host thread during WFI idle`

    Bus 持有 generation + `Mutex` + `Condvar` 组成的共享 WakeHub。完整 `Machine::run`/
    `run_for_test` 在 WFI 没有本地启用的未来 timer 时按“snapshot → poll → recheck → wait”
    阻塞；UART 输入先发布 RX/IRQ 状态、释放 UART mutex，再通知 WakeHub，避免 lost wake 和
    锁序反转。公开的 `run_next()` 保持非阻塞，已有未来 timer 继续立即快进，因此本提交没有
    引入实时时钟语义。

## 当前验证状态

最终代码树已经通过：

- `cargo +nightly-2024-09-05 test --workspace --locked`
  - `valheim-asm` 11 个测试；
  - `valheim-core` 60 个测试；
  - `valheim-jit` 34 个单元测试；
  - 9 个 native/naive differential tests；
  - 4 个 native memory fast-path integration tests；
  - `xtask` 4 个测试。
- `cargo +nightly-2024-09-05 test --locked --package valheim-core --features trace`：61/61。
- `cargo +nightly-2024-09-05 test --release --locked --package valheim-jit`，让
  34 + 9 + 4 项 JIT tests 真正在 release verifier-off 配置执行。
- `cargo +nightly-2024-09-05 build --release --locked --package valheim-cli`。
- 当前树的 debug xtask 在 naive 与 JIT 下均通过 96/96 `riscv-tests`。
- 最终代码树串行实跑三个 demo 的 naive/JIT 六种组合：xv6 两种引擎均进入 `$` 并成功
  执行 `echo`；RustSBI 两种引擎均输出完整 success marker；Debian 两种引擎均进入真实
  `debian13#`，`cat /etc/debian_version` 均返回 `13.6`。Debian JIT 还在 prompt 空闲约 5 秒后
  由 UART 输入成功唤醒。

`cargo fmt --all -- --check` 会要求把仓库既有的 2 空格 Rust 风格整体改成 rustfmt 默认布局，
因此当前不能作为局部改动的有效格式门禁。本轮只运行过只读 `--check`，失败后没有产生文件
修改；`git diff --check` 已通过。

## 尚可评估的优化

以下优先级是基于当前代码结构和已有统计的工程判断，不是性能结论。下一轮应先用 `perf` 和
新的低开销 counters 确认热点，再一次只实现并提交一个方向。

### 已测但未采用

| 实验 | 基线中位数 | 实验中位数 | 结论 |
| --- | ---: | ---: | --- |
| 将成功 native TB 的 `cpu.instr` 写回推迟到 batch 结束 | 4.755523 s | 4.847325 s | 时间 +1.93%，回滚 |
| Cranelift `speed` → `speed_and_size` | 4.719314 s | 4.752441 s | 时间 +0.70%，回滚 |

前者减少了一次逐 TB store，却增加了 `Option` 状态在 native exit/dispatcher 间的传播和分支；
实测净负收益。后者没有用代码尺寸收益抵消生成代码速度变化。两项都没有 commit，工作树已恢复
到各自基线。hot threshold 正式三次 sweep 的中位数为 500: 5.146647 s、750: 4.966736 s、
1000: 5.002096 s、1500: 4.999855 s；不要在没有新 workload 数据时继续靠猜测改默认值。

### P0：继续完善 profile，而不是重复已完成工作

本机已安装 `rustfilt 0.2.1`，并验证
`/usr/lib/linux-tools/5.15.0-1103-kvm/perf` 可用 software `cpu-clock:u` 采样。Cranelift JIT
代码已经能在报告中显示为 `[JIT] tid ...` / `.Lfn...`；最新 leaf profile 中
`JitExecutor::execute` 为 35.50%、解释器 `RV64Cpu::execute` 5.81%、`translate_to_host()`
4.75%、`Bus::read` 1.92%、`GuestBlock::translate` 1.25%、`Machine::dispatch_next` 1.00%。
release 关闭 verifier 后，verifier 热点已经消失。

WSL 中 `cycles`、`instructions`、`branches` 和 `branch-misses` 仍报告 `<not supported>`；需要
可靠硬件计数时必须在 native Linux x86_64 复测，不能把空计数当作结果。下一步可观测性工作：

1. 给 `.Lfn...` 增加 guest PC、SATP/privilege 和 code range 元数据或 jitdump，使生成代码
   样本能按 guest TB 聚合，而不只是看到匿名函数。
2. 只在 `--jit-stats` 下增加 batch stop-reason、每 dispatch 的 native TB 数、atomic helper、
   SFENCE/FENCE.I 和 TLB context invalidation 原因；stats-off 机器码不得新增 counter 分支。
3. 继续使用同一 prompt harness 和交错 A/B；在 native Linux 上补 `perf stat -r 5`，再用
   `perf record -g` 定位累计 CPU 热点。stats-on 与 stats-off 时间不可直接比较。

### P1：优先做的下一批实验

1. 细分并 lower 高频 system/CSR fallback。

   当前 stats-on 快照有 679,131 次 system fallback，解释器执行本身占 profile 5.81%。先按
   opcode/CSR 计数；若 `cycle/time/instret` 等纯读占主导，可把 dispatcher 基准计数和 TB 内
   instruction offset 显式传给 native code。ECALL、xRET、WFI、SATP/MSTATUS 写和 fence 仍应
   保留边界，时间 CSR 必须与当前“一条 guest 指令一个 tick”模型一致。

2. 为 TB builder 增加 page-local fetch translation cache。

   `GuestBlock::translate()` 当前对同一 code page 内的每条指令调用 `fetch_mem()`，并在最新
   profile 占 1.25%。TB 已限制在单页内，可通过 core 的 `translate_to_host()` 对该页执行一次
   Fetch 翻译，再从返回的 DRAM host page 解码后续 parcel。不能复制 Sv39 权限逻辑；跨页
   32-bit 指令、MMIO endpoint、A-bit 更新和 fault guest VA 仍由共享接口决定。

3. 继续降低 JIT 编译与 finalize 成本。

   默认 threshold 已调到 750，release verifier 已关闭，但 stats-on 快照仍有 4496 个 compiled
   TB、1.813 s 累计编译时间。可评估批量 finalize、多函数 module 提交，或由独立 backend
   后台编译后在 dispatcher 安全点安装。后台编译和 module 生命周期风险较高，必须先把
   compile/finalize 子阶段单独计时；`speed_and_size` 已实测无益，不应重复。

4. 基于 guest-PC profile 决定是否 inline Linux 热点原子操作。

   当前所有 LR/SC/AMO 都调用 Rust helper 并终止 TB。只有 profile 证实它是热点时，才对对齐
   DRAM fast path 做 x86_64/Cranelift lowering，并继续保留 terminator。必须保留 LR/SC
   reservation、A/D 位、MMIO、异常 PC 和 guest memory-order 语义，不能以“单 hart”为由删边界。

5. 最后再评估 context-tagged / packed software TLB。

   当前 256-entry direct-mapped TLB 的最新 hit rate 已约 99.86%，单纯增加 replacement 复杂度
   很难成为首选。若 guest-PC/profile 显示 4.75% 的 `translate_to_host()` 主要来自 context
   invalidation 而不是 decoded/fallback fetch，再考虑 16-byte packed tag、近期多 context 或
   superpage entry；任何压缩 tag 都必须证明无 false hit，`SFENCE.VMA` 仍须使旧翻译不可达。

### P2：结构性优化

1. page-local trace/superblock，再评估 direct native block chaining。

   Rust 侧 generation-guarded successor cache 已完成，但每个 TB 仍返回 Rust、间接调用下一
   generated function 并写回架构状态。superblock 可跨常见 branch，把多个 TB 的 GPR 保留在
   SSA 中；direct tail jump 则可绕过 Rust 调度。两者都必须让总 attempted 受 Machine budget
   限制，并为 exception、MMIO、TLB miss、atomic、system、WFI 和 IRQ 保留精确 side exit。
   native target patch 还要处理 module rotation、code arena 回收及旧 target 的原子失效。

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

4. 完善 level-triggered PLIC/SEIP 与通用异步设备通知。

   无 timer WFI 的 WakeHub 等待和 UART 通知已经在 `9348812` 完成，并且实际解决了 Debian
   prompt 的单核满载。剩余工作是让 PLIC 在 source enable/threshold/complete 变化时从 latched
   level 重新计算 claim/SEIP，并把未来真正异步的 VirtIO 等设备接入同一通知协议。当前 UART
   单脉冲若恰在 source 10 被 mask 时到达，可能要等后续同源 UART 脉冲再次更新 pending，不能
   把 WakeHub 验收解释为完整 level-triggered PLIC 已实现。

5. 分离 realtime 与 deterministic CLINT clock mode。

   本轮按约定没有修正时钟语义：`mtime` 沿用现有 dispatcher/attempted-instruction tick 模型，
   未来 timer 在 WFI 中立即快进，无 timer 的宿主阻塞期间 guest 时间暂停。若后续需要与 DTB
   10 MHz 声明一致，应以宿主 monotonic clock 驱动 realtime 模式，并用 timer deadline 作为
   WakeHub timeout；ISA 测试和可重复 benchmark 则保留 deterministic instruction clock。该
   改动会影响启动等待和 guest 可见时间，必须独立设计、A/B 和提交。

### 当前低优先级

- 更复杂的 code LRU：当前峰值 live code 约 3.44 MiB，远低于默认 128 MiB 上限，且没有
  code-cache flush；它不是当前 Debian 启动瓶颈。
- 单独增加 TLB replacement 复杂度：最新快照 hit rate 约 99.86%。应先区分 compulsory、
  direct-map conflict 和 context invalidation miss，再决定 2/4-way 是否值得。
- F/D native lowering：到 prompt 的快照只有 2784 次 floating-point fallback，对 Debian
  启动的潜在收益很低；它更像 ISA 覆盖工作。
- 为少量 MMIO/cross-page slow path 增加专门机器码：这些路径必须以正确性为先，已有快照只有
  1690 次，优先级低于 dispatcher、system fallback 和编译成本。
- persistent code cache 或跨进程复用：需要稳定 relocation、host feature、guest artifact 和
  失效协议，复杂度与当前约 5 s 启动阶段不匹配。

## 建议的下一轮顺序

1. 以当前 `0265308` 为基线，补 guest-PC JIT 符号和 stop-reason/system-opcode counters；计时
   始终关闭 stats。
2. 在 system/CSR lowering 与 page-local fetch cache 中只选一个，独立实现、交错 A/B、独立
   commit；收益不稳定就完整回滚。
3. 把 compile/finalize 分段计时后，再决定批量 finalize 或后台编译是否值得；不要重复已经
   否决的 `speed_and_size` 实验。
4. 只有 guest-PC profile 证实热点后，才选 inline atomic 或 context-tagged TLB。
5. 小步优化不能继续降低 `JitExecutor::execute` 热点时，再进入 superblock/direct native
   chaining，并先设计 budget/side-exit/module-rotation 协议。
6. 每个落地优化继续跑 workspace、release JIT tests、96 个 `riscv-tests` 及三个 JIT demo；
   涉及 core/设备时再补 naive、trace 和双引擎完整回归。

所有后续实现继续遵守两个不变量：JIT 不复制 Sv39 翻译/权限逻辑；任何 batching/chaining
都不能越过 Machine budget、精确 timer deadline 或 guest-visible side effect。
