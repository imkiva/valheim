# Valheim JIT 性能记录与后续方向

本文档记录截至 2026-07-15 已经落地的 JIT/运行时性能优化、可复现的 Debian
启动数据，以及下一轮仍值得评估的方向。JIT 的总体架构、语义约束和第一阶段验收见
[`JIT-PLAN.md`](JIT-PLAN.md)。

启动性能优化实现到 `0265308`；之后的 WFI 空闲修复实现到 `9348812`、
按目标特权投递中断实现于 `a0038d8`、realtime CLINT/TIME 实现于 `10cabc6`，
历史 RustSBI 的 timer relay 适配实现于 `258fdf6`。level-triggered PLIC、可供 Linux
使用的 legacy VirtIO block 和 writable ext4 Debian root 分别实现于 `e14e41e`、
`bcd378d`、`ded32a0`。后三项首先是设备正确性和 workload 变更，不应把它们当作 JIT
提速提交。ext4 workload 上的本轮设备/JIT 优化实现到 `746684b`；每项先独立验证、再单独
提交，没有把多个优化压进同一个 commit。下面“尚可评估的优化”均未实现，也不代表已经
验证会更快；没有收益的实验已经完整回滚或明确记录为性能中性。

## 范围与当前结论

- host 明确只支持 Linux x86_64 System V ABI。
- 截止 `0265308` 的性能 A/B 保持 guest、Linux kernel、RustSBI、Debian rootfs
  和 gzip 压缩的内建 initramfs 不变。realtime 验收为兼容旧 RustSBI 新增 timer relay
  patch；它是时钟正确性适配，不混入旧性能改动的归因。
- `ded32a0` 起 Debian 改为从 `/dev/vda` 的 read-write ext4 启动，kernel 不再内嵌
  initramfs。本轮已建立固定 CPU 16、threshold 750 的正式三次交错 A/B 和 software
  `cpu-clock:u` profile；旧 initramfs 数据仍只作历史对照。
- 性能主指标仍是宿主进程启动到真实行末 `debian13# ` prompt，包含 JIT 编译时间。
- 初始 JIT 的历史中位数为 13.447 s；上一轮 `b7bc41a` 工作树为 7.014 s。
- realtime 切换前的 `0265308` 在固定 CPU 16 上三次为
  4.814 / 4.757 / 4.806 s，中位数 4.806 s。
  这个绝对 checkpoint 与早期未绑核数据的调度条件不同；单项收益应引用下面的交错 A/B，
  不应直接用两个绝对 checkpoint 的差值归因。
- 相同历史口径的 naive 中位数为 60.926 s，对照为 12.677×，启动时间
  减少 92.1%。旧的 69.603 / 13.447 s 数据继续作为第一阶段历史基线保留。
  这些数据都基于 instruction-driven/fast-forward clock，不能与 realtime 树的
  绝对时间直接比较。
- `10cabc6` + `258fdf6` 后，固定 CPU 16 的 realtime JIT 三次为
  8.903906 / 8.379352 / 8.321334 s，中位数 8.379352 s。新语义不再将 guest
  等待快进，所以这是新基线，不是 JIT 热路径退化的独立 A/B。
- 同口径 realtime naive 额外做了一次完整验收，为 133.996711 s。该单次值
  不是三次中位数，不用它声称新的正式加速比。
- Debian JIT prompt 后的 WFI 忙轮询已经修复：修复前稳定占用一个 host core；实测两个
  约 5 秒窗口均为 0 个 user/system tick，同时 UART 唤醒和超过 80 字节的连续输入输出通过。
- ext4 初始树 `ded32a0` 与当前 `746684b` 的三次交错中位数为 3.957267 s 与
  3.371372 s，累计缩短 14.806%（1.1738×）。这个累计数包含 VirtIO、PLIC 和 JIT 多项提交，
  不能用于声称任何单项的独立收益。

当前运行时、设备和 JIT 代码终点是 `746684b`；其后单独提交本文档，不混入代码改动。

## 测量口径

测量环境与 `JIT-PLAN.md` 相同：Linux 6.6.87.2 WSL2、AMD Ryzen 9 9950X3D、
32 logical CPUs，历史 kernel/rootfs/initramfs artifacts 固定。realtime 前后使用表中各自标明的
release binary，realtime firmware 另含 timer relay patch；只有每项独立 A/B 才保证
除被测改动外的条件相同。外部计时从进程启动开始，到检测到真实行末 prompt 为止；
因此镜像加载和全部运行期 JIT 编译成本都被计入。下表前半保留旧 initramfs workload，
末两行是本轮 ext4 direct-root 的新口径，二者不能交叉比较绝对时间。

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
| realtime 前 naive，固定 CPU 16 | 66.382 / 60.926 / 60.849 s；中位数 60.926 s |
| realtime 前 `0265308`，固定 CPU 16 | 4.814 / 4.757 / 4.806 s；中位数 4.806 s |
| realtime JIT，`258fdf6`，固定 CPU 16 | 8.903906 / 8.379352 / 8.321334 s；中位数 8.379352 s |
| realtime naive，`258fdf6`，固定 CPU 16 | 单次验收 133.996711 s；非正式中位数 |
| ext4 初始树 `ded32a0`，固定 CPU 16 | 3.787534 / 4.023637 / 3.957267 s；中位数 3.957267 s |
| ext4 当前树 `746684b`，固定 CPU 16 | 3.263766 / 3.371372 / 3.540285 s；中位数 3.371372 s |

这些 checkpoint 在开发工作树上累计测得，之后才按职责拆成独立 Git commit；因此 Git 拓扑
顺序与测量顺序并不一一对应，不能用相邻行之差宣称单个 commit 的独立收益。可以单独引用的
A/B 结果是 compiled front cache；SATP/SFENCE 和当时的 instruction-clock CLINT
fast-forward 对 Debian prompt 基本中性。

`254db75` 当时把 instruction-clock 下的长 WFI timer idle wait 从
O(`mtimecmp - mtime`) 次空转降为 O(1) 次 deadline 跳转。该模型已被
`10cabc6` supersede：当前不跳变 `mtime`，而是阻塞到宿主 realtime deadline。

prompt 空闲问题使用单独口径诊断。修复前 `perf stat` 的 3 秒窗口为 3001.15 ms task-clock，
即稳定使用 1.000 个 host CPU；JIT stats 中 dispatch 从 2000 万增长到 10.2 亿时 guest
instruction/native execution 完全不变，证明是 WFI dispatcher 空转。`9348812` 后使用
`/proc/<pid>/stat`、`CLK_TCK=100` 测得两个约 5.002 秒窗口均为 0 user/system tick；随后
`echo`、Debian 版本查询和 114 字节 UART 命令/输出均成功返回 prompt。WSL 对应 perf 前端在
最终复测时缺少当前 kernel tools，因此最终 0-tick 结果没有伪装成 perf 数据。

realtime 树上重新验收 prompt 空闲：JIT 的 `pidstat -p PID 1 3` 三个窗口均为
0.00% CPU；naive 的 10 个 1 秒窗口中 8 个为 0%、2 个为 1%，平均 0.20%。
Debian `time sleep 1` 分别为 JIT 1.100 s、naive 1.144 s，`riscv-timer` IRQ 计数
前后都持续增长。这证明 timed WFI 同时保留了实时 timer 转发和宿主空闲。

realtime 切换前的本轮新增优化使用相同 Debian artifacts、固定 CPU 16、关闭 stats，
并交错运行旧/新二进制。
各行是各自独立实验，基线取样时间不同，因此不能把相邻行相减，也不能把所有倍数直接相乘：

| 独立优化 | 旧版中位数 | 新版中位数 | 独立结果 |
| --- | ---: | ---: | ---: |
| executor budget 32 → 1024 | 7.042948 s | 6.263692 s | 1.124408×；时间 -11.064% |
| generation-guarded successor cache | 6.321652 s | 5.789340 s | 1.091947×；时间 -8.420% |
| native exit fast path | 5.782143 s | 5.307218 s | 1.089487×；时间 -8.214% |
| batch 内复用 `JitFrame` | 5.117076 s | 5.018599 s | 1.019622×；时间 -1.924% |
| 默认 hot threshold 500 → 750 | 5.103571 s | 4.904941 s | 1.040496×；时间 -3.892% |
| release 关闭 Cranelift IR verifier | 4.928789 s | 4.541883 s | 1.085186×；时间 -7.850% |

最后一行只删除 release JIT 编译期的重复 IR 校验；启用 debug assertions 的构建（包括默认
`cargo test`）仍开启 verifier，所有 release 构建（包括 release tests）则关闭。verifier 本身
不是 guest 安全边界，但 malformed IR 在 release 可能更晚暴露为 panic 或错误机器码；因此
debug verifier、release verifier-off tests、ISA tests 和 demo 是不可省略的回归门禁。

realtime 切换前的 stats-on 快照记录 4496 个 compiled TB、1.813 s 累计编译时间、
4.11 MB 生成代码、
277,803,697 次 native TB 执行、273,461,839 次 successor hit 和 3,305,991 次 miss，且
compile failure 为 0。TLB 为 384,604,333 hit / 552,731 miss（约 99.86% hit）；统计开启会
显著改变运行时间，这些数值只用于热点排序，不能与 stats-off 的 4.806 s 直接比较。

### ext4 workload 本轮 A/B（`ded32a0..746684b`）

本轮 prompt 计时固定 CPU 16、显式 `--jit-hot-threshold 750`、关闭 stats，并为每次启动从
已校验的只读 ext4 base 创建新的 reflink/sparse 可写副本。kernel、RustSBI、rootfs、cmdline
和 host 环境保持一致；六次按旧/新、新/旧、旧/新顺序交错。Bash 会在 prompt 前输出
bracketed-paste 控制序列，harness 精确匹配真实的 `ESC[?2004hdebian13# `，不会误命中
`/init` 的说明文字。两个 endpoint 之间还包含纯文档提交
`0cc8a5e docs: record writable ext4 block root`；它不属于下文十个优化，也不影响机器码 A/B。

| 独立优化 | 旧版三次 | 新版三次 | 中位数与结论 |
| --- | --- | --- | --- |
| 缓存完整 fallback 指令，`479004c→676393d` | 3.183915 / 3.612101 / 3.371539 s | 3.314642 / 3.517692 / 3.293438 s | 3.371539→3.314642 s；时间 -1.688%，1.0172×；收益小且单次噪声明显 |
| TB 页内 fetch translation cache，`281a608→746684b` | 3.342623 / 3.318830 / 3.471942 s | 3.203207 / 3.216653 / 3.259755 s | 3.342623→3.216653 s；时间 -3.769%，1.0392×；三个配对方向一致 |
| 本轮累计，`ded32a0→746684b` | 3.787534 / 4.023637 / 3.957267 s | 3.263766 / 3.371372 / 3.540285 s | 3.957267→3.371372 s；时间 -14.806%，1.1738×；不可拆给单项归因 |

当前树另采一份 stats-on 快照；最后一行恰在 9,800,000 次 dispatch，略早于真实 prompt，
所以它只用于数量级和热点排序：88,608,106 条 guest instruction、8,863,850 次 decoded TB、
9,851,944 次 native TB、322,766 条 fallback（其中 system 298,865、F/D 5,220、other
17,067、memory 1,614），negative cache hit 276,766；编译 2,241 个 TB，累计 658.298 ms，
compile failure 为 0。software TLB 为 15,374,824 hit / 90,748 miss（99.413% hit）。stats-on
到 prompt 用时 6.373452 s，不能与 stats-off 的约 3.4 s 比较。

software `cpu-clock:u` 的相邻 TB fetch-page-translation-cache A/B 各约 3,000 个样本、
lost sample 为 0。
`translate_to_host()` 的 leaf share 从 9.69% 降到 8.10%；结合各自 event count，采样 CPU 时间
约从 339.1 ms 降到 249.7 ms（-26.35%）。`GuestBlock::translate()` 约从 207.5 ms 降到
188.7 ms（-9.07%）。百分比会受其他热点占比变化影响，因此以交错 prompt A/B 作为主结论。

## 已完成并独立提交的优化

`b432c42..0265308` 的启动性能代码包含以下十六个独立提交；中间的 `d980f5e` 仅是上一版
性能文档，不属于代码优化。随后五个代码提交修复 WFI/中断正确性、prompt
空闲 CPU、realtime 时钟和历史固件中继；再后的三个独立提交完善 PLIC、VirtIO block 与
ext4 workload，主要属于设备正确性和覆盖，不作 JIT 启动提速归因。

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
    pending-interrupt 检查次数。在该提交当时，未来 timer deadline 仍会精确缩短
    budget；上限继续约束 UART、
    VirtIO 等异步设备的最坏轮询延迟。这是该历史提交当时的模型；
    `10cabc6` 后 active execution 始终使用 1024 budget，realtime deadline 不对应
    instruction budget。

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
    `a0038d8` 后 global eligibility 不再按“当前 mode 的一个 xIE”统一判断。

18. `9348812 perf(core): block host thread during WFI idle`

    Bus 持有 generation + `Mutex` + `Condvar` 组成的共享 WakeHub。完整 `Machine::run`/
    `run_for_test` 在 WFI 没有本地启用的未来 timer 时按“snapshot → poll → recheck → wait”
    阻塞；UART 输入先发布 RX/IRQ 状态、释放 UART mutex，再通知 WakeHub，避免 lost wake 和
    锁序反转。公开的 `run_next()` 保持非阻塞，已有未来 timer 继续立即快进，因此本提交没有
    引入实时时钟语义。其 future-timer fast-forward 部分已被 `10cabc6`
    的 timed wait 取代。

19. `a0038d8 fix(cpu): honor interrupt target privilege`

    pending interrupt 现在逐候选结合 `MIDELEG` 判断目标特权。S-mode 即使
    `SIE=0` 也会立即进入更高特权的 MTI；delegated STI 仍受 SIE 约束，
    且不能反向中断 M-mode。WFI 唤醒仍与 global enable/delegation 解耦。

20. `10cabc6 fix(core): drive CLINT from host monotonic time`

    production `mtime` 的初始 guest anchor 为 0，之后按
    `guest_anchor + host monotonic elapsed × 10 MHz` 推进；MMIO 写 `mtime` 会同时重设
    guest/host anchor，随后继续实时推进。`TIME` 是同一计数器的 live read-only 视图，
    DTB 从同一常量生成 frequency。active execution 始终使用 1024 budget，timer delta
    不再映射为指令数；WFI 以 absolute `Instant` deadline + WakeHub generation 等待，
    UART 可提前打断。`ManualClock` 只是测试注入，不是 production deterministic/turbo mode。

21. `258fdf6 fix(demo): rearm RustSBI timer relay`

    历史 RustSBI 的 MachineTimer handler 会置 STIP 并清 MTIE。SBI TIME
    `set_timer` 现在按“屏蔽 MTIE → 更新 `mtimecmp` → 清旧 STIP → 重开 MTIE”
    首次建立并在后续重装中继，避免旧 deadline 竞态；未打 patch 时，当前初始化路径
    不会打开 MTIE，Linux 无法可靠获得 MTIP→STIP timer relay。

22. `e14e41e fix(plic): model level-triggered interrupt sources`

    PLIC 现在区分 pending、electrical level 和 in-service context；priority、enable、
    threshold、claim 与 complete 都会重算 M/S context 的 claim，level 在 complete 后仍高会
    重新 pending。CPU 每轮从 S-context claimability 重建 SEIP，避免把外部中断当成一次性脉冲。

23. `bcd378d fix(virtio): implement legacy block split queues`

    legacy VirtIO-MMIO v1 block 现在按 queue size 8 解析 direct split descriptor chain，批量
    drain avail ring，执行 IN/OUT/FLUSH，写 used ring/status，并以 interrupt-status ACK 控制
    PLIC level。DMA range 和 access direction 会完整校验；guest DMA write 清除 LR/SC reservation。
    mmap backend 测试覆盖 flush 成功、重新打开文件可见和越界；注入式 backend 单独覆盖 flush 失败，
    Linux reset/GuestPageSize 顺序也有回归测试。

24. `ded32a0 feat(demo): boot Debian from writable ext4`

    固定 Debian OCI layer 现在在单个 fakeroot 会话中转换为经过 fsck/debugfs 校验的 256 MiB
    sparse ext4 base，再复制成受锁保护的持久化 runtime。Linux 5.17 以内建 VirtIO block/ext4
    直接挂载 `/dev/vda` 为 `rw`；`RESET_DISK=1` 显式重置。实跑验证写入、`sync`、journal
    recovery、跨进程持久化、重置和 prompt 空闲；该提交当时尚未建立正式启动性能基线，
    本轮基线与优化数据见上文。

### ext4 workload 的设备与 JIT 优化

设备吞吐 A/B 同样固定 CPU 16、JIT threshold 750；每个样本从只读 base 新建独立
`cp --reflink=auto` 可写盘。guest 写命令为
`dd if=/dev/zero of=/root/io-bench.bin bs=1M count=64 conv=fsync status=none`；同一 clone 随后
读三次，每次先在计时区间外执行 `sync` 和 `echo 3 > /proc/sys/vm/drop_caches`，再计时
`dd if=/root/io-bench.bin of=/dev/null bs=1M status=none`。因此数据是 guest-cold、host-hot 的
emulator/VirtIO 路径，不是宿主裸盘测试；吞吐统一用 64 MiB 除以 Bash `EPOCHREALTIME`
测得的 dd 时间。除最早 queue-8 基线为两个 clone（2 写/6 读）外，各 variant 都是三个
clone（3 写/9 读），并核对 `/dev/root` 为 `rw,relatime` ext4。

25. `2edde12 perf(plic): skip unchanged source updates`

    pending bit 已经为 1 或 electrical level 没有变化时，不再重算两个 context 的 claim。
    新 pending、claim/complete、priority/enable/threshold 和 asserted-level re-pend 仍走原有
    路径，因而只消除重复通知，不改变 PLIC 可见状态。

26. `21e5b44 perf(virtio): expand the legacy block queue`

    QueueNumMax 从 8 扩到 128；设备继续按 guest 实际写入的 QueueNum 工作，测试同时覆盖 8
    和 128。真实 Linux 冷读吞吐相对 queue 8 提升约 9.4%（耗时约减少 8.6%），三组观察点的
    VirtIO IRQ 分别减少约 40% / 27% / 30%；写入没有可信提升。

27. `055b3f9 perf(virtio): advertise block segment limits`

    feature/config 正确公布 `VIRTIO_BLK_F_SEG_MAX`、FLUSH、`size_max=0` 和
    `seg_max=126`。128 个 direct descriptors 扣除 header/status 后可完整服务 126 个 data
    segment；Linux 实测 `/sys/block/vda/queue/max_segments=126`。固定冷读中位数从
    0.984040 s 降到 0.917611 s（-6.75%，1.0724×，约 65.04→69.75 MiB/s），写中位数
    3.270611→3.271469 s，实质不变。

28. `7b9a925 perf(virtio): reuse descriptor chain storage`

    device 内复用 descriptor vector，并用 QueueNum 上界终止 cycle，不再为每个 request 分配
    descriptor/visited storage。该 variant 的写/读中位数为 3.470095 / 0.924044 s：写入波动
    较大，读取与相邻稳定基线接近，未证明稳定吞吐收益。保留原因是降低每请求固定分配，并为
    未来抽取通用队列解析器减少热路径分配。

29. `81a697f perf(virtio): reuse block transfer scratch`

    非 mmap backend 的 64 KiB transfer scratch 在挂载 backend 时分配一次，并明确
    `read_at` 必须完整填充或报错。该 variant 的写/读中位数为 3.335542 / 0.947467 s，
    同样没有稳定收益；这是去掉热路径 allocation 的结构性改进。

30. `cc7a20e perf(virtio): copy mapped block data directly`

    mmap backend 可借用经过 checked-range 验证的 slice，guest DRAM 也提供只读 checked slice；
    IN/OUT 因而各只保留必需的一次 copy，其他 backend 仍用 scratch。三次写中位数
    3.460905 s，波动较大；九次冷读中位数 0.914786 s（69.96 MiB/s），与 SEG_MAX 稳定基线
    0.917611 s 实质相同，不能声称额外吞吐提升。

31. `479004c perf(plic): scan only enabled pending sources`

    claim recompute 改为逐 word 计算 `pending & enable`，再用 `trailing_zeros` 只枚举候选。
    word/bit 升序和严格 `priority > best` 保留“最高优先级、同级最低 IRQ”；跨 word、未 enable
    高优先级源和 level complete 均有回归。

32. `676393d perf(jit): cache decoded fallback instructions`

    negative cache 保存 fallback 的 `pc/raw/len/decoded`，命中时设置 `cpu.instr` 并直接调用
    共享 `cpu.execute()`，不再让 naive 重复 fetch/decode。TIME/CSR/WFI 仍读取 live CPU/CLINT
    状态；FENCE.I/SFENCE epoch 继续整体失效。页尾 compressed 和 32-bit 第二 parcel 只有
    fetch/decode 成功后才可缓存。Debian 独立 A/B 为 -1.688%，属于小幅收益。

33. `281a608 perf(virtio): batch used index publication`

    一次 notify drain 先写所有 used element，最后只发布一次最终 `used.idx`。只有 index 发布
    成功才报告 completion/IRQ；request data/status 的 DMA 标记、malformed request、
    `NO_INTERRUPT` 和 `u16` wrap 语义保持不变。

34. `746684b perf(jit): cache TB fetch page translation`

    TB builder 对普通 DRAM code page 只调用一次共享 `translate_to_host(Fetch)`，后续 parcel
    复用返回的 4 KiB host page。它不复制 Sv39 walker/权限逻辑；odd PC、页尾和 MMIO 仍走
    `fetch_mem()`，16-bit low parcel 判定为 32-bit 后才读取 high parcel。Sv39 X/A、非同址
    映射、MMIO、guest fault VA、非对齐 host pointer 和跨页第二 parcel 均有测试。独立 prompt
    A/B 为 -3.769%，且采样中的 `translate_to_host()` CPU 时间约下降 26.35%。默认 hot
    threshold 在整个本轮始终保持 750。

## 当前验证状态

最终代码树已经通过：

- `cargo +nightly-2024-09-05 test --workspace --locked`
  - `valheim-asm` 11 个测试；
  - `valheim-core` 102 个测试；
  - `valheim-jit` 40 个单元测试；
  - 10 个 native/naive differential tests；
  - 4 个 native memory fast-path integration tests；
  - `xtask` 4 个测试。
- 上述 workspace 合计 171/171；
  `cargo +nightly-2024-09-05 test --locked --package valheim-core --features trace`：103/103。
- `cargo +nightly-2024-09-05 test --release --locked --package valheim-jit`，让
  40 + 10 + 4 = 54 项 JIT tests 真正在 release verifier-off 配置执行。
- `cargo +nightly-2024-09-05 build --release --locked --package valheim-cli`。
- 当前树的 debug xtask 在 naive 与 JIT 下均通过 96/96 `riscv-tests`。
- 当前 `746684b` 代码树实跑三个 demo 的 naive/JIT 六种组合：xv6 两种引擎
  均进入 `$` 并成功执行 `echo`；RustSBI 两种引擎均输出完整 success marker；
- Debian 两种引擎均从 read-write ext4 进入真实 `debian13#`，版本为 `13.6`，
  `/dev/root` 均为 `rw,relatime` ext4，`max_segments` 为 126。JIT 进程写入并 `sync` 的
  marker 可由随后启动的 naive 进程读取，验证了跨进程持久化。
- Debian 两种引擎均输出 `SBI TIME extension detected`，且 `riscv-timer` IRQ 持续增长。
  从提交完整命令到观察到 guest `TIMER_END` marker 计时，`sleep 1` 为 JIT 1.482 s、naive
  1.819 s；对应 timer IRQ 分别从 783 增至 836、从 2072 增至 2169。另一次独立的 prompt
  空闲 5.005 s 窗口中，宿主进程分别只增加 3 和 12 个 `CLK_TCK=100` CPU tick，约为 0.6%
  和 2.4% 单核占用，均未恢复成满核忙轮询。
- Debian JIT 还验证写入、`sync`、journal recovery、显式 reset 和非零 VirtIO IRQ。
  backend FLUSH 的成功/失败和 mmap 落盘语义由独立单元测试覆盖，不能从 guest `sync`
  单独归因某一次具体 request。

`cargo fmt --all -- --check` 会要求把仓库既有的 2 空格 Rust 风格整体改成 rustfmt 默认布局，
因此当前不能作为局部改动的有效格式门禁。本轮只运行过只读 `--check`，失败后没有产生文件
修改；`git diff --check` 已通过。

## 尚可评估的优化

以下优先级是基于当前 `746684b` ext4 workload 的 profile、stats 和代码结构作出的工程判断，
不是尚未测量方向的性能承诺。后续仍应一次只实现、验证并提交一个优化；独立 A/B 没有稳定
收益时应回滚代码，只在本文档记录结论。

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
代码已经能在报告中显示为 `[JIT] tid ...` / `.Lfn...`。当前 ext4 stats-off profile 的 leaf
share 中，`JitExecutor::execute` 为 19.32%、`RV64Cpu::execute` 为 14.28%、
`translate_to_host()` 为 8.10%、`GuestBlock::translate()` 为 6.12%、`Bus::read` 为 3.35%；
host realtime clock 相关 leaf 合计仍约 8%–9%。`Virtio::service_queue` 只有 0.42%，PLIC
低于 0.2%。leaf 百分比会随调用内联和其他热点占比变化，不能相加成路径占比，但足以说明当前
Debian 启动首先应继续看 JIT、共享执行/MMU 和时钟，而不是继续猜测 VirtIO/PLIC 微优化。

当前 stats-on 数据也显示 system 占 322,766 条 fallback 中的 298,865 条；同时 native
software TLB 已有 99.413% hit。下一轮最有价值的可观测性工作是：

1. 给 `.Lfn...` 增加 guest PC、SATP/privilege 和 code range 元数据或 jitdump，使生成代码
   样本能按 guest TB 聚合，而不只是看到匿名函数。
2. 只在 `--jit-stats` 下细分 system opcode/CSR、batch stop reason、每 dispatch 的 native TB
   数、翻译调用方与 miss 原因、atomic helper、SFENCE/FENCE.I 和 TLB context invalidation；
   stats-off 的 Rust/native 热路径不得新增 counter 分支。
3. 把 658.298 ms 累计编译时间继续拆成 translate、Cranelift compile、finalize 和 module
   rotation，避免未分段就选择批量提交或后台编译。
4. 继续使用同一 prompt harness 和交错 A/B；stats-on 与 stats-off 时间不可直接比较。

WSL 中 `cycles`、`instructions`、`branches` 和 `branch-misses` 仍报告 `<not supported>`；需要
可靠硬件计数时必须在 native Linux x86_64 复测，不能把空计数当作结果。

### P1：优先做的下一批实验

1. 细分并 lower 高频 system/CSR fallback。

   当前快照中 system 是 fallback 的绝对主体，但必须先按 opcode/CSR 计数；若
   `cycle/instret` 等纯读占主导，可把 dispatcher 基准计数和 TB 内 instruction offset
   显式传给 native code。`time/rdtime` 必须每次从 host-clock-backed CLINT live 读取或调用
   等价 helper，绝不能从 instruction offset 合成。ECALL、xRET、WFI、SATP/MSTATUS 写、
   `FENCE.I` 和 `SFENCE.VMA` 仍应保留精确边界。

2. 为 decoded/解释器路径增加共享的 core translation cache。

   TB 页内 fetch cache 已把普通 DRAM code page 内的 builder 重复页翻译去掉，但当前
   `translate_to_host()` 仍有 8.10% leaf share；native software TLB 的 99.413% hit 并不能覆盖
   decoded/fallback、取指和 core slow path。可在 core 层按 VPN、访问类型、privilege、
   MPRV/SUM/MXR、SATP/ASID 与 generation 缓存普通 DRAM translation target，所有 miss
   仍只能调用唯一的
   `translate_to_host()` 填充。必须保留 PTE A/D 更新、最终 endpoint fault、fragment guest VA、
   `SFENCE.VMA`/SATP/MSTATUS 失效和 MMIO side effect；JIT 不得私自维护第二份 Sv39 权限逻辑。

3. 测量并削减 realtime clock 热路径。

   host clock 相关 leaf 合计仍约 8%–9%。先按 interrupt poll、TIME CSR、CLINT MMIO 和 WFI
   deadline 细分调用次数；随后可独立评估减少同一 dispatcher 内的重复 `Instant::now()`，以及
   用秒/纳秒的 10 MHz 精确换算替代通用 `u128` 路径。TIME 必须保持 live read，MTIP 不得提前，
   WFI 仍使用绝对 deadline 并允许 WakeHub 提前唤醒，不能为了 benchmark 恢复 instruction clock。

4. 继续降低 JIT 编译与 finalize 成本。

   当前 stats-on 到 prompt 编译 2,241 个 TB、累计 658.298 ms。分段计时后可评估批量 finalize、
   多函数 module 提交，或由独立 backend 后台编译后在 dispatcher 安全点安装。后台编译和 module
   生命周期风险较高；`speed_and_size` 已实测无益，不应重复。

5. 基于 guest-PC profile 决定是否 inline Linux 热点原子操作。

   当前所有 LR/SC/AMO 都调用 Rust helper 并终止 TB。只有 profile 证实它是热点时，才对对齐
   DRAM fast path 做 x86_64/Cranelift lowering，并继续保留 terminator。必须保留 LR/SC
   reservation、A/D 位、MMIO、异常 PC 和 guest memory-order 语义，不能以“单 hart”为由删边界。

### P2：结构性优化

1. 实现 VirtIO indirect descriptors。

   queue 128 + `SEG_MAX=126` 已明显改善冷读，但仍只有 direct split descriptors。indirect table
   可减少主 descriptor table 压力。该 feature 必须单独协商、实现和提交，并覆盖 table
   对齐/长度、越界、cycle、indirect nesting 和 descriptor 方向。
   legacy version 1 与现代 version 2 是另一项兼容工作，不能在同一优化中顺带切换。

2. 实现 VirtIO `EVENT_IDX`。

   当前一次 notify drain 已批量发布 used index，但 guest/device 通知仍使用基础 flag 语义。
   `EVENT_IDX` 可按 ring event index 更精确地抑制通知；必须单独协商、A/B 和提交，正确处理
   `u16` wrap、`vring_need_event`、空批次、`NO_NOTIFY`/`NO_INTERRUPT` fallback 和 level IRQ。

3. 为真实 block backend 设计异步请求生命周期。

   当前 mmap backend 在 Machine 线程内同步执行 copy/flush；对 host-hot 256 MiB image 足够，
   但真实文件或更大 rootfs 会把宿主 I/O latency 串行化。后台 I/O 需要拥有稳定的 request
   元数据；worker 只通过 WakeHub 通知完成，Machine 线程再发布 DMA/status、used ring 和 level
   IRQ，并正确处理 reset、queue reconfigure、guest buffer 生命周期与 emulator 退出。应先用
   host-cold 或受控 backend latency 证明收益。

4. 按 dirty range 缩小 FLUSH 工作量。

   当前 mmap FLUSH 面向整个 mapping。若 write-heavy profile 证明它是热点，可跟踪 block-aligned
   dirty range 并合并 flush；必须保证 guest 已完成的 OUT 在成功 FLUSH 后具备原有持久性，处理
   overlap、reset 和 host page alignment。不能用延迟或丢弃 FLUSH 换取表面吞吐。

5. page-local trace/superblock，再评估 direct native block chaining。

   Rust 侧 generation-guarded successor cache 已完成，但每个 TB 仍返回 Rust、间接调用下一
   generated function 并写回架构状态。superblock 可跨常见 branch，把多个 TB 的 GPR 保留在
   SSA 中；direct tail jump 则可绕过 Rust 调度。两者都必须让总 attempted 受 Machine budget
   限制，并为 exception、MMIO、TLB miss、atomic、system、WFI 和 IRQ 保留精确 side exit。
   native target patch 还要处理 module rotation、code arena 回收及旧 target 的原子失效。

6. context-tagged/superpage-aware TLB 与 page-walk cache。

   现有 TLB 以 4 KiB host page 填充，即使 Sv39 leaf 是 2 MiB/1 GiB 也不能复用更大 span。
   可以让 core 的 `TranslationTarget` 返回经过验证的 page size/span，再由 JIT 填充 superpage
   entry；也可缓存中间页表层级。页表遍历和 PTE 权限/A/D 更新仍只能由
   `translate_to_host()` 的共享逻辑决定，JIT 不得另写一份 Sv39 walker。

7. 精细化 `SFENCE.VMA` 失效。

   当前任意显式 SFENCE 都保守清空全部 decoded/native cache。可让 core 记录 rs1 VA、rs2
   ASID 和 fence generation，按 RISC-V 规则只失效匹配 TLB/TB；代码页依赖也需要记录 guest
   VA 到物理页的关系。该优化很容易制造 stale-code 或 stale-permission bug，只有统计显示
   SFENCE 导致大量重译时才值得做。

### 本轮已从候选移除

realtime CLINT 已在 `10cabc6` 完成：production 仅使用 host monotonic 10 MHz
clock，WFI 不快进，`ClockSource`/`ManualClock` 仅用于无 sleep 的可重复测试。
若未来需要 CLI 可选 deterministic benchmark mode，那是新功能，不是当前时钟的
第二个 production 模式。

level-triggered PLIC/SEIP 与 VirtIO IRQ 1 已分别在 `e14e41e`/`bcd378d` 完成；后续只把
经 profile 证明的冗余扫描当作性能实验，不再把基础中断正确性列为未实现项。

本轮还已完成并独立提交 queue 128、`SEG_MAX=126`、descriptor/transfer scratch 复用、mmap
direct copy、used index 批量发布、两项 PLIC 扫描削减、完整 fallback 指令缓存和 TB 页内 fetch
translation cache。它们不再列作下一轮候选；其中结构性改进和性能中性结果也保留在上文，避免
以后重复实验。

### 当前低优先级

- 更复杂的 code LRU：realtime 前快照的峰值 live code 约 3.44 MiB，远低于默认
  128 MiB 上限，且没有 code-cache flush；它不是当前 Debian 启动瓶颈。
- 单独增加 native TLB replacement 复杂度：当前快照 hit rate 为 99.413%。应先区分 compulsory、
  direct-map conflict 和 context invalidation miss，再决定 2/4-way 是否值得。
- F/D native lowering：当前到 prompt 前的快照只有 5,220 次 floating-point
  fallback，对 Debian 启动的潜在收益很低；它更像 ISA 覆盖工作。
- 为少量 memory/MMIO/cross-page slow path 增加专门机器码：这些路径必须以正确性为先，当前
  memory fallback 只有 1,614 次，优先级低于 system fallback、共享翻译和时钟成本。
- persistent code cache 或跨进程复用：需要稳定 relocation、host feature、guest artifact 和
  失效协议；当前累计编译时间约 658 ms，复杂度仍高于先优化运行期热点。

## 建议的下一轮顺序

1. 增加 stats-only 的 system opcode/CSR、translation caller/reason 和 compile phase counters，
   同时补 guest-PC JIT 符号；prompt 计时继续关闭 stats。
2. 根据新 counters，在 system/CSR native lowering 与 core 共享 translation cache 中只选一个，
   独立实现、交错 A/B、独立 commit；收益不稳定就完整回滚。
3. 单独测量 realtime clock 的调用来源；只有确认热点后才优化换算或去除冗余采样，并用
   `sleep 1`、timer IRQ 和 prompt 空闲共同验收语义。
4. 把 compile/finalize 分段计时后，再决定批量 finalize 或后台编译是否值得；不要重复已经
   否决的 `speed_and_size` 实验。
5. 只有 guest-PC profile 证实热点后，才选 inline atomic、superpage/native TLB 或更细
   `SFENCE.VMA` 失效。
6. 设备侧下一步在 indirect descriptors 与 `EVENT_IDX` 中只选一个独立实验和提交；异步
   backend 和 dirty range flush 只在更真实的 I/O workload 证明当前同步路径受限后开展。
7. 小步优化不能继续降低 `JitExecutor::execute` 热点时，再进入 superblock/direct native
   chaining，并先设计 budget/side-exit/module-rotation 协议。
8. 每个落地优化继续跑 workspace、release JIT tests、96 个 `riscv-tests` 及三个 JIT demo；
   涉及 core/设备时再补 naive、trace 和双引擎完整回归。

所有后续实现继续遵守两个不变量：JIT 不复制 Sv39 翻译/权限逻辑；任何 batching/chaining
都不能越过 Machine instruction budget 或 guest-visible side effect。realtime timer 的
active-delivery latency 必须受 dispatcher ceiling 约束；WFI 的 timer 不得在
deadline 前恢复 guest，且 timed wait 必须能被设备通知打断。
