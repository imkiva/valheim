# Valheim x86_64 JIT 实施计划

本文档记录 Valheim JIT 的设计决策、实施顺序和验收标准。第一阶段实现已于
2026-07-15 完成；下文保留原始设计依据，并在本节记录最终实现和验收结果。

## 实现与验收状态（2026-07-15）

阶段 0–6 已完成；阶段 7 已完成有界 code arena，以及两轮由 profile/A/B 选出的 runtime、
dispatcher 和编译期开销优化。superblock、direct machine-code chaining 等仍不是第一阶段
完成条件；最新数据和后续方向见 [`JIT-PERF.md`](JIT-PERF.md)：

- `valheim-core` 提供共享 `translate_to_host()`、显式执行 budget、host-monotonic 10 MHz
  CLINT（可注入 `ClockSource` 供测试）及 SATP/SFENCE.VMA/FENCE.I epoch。跨页
  32-bit 取指会分别以 Fetch 权限翻译两个
  16-bit parcel；页表隐式访问与最终 endpoint fault 都保留原访问类型和 guest VA。
- `valheim-jit` 提供 decoded TB、negative cache、Cranelift RV64I/M、A helper、SSA GPR、
  software TLB、DRAM direct access、页内 fetch translation cache、精确 exception/MMIO
  side exit，以及有界 code arena。negative cache 保存完整的已解码 fallback 指令，命中时
  不再重复 fetch/decode。
- JIT 可在一个 Machine budget 内连续执行多个已编译 TB，并用 generation-guarded successor
  cache 连接常见边；decoded/fallback、atomic、system、WFI、budget 或 side exit 会结束 batch。
  MMIO 若位于 native 前缀之后，会先返回 dispatcher，并在下一次
  dispatcher/IRQ 轮询边界精确执行。
- 每个 Cranelift module 最多 4096 个函数；旧 module 只在其函数指针仍被 TB cache 引用时
  保留。所有存活 module 的机器码总量达到默认 128 MiB 上限后，在下一个
  dispatcher 安全点清 cache 并释放全部 module。
- CLI 默认仍为 `naive`，可用 `--engine jit` 明确启用；完整 trace 会强制回到 naive。
- 当前 JIT-enabled workspace 明确只支持 Linux x86_64 System V ABI；F/D、其他 host、direct
  machine-code tail chaining 和逐指令 JIT trace 仍是非目标。Rust 侧 successor cache 不会
  patch native jump target。

第一阶段当时的最终验证（固定 `nightly-2024-09-05`）：

| 项目 | naive | JIT |
| --- | --- | --- |
| workspace / trace / 差分单元测试 | workspace 合计 137/137（含右列 48 项 JIT tests）、core trace 75/75 | 34 个 JIT unit + 10 个 differential + 4 个 memory fast-path tests；release 配置同样通过 |
| `riscv-tests` | 96/96 | 96/96；xtask 使用 hot threshold 1 以覆盖热编译路径 |
| xv6 | 进入 `$` 并执行 `echo` | 进入 `$` 并执行 `echo` |
| RustSBI | success marker | success marker |
| Debian 13 | 进入 `debian13#`，读取版本 `13.6` | 进入 `debian13#`，读取版本 `13.6` |

上表和下方计时保留第一阶段的内建-initramfs 历史快照。后续 level-triggered PLIC、
legacy VirtIO block 与 ext4 direct-root 增加回归后，当前 workspace/trace 已分别为
171/171、103/103，release JIT 为 54/54；Debian 两种 engine 都从 read-write ext4 启动，
naive/JIT 的 ISA 测试仍为 96/96；这些新结果及剩余方向见
`JIT-PERF.md`，不能与下方旧 workload 的绝对启动时间直接比较。

第一阶段性能环境为 Linux 6.6.87.2 WSL2、AMD Ryzen 9 9950X3D、32 logical CPUs，基于
`e24b4d9` 的验收树。使用同一 release binary 和已构建的固定 Debian artifacts；外部计时
从进程启动到真实行末 `debian13# `，因此把少量相同的 CLI/镜像加载开销也计入两种 engine，
并包含全部 JIT 编译成本：

| engine | 三次结果 | 中位数 |
| --- | --- | --- |
| naive | 69.428 s / 69.673 s / 69.603 s | 69.603 s |
| JIT | 13.447 s / 13.399 s / 13.452 s | 13.447 s |

最终加速为 **5.176×**，超过 3× 初始目标。接近 Debian prompt 的统计快照为：5586 个累计
compiled TB、1.604 s 累计编译时间（平均 287.2 µs，p50 187.1 µs，p95 890.5 µs）、
4,648,470 bytes（4.43 MiB）累计代码、3,635,810 bytes（3.47 MiB）峰值 live
code、383,368,809 次 TLB hit 和 4,322,837 次
miss（98.885% hit rate）、2999 次 memory slow path、20 次 memory fault，且 compile failure
为 0。第一阶段默认统一 hot threshold 为 500；第二轮完整 Debian A/B 将当前默认调为 750，
早期 3/16/2 启发式没有保留。约 4.8 s 是切换 realtime 前的历史 checkpoint；
`10cabc6` 之后 guest 等待不再快进，新口径与旧绝对时间不可直接对比，详见
`JIT-PERF.md`。

## 目标与已确定的范围

主要目标是显著缩短 Debian 13 demo 到出现 `debian13#` 的时间，而不牺牲 guest
异常、分页和特权级语义的可诊断性。最终验收使用从宿主进程启动到真实行末 prompt
的可复现口径。

已经确定：

- JIT 后端使用 Cranelift。
- 第一阶段只支持 Linux x86_64 System V ABI。
- Valheim 的现有解释器继续保留，既作为回退路径，也作为差分测试基准。
- JIT 采用 translation block（TB）形式，而不是逐条指令调用生成代码。
- 使用分层执行：冷代码运行 decoded TB，热代码才交给 Cranelift 编译。
- 在实现 JIT 访存前先重构 core MMU，建立解释器与 JIT 共用的显式
  `translate_to_host()` 接口；JIT 不复制页表遍历或权限判断逻辑。
- 为了让 Debian 获得实际加速，software TLB 和 DRAM fast path 属于早期里程碑，
  不能长期让所有访存都经过完整 Rust/MMU slow path。
- 首版不实现 direct native tail chaining，不支持 Windows x64 ABI，不直接编译浮点指令；
  后续已加入不修改机器码的 Rust successor cache。

非目标：

- 不在本项目中实现通用优化编译器或独立 SSA IR。
- 不在首版支持其他 host ISA。
- 不以非规范、未执行 `FENCE.I` 的自修改代码作为首版兼容目标。
- 不在首版让当前逐指令 trace 与 JIT 同时工作。

## 实施前约束（历史设计输入）

实施前 `RV64Interpreter::interp(&self, cpu)` 一次执行一条指令，而
`Machine::run_next()` 在每次调用解释器前推进一次 CLINT 并检查一次中断。因此，当时
“一次解释器调用”同时也是“一条 guest 指令的计时和中断边界”。JIT 的执行器契约与
budget 设计源于这些语义约束。

其他重要约束：

- `RV64Cpu::execute` 只在成功执行后提交下一 PC；异常时 PC 保持在 faulting instruction。
- 非法指令的 `mtval` 来自 `cpu.instr`，JIT 必须保留原始指令位模式。
- RVC 已由 `valheim-asm` 解码并展开为基础 typed instruction，但 JIT 仍需保留原始长度 2/4。
- `FENCE.I` 和 `SFENCE.VMA` 当时为空操作，引入 JIT 后必须成为真实失效点。
- `Regs` 和 `RV64Cpu` 没有稳定的 C layout，生成代码不能硬编码其 Rust 字段偏移。
- MMU 当时没有 TLB，每次访存都可能重新进行页表遍历。
- `Memory` 当时存在跨宽度边界和通过引用进行非对齐访问等问题；这些不变量必须在
  native code 直接访问 RAM 前收紧。
- 完整 trace 依赖 Rust 寄存器和内存访问接口；JIT 直接访问会绕过 Journal。

## 总体架构

```text
                         ┌──────────────────────┐
                         │  Machine dispatcher  │
                         └──────────┬───────────┘
                                    │ budget / IRQ
                                    ▼
                         ┌──────────────────────┐
                         │     JitExecutor      │
                         └──────┬────────┬──────┘
                                │        │
                    cold/miss   │        │ hot/cache hit
                                ▼        ▼
                      ┌──────────────┐  ┌──────────────┐
                      │ Decoded TB   │  │ Native x64   │
                      │ executor     │  │ TB           │
                      └──────┬───────┘  └──────┬───────┘
                             │                 │
                             └────────┬────────┘
                                      ▼
                           normal / fault / fallback
                                      │
                                      ▼
                         Exception::handle / next TB
```

最终新增的独立 workspace crate 为：

```text
valheim-jit/
└── src/
    ├── lib.rs
    ├── block.rs          # GuestBlock、TB builder 和元数据
    ├── runtime.rs        # cache、hotness、JitExecutor、JitFrame、side exit
    ├── cranelift.rs      # x86_64 CLIF lowering 和 JITModule
    ├── memory.rs         # software TLB 和访存 helper
    └── atomic.rs         # A 扩展 helper
```

依赖方向保持为：

```text
valheim-asm <- valheim-core <- valheim-jit <- valheim-cli
```

`valheim-core` 定义执行器契约和 CPU helper；`valheim-jit` 实现该契约；CLI 根据参数选择执行器。
这样不会让 portable core 无条件依赖 host-specific Cranelift 代码，也不会形成循环依赖。

## 执行器与 Machine 调度契约

建议把“解释器”抽象提升为“执行器”概念：

```rust
pub struct ExecOutcome {
  /// 包含产生异常的指令；已经处于 WFI 空转时为 0。
  pub attempted: u32,
  pub result: Result<(), Exception>,
}

pub trait RV64Executor {
  fn execute(
    &mut self,
    cpu: &mut RV64Cpu,
    budget: u32,
  ) -> ExecOutcome;
}
```

设计要求：

- 使用 `&mut self`，让 JIT 可以直接维护 cache、hotness、Cranelift context 和 code arena。
- `NaiveInterpreter` 每次仍只尝试一条指令。
- `JitExecutor` 的 decoded/fallback 路径每次最多执行一个 TB；native 路径可在同一 budget 内
  连续执行多个满足精确 side-exit 约束的 compiled TB。
- native code 不直接进入 RISC-V trap；异常由 `Machine` 继续调用 `Exception::handle`。
- `attempted` 包含 faulting instruction，用于 budget 与精确提交计数；它不推进
  `mtime`。
- 若 JIT 执行了一个可编译前缀，并在下一条 unsupported instruction 前退出，本轮只报告前缀；
  下一次调度重新经过实时中断检查后再执行 fallback。

当前 CLINT 的计数与 deadline 接口是：

```rust
pub trait ClockSource: Send + Sync {
  fn now(&self) -> Duration;
}

impl Clint {
  pub fn mtime(&self) -> u64;
  pub fn duration_until_timer(&self) -> Option<Duration>;
}
```

Machine 调度顺序：

1. 从 realtime CLINT 重新派生 level-triggered MSIP/MTIP，检查并处理 pending
   interrupt。
2. 使用固定 executor ceiling（当前 1024 条 guest 指令）作为 budget。
3. 执行 decoded/native TB，得到 `attempted = N` 和结果。
4. 统一处理同步异常或进入下一次 dispatcher。

realtime timer delta 不是指令数，因此不缩短 TB budget。timer 若在 active native
batch 中到期，会在下一个 dispatcher 边界观察；1024 上限同时约束 timer
和 UART/VirtIO 等异步事件的最坏指令延迟。WFI 必须立即结束 TB；已经
处于 WFI 时执行器返回 `attempted = 0`。完整运行入口以 WakeHub generation
防止 lost wake：本地启用的未来 timer 使用绝对宿主 deadline timed wait，UART
通知可提前结束宿主等待并回到 dispatcher；已到期 timer 不等待。`run_next()` 仍保持非阻塞。production
不存在 instruction-clock/turbo mode，`ClockSource` 注入仅用于测试。

## GuestBlock 构建

不为首版建立新的通用 IR，直接复用 `valheim-asm` 的 typed `Instr` 作为前端 IR：

```rust
pub struct GuestInst {
  pub pc: u64,
  pub raw: u32,
  pub len: u8,
  pub decoded: Instr,
}

pub struct GuestBlock {
  pub start_pc: u64,
  pub instructions: Vec<GuestInst>,
  pub code_dependency: CodeDependency,
}
```

首版 TB 规则：

- 最多 32 条指令。
- 不跨 guest 4 KiB 虚拟代码页。
- conditional branch、JAL、JALR 在执行后结束 TB。
- CSR、ECALL、EBREAK、MRET、SRET、WFI、`FENCE.I` 和 `SFENCE.VMA` 是终止点。
- 普通 `FENCE` 可以编译为 no-op。
- 不支持的指令位于 TB 第一条时，只有在完整 fetch/decode 成功后才把 `GuestInst` 写入
  negative cache，并调用共享 `cpu.execute()` 单步；后续命中不再 fetch/decode。
- 不支持的指令位于 TB 中间时，只编译其之前的合法前缀。
- RVC 解码后继续记录 `len = 2`，其余为 `len = 4`。
- 编译后续指令发生 fetch/decode fault 时，如果 TB 已有合法前缀，只在此前结束，不能提前向
  guest 抛异常；空 TB 才按当前 PC 返回真实 fetch/decode 异常。
- 页尾、odd PC、MMIO 或 direct host page 无法安全等价时，builder 回到共享 `fetch_mem()`；
  跨页 32-bit 指令的两个 parcel 继续分别翻译，不能把 fault 提前或错误缓存。

TB builder 应使用无 trace 的翻译 fetch。它不能在预译后续代码时产生额外 Journal 记录，也
不能让后续页面的 fault 或页表副作用提前发生。因此首版严格限制在首条指令所在代码页。

## 分层执行与热度策略

Cranelift 不应编译所有首次出现的代码。Debian 启动会经过大量只执行一次的初始化路径，立即
编译会让 host 编译成本抵消 guest 执行收益。

建议流程：

1. cache miss 时构造并缓存 decoded TB。
2. 冷 TB 由 decoded-block executor 执行，不再重复 fetch/decode。
3. 每次执行更新 hotness。
4. 达到阈值后使用 Cranelift 编译，并替换 cache entry。

设计时曾考虑的初始启发式：

- 普通 TB 第 3 次执行时编译。
- 单指令 TB 第 16 次执行时编译。
- 检测到循环回边的 TB 可在第 2 次执行时编译。
- 含大量 fallback 的 TB 提高阈值或永久保留 decoded 形式。

阈值必须可配置并进入统计数据，最终以完整 Debian 启动时间而不是微基准决定。
最终实现没有采用上述 3/16/2 分类规则，而是使用统一可配阈值，当前默认为 750
且最小为 1。第 N 次 decoded TB 完整成功执行后编译，从下一次命中起运行 native。

decoded-block executor 是独立里程碑：它仍循环调用现有 `cpu.execute()`，用于先验证 TB 边界、
PC、异常、计时、fallback 和 cache 失效，避免将 runtime bug 与机器码 bug 混在一起。

## Cranelift 后端

### 版本和 host 约束

固定所有 Cranelift crate 为完全相同的版本：

```text
cranelift-codegen = =0.112.3
cranelift-frontend = =0.112.3
cranelift-jit = =0.112.3
cranelift-module = =0.112.3
cranelift-native = =0.112.3
```

该版本与仓库固定的 Rust 1.83 nightly 兼容。禁止使用宽松版本范围，以免解析到要求更新 Rust
的版本。

JIT 只在以下配置可用：

```rust
#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
```

生成函数和 helper 明确使用 System V calling convention。`valheim-jit` 在其他 host 上会直接
产生编译错误，而 `valheim-cli` 无条件依赖该 crate；因此 JIT-enabled CLI/完整 workspace
明确只支持 Linux x86_64 SysV。`valheim-core` 等不依赖 JIT 的 portable crate 仍可单独构建。

### Native 函数 ABI

生成代码不能依赖 `RV64Cpu` 或 `Regs` 的 Rust 默认布局。使用固定 C layout frame：

```rust
#[repr(C)]
pub struct JitFrame {
  pub cpu: *mut RV64Cpu,
  pub xregs: *mut u64,

  pub next_pc: u64,
  pub fault_pc: u64,
  pub fault_addr: u64,
  pub raw_instr: u32,

  pub attempted: u32,
  pub exit_kind: u32,
}
```

概念上的 block signature：

```rust
unsafe extern "C" fn(frame: *mut JitFrame) -> u32
```

所有 frame offset 由 Rust `offset_of!` 产生，禁止手写数字。CPU 指针和寄存器指针每次调用时
传入，不能把可能移动的 `RV64Cpu` 地址永久嵌进生成代码。

helper 使用稳定的 `extern "C"` 整数/指针参数和返回值；不跨 JIT ABI 传递 Rust enum、
`Result`、trait object 或可 unwind 的 panic。所有 helper 必须拦截内部错误并转换成 side exit。

### Guest GPR 的 SSA 表示

既然使用 Cranelift，就利用其 SSA 和寄存器分配，而不是让所有 guest GPR 在每条指令间都回到
内存：

- 第一次读取 guest register 时，从 `xregs` 加载为 CLIF value。
- 后续读取复用当前 value。
- guest register 写入更新 block-local value，并标记 dirty。
- x0 读取直接生成常数 0，写入丢弃。
- 在所有正常、分支和 fault 出口统一写回 dirty registers。
- helper 不直接读取 guest GPR；需要的地址和值作为显式参数传入。
- helper call 的 host 寄存器保存由 Cranelift/System V ABI 处理。

异常精度要求：

- helper 前记录当前 `fault_pc`、`raw_instr` 和 attempted count。
- faulting load 成功前不能更新目标 register 的 SSA value。
- load 到 x0 仍必须执行访问，以保留 fault 和 MMIO side effect。
- store fault 前不能对 guest RAM 产生部分写入。
- fault 出口先提交此前已成功执行指令的 dirty registers，再返回 dispatcher。
- JALR 必须先读取 rs1，再写 rd，并将目标地址 bit 0 清零。

### 首批 lowering 范围

第一组：

- NOP、LUI、AUIPC
- ADDI、XORI、ORI、ANDI
- ADD、SUB、XOR、OR、AND
- SLT、SLTU
- SLL、SRL、SRA 及 immediate 版本
- ADDW、SUBW 和所有 W shift
- 六种 conditional branch
- JAL、JALR
- 普通 FENCE

第二组：

- load/store 与 software TLB
- MUL、MULH、MULHU、MULHSU、MULW
- DIV、DIVU、REM、REMU 及 W 版本
- A 扩展 helper

x86_64 特别注意：

- W 运算必须先截断到 32 位，再符号扩展到 64 位。
- shift count 必须符合 RV64 的 6-bit 或 W 操作的 5-bit mask。
- RISC-V 的除零和 `MIN / -1` 有规定结果，必须显式 guard，不能触发 host `#DE`。
- MULH/MULHU/MULHSU 的 signedness 和高 64 位必须分别验证。

CSR、ECALL、EBREAK、MRET、SRET、WFI、FENCE.I、SFENCE.VMA 和 F/D 继续走缓存完整指令的
fallback，并调用共享 CPU 语义单步；A 扩展通过专用 helper 保持 LR/SC reservation 和 AMO
语义。

## 访存、MMU 与 software TLB

### 为什么属于早期里程碑

如果每个 load/store 都调用当前 `read_mem/write_mem`，依然会有 Rust 调用、动态总线分发和
重复 Sv39 页表遍历。只编译 ALU 很可能无法显著改善完整 Debian 启动时间。因此第一版性能
里程碑必须包含 inline TLB lookup 和 DRAM direct access。

### MMU helper 接口

这是已确定的前置设计，不是可选优化：在实现 JIT 访存前先重构 core MMU，提供解释器和 JIT
共用的显式翻译结果。JIT 中不得复制 Sv39/Sv48/Sv57 的页表遍历、PTE A/D 更新或权限判断
逻辑。

```rust
enum TranslationTarget {
  Dram {
    host_page: *mut u8,
    phys_page: u64,
    permissions: u8,
  },
  Mmio,
}

fn translate_to_host(
  &mut self,
  vaddr: VirtAddr,
  access: AccessType,
) -> Result<TranslationTarget, Exception>;
```

具体接口可在实现时调整，但必须做到：

- 页表遍历和权限判断只有一个语义来源。
- 现有 `fetch_mem/read_mem/write_mem` 先迁移到该接口，并在 naive 模式下完成全部回归验证。
- 能区分普通 DRAM、MMIO 和 fault。
- MMIO 不返回可直接解引用的 host pointer。
- miss helper 能把成功的 DRAM 翻译填入 software TLB。
- JIT software TLB miss 只能调用该接口，不能拥有另一份 page walker。
- trace、总线 side effect 和异常分类仍由 core 的共享路径负责；返回 host pointer 不得让解释器
  绕过这些语义。

### TLB entry 和 lookup

概念上的 entry：

```rust
#[repr(C)]
struct TlbEntry {
  tag: u64,
  host_page: *mut u8,
  phys_page: u64,
  permissions: u8,
  epoch: u64,
}
```

首版使用简单的 direct-mapped 或低路组相联 TLB，load 和 store 分开，避免每次命中仍进行复杂
权限分支。entry tag 至少覆盖：

- guest VPN
- SATP/ASID 或等价 address-space identity
- 有效 privilege
- access type
- translation epoch
- 对 data access 有影响的 MPRV/MPP、SUM、MXR

最终实现的 entry tag 使用 VPN 和 TLB generation；DRAM host base、privilege、SATP、
MSTATUS 与 translation epoch 放在 `TlbContext` 中，context 变化时整体失效 generation，
等价覆盖上述身份维度。

生成代码中的 load/store 顺序：

1. 计算 guest virtual address。
2. 按当前解释器规则检查对齐；失败则精确 side exit。
3. inline 查询 TLB tag 和 epoch。
4. 命中 DRAM entry 时，用 `host_page + page_offset` 直接访问。
5. miss 时调用共享 MMU helper。
6. helper 返回 DRAM 时填充 entry 并重试。
7. helper 返回 MMIO 时，不在 native block 中执行设备操作；退出到 slow path。
8. helper 返回 fault 时，提交此前 dirty state 并返回异常。

M-mode/bare mode 可以先使用更简单的物理 DRAM base/limit guard，无需 TLB page walk。

在启用 direct access 前，必须修正并测试 RAM 的：

- `contains(addr, width)` 边界判断
- 半开区间 end 语义
- `read_unaligned/write_unaligned` 或明确的 alignment contract
- host pointer 生命周期
- MMIO 与 DRAM 范围判定

## TB cache、分页与代码失效

初始 cache key：

```rust
struct TbKey {
  vpc: u64,
  privilege: u8,
  satp: u64,
  translation_epoch: u64,
  icache_epoch: u64,
}
```

说明：

- 主键使用虚拟 PC；AUIPC/JAL 依赖虚拟地址，同一物理页的不同 VA 映射不能无条件共享代码。
- privilege 必需，因为 M-mode 会绕过分页，特权语义也不同。
- SATP 包含 MODE、ASID 和根 PPN。
- `translation_epoch` 处理同一个 SATP 下页表修改后的 `SFENCE.VMA`。
- `icache_epoch` 处理 `FENCE.I`。
- data access 初期读取 JitFrame/TLB 的实时上下文；若后续在 TB 中专门化 MPRV/SUM/MXR，
  需要把相应状态加入 key 或入口 guard。

失效规则：

- 成功写 SATP：同步现有 MMU 状态并增加 `translation_epoch`。
- `SFENCE.VMA`：首版忽略 rs1/rs2 的定向范围，增加全局 `translation_epoch` 并清 TLB。
- `FENCE.I`：增加 `icache_epoch` 并清 TB lookup map。
- MRET/SRET/trap：不必清 cache，新的 privilege 会选择不同 key。
- 普通 FENCE：不清 TB。
- epoch 溢出：清空相关 cache 并从新 generation 重新开始。

首版为了 hot store 性能，不在每次普通 DRAM store 上 inline 检查“是否写入已编译代码页”。
规范 guest 在修改将执行的指令后必须使用 `FENCE.I`。调试模式以后可增加代码页 generation
或 checksum；若需要完全匹配 naive 对非规范自修改代码的立即可见行为，再实现受监控代码页。

## Cranelift JITModule 生命周期

`JITModule` 提供 executable code memory，但已取得的函数指针只能在所属 module 的内存仍有效
时使用。实现不尝试逐 TB 回收 native code：

- active module 默认最多定义 4096 个函数；满后新建 module。
- TB map 保存 `TbKey -> CompiledBlock` 和函数指针；只要 map 仍可达旧函数，所属
  retired module 就保留。
- `FENCE.I` 或 translation epoch 变化会在 dispatcher 安全点清 map，并立即释放
  active 与 retired module。
- 机器码字节数来自 Cranelift `code_info().total_size`，并记录 generated/live/peak 统计。
- 所有存活 module 的机器码总量达到默认 128 MiB 上限时，下一个 dispatcher
  安全点清 TB cache 并整体释放所有 module，然后开始新 generation。
- 没有 direct native jump patching；Rust successor cache 只保存 generation-guarded arena index，
  因此整体 rotation 时不存在 native block 之间的悬空机器码跳转边。

每次编译复用 Cranelift `Context` 和 `FunctionBuilderContext`，减少 host 分配。函数按需
`define`/`finalize`；是否批量 finalize 由实际 profile 决定，不能为了理论吞吐增加首次热块延迟。

## Side exit 与 fallback

建议的 exit 分类：

```rust
#[repr(u32)]
enum JitExitKind {
  Continue,
  Branch,
  Budget,
  Exception,
  Mmio,
  InterpretOne,
  StaleBlock,
}
```

native block 只返回整数 exit code 和 `JitFrame` 数据；Rust runtime 负责重建现有 `Exception`。
禁止让 host signal fault 代替 guest page fault，也禁止 Rust unwind 穿过生成代码。

fallback 规则：

- unsupported 位于 TB 起点：成功 fetch/decode 后缓存完整 `GuestInst`，同一 executor
  dispatch 内调用共享 `cpu.execute()` 单步一次；后续命中不重复 fetch/decode。
- unsupported 位于已执行前缀之后：先返回 dispatcher，下一次调度再按上述缓存路径单步。
- MMIO instruction 未执行前退出，下一次由 slow path 执行，避免设备 side effect 重复。
- CSR/特权指令 fallback 成功后，JIT runtime 检查可能影响 cache key、TLB 或 privilege 的状态。
- FENCE.I、SFENCE.VMA 和 SATP 写入最好在 core 的统一语义位置更新 epoch，而不是由 JIT 猜测。

## CLI、feature 和可观测性

CLI 增加：

```text
--engine naive|jit
```

当前默认 `naive`。同时已提供：

- `--jit-hot-threshold N`，默认 750，最小 1
- `--jit-max-block-len N`，默认 32，范围 1–32
- `--jit-max-compiled-blocks N`，默认每 module 4096
- `--jit-max-code-bytes N`，默认 134217728（aggregate live code）
- `--jit-stats`
- `--jit-stats-interval N`，默认每 1000000 次 dispatch 输出一次

`--jit-dump-clif DIR` 没有纳入首轮实现，也不是第一阶段完成条件。

首批统计：

- decoded TB 数量
- native TB 数量
- cache hit/miss
- negative cache 数量
- 每种 exit reason 次数
- 编译总耗时、平均和分位数
- 生成代码精确字节数与 live/peak 字节数
- software TLB hit/miss
- MMIO/fault slow-path 次数
- fallback 指令分类

启用当前完整 `valheim-core/trace` feature 或传入 trace 文件时，首版强制使用 naive 并给出清楚
提示。后续若需要 JIT trace，应单独设计 block-level 或 instrumented JIT，而不是让 fast path 默认
记录逐寄存器事件。

## 分阶段实施

### 阶段 0：基准与语义护栏（已完成）

- 固定 naive Debian、xv6、RustSBI 和 ISA tests 的基线结果。
- 记录 Debian 从宿主进程启动到真实行末 `debian13# ` 的时间和 host 环境。
- 为 PC、fault、RVC 长度、timer deadline 和 WFI 增加针对性测试。

完成标准：没有 JIT 代码，已有可重复的正确性和性能基线。

### 阶段 1：执行器契约和批量调度（已完成）

- 引入 `RV64Executor`、`ExecOutcome` 和 budget。
- 将 `Machine` 改为注入执行器。
- 阶段 1 当时增加了 CLINT `advance/ticks_until_timer`；`10cabc6` 后已由
  host-monotonic `ClockSource`/`mtime`/`duration_until_timer` 取代，`attempted`
  不再推进时间。
- 合并普通运行和 test 运行中重复的调度语义。
- Naive 模式仍严格逐指令运行。

完成标准：workspace tests、96 个 ISA tests 和三个 demo 在 naive 下无行为回归。

### 阶段 2：共享 MMU 翻译接口（已完成）

- 定义统一的 access type 和 `TranslationTarget`/等价结果类型。
- 把 Sv39/Sv48/Sv57 页表遍历、权限检查、PTE A/D 更新和异常分类收敛到
  `translate_to_host()`。
- 将解释器的 `fetch_mem/read_mem/write_mem` 全部迁移到该接口。
- 保留现有 trace、MMIO、总线访问和异常行为。
- 为 M/S/U mode、MPRV/MPP、SUM、MXR、fetch/load/store 权限和页故障建立单元测试。
- 此阶段不加入 JIT TLB，也不改变 naive 的逐指令执行方式。

完成标准：解释器已成为共享接口的第一个消费者；workspace tests、96 个 ISA tests 和三个 demo
无行为回归。后续 JIT page-table miss 只能复用该接口。

### 阶段 3：decoded TB executor（已完成）

- 实现 GuestBlock builder、页内边界和 cache key。
- decoded TB 仍调用 `cpu.execute()`。
- 实现 hotness、negative cache、fallback 和 epoch。
- 实现统计信息。

完成标准：decoded 模式与 naive 差分一致，能够完整运行 ISA tests 和 demo；此阶段不承诺明显
性能提升。

### 阶段 4：Cranelift RV64I baseline（已完成）

- 加入固定版本 Cranelift 和 Linux x86_64 host gate。
- 实现 JitFrame、helper ABI 和 JITModule 生命周期。
- 编译整数 ALU、W 操作、branch、JAL/JALR。
- 使用 block-local SSA guest registers 和精确 dirty writeback。
- 其余指令可靠 fallback。

完成标准：RV64I 相关 ISA tests 在 JIT 下通过；随机整数 block 与 naive 差分一致；所有异常出口
保留正确 PC、raw instruction 和已提交状态。

### 阶段 5：DRAM fast path 和 software TLB（已完成）

- 收紧 Memory/Bus 边界与非对齐访问。
- 将阶段 2 的 `translate_to_host()` 接入 JIT TLB miss helper。
- 实现 M-mode direct DRAM path。
- 实现 S/U-mode software TLB、miss helper、权限和 epoch。
- MMIO 使用精确 side exit。

完成标准：load/store/page fault/misalignment/MMIO 差分测试通过，TLB 有可观测命中率，Debian
启动开始获得主要性能收益。

### 阶段 6：M 和 A 扩展（已完成）

- 直接编译 M 扩展并覆盖除零/溢出边界。
- A 扩展先通过专用 helper 保留 LR/SC reservation 和 AMO 语义。
- 根据 profile 决定是否将部分原子操作改成 native fast path。

完成标准：对应 ISA tests 通过，xv6/Linux 锁和原子热路径没有频繁退回大 match interpreter。

### 阶段 7：性能优化（本轮选中项已完成）

只根据统计和 profile 选择：

- 已选：hot threshold 750、executor ceiling 1024
- 已选：有界、分段的 native code arena rotation
- 已选：native multi-TB batching、generation-guarded successor cache、batch 复用 `JitFrame`
- 已选：native exit 热/冷路径拆分、关闭 disabled stats 开销
- 已选：所有 release 构建（含 release tests）关闭 Cranelift IR verifier；启用 debug
  assertions 的构建（含默认 tests）保持开启
- 未选：更复杂的 TLB replacement
- 未选：page-local trace/superblock
- 未选：direct machine-code tail chaining
- 未选：更精细的 SFENCE.VMA 失效
- 未选：逐块 code LRU

F/D、精确 JIT trace 和其他 host 后端不属于该阶段的默认范围。

## 正确性验证

### 单元和差分测试

- 同一初始 CPU 状态分别运行 naive 与 JIT，比较 PC、GPR、相关 CSR 和 RAM。
- 随机生成有效 RV64I/M block，与 naive oracle 比较。
- RVC 2/4 字节长度和 branch link address。
- JALR `rd == rs1`。
- load 到 x0 的 fault/MMIO side effect。
- faulting load 不写 rd，faulting store 不部分写内存。
- DIV/REM 的零除和 signed overflow。
- 页边界、misalignment、Sv39 权限和 PTE A/D；跨页 32-bit 取指分别翻译两个 parcel，
  compressed 指令不访问下一页，PTE/endpoint access fault 保留故障 guest VA。
- SATP、SFENCE.VMA、FENCE.I 和 privilege change 后的 cache/TLB 失效。
- 10 MHz realtime `mtime`、MMIO 写入重设 guest/host anchor、live read-only `rdtime`、
  timer 不缩短 TB budget，以及
  WFI 不早于宿主 deadline 恢复 guest、deadline 后重查 MTIP，UART 可提前结束 host wait。
- helper panic 不得穿过 native frame。

### 项目级验证

每个相关阶段至少运行：

```bash
cargo +nightly-2024-09-05 test --workspace --locked
cargo +nightly-2024-09-05 build --release --locked --package valheim-cli
```

CPU/MMU/中断/内存改动完成后，按根 `AGENTS.md` 运行：

```bash
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

JIT 与 naive 两种 engine 都要覆盖适用的测试。验收必须进入 xv6 `$` 和 Debian `debian13#`，
并分别执行实际 guest 命令。

## 性能验收

主要 benchmark：

- 使用已构建好的固定 Debian artifact，不把下载、Linux 编译或 Valheim 编译计入 guest 启动。
- 使用外部进程计时，从启动 `valheim-cli` 到读到真实行末 `debian13# `。这个口径
  比仅计 `Machine::run` 更保守，但两种 engine 都包含相同的 CLI 和镜像加载开销。
- JIT 的运行时编译成本必须包含在测量内。
- 同一台 host 上 naive 和 JIT 各运行至少三次，取中位数。
- 同时记录 TB 编译时间、TLB 命中率、fallback 分类和 code size。

初始目标：

- JIT 完整启动至少比 release naive 快 3 倍。
- 进一步目标是将当前约 1–3 分钟的 shell 启动时间降低到约 30 秒量级。
- 如果 RV64I native 已完成但未达到目标，不以“JIT 已能运行”作为性能里程碑完成；应继续定位
  MMU、dispatcher、fallback 或编译成本瓶颈。

## 主要风险和应对

| 风险 | 应对 |
| --- | --- |
| Cranelift 编译大量冷启动代码，启动反而变慢 | decoded tier、hot threshold、记录编译时间 |
| 所有访存走 helper，ALU 加速被 MMU 开销吞没 | 尽早实现 software TLB 和 DRAM fast path |
| fault PC 或寄存器提交顺序错误 | 单一 JitFrame、统一 side exit、naive 差分 |
| realtime timer 在 native batch 中到期 | 每个 dispatcher 从 CLINT level 重新派生 MTIP；固定 1024 ceiling 约束 active 投递延迟，WFI timeout 后重查 MTIP，不会在 deadline 前恢复 guest |
| 外部 IRQ 延迟过大 | 以 executor ceiling 限制 native batch，不做 direct tail chaining，按 demo 测量延迟 |
| 页表或指令缓存失效后执行陈旧代码 | SATP/SFENCE.VMA/FENCE.I epoch 和 conservative flush |
| Rust layout 变化破坏 native code | 只访问 `repr(C)` frame，不硬编码 RV64Cpu offset |
| JITModule 代码无法逐块回收 | 逻辑失效、容量上限、dispatcher 安全点整体 rotation |
| reference interpreter 本身存在边角错误 | 先定义是匹配当前行为还是共同修正规范，修复时双引擎同步 |
| trace 被 fast path 绕过 | 首版 trace 强制 naive |

## 完成定义

JIT 第一阶段总体完成需同时满足：

1. Linux x86_64 SysV 上可由 CLI 明确选择 JIT。
2. 冷代码 decoded 执行、热代码 Cranelift 编译和 fallback 均稳定工作。
3. RV64I/M native、A helper、DRAM fast path 和 software TLB 已覆盖 Debian 主要热路径。
4. 精确异常、分页、SATP/SFENCE.VMA/FENCE.I、timer 和 WFI 测试通过。
5. 96 个 ISA tests 及 xv6、RustSBI、Debian demo 达到根 `AGENTS.md` 的验收标准。
6. 完整 Debian 启动包含 JIT 编译成本后至少达到 3 倍加速，或有 profile 数据明确说明下一瓶颈。
7. naive executor 保持可用，可作为兼容、trace 和差分基准。

截至 2026-07-15，上述 1–7 项均已满足；具体测试和性能数据见文档开头的
“实现与验收状态”。

## 参考资料

- [Cranelift JITModule](https://docs.rs/cranelift-jit/0.112.3/cranelift_jit/struct.JITModule.html)
- [Cranelift 0.112.3 crate](https://crates.io/crates/cranelift-jit/0.112.3)
- [RISC-V FENCE.I](https://docs.riscv.org/reference/isa/unpriv/zifencei.html)
- [RISC-V SFENCE.VMA](https://riscv-software-src.github.io/riscv-unified-db/example_cfg/html/example_rv64_with_overlay/insts/sfence.vma.html)
