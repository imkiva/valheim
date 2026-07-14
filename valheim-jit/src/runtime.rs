use std::panic::{catch_unwind, AssertUnwindSafe};
use std::time::Instant;

use rustc_hash::FxHashMap;
use valheim_core::cpu::csr::CSRMap::{MSTATUS, SATP};
use valheim_core::cpu::RV64Cpu;
use valheim_core::interp::naive::NaiveInterpreter;
use valheim_core::interp::{ExecOutcome, RV64Executor};
use valheim_core::memory::VirtAddr;

use crate::block::{BlockBuild, FallbackKind, GuestBlock, GuestInst, MAX_BLOCK_LEN};
use crate::cranelift::{CompiledBlock, CraneliftBackend, JitError, JitFrame};
use crate::memory::{exception_from_frame, is_slow_memory_exit, SoftwareTlb};

const DEFAULT_HOT_THRESHOLD: u32 = 500;
// Keep one Cranelift module comfortably below the x86-64 PLT/GOT ±2 GiB relocation limit. The
// generated memory/TLB side exits make each function substantially larger than a pure ALU TB.
const DEFAULT_MAX_COMPILED_BLOCKS: usize = 4 * 1024;
const DEFAULT_MAX_LIVE_CODE_BYTES: u64 = 128 * 1024 * 1024;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TbKey {
  pc: u64,
  privilege: u8,
  satp: u64,
  translation_epoch: u64,
  icache_epoch: u64,
}

impl TbKey {
  fn new(cpu: &RV64Cpu) -> Self {
    Self {
      pc: cpu.read_pc().0,
      privilege: cpu.mode as u8,
      satp: cpu.csrs.read_unchecked(SATP),
      translation_epoch: cpu.translation_epoch,
      icache_epoch: cpu.icache_epoch,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TlbContext {
  dram_host_base: usize,
  privilege: u8,
  satp: u64,
  mstatus: u64,
  translation_epoch: u64,
}

impl TlbContext {
  fn new(cpu: &RV64Cpu) -> Self {
    Self {
      dram_host_base: cpu.bus.mem.memory.as_ptr() as usize,
      privilege: cpu.mode as u8,
      satp: cpu.csrs.read_unchecked(SATP),
      mstatus: cpu.csrs.read_unchecked(MSTATUS),
      translation_epoch: cpu.translation_epoch,
    }
  }
}

struct CachedBlock {
  block: GuestBlock,
  executions: u32,
  compiled: Option<CompiledBlock>,
  compile_failed: bool,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitStats {
  pub hot_threshold: u32,
  pub max_block_len: usize,
  pub max_compiled_blocks_per_module: usize,
  pub max_live_code_bytes: u64,
  pub cache_hits: u64,
  pub cache_misses: u64,
  pub decoded_executions: u64,
  pub native_executions: u64,
  pub compiled_blocks: u64,
  pub fallback_instructions: u64,
  pub fallback_system: u64,
  pub fallback_floating_point: u64,
  pub fallback_other: u64,
  pub fallback_memory: u64,
  pub compile_failures: u64,
  pub compilation_nanos: u64,
  pub compilation_average_nanos: u64,
  pub compilation_p50_nanos: u64,
  pub compilation_p95_nanos: u64,
  pub negative_blocks: u64,
  pub negative_cache_hits: u64,
  pub module_rotations: u64,
  pub live_modules: usize,
  pub code_cache_flushes: u64,
  pub generated_code_bytes: u64,
  pub live_code_bytes: u64,
  pub peak_live_code_bytes: u64,
  pub dispatches: u64,
  pub guest_instructions: u64,
  pub tlb_hits: u64,
  pub tlb_misses: u64,
  pub memory_slow_paths: u64,
  pub memory_faults: u64,
  pub successful_exits: u64,
  pub exception_exits: u64,
}

pub struct JitExecutor {
  naive: NaiveInterpreter,
  backend: CraneliftBackend,
  retired_backends: Vec<CraneliftBackend>,
  tlb: SoftwareTlb,
  tlb_context: Option<TlbContext>,
  cache: FxHashMap<TbKey, CachedBlock>,
  negative_cache: FxHashMap<TbKey, FallbackKind>,
  pending_slow_memory: FxHashMap<TbKey, GuestInst>,
  hot_threshold: u32,
  max_block_len: usize,
  max_compiled_blocks: usize,
  max_live_code_bytes: u64,
  compiled_in_generation: usize,
  stats_interval: Option<u64>,
  compilation_samples: Vec<u64>,
  seen_translation_epoch: u64,
  seen_icache_epoch: u64,
  stats: JitStats,
}

impl JitExecutor {
  pub fn new() -> Result<Self, JitError> {
    Ok(Self {
      naive: NaiveInterpreter::new(),
      backend: CraneliftBackend::new()?,
      retired_backends: Vec::new(),
      tlb: SoftwareTlb::new(),
      tlb_context: None,
      cache: FxHashMap::default(),
      negative_cache: FxHashMap::default(),
      pending_slow_memory: FxHashMap::default(),
      hot_threshold: DEFAULT_HOT_THRESHOLD,
      max_block_len: MAX_BLOCK_LEN,
      max_compiled_blocks: DEFAULT_MAX_COMPILED_BLOCKS,
      max_live_code_bytes: DEFAULT_MAX_LIVE_CODE_BYTES,
      compiled_in_generation: 0,
      stats_interval: None,
      compilation_samples: Vec::new(),
      seen_translation_epoch: 0,
      seen_icache_epoch: 0,
      stats: JitStats::default(),
    })
  }

  pub fn with_hot_threshold(mut self, hot_threshold: u32) -> Self {
    self.hot_threshold = hot_threshold.max(1);
    self
  }

  pub fn with_max_block_len(mut self, max_block_len: usize) -> Self {
    self.max_block_len = max_block_len.clamp(1, MAX_BLOCK_LEN);
    self
  }

  pub fn with_max_compiled_blocks(mut self, max_compiled_blocks: usize) -> Self {
    self.max_compiled_blocks = max_compiled_blocks.max(1);
    self
  }

  pub fn with_max_live_code_bytes(mut self, max_live_code_bytes: u64) -> Self {
    self.max_live_code_bytes = max_live_code_bytes.max(1);
    self
  }

  pub fn with_stats_interval(mut self, interval: Option<u64>) -> Self {
    self.stats_interval = interval.map(|interval| interval.max(1));
    self
  }

  pub fn stats(&self) -> JitStats {
    let mut stats = self.stats;
    stats.hot_threshold = self.hot_threshold;
    stats.max_block_len = self.max_block_len;
    stats.max_compiled_blocks_per_module = self.max_compiled_blocks;
    stats.max_live_code_bytes = self.max_live_code_bytes;
    stats.live_modules = self.retired_backends.len() + 1;
    let tlb = self.tlb.stats();
    stats.tlb_hits = tlb.hits;
    stats.tlb_misses = tlb.misses;
    stats.memory_slow_paths = tlb.slow_paths;
    stats.memory_faults = tlb.faults;
    stats.live_code_bytes = self.live_code_bytes();
    if !self.compilation_samples.is_empty() {
      let mut samples = self.compilation_samples.clone();
      samples.sort_unstable();
      stats.compilation_average_nanos = stats.compilation_nanos / samples.len() as u64;
      stats.compilation_p50_nanos = samples[(samples.len() - 1) * 50 / 100];
      stats.compilation_p95_nanos = samples[(samples.len() - 1) * 95 / 100];
    }
    stats
  }

  fn live_code_bytes(&self) -> u64 {
    self.retired_backends.iter().fold(
      self.backend.code_bytes(),
      |total, backend| total.saturating_add(backend.code_bytes()),
    )
  }

  fn replace_code_backend(&mut self) -> bool {
    let Ok(backend) = CraneliftBackend::new() else {
      self.max_compiled_blocks = usize::MAX;
      self.max_live_code_bytes = u64::MAX;
      self.stats.compile_failures += 1;
      return false;
    };

    self.cache.clear();
    let current = std::mem::replace(&mut self.backend, backend);
    unsafe { current.free_memory() };
    for retired in self.retired_backends.drain(..) {
      unsafe { retired.free_memory() };
    }
    self.compiled_in_generation = 0;
    true
  }

  fn invalidate_changed_epochs(&mut self, cpu: &RV64Cpu) {
    if self.seen_translation_epoch != cpu.translation_epoch
      || self.seen_icache_epoch != cpu.icache_epoch
    {
      self.cache.clear();
      self.negative_cache.clear();
      self.pending_slow_memory.clear();
      self.replace_code_backend();
      self.seen_translation_epoch = cpu.translation_epoch;
      self.seen_icache_epoch = cpu.icache_epoch;
    }
  }

  fn sync_tlb_context(&mut self, cpu: &RV64Cpu) {
    let context = TlbContext::new(cpu);
    if self.tlb_context != Some(context) {
      self.tlb.invalidate();
      self.tlb_context = Some(context);
    }
  }

  fn rotate_code_cache_if_needed(&mut self) {
    let module_full = self.compiled_in_generation >= self.max_compiled_blocks;
    let arena_full = self.live_code_bytes() >= self.max_live_code_bytes;
    if !module_full && !arena_full {
      return;
    }
    if arena_full {
      if self.replace_code_backend() {
        self.stats.module_rotations += 1;
        self.stats.code_cache_flushes += 1;
      }
      return;
    }
    match CraneliftBackend::new() {
      Ok(backend) => {
        let old = std::mem::replace(&mut self.backend, backend);
        // Keep the old module alive because cache entries still hold its function pointers.
        // There is no block chaining, so generations are independent.
        self.retired_backends.push(old);
        self.compiled_in_generation = 0;
        self.stats.module_rotations += 1;
      }
      Err(_) => {
        // Keep valid code pointers in the old module. Avoid retrying allocation on every TB.
        self.max_compiled_blocks = usize::MAX;
        self.stats.compile_failures += 1;
      }
    }
  }

  fn execute_decoded(cpu: &mut RV64Cpu, block: &GuestBlock, budget: u32) -> ExecOutcome {
    let mut attempted = 0;
    for inst in block.instructions.iter().take(budget as usize) {
      if cpu.read_pc() != VirtAddr(inst.pc) {
        break;
      }
      cpu.instr = inst.raw as u64;
      attempted += 1;
      if let Err(exception) = cpu.execute(VirtAddr(inst.pc), inst.decoded, inst.len == 2) {
        return ExecOutcome::new(attempted, Err(exception));
      }
    }
    ExecOutcome::new(attempted, Ok(()))
  }

  fn execute_native(
    tlb: &mut SoftwareTlb,
    cpu: &mut RV64Cpu,
    block: &GuestBlock,
    compiled: CompiledBlock,
  ) -> (ExecOutcome, Option<GuestInst>) {
    let xregs = cpu.regs.x.as_mut_ptr();
    let load_tlb = tlb.load_ptr();
    let store_tlb = tlb.store_ptr();
    let tlb_generation = tlb.generation();
    let tlb_stats = tlb.stats_ptr();
    let mut frame = JitFrame::new(cpu, xregs, load_tlb, store_tlb, tlb_generation, tlb_stats);
    let attempted = unsafe { (compiled.entry)(&mut frame) };
    if let Some(exception) = exception_from_frame(&frame) {
      cpu.write_pc(VirtAddr(frame.fault_pc));
      cpu.instr = frame.raw_instr as u64;
      return (ExecOutcome::new(attempted, Err(exception)), None);
    }
    if is_slow_memory_exit(&frame) {
      let index = attempted
        .checked_sub(1)
        .expect("memory side exit attempted no instruction");
      let inst = block.instructions[index as usize];
      debug_assert_eq!(inst.pc, frame.fault_pc);
      cpu.write_pc(VirtAddr(frame.fault_pc));
      cpu.instr = frame.raw_instr as u64;
      if index != 0 {
        cpu.instr = block.instructions[index as usize - 1].raw as u64;
        return (ExecOutcome::new(index, Ok(())), Some(inst));
      }
      let result = cpu.execute(VirtAddr(inst.pc), inst.decoded, inst.len == 2);
      return (ExecOutcome::new(attempted, result), None);
    }
    cpu.write_pc(VirtAddr(frame.next_pc));
    if attempted != 0 {
      cpu.instr = block.instructions[attempted as usize - 1].raw as u64;
    }
    (ExecOutcome::new(attempted, Ok(())), None)
  }

  fn fallback_one(
    &mut self,
    cpu: &mut RV64Cpu,
    budget: u32,
    kind: FallbackKind,
  ) -> ExecOutcome {
    self.stats.fallback_instructions += 1;
    match kind {
      FallbackKind::System => self.stats.fallback_system += 1,
      FallbackKind::FloatingPoint => self.stats.fallback_floating_point += 1,
      FallbackKind::Other => self.stats.fallback_other += 1,
    }
    self.naive.execute(cpu, budget.min(1))
  }
}

impl JitExecutor {
  fn execute_one(&mut self, cpu: &mut RV64Cpu, budget: u32) -> ExecOutcome {
    if budget == 0 || cpu.wfi {
      return ExecOutcome::new(0, Ok(()));
    }

    self.invalidate_changed_epochs(cpu);
    self.sync_tlb_context(cpu);
    self.rotate_code_cache_if_needed();
    let key = TbKey::new(cpu);
    if let Some(inst) = self.pending_slow_memory.remove(&key) {
      self.stats.fallback_instructions += 1;
      self.stats.fallback_memory += 1;
      cpu.instr = inst.raw as u64;
      let result = cpu.execute(VirtAddr(inst.pc), inst.decoded, inst.len == 2);
      return ExecOutcome::new(1, result);
    }
    if let Some(kind) = self.negative_cache.get(&key).copied() {
      self.stats.cache_hits += 1;
      self.stats.negative_cache_hits += 1;
      return self.fallback_one(cpu, budget, kind);
    }
    if !self.cache.contains_key(&key) {
      self.stats.cache_misses += 1;
      match GuestBlock::translate(cpu, self.max_block_len) {
        BlockBuild::Block(block) => {
          self.cache.insert(
            key,
            CachedBlock {
              block,
              executions: 0,
              compiled: None,
              compile_failed: false,
            },
          );
        }
        BlockBuild::InterpretOne(kind) => {
          self.negative_cache.insert(key, kind);
          self.stats.negative_blocks += 1;
          return self.fallback_one(cpu, budget, kind);
        }
        BlockBuild::Fault { raw, exception } => {
          if let Some(raw) = raw {
            cpu.instr = raw as u64;
          }
          return ExecOutcome::new(1, Err(exception));
        }
      }
    } else {
      self.stats.cache_hits += 1;
    }

    let (block_len, compiled) = {
      let cached = self.cache.get(&key).expect("newly inserted TB is missing");
      (cached.block.instructions.len(), cached.compiled)
    };
    if let Some(compiled) = compiled {
      if block_len <= budget as usize {
        self.stats.native_executions += 1;
        let (cache, tlb) = (&self.cache, &mut self.tlb);
        let block = &cache.get(&key).expect("native TB is missing").block;
        let (outcome, pending) = Self::execute_native(tlb, cpu, block, compiled);
        if let Some(inst) = pending {
          self.pending_slow_memory.insert(TbKey::new(cpu), inst);
        }
        return outcome;
      }
    }

    self.stats.decoded_executions += 1;
    let outcome = {
      let block = &self.cache.get(&key).expect("decoded TB is missing").block;
      Self::execute_decoded(cpu, block, budget)
    };

    let should_compile = {
      let cached = self.cache.get_mut(&key).expect("executed TB is missing");
      cached.executions = cached.executions.saturating_add(1);
      outcome.result.is_ok()
        && outcome.attempted as usize == block_len
        && cached.compiled.is_none()
        && !cached.compile_failed
        && cached.executions >= self.hot_threshold
    };
    if should_compile {
      let started = Instant::now();
      let result = {
        let (cache, backend) = (&self.cache, &mut self.backend);
        let block = &cache.get(&key).expect("compile TB is missing").block;
        catch_unwind(AssertUnwindSafe(|| backend.compile(block)))
      };
      match result {
        Ok(Ok(compiled)) => {
          self.cache.get_mut(&key).unwrap().compiled = Some(compiled);
          self.stats.compiled_blocks += 1;
          self.stats.generated_code_bytes = self
            .stats
            .generated_code_bytes
            .saturating_add(compiled.code_size);
          self.compiled_in_generation += 1;
          self.stats.peak_live_code_bytes = self
            .stats
            .peak_live_code_bytes
            .max(self.live_code_bytes());
        }
        Ok(Err(_)) => {
          self.cache.get_mut(&key).unwrap().compile_failed = true;
          self.stats.compile_failures += 1;
        }
        Err(_) => {
          // A Cranelift allocator/relocation panic must not terminate the guest. Drop all native
          // pointers at this dispatcher safe point and continue with decoded execution.
          self.stats.compile_failures += 1;
          self.compiled_in_generation = self.max_compiled_blocks;
          self.rotate_code_cache_if_needed();
        }
      }
      let elapsed = started.elapsed().as_nanos().min(u64::MAX as u128) as u64;
      self.stats.compilation_nanos = self.stats.compilation_nanos.saturating_add(elapsed);
      self.compilation_samples.push(elapsed);
    }

    outcome
  }
}

impl RV64Executor for JitExecutor {
  fn execute(&mut self, cpu: &mut RV64Cpu, budget: u32) -> ExecOutcome {
    self.stats.dispatches = self.stats.dispatches.saturating_add(1);
    if let Some(interval) = self.stats_interval {
      if self.stats.dispatches % interval == 0 {
        eprintln!("[valheim-jit] {:?}", self.stats());
      }
    }
    if budget == 0 || cpu.wfi {
      return ExecOutcome::new(0, Ok(()));
    }

    let outcome = self.execute_one(cpu, budget);
    self.stats.guest_instructions = self
      .stats
      .guest_instructions
      .saturating_add(outcome.attempted as u64);
    if outcome.result.is_ok() {
      self.stats.successful_exits = self.stats.successful_exits.saturating_add(1);
    } else {
      self.stats.exception_exits = self.stats.exception_exits.saturating_add(1);
    }
    outcome
  }

  fn diagnostics(&self) -> Option<String> {
    Some(format!("{:?}", self.stats()))
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use valheim_asm::asm::encode32::Encode32;
  use valheim_asm::isa::data::Fin;
  use valheim_asm::isa::rv32::RV32Instr;
  use valheim_asm::isa::rv64::{CSRAddr, RV64Instr};
  use valheim_asm::isa::typed::{Imm32, Rd, Reg, Rs1, Rs2, AQ, RL};
  use valheim_core::cpu::bus::{RV64_MEMORY_BASE, RV64_MEMORY_END, VIRT_MROM_BASE};
  use valheim_core::cpu::irq::Exception;
  use valheim_core::cpu::mmu::{
    PAGE_SHIFT, PTE_A, PTE_R, PTE_V, PTE_X, SATP64_MODE_SHIFT, VMMode,
  };
  use valheim_core::cpu::PrivilegeMode;

  const SV39_ROOT_PAGE: u64 = RV64_MEMORY_BASE + 0x1000;
  const SV39_LEVEL1_PAGE: u64 = RV64_MEMORY_BASE + 0x2000;
  const SV39_LEVEL0_PAGE: u64 = RV64_MEMORY_BASE + 0x3000;

  fn install_sv39_mapping(
    cpu: &mut RV64Cpu,
    vaddr: VirtAddr,
    paddr: VirtAddr,
    flags: u64,
  ) -> VirtAddr {
    let vpn0 = (vaddr.0 >> 12) & 0x1ff;
    let vpn1 = (vaddr.0 >> 21) & 0x1ff;
    let vpn2 = (vaddr.0 >> 30) & 0x1ff;
    let pointer = |page: u64| ((page >> PAGE_SHIFT) << 10) | (1 << PTE_V);

    cpu
      .bus
      .write::<u64>(VirtAddr(SV39_ROOT_PAGE + vpn2 * 8), pointer(SV39_LEVEL1_PAGE))
      .unwrap();
    cpu
      .bus
      .write::<u64>(VirtAddr(SV39_LEVEL1_PAGE + vpn1 * 8), pointer(SV39_LEVEL0_PAGE))
      .unwrap();
    let leaf = VirtAddr(SV39_LEVEL0_PAGE + vpn0 * 8);
    cpu
      .bus
      .write::<u64>(leaf, ((paddr.0 >> PAGE_SHIFT) << 10) | flags)
      .unwrap();
    leaf
  }

  fn enable_sv39(cpu: &mut RV64Cpu) {
    cpu.mode = PrivilegeMode::Supervisor;
    cpu
      .csrs
      .write_unchecked(
        SATP,
        ((VMMode::SV39 as u64) << SATP64_MODE_SHIFT) | (SV39_ROOT_PAGE >> PAGE_SHIFT),
      )
      .unwrap();
    cpu.sync_pagetable();
  }

  #[test]
  fn hot_straight_line_switches_to_native_code() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.bus.write::<u32>(pc, 0x0010_8093).unwrap(); // addi x1, x1, 1
    cpu.bus.write::<u32>(pc + VirtAddr(4), 0x0000_006f).unwrap(); // jal x0, 0
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(2);

    for expected in 1..=3 {
      cpu.write_pc(pc);
      let outcome = jit.execute(&mut cpu, 2);
      assert_eq!(outcome, ExecOutcome::new(2, Ok(())));
      assert_eq!(cpu.read_reg(Reg::X(Fin::new(1))), Some(expected));
    }

    assert_eq!(jit.stats().compiled_blocks, 1);
    assert_eq!(jit.stats().native_executions, 1);
  }

  #[test]
  fn timer_budget_uses_decoded_prefix_instead_of_oversized_native_block() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.write_pc(pc);
    cpu.bus.write::<u32>(pc, 0x0010_8093).unwrap();
    cpu.bus.write::<u32>(pc + VirtAddr(4), 0x0010_8093).unwrap();
    cpu.bus.write::<u32>(pc + VirtAddr(8), 0x0000_006f).unwrap();
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    assert_eq!(jit.execute(&mut cpu, 1).attempted, 1);
    assert_eq!(cpu.read_pc(), pc + VirtAddr(4));
  }

  #[test]
  fn executor_stops_at_a_basic_block_boundary() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let x1 = Reg::X(Fin::new(1));
    cpu.bus.write::<u32>(pc, 0x0010_8093).unwrap(); // addi x1, x1, 1
    cpu.bus.write::<u32>(pc + VirtAddr(4), 0xffdff06f).unwrap(); // jal x0, -4
    cpu.write_pc(pc);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(2, Ok(())));
    assert_eq!(cpu.read_reg(x1), Some(1));
    assert_eq!(jit.stats().dispatches, 1);
    assert_eq!(jit.stats().guest_instructions, 2);
    assert_eq!(jit.stats().native_executions, 0);
  }

  #[test]
  fn unsupported_instruction_uses_negative_cache() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu.bus.write::<u32>(pc, 0x0000_0073).unwrap(); // ecall
    cpu.write_pc(pc);
    let mut jit = JitExecutor::new().unwrap();

    assert_eq!(
      jit.execute(&mut cpu, 32),
      ExecOutcome::new(1, Err(Exception::MachineEcall))
    );
    assert_eq!(
      jit.execute(&mut cpu, 32),
      ExecOutcome::new(1, Err(Exception::MachineEcall))
    );
    assert_eq!(jit.stats().negative_blocks, 1);
    assert_eq!(jit.stats().negative_cache_hits, 1);
    assert_eq!(jit.stats().fallback_system, 2);
    assert_eq!(jit.stats().fallback_instructions, 2);
  }

  #[test]
  fn module_rotation_keeps_retired_function_pointers_valid() {
    let mut cpu = RV64Cpu::new(None);
    let first = VirtAddr(RV64_MEMORY_BASE);
    let second = first + VirtAddr(0x100);
    let x1 = Reg::X(Fin::new(1));
    let x2 = Reg::X(Fin::new(2));
    cpu.bus.write::<u32>(first, 0x0010_8093).unwrap(); // addi x1, x1, 1
    cpu
      .bus
      .write::<u32>(first + VirtAddr(4), 0xffdff06f)
      .unwrap();
    cpu.bus.write::<u32>(second, 0x0011_0113).unwrap(); // addi x2, x2, 1
    cpu
      .bus
      .write::<u32>(second + VirtAddr(4), 0xffdff06f)
      .unwrap();
    let mut jit = JitExecutor::new()
      .unwrap()
      .with_hot_threshold(1)
      .with_max_compiled_blocks(1);

    cpu.write_pc(first);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    cpu.write_pc(second);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    cpu.write_pc(first);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));

    assert_eq!(cpu.read_reg(x1), Some(2));
    assert_eq!(cpu.read_reg(x2), Some(1));
    assert!(jit.stats().module_rotations >= 1);
    assert!(jit.stats().native_executions >= 1);
  }

  #[test]
  fn code_arena_limit_clears_all_native_pointers_before_freeing_modules() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let x1 = Reg::X(Fin::new(1));
    cpu.bus.write::<u32>(pc, 0x0010_8093).unwrap(); // addi x1, x1, 1
    cpu
      .bus
      .write::<u32>(pc + VirtAddr(4), 0xffdff06f)
      .unwrap();
    let mut jit = JitExecutor::new()
      .unwrap()
      .with_hot_threshold(1)
      .with_max_live_code_bytes(1);

    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert!(jit.stats().live_code_bytes > 1);

    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(jit.stats().code_cache_flushes, 1);
    assert!(jit.retired_backends.is_empty());

    jit.max_live_code_bytes = u64::MAX;
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(cpu.read_reg(x1), Some(3));
    assert_eq!(jit.stats().native_executions, 1);
  }

  #[test]
  fn fence_sfence_and_satp_write_invalidate_compiled_cache_generations() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let fence_pc = pc + VirtAddr(0x100);
    let sfence_pc = pc + VirtAddr(0x104);
    let satp_pc = pc + VirtAddr(0x108);
    let x1 = Reg::X(Fin::new(1));
    cpu.bus.write::<u32>(pc, 0x0010_0093).unwrap(); // addi x1, x0, 1
    cpu
      .bus
      .write::<u32>(pc + VirtAddr(4), 0xffdff06f)
      .unwrap();
    cpu
      .bus
      .write::<u32>(
        fence_pc,
        RV64Instr::FENCE_I(Rd(Reg::ZERO), Rs1(Reg::ZERO), Imm32::from(0)).encode32(),
      )
      .unwrap();
    cpu
      .bus
      .write::<u32>(
        sfence_pc,
        RV64Instr::SFENCE_VMA(Rs1(Reg::ZERO), Rs2(Reg::ZERO)).encode32(),
      )
      .unwrap();
    cpu
      .bus
      .write::<u32>(
        satp_pc,
        RV64Instr::CSRRW(
          Rd(Reg::ZERO),
          Rs1(x1),
          CSRAddr(Imm32::from(SATP as u32)),
        )
        .encode32(),
      )
      .unwrap();
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(jit.stats().compiled_blocks, 1);
    cpu.bus.write::<u32>(pc, 0x0020_0093).unwrap(); // addi x1, x0, 2
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(cpu.read_reg(x1), Some(1));

    cpu.write_pc(fence_pc);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.icache_epoch, 1);
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(cpu.read_reg(x1), Some(2));
    assert_eq!(jit.seen_icache_epoch, 1);

    cpu.write_pc(sfence_pc);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.translation_epoch, 1);
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(jit.seen_translation_epoch, 1);

    cpu.write_reg(x1, 0);
    cpu.write_pc(satp_pc);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.translation_epoch, 2);
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(jit.seen_translation_epoch, 2);
    assert_eq!(jit.stats().compiled_blocks, 4);
  }

  #[test]
  fn native_load_store_round_trip_uses_core_memory_path() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let data = VirtAddr(RV64_MEMORY_BASE + 0x100);
    cpu.bus.write::<u32>(pc, 0x0000_b103).unwrap(); // ld x2, 0(x1)
    cpu.bus.write::<u32>(pc + VirtAddr(4), 0x0020_b423).unwrap(); // sd x2, 8(x1)
    cpu.bus.write::<u32>(pc + VirtAddr(8), 0x0000_006f).unwrap(); // jal x0, 0
    cpu.bus.write::<u64>(data, 0x0123_4567_89ab_cdef).unwrap();
    cpu.write_reg(Reg::X(Fin::new(1)), data.0);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    for _ in 0..3 {
      cpu.write_pc(pc);
      assert_eq!(jit.execute(&mut cpu, 3), ExecOutcome::new(3, Ok(())));
    }

    assert_eq!(
      cpu.bus.read::<u64>(data + VirtAddr(8)).unwrap(),
      0x0123_4567_89ab_cdef
    );
    assert_eq!(jit.stats().native_executions, 2);
    assert_eq!(jit.stats().tlb_misses, 2);
    assert_eq!(jit.stats().tlb_hits, 2);
  }

  #[test]
  fn native_memory_fault_commits_only_older_guest_instructions() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let valid = VirtAddr(RV64_MEMORY_BASE + 0x100);
    cpu.bus.write::<u32>(pc, 0x0011_8193).unwrap(); // addi x3, x3, 1
    cpu.bus.write::<u32>(pc + VirtAddr(4), 0x0000_b103).unwrap(); // ld x2, 0(x1)
    cpu.bus.write::<u32>(pc + VirtAddr(8), 0x0000_006f).unwrap(); // jal x0, 0
    cpu.bus.write::<u64>(valid, 7).unwrap();
    cpu.write_reg(Reg::X(Fin::new(1)), valid.0);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 3), ExecOutcome::new(3, Ok(())));

    cpu.write_pc(pc);
    cpu.write_reg(Reg::X(Fin::new(1)), 0);
    let outcome = jit.execute(&mut cpu, 3);
    assert_eq!(outcome, ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), pc + VirtAddr(4));
    assert_eq!(cpu.instr, 0x0011_8193);
    assert_eq!(cpu.read_reg(Reg::X(Fin::new(3))), Some(2));

    let outcome = jit.execute(&mut cpu, 3);
    assert_eq!(
      outcome,
      ExecOutcome::new(1, Err(Exception::LoadAccessFault(VirtAddr(0))))
    );
    assert_eq!(cpu.read_pc(), pc + VirtAddr(4));
    assert_eq!(cpu.instr, 0x0000_b103);
    assert_eq!(cpu.read_reg(Reg::X(Fin::new(3))), Some(2));
    assert_eq!(jit.stats().native_executions, 1);
  }

  #[test]
  fn native_m_extension_handles_division_edge_cases() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let x = |index| Reg::X(Fin::new(index));
    let instructions = [
      RV32Instr::DIV(Rd(x(3)), Rs1(x(1)), Rs2(x(2))).encode32(),
      RV32Instr::REM(Rd(x(4)), Rs1(x(1)), Rs2(x(2))).encode32(),
      RV64Instr::DIVW(Rd(x(5)), Rs1(x(7)), Rs2(x(8))).encode32(),
      RV64Instr::REMUW(Rd(x(6)), Rs1(x(7)), Rs2(x(8))).encode32(),
      0x0000_006f,
    ];
    for (index, instruction) in instructions.into_iter().enumerate() {
      cpu
        .bus
        .write::<u32>(pc + VirtAddr(index as u64 * 4), instruction)
        .unwrap();
    }
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.write_reg(x(1), 12);
    cpu.write_reg(x(2), 3);
    cpu.write_reg(x(7), 12);
    cpu.write_reg(x(8), 3);
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 5), ExecOutcome::new(5, Ok(())));

    cpu.write_reg(x(1), i64::MIN as u64);
    cpu.write_reg(x(2), u64::MAX);
    cpu.write_reg(x(7), 0x8000_0000);
    cpu.write_reg(x(8), 0);
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 5), ExecOutcome::new(5, Ok(())));
    assert_eq!(cpu.read_reg(x(3)), Some(i64::MIN as u64));
    assert_eq!(cpu.read_reg(x(4)), Some(0));
    assert_eq!(cpu.read_reg(x(5)), Some(u64::MAX));
    assert_eq!(cpu.read_reg(x(6)), Some(0xffff_ffff_8000_0000));
    assert_eq!(jit.stats().native_executions, 1);
    assert_eq!(jit.stats().compile_failures, 0);
  }

  #[test]
  fn native_a_extension_uses_atomic_helper() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let data = VirtAddr(RV64_MEMORY_BASE + 0x200);
    let x = |index| Reg::X(Fin::new(index));
    let amoadd =
      RV64Instr::AMOADD_D(Rd(x(3)), Rs1(x(1)), Rs2(x(2)), AQ(false), RL(false)).encode32();
    cpu.bus.write::<u32>(pc, amoadd).unwrap();
    cpu.bus.write::<u32>(pc + VirtAddr(4), 0x0000_006f).unwrap();
    cpu.write_reg(x(1), data.0);
    cpu.write_reg(x(2), 7);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.bus.write::<u64>(data, 5).unwrap();
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    cpu.bus.write::<u64>(data, 10).unwrap();
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));

    assert_eq!(cpu.read_reg(x(3)), Some(10));
    assert_eq!(cpu.bus.read::<u64>(data), Ok(17));
    assert_eq!(jit.stats().native_executions, 1);
    assert_eq!(jit.stats().compile_failures, 0);
  }

  #[test]
  fn native_lr_sc_faults_preserve_destinations_and_reservation_ordering() {
    let mut cpu = RV64Cpu::new(None);
    let lr_pc = VirtAddr(RV64_MEMORY_BASE);
    let sc_pc = lr_pc + VirtAddr(0x100);
    let data = VirtAddr(RV64_MEMORY_BASE + 0x200);
    let x = |index| Reg::X(Fin::new(index));
    cpu
      .bus
      .write::<u32>(
        lr_pc,
        RV64Instr::LR_D(Rd(x(3)), Rs1(x(1)), AQ(false), RL(false)).encode32(),
      )
      .unwrap();
    cpu
      .bus
      .write::<u32>(lr_pc + VirtAddr(4), 0x0000_006f)
      .unwrap();
    cpu
      .bus
      .write::<u32>(
        sc_pc,
        RV64Instr::SC_D(Rd(x(4)), Rs1(x(1)), Rs2(x(2)), AQ(false), RL(false)).encode32(),
      )
      .unwrap();
    cpu
      .bus
      .write::<u32>(sc_pc + VirtAddr(4), 0x0000_006f)
      .unwrap();
    cpu.bus.write::<u64>(data, 5).unwrap();
    cpu.write_reg(x(1), data.0);
    cpu.write_reg(x(2), 9);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.write_pc(lr_pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    cpu.write_pc(sc_pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));

    cpu.write_reg(x(1), 0);
    cpu.write_reg(x(3), 0x1111);
    cpu.write_pc(lr_pc);
    assert_eq!(
      jit.execute(&mut cpu, 2),
      ExecOutcome::new(1, Err(Exception::LoadAccessFault(VirtAddr(0))))
    );
    assert_eq!(cpu.read_reg(x(3)), Some(0x1111));

    cpu.reserved.push(VirtAddr(0));
    cpu.write_reg(x(4), 0x2222);
    cpu.write_pc(sc_pc);
    assert_eq!(
      jit.execute(&mut cpu, 2),
      ExecOutcome::new(1, Err(Exception::StoreAccessFault(VirtAddr(0))))
    );
    assert_eq!(cpu.read_reg(x(4)), Some(0x2222));
    assert!(!cpu.reserved.contains(&VirtAddr(0)));
    assert_eq!(jit.stats().native_executions, 2);
  }

  #[test]
  fn native_slow_memory_access_fault_reports_the_sv39_guest_address() {
    const GUEST_CODE: VirtAddr = VirtAddr(0x1234_4000);
    const GUEST_DATA: VirtAddr = VirtAddr(0x1234_5000);
    const PHYSICAL_CODE: VirtAddr = VirtAddr(RV64_MEMORY_BASE + 0x4000);
    const PHYSICAL_DATA: VirtAddr = VirtAddr(RV64_MEMORY_BASE + 0x5000);

    let mut cpu = RV64Cpu::new(None);
    cpu
      .bus
      .write::<u32>(PHYSICAL_CODE, 0x0000_b103)
      .unwrap(); // ld x2, 0(x1)
    cpu
      .bus
      .write::<u32>(PHYSICAL_CODE + VirtAddr(4), 0x0000_006f)
      .unwrap(); // jal x0, 0
    cpu
      .bus
      .write::<u64>(PHYSICAL_DATA, 0x0123_4567_89ab_cdef)
      .unwrap();

    let code_flags = (1 << PTE_V) | (1 << PTE_X) | (1 << PTE_A);
    let data_flags = (1 << PTE_V) | (1 << PTE_R) | (1 << PTE_A);
    install_sv39_mapping(&mut cpu, GUEST_CODE, PHYSICAL_CODE, code_flags);
    let data_leaf = install_sv39_mapping(&mut cpu, GUEST_DATA, PHYSICAL_DATA, data_flags);
    enable_sv39(&mut cpu);

    let x1 = Reg::X(Fin::new(1));
    let x2 = Reg::X(Fin::new(2));
    cpu.write_reg(x1, GUEST_DATA.0);
    cpu.write_pc(GUEST_CODE);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    // The first execution is decoded: it compiles the TB without populating the JIT data TLB.
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(cpu.read_reg(x2), Some(0x0123_4567_89ab_cdef));
    assert_eq!(jit.stats().compiled_blocks, 1);

    // Retarget the leaf before the first native execution so its TLB miss takes the slow path.
    let inaccessible_pte = ((RV64_MEMORY_END >> PAGE_SHIFT) << 10) | data_flags;
    cpu.bus.write::<u64>(data_leaf, inaccessible_pte).unwrap();
    cpu.write_reg(x2, 0xfeed_face_cafe_beef);
    cpu.write_pc(GUEST_CODE);

    assert_eq!(
      jit.execute(&mut cpu, 2),
      ExecOutcome::new(1, Err(Exception::LoadAccessFault(GUEST_DATA))),
    );
    assert_ne!(GUEST_DATA, VirtAddr(RV64_MEMORY_END));
    assert_eq!(cpu.read_reg(x2), Some(0xfeed_face_cafe_beef));
    assert_eq!(jit.stats().native_executions, 1);
    assert_eq!(jit.stats().memory_slow_paths, 1);
  }

  #[test]
  fn native_mmio_load_after_a_prefix_resumes_at_the_next_dispatch() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let valid = VirtAddr(RV64_MEMORY_BASE + 0x200);
    let mmio = VirtAddr(VIRT_MROM_BASE + 0x100);
    let x1 = Reg::X(Fin::new(1));
    let x2 = Reg::X(Fin::new(2));
    cpu.bus.write::<u32>(pc, 0x0011_8193).unwrap(); // addi x3, x3, 1
    cpu.bus.write::<u32>(pc + VirtAddr(4), 0x0000_b103).unwrap(); // ld x2, 0(x1)
    cpu.bus.write::<u32>(pc + VirtAddr(8), 0x0000_006f).unwrap();
    cpu.bus.write::<u64>(valid, 1).unwrap();
    cpu.bus.write::<u64>(mmio, 0xfeed_face_cafe_beef).unwrap();
    cpu.write_reg(x1, valid.0);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 3), ExecOutcome::new(3, Ok(())));
    cpu.write_reg(x1, mmio.0);
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 3), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), pc + VirtAddr(4));
    assert_eq!(cpu.read_reg(x2), Some(1));
    assert_eq!(jit.execute(&mut cpu, 3), ExecOutcome::new(1, Ok(())));

    assert_eq!(cpu.read_reg(x2), Some(0xfeed_face_cafe_beef));
    assert_eq!(cpu.read_pc(), pc + VirtAddr(8));
    assert_eq!(jit.stats().memory_slow_paths, 1);
  }
}
