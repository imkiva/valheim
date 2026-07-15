use std::collections::hash_map::Entry;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::time::Instant;

use rustc_hash::FxHashMap;
use valheim_asm::isa::rv32::RV32Instr;
use valheim_asm::isa::rv64::RV64Instr;
use valheim_asm::isa::typed::{Instr, Reg};
use valheim_core::cpu::csr::CSRMap::{MSTATUS, SATP};
use valheim_core::cpu::RV64Cpu;
use valheim_core::interp::{ExecOutcome, RV64Executor};
use valheim_core::memory::VirtAddr;

use crate::block::{
  is_native_csr_read, BlockBuild, FallbackKind, GuestBlock, GuestInst, MAX_BLOCK_LEN,
};
use crate::cranelift::{CompiledBlock, CraneliftBackend, JitError, JitFrame};
use crate::memory::{
  exception_from_frame, is_deferred_memory_exit, is_slow_memory_exit, SoftwareTlb, FAULT_NONE,
};

const DEFAULT_HOT_THRESHOLD: u32 = 750;
// Keep one Cranelift module comfortably below the x86-64 PLT/GOT ±2 GiB relocation limit. The
// generated memory/TLB side exits make each function substantially larger than a pure ALU TB.
const DEFAULT_MAX_COMPILED_BLOCKS: usize = 4 * 1024;
const DEFAULT_MAX_LIVE_CODE_BYTES: u64 = 128 * 1024 * 1024;
const FAST_CACHE_SET_COUNT: usize = 4 * 1024;
const MSTATUS_TRANSLATION_MASK: u64 = (0b11 << 11) | (1 << 17) | (1 << 18) | (1 << 19);

/// A stats-only classification of instructions that terminate a TB and use the shared CPU
/// fallback. CSR forms distinguish zero-source reads from operations that can write the CSR so
/// profiling can select safe lowering candidates without inspecting every raw encoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SystemFallback {
  Ecall,
  Ebreak,
  FenceI,
  Sret,
  Mret,
  Wfi,
  SfenceVma,
  SinvalVma,
  SfenceWInvalidation,
  SfenceInvalidationIr,
  Csrrw(u16),
  CsrrsRead(u16),
  CsrrsWrite(u16),
  CsrrcRead(u16),
  CsrrcWrite(u16),
  Csrrwi(u16),
  CsrrsiRead(u16),
  CsrrsiWrite(u16),
  CsrrciRead(u16),
  CsrrciWrite(u16),
  Other(u32),
}

impl SystemFallback {
  fn classify(inst: GuestInst) -> Self {
    match inst.decoded {
      Instr::RV32(RV32Instr::ECALL) => Self::Ecall,
      Instr::RV32(RV32Instr::EBREAK) => Self::Ebreak,
      Instr::RV64(RV64Instr::FENCE_I(..)) => Self::FenceI,
      Instr::RV64(RV64Instr::SRET) => Self::Sret,
      Instr::RV64(RV64Instr::MRET) => Self::Mret,
      Instr::RV64(RV64Instr::WFI) => Self::Wfi,
      Instr::RV64(RV64Instr::SFENCE_VMA(..)) => Self::SfenceVma,
      Instr::RV64(RV64Instr::SINVAL_VMA(..)) => Self::SinvalVma,
      Instr::RV64(RV64Instr::SFENCE_W_INVAL) => Self::SfenceWInvalidation,
      Instr::RV64(RV64Instr::SFENCE_INVAL_IR) => Self::SfenceInvalidationIr,
      Instr::RV64(RV64Instr::CSRRW(_, _, csr)) => Self::Csrrw(csr.value()),
      Instr::RV64(RV64Instr::CSRRS(_, rs1, csr)) => {
        if is_zero_register(rs1.0) {
          Self::CsrrsRead(csr.value())
        } else {
          Self::CsrrsWrite(csr.value())
        }
      }
      Instr::RV64(RV64Instr::CSRRC(_, rs1, csr)) => {
        if is_zero_register(rs1.0) {
          Self::CsrrcRead(csr.value())
        } else {
          Self::CsrrcWrite(csr.value())
        }
      }
      Instr::RV64(RV64Instr::CSRRWI(_, _, csr)) => Self::Csrrwi(csr.value()),
      Instr::RV64(RV64Instr::CSRRSI(_, imm, csr)) => {
        if imm.value() == 0 {
          Self::CsrrsiRead(csr.value())
        } else {
          Self::CsrrsiWrite(csr.value())
        }
      }
      Instr::RV64(RV64Instr::CSRRCI(_, imm, csr)) => {
        if imm.value() == 0 {
          Self::CsrrciRead(csr.value())
        } else {
          Self::CsrrciWrite(csr.value())
        }
      }
      _ => Self::Other(inst.raw),
    }
  }
}

fn is_zero_register(reg: Reg) -> bool {
  match reg {
    Reg::ZERO => true,
    Reg::X(index) => index.value() == 0,
    _ => false,
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct TbKey {
  pc: u64,
  privilege: u8,
  satp: u64,
}

impl TbKey {
  fn new(cpu: &RV64Cpu, privilege: u8, satp: u64) -> Self {
    Self {
      pc: cpu.read_pc().0,
      privilege,
      satp,
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TlbContext {
  dram_backing_id: u64,
  privilege: u8,
  satp: u64,
  mstatus: u64,
  translation_epoch: u64,
}

impl TlbContext {
  fn new(cpu: &RV64Cpu) -> Self {
    Self {
      dram_backing_id: cpu.bus.mem.backing_id(),
      privilege: cpu.mode as u8,
      satp: cpu.csrs.read_unchecked(SATP),
      mstatus: cpu.csrs.read_unchecked(MSTATUS) & MSTATUS_TRANSLATION_MASK,
      translation_epoch: cpu.translation_epoch,
    }
  }
}

struct CachedBlock {
  block: GuestBlock,
  chainable: bool,
  executions: u32,
  compiled: Option<CompiledBlock>,
  compile_failed: bool,
  successors: SuccessorCache,
}

#[derive(Clone, Copy)]
enum CacheEntry {
  Block(usize),
  Fallback {
    kind: FallbackKind,
    inst: GuestInst,
  },
}

#[derive(Clone, Copy)]
struct FastCacheWay {
  key: TbKey,
  block_index: usize,
}

#[derive(Clone, Copy)]
struct FastCacheSet {
  generation: u64,
  ways: [Option<FastCacheWay>; 2],
  next_way: u8,
}

#[derive(Clone, Copy)]
struct SuccessorLink {
  generation: u64,
  key: TbKey,
  block_index: usize,
}

#[derive(Clone, Copy)]
struct SuccessorCache {
  links: [Option<SuccessorLink>; 2],
  next_way: u8,
}

impl SuccessorCache {
  const EMPTY: Self = Self {
    links: [None, None],
    next_way: 0,
  };

  #[inline(always)]
  fn lookup(&self, generation: u64, key: TbKey) -> Option<usize> {
    if let Some(link) = self.links[0] {
      if link.generation == generation && link.key == key {
        return Some(link.block_index);
      }
    }
    if let Some(link) = self.links[1] {
      if link.generation == generation && link.key == key {
        return Some(link.block_index);
      }
    }
    None
  }

  #[inline(always)]
  fn insert(&mut self, generation: u64, key: TbKey, block_index: usize) {
    let link = SuccessorLink {
      generation,
      key,
      block_index,
    };
    for slot in &mut self.links {
      if slot.map(|old| old.key == key).unwrap_or(true) {
        *slot = Some(link);
        return;
      }
    }
    let replacement = self.next_way as usize;
    self.links[replacement] = Some(link);
    self.next_way ^= 1;
  }
}

impl FastCacheSet {
  const EMPTY: Self = Self {
    generation: 0,
    ways: [None, None],
    next_way: 0,
  };
}

/// A small compiled-TB front cache. The authoritative map stores stable indices into the block
/// arena, so this cache never owns guest blocks or native function pointers. Invalidating its
/// generation before clearing the arena makes stale indices unreachable without an O(n) sweep.
struct FastCache {
  sets: Box<[FastCacheSet]>,
  generation: u64,
}

impl FastCache {
  fn new() -> Self {
    debug_assert!(FAST_CACHE_SET_COUNT.is_power_of_two());
    Self {
      sets: vec![FastCacheSet::EMPTY; FAST_CACHE_SET_COUNT].into_boxed_slice(),
      generation: 1,
    }
  }

  #[inline(always)]
  fn set_index(key: TbKey) -> usize {
    let mut hash = (key.pc >> 1)
      ^ key.satp
      ^ key.satp.rotate_right(23)
      ^ (key.privilege as u64).wrapping_mul(0x9e37_79b9);
    hash ^= hash >> 32;
    hash as usize & (FAST_CACHE_SET_COUNT - 1)
  }

  #[inline(always)]
  fn lookup(&self, key: TbKey) -> Option<usize> {
    let set = &self.sets[Self::set_index(key)];
    if set.generation != self.generation {
      return None;
    }
    if let Some(way) = set.ways[0] {
      if way.key == key {
        return Some(way.block_index);
      }
    }
    if let Some(way) = set.ways[1] {
      if way.key == key {
        return Some(way.block_index);
      }
    }
    None
  }

  #[inline(always)]
  fn insert(&mut self, key: TbKey, block_index: usize) {
    let set = &mut self.sets[Self::set_index(key)];
    if set.generation != self.generation {
      *set = FastCacheSet {
        generation: self.generation,
        ..FastCacheSet::EMPTY
      };
    }
    let way = FastCacheWay { key, block_index };
    for slot in &mut set.ways {
      if slot.map(|old| old.key == key).unwrap_or(true) {
        *slot = Some(way);
        return;
      }
    }
    let replacement = set.next_way as usize;
    set.ways[replacement] = Some(way);
    set.next_way ^= 1;
  }

  #[inline(always)]
  fn generation(&self) -> u64 {
    self.generation
  }

  fn invalidate(&mut self) {
    self.generation = self.generation.wrapping_add(1);
    if self.generation == 0 {
      self.sets.fill(FastCacheSet::EMPTY);
      self.generation = 1;
    }
  }
}

#[derive(Debug)]
struct ExecuteOne {
  outcome: ExecOutcome,
  chain_source: Option<usize>,
}

impl ExecuteOne {
  fn stop(outcome: ExecOutcome) -> Self {
    Self {
      outcome,
      chain_source: None,
    }
  }
}

struct NativeExecution {
  outcome: ExecOutcome,
  pending_index: Option<usize>,
  completed_block: bool,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct JitStats {
  pub hot_threshold: u32,
  pub max_block_len: usize,
  pub max_compiled_blocks_per_module: usize,
  pub max_live_code_bytes: u64,
  pub cache_hits: u64,
  pub cache_misses: u64,
  pub successor_hits: u64,
  pub successor_misses: u64,
  pub successor_installs: u64,
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
  backend: CraneliftBackend,
  retired_backends: Vec<CraneliftBackend>,
  tlb: SoftwareTlb,
  tlb_context: Option<TlbContext>,
  cache: FxHashMap<TbKey, CacheEntry>,
  blocks: Vec<CachedBlock>,
  fast_cache: FastCache,
  // Usually empty. A small vector avoids a hash on every TB while still allowing an interrupt
  // handler to create its own deferred MMIO instruction before an older one resumes.
  pending_slow_memory: Vec<(TbKey, GuestInst)>,
  hot_threshold: u32,
  max_block_len: usize,
  max_compiled_blocks: usize,
  max_live_code_bytes: u64,
  compiled_in_generation: usize,
  live_code_bytes: u64,
  stats_enabled: bool,
  stats_interval: Option<u64>,
  compilation_samples: Vec<u64>,
  system_fallbacks: FxHashMap<SystemFallback, u64>,
  seen_sfence_epoch: u64,
  seen_icache_epoch: u64,
  stats: JitStats,
}

impl JitExecutor {
  pub fn new() -> Result<Self, JitError> {
    Ok(Self {
      backend: CraneliftBackend::new()?,
      retired_backends: Vec::new(),
      tlb: SoftwareTlb::new(),
      tlb_context: None,
      cache: FxHashMap::default(),
      blocks: Vec::new(),
      fast_cache: FastCache::new(),
      pending_slow_memory: Vec::new(),
      hot_threshold: DEFAULT_HOT_THRESHOLD,
      max_block_len: MAX_BLOCK_LEN,
      max_compiled_blocks: DEFAULT_MAX_COMPILED_BLOCKS,
      max_live_code_bytes: DEFAULT_MAX_LIVE_CODE_BYTES,
      compiled_in_generation: 0,
      live_code_bytes: 0,
      stats_enabled: true,
      stats_interval: None,
      compilation_samples: Vec::new(),
      system_fallbacks: FxHashMap::default(),
      seen_sfence_epoch: 0,
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

  pub fn with_stats_enabled(mut self, enabled: bool) -> Self {
    self.stats_enabled = enabled;
    self.tlb.set_stats_enabled(enabled);
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
    stats.live_code_bytes = self.live_code_bytes;
    if !self.compilation_samples.is_empty() {
      let mut samples = self.compilation_samples.clone();
      samples.sort_unstable();
      stats.compilation_average_nanos = stats.compilation_nanos / samples.len() as u64;
      stats.compilation_p50_nanos = samples[(samples.len() - 1) * 50 / 100];
      stats.compilation_p95_nanos = samples[(samples.len() - 1) * 95 / 100];
    }
    stats
  }

  pub fn system_fallbacks(&self) -> Vec<(SystemFallback, u64)> {
    let mut counts: Vec<_> = self
      .system_fallbacks
      .iter()
      .map(|(instruction, count)| (*instruction, *count))
      .collect();
    counts.sort_unstable_by_key(|(instruction, _)| *instruction);
    counts
  }

  fn diagnostics_string(&self) -> String {
    format!(
      "{:?} system_fallbacks={:?}",
      self.stats(),
      self.system_fallbacks(),
    )
  }

  fn clear_block_cache(&mut self) {
    // Make every front-cache index unreachable before dropping its arena element.
    self.fast_cache.invalidate();
    self.cache.clear();
    self.blocks.clear();
  }

  fn replace_code_backend(&mut self) -> bool {
    let Ok(backend) = CraneliftBackend::new() else {
      self.max_compiled_blocks = usize::MAX;
      self.max_live_code_bytes = u64::MAX;
      if self.stats_enabled {
        self.stats.compile_failures += 1;
      }
      return false;
    };

    self.clear_block_cache();
    let current = std::mem::replace(&mut self.backend, backend);
    unsafe { current.free_memory() };
    for retired in self.retired_backends.drain(..) {
      unsafe { retired.free_memory() };
    }
    self.compiled_in_generation = 0;
    self.live_code_bytes = 0;
    true
  }

  fn invalidate_changed_epochs(&mut self, cpu: &RV64Cpu) {
    if self.seen_sfence_epoch != cpu.sfence_epoch || self.seen_icache_epoch != cpu.icache_epoch {
      self.clear_block_cache();
      self.pending_slow_memory.clear();
      self.replace_code_backend();
      self.seen_sfence_epoch = cpu.sfence_epoch;
      self.seen_icache_epoch = cpu.icache_epoch;
    }
  }

  fn sync_tlb_context(&mut self, cpu: &RV64Cpu) -> TlbContext {
    let context = TlbContext::new(cpu);
    if self.tlb_context != Some(context) {
      self.tlb.invalidate();
      self.tlb_context = Some(context);
    }
    context
  }

  fn rotate_code_cache_if_needed(&mut self) {
    let module_full = self.compiled_in_generation >= self.max_compiled_blocks;
    let arena_full = self.live_code_bytes >= self.max_live_code_bytes;
    if !module_full && !arena_full {
      return;
    }
    if arena_full {
      if self.replace_code_backend() {
        if self.stats_enabled {
          self.stats.module_rotations += 1;
          self.stats.code_cache_flushes += 1;
        }
      }
      return;
    }
    match CraneliftBackend::new() {
      Ok(backend) => {
        let old = std::mem::replace(&mut self.backend, backend);
        // Keep the old module alive because cache entries still hold its function pointers. Rust
        // successor links only retain arena indices, so modules remain independent and there are
        // no native cross-module jumps to patch.
        self.retired_backends.push(old);
        self.compiled_in_generation = 0;
        if self.stats_enabled {
          self.stats.module_rotations += 1;
        }
      }
      Err(_) => {
        // Keep valid code pointers in the old module. Avoid retrying allocation on every TB.
        self.max_compiled_blocks = usize::MAX;
        if self.stats_enabled {
          self.stats.compile_failures += 1;
        }
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

  fn new_native_frame(tlb: &mut SoftwareTlb, cpu: &mut RV64Cpu) -> JitFrame {
    let xregs = cpu.regs.x.as_mut_ptr();
    let load_tlb = tlb.load_ptr();
    let store_tlb = tlb.store_ptr();
    let tlb_generation = tlb.generation();
    let tlb_stats = tlb.stats_ptr();
    JitFrame::new(cpu, xregs, load_tlb, store_tlb, tlb_generation, tlb_stats)
  }

  fn execute_native(
    frame: &mut JitFrame,
    cpu: &mut RV64Cpu,
    block: &GuestBlock,
    compiled: CompiledBlock,
    defer_first_slow: bool,
  ) -> NativeExecution {
    frame.reset_for_block(cpu, defer_first_slow);
    let attempted = unsafe { (compiled.entry)(frame) };
    if frame.exit_kind == FAULT_NONE {
      cpu.write_pc(VirtAddr(frame.next_pc));
      if attempted != 0 {
        cpu.instr = block.instructions[attempted as usize - 1].raw as u64;
      }
      return NativeExecution {
        outcome: ExecOutcome::new(attempted, Ok(())),
        pending_index: None,
        completed_block: true,
      };
    }
    Self::finish_native_exit(cpu, block, frame, attempted, defer_first_slow)
  }

  #[cold]
  #[inline(never)]
  fn finish_native_exit(
    cpu: &mut RV64Cpu,
    block: &GuestBlock,
    frame: &JitFrame,
    attempted: u32,
    defer_first_slow: bool,
  ) -> NativeExecution {
    if let Some(exception) = exception_from_frame(frame) {
      cpu.write_pc(VirtAddr(frame.fault_pc));
      cpu.instr = frame.raw_instr as u64;
      return NativeExecution {
        outcome: ExecOutcome::new(attempted, Err(exception)),
        pending_index: None,
        completed_block: false,
      };
    }
    if is_deferred_memory_exit(frame) {
      let completed = attempted
        .checked_sub(1)
        .expect("deferred memory exit attempted no instruction");
      debug_assert_eq!(
        block.instructions[completed as usize].pc,
        frame.fault_pc,
      );
      cpu.write_pc(VirtAddr(frame.fault_pc));
      if completed != 0 {
        cpu.instr = block.instructions[completed as usize - 1].raw as u64;
      }
      return NativeExecution {
        outcome: ExecOutcome::new(completed, Ok(())),
        pending_index: None,
        completed_block: false,
      };
    }
    if is_slow_memory_exit(frame) {
      let index = attempted
        .checked_sub(1)
        .expect("memory side exit attempted no instruction") as usize;
      debug_assert!(index < block.instructions.len());
      let inst = block.instructions[index];
      debug_assert_eq!(inst.pc, frame.fault_pc);
      cpu.write_pc(VirtAddr(frame.fault_pc));
      if index != 0 {
        cpu.instr = block.instructions[index - 1].raw as u64;
        return NativeExecution {
          outcome: ExecOutcome::new(index as u32, Ok(())),
          pending_index: Some(index),
          completed_block: false,
        };
      }
      if defer_first_slow {
        return NativeExecution {
          outcome: ExecOutcome::new(0, Ok(())),
          pending_index: Some(index),
          completed_block: false,
        };
      }
      cpu.instr = frame.raw_instr as u64;
      let result = cpu.execute(VirtAddr(inst.pc), inst.decoded, inst.len == 2);
      return NativeExecution {
        outcome: ExecOutcome::new(attempted, result),
        pending_index: None,
        completed_block: false,
      };
    }
    unreachable!("native frame reported an unknown exit kind")
  }

  fn fallback_one(
    stats: &mut JitStats,
    system_fallbacks: &mut FxHashMap<SystemFallback, u64>,
    stats_enabled: bool,
    cpu: &mut RV64Cpu,
    kind: FallbackKind,
    inst: GuestInst,
  ) -> ExecOutcome {
    if stats_enabled {
      stats.fallback_instructions += 1;
      match kind {
        FallbackKind::System => {
          stats.fallback_system += 1;
          let count = system_fallbacks.entry(SystemFallback::classify(inst)).or_default();
          *count = count.saturating_add(1);
        }
        FallbackKind::FloatingPoint => stats.fallback_floating_point += 1,
        FallbackKind::Other => stats.fallback_other += 1,
      }
    }
    debug_assert_eq!(cpu.read_pc(), VirtAddr(inst.pc));
    cpu.instr = inst.raw as u64;
    ExecOutcome::new(
      1,
      cpu.execute(VirtAddr(inst.pc), inst.decoded, inst.len == 2),
    )
  }

  fn remember_slow_memory(
    &mut self,
    cpu: &RV64Cpu,
    privilege: u8,
    satp: u64,
    inst: GuestInst,
  ) {
    let pending_key = TbKey::new(cpu, privilege, satp);
    if let Some((_, old)) = self
      .pending_slow_memory
      .iter_mut()
      .find(|(key, _)| *key == pending_key)
    {
      *old = inst;
    } else {
      self.pending_slow_memory.push((pending_key, inst));
    }
  }
}

impl JitExecutor {
  // This runs once per native TB. Leaving it out of line adds a measurable host call to every
  // successor edge and overwhelms the lookup saved by the inline cache.
  #[inline(always)]
  fn execute_compiled_block(
    &mut self,
    frame: &mut JitFrame,
    cpu: &mut RV64Cpu,
    budget: u32,
    native_only: bool,
    privilege: u8,
    satp: u64,
    key: TbKey,
    block_index: usize,
    link_source: Option<usize>,
  ) -> Option<ExecuteOne> {
    let cached = self
      .blocks
      .get(block_index)
      .expect("compiled block index outlived its arena");
    let block_len = cached.block.instructions.len();
    if block_len > budget as usize {
      return native_only.then(|| ExecuteOne::stop(ExecOutcome::new(0, Ok(()))));
    }
    if native_only && !cached.chainable {
      return Some(ExecuteOne::stop(ExecOutcome::new(0, Ok(()))));
    }
    let compiled = cached
      .compiled
      .expect("compiled cache path reached a decoded block");
    let chainable = cached.chainable;
    if self.stats_enabled {
      self.stats.native_executions += 1;
    }
    let native = Self::execute_native(
      frame,
      cpu,
      &cached.block,
      compiled,
      native_only,
    );
    let NativeExecution {
      outcome,
      pending_index,
      completed_block,
    } = native;
    let pending = pending_index.map(|index| cached.block.instructions[index as usize]);
    if let Some(inst) = pending {
      self.remember_slow_memory(cpu, privilege, satp, inst);
    }
    let chain_source = (completed_block && chainable).then_some(block_index);
    if chain_source.is_some() {
      if let Some(source_index) = link_source {
        let generation = self.fast_cache.generation();
        self
          .blocks
          .get_mut(source_index)
          .expect("successor source index outlived its arena")
          .successors
          .insert(generation, key, block_index);
        if self.stats_enabled {
          self.stats.successor_installs += 1;
        }
      }
    }
    Some(ExecuteOne {
      outcome,
      chain_source,
    })
  }

  fn execute_one(
    &mut self,
    frame: &mut JitFrame,
    cpu: &mut RV64Cpu,
    budget: u32,
    native_only: bool,
    privilege: u8,
    satp: u64,
    predecessor: Option<usize>,
  ) -> ExecuteOne {
    if budget == 0 || cpu.wfi {
      return ExecuteOne::stop(ExecOutcome::new(0, Ok(())));
    }

    let key = TbKey::new(cpu, privilege, satp);

    let pending_index = if self.pending_slow_memory.is_empty() {
      None
    } else {
      self
        .pending_slow_memory
        .iter()
        .position(|(pending_key, _)| *pending_key == key)
    };
    if let Some(index) = pending_index {
      if native_only {
        return ExecuteOne::stop(ExecOutcome::new(0, Ok(())));
      }
      let (_, inst) = self.pending_slow_memory.swap_remove(index);
      if self.stats_enabled {
        self.stats.fallback_instructions += 1;
        self.stats.fallback_memory += 1;
      }
      cpu.instr = inst.raw as u64;
      let result = cpu.execute(VirtAddr(inst.pc), inst.decoded, inst.len == 2);
      return ExecuteOne::stop(ExecOutcome::new(1, result));
    }

    let mut link_source = None;
    if let Some(source_index) = predecessor {
      let generation = self.fast_cache.generation();
      let target = self
        .blocks
        .get(source_index)
        .expect("successor source index outlived its arena")
        .successors
        .lookup(generation, key);
      if let Some(block_index) = target {
        if self.stats_enabled {
          self.stats.cache_hits += 1;
          self.stats.successor_hits += 1;
        }
        return self
          .execute_compiled_block(
            frame,
            cpu,
            budget,
            native_only,
            privilege,
            satp,
            key,
            block_index,
            None,
          )
          .expect("native successor rejected a continuation budget");
      }
      if self.stats_enabled {
        self.stats.successor_misses += 1;
      }
      link_source = Some(source_index);
    }

    if let Some(block_index) = self.fast_cache.lookup(key) {
      let cached = self
        .blocks
        .get(block_index)
        .expect("fast-cache block index outlived its arena");
      let block_len = cached.block.instructions.len();
      if block_len <= budget as usize {
        if self.stats_enabled {
          self.stats.cache_hits += 1;
        }
        return self
          .execute_compiled_block(
            frame,
            cpu,
            budget,
            native_only,
            privilege,
            satp,
            key,
            block_index,
            link_source,
          )
          .expect("fast-cache block rejected a sufficient budget");
      }
      if native_only {
        if self.stats_enabled {
          self.stats.cache_hits += 1;
        }
        return ExecuteOne::stop(ExecOutcome::new(0, Ok(())));
      }
    }

    let mut rotate_after_panic = false;
    let outcome = {
      let block_index = match self.cache.entry(key) {
        Entry::Occupied(entry) => {
          if self.stats_enabled {
            self.stats.cache_hits += 1;
          }
          match *entry.get() {
            CacheEntry::Block(block_index) => block_index,
            CacheEntry::Fallback { kind, inst } => {
              if native_only {
                return ExecuteOne::stop(ExecOutcome::new(0, Ok(())));
              }
              if self.stats_enabled {
                self.stats.negative_cache_hits += 1;
              }
              return ExecuteOne::stop(Self::fallback_one(
                &mut self.stats,
                &mut self.system_fallbacks,
                self.stats_enabled,
                cpu,
                kind,
                inst,
              ));
            }
          }
        }
        Entry::Vacant(entry) => {
          if native_only {
            return ExecuteOne::stop(ExecOutcome::new(0, Ok(())));
          }
          if self.stats_enabled {
            self.stats.cache_misses += 1;
          }
          match GuestBlock::translate(cpu, self.max_block_len) {
            BlockBuild::Block(block) => {
              let chainable = block
                .instructions
                .iter()
                .all(|inst| {
                  (inst.len != 4 || inst.raw & 0x7f != 0x2f)
                    && !is_native_csr_read(inst.decoded)
                });
              let block_index = self.blocks.len();
              self.blocks.push(CachedBlock {
                block,
                chainable,
                executions: 0,
                compiled: None,
                compile_failed: false,
                successors: SuccessorCache::EMPTY,
              });
              entry.insert(CacheEntry::Block(block_index));
              block_index
            }
            BlockBuild::InterpretOne { kind, inst } => {
              entry.insert(CacheEntry::Fallback { kind, inst });
              if self.stats_enabled {
                self.stats.negative_blocks += 1;
              }
              return ExecuteOne::stop(Self::fallback_one(
                &mut self.stats,
                &mut self.system_fallbacks,
                self.stats_enabled,
                cpu,
                kind,
                inst,
              ));
            }
            BlockBuild::Fault { raw, exception } => {
              if let Some(raw) = raw {
                cpu.instr = raw as u64;
              }
              return ExecuteOne::stop(ExecOutcome::new(1, Err(exception)));
            }
          }
        }
      };
      let block_len = self.blocks[block_index].block.instructions.len();
      if self.blocks[block_index].compiled.is_some() {
        self.fast_cache.insert(key, block_index);
        if let Some(outcome) = self.execute_compiled_block(
          frame,
          cpu,
          budget,
          native_only,
          privilege,
          satp,
          key,
          block_index,
          link_source,
        ) {
          return outcome;
        }
      }

      if native_only {
        return ExecuteOne::stop(ExecOutcome::new(0, Ok(())));
      }

      if self.stats_enabled {
        self.stats.decoded_executions += 1;
      }
      let cached = &mut self.blocks[block_index];
      let outcome = Self::execute_decoded(cpu, &cached.block, budget);

      cached.executions = cached.executions.saturating_add(1);
      let should_compile = outcome.result.is_ok()
        && outcome.attempted as usize == block_len
        && cached.compiled.is_none()
        && !cached.compile_failed
        && cached.executions >= self.hot_threshold;
      if should_compile {
        let started = self.stats_enabled.then(Instant::now);
        let result = catch_unwind(AssertUnwindSafe(|| {
          self.backend.compile(&cached.block, self.stats_enabled)
        }));
        match result {
          Ok(Ok(compiled)) => {
            cached.compiled = Some(compiled);
            self.fast_cache.insert(key, block_index);
            self.compiled_in_generation += 1;
            self.live_code_bytes = self.live_code_bytes.saturating_add(compiled.code_size);
            if self.stats_enabled {
              self.stats.compiled_blocks += 1;
              self.stats.generated_code_bytes = self
                .stats
                .generated_code_bytes
                .saturating_add(compiled.code_size);
              self.stats.peak_live_code_bytes =
                self.stats.peak_live_code_bytes.max(self.live_code_bytes);
            }
          }
          Ok(Err(_)) => {
            cached.compile_failed = true;
            if self.stats_enabled {
              self.stats.compile_failures += 1;
            }
          }
          Err(_) => {
            // A Cranelift allocator/relocation panic must not terminate the guest. Drop all native
            // pointers at this dispatcher safe point and continue with decoded execution.
            if self.stats_enabled {
              self.stats.compile_failures += 1;
            }
            self.compiled_in_generation = self.max_compiled_blocks;
            rotate_after_panic = true;
          }
        }
        if let Some(started) = started {
          let elapsed = started.elapsed().as_nanos().min(u64::MAX as u128) as u64;
          self.stats.compilation_nanos = self.stats.compilation_nanos.saturating_add(elapsed);
          self.compilation_samples.push(elapsed);
        }
      }

      ExecuteOne::stop(outcome)
    };

    if rotate_after_panic {
      self.rotate_code_cache_if_needed();
    }
    outcome
  }
}

impl RV64Executor for JitExecutor {
  fn execute(&mut self, cpu: &mut RV64Cpu, budget: u32) -> ExecOutcome {
    if self.stats_enabled {
      self.stats.dispatches = self.stats.dispatches.saturating_add(1);
      if let Some(interval) = self.stats_interval {
        if self.stats.dispatches % interval == 0 {
          eprintln!("[valheim-jit] {}", self.diagnostics_string());
        }
      }
    }
    if budget == 0 || cpu.wfi {
      return ExecOutcome::new(0, Ok(()));
    }

    // A chain continuation only enters baseline-native blocks. Those instructions cannot change
    // privilege, SATP/mstatus, translation generations, or the code-cache ownership model, so the
    // dispatcher checks these contexts once before the batch rather than once per TB.
    self.invalidate_changed_epochs(cpu);
    let tlb_context = self.sync_tlb_context(cpu);
    self.rotate_code_cache_if_needed();
    // Native continuations cannot invalidate or replace these CPU/TLB allocations. Reuse their
    // frame pointers for the whole batch and reset only block-local exit state before each call.
    let mut frame = Self::new_native_frame(&mut self.tlb, cpu);

    let mut attempted = 0;
    let mut predecessor = None;
    let result = loop {
      let step = self.execute_one(
        &mut frame,
        cpu,
        budget - attempted,
        attempted != 0,
        tlb_context.privilege,
        tlb_context.satp,
        predecessor,
      );
      attempted += step.outcome.attempted;
      match step.outcome.result {
        Err(exception) => break Err(exception),
        Ok(())
          if step.outcome.attempted == 0
            || step.chain_source.is_none()
            || attempted == budget
            || cpu.wfi =>
        {
          break Ok(())
        }
        Ok(()) => predecessor = step.chain_source,
      }
    };
    let outcome = ExecOutcome::new(attempted, result);
    if self.stats_enabled {
      self.stats.guest_instructions = self
        .stats
        .guest_instructions
        .saturating_add(outcome.attempted as u64);
      if outcome.result.is_ok() {
        self.stats.successful_exits = self.stats.successful_exits.saturating_add(1);
      } else {
        self.stats.exception_exits = self.stats.exception_exits.saturating_add(1);
      }
    }
    outcome
  }

  fn diagnostics(&self) -> Option<String> {
    Some(self.diagnostics_string())
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
  use valheim_core::cpu::bus::{
    RV64_MEMORY_BASE, RV64_MEMORY_END, RV64_MEMORY_SIZE, VIRT_MROM_BASE,
  };
  use valheim_core::cpu::irq::Exception;
  use valheim_core::cpu::mmu::{
    PAGE_SHIFT, PTE_A, PTE_D, PTE_R, PTE_V, PTE_W, PTE_X, SATP64_MODE_SHIFT, VMMode,
  };
  use valheim_core::cpu::{GuestCsrOp, PrivilegeMode};
  use valheim_core::memory::Memory;

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

  fn rewrite_satp_to_invalidate_translation(cpu: &mut RV64Cpu) {
    let satp = cpu.csrs.read_unchecked(SATP);
    assert_eq!(
      cpu.execute_guest_csr(
        CSRAddr(Imm32::from(SATP as u32)),
        GuestCsrOp::Write,
        satp,
      ),
      Ok(satp),
    );
  }

  fn jal_x0(offset: i32) -> u32 {
    RV32Instr::JAL(
      Rd(Reg::ZERO),
      Imm32::<20, 1>::from((offset >> 1) as u32),
    )
    .encode32()
  }

  #[test]
  fn compiled_front_cache_is_two_way_and_generation_guarded() {
    let mut keys = Vec::new();
    let first = TbKey {
      pc: RV64_MEMORY_BASE,
      privilege: PrivilegeMode::Machine as u8,
      satp: 0,
    };
    let wanted_set = FastCache::set_index(first);
    for offset in (0..0x20_000).step_by(2) {
      let key = TbKey {
        pc: RV64_MEMORY_BASE + offset,
        ..first
      };
      if FastCache::set_index(key) == wanted_set {
        keys.push(key);
        if keys.len() == 3 {
          break;
        }
      }
    }
    assert_eq!(keys.len(), 3);

    let mut cache = FastCache::new();
    cache.insert(keys[0], 10);
    cache.insert(keys[1], 11);
    assert_eq!(cache.lookup(keys[0]), Some(10));
    assert_eq!(cache.lookup(keys[1]), Some(11));

    cache.insert(keys[2], 12);
    assert_eq!(cache.lookup(keys[2]), Some(12));
    assert_eq!(
      usize::from(cache.lookup(keys[0]).is_some())
        + usize::from(cache.lookup(keys[1]).is_some()),
      1,
    );

    cache.invalidate();
    assert_eq!(cache.lookup(keys[0]), None);
    assert_eq!(cache.lookup(keys[1]), None);
    assert_eq!(cache.lookup(keys[2]), None);
    cache.insert(keys[0], 99);
    assert_eq!(cache.lookup(keys[0]), Some(99));
  }

  #[test]
  fn successor_cache_is_two_way_and_context_generation_guarded() {
    let key = TbKey {
      pc: RV64_MEMORY_BASE,
      privilege: PrivilegeMode::Machine as u8,
      satp: 0,
    };
    let other_pc = TbKey {
      pc: key.pc + 4,
      ..key
    };
    let other_privilege = TbKey {
      privilege: PrivilegeMode::Supervisor as u8,
      ..key
    };
    let other_satp = TbKey { satp: 1, ..key };
    let mut cache = SuccessorCache::EMPTY;

    cache.insert(7, key, 10);
    cache.insert(7, other_pc, 11);
    assert_eq!(cache.lookup(7, key), Some(10));
    assert_eq!(cache.lookup(7, other_pc), Some(11));
    assert_eq!(cache.lookup(8, key), None);
    assert_eq!(cache.lookup(7, other_privilege), None);
    assert_eq!(cache.lookup(7, other_satp), None);

    cache.insert(7, other_privilege, 12);
    assert_eq!(cache.lookup(7, other_privilege), Some(12));
    assert_eq!(
      usize::from(cache.lookup(7, key).is_some())
        + usize::from(cache.lookup(7, other_pc).is_some()),
      1,
    );
  }

  #[test]
  fn tlb_context_tracks_translation_controls_and_dram_backing() {
    let mut cpu = RV64Cpu::new(None);
    let original = TlbContext::new(&cpu);

    cpu.csrs.write_mstatus_MIE(true);
    assert_eq!(TlbContext::new(&cpu), original);

    cpu.csrs.write_mstatus_SUM(true);
    assert_ne!(TlbContext::new(&cpu), original);

    cpu.csrs.write_mstatus_SUM(false);
    cpu.bus.mem = Memory::new(RV64_MEMORY_BASE, RV64_MEMORY_SIZE as usize).unwrap();
    assert_ne!(TlbContext::new(&cpu), original);
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
  fn short_budget_uses_decoded_prefix_instead_of_oversized_native_block() {
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
  fn hot_native_loop_batches_multiple_basic_blocks_within_the_budget() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let x1 = Reg::X(Fin::new(1));
    cpu.bus.write::<u32>(pc, 0x0010_8093).unwrap(); // addi x1, x1, 1
    cpu
      .bus
      .write::<u32>(pc + VirtAddr(4), 0xffdff06f)
      .unwrap(); // jal x0, -4
    cpu.write_pc(pc);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(jit.execute(&mut cpu, 6), ExecOutcome::new(6, Ok(())));

    assert_eq!(cpu.read_reg(x1), Some(4));
    assert_eq!(cpu.read_pc(), pc);
    assert_eq!(jit.stats().dispatches, 2);
    assert_eq!(jit.stats().native_executions, 3);
    assert_eq!(jit.stats().guest_instructions, 8);
    assert_eq!(jit.stats().successor_misses, 1);
    assert_eq!(jit.stats().successor_installs, 1);
    assert_eq!(jit.stats().successor_hits, 1);
  }

  #[test]
  fn linked_successor_never_exceeds_the_remaining_budget() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let x1 = Reg::X(Fin::new(1));
    cpu.bus.write::<u32>(pc, 0x0010_8093).unwrap(); // addi x1, x1, 1
    cpu
      .bus
      .write::<u32>(pc + VirtAddr(4), 0xffdff06f)
      .unwrap(); // jal x0, -4
    cpu.write_pc(pc);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(jit.execute(&mut cpu, 6), ExecOutcome::new(6, Ok(())));
    let hits_before = jit.stats().successor_hits;
    assert_eq!(jit.execute(&mut cpu, 3), ExecOutcome::new(2, Ok(())));

    assert_eq!(cpu.read_reg(x1), Some(5));
    assert_eq!(cpu.read_pc(), pc);
    assert_eq!(jit.stats().successor_hits, hits_before + 1);
  }

  #[test]
  fn native_batch_stops_before_a_cached_system_fallback() {
    let mut cpu = RV64Cpu::new(None);
    let source = VirtAddr(RV64_MEMORY_BASE);
    let target = source + VirtAddr(0x100);
    let jump = jal_x0((target.0 - source.0) as i32);
    cpu.bus.write::<u32>(source, jump).unwrap();
    cpu.bus.write::<u32>(target, 0x0000_0073).unwrap(); // ecall
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.write_pc(target);
    assert_eq!(
      jit.execute(&mut cpu, 1),
      ExecOutcome::new(1, Err(Exception::MachineEcall))
    );
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));

    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), target);
    assert_eq!(cpu.instr, jump as u64);
    assert_eq!(
      jit.execute(&mut cpu, 32),
      ExecOutcome::new(1, Err(Exception::MachineEcall))
    );
    assert_eq!(jit.stats().successor_installs, 0);
  }

  #[test]
  fn native_batch_stops_before_a_terminal_csr_read() {
    let mut cpu = RV64Cpu::new(None);
    let source = VirtAddr(RV64_MEMORY_BASE);
    let target = source + VirtAddr(0x100);
    let jump = jal_x0((target.0 - source.0) as i32);
    let destination = Reg::X(Fin::new(5));
    let time = CSRAddr(Imm32::from(0xc01));
    let rdtime = RV64Instr::CSRRS(Rd(destination), Rs1(Reg::ZERO), time).encode32();
    cpu.bus.write::<u32>(source, jump).unwrap();
    cpu.bus.write::<u32>(target, rdtime).unwrap();
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.write_pc(target);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));

    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), target);
    assert_eq!(cpu.instr, jump as u64);
    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), target + VirtAddr(4));
    assert_eq!(cpu.instr, rdtime as u64);
    assert_eq!(jit.stats().successor_installs, 0);
  }

  #[test]
  fn native_batch_stops_before_an_atomic_target() {
    let mut cpu = RV64Cpu::new(None);
    let target = VirtAddr(RV64_MEMORY_BASE);
    let source = target + VirtAddr(0x100);
    let data = target + VirtAddr(0x200);
    let x = |index| Reg::X(Fin::new(index));
    let amoadd =
      RV64Instr::AMOADD_D(Rd(x(3)), Rs1(x(1)), Rs2(x(2)), AQ(false), RL(false)).encode32();
    let jump = jal_x0((target.0 as i64 - source.0 as i64) as i32);
    cpu.bus.write::<u32>(target, amoadd).unwrap();
    cpu.bus.write::<u32>(source, jump).unwrap();
    cpu.write_reg(x(1), data.0);
    cpu.write_reg(x(2), 7);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.bus.write::<u64>(data, 5).unwrap();
    cpu.write_pc(target);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));

    cpu.bus.write::<u64>(data, 10).unwrap();
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), target);
    assert_eq!(cpu.bus.read::<u64>(data), Ok(10));

    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.bus.read::<u64>(data), Ok(17));
    assert_eq!(jit.stats().successor_installs, 0);
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
    assert_eq!(cpu.instr, 0x0000_0073);

    // The cached fallback owns the decoded instruction just like a translated TB. Guest code
    // modifications become visible only after the instruction-cache epoch changes.
    cpu.bus.write::<u32>(pc, 0x0010_0073).unwrap(); // ebreak
    assert_eq!(
      jit.execute(&mut cpu, 32),
      ExecOutcome::new(1, Err(Exception::MachineEcall))
    );
    assert_eq!(cpu.instr, 0x0000_0073);
    assert_eq!(jit.stats().negative_blocks, 1);
    assert_eq!(jit.stats().negative_cache_hits, 1);
    assert_eq!(jit.stats().fallback_system, 2);
    assert_eq!(jit.stats().fallback_instructions, 2);

    cpu.icache_epoch = cpu.icache_epoch.wrapping_add(1);
    assert_eq!(
      jit.execute(&mut cpu, 32),
      ExecOutcome::new(1, Err(Exception::Breakpoint))
    );
    assert_eq!(cpu.instr, 0x0010_0073);
    assert_eq!(jit.stats().negative_blocks, 2);
  }

  #[test]
  fn precise_system_exit_restores_attempted_pc_and_raw_instruction() {
    let mut cpu = RV64Cpu::new(None);
    let start = RV64_MEMORY_BASE;
    let first_raw = 0x0010_0093; // addi x1, x0, 1
    let fault_raw = 0xffff_ffff;
    let block = GuestBlock {
      start_pc: start,
      instructions: vec![
        GuestInst {
          pc: start,
          raw: first_raw,
          len: 4,
          decoded: Instr::decode32(first_raw).unwrap(),
        },
        GuestInst {
          pc: start + 4,
          raw: 0x0020_0113,
          len: 4,
          decoded: Instr::decode32(0x0020_0113).unwrap(),
        },
      ],
    };
    let mut tlb = SoftwareTlb::new();
    let mut frame = JitExecutor::new_native_frame(&mut tlb, &mut cpu);
    frame.exit_kind = crate::memory::EXIT_ILLEGAL_INSTRUCTION;
    frame.fault_pc = start + 4;
    frame.raw_instr = fault_raw;
    frame.attempted = 2;

    let exit = JitExecutor::finish_native_exit(&mut cpu, &block, &frame, 2, false);

    assert_eq!(
      exit.outcome,
      ExecOutcome::new(2, Err(Exception::IllegalInstruction))
    );
    assert_eq!(cpu.read_pc(), VirtAddr(start + 4));
    assert_eq!(cpu.instr, fault_raw as u64);
    assert_eq!(exit.pending_index, None);
    assert!(!exit.completed_block);
  }

  #[test]
  fn deferred_memory_exit_commits_only_older_trace_instructions() {
    let mut cpu = RV64Cpu::new(None);
    let start = RV64_MEMORY_BASE;
    let first_raw = 0x0010_8093; // addi x1, x1, 1
    let load_raw = 0x0000_b103; // ld x2, 0(x1)
    let block = GuestBlock {
      start_pc: start,
      instructions: vec![
        GuestInst {
          pc: start,
          raw: first_raw,
          len: 4,
          decoded: Instr::decode32(first_raw).unwrap(),
        },
        GuestInst {
          pc: start + 4,
          raw: load_raw,
          len: 4,
          decoded: Instr::decode32(load_raw).unwrap(),
        },
      ],
    };
    let mut tlb = SoftwareTlb::new();
    let mut frame = JitExecutor::new_native_frame(&mut tlb, &mut cpu);
    frame.exit_kind = crate::memory::EXIT_DEFER_MEMORY;
    frame.fault_pc = start + 4;

    let exit = JitExecutor::finish_native_exit(&mut cpu, &block, &frame, 2, true);

    assert_eq!(exit.outcome, ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), VirtAddr(start + 4));
    assert_eq!(cpu.instr, first_raw as u64);
    assert_eq!(exit.pending_index, None);
    assert!(!exit.completed_block);
  }

  #[test]
  fn system_fallback_profile_distinguishes_csr_reads_and_writes() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    let time = CSRAddr(Imm32::from(0xc01));
    let mscratch = CSRAddr(Imm32::from(0x340));
    let x1 = Reg::X(Fin::new(1));
    let x2 = Reg::X(Fin::new(2));
    cpu
      .bus
      .write::<u32>(
        pc,
        RV64Instr::CSRRS(Rd(x1), Rs1(Reg::ZERO), time).encode32(),
      )
      .unwrap();
    cpu
      .bus
      .write::<u32>(
        pc + VirtAddr(4),
        RV64Instr::CSRRS(Rd(x1), Rs1(x2), mscratch).encode32(),
      )
      .unwrap();
    cpu
      .bus
      .write::<u32>(pc + VirtAddr(8), RV32Instr::ECALL.encode32())
      .unwrap();
    cpu.write_reg(x2, 1);
    cpu.write_pc(pc);
    let mut jit = JitExecutor::new().unwrap();

    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    assert_eq!(
      jit.execute(&mut cpu, 1),
      ExecOutcome::new(1, Err(Exception::MachineEcall)),
    );
    assert_eq!(
      jit.system_fallbacks(),
      vec![
        (SystemFallback::Ecall, 1),
        (SystemFallback::CsrrsWrite(mscratch.value()), 1),
      ],
    );
    assert_eq!(jit.stats().fallback_system, 2);
    assert_eq!(
      SystemFallback::classify(GuestInst {
        pc: pc.0,
        raw: RV64Instr::CSRRS(Rd(x1), Rs1(Reg::ZERO), time).encode32(),
        len: 4,
        decoded: Instr::RV64(RV64Instr::CSRRS(Rd(x1), Rs1(Reg::ZERO), time)),
      }),
      SystemFallback::CsrrsRead(time.value()),
    );
  }

  #[test]
  fn disabled_stats_do_not_collect_system_fallback_details() {
    let mut cpu = RV64Cpu::new(None);
    let pc = VirtAddr(RV64_MEMORY_BASE);
    cpu
      .bus
      .write::<u32>(pc, RV32Instr::ECALL.encode32())
      .unwrap();
    cpu.write_pc(pc);
    let mut jit = JitExecutor::new().unwrap().with_stats_enabled(false);

    assert_eq!(
      jit.execute(&mut cpu, 1),
      ExecOutcome::new(1, Err(Exception::MachineEcall)),
    );
    assert!(jit.system_fallbacks().is_empty());
    assert_eq!(jit.stats().fallback_system, 0);
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
  fn successor_links_cross_retired_and_current_modules_by_arena_index() {
    let mut cpu = RV64Cpu::new(None);
    let source = VirtAddr(RV64_MEMORY_BASE);
    let target = source + VirtAddr(0x100);
    let x1 = Reg::X(Fin::new(1));
    cpu
      .bus
      .write::<u32>(source, jal_x0((target.0 - source.0) as i32))
      .unwrap();
    cpu.bus.write::<u32>(target, 0x0010_8093).unwrap(); // addi x1, x1, 1
    cpu
      .bus
      .write::<u32>(target + VirtAddr(4), 0xffdff06f)
      .unwrap(); // jal x0, -4
    let mut jit = JitExecutor::new()
      .unwrap()
      .with_hot_threshold(1)
      .with_max_compiled_blocks(1);

    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    cpu.write_pc(target);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 3), ExecOutcome::new(3, Ok(())));
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 3), ExecOutcome::new(3, Ok(())));

    assert_eq!(cpu.read_reg(x1), Some(3));
    assert!(jit.stats().module_rotations >= 2);
    assert_eq!(jit.stats().successor_installs, 1);
    assert_eq!(jit.stats().successor_hits, 1);
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
  fn fences_invalidate_compiled_cache_while_satp_writes_keep_keyed_blocks() {
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
    assert_eq!(cpu.sfence_epoch, 1);
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(jit.seen_sfence_epoch, 1);

    cpu.write_reg(x1, 0);
    cpu.write_pc(satp_pc);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.translation_epoch, 2);
    assert_eq!(cpu.sfence_epoch, 1);
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(jit.seen_sfence_epoch, 1);
    assert_eq!(jit.stats().compiled_blocks, 3);
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
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(1, Ok(())));
    cpu.bus.write::<u64>(data, 10).unwrap();
    cpu.write_pc(pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(1, Ok(())));

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
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(1, Ok(())));
    cpu.write_pc(sc_pc);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(1, Ok(())));

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
    rewrite_satp_to_invalidate_translation(&mut cpu);
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
  fn native_tlb_miss_reuses_and_invalidates_the_core_translation_cache() {
    const GUEST_CODE: VirtAddr = VirtAddr(0x1234_4000);
    const GUEST_DATA: VirtAddr = VirtAddr(0x1234_5000);
    const PHYSICAL_CODE: VirtAddr = VirtAddr(RV64_MEMORY_BASE + 0x4000);
    const FIRST_DATA: VirtAddr = VirtAddr(RV64_MEMORY_BASE + 0x5000);
    const SECOND_DATA: VirtAddr = VirtAddr(RV64_MEMORY_BASE + 0x6000);

    let mut cpu = RV64Cpu::new(None);
    cpu
      .bus
      .write::<u32>(PHYSICAL_CODE, 0x0000_b103)
      .unwrap(); // ld x2, 0(x1)
    cpu
      .bus
      .write::<u32>(PHYSICAL_CODE + VirtAddr(4), 0x0000_006f)
      .unwrap(); // jal x0, 0
    cpu.bus.write::<u64>(FIRST_DATA, 0x1111).unwrap();
    cpu.bus.write::<u64>(SECOND_DATA, 0x2222).unwrap();

    let code_flags = (1 << PTE_V) | (1 << PTE_X) | (1 << PTE_A);
    let data_flags = (1 << PTE_V) | (1 << PTE_R) | (1 << PTE_A);
    install_sv39_mapping(&mut cpu, GUEST_CODE, PHYSICAL_CODE, code_flags);
    let data_leaf = install_sv39_mapping(&mut cpu, GUEST_DATA, FIRST_DATA, data_flags);
    enable_sv39(&mut cpu);

    let x1 = Reg::X(Fin::new(1));
    let x2 = Reg::X(Fin::new(2));
    cpu.write_reg(x1, GUEST_DATA.0);
    cpu.write_pc(GUEST_CODE);
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    // Decoded execution fills the core cache while compiling the TB, but not the native L1 TLB.
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(cpu.read_reg(x2), Some(0x1111));
    assert_eq!(jit.stats().compiled_blocks, 1);

    cpu
      .bus
      .write::<u64>(
        data_leaf,
        ((SECOND_DATA.0 >> PAGE_SHIFT) << 10) | data_flags,
      )
      .unwrap();
    jit.tlb.invalidate();
    cpu.write_pc(GUEST_CODE);

    // The forced native L1 miss still observes the cached old PTE while the translation context
    // remains unchanged.
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(cpu.read_reg(x2), Some(0x1111));
    assert_eq!(jit.stats().native_executions, 1);

    // Valheim conservatively invalidates its translation context even for a same-value SATP write.
    rewrite_satp_to_invalidate_translation(&mut cpu);
    cpu.write_pc(GUEST_CODE);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    assert_eq!(cpu.read_reg(x2), Some(0x2222));
    assert_eq!(jit.stats().native_executions, 2);
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

  #[test]
  fn native_batch_defers_a_target_blocks_first_mmio_access() {
    let mut cpu = RV64Cpu::new(None);
    let target = VirtAddr(RV64_MEMORY_BASE);
    let source = target + VirtAddr(0x100);
    let valid = target + VirtAddr(0x200);
    let mmio = VirtAddr(VIRT_MROM_BASE + 0x100);
    let x1 = Reg::X(Fin::new(1));
    let x2 = Reg::X(Fin::new(2));
    let jump = jal_x0((target.0 as i64 - source.0 as i64) as i32);
    cpu.bus.write::<u32>(target, 0x0000_b103).unwrap(); // ld x2, 0(x1)
    cpu
      .bus
      .write::<u32>(target + VirtAddr(4), 0x0000_006f)
      .unwrap();
    cpu.bus.write::<u32>(source, jump).unwrap();
    cpu.bus.write::<u64>(valid, 1).unwrap();
    cpu.bus.write::<u64>(mmio, 0xfeed_face_cafe_beef).unwrap();
    let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

    cpu.write_reg(x1, valid.0);
    cpu.write_pc(target);
    assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));

    // Warm the target's JIT TLB, then take the edge again so source -> target has an installed
    // successor link before the MMIO address forces a fresh first-instruction miss.
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), target);
    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(2, Ok(())));
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(3, Ok(())));
    assert_eq!(jit.stats().successor_installs, 1);

    cpu.write_reg(x1, mmio.0);
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), target);
    assert_eq!(cpu.instr, jump as u64);
    assert_eq!(cpu.read_reg(x2), Some(1));

    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), target + VirtAddr(4));
    assert_eq!(cpu.read_reg(x2), Some(0xfeed_face_cafe_beef));
    assert_eq!(jit.stats().memory_slow_paths, 1);
    assert_eq!(jit.stats().successor_hits, 1);
  }

  #[test]
  fn native_batch_defers_first_tlb_miss_before_sv39_dirty_bit_update() {
    const GUEST_CODE: VirtAddr = VirtAddr(0x1234_4000);
    const GUEST_DATA: VirtAddr = VirtAddr(0x1234_5000);
    const PHYSICAL_CODE: VirtAddr = VirtAddr(RV64_MEMORY_BASE + 0x4000);
    const PHYSICAL_DATA: VirtAddr = VirtAddr(RV64_MEMORY_BASE + 0x5000);

    let mut cpu = RV64Cpu::new(None);
    let source = GUEST_CODE + VirtAddr(0x100);
    let physical_source = PHYSICAL_CODE + VirtAddr(0x100);
    let jump = jal_x0((GUEST_CODE.0 as i64 - source.0 as i64) as i32);
    cpu
      .bus
      .write::<u32>(PHYSICAL_CODE, 0x0020_b023)
      .unwrap(); // sd x2, 0(x1)
    cpu.bus.write::<u32>(physical_source, jump).unwrap();

    let code_flags = (1 << PTE_V) | (1 << PTE_X) | (1 << PTE_A);
    let data_flags =
      (1 << PTE_V) | (1 << PTE_R) | (1 << PTE_W) | (1 << PTE_A) | (1 << PTE_D);
    install_sv39_mapping(&mut cpu, GUEST_CODE, PHYSICAL_CODE, code_flags);
    let data_leaf = install_sv39_mapping(&mut cpu, GUEST_DATA, PHYSICAL_DATA, data_flags);
    enable_sv39(&mut cpu);

    let x1 = Reg::X(Fin::new(1));
    let x2 = Reg::X(Fin::new(2));
    cpu.write_reg(x1, GUEST_DATA.0);
    cpu.write_reg(x2, 0x0123_4567_89ab_cdef);
    let mut jit = JitExecutor::new()
      .unwrap()
      .with_hot_threshold(1)
      .with_max_block_len(1);

    cpu.write_pc(GUEST_CODE);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));
    cpu.write_pc(source);
    assert_eq!(jit.execute(&mut cpu, 1), ExecOutcome::new(1, Ok(())));

    let mmio_pte = ((VIRT_MROM_BASE >> PAGE_SHIFT) << 10)
      | (1 << PTE_V)
      | (1 << PTE_R)
      | (1 << PTE_W)
      | (1 << PTE_A);
    cpu.bus.write::<u64>(data_leaf, mmio_pte).unwrap();
    rewrite_satp_to_invalidate_translation(&mut cpu);
    cpu.write_pc(source);

    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), GUEST_CODE);
    assert_eq!(cpu.instr, jump as u64);
    assert_eq!(cpu.bus.read::<u64>(data_leaf).unwrap() & (1 << PTE_D), 0);

    assert_eq!(jit.execute(&mut cpu, 32), ExecOutcome::new(1, Ok(())));
    assert_eq!(cpu.read_pc(), GUEST_CODE + VirtAddr(4));
    assert_ne!(cpu.bus.read::<u64>(data_leaf).unwrap() & (1 << PTE_D), 0);
    assert_eq!(
      cpu.bus.read::<u64>(VirtAddr(VIRT_MROM_BASE)),
      Ok(0x0123_4567_89ab_cdef)
    );
  }
}
