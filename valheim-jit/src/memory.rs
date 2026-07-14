use std::panic::{catch_unwind, AssertUnwindSafe};

use valheim_core::cpu::irq::Exception;
use valheim_core::cpu::mmu::{AccessType, TranslationTarget, PAGE_SHIFT};
use valheim_core::memory::VirtAddr;

use crate::cranelift::JitFrame;

pub(crate) const FAULT_NONE: u32 = 0;
const FAULT_LOAD_ACCESS: u32 = 1;
const FAULT_STORE_ACCESS: u32 = 2;
const FAULT_LOAD_MISALIGNED: u32 = 3;
const FAULT_STORE_MISALIGNED: u32 = 4;
const FAULT_LOAD_PAGE: u32 = 5;
const FAULT_STORE_PAGE: u32 = 6;
pub const EXIT_SLOW_MEMORY: u32 = 7;
pub const EXIT_DEFER_MEMORY: u32 = 8;

pub const TLB_ACCESS_READ: u32 = 0;
pub const TLB_ACCESS_WRITE: u32 = 1;
pub const TLB_ENTRY_COUNT: usize = 256;
pub const TLB_INDEX_MASK: u64 = TLB_ENTRY_COUNT as u64 - 1;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct TlbEntry {
  pub tag: u64,
  pub host_page: *mut u8,
  pub generation: u64,
}

impl TlbEntry {
  const EMPTY: Self = Self {
    tag: 0,
    host_page: std::ptr::null_mut(),
    generation: 0,
  };
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct TlbStats {
  pub hits: u64,
  pub misses: u64,
  pub slow_paths: u64,
  pub faults: u64,
}

pub struct SoftwareTlb {
  load: Box<[TlbEntry; TLB_ENTRY_COUNT]>,
  store: Box<[TlbEntry; TLB_ENTRY_COUNT]>,
  generation: u64,
  stats_enabled: bool,
  stats: TlbStats,
  disabled_stats: TlbStats,
}

impl SoftwareTlb {
  pub fn new() -> Self {
    Self {
      load: Box::new([TlbEntry::EMPTY; TLB_ENTRY_COUNT]),
      store: Box::new([TlbEntry::EMPTY; TLB_ENTRY_COUNT]),
      generation: 1,
      stats_enabled: true,
      stats: TlbStats::default(),
      disabled_stats: TlbStats::default(),
    }
  }

  pub fn invalidate(&mut self) {
    self.generation = self.generation.wrapping_add(1);
    if self.generation == 0 {
      self.load.fill(TlbEntry::EMPTY);
      self.store.fill(TlbEntry::EMPTY);
      self.generation = 1;
    }
  }

  pub fn load_ptr(&mut self) -> *mut TlbEntry {
    self.load.as_mut_ptr()
  }

  pub fn store_ptr(&mut self) -> *mut TlbEntry {
    self.store.as_mut_ptr()
  }

  pub fn generation(&self) -> u64 {
    self.generation
  }

  pub fn stats_ptr(&mut self) -> *mut TlbStats {
    if self.stats_enabled {
      &mut self.stats
    } else {
      // Keep a valid sink for native blocks compiled while statistics were enabled. The public
      // builder can be applied to an executor that already owns native code, so returning null
      // here would make an older block's generated counter updates unsafe.
      &mut self.disabled_stats
    }
  }

  pub fn set_stats_enabled(&mut self, enabled: bool) {
    self.stats_enabled = enabled;
  }

  pub fn stats(&self) -> TlbStats {
    self.stats
  }
}

impl Default for SoftwareTlb {
  fn default() -> Self {
    Self::new()
  }
}

pub const WIDTH_8: u32 = 0;
pub const WIDTH_16: u32 = 1;
pub const WIDTH_32: u32 = 2;
pub const WIDTH_64: u32 = 3;

/// Records a precise native side-exit exception and accounts it in the shared JIT memory stats.
///
/// All fallible helpers use this function so the private numeric encoding cannot drift between
/// ordinary memory operations and A-extension operations.
pub(crate) unsafe fn record_exception(frame: *mut JitFrame, exception: Exception) {
  let frame = &mut *frame;
  let (kind, address) = match exception {
    Exception::LoadAccessFault(address) => (FAULT_LOAD_ACCESS, address),
    Exception::StoreAccessFault(address) => (FAULT_STORE_ACCESS, address),
    Exception::LoadAddressMisaligned(address) => (FAULT_LOAD_MISALIGNED, address),
    Exception::StoreAddressMisaligned(address) => (FAULT_STORE_MISALIGNED, address),
    Exception::LoadPageFault(address) => (FAULT_LOAD_PAGE, address),
    Exception::StorePageFault(address) => (FAULT_STORE_PAGE, address),
    _ => (FAULT_LOAD_ACCESS, VirtAddr(frame.fault_pc)),
  };
  frame.exit_kind = kind;
  frame.fault_addr = address.0;
  if let Some(stats) = frame.tlb_stats.as_mut() {
    stats.faults = stats.faults.saturating_add(1);
  }
}

pub unsafe extern "C" fn jit_tlb_fill(frame: *mut JitFrame, address: u64, access: u32) -> u64 {
  let result = catch_unwind(AssertUnwindSafe(|| {
    let access_type = match access {
      TLB_ACCESS_READ => AccessType::Read,
      TLB_ACCESS_WRITE => AccessType::Write,
      _ => return Err(Exception::LoadAccessFault(VirtAddr(address))),
    };
    let frame = &mut *frame;
    if let Some(stats) = frame.tlb_stats.as_mut() {
      stats.misses = stats.misses.saturating_add(1);
    }
    let target = (&mut *frame.cpu).translate_to_host(VirtAddr(address), access_type)?;
    match target {
      TranslationTarget::Dram { host_page, .. } => {
        let tag = address >> PAGE_SHIFT;
        let index = (tag & TLB_INDEX_MASK) as usize;
        let entries = match access {
          TLB_ACCESS_READ => frame.load_tlb,
          TLB_ACCESS_WRITE => frame.store_tlb,
          _ => unreachable!(),
        };
        *entries.add(index) = TlbEntry {
          tag,
          host_page,
          generation: frame.tlb_generation,
        };
        Ok(host_page as u64)
      }
      TranslationTarget::Mmio { .. } => {
        frame.exit_kind = EXIT_SLOW_MEMORY;
        frame.fault_addr = address;
        if let Some(stats) = frame.tlb_stats.as_mut() {
          stats.slow_paths = stats.slow_paths.saturating_add(1);
        }
        Ok(0)
      }
    }
  }));

  match result {
    Ok(Ok(host_page)) => host_page,
    Ok(Err(exception)) => {
      record_exception(frame, exception);
      0
    }
    Err(_) => {
      let exception = match access {
        TLB_ACCESS_WRITE => Exception::StoreAccessFault(VirtAddr(address)),
        _ => Exception::LoadAccessFault(VirtAddr(address)),
      };
      record_exception(frame, exception);
      0
    }
  }
}

pub fn exception_from_frame(frame: &JitFrame) -> Option<Exception> {
  let address = VirtAddr(frame.fault_addr);
  match frame.exit_kind {
    FAULT_NONE => None,
    FAULT_LOAD_ACCESS => Some(Exception::LoadAccessFault(address)),
    FAULT_STORE_ACCESS => Some(Exception::StoreAccessFault(address)),
    FAULT_LOAD_MISALIGNED => Some(Exception::LoadAddressMisaligned(address)),
    FAULT_STORE_MISALIGNED => Some(Exception::StoreAddressMisaligned(address)),
    FAULT_LOAD_PAGE => Some(Exception::LoadPageFault(address)),
    FAULT_STORE_PAGE => Some(Exception::StorePageFault(address)),
    EXIT_SLOW_MEMORY | EXIT_DEFER_MEMORY => None,
    _ => Some(Exception::LoadAccessFault(address)),
  }
}

pub fn is_slow_memory_exit(frame: &JitFrame) -> bool {
  frame.exit_kind == EXIT_SLOW_MEMORY
}

pub fn is_deferred_memory_exit(frame: &JitFrame) -> bool {
  frame.exit_kind == EXIT_DEFER_MEMORY
}
