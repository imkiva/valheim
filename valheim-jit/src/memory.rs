use std::panic::{catch_unwind, AssertUnwindSafe};

use valheim_core::cpu::irq::Exception;
use valheim_core::cpu::mmu::{AccessType, TranslationTarget, PAGE_SHIFT};
use valheim_core::cpu::PrivilegeMode;
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
pub const EXIT_ILLEGAL_INSTRUCTION: u32 = 9;
pub const EXIT_BREAKPOINT: u32 = 10;
pub const EXIT_ECALL: u32 = 11;

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

/// Records a precise native side-exit exception.
///
/// Data-memory faults are also accounted in the shared JIT memory stats. System exceptions must
/// not increment that counter: future fallible system helpers can therefore use the same private
/// numeric encoding without making the native TLB fault statistic include non-memory exits.
pub(crate) unsafe fn record_exception(frame: *mut JitFrame, exception: Exception) {
  let frame = &mut *frame;
  let (kind, address, memory_fault) = match exception {
    Exception::LoadAccessFault(address) => (FAULT_LOAD_ACCESS, address, true),
    Exception::StoreAccessFault(address) => (FAULT_STORE_ACCESS, address, true),
    Exception::LoadAddressMisaligned(address) => (FAULT_LOAD_MISALIGNED, address, true),
    Exception::StoreAddressMisaligned(address) => (FAULT_STORE_MISALIGNED, address, true),
    Exception::LoadPageFault(address) => (FAULT_LOAD_PAGE, address, true),
    Exception::StorePageFault(address) => (FAULT_STORE_PAGE, address, true),
    Exception::IllegalInstruction => (EXIT_ILLEGAL_INSTRUCTION, VirtAddr(0), false),
    Exception::Breakpoint => (EXIT_BREAKPOINT, VirtAddr(0), false),
    Exception::UserEcall | Exception::SupervisorEcall | Exception::MachineEcall => {
      (EXIT_ECALL, VirtAddr(0), false)
    }
    Exception::InstructionAddressMisaligned(_)
    | Exception::InstructionAccessFault(_)
    | Exception::InstructionPageFault(_) => {
      unreachable!("instruction-fetch exception cannot originate in a native helper")
    }
  };
  frame.exit_kind = kind;
  frame.fault_addr = address.0;
  if memory_fault {
    if let Some(stats) = frame.tlb_stats.as_mut() {
      stats.faults = stats.faults.saturating_add(1);
    }
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
    EXIT_ILLEGAL_INSTRUCTION => Some(Exception::IllegalInstruction),
    EXIT_BREAKPOINT => Some(Exception::Breakpoint),
    EXIT_ECALL => {
      let cpu = unsafe { frame.cpu.as_ref() }.expect("native frame has a null CPU pointer");
      Some(match cpu.mode {
        PrivilegeMode::User => Exception::UserEcall,
        PrivilegeMode::Supervisor => Exception::SupervisorEcall,
        PrivilegeMode::Machine => Exception::MachineEcall,
      })
    }
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

#[cfg(test)]
mod tests {
  use super::*;
  use valheim_core::cpu::RV64Cpu;

  fn frame_with_stats(cpu: &mut RV64Cpu, stats: &mut TlbStats) -> JitFrame {
    let xregs = cpu.regs.x.as_mut_ptr();
    JitFrame::new(
      cpu,
      xregs,
      std::ptr::null_mut(),
      std::ptr::null_mut(),
      1,
      stats,
    )
  }

  #[test]
  fn unified_ecall_exit_uses_current_privilege_mode() {
    let mut cpu = RV64Cpu::new(None);
    let mut stats = TlbStats::default();
    let mut frame = frame_with_stats(&mut cpu, &mut stats);
    frame.exit_kind = EXIT_ECALL;

    for (mode, expected) in [
      (PrivilegeMode::User, Exception::UserEcall),
      (PrivilegeMode::Supervisor, Exception::SupervisorEcall),
      (PrivilegeMode::Machine, Exception::MachineEcall),
    ] {
      cpu.mode = mode;
      assert_eq!(exception_from_frame(&frame), Some(expected));
    }
  }

  #[test]
  fn system_exit_kinds_restore_exact_exceptions() {
    let mut cpu = RV64Cpu::new(None);
    let mut stats = TlbStats::default();
    let mut frame = frame_with_stats(&mut cpu, &mut stats);

    frame.exit_kind = EXIT_ILLEGAL_INSTRUCTION;
    assert_eq!(
      exception_from_frame(&frame),
      Some(Exception::IllegalInstruction)
    );
    frame.exit_kind = EXIT_BREAKPOINT;
    assert_eq!(exception_from_frame(&frame), Some(Exception::Breakpoint));
  }

  #[test]
  fn system_exceptions_do_not_increment_memory_fault_stats() {
    let mut cpu = RV64Cpu::new(None);
    let mut stats = TlbStats::default();
    let mut frame = frame_with_stats(&mut cpu, &mut stats);

    unsafe {
      record_exception(&mut frame, Exception::IllegalInstruction);
    }
    assert_eq!(frame.exit_kind, EXIT_ILLEGAL_INSTRUCTION);
    assert_eq!(stats.faults, 0);

    unsafe {
      record_exception(&mut frame, Exception::MachineEcall);
    }
    assert_eq!(frame.exit_kind, EXIT_ECALL);
    assert_eq!(stats.faults, 0);

    let address = VirtAddr(0x1234);
    unsafe {
      record_exception(&mut frame, Exception::LoadPageFault(address));
    }
    assert_eq!(frame.exit_kind, FAULT_LOAD_PAGE);
    assert_eq!(frame.fault_addr, address.0);
    assert_eq!(stats.faults, 1);
  }
}
