//! Slow-path helper for native CSR instructions.
//!
//! The helper deliberately delegates every architectural rule to
//! [`RV64Cpu::execute_guest_csr`]. This keeps live counters, CSR aliases, WARL behavior, and
//! address-translation generation changes identical to the interpreter.

// The helper is intentionally staged before its Cranelift lowering is connected.
#![allow(dead_code)]

use std::panic::{catch_unwind, AssertUnwindSafe};

use valheim_asm::isa::rv64::CSRAddr;
use valheim_asm::isa::typed::Imm32;
use valheim_core::cpu::irq::Exception;
use valheim_core::cpu::GuestCsrOp;

use crate::cranelift::JitFrame;
use crate::memory::{record_exception, FAULT_NONE};

/// Operation selector passed to [`jit_csr`].
///
/// These discriminants form part of the private JIT helper ABI and may be embedded as CLIF
/// constants. The explicit conversion keeps that ABI independent from Rust's representation of
/// [`GuestCsrOp`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub(crate) enum CsrOp {
  Read = 0,
  Write = 1,
  Set = 2,
  Clear = 3,
}

impl CsrOp {
  fn from_abi(value: u32) -> Option<Self> {
    Some(match value {
      0 => Self::Read,
      1 => Self::Write,
      2 => Self::Set,
      3 => Self::Clear,
      _ => return None,
    })
  }

  fn guest_operation(self) -> GuestCsrOp {
    match self {
      Self::Read => GuestCsrOp::Read,
      Self::Write => GuestCsrOp::Write,
      Self::Set => GuestCsrOp::Set,
      Self::Clear => GuestCsrOp::Clear,
    }
  }
}

fn csr_addr_from_abi(value: u32) -> Result<CSRAddr, Exception> {
  if value < (1 << 12) {
    Ok(CSRAddr(Imm32::from(value)))
  } else {
    // Constructing CSRAddr directly would silently mask high bits during `value()`. Reject those
    // values so a corrupt generated-code call cannot alias a different architectural CSR.
    Err(Exception::IllegalInstruction)
  }
}

/// Executes one architectural CSR operation through the shared CPU path.
///
/// ABI contract:
///
/// - Linux x86_64 `extern "C"` is the System V ABI used by the Cranelift module.
/// - `csr` must be a complete 12-bit CSR address and `operation` a [`CsrOp`] discriminant.
/// - The return value is the CSR value observed before any write.
/// - On an exception, `frame.exit_kind` is populated and generated code must not commit the
///   return value to `rd`.
/// - The caller fills `fault_pc`, `raw_instr`, and `attempted` before the call, then branches to
///   the common precise-fault side exit when `exit_kind != 0`.
///
/// All Rust panics are converted into illegal-instruction exits so unwinding never crosses the
/// native JIT frame.
pub(crate) unsafe extern "C" fn jit_csr(
  frame: *mut JitFrame,
  csr: u32,
  operand: u64,
  operation: u32,
) -> u64 {
  if frame.is_null() {
    return 0;
  }

  let result = catch_unwind(AssertUnwindSafe(|| {
    (*frame).exit_kind = FAULT_NONE;
    (*frame).fault_addr = 0;

    let csr = csr_addr_from_abi(csr)?;
    let operation =
      CsrOp::from_abi(operation).ok_or(Exception::IllegalInstruction)?.guest_operation();
    let cpu = (*frame).cpu;
    if cpu.is_null() {
      return Err(Exception::IllegalInstruction);
    }

    (&mut *cpu).execute_guest_csr(csr, operation, operand)
  }));

  match result {
    Ok(Ok(old)) => old,
    Ok(Err(exception)) => {
      record_exception(frame, exception);
      0
    }
    Err(_) => {
      record_exception(frame, Exception::IllegalInstruction);
      0
    }
  }
}

#[cfg(test)]
mod tests {
  use std::sync::atomic::{AtomicU64, Ordering};
  use std::sync::Arc;
  use std::time::Duration;

  use valheim_core::cpu::csr::CSRMap::{MCYCLE, MSCRATCH, SATP, TIME};
  use valheim_core::cpu::RV64Cpu;
  use valheim_core::device::clint::{ClockSource, TIMEBASE_FREQUENCY};

  use super::*;
  use crate::memory::{exception_from_frame, SoftwareTlb};

  #[derive(Default)]
  struct ManualClock {
    nanos: AtomicU64,
  }

  impl ManualClock {
    fn set_ticks(&self, ticks: u64) {
      let nanos_per_tick = 1_000_000_000 / TIMEBASE_FREQUENCY;
      self
        .nanos
        .store(ticks.saturating_mul(nanos_per_tick), Ordering::Relaxed);
    }
  }

  impl ClockSource for ManualClock {
    fn now(&self) -> Duration {
      Duration::from_nanos(self.nanos.load(Ordering::Relaxed))
    }
  }

  #[derive(Default)]
  struct PanicAfterInitializationClock {
    calls: AtomicU64,
  }

  impl ClockSource for PanicAfterInitializationClock {
    fn now(&self) -> Duration {
      if self.calls.fetch_add(1, Ordering::Relaxed) == 0 {
        Duration::ZERO
      } else {
        panic!("test clock panic")
      }
    }
  }

  fn invoke_with_tlb(
    cpu: &mut RV64Cpu,
    tlb: &mut SoftwareTlb,
    csr: u32,
    operand: u64,
    operation: u32,
  ) -> (u64, JitFrame) {
    let xregs = cpu.regs.x.as_mut_ptr();
    let load_tlb = tlb.load_ptr();
    let store_tlb = tlb.store_ptr();
    let generation = tlb.generation();
    let stats = tlb.stats_ptr();
    let mut frame = JitFrame::new(cpu, xregs, load_tlb, store_tlb, generation, stats);
    frame.fault_pc = 0x8000_1000;
    frame.raw_instr = 0xfeed_beef;
    frame.attempted = 1;
    let result = unsafe { jit_csr(&mut frame, csr, operand, operation) };
    (result, frame)
  }

  fn invoke(
    cpu: &mut RV64Cpu,
    csr: u16,
    operand: u64,
    operation: CsrOp,
  ) -> (u64, JitFrame) {
    let mut tlb = SoftwareTlb::new();
    invoke_with_tlb(cpu, &mut tlb, csr as u32, operand, operation as u32)
  }

  fn exception(frame: &JitFrame) -> Option<Exception> {
    exception_from_frame(frame)
  }

  #[test]
  fn read_write_set_and_clear_return_old_values_and_commit_shared_semantics() {
    let mut cpu = RV64Cpu::new(None);

    let (old, frame) = invoke(&mut cpu, MSCRATCH, u64::MAX, CsrOp::Read);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 0);

    let (old, frame) = invoke(&mut cpu, MSCRATCH, 0x30, CsrOp::Write);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 0);

    let (old, frame) = invoke(&mut cpu, MSCRATCH, 0x05, CsrOp::Set);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 0x30);

    let (old, frame) = invoke(&mut cpu, MSCRATCH, 0x11, CsrOp::Clear);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 0x35);
    assert_eq!(cpu.csrs.read_unchecked(MSCRATCH), 0x24);
  }

  #[test]
  fn time_reads_use_the_live_clock() {
    let clock = Arc::new(ManualClock::default());
    let mut cpu = RV64Cpu::new_with_clock(None, clock.clone());

    clock.set_ticks(42);
    let (old, frame) = invoke(&mut cpu, TIME, 0, CsrOp::Read);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 42);

    clock.set_ticks(99);
    let (old, frame) = invoke(&mut cpu, TIME, 0, CsrOp::Read);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 99);
  }

  #[test]
  fn read_only_writes_exit_as_illegal_without_counting_memory_faults() {
    let clock = Arc::new(ManualClock::default());
    let mut cpu = RV64Cpu::new_with_clock(None, clock.clone());
    let mut tlb = SoftwareTlb::new();
    clock.set_ticks(77);

    let (result, frame) =
      invoke_with_tlb(&mut cpu, &mut tlb, TIME as u32, 1, CsrOp::Write as u32);
    assert_eq!(result, 0);
    assert_eq!(exception(&frame), Some(Exception::IllegalInstruction));

    let (result, frame) =
      invoke_with_tlb(&mut cpu, &mut tlb, MCYCLE as u32, 0, CsrOp::Set as u32);
    assert_eq!(result, 0);
    assert_eq!(exception(&frame), Some(Exception::IllegalInstruction));
    assert_eq!(cpu.bus.clint.mtime(), 77);
    assert_eq!(cpu.csrs.read_unchecked(MCYCLE), 0);
    assert_eq!(tlb.stats().faults, 0);
  }

  #[test]
  fn satp_write_operations_bump_translation_epoch_but_reads_do_not() {
    let mut cpu = RV64Cpu::new(None);

    let (_, frame) = invoke(&mut cpu, SATP, 0, CsrOp::Read);
    assert_eq!(exception(&frame), None);
    assert_eq!(cpu.translation_epoch, 0);

    let (old, frame) = invoke(&mut cpu, SATP, 1, CsrOp::Write);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 0);
    assert_eq!(cpu.translation_epoch, 1);

    let (old, frame) = invoke(&mut cpu, SATP, 2, CsrOp::Set);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 1);
    assert_eq!(cpu.translation_epoch, 2);

    let (old, frame) = invoke(&mut cpu, SATP, 1, CsrOp::Clear);
    assert_eq!(exception(&frame), None);
    assert_eq!(old, 3);
    assert_eq!(cpu.translation_epoch, 3);
    assert_eq!(cpu.vmppn, 2 << 12);
  }

  #[test]
  fn malformed_calls_and_panics_never_unwind_across_the_helper() {
    let mut cpu = RV64Cpu::new(None);
    let mut tlb = SoftwareTlb::new();

    let (_, frame) = invoke_with_tlb(
      &mut cpu,
      &mut tlb,
      MSCRATCH as u32,
      0,
      u32::MAX,
    );
    assert_eq!(exception(&frame), Some(Exception::IllegalInstruction));

    let (_, frame) = invoke_with_tlb(
      &mut cpu,
      &mut tlb,
      1 << 12,
      0,
      CsrOp::Read as u32,
    );
    assert_eq!(exception(&frame), Some(Exception::IllegalInstruction));

    let xregs = cpu.regs.x.as_mut_ptr();
    let mut frame = JitFrame::new(
      &mut cpu,
      xregs,
      tlb.load_ptr(),
      tlb.store_ptr(),
      tlb.generation(),
      tlb.stats_ptr(),
    );
    frame.cpu = std::ptr::null_mut();
    let result = unsafe { jit_csr(&mut frame, MSCRATCH as u32, 0, CsrOp::Read as u32) };
    assert_eq!(result, 0);
    assert_eq!(exception(&frame), Some(Exception::IllegalInstruction));

    let result = unsafe {
      jit_csr(
        std::ptr::null_mut(),
        MSCRATCH as u32,
        0,
        CsrOp::Read as u32,
      )
    };
    assert_eq!(result, 0);
    assert_eq!(tlb.stats().faults, 0);

    let mut panic_cpu =
      RV64Cpu::new_with_clock(None, Arc::new(PanicAfterInitializationClock::default()));
    let (_, frame) = invoke(&mut panic_cpu, TIME, 0, CsrOp::Read);
    assert_eq!(exception(&frame), Some(Exception::IllegalInstruction));
  }
}
