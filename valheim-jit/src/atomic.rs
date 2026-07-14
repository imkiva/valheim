//! Slow-path helpers for the RISC-V A extension.
//!
//! Valheim currently models a single hart, so the interpreter's A-extension semantics are a
//! translated read followed by a translated write rather than a host-atomic operation. Keeping
//! that sequence here is intentional: it preserves MMIO side effects, Sv39 permission checks,
//! PTE A/D updates, and the interpreter's virtual-address reservation representation.

#[cfg(not(all(target_arch = "x86_64", target_os = "linux")))]
compile_error!("valheim-jit atomic helpers require Linux x86_64 (System V ABI)");

use std::panic::{catch_unwind, AssertUnwindSafe};

use valheim_core::cpu::irq::Exception;
use valheim_core::cpu::RV64Cpu;
use valheim_core::memory::VirtAddr;

use crate::cranelift::JitFrame;
use crate::memory::{record_exception, FAULT_NONE};

/// Operation selector passed to [`jit_atomic`].
///
/// `aq` and `rl` are absent because the current single-hart interpreter intentionally ignores
/// them. The discriminants form part of the private JIT helper ABI and may be embedded as CLIF
/// constants.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum AtomicOp {
  LrW = 0,
  ScW = 1,
  SwapW = 2,
  AddW = 3,
  XorW = 4,
  AndW = 5,
  OrW = 6,
  MinW = 7,
  MaxW = 8,
  MinuW = 9,
  MaxuW = 10,

  LrD = 16,
  ScD = 17,
  SwapD = 18,
  AddD = 19,
  XorD = 20,
  AndD = 21,
  OrD = 22,
  MinD = 23,
  MaxD = 24,
  MinuD = 25,
  MaxuD = 26,
}

impl AtomicOp {
  fn from_abi(value: u32) -> Option<Self> {
    Some(match value {
      0 => Self::LrW,
      1 => Self::ScW,
      2 => Self::SwapW,
      3 => Self::AddW,
      4 => Self::XorW,
      5 => Self::AndW,
      6 => Self::OrW,
      7 => Self::MinW,
      8 => Self::MaxW,
      9 => Self::MinuW,
      10 => Self::MaxuW,
      16 => Self::LrD,
      17 => Self::ScD,
      18 => Self::SwapD,
      19 => Self::AddD,
      20 => Self::XorD,
      21 => Self::AndD,
      22 => Self::OrD,
      23 => Self::MinD,
      24 => Self::MaxD,
      25 => Self::MinuD,
      26 => Self::MaxuD,
      _ => return None,
    })
  }

  fn width(self) -> u64 {
    if (self as u32) < Self::LrD as u32 {
      4
    } else {
      8
    }
  }

  fn is_sc(self) -> bool {
    matches!(self, Self::ScW | Self::ScD)
  }
}

#[derive(Clone, Copy)]
enum PanicAccess {
  Load,
  Store,
}

fn sign_extend_word(value: u32) -> u64 {
  value as i32 as i64 as u64
}

fn check_alignment(address: VirtAddr, width: u64) -> Result<(), Exception> {
  if address.0 % width == 0 {
    Ok(())
  } else {
    // This intentionally matches the existing `amo_addr!` macro, including its use of a load
    // misalignment exception for SC and every AMO instruction.
    Err(Exception::LoadAddressMisaligned(address))
  }
}

unsafe fn execute_word(
  cpu: &mut RV64Cpu,
  address: VirtAddr,
  operand: u64,
  operation: AtomicOp,
  panic_access: &mut PanicAccess,
) -> Result<u64, Exception> {
  match operation {
    AtomicOp::LrW => {
      let old = cpu.read_mem::<u32>(address)?;
      cpu.reserved.push(address);
      Ok(sign_extend_word(old))
    }
    AtomicOp::ScW => {
      *panic_access = PanicAccess::Store;
      if cpu.reserved.contains(&address) {
        // Match the interpreter: only reservations for this exact virtual address are removed,
        // and they are removed before a potentially faulting store.
        cpu.reserved.retain(|reserved| *reserved != address);
        cpu.write_mem::<u32>(address, operand as u32)?;
        Ok(0)
      } else {
        cpu.reserved.retain(|reserved| *reserved != address);
        Ok(1)
      }
    }
    operation => {
      let old = cpu.read_mem::<u32>(address)?;
      *panic_access = PanicAccess::Store;
      let rhs = operand as u32;
      let result = match operation {
        AtomicOp::SwapW => rhs,
        AtomicOp::AddW => old.wrapping_add(rhs),
        AtomicOp::XorW => old ^ rhs,
        AtomicOp::AndW => old & rhs,
        AtomicOp::OrW => old | rhs,
        AtomicOp::MinW => (old as i32).min(rhs as i32) as u32,
        AtomicOp::MaxW => (old as i32).max(rhs as i32) as u32,
        AtomicOp::MinuW => old.min(rhs),
        AtomicOp::MaxuW => old.max(rhs),
        _ => return Err(Exception::LoadAccessFault(address)),
      };
      cpu.write_mem::<u32>(address, result)?;
      Ok(sign_extend_word(old))
    }
  }
}

unsafe fn execute_double(
  cpu: &mut RV64Cpu,
  address: VirtAddr,
  operand: u64,
  operation: AtomicOp,
  panic_access: &mut PanicAccess,
) -> Result<u64, Exception> {
  match operation {
    AtomicOp::LrD => {
      let old = cpu.read_mem::<u64>(address)?;
      cpu.reserved.push(address);
      Ok(old)
    }
    AtomicOp::ScD => {
      *panic_access = PanicAccess::Store;
      if cpu.reserved.contains(&address) {
        cpu.reserved.retain(|reserved| *reserved != address);
        cpu.write_mem::<u64>(address, operand)?;
        Ok(0)
      } else {
        cpu.reserved.retain(|reserved| *reserved != address);
        Ok(1)
      }
    }
    operation => {
      let old = cpu.read_mem::<u64>(address)?;
      *panic_access = PanicAccess::Store;
      let result = match operation {
        AtomicOp::SwapD => operand,
        AtomicOp::AddD => old.wrapping_add(operand),
        AtomicOp::XorD => old ^ operand,
        AtomicOp::AndD => old & operand,
        AtomicOp::OrD => old | operand,
        AtomicOp::MinD => (old as i64).min(operand as i64) as u64,
        AtomicOp::MaxD => (old as i64).max(operand as i64) as u64,
        AtomicOp::MinuD => old.min(operand),
        AtomicOp::MaxuD => old.max(operand),
        _ => return Err(Exception::LoadAccessFault(address)),
      };
      cpu.write_mem::<u64>(address, result)?;
      Ok(old)
    }
  }
}

/// Executes one RV64 A-extension instruction through the shared CPU memory path.
///
/// ABI contract:
///
/// - Linux x86_64 `extern "C"` is the System V ABI used by the Cranelift module.
/// - `operation` is an [`AtomicOp`] discriminant; `operand` is ignored by LR.
/// - The return value is the architectural value for `rd` (including W sign extension).
/// - On an exception, `frame.exit_kind`/`fault_addr` are populated and the return value must not
///   be committed to `rd` by generated code.
/// - The caller fills `fault_pc`, `raw_instr`, and `attempted` before the call, then branches to the
///   common precise-fault side exit when `exit_kind != 0`.
///
/// The helper catches all Rust panics so unwinding never crosses the native JIT frame.
pub unsafe extern "C" fn jit_atomic(
  frame: *mut JitFrame,
  address: u64,
  operand: u64,
  operation: u32,
) -> u64 {
  if frame.is_null() {
    return 0;
  }

  let address = VirtAddr(address);
  let mut panic_access = PanicAccess::Load;
  let result = catch_unwind(AssertUnwindSafe(|| {
    (*frame).exit_kind = FAULT_NONE;
    (*frame).fault_addr = 0;

    let operation = AtomicOp::from_abi(operation).ok_or(Exception::LoadAccessFault(address))?;
    check_alignment(address, operation.width())?;
    if operation.is_sc() {
      panic_access = PanicAccess::Store;
    }

    let cpu = (*frame).cpu;
    if cpu.is_null() {
      return Err(match panic_access {
        PanicAccess::Load => Exception::LoadAccessFault(address),
        PanicAccess::Store => Exception::StoreAccessFault(address),
      });
    }
    let cpu = &mut *cpu;
    match operation.width() {
      4 => execute_word(cpu, address, operand, operation, &mut panic_access),
      8 => execute_double(cpu, address, operand, operation, &mut panic_access),
      _ => unreachable!(),
    }
  }));

  match result {
    Ok(Ok(value)) => value,
    Ok(Err(exception)) => {
      record_exception(frame, exception);
      0
    }
    Err(_) => {
      let exception = match panic_access {
        PanicAccess::Load => Exception::LoadAccessFault(address),
        PanicAccess::Store => Exception::StoreAccessFault(address),
      };
      record_exception(frame, exception);
      0
    }
  }
}

#[cfg(test)]
mod tests {
  use std::sync::Arc;

  use valheim_core::cpu::bus::RV64_MEMORY_BASE;
  use valheim_core::device::Device;
  use valheim_core::memory::Memory;

  use super::*;
  use crate::memory::SoftwareTlb;

  const ADDR: u64 = RV64_MEMORY_BASE + 0x1000;

  fn invoke_with_tlb(
    cpu: &mut RV64Cpu,
    tlb: &mut SoftwareTlb,
    address: u64,
    operand: u64,
    operation: AtomicOp,
  ) -> (u64, JitFrame) {
    let xregs = cpu.regs.x.as_mut_ptr();
    let load_tlb = tlb.load_ptr();
    let store_tlb = tlb.store_ptr();
    let generation = tlb.generation();
    let stats = tlb.stats_ptr();
    let mut frame = JitFrame::new(cpu, xregs, load_tlb, store_tlb, generation, stats);
    frame.fault_pc = RV64_MEMORY_BASE;
    frame.raw_instr = 0xfeed_beef;
    frame.attempted = 1;
    let result = unsafe { jit_atomic(&mut frame, address, operand, operation as u32) };
    (result, frame)
  }

  fn invoke(cpu: &mut RV64Cpu, address: u64, operand: u64, operation: AtomicOp) -> (u64, JitFrame) {
    let mut tlb = SoftwareTlb::new();
    invoke_with_tlb(cpu, &mut tlb, address, operand, operation)
  }

  fn exception(frame: &JitFrame) -> Option<Exception> {
    crate::memory::exception_from_frame(frame)
  }

  #[test]
  fn word_amos_return_sign_extended_old_value_and_store_expected_result() {
    let mut cpu = RV64Cpu::new(None);
    let old = 0x8000_0005_u32;
    let rhs = 7_u64;
    let expected_old = 0xffff_ffff_8000_0005;
    let cases = [
      (AtomicOp::SwapW, 7),
      (AtomicOp::AddW, 0x8000_000c),
      (AtomicOp::XorW, 0x8000_0002),
      (AtomicOp::AndW, 5),
      (AtomicOp::OrW, 0x8000_0007),
      (AtomicOp::MinW, old),
      (AtomicOp::MaxW, 7),
      (AtomicOp::MinuW, 7),
      (AtomicOp::MaxuW, old),
    ];

    for (operation, expected_memory) in cases {
      cpu.bus.write::<u32>(VirtAddr(ADDR), old).unwrap();
      let (result, frame) = invoke(&mut cpu, ADDR, rhs, operation);
      assert_eq!(exception(&frame), None, "{operation:?}");
      assert_eq!(result, expected_old, "{operation:?}");
      assert_eq!(
        cpu.bus.read::<u32>(VirtAddr(ADDR)),
        Ok(expected_memory),
        "{operation:?}"
      );
    }
  }

  #[test]
  fn double_amos_cover_signed_unsigned_and_wrapping_operations() {
    let mut cpu = RV64Cpu::new(None);
    let old = 0x8000_0000_0000_0005_u64;
    let rhs = 7_u64;
    let cases = [
      (AtomicOp::SwapD, 7),
      (AtomicOp::AddD, 0x8000_0000_0000_000c),
      (AtomicOp::XorD, 0x8000_0000_0000_0002),
      (AtomicOp::AndD, 5),
      (AtomicOp::OrD, 0x8000_0000_0000_0007),
      (AtomicOp::MinD, old),
      (AtomicOp::MaxD, 7),
      (AtomicOp::MinuD, 7),
      (AtomicOp::MaxuD, old),
    ];

    for (operation, expected_memory) in cases {
      cpu.bus.write::<u64>(VirtAddr(ADDR), old).unwrap();
      let (result, frame) = invoke(&mut cpu, ADDR, rhs, operation);
      assert_eq!(exception(&frame), None, "{operation:?}");
      assert_eq!(result, old, "{operation:?}");
      assert_eq!(
        cpu.bus.read::<u64>(VirtAddr(ADDR)),
        Ok(expected_memory),
        "{operation:?}"
      );
    }
  }

  #[test]
  fn lr_sc_match_the_interpreters_virtual_address_reservation_rules() {
    let mut cpu = RV64Cpu::new(None);
    cpu.bus.write::<u32>(VirtAddr(ADDR), 0x8000_0001).unwrap();

    let (loaded, frame) = invoke(&mut cpu, ADDR, 0, AtomicOp::LrW);
    assert_eq!(exception(&frame), None);
    assert_eq!(loaded, 0xffff_ffff_8000_0001);
    assert_eq!(cpu.reserved, vec![VirtAddr(ADDR)]);

    // Reservations carry no width in the current CPU model, so LR.W also permits SC.D at the same
    // 8-byte-aligned virtual address.
    let (status, frame) = invoke(&mut cpu, ADDR, 0x1122_3344_5566_7788, AtomicOp::ScD);
    assert_eq!(exception(&frame), None);
    assert_eq!(status, 0);
    assert!(cpu.reserved.is_empty());
    assert_eq!(
      cpu.bus.read::<u64>(VirtAddr(ADDR)),
      Ok(0x1122_3344_5566_7788)
    );

    // A failed SC neither translates nor touches memory. It only removes reservations for the
    // exact attempted address (not every reservation held by the hart).
    cpu.reserved.push(VirtAddr(ADDR));
    let unmapped = 0x4000_0000;
    let (status, frame) = invoke(&mut cpu, unmapped, 0x55, AtomicOp::ScW);
    assert_eq!(exception(&frame), None);
    assert_eq!(status, 1);
    assert_eq!(cpu.reserved, vec![VirtAddr(ADDR)]);
  }

  #[test]
  fn all_atomic_misalignment_uses_the_interpreters_load_exception() {
    let mut cpu = RV64Cpu::new(None);
    cpu.reserved.push(VirtAddr(ADDR + 2));

    let (_, frame) = invoke(&mut cpu, ADDR + 2, 0x55, AtomicOp::ScW);
    assert_eq!(
      exception(&frame),
      Some(Exception::LoadAddressMisaligned(VirtAddr(ADDR + 2))),
    );
    assert_eq!(cpu.reserved, vec![VirtAddr(ADDR + 2)]);

    let (_, frame) = invoke(&mut cpu, ADDR + 4, 0x55, AtomicOp::AddD);
    assert_eq!(
      exception(&frame),
      Some(Exception::LoadAddressMisaligned(VirtAddr(ADDR + 4))),
    );
  }

  #[test]
  fn atomic_faults_increment_the_shared_memory_fault_counter() {
    let mut cpu = RV64Cpu::new(None);
    let mut tlb = SoftwareTlb::new();

    let (_, frame) = invoke_with_tlb(&mut cpu, &mut tlb, ADDR + 2, 0, AtomicOp::LrW);
    assert_eq!(
      exception(&frame),
      Some(Exception::LoadAddressMisaligned(VirtAddr(ADDR + 2))),
    );
    assert_eq!(tlb.stats().faults, 1);

    let (_, frame) = invoke_with_tlb(&mut cpu, &mut tlb, ADDR, 0, AtomicOp::LrW);
    assert_eq!(exception(&frame), None);
    assert_eq!(tlb.stats().faults, 1);
  }

  struct ReadOnlyDevice {
    base: VirtAddr,
  }

  impl Device for ReadOnlyDevice {
    fn name(&self) -> &'static str {
      "atomic-read-only-test"
    }
    fn vendor_id(&self) -> u16 {
      0
    }
    fn device_id(&self) -> u16 {
      0
    }
    fn init(&self) -> Result<Vec<(VirtAddr, VirtAddr)>, ()> {
      Ok(vec![(self.base, self.base + VirtAddr(0x100))])
    }
    fn destroy(&self) -> Result<(), ()> {
      Ok(())
    }
    fn dma_read(&self, _: VirtAddr) -> Option<&Memory> {
      None
    }
    fn dma_write(&self, _: VirtAddr) -> Option<&mut Memory> {
      None
    }
    fn mmio_read(&self, _: VirtAddr) -> Option<u8> {
      Some(0x11)
    }
    fn mmio_write(&self, _: VirtAddr, _: u8) -> Result<(), ()> {
      Err(())
    }
    fn is_interrupting(&self) -> Option<u64> {
      None
    }
  }

  #[test]
  fn amo_and_successful_sc_preserve_store_fault_and_reservation_ordering() {
    let mut cpu = RV64Cpu::new(None);
    let address = VirtAddr(0x4000_0000);
    unsafe {
      cpu
        .bus
        .add_device(Arc::new(ReadOnlyDevice { base: address }))
        .unwrap()
    };

    let (_, frame) = invoke(&mut cpu, address.0, 1, AtomicOp::AddW);
    assert_eq!(
      exception(&frame),
      Some(Exception::StoreAccessFault(address))
    );

    cpu.reserved.push(address);
    let (_, frame) = invoke(&mut cpu, address.0, 1, AtomicOp::ScW);
    assert_eq!(
      exception(&frame),
      Some(Exception::StoreAccessFault(address))
    );
    assert!(!cpu.reserved.contains(&address));
  }

  struct PanicDevice {
    base: VirtAddr,
  }

  impl Device for PanicDevice {
    fn name(&self) -> &'static str {
      "atomic-panic-test"
    }
    fn vendor_id(&self) -> u16 {
      0
    }
    fn device_id(&self) -> u16 {
      0
    }
    fn init(&self) -> Result<Vec<(VirtAddr, VirtAddr)>, ()> {
      Ok(vec![(self.base, self.base + VirtAddr(0x100))])
    }
    fn destroy(&self) -> Result<(), ()> {
      Ok(())
    }
    fn dma_read(&self, _: VirtAddr) -> Option<&Memory> {
      None
    }
    fn dma_write(&self, _: VirtAddr) -> Option<&mut Memory> {
      None
    }
    fn mmio_read(&self, _: VirtAddr) -> Option<u8> {
      panic!("test helper panic")
    }
    fn mmio_write(&self, _: VirtAddr, _: u8) -> Result<(), ()> {
      Ok(())
    }
    fn is_interrupting(&self) -> Option<u64> {
      None
    }
  }

  #[test]
  fn panic_is_caught_before_crossing_the_jit_abi() {
    let mut cpu = RV64Cpu::new(None);
    let address = VirtAddr(0x4001_0000);
    unsafe {
      cpu
        .bus
        .add_device(Arc::new(PanicDevice { base: address }))
        .unwrap()
    };

    let (_, frame) = invoke(&mut cpu, address.0, 0, AtomicOp::LrW);
    assert_eq!(exception(&frame), Some(Exception::LoadAccessFault(address)));
  }
}
