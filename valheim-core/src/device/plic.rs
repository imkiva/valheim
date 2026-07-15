use std::cell::Cell;

use crate::cpu::bus::PLIC_BASE;
use crate::cpu::irq::Exception;
use crate::memory::VirtAddr;

/// The address for interrupt source priority. 1024 4-byte registers exist. Each interrupt into the
/// PLIC has a configurable priority, from 1-7, with 7 being the highest priority. A value of 0
/// means do not interrupt, effectively disabling that interrupt.
const SOURCE_PRIORITY: u64 = PLIC_BASE;
const SOURCE_PRIORITY_END: u64 = PLIC_BASE + 0xfff;

/// The address range for interrupt pending bits. 32 4-byte (1024 bits) registers exist.
const PENDING: u64 = PLIC_BASE + 0x1000;
const PENDING_END: u64 = PLIC_BASE + 0x107f;

/// The address range for enable registers. This PLIC implements M-mode context 0 and S-mode
/// context 1 for the single hart exposed by Valheim.
const ENABLE: u64 = PLIC_BASE + 0x2000;
const ENABLE_END: u64 = PLIC_BASE + 0x20ff;

/// Context 0 starts at offset 0 and context 1 at offset 0x1000. Each context has a priority
/// threshold at offset 0 and a claim/complete register at offset 4.
const THRESHOLD_AND_CLAIM: u64 = PLIC_BASE + 0x200000;
const THRESHOLD_AND_CLAIM_END: u64 = PLIC_BASE + 0x201007;

const WORD_SIZE: u64 = 4;
const CONTEXT_OFFSET: u64 = 0x1000;
const SOURCE_NUM: usize = 1024;
const SOURCE_WORDS: usize = SOURCE_NUM / 32;
const CONTEXT_NUM: usize = 2;
const NO_CONTEXT: u8 = 0;

/// The platform-level interrupt controller (PLIC).
///
/// `Bus::read` only needs a shared reference, but reading a claim register is inherently
/// stateful. Pending, level and in-service bookkeeping therefore use `Cell`; configuration writes
/// still require an exclusive reference through the normal MMIO write path.
pub struct Plic {
  priority: [u32; SOURCE_NUM],
  pending: [Cell<u32>; SOURCE_WORDS],
  enable: [u32; SOURCE_WORDS * CONTEXT_NUM],
  threshold: [u32; CONTEXT_NUM],
  claim: [Cell<u32>; CONTEXT_NUM],
  level: [Cell<u32>; SOURCE_WORDS],
  /// Zero means that the source is not in service; otherwise this stores `context + 1`.
  in_service_context: [Cell<u8>; SOURCE_NUM],
}

impl Plic {
  pub fn new() -> Self {
    Self {
      priority: [0; SOURCE_NUM],
      pending: std::array::from_fn(|_| Cell::new(0)),
      enable: [0; SOURCE_WORDS * CONTEXT_NUM],
      threshold: [0; CONTEXT_NUM],
      claim: std::array::from_fn(|_| Cell::new(0)),
      level: std::array::from_fn(|_| Cell::new(0)),
      in_service_context: std::array::from_fn(|_| Cell::new(NO_CONTEXT)),
    }
  }

  fn source_position(irq: u64) -> Option<(usize, u32)> {
    let irq = usize::try_from(irq).ok()?;
    if irq == 0 || irq >= SOURCE_NUM {
      return None;
    }
    Some((irq / 32, 1_u32 << (irq % 32)))
  }

  fn source_is_pending(&self, irq: usize) -> bool {
    let word = irq / 32;
    let mask = 1_u32 << (irq % 32);
    self.pending[word].get() & mask != 0
  }

  fn source_level_is_high(&self, irq: usize) -> bool {
    let word = irq / 32;
    let mask = 1_u32 << (irq % 32);
    self.level[word].get() & mask != 0
  }

  fn set_pending(&self, irq: u64) {
    let Some((word, mask)) = Self::source_position(irq) else {
      return;
    };
    let irq = irq as usize;
    if self.in_service_context[irq].get() == NO_CONTEXT {
      self.pending[word].set(self.pending[word].get() | mask);
      self.recompute_claims();
    }
  }

  fn clear_pending(&self, irq: usize) {
    let word = irq / 32;
    let mask = 1_u32 << (irq % 32);
    self.pending[word].set(self.pending[word].get() & !mask);
  }

  /// Injects a pulse/edge request. This preserves the one-shot interface used by UART devices.
  /// Pulses arriving while the source already has a pending or in-service request are coalesced by
  /// the PLIC gateway.
  pub fn update_pending(&self, irq: u64) {
    self.set_pending(irq);
  }

  /// Sets the electrical level of an interrupt source. An asserted source creates one pending
  /// request. If it remains asserted when software completes that request, the gateway creates a
  /// new request; deasserting it prevents that re-pend.
  pub fn set_source_level(&self, irq: u64, asserted: bool) {
    let Some((word, mask)) = Self::source_position(irq) else {
      return;
    };
    if asserted {
      self.level[word].set(self.level[word].get() | mask);
      self.set_pending(irq);
    } else {
      self.level[word].set(self.level[word].get() & !mask);
    }
  }

  /// Whether S-mode context 1 currently has a claimable external interrupt.
  pub fn supervisor_irq_pending(&self) -> bool {
    self.claim[1].get() != 0
  }

  fn is_enabled(&self, context: usize, irq: usize) -> bool {
    let word = irq / 32;
    let mask = 1_u32 << (irq % 32);
    self.enable[context * SOURCE_WORDS + word] & mask != 0
  }

  fn recompute_claim(&self, context: usize) {
    let mut best_irq = 0;
    let mut best_priority = self.threshold[context];
    for irq in 1..SOURCE_NUM {
      let priority = self.priority[irq];
      if self.source_is_pending(irq)
        && self.is_enabled(context, irq)
        && priority > best_priority
      {
        best_irq = irq as u32;
        best_priority = priority;
      }
    }
    self.claim[context].set(best_irq);
  }

  fn recompute_claims(&self) {
    for context in 0..CONTEXT_NUM {
      self.recompute_claim(context);
    }
  }

  fn claim(&self, context: usize) -> u32 {
    self.recompute_claim(context);
    let irq = self.claim[context].get();
    if irq == 0 {
      return 0;
    }

    self.clear_pending(irq as usize);
    self.in_service_context[irq as usize].set(context as u8 + 1);
    self.recompute_claims();
    irq
  }

  fn complete(&self, context: usize, irq: u32) {
    let irq = irq as usize;
    if irq == 0
      || irq >= SOURCE_NUM
      || self.in_service_context[irq].get() != context as u8 + 1
    {
      return;
    }

    self.in_service_context[irq].set(NO_CONTEXT);
    if self.source_level_is_high(irq) {
      let word = irq / 32;
      let mask = 1_u32 << (irq % 32);
      self.pending[word].set(self.pending[word].get() | mask);
    }
    self.recompute_claims();
  }

  pub fn read(&self, addr: VirtAddr) -> Result<u32, Exception> {
    let addr = addr.0;
    match addr {
      SOURCE_PRIORITY..=SOURCE_PRIORITY_END => {
        if (addr - SOURCE_PRIORITY) % WORD_SIZE != 0 {
          return Err(Exception::LoadAccessFault(VirtAddr(addr)));
        }
        let index = ((addr - SOURCE_PRIORITY) / WORD_SIZE) as usize;
        Ok(self.priority[index])
      }
      PENDING..=PENDING_END => {
        if (addr - PENDING) % WORD_SIZE != 0 {
          return Err(Exception::LoadAccessFault(VirtAddr(addr)));
        }
        let index = ((addr - PENDING) / WORD_SIZE) as usize;
        Ok(self.pending[index].get())
      }
      ENABLE..=ENABLE_END => {
        if (addr - ENABLE) % WORD_SIZE != 0 {
          return Err(Exception::LoadAccessFault(VirtAddr(addr)));
        }
        let index = ((addr - ENABLE) / WORD_SIZE) as usize;
        Ok(self.enable[index])
      }
      THRESHOLD_AND_CLAIM..=THRESHOLD_AND_CLAIM_END => {
        let context = ((addr - THRESHOLD_AND_CLAIM) / CONTEXT_OFFSET) as usize;
        let offset = addr - (THRESHOLD_AND_CLAIM + CONTEXT_OFFSET * context as u64);
        match offset {
          0 => Ok(self.threshold[context]),
          4 => Ok(self.claim(context)),
          _ => Err(Exception::LoadAccessFault(VirtAddr(addr))),
        }
      }
      _ => Err(Exception::LoadAccessFault(VirtAddr(addr))),
    }
  }

  pub fn write(&mut self, addr: VirtAddr, value: u32) -> Result<(), Exception> {
    let addr = addr.0;
    match addr {
      SOURCE_PRIORITY..=SOURCE_PRIORITY_END => {
        if (addr - SOURCE_PRIORITY) % WORD_SIZE != 0 {
          return Err(Exception::StoreAccessFault(VirtAddr(addr)));
        }
        let index = ((addr - SOURCE_PRIORITY) / WORD_SIZE) as usize;
        if index != 0 {
          self.priority[index] = value;
          self.recompute_claims();
        }
      }
      PENDING..=PENDING_END => {
        if (addr - PENDING) % WORD_SIZE != 0 {
          return Err(Exception::StoreAccessFault(VirtAddr(addr)));
        }
        // Pending registers are read-only. In particular, source 0 must remain hard-wired to zero.
      }
      ENABLE..=ENABLE_END => {
        if (addr - ENABLE) % WORD_SIZE != 0 {
          return Err(Exception::StoreAccessFault(VirtAddr(addr)));
        }
        let index = ((addr - ENABLE) / WORD_SIZE) as usize;
        let context = index / SOURCE_WORDS;
        self.enable[index] = if index % SOURCE_WORDS == 0 {
          value & !1
        } else {
          value
        };
        self.recompute_claim(context);
      }
      THRESHOLD_AND_CLAIM..=THRESHOLD_AND_CLAIM_END => {
        let context = ((addr - THRESHOLD_AND_CLAIM) / CONTEXT_OFFSET) as usize;
        let offset = addr - (THRESHOLD_AND_CLAIM + CONTEXT_OFFSET * context as u64);
        match offset {
          0 => {
            self.threshold[context] = value;
            self.recompute_claim(context);
          }
          4 => self.complete(context, value),
          _ => return Err(Exception::StoreAccessFault(VirtAddr(addr))),
        }
      }
      _ => return Err(Exception::StoreAccessFault(VirtAddr(addr))),
    }
    Ok(())
  }
}

impl Default for Plic {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::{Plic, ENABLE, PENDING, SOURCE_PRIORITY, THRESHOLD_AND_CLAIM};
  use crate::memory::VirtAddr;

  const S_ENABLE: u64 = ENABLE + 0x80;
  const S_THRESHOLD: u64 = THRESHOLD_AND_CLAIM + 0x1000;
  const S_CLAIM: u64 = S_THRESHOLD + 4;

  fn configure_source(plic: &mut Plic, irq: u32, priority: u32) {
    plic
      .write(VirtAddr(SOURCE_PRIORITY + u64::from(irq) * 4), priority)
      .unwrap();
    let word = u64::from(irq / 32);
    let old = plic.read(VirtAddr(S_ENABLE + word * 4)).unwrap();
    plic
      .write(VirtAddr(S_ENABLE + word * 4), old | (1_u32 << (irq % 32)))
      .unwrap();
  }

  #[test]
  fn pending_uses_irq_divided_by_32_and_source_zero_is_invalid() {
    let plic = Plic::new();
    plic.update_pending(33);
    plic.update_pending(0);
    plic.update_pending(1024);

    assert_eq!(plic.read(VirtAddr(PENDING)).unwrap(), 0);
    assert_eq!(plic.read(VirtAddr(PENDING + 4)).unwrap(), 1 << 1);
  }

  #[test]
  fn supervisor_claim_recomputes_after_configuration_changes() {
    let mut plic = Plic::new();
    plic.update_pending(5);
    assert!(!plic.supervisor_irq_pending());

    plic.write(VirtAddr(SOURCE_PRIORITY + 5 * 4), 3).unwrap();
    assert!(!plic.supervisor_irq_pending());

    plic.write(VirtAddr(S_ENABLE), 1 << 5).unwrap();
    assert!(plic.supervisor_irq_pending());

    plic.write(VirtAddr(S_THRESHOLD), 3).unwrap();
    assert!(!plic.supervisor_irq_pending());
    plic.write(VirtAddr(S_THRESHOLD), 2).unwrap();
    assert!(plic.supervisor_irq_pending());

    plic.write(VirtAddr(S_ENABLE), 0).unwrap();
    assert!(!plic.supervisor_irq_pending());
  }

  #[test]
  fn claim_selects_highest_priority_and_marks_source_in_service() {
    let mut plic = Plic::new();
    configure_source(&mut plic, 5, 2);
    configure_source(&mut plic, 6, 3);
    plic.update_pending(5);
    plic.update_pending(6);

    assert_eq!(plic.read(VirtAddr(S_CLAIM)).unwrap(), 6);
    assert_eq!(plic.read(VirtAddr(PENDING)).unwrap() & (1 << 6), 0);
    assert_eq!(plic.in_service_context[6].get(), 2);
    assert_eq!(plic.claim[1].get(), 5);
  }

  #[test]
  fn equal_priority_claims_use_lowest_source_id() {
    let mut plic = Plic::new();
    configure_source(&mut plic, 5, 3);
    configure_source(&mut plic, 6, 3);
    plic.update_pending(6);
    plic.update_pending(5);

    assert_eq!(plic.read(VirtAddr(S_CLAIM)).unwrap(), 5);
  }

  #[test]
  fn completing_asserted_level_source_repends_until_deasserted() {
    let mut plic = Plic::new();
    configure_source(&mut plic, 1, 1);
    plic.set_source_level(1, true);

    assert_eq!(plic.read(VirtAddr(S_CLAIM)).unwrap(), 1);
    assert!(!plic.supervisor_irq_pending());
    plic.write(VirtAddr(S_CLAIM), 1).unwrap();
    assert!(plic.supervisor_irq_pending());

    assert_eq!(plic.read(VirtAddr(S_CLAIM)).unwrap(), 1);
    plic.set_source_level(1, false);
    plic.write(VirtAddr(S_CLAIM), 1).unwrap();
    assert!(!plic.supervisor_irq_pending());
  }

  #[test]
  fn completing_pulse_source_does_not_repend() {
    let mut plic = Plic::new();
    configure_source(&mut plic, 10, 1);
    plic.update_pending(10);

    assert_eq!(plic.read(VirtAddr(S_CLAIM)).unwrap(), 10);
    plic.write(VirtAddr(S_CLAIM), 10).unwrap();
    assert!(!plic.supervisor_irq_pending());
  }

  #[test]
  fn source_zero_priority_enable_and_pending_are_hardwired_to_zero() {
    let mut plic = Plic::new();
    plic.write(VirtAddr(SOURCE_PRIORITY), u32::MAX).unwrap();
    plic.write(VirtAddr(S_ENABLE), u32::MAX).unwrap();
    plic.write(VirtAddr(PENDING), u32::MAX).unwrap();

    assert_eq!(plic.read(VirtAddr(SOURCE_PRIORITY)).unwrap(), 0);
    assert_eq!(plic.read(VirtAddr(S_ENABLE)).unwrap() & 1, 0);
    assert_eq!(plic.read(VirtAddr(PENDING)).unwrap() & 1, 0);
  }
}
