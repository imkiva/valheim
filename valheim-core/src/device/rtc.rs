use std::cell::Cell;
use std::sync::Arc;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crate::cpu::irq::Exception;
use crate::memory::VirtAddr;

/// QEMU RISC-V `virt` Goldfish RTC MMIO base.
pub const RTC_BASE: u64 = 0x0010_1000;

/// PLIC source used by the QEMU `virt` Goldfish RTC.
pub const RTC_IRQ: u64 = 11;

pub const RTC_TIME_LOW: u64 = RTC_BASE;
pub const RTC_TIME_HIGH: u64 = RTC_BASE + 0x04;
pub const RTC_ALARM_LOW: u64 = RTC_BASE + 0x08;
pub const RTC_ALARM_HIGH: u64 = RTC_BASE + 0x0c;
pub const RTC_IRQ_ENABLED: u64 = RTC_BASE + 0x10;
pub const RTC_CLEAR_ALARM: u64 = RTC_BASE + 0x14;
pub const RTC_ALARM_STATUS: u64 = RTC_BASE + 0x18;
pub const RTC_CLEAR_INTERRUPT: u64 = RTC_BASE + 0x1c;

const RTC_REGISTER_SPACE_SIZE: u64 = 0x24;

/// Wall-clock source used by the RTC, represented as time since the Unix epoch.
///
/// This is deliberately separate from CLINT's monotonic clock. Production follows changes to the
/// host wall clock while tests can inject deterministic epoch nanoseconds without sleeping.
pub trait RtcClock: Send + Sync {
  fn now(&self) -> Duration;
}

pub struct HostRtcClock;

impl RtcClock for HostRtcClock {
  fn now(&self) -> Duration {
    SystemTime::now()
      .duration_since(UNIX_EPOCH)
      .unwrap_or(Duration::ZERO)
  }
}

/// QEMU-compatible Goldfish RTC used by the RISC-V `virt` platform.
///
/// Register values are Unix epoch nanoseconds. Reading `TIME_LOW` samples the complete counter and
/// latches `TIME_HIGH`, matching the ordering relied upon by Linux's `rtc-goldfish` driver.
pub struct GoldfishRtc {
  clock: Arc<dyn RtcClock>,
  tick_offset: Cell<u64>,
  alarm_next: Cell<u64>,
  alarm_deadline: Cell<u64>,
  alarm_running: Cell<bool>,
  irq_pending: Cell<bool>,
  irq_enabled: Cell<bool>,
  time_high: Cell<u32>,
}

impl GoldfishRtc {
  pub fn new() -> Self {
    Self::new_with_clock(Arc::new(HostRtcClock))
  }

  pub fn new_with_clock(clock: Arc<dyn RtcClock>) -> Self {
    Self {
      clock,
      tick_offset: Cell::new(0),
      alarm_next: Cell::new(0),
      alarm_deadline: Cell::new(0),
      alarm_running: Cell::new(false),
      irq_pending: Cell::new(false),
      irq_enabled: Cell::new(false),
      time_high: Cell::new(0),
    }
  }

  fn host_nanos(&self) -> u64 {
    self.clock.now().as_nanos().min(u128::from(u64::MAX)) as u64
  }

  pub fn count(&self) -> u64 {
    self.host_nanos().wrapping_add(self.tick_offset.get())
  }

  fn set_count(&self, count: u64, sampled_count: u64) {
    self.tick_offset.set(
      self
        .tick_offset
        .get()
        .wrapping_add(count.wrapping_sub(sampled_count)),
    );
  }

  fn refresh_alarm_at(&self, host_nanos: u64) -> bool {
    if self.alarm_running.get() && self.alarm_deadline.get() <= host_nanos {
      self.alarm_running.set(false);
      self.irq_pending.set(true);
      true
    } else {
      false
    }
  }

  fn refresh_alarm(&self) -> bool {
    self.refresh_alarm_at(self.host_nanos())
  }

  fn arm_alarm(&self) {
    let host_nanos = self.host_nanos();
    let current = host_nanos.wrapping_add(self.tick_offset.get());
    if self.alarm_next.get() <= current {
      self.alarm_running.set(false);
      self.irq_pending.set(true);
    } else {
      self.alarm_deadline.set(
        host_nanos.saturating_add(self.alarm_next.get().saturating_sub(current)),
      );
      self.alarm_running.set(true);
    }
  }

  pub fn interrupt_asserted(&self) -> bool {
    self.refresh_alarm();
    self.irq_pending.get() && self.irq_enabled.get()
  }

  /// Returns the host wait until an enabled alarm can assert its interrupt line.
  ///
  /// `Some(Duration::ZERO)` means that the level is already asserted. `None` means no enabled
  /// future or pending alarm currently needs a host deadline.
  pub fn duration_until_alarm(&self) -> Option<Duration> {
    let host_nanos = self.host_nanos();
    let fired = self.refresh_alarm_at(host_nanos);
    if !self.irq_enabled.get() {
      return None;
    }
    // A pending line that was already observed by normal device polling must not create a zero
    // timeout loop when the guest has masked the PLIC source. A newly fired alarm returns zero
    // exactly once to close the interrupt-poll-to-WFI-wait race.
    if fired {
      return Some(Duration::ZERO);
    }
    if !self.alarm_running.get() {
      return None;
    }
    Some(Duration::from_nanos(
      self.alarm_deadline.get().saturating_sub(host_nanos),
    ))
  }

  pub fn accepts(&self, addr: VirtAddr, width: usize) -> bool {
    let Some(offset) = addr.0.checked_sub(RTC_BASE) else {
      return false;
    };
    width == 4 && offset % 4 == 0 && offset < RTC_REGISTER_SPACE_SIZE
  }

  pub fn read(&self, addr: VirtAddr) -> Result<u32, Exception> {
    if !self.accepts(addr, 4) {
      return Err(Exception::LoadAccessFault(addr));
    }
    let value = match addr.0 {
      RTC_TIME_LOW => {
        let count = self.count();
        self.time_high.set((count >> 32) as u32);
        count as u32
      }
      RTC_TIME_HIGH => self.time_high.get(),
      RTC_ALARM_LOW => self.alarm_next.get() as u32,
      RTC_ALARM_HIGH => (self.alarm_next.get() >> 32) as u32,
      RTC_IRQ_ENABLED => u32::from(self.irq_enabled.get()),
      RTC_ALARM_STATUS => {
        self.refresh_alarm();
        u32::from(self.alarm_running.get())
      }
      // QEMU accepts these reads but reports the registers as unimplemented and returns zero.
      _ => 0,
    };
    Ok(value)
  }

  pub fn write(&self, addr: VirtAddr, value: u32) -> Result<(), Exception> {
    if !self.accepts(addr, 4) {
      return Err(Exception::StoreAccessFault(addr));
    }
    match addr.0 {
      RTC_TIME_LOW => {
        let current = self.count();
        let count = (current & 0xffff_ffff_0000_0000) | u64::from(value);
        self.set_count(count, current);
      }
      RTC_TIME_HIGH => {
        let current = self.count();
        let count = (u64::from(value) << 32) | (current & 0xffff_ffff);
        self.set_count(count, current);
      }
      RTC_ALARM_LOW => {
        self
          .alarm_next
          .set((self.alarm_next.get() & 0xffff_ffff_0000_0000) | u64::from(value));
        self.arm_alarm();
      }
      RTC_ALARM_HIGH => self
        .alarm_next
        .set((u64::from(value) << 32) | (self.alarm_next.get() & 0xffff_ffff)),
      RTC_IRQ_ENABLED => self.irq_enabled.set(value & 1 != 0),
      RTC_CLEAR_ALARM => self.alarm_running.set(false),
      RTC_CLEAR_INTERRUPT => self.irq_pending.set(false),
      // Writes to the read-only status and the final reserved word are ignored by QEMU.
      _ => {}
    }
    Ok(())
  }

  pub fn reset(&self) {
    self.tick_offset.set(0);
    self.alarm_next.set(0);
    self.alarm_deadline.set(0);
    self.alarm_running.set(false);
    self.irq_pending.set(false);
    self.irq_enabled.set(false);
    self.time_high.set(0);
  }
}

impl Default for GoldfishRtc {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use std::sync::atomic::{AtomicU64, Ordering};

  use super::*;

  #[derive(Default)]
  struct ManualRtcClock {
    nanos: AtomicU64,
  }

  impl ManualRtcClock {
    fn set_nanos(&self, nanos: u64) {
      self.nanos.store(nanos, Ordering::Relaxed);
    }
  }

  impl RtcClock for ManualRtcClock {
    fn now(&self) -> Duration {
      Duration::from_nanos(self.nanos.load(Ordering::Relaxed))
    }
  }

  fn rtc_with_clock(nanos: u64) -> (GoldfishRtc, Arc<ManualRtcClock>) {
    let clock = Arc::new(ManualRtcClock::default());
    clock.set_nanos(nanos);
    (GoldfishRtc::new_with_clock(clock.clone()), clock)
  }

  #[test]
  fn time_low_samples_and_latches_time_high() {
    let (rtc, clock) = rtc_with_clock(0x1234_5678_ffff_fffe);

    assert_eq!(rtc.read(VirtAddr(RTC_TIME_HIGH)), Ok(0));
    assert_eq!(rtc.read(VirtAddr(RTC_TIME_LOW)), Ok(0xffff_fffe));
    clock.set_nanos(0x1234_5679_0000_0001);
    assert_eq!(rtc.read(VirtAddr(RTC_TIME_HIGH)), Ok(0x1234_5678));

    assert_eq!(rtc.read(VirtAddr(RTC_TIME_LOW)), Ok(1));
    assert_eq!(rtc.read(VirtAddr(RTC_TIME_HIGH)), Ok(0x1234_5679));
  }

  #[test]
  fn time_high_then_low_writes_reanchor_the_counter() {
    let (rtc, clock) = rtc_with_clock(100);

    rtc
      .write(VirtAddr(RTC_TIME_HIGH), 0x1122_3344)
      .unwrap();
    rtc.write(VirtAddr(RTC_TIME_LOW), 0x5566_7788).unwrap();
    assert_eq!(rtc.count(), 0x1122_3344_5566_7788);

    clock.set_nanos(200);
    assert_eq!(rtc.count(), 0x1122_3344_5566_77ec);
  }

  #[test]
  fn future_alarm_expires_and_drives_a_level_interrupt() {
    let (rtc, clock) = rtc_with_clock(1_000);
    rtc.write(VirtAddr(RTC_ALARM_HIGH), 0).unwrap();
    rtc.write(VirtAddr(RTC_ALARM_LOW), 1_500).unwrap();
    rtc.write(VirtAddr(RTC_IRQ_ENABLED), 1).unwrap();

    assert_eq!(rtc.read(VirtAddr(RTC_ALARM_STATUS)), Ok(1));
    assert_eq!(rtc.duration_until_alarm(), Some(Duration::from_nanos(500)));
    assert!(!rtc.interrupt_asserted());

    clock.set_nanos(1_500);
    assert_eq!(rtc.duration_until_alarm(), Some(Duration::ZERO));
    assert_eq!(rtc.read(VirtAddr(RTC_ALARM_STATUS)), Ok(0));
    assert!(rtc.interrupt_asserted());

    rtc.write(VirtAddr(RTC_CLEAR_INTERRUPT), 1).unwrap();
    assert!(!rtc.interrupt_asserted());
    assert_eq!(rtc.duration_until_alarm(), None);
  }

  #[test]
  fn alarm_high_write_does_not_reschedule_an_active_alarm() {
    let now = 0x0000_0002_0000_0064;
    let (rtc, clock) = rtc_with_clock(now);
    rtc.write(VirtAddr(RTC_ALARM_HIGH), 3).unwrap();
    rtc.write(VirtAddr(RTC_ALARM_LOW), 50).unwrap();
    rtc.write(VirtAddr(RTC_IRQ_ENABLED), 1).unwrap();

    // Linux stages a replacement high half before committing it with ALARM_LOW. The intermediate
    // register value is in the past, but the already-scheduled host timer must remain active.
    rtc.write(VirtAddr(RTC_ALARM_HIGH), 2).unwrap();
    assert!(!rtc.interrupt_asserted());
    assert_eq!(
      rtc.duration_until_alarm(),
      Some(Duration::from_nanos(0xffff_ffce)),
    );

    rtc.write(VirtAddr(RTC_ALARM_LOW), 200).unwrap();
    assert!(!rtc.interrupt_asserted());
    assert_eq!(rtc.duration_until_alarm(), Some(Duration::from_nanos(100)));

    clock.set_nanos(now + 100);
    assert_eq!(rtc.duration_until_alarm(), Some(Duration::ZERO));
    assert!(rtc.interrupt_asserted());
  }

  #[test]
  fn setting_time_does_not_move_an_active_alarm_host_deadline() {
    let (rtc, clock) = rtc_with_clock(1_000);
    rtc.write(VirtAddr(RTC_ALARM_HIGH), 0).unwrap();
    rtc.write(VirtAddr(RTC_ALARM_LOW), 2_000).unwrap();
    rtc.write(VirtAddr(RTC_IRQ_ENABLED), 1).unwrap();
    assert_eq!(rtc.duration_until_alarm(), Some(Duration::from_nanos(1_000)));

    rtc.write(VirtAddr(RTC_TIME_HIGH), 0).unwrap();
    rtc.write(VirtAddr(RTC_TIME_LOW), 5_000).unwrap();
    assert_eq!(rtc.count(), 5_000);
    assert!(!rtc.interrupt_asserted());
    assert_eq!(rtc.duration_until_alarm(), Some(Duration::from_nanos(1_000)));

    clock.set_nanos(2_000);
    assert_eq!(rtc.duration_until_alarm(), Some(Duration::ZERO));
    assert!(rtc.interrupt_asserted());
  }

  #[test]
  fn disabled_alarm_latches_pending_until_enabled_or_cleared() {
    let (rtc, _) = rtc_with_clock(1_000);
    rtc.write(VirtAddr(RTC_ALARM_LOW), 999).unwrap();

    assert!(!rtc.interrupt_asserted());
    rtc.write(VirtAddr(RTC_CLEAR_ALARM), 1).unwrap();
    rtc.write(VirtAddr(RTC_IRQ_ENABLED), 1).unwrap();
    assert!(rtc.interrupt_asserted());

    rtc.write(VirtAddr(RTC_IRQ_ENABLED), 0).unwrap();
    assert!(!rtc.interrupt_asserted());
    rtc.write(VirtAddr(RTC_IRQ_ENABLED), 1).unwrap();
    assert!(rtc.interrupt_asserted());
    rtc.write(VirtAddr(RTC_CLEAR_INTERRUPT), 1).unwrap();
    assert!(!rtc.interrupt_asserted());
  }

  #[test]
  fn clear_alarm_cancels_a_future_deadline_without_clearing_pending() {
    let (rtc, clock) = rtc_with_clock(10);
    rtc.write(VirtAddr(RTC_IRQ_ENABLED), 1).unwrap();
    rtc.write(VirtAddr(RTC_ALARM_LOW), 20).unwrap();
    rtc.write(VirtAddr(RTC_CLEAR_ALARM), 0).unwrap();

    clock.set_nanos(30);
    assert_eq!(rtc.read(VirtAddr(RTC_ALARM_STATUS)), Ok(0));
    assert!(!rtc.interrupt_asserted());
    assert_eq!(rtc.duration_until_alarm(), None);
  }

  #[test]
  fn register_window_requires_aligned_32_bit_accesses() {
    let (rtc, _) = rtc_with_clock(0);
    assert!(rtc.accepts(VirtAddr(RTC_BASE), 4));
    assert!(rtc.accepts(VirtAddr(RTC_BASE + 0x20), 4));
    assert!(!rtc.accepts(VirtAddr(RTC_BASE), 1));
    assert!(!rtc.accepts(VirtAddr(RTC_BASE + 2), 4));
    assert!(!rtc.accepts(VirtAddr(RTC_BASE + RTC_REGISTER_SPACE_SIZE), 4));
    assert_eq!(rtc.read(VirtAddr(RTC_BASE + 0x20)), Ok(0));
  }

  #[test]
  fn reset_restores_host_time_and_clears_alarm_state() {
    let (rtc, clock) = rtc_with_clock(100);
    rtc.write(VirtAddr(RTC_TIME_LOW), 42).unwrap();
    rtc.write(VirtAddr(RTC_ALARM_LOW), 42).unwrap();
    rtc.write(VirtAddr(RTC_IRQ_ENABLED), 1).unwrap();
    assert!(rtc.interrupt_asserted());

    clock.set_nanos(200);
    rtc.reset();
    assert_eq!(rtc.count(), 200);
    assert_eq!(rtc.read(VirtAddr(RTC_ALARM_LOW)), Ok(0));
    assert_eq!(rtc.read(VirtAddr(RTC_IRQ_ENABLED)), Ok(0));
    assert!(!rtc.interrupt_asserted());
  }
}
