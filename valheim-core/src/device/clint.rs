// https://github.com/qemu/qemu/blob/master/hw/intc/sifive_clint.c
// https://github.com/qemu/qemu/blob/master/include/hw/intc/sifive_clint.h

use std::sync::Arc;
use std::time::{Duration, Instant};

use crate::cpu::bus::CLINT_BASE;
use crate::cpu::csr::CSRMap::{MSIP_MASK, MTIP_MASK};
use crate::cpu::irq::Exception;
use crate::memory::{CanIO, VirtAddr};

pub const TIMEBASE_FREQUENCY: u64 = 10_000_000;
const NANOS_PER_SECOND: u64 = 1_000_000_000;
const _: () = assert!(NANOS_PER_SECOND % TIMEBASE_FREQUENCY == 0);
// The platform timebase is fixed at exactly 100 ns per tick.
const NANOS_PER_TICK: u64 = NANOS_PER_SECOND / TIMEBASE_FREQUENCY;

/// Monotonic time source used by the platform real-time counter.
///
/// Production machines use [`HostClock`]. The explicit interface lets unit and differential tests
/// advance time without sleeping or reintroducing an instruction-count clock mode.
pub trait ClockSource: Send + Sync {
  fn now(&self) -> Duration;
}

pub struct HostClock {
  started: Instant,
}

impl HostClock {
  pub fn new() -> Self {
    Self { started: Instant::now() }
  }
}

impl Default for HostClock {
  fn default() -> Self {
    Self::new()
  }
}

impl ClockSource for HostClock {
  fn now(&self) -> Duration {
    self.started.elapsed()
  }
}

/// machine timer register
const MTIME: u64 = CLINT_BASE + 0xbff8;
const MTIME_WIDTH: usize = 8;

/// machine timer compare register
/// 3.2.1 Machine Timer Registers (mtime and mtimecmp)
/// Lower privilege levels do not have their own timecmp registers.
/// Machine-mode software multiplexes lower-privilege timers through the next mtimecmp deadline.
const MTIMECMP: u64 = CLINT_BASE + 0x4000;
const MTIMECMP_WIDTH: usize = 8;

/// machine software interrupt pending register
const MSIP: u64 = CLINT_BASE;
const MSIP_WIDTH: usize = 4;

pub struct Clint {
  msip: u32,
  mtimecmp: u64,
  clock: Arc<dyn ClockSource>,
  host_anchor: Duration,
  guest_anchor: u64,
}

impl Clint {
  pub fn new() -> Self {
    Self::new_with_clock(Arc::new(HostClock::new()))
  }

  pub fn new_with_clock(clock: Arc<dyn ClockSource>) -> Self {
    let host_anchor = clock.now();
    Self {
      msip: 0,
      mtimecmp: 0,
      clock,
      host_anchor,
      guest_anchor: 0,
    }
  }

  fn ticks_for_duration(duration: Duration) -> u64 {
    duration
      .as_secs()
      .wrapping_mul(TIMEBASE_FREQUENCY)
      .wrapping_add(u64::from(duration.subsec_nanos()) / NANOS_PER_TICK)
  }

  fn duration_for_ticks_ceil(ticks: u64) -> Duration {
    Duration::new(
      ticks / TIMEBASE_FREQUENCY,
      ((ticks % TIMEBASE_FREQUENCY) * NANOS_PER_TICK) as u32,
    )
  }

  fn mtime_at(&self, now: Duration) -> u64 {
    let elapsed = now.checked_sub(self.host_anchor).unwrap_or(Duration::ZERO);
    self
      .guest_anchor
      .wrapping_add(Self::ticks_for_duration(elapsed))
  }

  pub fn mtime(&self) -> u64 {
    self.mtime_at(self.clock.now())
  }

  /// Level-triggered local interrupt state derived from the current clock and software register.
  pub fn local_pending_bits(&self) -> u64 {
    let mtime = self.mtime();
    let mut pending = 0;
    if (self.msip & 1) != 0 {
      pending |= MSIP_MASK;
    }
    if mtime >= self.mtimecmp {
      pending |= MTIP_MASK;
    }
    pending
  }

  /// Host duration until a future timer deadline, rounded up so MTIP is never posted early.
  pub fn duration_until_timer(&self) -> Option<Duration> {
    let mtime = self.mtime();
    if self.mtimecmp > mtime {
      Some(Self::duration_for_ticks_ceil(self.mtimecmp - mtime))
    } else {
      None
    }
  }

  fn register_offset(addr: u64, base: u64, register_width: usize, width: usize) -> Option<u64> {
    let offset = addr.checked_sub(base)?;
    (offset.checked_add(width as u64)? <= register_width as u64).then_some(offset)
  }

  fn register_value(&self, addr: VirtAddr, width: usize) -> Option<(u64, u64)> {
    if let Some(offset) = Self::register_offset(addr.0, MSIP, MSIP_WIDTH, width) {
      return Some((self.msip as u64, offset));
    }
    if let Some(offset) = Self::register_offset(addr.0, MTIMECMP, MTIMECMP_WIDTH, width) {
      return Some((self.mtimecmp, offset));
    }
    Self::register_offset(addr.0, MTIME, MTIME_WIDTH, width)
      .map(|offset| (self.mtime(), offset))
  }

  pub fn accepts(&self, addr: VirtAddr, width: usize) -> bool {
    Self::register_offset(addr.0, MSIP, MSIP_WIDTH, width).is_some() ||
      Self::register_offset(addr.0, MTIMECMP, MTIMECMP_WIDTH, width).is_some() ||
      Self::register_offset(addr.0, MTIME, MTIME_WIDTH, width).is_some()
  }

  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Result<u64, Exception> {
    let width = std::mem::size_of::<T>();
    let (val, offset) = self
      .register_value(addr, width)
      .ok_or(Exception::LoadAccessFault(addr))?;

    let val = match width {
      1 => (val >> (offset * 8)) & 0xff,
      2 => (val >> (offset * 8)) & 0xffff,
      4 => (val >> (offset * 8)) & 0xffffffff,
      8 => val,
      _ => return Err(Exception::LoadAccessFault(addr)),
    };
    Ok(val)
  }

  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, value: u64) -> Result<(), Exception> {
    let width = std::mem::size_of::<T>();
    let mut now = None;
    let (mut old, offset) =
      if let Some(offset) = Self::register_offset(addr.0, MSIP, MSIP_WIDTH, width) {
        (self.msip as u64, offset)
      } else if let Some(offset) =
        Self::register_offset(addr.0, MTIMECMP, MTIMECMP_WIDTH, width)
      {
        (self.mtimecmp, offset)
      } else if let Some(offset) = Self::register_offset(addr.0, MTIME, MTIME_WIDTH, width) {
        let sampled_now = self.clock.now();
        now = Some(sampled_now);
        (self.mtime_at(sampled_now), offset)
      } else {
        return Err(Exception::StoreAccessFault(addr));
      };

    // Calculate the new value of the target register based on `size` and `offset`.
    match width {
      1 => {
        // Clear the target byte.
        old = old & (!(0xff << (offset * 8)));
        // Set the new `value` to the target byte.
        old = old | ((value & 0xff) << (offset * 8));
      }
      2 => {
        old = old & (!(0xffff << (offset * 8)));
        old = old | ((value & 0xffff) << (offset * 8));
      }
      4 => {
        old = old & (!(0xffffffff << (offset * 8)));
        old = old | ((value & 0xffffffff) << (offset * 8));
      }
      8 => {
        old = value;
      }
      _ => return Err(Exception::StoreAccessFault(addr)),
    }

    // Store the new value to the target register.
    if Self::register_offset(addr.0, MSIP, MSIP_WIDTH, width).is_some() {
      self.msip = old as u32;
    } else if Self::register_offset(addr.0, MTIMECMP, MTIMECMP_WIDTH, width).is_some() {
      self.mtimecmp = old;
    } else if Self::register_offset(addr.0, MTIME, MTIME_WIDTH, width).is_some() {
      self.host_anchor = now.expect("MTIME writes sample the clock");
      self.guest_anchor = old;
    } else {
      return Err(Exception::StoreAccessFault(addr));
    }
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use std::sync::atomic::{AtomicU64, Ordering};

  use super::*;

  #[derive(Default)]
  struct ManualClock {
    nanos: AtomicU64,
    calls: AtomicU64,
  }

  impl ManualClock {
    fn set_nanos(&self, nanos: u64) {
      self.nanos.store(nanos, Ordering::Relaxed);
    }

    fn call_count(&self) -> u64 {
      self.calls.load(Ordering::Relaxed)
    }
  }

  impl ClockSource for ManualClock {
    fn now(&self) -> Duration {
      self.calls.fetch_add(1, Ordering::Relaxed);
      Duration::from_nanos(self.nanos.load(Ordering::Relaxed))
    }
  }

  #[test]
  fn host_elapsed_time_drives_mtime_at_ten_megahertz() {
    let clock = Arc::new(ManualClock::default());
    let mut clint = Clint::new_with_clock(clock.clone());
    clint.write::<u64>(VirtAddr(MTIMECMP), 10_000_000).unwrap();

    clock.set_nanos(99);
    assert_eq!(clint.mtime(), 0);

    clock.set_nanos(100);
    assert_eq!(clint.mtime(), 1);

    clock.set_nanos(1_000_000_000);
    assert_eq!(clint.mtime(), 10_000_000);
    assert_eq!(clint.duration_until_timer(), None);
    assert_ne!(clint.local_pending_bits() & MTIP_MASK, 0);
  }

  #[test]
  fn fixed_timebase_conversions_are_exact_and_preserve_wrapping_ticks() {
    let duration = Duration::new(123, 456_789_123);
    assert_eq!(Clint::ticks_for_duration(duration), 1_234_567_891);
    assert_eq!(
      Clint::duration_for_ticks_ceil(12_345_678),
      Duration::new(1, 234_567_800),
    );

    let max_duration = Clint::duration_for_ticks_ceil(u64::MAX);
    assert_eq!(Clint::ticks_for_duration(max_duration), u64::MAX);
  }

  #[test]
  fn only_mtime_writes_sample_the_realtime_clock() {
    let clock = Arc::new(ManualClock::default());
    let mut clint = Clint::new_with_clock(clock.clone());
    assert_eq!(clock.call_count(), 1);

    clint.write::<u32>(VirtAddr(MSIP), 1).unwrap();
    clint.write::<u64>(VirtAddr(MTIMECMP), 42).unwrap();
    assert!(matches!(
      clint.write::<u64>(VirtAddr(MTIME + MTIME_WIDTH as u64), 0),
      Err(Exception::StoreAccessFault(_)),
    ));
    assert_eq!(clock.call_count(), 1);

    clint.write::<u64>(VirtAddr(MTIME), 7).unwrap();
    assert_eq!(clock.call_count(), 2);

    clint.write::<u16>(VirtAddr(MTIME + 2), 0).unwrap();
    assert_eq!(clock.call_count(), 3);
  }

  #[test]
  fn mtime_write_reanchors_the_realtime_counter() {
    let clock = Arc::new(ManualClock::default());
    let mut clint = Clint::new_with_clock(clock.clone());

    clock.set_nanos(1_000_000);
    clint.write::<u64>(VirtAddr(MTIME), 42).unwrap();
    assert_eq!(clint.mtime(), 42);

    clock.set_nanos(1_001_000);
    assert_eq!(clint.mtime(), 52);
    assert_eq!(clint.read::<u64>(VirtAddr(MTIME)), Ok(52));
  }

  #[test]
  fn local_pending_tracks_mtip_as_a_level_when_compare_is_reprogrammed() {
    let clock = Arc::new(ManualClock::default());
    let mut clint = Clint::new_with_clock(clock.clone());
    clock.set_nanos(400);
    let now = clint.mtime();
    clint
      .write::<u64>(VirtAddr(MTIMECMP), now + 1)
      .unwrap();

    assert_eq!(clint.mtime(), 4);
    assert_eq!(clint.duration_until_timer(), Some(Duration::from_nanos(100)));
    assert_eq!(clint.local_pending_bits() & MTIP_MASK, 0);

    clock.set_nanos(500);
    assert_ne!(clint.local_pending_bits() & MTIP_MASK, 0);

    clint.write::<u64>(VirtAddr(MTIMECMP), 10).unwrap();
    assert_eq!(clint.local_pending_bits() & MTIP_MASK, 0);
  }

  #[test]
  fn partial_mtime_writes_merge_little_endian_and_reanchor() {
    let clock = Arc::new(ManualClock::default());
    let mut clint = Clint::new_with_clock(clock.clone());
    clint.write::<u64>(VirtAddr(MTIME), 0x1122_3344_5566_7788).unwrap();

    clint.write::<u16>(VirtAddr(MTIME + 2), 0xaabb).unwrap();
    assert_eq!(clint.mtime(), 0x1122_3344_aabb_7788);

    clock.set_nanos(100);
    assert_eq!(clint.mtime(), 0x1122_3344_aabb_7789);
  }

  #[test]
  fn register_accesses_must_fit_inside_one_clint_register() {
    let mut clint = Clint::new_with_clock(Arc::new(ManualClock::default()));

    assert!(matches!(
      clint.read::<u8>(VirtAddr(MTIME + 8)),
      Err(Exception::LoadAccessFault(_)),
    ));
    assert!(matches!(
      clint.read::<u64>(VirtAddr(MTIME + 1)),
      Err(Exception::LoadAccessFault(_)),
    ));
    assert!(matches!(
      clint.write::<u32>(VirtAddr(MSIP + 1), 0),
      Err(Exception::StoreAccessFault(_)),
    ));
  }
}
