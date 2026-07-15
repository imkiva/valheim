use std::sync::{Condvar, Mutex};
use std::time::Instant;

/// Coordinates host-side asynchronous device events with a sleeping machine thread.
///
/// A producer must publish its device state before calling [`WakeHub::notify`]. The machine takes
/// a generation snapshot before polling devices and only waits if the generation is still current,
/// so an event racing with the poll-to-wait transition cannot be lost.
pub(crate) struct WakeHub {
  generation: Mutex<u64>,
  changed: Condvar,
}

impl WakeHub {
  pub(crate) fn new() -> Self {
    Self {
      generation: Mutex::new(0),
      changed: Condvar::new(),
    }
  }

  pub(crate) fn snapshot(&self) -> u64 {
    *self.generation.lock().expect("cannot lock wake generation")
  }

  pub(crate) fn notify(&self) {
    let mut generation = self.generation.lock().expect("cannot lock wake generation");
    *generation = generation.wrapping_add(1);
    drop(generation);
    self.changed.notify_one();
  }

  pub(crate) fn wait_for_change(&self, observed: u64) {
    let generation = self.generation.lock().expect("cannot lock wake generation");
    let _generation = self
      .changed
      .wait_while(generation, |generation| *generation == observed)
      .expect("cannot wait on wake generation");
  }

  /// Wait until an asynchronous notification or a host-time deadline. The absolute-deadline loop
  /// tolerates spurious wakeups and scheduler delays; the generation predicate preserves events
  /// published between the dispatcher poll and the actual condvar wait.
  pub(crate) fn wait_for_change_until(&self, observed: u64, deadline: Instant) -> bool {
    let mut generation = self.generation.lock().expect("cannot lock wake generation");
    loop {
      if *generation != observed {
        return true;
      }
      let remaining = match deadline.checked_duration_since(Instant::now()) {
        Some(remaining) if !remaining.is_zero() => remaining,
        _ => return false,
      };
      let (next_generation, _) = self
        .changed
        .wait_timeout(generation, remaining)
        .expect("cannot wait on wake generation");
      generation = next_generation;
    }
  }
}

#[cfg(test)]
mod tests {
  use std::sync::{mpsc, Arc};
  use std::time::Duration;

  use super::*;

  #[test]
  fn notification_between_snapshot_and_wait_is_not_lost() {
    let wake_hub = Arc::new(WakeHub::new());
    let observed = wake_hub.snapshot();
    wake_hub.notify();

    let (sender, receiver) = mpsc::channel();
    let waiter_hub = wake_hub.clone();
    let waiter = std::thread::spawn(move || {
      waiter_hub.wait_for_change(observed);
      sender.send(()).unwrap();
    });

    receiver
      .recv_timeout(Duration::from_secs(1))
      .expect("a notification published before wait was lost");
    waiter.join().unwrap();
  }

  #[test]
  fn timed_wait_reports_notification_before_deadline() {
    let wake_hub = Arc::new(WakeHub::new());
    let observed = wake_hub.snapshot();
    let notifying_hub = wake_hub.clone();
    let notifier = std::thread::spawn(move || {
      std::thread::sleep(Duration::from_millis(10));
      notifying_hub.notify();
    });

    assert!(wake_hub.wait_for_change_until(
      observed,
      Instant::now() + Duration::from_secs(1),
    ));
    notifier.join().unwrap();
  }

  #[test]
  fn timed_wait_does_not_lose_a_notification_published_before_wait() {
    let wake_hub = WakeHub::new();
    let observed = wake_hub.snapshot();
    wake_hub.notify();

    assert!(wake_hub.wait_for_change_until(
      observed,
      Instant::now() + Duration::from_secs(1),
    ));
  }

  #[test]
  fn timed_wait_expires_without_a_notification() {
    let wake_hub = WakeHub::new();
    let observed = wake_hub.snapshot();
    let started = Instant::now();

    assert!(!wake_hub.wait_for_change_until(
      observed,
      Instant::now() + Duration::from_millis(10),
    ));
    assert!(started.elapsed() >= Duration::from_millis(5));
  }

  #[test]
  fn zero_timeout_returns_immediately() {
    let wake_hub = WakeHub::new();
    let observed = wake_hub.snapshot();
    assert!(!wake_hub.wait_for_change_until(observed, Instant::now()));
  }
}
