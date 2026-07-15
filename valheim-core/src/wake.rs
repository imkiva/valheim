use std::sync::{Condvar, Mutex};

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
}
