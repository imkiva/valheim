use std::ffi::OsString;
use std::fs::{self, File};
use std::io::{self, ErrorKind, Read, Write};
use std::net::Ipv4Addr;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd, RawFd};
use std::os::unix::fs::FileExt;
use std::os::unix::net::UnixStream;
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{self, Receiver, SyncSender, TryRecvError, TrySendError};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use valheim_core::device::virtio::{EthernetBackend, EthernetBackendFactory, NetworkWake};

use crate::NetworkConfig;

const ETHERNET_HEADER_LEN: usize = 14;
const MAX_STREAM_FRAME_LEN: usize = u16::MAX as usize;
const TX_QUEUE_CAPACITY: usize = 256;
const RX_QUEUE_CAPACITY: usize = 256;
const PASST_STARTUP_TIMEOUT: Duration = Duration::from_secs(5);
const PASST_READINESS_POLL_INTERVAL: Duration = Duration::from_millis(10);
const MAX_READINESS_BYTES: usize = 32;
const MIN_INHERITED_FD: RawFd = 3;
const HOST_RESOLV_CONF_PATH: &str = "/etc/resolv.conf";

/// Factory for the Linux passt user-mode networking backend.
///
/// passt is supplied by the caller so the CLI/demo can pin and verify its executable independently
/// from this library crate.
#[derive(Clone, Debug)]
pub struct PasstBackendFactory {
  executable: PathBuf,
  config: NetworkConfig,
  resolv_conf_path: PathBuf,
}

impl PasstBackendFactory {
  pub fn new(executable: impl Into<PathBuf>, config: NetworkConfig) -> Self {
    Self {
      executable: executable.into(),
      config,
      resolv_conf_path: HOST_RESOLV_CONF_PATH.into(),
    }
  }

  pub fn executable(&self) -> &Path {
    &self.executable
  }

  pub fn config(&self) -> NetworkConfig {
    self.config
  }

  #[cfg(test)]
  fn with_resolv_conf_path(mut self, path: impl Into<PathBuf>) -> Self {
    self.resolv_conf_path = path.into();
    self
  }

  fn command(&self, inherited_fd: RawFd, readiness_path: &str, host_dns: Ipv4Addr) -> Command {
    let mut command = Command::new(&self.executable);
    command
      .args(command_arguments(
        self.config,
        inherited_fd,
        readiness_path,
        host_dns,
      ))
      .stdin(Stdio::null())
      .stdout(Stdio::null())
      .stderr(Stdio::inherit());

    // UnixStream::pair() creates close-on-exec descriptors. Clear that bit in the child after fork,
    // avoiding a process-wide window where an unrelated concurrent Command could inherit it.
    unsafe {
      command.pre_exec(move || {
        let flags = libc::fcntl(inherited_fd, libc::F_GETFD);
        if flags == -1 {
          return Err(io::Error::last_os_error());
        }
        if libc::fcntl(inherited_fd, libc::F_SETFD, flags & !libc::FD_CLOEXEC) == -1 {
          return Err(io::Error::last_os_error());
        }
        Ok(())
      });
    }
    command
  }

  fn spawn(&self, wake: NetworkWake) -> io::Result<PasstBackend> {
    self.spawn_with_timeout(wake, PASST_STARTUP_TIMEOUT)
  }

  fn spawn_with_timeout(
    &self,
    wake: NetworkWake,
    startup_timeout: Duration,
  ) -> io::Result<PasstBackend> {
    let host_dns = discover_host_dns(&self.resolv_conf_path)?;
    self.spawn_with_host_dns(wake, startup_timeout, host_dns)
  }

  fn spawn_with_host_dns(
    &self,
    wake: NetworkWake,
    startup_timeout: Duration,
    host_dns: Ipv4Addr,
  ) -> io::Result<PasstBackend> {
    let (host_socket, passt_socket) = UnixStream::pair()?;
    let inherited_socket = duplicate_inherited_fd(passt_socket.as_raw_fd())?;
    drop(passt_socket);

    let readiness_file = create_readiness_file()?;
    let readiness_path = format!(
      "/proc/{}/fd/{}",
      std::process::id(),
      readiness_file.as_raw_fd()
    );

    let inherited_socket_fd = inherited_socket.as_raw_fd();
    let mut child = match self
      .command(inherited_socket_fd, &readiness_path, host_dns)
      .spawn()
    {
      Ok(child) => child,
      Err(error) => return Err(error),
    };

    drop(inherited_socket);

    if let Err(error) = wait_for_passt_readiness(&mut child, &readiness_file, startup_timeout) {
      return Err(cleanup_failed_start(&mut child, error));
    }

    let mut backend = match PasstBackend::from_stream(host_socket, None, wake) {
      Ok(backend) => backend,
      Err(error) => {
        return Err(cleanup_failed_start(&mut child, error));
      }
    };
    match child.try_wait() {
      Ok(Some(status)) => {
        let _ = backend.shutdown_backend();
        return Err(cleanup_failed_start(
          &mut child,
          io::Error::new(
            ErrorKind::Other,
            format!("passt exited during startup with {status}"),
          ),
        ));
      }
      Ok(None) => {}
      Err(error) => {
        let _ = backend.shutdown_backend();
        return Err(cleanup_failed_start(&mut child, error));
      }
    }
    backend.child = Some(child);
    Ok(backend)
  }
}

impl EthernetBackendFactory for PasstBackendFactory {
  fn start(self: Box<Self>, wake: NetworkWake) -> io::Result<Box<dyn EthernetBackend>> {
    Ok(Box::new(self.spawn(wake)?))
  }
}

fn command_arguments(
  config: NetworkConfig,
  inherited_fd: RawFd,
  readiness_path: &str,
  host_dns: Ipv4Addr,
) -> Vec<OsString> {
  let guest_cidr = format!("{}/{}", config.guest(), config.subnet().prefix_len());
  vec![
    "--foreground".into(),
    "--quiet".into(),
    "--ipv4-only".into(),
    "--fd".into(),
    inherited_fd.to_string().into(),
    "--pid".into(),
    readiness_path.into(),
    "--one-off".into(),
    "--address".into(),
    guest_cidr.into(),
    "--gateway".into(),
    config.gateway().to_string().into(),
    "--dns".into(),
    config.dns().to_string().into(),
    "--dns-forward".into(),
    config.dns().to_string().into(),
    "--dns-host".into(),
    host_dns.to_string().into(),
    "--mtu".into(),
    config.mtu().to_string().into(),
    "--tcp-ports".into(),
    "none".into(),
    "--udp-ports".into(),
    "none".into(),
    "--no-map-gw".into(),
  ]
}

fn discover_host_dns(resolv_conf_path: &Path) -> io::Result<Ipv4Addr> {
  let contents = fs::read_to_string(resolv_conf_path).map_err(|error| {
    io::Error::new(
      error.kind(),
      format!(
        "cannot determine host DNS for passt: failed to read {}: {error}",
        resolv_conf_path.display()
      ),
    )
  })?;
  first_usable_ipv4_nameserver(&contents).ok_or_else(|| {
    io::Error::new(
      ErrorKind::InvalidData,
      format!(
        "cannot start passt NAT: {} contains no usable IPv4 nameserver",
        resolv_conf_path.display()
      ),
    )
  })
}

fn first_usable_ipv4_nameserver(contents: &str) -> Option<Ipv4Addr> {
  contents.lines().find_map(|line| {
    let mut fields = line.split_whitespace();
    if fields.next()? != "nameserver" {
      return None;
    }
    let address = fields.next()?.parse::<Ipv4Addr>().ok()?;
    if address.is_unspecified()
      || address.is_multicast()
      || address == Ipv4Addr::new(255, 255, 255, 255)
    {
      None
    } else {
      Some(address)
    }
  })
}

fn duplicate_inherited_fd(fd: RawFd) -> io::Result<OwnedFd> {
  let duplicated = unsafe { libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, MIN_INHERITED_FD) };
  if duplicated == -1 {
    return Err(io::Error::last_os_error());
  }
  // SAFETY: F_DUPFD_CLOEXEC returned a new descriptor owned by this process.
  Ok(unsafe { OwnedFd::from_raw_fd(duplicated) })
}

fn create_readiness_file() -> io::Result<File> {
  const NAME: &[u8] = b"valheim-passt-readiness\0";
  let fd = unsafe { libc::memfd_create(NAME.as_ptr() as *const libc::c_char, libc::MFD_CLOEXEC) };
  if fd == -1 {
    return Err(io::Error::last_os_error());
  }
  // SAFETY: memfd_create returned a new descriptor owned by this process.
  Ok(unsafe { File::from_raw_fd(fd) })
}

fn wait_for_passt_readiness(
  child: &mut Child,
  readiness_file: &File,
  timeout: Duration,
) -> io::Result<()> {
  let deadline = Instant::now().checked_add(timeout).ok_or_else(|| {
    io::Error::new(
      ErrorKind::InvalidInput,
      "passt startup timeout is too large",
    )
  })?;

  loop {
    if let Some(status) = child.try_wait()? {
      return Err(io::Error::new(
        ErrorKind::Other,
        format!("passt exited during startup with {status}"),
      ));
    }

    let mut readiness = [0_u8; MAX_READINESS_BYTES];
    let count = match readiness_file.read_at(&mut readiness, 0) {
      Ok(count) => count,
      Err(error) if error.kind() == ErrorKind::Interrupted => continue,
      Err(error) => return Err(error),
    };
    if let Some(newline) = readiness[..count].iter().position(|byte| *byte == b'\n') {
      return validate_readiness_pid(child, &readiness[..newline]);
    }
    if count == MAX_READINESS_BYTES {
      return Err(io::Error::new(
        ErrorKind::InvalidData,
        "passt wrote an invalid readiness PID",
      ));
    }

    let now = Instant::now();
    if now >= deadline {
      return Err(io::Error::new(
        ErrorKind::TimedOut,
        format!(
          "passt did not become ready within {} ms",
          timeout.as_millis()
        ),
      ));
    }
    thread::sleep(std::cmp::min(
      deadline.duration_since(now),
      PASST_READINESS_POLL_INTERVAL,
    ));
  }
}

fn validate_readiness_pid(child: &mut Child, bytes: &[u8]) -> io::Result<()> {
  let pid = std::str::from_utf8(bytes)
    .ok()
    .and_then(|value| value.parse::<u32>().ok())
    .ok_or_else(|| {
      io::Error::new(
        ErrorKind::InvalidData,
        "passt wrote an invalid readiness PID",
      )
    })?;
  if pid != child.id() {
    return Err(io::Error::new(
      ErrorKind::InvalidData,
      format!(
        "passt readiness PID {pid} did not match child PID {}",
        child.id()
      ),
    ));
  }
  if let Some(status) = child.try_wait()? {
    return Err(io::Error::new(
      ErrorKind::Other,
      format!("passt exited during startup with {status}"),
    ));
  }
  Ok(())
}

fn cleanup_failed_start(child: &mut Child, error: io::Error) -> io::Error {
  match terminate_and_reap(child) {
    Ok(()) => error,
    Err(cleanup_error) => io::Error::new(
      error.kind(),
      format!("{error}; also failed to terminate passt: {cleanup_error}"),
    ),
  }
}

fn terminate_and_reap(child: &mut Child) -> io::Result<()> {
  let mut first_error = None;
  let already_exited = match child.try_wait() {
    Ok(status) => status.is_some(),
    Err(error) => {
      first_error = Some(error);
      false
    }
  };
  if !already_exited {
    if let Err(error) = child.kill() {
      if error.kind() != ErrorKind::InvalidInput && first_error.is_none() {
        first_error = Some(error);
      }
    }
  }
  if let Err(error) = child.wait() {
    if first_error.is_none() {
      first_error = Some(error);
    }
  }
  match first_error {
    Some(error) => Err(error),
    None => Ok(()),
  }
}

#[derive(Clone, Debug)]
struct WorkerFailure {
  kind: ErrorKind,
  message: String,
}

impl WorkerFailure {
  fn into_error(self) -> io::Error {
    io::Error::new(self.kind, self.message)
  }
}

type SharedFailure = Arc<Mutex<Option<WorkerFailure>>>;

fn publish_failure(failure: &SharedFailure, error: io::Error, wake: &NetworkWake) {
  let mut slot = failure.lock().expect("cannot lock passt worker failure");
  if slot.is_none() {
    *slot = Some(WorkerFailure {
      kind: error.kind(),
      message: error.to_string(),
    });
    drop(slot);
    wake();
  }
}

struct PasstBackend {
  tx_sender: Option<SyncSender<Vec<u8>>>,
  rx_receiver: Receiver<Vec<u8>>,
  control_socket: UnixStream,
  child: Option<Child>,
  reader: Option<JoinHandle<()>>,
  writer: Option<JoinHandle<()>>,
  failure: SharedFailure,
  stopping: Arc<AtomicBool>,
  stopped: bool,
}

impl PasstBackend {
  fn from_stream(
    control_socket: UnixStream,
    child: Option<Child>,
    wake: NetworkWake,
  ) -> io::Result<Self> {
    let reader_socket = control_socket.try_clone()?;
    let writer_socket = control_socket.try_clone()?;
    let (tx_sender, tx_receiver) = mpsc::sync_channel(TX_QUEUE_CAPACITY);
    let (rx_sender, rx_receiver) = mpsc::sync_channel(RX_QUEUE_CAPACITY);
    let failure = Arc::new(Mutex::new(None));
    let stopping = Arc::new(AtomicBool::new(false));

    let reader_failure = failure.clone();
    let reader_stopping = stopping.clone();
    let reader_wake = wake.clone();
    let reader = thread::Builder::new()
      .name("valheim-passt-rx".into())
      .spawn(move || {
        read_frames(
          reader_socket,
          rx_sender,
          reader_stopping,
          reader_failure,
          reader_wake,
        )
      })?;

    let writer_failure = failure.clone();
    let writer_stopping = stopping.clone();
    let writer_wake = wake;
    let writer = match thread::Builder::new()
      .name("valheim-passt-tx".into())
      .spawn(move || {
        write_frames(
          writer_socket,
          tx_receiver,
          writer_stopping,
          writer_failure,
          writer_wake,
        )
      }) {
      Ok(writer) => writer,
      Err(error) => {
        stopping.store(true, Ordering::Release);
        let _ = control_socket.shutdown(std::net::Shutdown::Both);
        let _ = reader.join();
        return Err(error);
      }
    };

    Ok(Self {
      tx_sender: Some(tx_sender),
      rx_receiver,
      control_socket,
      child,
      reader: Some(reader),
      writer: Some(writer),
      failure,
      stopping,
      stopped: false,
    })
  }

  fn current_failure(&self) -> Option<io::Error> {
    self
      .failure
      .lock()
      .expect("cannot lock passt worker failure")
      .clone()
      .map(WorkerFailure::into_error)
  }

  fn try_send_frame(&mut self, frame: &[u8]) -> io::Result<bool> {
    validate_frame_len(frame.len())?;
    if let Some(error) = self.current_failure() {
      return Err(error);
    }
    let sender = self
      .tx_sender
      .as_ref()
      .ok_or_else(|| io::Error::new(ErrorKind::BrokenPipe, "passt backend has been shut down"))?;
    match sender.try_send(frame.to_vec()) {
      Ok(()) => Ok(true),
      Err(TrySendError::Full(_)) => Ok(false),
      Err(TrySendError::Disconnected(_)) => {
        Err(self.current_failure().unwrap_or_else(|| {
          io::Error::new(ErrorKind::BrokenPipe, "passt transmit worker stopped")
        }))
      }
    }
  }

  fn try_recv_frame(&mut self) -> io::Result<Option<Vec<u8>>> {
    match self.rx_receiver.try_recv() {
      Ok(frame) => Ok(Some(frame)),
      Err(TryRecvError::Empty) => match self.current_failure() {
        Some(error) => Err(error),
        None => Ok(None),
      },
      Err(TryRecvError::Disconnected) => Err(
        self
          .current_failure()
          .unwrap_or_else(|| io::Error::new(ErrorKind::BrokenPipe, "passt receive worker stopped")),
      ),
    }
  }

  fn shutdown_backend(&mut self) -> io::Result<()> {
    if self.stopped {
      return Ok(());
    }
    self.stopped = true;
    self.stopping.store(true, Ordering::Release);
    self.tx_sender.take();

    let mut first_error = None;
    if let Err(error) = self.control_socket.shutdown(std::net::Shutdown::Both) {
      if error.kind() != ErrorKind::NotConnected {
        first_error = Some(error);
      }
    }

    if let Some(child) = self.child.as_mut() {
      match child.try_wait() {
        Ok(Some(_)) => {}
        Ok(None) => {
          if let Err(error) = child.kill() {
            if error.kind() != ErrorKind::InvalidInput && first_error.is_none() {
              first_error = Some(error);
            }
          }
        }
        Err(error) if first_error.is_none() => first_error = Some(error),
        Err(_) => {}
      }
      if let Err(error) = child.wait() {
        if first_error.is_none() {
          first_error = Some(error);
        }
      }
    }
    self.child.take();

    if let Some(writer) = self.writer.take() {
      if writer.join().is_err() && first_error.is_none() {
        first_error = Some(io::Error::new(
          ErrorKind::Other,
          "passt transmit worker panicked",
        ));
      }
    }
    if let Some(reader) = self.reader.take() {
      if reader.join().is_err() && first_error.is_none() {
        first_error = Some(io::Error::new(
          ErrorKind::Other,
          "passt receive worker panicked",
        ));
      }
    }

    match first_error {
      Some(error) => Err(error),
      None => Ok(()),
    }
  }
}

impl Drop for PasstBackend {
  fn drop(&mut self) {
    let _ = self.shutdown_backend();
  }
}

impl EthernetBackend for PasstBackend {
  fn try_send(&mut self, frame: &[u8]) -> io::Result<bool> {
    self.try_send_frame(frame)
  }

  fn try_recv(&mut self) -> io::Result<Option<Vec<u8>>> {
    self.try_recv_frame()
  }

  fn shutdown(&mut self) -> io::Result<()> {
    self.shutdown_backend()
  }
}

fn read_frames(
  mut socket: UnixStream,
  receiver: SyncSender<Vec<u8>>,
  stopping: Arc<AtomicBool>,
  failure: SharedFailure,
  wake: NetworkWake,
) {
  loop {
    match read_stream_frame(&mut socket) {
      Ok(Some(frame)) => match receiver.try_send(frame) {
        Ok(()) => wake(),
        Err(TrySendError::Full(_)) => {}
        Err(TrySendError::Disconnected(_)) => break,
      },
      Ok(None) => {
        if !stopping.load(Ordering::Acquire) {
          publish_failure(
            &failure,
            io::Error::new(ErrorKind::UnexpectedEof, "passt closed the frame stream"),
            &wake,
          );
        }
        break;
      }
      Err(error) => {
        if !stopping.load(Ordering::Acquire) {
          publish_failure(&failure, error, &wake);
        }
        break;
      }
    }
  }
}

fn write_frames(
  mut socket: UnixStream,
  sender: Receiver<Vec<u8>>,
  stopping: Arc<AtomicBool>,
  failure: SharedFailure,
  wake: NetworkWake,
) {
  while let Ok(frame) = sender.recv() {
    if stopping.load(Ordering::Acquire) {
      break;
    }
    if let Err(error) = write_stream_frame(&mut socket, &frame) {
      if !stopping.load(Ordering::Acquire) {
        publish_failure(&failure, error, &wake);
      }
      break;
    }
  }
  let _ = socket.shutdown(std::net::Shutdown::Write);
}

fn validate_frame_len(length: usize) -> io::Result<()> {
  if !(ETHERNET_HEADER_LEN..=MAX_STREAM_FRAME_LEN).contains(&length) {
    return Err(io::Error::new(
      ErrorKind::InvalidData,
      format!(
        "invalid Ethernet frame length {length}; expected {ETHERNET_HEADER_LEN}..={MAX_STREAM_FRAME_LEN}"
      ),
    ));
  }
  Ok(())
}

/// Read QEMU stream framing: a 32-bit network-byte-order length followed by one Ethernet frame.
fn read_stream_frame(reader: &mut impl Read) -> io::Result<Option<Vec<u8>>> {
  let mut length_bytes = [0u8; 4];
  let mut read = 0;
  while read < length_bytes.len() {
    match reader.read(&mut length_bytes[read..]) {
      Ok(0) if read == 0 => return Ok(None),
      Ok(0) => {
        return Err(io::Error::new(
          ErrorKind::UnexpectedEof,
          "partial QEMU stream frame length",
        ))
      }
      Ok(count) => read += count,
      Err(error) if error.kind() == ErrorKind::Interrupted => {}
      Err(error) => return Err(error),
    }
  }

  let length = u32::from_be_bytes(length_bytes) as usize;
  validate_frame_len(length)?;
  let mut frame = vec![0; length];
  reader.read_exact(&mut frame)?;
  Ok(Some(frame))
}

fn write_stream_frame(writer: &mut impl Write, frame: &[u8]) -> io::Result<()> {
  validate_frame_len(frame.len())?;
  writer.write_all(&(frame.len() as u32).to_be_bytes())?;
  writer.write_all(frame)
}

#[cfg(test)]
mod tests {
  use std::fs::{self, OpenOptions};
  use std::io::Cursor;
  use std::os::unix::fs::{FileExt, PermissionsExt};
  use std::path::PathBuf;
  use std::sync::atomic::{AtomicU64, Ordering as AtomicOrdering};
  use std::sync::mpsc;
  use std::time::{Duration, Instant};

  use super::*;

  static NEXT_FAKE_HELPER: AtomicU64 = AtomicU64::new(0);
  const TEST_HOST_DNS: Ipv4Addr = Ipv4Addr::new(192, 168, 50, 3);

  struct FakePasstHelper {
    path: PathBuf,
  }

  impl FakePasstHelper {
    fn new(body: &str) -> Self {
      let serial = NEXT_FAKE_HELPER.fetch_add(1, AtomicOrdering::Relaxed);
      let path = std::env::temp_dir().join(format!(
        "valheim-fake-passt-{}-{serial}.sh",
        std::process::id()
      ));
      let script = format!(
        r#"#!/bin/sh
pidfile=
socket_fd=
while [ "$#" -gt 0 ]; do
  case "$1" in
    --pid)
      [ "$#" -ge 2 ] || exit 90
      pidfile="$2"
      shift 2
      ;;
    --fd)
      [ "$#" -ge 2 ] || exit 90
      socket_fd="$2"
      shift 2
      ;;
    *)
      shift
      ;;
  esac
done
[ -n "$pidfile" ] && [ -n "$socket_fd" ] || exit 90
case "$socket_fd" in 0|1|2) exit 91 ;; esac
case "$pidfile" in /proc/[0-9]*/fd/*) ;; *) exit 92 ;; esac
{body}
"#
      );
      fs::write(&path, script).unwrap();
      let mut permissions = fs::metadata(&path).unwrap().permissions();
      permissions.set_mode(0o755);
      fs::set_permissions(&path, permissions).unwrap();
      Self { path }
    }

    fn path(&self) -> &Path {
      &self.path
    }

    fn pid_capture_path(&self) -> PathBuf {
      PathBuf::from(format!("{}.pid", self.path.display()))
    }
  }

  impl Drop for FakePasstHelper {
    fn drop(&mut self) {
      let _ = fs::remove_file(&self.path);
      let _ = fs::remove_file(self.pid_capture_path());
    }
  }

  fn frame(fill: u8, length: usize) -> Vec<u8> {
    vec![fill; length]
  }

  #[derive(Default)]
  struct ChunkedWriter {
    bytes: Vec<u8>,
    max_write: usize,
  }

  impl Write for ChunkedWriter {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
      let count = bytes.len().min(self.max_write);
      self.bytes.extend_from_slice(&bytes[..count]);
      Ok(count)
    }

    fn flush(&mut self) -> io::Result<()> {
      Ok(())
    }
  }

  struct ChunkedReader<R> {
    inner: R,
    max_read: usize,
  }

  impl<R: Read> Read for ChunkedReader<R> {
    fn read(&mut self, bytes: &mut [u8]) -> io::Result<usize> {
      let count = bytes.len().min(self.max_read);
      self.inner.read(&mut bytes[..count])
    }
  }

  #[test]
  fn passt_arguments_fix_ipv4_outbound_only_policy() {
    let arguments = command_arguments(
      NetworkConfig::default(),
      47,
      "/proc/123/fd/48",
      TEST_HOST_DNS,
    )
    .into_iter()
    .map(|argument| argument.into_string().unwrap())
    .collect::<Vec<_>>();
    assert_eq!(
      arguments,
      vec![
        "--foreground",
        "--quiet",
        "--ipv4-only",
        "--fd",
        "47",
        "--pid",
        "/proc/123/fd/48",
        "--one-off",
        "--address",
        "10.172.0.15/16",
        "--gateway",
        "10.172.0.2",
        "--dns",
        "10.172.0.3",
        "--dns-forward",
        "10.172.0.3",
        "--dns-host",
        "192.168.50.3",
        "--mtu",
        "1500",
        "--tcp-ports",
        "none",
        "--udp-ports",
        "none",
        "--no-map-gw",
      ]
    );
  }

  #[test]
  fn resolv_conf_selects_first_usable_ipv4_nameserver() {
    let contents = r#"
# Generated by a resolver manager
options edns0 trust-ad
nameserver ::1
nameserver not-an-address
nameserver 0.0.0.0
nameserver 224.0.0.251
nameserver 255.255.255.255
nameserver 127.0.0.53
nameserver 192.168.50.3
"#;
    assert_eq!(
      first_usable_ipv4_nameserver(contents),
      Some(Ipv4Addr::new(127, 0, 0, 53))
    );
  }

  #[test]
  fn discover_host_dns_reads_selected_resolv_conf() {
    let serial = NEXT_FAKE_HELPER.fetch_add(1, AtomicOrdering::Relaxed);
    let path = std::env::temp_dir().join(format!(
      "valheim-test-resolv-conf-{}-{serial}",
      std::process::id()
    ));
    fs::write(
      &path,
      format!("nameserver 2001:db8::1\nnameserver {TEST_HOST_DNS}\n"),
    )
    .unwrap();
    assert_eq!(discover_host_dns(&path).unwrap(), TEST_HOST_DNS);
    fs::remove_file(path).unwrap();
  }

  #[test]
  fn startup_reports_missing_host_resolver_configuration() {
    let serial = NEXT_FAKE_HELPER.fetch_add(1, AtomicOrdering::Relaxed);
    let missing = std::env::temp_dir().join(format!(
      "valheim-missing-resolv-conf-{}-{serial}",
      std::process::id()
    ));
    let _ = fs::remove_file(&missing);
    let factory = PasstBackendFactory::new(
      "/definitely/missing/valheim-passt-test",
      NetworkConfig::default(),
    )
    .with_resolv_conf_path(&missing);
    let error = match factory.spawn(Arc::new(|| {})) {
      Ok(_) => panic!("passt unexpectedly started without host DNS configuration"),
      Err(error) => error,
    };
    assert_eq!(error.kind(), ErrorKind::NotFound);
    assert!(error
      .to_string()
      .contains("cannot determine host DNS for passt"));
    assert!(error.to_string().contains(&missing.display().to_string()));
  }

  #[test]
  fn startup_rejects_resolv_conf_without_usable_ipv4_nameserver() {
    let serial = NEXT_FAKE_HELPER.fetch_add(1, AtomicOrdering::Relaxed);
    let path = std::env::temp_dir().join(format!(
      "valheim-unusable-resolv-conf-{}-{serial}",
      std::process::id()
    ));
    fs::write(&path, "nameserver ::1\nnameserver 0.0.0.0\n").unwrap();
    let factory = PasstBackendFactory::new(
      "/definitely/missing/valheim-passt-test",
      NetworkConfig::default(),
    )
    .with_resolv_conf_path(&path);
    let error = match factory.spawn(Arc::new(|| {})) {
      Ok(_) => panic!("passt unexpectedly started without a usable IPv4 resolver"),
      Err(error) => error,
    };
    fs::remove_file(&path).unwrap();
    assert_eq!(error.kind(), ErrorKind::InvalidData);
    assert!(error
      .to_string()
      .contains("contains no usable IPv4 nameserver"));
    assert!(error.to_string().contains(&path.display().to_string()));
  }

  #[test]
  fn inherited_socket_descriptor_is_duplicated_above_stdio() {
    let (stream, _peer) = UnixStream::pair().unwrap();
    let inherited = duplicate_inherited_fd(stream.as_raw_fd()).unwrap();
    assert!(inherited.as_raw_fd() >= MIN_INHERITED_FD);
  }

  #[test]
  fn readiness_memfd_supports_passt_output_file_open_flags() {
    let readiness = create_readiness_file().unwrap();
    let flags = unsafe { libc::fcntl(readiness.as_raw_fd(), libc::F_GETFD) };
    assert_ne!(flags & libc::FD_CLOEXEC, 0);

    let path = format!("/proc/{}/fd/{}", std::process::id(), readiness.as_raw_fd());
    let mut writer = OpenOptions::new()
      .create(true)
      .truncate(true)
      .write(true)
      .open(path)
      .unwrap();
    writer.write_all(b"12345\n").unwrap();
    drop(writer);

    let mut bytes = [0_u8; 16];
    let count = readiness.read_at(&mut bytes, 0).unwrap();
    assert_eq!(&bytes[..count], b"12345\n");
  }

  #[test]
  fn startup_waits_for_fake_helper_readiness() {
    let helper = FakePasstHelper::new(
      r#"printf '%s\n' "$$" > "$pidfile"
kill -STOP "$$""#,
    );
    let factory = PasstBackendFactory::new(helper.path(), NetworkConfig::default());
    let mut backend = factory
      .spawn_with_host_dns(Arc::new(|| {}), Duration::from_secs(2), TEST_HOST_DNS)
      .unwrap();
    backend.shutdown_backend().unwrap();
  }

  #[test]
  fn startup_reports_helper_exit_before_readiness() {
    let helper = FakePasstHelper::new("exit 42");
    let factory = PasstBackendFactory::new(helper.path(), NetworkConfig::default());
    let started = Instant::now();
    let error =
      match factory.spawn_with_host_dns(Arc::new(|| {}), Duration::from_secs(2), TEST_HOST_DNS) {
        Ok(_) => panic!("failed fake passt unexpectedly became ready"),
        Err(error) => error,
      };
    assert!(started.elapsed() < Duration::from_secs(2));
    assert!(error.to_string().contains("exited during startup"));
  }

  #[test]
  fn startup_rejects_malformed_readiness_pid() {
    let helper = FakePasstHelper::new(
      r#"printf 'not-a-pid\n' > "$pidfile"
kill -STOP "$$""#,
    );
    let factory = PasstBackendFactory::new(helper.path(), NetworkConfig::default());
    let error =
      match factory.spawn_with_host_dns(Arc::new(|| {}), Duration::from_secs(2), TEST_HOST_DNS) {
        Ok(_) => panic!("malformed fake passt readiness was accepted"),
        Err(error) => error,
      };
    assert_eq!(error.kind(), ErrorKind::InvalidData);
  }

  #[test]
  fn startup_rejects_readiness_from_a_different_pid() {
    let helper = FakePasstHelper::new(
      r#"printf '1\n' > "$pidfile"
kill -STOP "$$""#,
    );
    let factory = PasstBackendFactory::new(helper.path(), NetworkConfig::default());
    let error =
      match factory.spawn_with_host_dns(Arc::new(|| {}), Duration::from_secs(2), TEST_HOST_DNS) {
        Ok(_) => panic!("mismatched fake passt readiness was accepted"),
        Err(error) => error,
      };
    assert_eq!(error.kind(), ErrorKind::InvalidData);
    assert!(error.to_string().contains("did not match child PID"));
  }

  #[test]
  fn startup_timeout_kills_and_reaps_helper() {
    let helper = FakePasstHelper::new(
      r#"printf '%s\n' "$$" > "$0.pid"
kill -STOP "$$""#,
    );
    let factory = PasstBackendFactory::new(helper.path(), NetworkConfig::default());
    let error =
      match factory.spawn_with_host_dns(Arc::new(|| {}), Duration::from_secs(1), TEST_HOST_DNS) {
        Ok(_) => panic!("unready fake passt unexpectedly started"),
        Err(error) => error,
      };
    assert_eq!(error.kind(), ErrorKind::TimedOut);

    let pid = fs::read_to_string(helper.pid_capture_path())
      .unwrap()
      .trim()
      .parse::<libc::pid_t>()
      .unwrap();
    assert_eq!(unsafe { libc::kill(pid, 0) }, -1);
    assert_eq!(io::Error::last_os_error().raw_os_error(), Some(libc::ESRCH));
  }

  #[test]
  fn stream_writer_handles_partial_writes_and_uses_network_byte_order() {
    let payload = frame(0xa5, 64);
    let mut writer = ChunkedWriter {
      bytes: Vec::new(),
      max_write: 3,
    };
    write_stream_frame(&mut writer, &payload).unwrap();
    assert_eq!(&writer.bytes[..4], &[0, 0, 0, 64]);
    assert_eq!(&writer.bytes[4..], payload);
  }

  #[test]
  fn stream_reader_handles_partial_reads() {
    let payload = frame(0x33, 60);
    let mut encoded = Vec::new();
    write_stream_frame(&mut encoded, &payload).unwrap();
    let mut reader = ChunkedReader {
      inner: Cursor::new(encoded),
      max_read: 1,
    };
    assert_eq!(read_stream_frame(&mut reader).unwrap(), Some(payload));
    assert_eq!(read_stream_frame(&mut reader).unwrap(), None);
  }

  #[test]
  fn stream_reader_preserves_coalesced_frame_boundaries() {
    let first = frame(1, 60);
    let second = frame(2, 1518);
    let mut encoded = Vec::new();
    write_stream_frame(&mut encoded, &first).unwrap();
    write_stream_frame(&mut encoded, &second).unwrap();
    let mut reader = Cursor::new(encoded);
    assert_eq!(read_stream_frame(&mut reader).unwrap(), Some(first));
    assert_eq!(read_stream_frame(&mut reader).unwrap(), Some(second));
    assert_eq!(read_stream_frame(&mut reader).unwrap(), None);
  }

  #[test]
  fn stream_reader_rejects_partial_and_invalid_frames() {
    let error = read_stream_frame(&mut Cursor::new(vec![0, 0])).unwrap_err();
    assert_eq!(error.kind(), ErrorKind::UnexpectedEof);

    let error = read_stream_frame(&mut Cursor::new(vec![0, 0, 0, 60, 1])).unwrap_err();
    assert_eq!(error.kind(), ErrorKind::UnexpectedEof);

    let error = read_stream_frame(&mut Cursor::new(13u32.to_be_bytes())).unwrap_err();
    assert_eq!(error.kind(), ErrorKind::InvalidData);

    let error = read_stream_frame(&mut Cursor::new(65_536u32.to_be_bytes())).unwrap_err();
    assert_eq!(error.kind(), ErrorKind::InvalidData);
  }

  #[test]
  fn workers_exchange_frames_and_notify_after_rx_publish() {
    let (host, mut peer) = UnixStream::pair().unwrap();
    let (wake_sender, wake_receiver) = mpsc::channel();
    let wake = Arc::new(move || {
      let _ = wake_sender.send(());
    });
    let mut backend = PasstBackend::from_stream(host, None, wake).unwrap();
    let outbound = frame(0x11, 60);
    let inbound = frame(0x22, 128);
    let expected_outbound = outbound.clone();
    let expected_inbound = inbound.clone();
    let peer_thread = thread::spawn(move || {
      assert_eq!(
        read_stream_frame(&mut peer).unwrap(),
        Some(expected_outbound)
      );
      write_stream_frame(&mut peer, &expected_inbound).unwrap();
    });

    assert!(backend.try_send_frame(&outbound).unwrap());
    wake_receiver
      .recv_timeout(Duration::from_secs(1))
      .expect("receive worker did not publish a wake notification");
    assert_eq!(backend.try_recv_frame().unwrap(), Some(inbound));
    backend.shutdown_backend().unwrap();
    backend.shutdown_backend().unwrap();
    peer_thread.join().unwrap();
  }

  #[test]
  fn unexpected_peer_eof_is_reported_and_wakes_consumer() {
    let (host, peer) = UnixStream::pair().unwrap();
    let (wake_sender, wake_receiver) = mpsc::channel();
    let wake = Arc::new(move || {
      let _ = wake_sender.send(());
    });
    let mut backend = PasstBackend::from_stream(host, None, wake).unwrap();
    drop(peer);

    wake_receiver
      .recv_timeout(Duration::from_secs(1))
      .expect("receive worker did not notify on passt EOF");
    assert_eq!(
      backend.try_recv_frame().unwrap_err().kind(),
      ErrorKind::UnexpectedEof
    );
    backend.shutdown_backend().unwrap();
  }

  #[test]
  fn shutdown_terminates_and_reaps_the_owned_child() {
    let (host, _peer) = UnixStream::pair().unwrap();
    let child = Command::new("/bin/sh")
      .args(["-c", "exec sleep 30"])
      .spawn()
      .unwrap();
    let pid = child.id() as libc::pid_t;
    let mut backend = PasstBackend::from_stream(host, Some(child), Arc::new(|| {})).unwrap();

    backend.shutdown_backend().unwrap();
    let result = unsafe { libc::kill(pid, 0) };
    assert_eq!(result, -1);
    assert_eq!(io::Error::last_os_error().raw_os_error(), Some(libc::ESRCH));
  }

  #[test]
  fn missing_passt_executable_fails_synchronously() {
    let factory = PasstBackendFactory::new(
      "/definitely/missing/valheim-passt-test",
      NetworkConfig::default(),
    );
    let error =
      match factory.spawn_with_host_dns(Arc::new(|| {}), PASST_STARTUP_TIMEOUT, TEST_HOST_DNS) {
        Ok(_) => panic!("a missing passt executable unexpectedly started"),
        Err(error) => error,
      };
    assert_eq!(error.kind(), ErrorKind::NotFound);
  }
}
