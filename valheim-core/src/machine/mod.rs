use std::fs::OpenOptions;
use std::sync::Arc;

use memmap2::MmapMut;

use valheim_asm::isa::data::Fin;
use valheim_asm::isa::rv64::CSRAddr;
use valheim_asm::isa::typed::{Imm32, Reg};

use crate::cpu::bus::VIRT_MROM_BASE;
use crate::cpu::csr::CSRMap::{MIE, MTIE_MASK};
use crate::cpu::irq::Exception;
use crate::cpu::RV64Cpu;
use crate::device::ns16550a::Uart16550a;
use crate::dtb::generate_device_tree_rom;
use crate::interp::naive::NaiveInterpreter;
use crate::interp::RV64Executor;
use crate::memory::VirtAddr;

const RV64_PC_RESET: u64 = 0x80000000;
// Linux expects the flattened device tree to remain reachable through the
// normal DRAM linear mapping after it installs the final page tables.
const RV64_DTB_ADDR: u64 = 0x87f00000;
const DEVICE_TREE_ROM_HEADER_SIZE: usize = 32;
const DEFAULT_CMDLINE: &str = "root=/dev/vda ro console=ttyS0";
// Keep asynchronous device interrupts bounded while amortizing Machine/JIT dispatcher work.
// Timer deadlines below this ceiling still shorten the budget exactly.
const MAX_EXECUTOR_BUDGET: u32 = 1024;

pub struct Machine {
  pub cpu: RV64Cpu,
  pub executor: Box<dyn RV64Executor>,
}

macro_rules! csr {
    ($self:expr,$csr:ident) => {$self.cpu.csrs.read(CSRAddr(Imm32::from(crate::cpu::csr::CSRMap::$csr as u32)))};
}

impl Machine {
  pub fn new(cmdline: Option<String>, trace: Option<String>) -> Machine {
    Self::new_with_executor(cmdline, trace, Box::new(NaiveInterpreter::new()))
  }

  pub fn new_with_executor(
    cmdline: Option<String>,
    trace: Option<String>,
    executor: Box<dyn RV64Executor>,
  ) -> Machine {
    let mut machine = Machine {
      cpu: RV64Cpu::new(trace),
      executor,
    };

    let cmdline = cmdline.unwrap_or(DEFAULT_CMDLINE.to_string());
    let memory_size = machine.cpu.bus.mem.memory_size as u64;
    let memory_base = machine.cpu.bus.mem.memory_base.0;
    let device_tree_rom = generate_device_tree_rom(cmdline, memory_base, memory_size)
      .expect("Cannot generate device tree");
    machine.load_device_tree(device_tree_rom.as_slice())
      .expect("Cannot load device tree");
    machine.load_memory(
      RV64_DTB_ADDR as usize,
      &device_tree_rom[DEVICE_TREE_ROM_HEADER_SIZE..],
    );
    let wake_hub = machine.cpu.bus.wake_hub.clone();
    unsafe {
      machine
        .cpu
        .bus
        .add_device(Arc::new(Uart16550a::new(wake_hub)))
        .expect("Cannot install UART device")
    };
    machine
  }

  pub fn run(&mut self) {
    self.cpu.write_pc(VirtAddr(RV64_PC_RESET));
    self.cpu.write_reg(Reg::X(Fin::new(11)), RV64_DTB_ADDR);
    loop {
      let wake_generation = self.prepare_wfi_wait();
      let cont = self.run_next();
      match cont {
        true => self.wait_if_still_idle(wake_generation),
        false => break,
      }
    }
    self.halt();
  }

  pub fn run_next(&mut self) -> bool {
    match self.dispatch_next() {
      Ok(_) => true,
      // TODO: stop treating breakpoint as good trap
      Err(Exception::Breakpoint) => false,
      Err(ex) => {
        // println!("[Valheim] Exception: {:?} at {:#x} in {:?}", ex, self.cpu.read_pc().0, self.cpu.mode);
        // self.show_status();
        // TODO: add watchdog to prevent kernels that do not handle double fault?
        // note: double/triple-fault can be handled by the M-mode program.
        // see: https://github.com/riscv/riscv-isa-manual/issues/3#issuecomment-278495907
        let _ = ex.handle(&mut self.cpu);
        true
      }
    }
  }

  fn dispatch_next(&mut self) -> Result<(), Exception> {
    self.cpu.bus.clint.tick(&mut self.cpu.csrs);

    if let Some(irq) = self.cpu.pending_interrupt() {
      // TODO: can IRQ fail to handle?
      let _ = irq.handle(&mut self.cpu);
    }

    // A waiting hart cannot make guest-visible progress before a locally enabled interrupt. Skip
    // idle dispatches up to the next machine-timer deadline, then use the normal interrupt path so
    // delegation, priority, trap CSRs, and WFI wakeup semantics remain centralized there. Polling
    // pending interrupts both before and after the jump also preserves asynchronous device events.
    if self.cpu.wfi && self.cpu.csrs.read_unchecked(MIE) & MTIE_MASK != 0 {
      if let Some(ticks) = self.cpu.bus.clint.ticks_until_timer() {
        self.cpu.bus.clint.advance(&mut self.cpu.csrs, ticks);
        if let Some(irq) = self.cpu.pending_interrupt() {
          let _ = irq.handle(&mut self.cpu);
        }
      }
    }

    let budget = self
      .cpu
      .bus
      .clint
      .ticks_until_timer()
      .unwrap_or(MAX_EXECUTOR_BUDGET as u64)
      .min(MAX_EXECUTOR_BUDGET as u64) as u32;
    let outcome = self.executor.execute(&mut self.cpu, budget);
    assert!(
      outcome.attempted <= budget,
      "executor attempted {} instructions with a budget of {}",
      outcome.attempted,
      budget,
    );
    self.cpu.bus.clint.advance(
      &mut self.cpu.csrs,
      outcome.attempted.saturating_sub(1) as u64,
    );
    outcome.result
  }

  /// Capture the asynchronous-device generation before the dispatcher polls devices. Waiting is
  /// only useful when WFI has no enabled future timer: enabled timer deadlines retain the existing
  /// immediate guest-time fast-forward path in `dispatch_next`.
  fn prepare_wfi_wait(&self) -> Option<u64> {
    self.should_wait_for_async_device()
      .then(|| self.cpu.bus.wake_hub.snapshot())
  }

  fn wait_if_still_idle(&self, wake_generation: Option<u64>) {
    if let Some(wake_generation) = wake_generation {
      // run_next may have observed an interrupt and cleared WFI, or made a future timer eligible.
      // Recheck both conditions before blocking the host thread.
      if self.should_wait_for_async_device() {
        self.cpu.bus.wake_hub.wait_for_change(wake_generation);
      }
    }
  }

  fn should_wait_for_async_device(&self) -> bool {
    let has_enabled_future_timer = self.cpu.csrs.read_unchecked(MIE) & MTIE_MASK != 0 &&
      self.cpu.bus.clint.ticks_until_timer().is_some();
    self.cpu.wfi && !has_enabled_future_timer
  }

  pub fn run_for_test(&mut self, test_name: String) -> i32 {
    self.cpu.write_pc(VirtAddr(RV64_PC_RESET));
    self.cpu.write_reg(Reg::X(Fin::new(11)), RV64_DTB_ADDR);
    loop {
      let wake_generation = self.prepare_wfi_wait();
      let cont = self.run_next_for_test();
      match cont {
        true => self.wait_if_still_idle(wake_generation),
        false => {
          // riscv-tests uses ecall to tell test results
          let gp = self.cpu.read_reg(Reg::X(Fin::new(3))).unwrap();
          let a0 = self.cpu.read_reg(Reg::X(Fin::new(10))).unwrap();
          let a7 = self.cpu.read_reg(Reg::X(Fin::new(17))).unwrap();
          if a7 != 93 { continue; } // not the result telling ecall
          return if a0 == 0 && gp == 1 {
            println!("[Valheim:{:?}] {}: Test passed!", self.cpu.mode, test_name);
            0
          } else {
            let failed = gp >> 1;
            println!("[Valheim:{:?}] {}: Test {} failed!, gp = {}, a0 = {}", self.cpu.mode, test_name, failed, gp, a0);
            self.show_status();
            1
          };
        }
      }
    }
  }

  pub fn run_next_for_test(&mut self) -> bool {
    match self.dispatch_next() {
      Ok(_) => true,
      // riscv-tests uses ecall to tell test results
      Err(Exception::MachineEcall) => false,
      Err(Exception::UserEcall) => false,
      Err(Exception::SupervisorEcall) => false,
      Err(ex) => {
        let _ = ex.handle(&mut self.cpu);
        true
      }
    }
  }

  pub fn halt(&mut self) {
    self.cpu.bus.halt();
    self.cpu.journal.flush();
    self.show_status();
  }

  pub fn show_status(&self) {
    let xabi = [
      "zero", " ra ", " sp ", " gp ", " tp ", " t0 ", " t1 ", " t2 ", " s0 ", " s1 ", " a0 ",
      " a1 ", " a2 ", " a3 ", " a4 ", " a5 ", " a6 ", " a7 ", " s2 ", " s3 ", " s4 ", " s5 ",
      " s6 ", " s7 ", " s8 ", " s9 ", " s10", " s11", " t3 ", " t4 ", " t5 ", " t6 ",
    ];
    let fabi = [
      " ft0", " ft1", " ft2", " ft3", " ft4", " ft5", " ft6", " ft7",
      " fs0", " fs1",
      " fa0", " fa1",
      " fa2", " fa3", " fa4", " fa5", " fa6", " fa7",
      " fs2", " fs3", " fs4", " fs5", " fs6", " fs7", " fs8", " fs9", "fs10", "fs11",
      " ft8", " ft9", "ft10", "ft11",
    ];
    println!("=======================================");
    println!("Privileged mode: {:?}", self.cpu.mode);
    println!("General purpose registers:");
    println!("  pc : {:#x}", self.cpu.read_pc().0);
    for i in 0..31 {
      println!("  x{:<2} ({}): {:<#18x}    f{:<2} ({}): {:}", i, xabi[i], self.cpu.regs.x[i], i, fabi[i], self.cpu.regs.f[i]);
    }
    println!("CSR registers:");
    println!("  Machine Level CSR register:");
    println!("    mstatus: {:<#18x}    mtvec:   {:<#18x}    mepc:    {:<#18x}", csr!(self, MSTATUS), csr!(self, MTVEC), csr!(self, MEPC));
    println!("    mcause:  {:<#18x}    medeleg: {:<#18x}    mideleg: {:<#18x}", csr!(self, MCAUSE), csr!(self, MEDELEG), csr!(self, MIDELEG));
    println!("    mscratch: {:<#18x}", csr!(self, MSCRATCH));
    println!("  Supervisor Level CSR register:");
    println!("    sstatus: {:<#18x}    stvec:   {:<#18x}    sepc:    {:<#18x}", csr!(self, SSTATUS), csr!(self, STVEC), csr!(self, SEPC));
    println!("    scause:  {:<#18x}    satp:    {:<#18x}", csr!(self, SCAUSE), csr!(self, SATP));
    println!("=======================================");
  }

  pub fn load_memory(&mut self, offset: usize, mem: &[u8]) {
    self.cpu.bus.mem.load(offset, mem);
  }

  pub fn load_device_tree(&mut self, bytes: &[u8]) -> Option<()> {
    self.cpu.bus.device_tree.load(VIRT_MROM_BASE as usize, bytes)
  }

  pub fn load_disk_file(&mut self, file: String) -> Result<(), std::io::Error> {
    let file = OpenOptions::new().read(true).write(true).open(&file)?;
    let mmap = unsafe { MmapMut::map_mut(&file) }?;
    self.cpu.bus.virtio.set_image(mmap);
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use std::sync::atomic::{AtomicBool, Ordering};
  use std::sync::{mpsc, Arc, Mutex};
  use std::time::Duration;

  use super::*;
  use crate::cpu::bus::CLINT_BASE;
  use crate::cpu::csr::CSRMap::{
    MCAUSE, MEPC, MIP, MSIE_MASK, MTIP_MASK, MTVEC, SEIE_MASK, TIME,
  };
  use crate::device::ns16550a::UART_IER;
  use crate::device::Device;
  use crate::interp::ExecOutcome;

  struct RecordingExecutor {
    budgets: Arc<Mutex<Vec<u32>>>,
    attempted: u32,
  }

  impl RV64Executor for RecordingExecutor {
    fn execute(&mut self, _cpu: &mut RV64Cpu, budget: u32) -> ExecOutcome {
      self.budgets.lock().unwrap().push(budget);
      ExecOutcome::new(self.attempted.min(budget), Ok(()))
    }
  }

  #[test]
  fn dispatcher_limits_blocks_to_timer_deadline_and_advances_in_bulk() {
    let budgets = Arc::new(Mutex::new(Vec::new()));
    let executor = RecordingExecutor {
      budgets: budgets.clone(),
      attempted: 32,
    };
    let mut machine = Machine {
      cpu: RV64Cpu::new(None),
      executor: Box::new(executor),
    };
    machine
      .cpu
      .bus
      .clint
      .write::<u64>(VirtAddr(CLINT_BASE + 0x4000), 5)
      .unwrap();

    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(&*budgets.lock().unwrap(), &[4]);
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 4);

    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(&*budgets.lock().unwrap(), &[4, MAX_EXECUTOR_BUDGET]);
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 36);
  }

  #[test]
  fn waiting_hart_still_ticks_once_per_dispatch() {
    let mut cpu = RV64Cpu::new(None);
    cpu.wfi = true;
    let mut machine = Machine {
      cpu,
      executor: Box::new(NaiveInterpreter::new()),
    };

    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 1);
    assert!(machine.cpu.wfi);

    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 2);
    assert!(machine.cpu.wfi);
  }

  #[test]
  fn waiting_hart_fast_forwards_to_an_enabled_timer_deadline() {
    let budgets = Arc::new(Mutex::new(Vec::new()));
    let executor = RecordingExecutor {
      budgets: budgets.clone(),
      attempted: 0,
    };
    let mut cpu = RV64Cpu::new(None);
    cpu.wfi = true;
    cpu.write_pc(VirtAddr(0x8000_0000));
    cpu.csrs.write_unchecked(MTVEC, 0x8000_0100).unwrap();
    cpu.csrs.write_unchecked(MIE, MTIE_MASK).unwrap();
    cpu.csrs.write_mstatus_MIE(true);
    cpu
      .bus
      .clint
      .write::<u64>(VirtAddr(CLINT_BASE + 0x4000), 100)
      .unwrap();
    let mut machine = Machine {
      cpu,
      executor: Box::new(executor),
    };

    assert_eq!(machine.prepare_wfi_wait(), None);
    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 100);
    assert!(!machine.cpu.wfi);
    assert_eq!(machine.cpu.read_pc(), VirtAddr(0x8000_0100));
    assert_eq!(machine.cpu.csrs.read_unchecked(MEPC), 0x8000_0000);
    assert_eq!(machine.cpu.csrs.read_unchecked(MCAUSE), (1_u64 << 63) | 7);
    assert_eq!(&*budgets.lock().unwrap(), &[MAX_EXECUTOR_BUDGET]);
  }

  #[test]
  fn waiting_hart_services_an_enabled_software_interrupt_before_the_timer() {
    let budgets = Arc::new(Mutex::new(Vec::new()));
    let executor = RecordingExecutor {
      budgets: budgets.clone(),
      attempted: 0,
    };
    let mut cpu = RV64Cpu::new(None);
    cpu.wfi = true;
    cpu.csrs
      .write_unchecked(MIE, MSIE_MASK | MTIE_MASK)
      .unwrap();
    cpu.csrs.write_mstatus_MIE(true);
    cpu.bus.clint.write::<u32>(VirtAddr(CLINT_BASE), 1).unwrap();
    cpu
      .bus
      .clint
      .write::<u64>(VirtAddr(CLINT_BASE + 0x4000), 100)
      .unwrap();
    let mut machine = Machine {
      cpu,
      executor: Box::new(executor),
    };

    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 1);
    assert!(!machine.cpu.wfi);
    assert_eq!(machine.cpu.csrs.read_unchecked(MCAUSE), (1_u64 << 63) | 3);
    // The software interrupt is handled after the initial tick, so the future timer deadline,
    // rather than the larger dispatcher ceiling, limits this execution batch.
    assert_eq!(&*budgets.lock().unwrap(), &[99]);
  }

  #[test]
  fn waiting_hart_does_not_overshoot_a_timer_reached_by_the_initial_tick() {
    let budgets = Arc::new(Mutex::new(Vec::new()));
    let mut cpu = RV64Cpu::new(None);
    cpu.wfi = true;
    cpu.csrs.write_unchecked(MIE, MTIE_MASK).unwrap();
    cpu.csrs.write_mstatus_MIE(true);
    cpu
      .bus
      .clint
      .write::<u64>(VirtAddr(CLINT_BASE + 0x4000), 1)
      .unwrap();
    let mut machine = Machine {
      cpu,
      executor: Box::new(RecordingExecutor {
        budgets,
        attempted: 0,
      }),
    };

    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 1);
    assert!(!machine.cpu.wfi);
    assert_eq!(machine.cpu.csrs.read_unchecked(MCAUSE), (1_u64 << 63) | 7);
  }

  #[test]
  fn waiting_hart_wakes_without_trapping_when_global_interrupts_are_disabled() {
    let budgets = Arc::new(Mutex::new(Vec::new()));
    let mut cpu = RV64Cpu::new(None);
    cpu.wfi = true;
    cpu.write_pc(VirtAddr(0x8000_0000));
    cpu.csrs.write_unchecked(MTVEC, 0x8000_0100).unwrap();
    cpu.csrs.write_unchecked(MIE, MTIE_MASK).unwrap();
    cpu
      .bus
      .clint
      .write::<u64>(VirtAddr(CLINT_BASE + 0x4000), 100)
      .unwrap();
    let mut machine = Machine {
      cpu,
      executor: Box::new(RecordingExecutor {
        budgets: budgets.clone(),
        attempted: 0,
      }),
    };

    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 100);
    assert!(!machine.cpu.wfi);
    assert_eq!(machine.cpu.read_pc(), VirtAddr(0x8000_0000));
    assert_eq!(machine.cpu.csrs.read_unchecked(MCAUSE), 0);
    assert_ne!(machine.cpu.csrs.read_unchecked(MIP) & MTIP_MASK, 0);
    assert_eq!(&*budgets.lock().unwrap(), &[MAX_EXECUTOR_BUDGET]);
  }

  #[test]
  fn waiting_hart_does_not_fast_forward_to_a_disabled_timer() {
    let mut cpu = RV64Cpu::new(None);
    cpu.wfi = true;
    cpu
      .bus
      .clint
      .write::<u64>(VirtAddr(CLINT_BASE + 0x4000), 100)
      .unwrap();
    let mut machine = Machine {
      cpu,
      executor: Box::new(NaiveInterpreter::new()),
    };

    assert_eq!(machine.dispatch_next(), Ok(()));
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 1);
    assert!(machine.cpu.wfi);
  }

  #[test]
  fn waiting_hart_sleeps_until_uart_input_without_advancing_guest_time() {
    let budgets = Arc::new(Mutex::new(Vec::new()));
    let mut cpu = RV64Cpu::new(None);
    cpu.wfi = true;
    cpu.write_pc(VirtAddr(0x8000_0000));
    cpu.csrs.write_unchecked(MTVEC, 0x8000_0100).unwrap();
    cpu.csrs.write_unchecked(MIE, SEIE_MASK).unwrap();
    cpu.csrs.write_mstatus_MIE(true);

    let wake_hub = cpu.bus.wake_hub.clone();
    let uart = Arc::new(Uart16550a::new_without_input_thread(wake_hub.clone()));
    uart.mmio_write(VirtAddr(UART_IER), 1).unwrap();
    unsafe {
      cpu.bus.add_device(uart.clone()).unwrap();
    }

    let mut machine = Machine {
      cpu,
      executor: Box::new(RecordingExecutor {
        budgets,
        attempted: 0,
      }),
    };

    let wake_generation = machine.prepare_wfi_wait();
    assert!(wake_generation.is_some());
    assert!(machine.run_next());
    assert!(machine.cpu.wfi);
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 1);

    let input_uart = uart.clone();
    let watchdog_hub = wake_hub.clone();
    let watchdog_fired = Arc::new(AtomicBool::new(false));
    let producer_watchdog_fired = watchdog_fired.clone();
    let (cancel_watchdog, watchdog_cancelled) = mpsc::channel();
    let producer = std::thread::spawn(move || {
      std::thread::sleep(Duration::from_millis(10));
      input_uart.inject_input(b'x');
      if watchdog_cancelled.recv_timeout(Duration::from_secs(1)).is_err() {
        producer_watchdog_fired.store(true, Ordering::Relaxed);
        watchdog_hub.notify();
      }
    });
    machine.wait_if_still_idle(wake_generation);
    cancel_watchdog.send(()).unwrap();
    producer.join().unwrap();
    assert!(!watchdog_fired.load(Ordering::Relaxed));

    // Host waiting does not advance mtime. The normal dispatcher tick and interrupt path run only
    // after the asynchronous notification wakes the machine thread.
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 1);
    assert!(machine.cpu.wfi);
    assert!(machine.run_next());
    assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 2);
    assert!(!machine.cpu.wfi);
    assert_eq!(machine.cpu.read_pc(), VirtAddr(0x8000_0100));
    assert_eq!(machine.cpu.csrs.read_unchecked(MCAUSE), (1_u64 << 63) | 9);
  }
}
