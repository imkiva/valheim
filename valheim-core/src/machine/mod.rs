use std::fs::OpenOptions;
use std::sync::Arc;

use memmap2::MmapMut;

use valheim_asm::isa::data::Fin;
use valheim_asm::isa::rv64::CSRAddr;
use valheim_asm::isa::typed::{Imm32, Reg};

use crate::cpu::bus::VIRT_MROM_BASE;
use crate::cpu::irq::Exception;
use crate::cpu::RV64Cpu;
use crate::device::ns16550a::Uart16550a;
use crate::dtb::generate_device_tree_rom;
use crate::interp::naive::NaiveInterpreter;
use crate::interp::RV64Interpreter;
use crate::memory::VirtAddr;

const RV64_PC_RESET: u64 = 0x80000000;
const DEFAULT_CMDLINE: &str = "root=/dev/vda ro console=ttyS0";

pub struct Machine {
  pub cpu: RV64Cpu,
  pub interpreter: Box<dyn RV64Interpreter>,
}

macro_rules! csr {
    ($self:expr,$csr:ident) => {$self.cpu.csrs.read(CSRAddr(Imm32::from(crate::cpu::csr::CSRMap::$csr as u32)))};
}

impl Machine {
  pub fn new(cmdline: Option<String>, trace: Option<String>) -> Machine {
    let mut machine = Machine {
      cpu: RV64Cpu::new(trace),
      interpreter: Box::new(NaiveInterpreter::new()),
    };

    let cmdline = cmdline.unwrap_or(DEFAULT_CMDLINE.to_string());
    let memory_size = machine.cpu.bus.mem.memory_size as u64;
    let memory_base = machine.cpu.bus.mem.memory_base.0;
    let device_tree_rom = generate_device_tree_rom(cmdline, memory_base, memory_size)
      .expect("Cannot generate device tree");
    machine.load_device_tree(device_tree_rom.as_slice())
      .expect("Cannot load device tree");
    unsafe {
      machine.cpu.bus.add_device(Arc::new(Uart16550a::new()))
        .expect("Cannot install UART device")
    };
    machine
  }

  pub fn run(&mut self) {
    self.cpu.write_pc(VirtAddr(RV64_PC_RESET));
    self.cpu.write_reg(Reg::X(Fin::new(11)), 0x1020);
    loop {
      let cont = self.run_next();
      match cont {
        true => (),
        false => break,
      }
    }
    self.halt();
  }

  pub fn run_next(&mut self) -> bool {
    self.cpu.bus.clint.tick(&mut self.cpu.csrs);

    if let Some(irq) = self.cpu.pending_interrupt() {
      // TODO: can IRQ fail to handle?
      let _ = irq.handle(&mut self.cpu);
    }

    match self.interpreter.interp(&mut self.cpu) {
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

  pub fn run_for_test(&mut self, test_name: String) -> i32 {
    self.cpu.write_pc(VirtAddr(RV64_PC_RESET));
    self.cpu.write_reg(Reg::X(Fin::new(11)), 0x1020);
    loop {
      let cont = self.run_next_for_test();
      match cont {
        true => (),
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
    self.cpu.bus.clint.tick(&mut self.cpu.csrs);

    if let Some(irq) = self.cpu.pending_interrupt() {
      let _ = irq.handle(&mut self.cpu);
    }

    match self.interpreter.interp(&mut self.cpu) {
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
