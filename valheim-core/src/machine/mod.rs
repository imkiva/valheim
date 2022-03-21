use std::sync::Arc;
use crate::cpu::irq::Exception;
use crate::cpu::RV64Cpu;
use crate::device::ns16550a::Uart16550a;
use crate::interp::naive::NaiveInterpreter;
use crate::interp::RV64Interpreter;
use crate::isa::data::Fin;
use crate::isa::rv64::CSRAddr;
use crate::isa::typed::{Imm32, Reg};
use crate::memory::{CanIO, VirtAddr};

const RV64_PC_RESET: u64 = 0x80000000;

pub struct Machine {
  pub cpu: RV64Cpu,
  pub interpreter: Box<dyn RV64Interpreter>,
}

macro_rules! csr {
    ($self:expr,$csr:ident) => {$self.cpu.csrs.read(CSRAddr(Imm32::from(crate::cpu::csr::CSRMap::$csr as u32)))};
}

impl Machine {
  pub fn new(trace: Option<String>) -> Machine {
    let mut cpu = RV64Cpu::new(trace);
    unsafe {
      cpu.bus.add_device(Arc::new(Uart16550a::new()))
        .expect("Cannot install UART device")
    };
    Machine {
      cpu,
      interpreter: Box::new(NaiveInterpreter::new()),
    }
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
        println!("[Valheim] Exception: {:?} at {:#x}", ex, self.cpu.read_pc().0);
        self.show_status();
        // TODO: add watchdog to prevent kernels that do not handle double fault?
        // note: double/triple-fault can be handled by the M-mode program.
        // see: https://github.com/riscv/riscv-isa-manual/issues/3#issuecomment-278495907
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
    println!("=======================================");
    println!("Privileged mode: {:?}", self.cpu.mode);
    println!("General purpose registers:");
    println!("  pc : {:#x}", self.cpu.read_pc().0);
    for i in 0..31 {
      println!("  x{:<2}: {:<#18x}    f{:<2}: {:}", i, self.cpu.regs.x[i], i, self.cpu.regs.f[i]);
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

  pub fn load<T: CanIO>(&mut self, offset: usize, mem: &[T]) {
    self.cpu.bus.load(mem, offset);
  }
}
