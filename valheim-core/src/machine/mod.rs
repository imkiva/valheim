use crate::cpu::exception::Exception;
use crate::cpu::RV64Cpu;
use crate::cpu::trap::Trap;
use crate::interp::naive::NaiveInterpreter;
use crate::interp::RV64Interpreter;
use crate::isa::rv64::CSRAddr;
use crate::isa::typed::Imm32;
use crate::memory::{CanIO, VirtAddr};

const RV64_KERNEL_BASE: u64 = 0x80000000;

pub struct Machine {
  pub cpu: RV64Cpu,
  pub interpreter: Box<dyn RV64Interpreter>,
}

macro_rules! csr {
    ($self:expr,$csr:ident) => {$self.cpu.csrs.read(CSRAddr(Imm32::from(crate::cpu::csr::CSRMap::$csr as u32)))};
}

impl Machine {
  pub fn new(memory_size: usize) -> Machine {
    Machine {
      cpu: RV64Cpu::new(memory_size, None),
      interpreter: Box::new(NaiveInterpreter::new()),
    }
  }

  pub fn run(&mut self) {
    loop {
      // TODO: check interrupts
      // TODO: update devices
      let trap = match self.interpreter.interp(&mut self.cpu) {
        Ok(_) => None,
        Err(Exception::ValheimEbreak) => Some(Trap::ValheimEbreak),
        // TODO: handle exceptions
        Err(_) => None,
      };
      // TODO: traps
      match trap {
        Some(Trap::ValheimEbreak) => break,
        None => (),
      }
    }
    self.halt();
  }

  pub fn halt(&mut self) {
    self.cpu.bus.halt();
    self.show_status();
  }

  pub fn show_status(&self) {
    println!("=======================================");
    println!("General purpose registers:");
    for i in 0..31 {
      println!("  x{:<2}: {:<#18x}    f{:<2}: {:}", i, self.cpu.regs.x[i], i, self.cpu.regs.f[i]);
    }
    println!("CSR registers:");
    println!("  Machine Level CSR register:");
    println!("    mstatus: {:<#18x}    mtvec:   {:<#18x}    mepc:    {:<#18x}", csr!(self, MSTATUS), csr!(self, MTVEC), csr!(self, MEPC));
    println!("    mcause:  {:<#18x}    medeleg: {:<#18x}    mideleg: {:<#18x}", csr!(self, MCAUSE), csr!(self, MEDELEG), csr!(self, MIDELEG));
    println!("  Supervisor Level CSR register:");
    println!("    sstatus: {:<#18x}    stvec:   {:<#18x}    sepc:    {:<#18x}", csr!(self, SSTATUS), csr!(self, STVEC), csr!(self, SEPC));
    println!("    scause:  {:<#18x}    sedeleg: {:<#18x}    sideleg: {:<#18x}", csr!(self, SCAUSE), csr!(self, SEDELEG), csr!(self, SIDELEG));
    println!("  User Level CSR register:");
    println!("    ustatus: {:<#18x}    utvec:   {:<#18x}    uepc:    {:<#18x}", csr!(self, USTATUS), csr!(self, UTVEC), csr!(self, UEPC));
    println!("    ucause:  {:<#18x}", csr!(self, UCAUSE));
    println!("=======================================");
  }

  pub fn load_kernel<T: CanIO>(&mut self, mem: &[T]) {
    self.cpu.bus.load_kernel(mem, RV64_KERNEL_BASE as usize);
    self.cpu.write_pc(VirtAddr(RV64_KERNEL_BASE));
  }
}
