use crate::cpu::{PrivilegeMode, RV64Cpu};
use crate::cpu::csr::CSRMap::{MCAUSE, MEDELEG, MEIP_MASK, MEPC, MIDELEG, MIE, MIP, MSIP_MASK, MSTATUS, MTIP_MASK, MTVAL, MTVEC, SCAUSE, SEIP_MASK, SEPC, SIE, SIP, SSIP_MASK, SSTATUS, STIP_MASK, STVAL, STVEC};
use crate::device::virtio::Virtio;
use crate::isa::compressed::untyped::Bytecode16;
use crate::isa::untyped::Bytecode;
use crate::memory::VirtAddr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IRQ {
  /// Machine external IRQ
  MEI,
  /// Machine software IRQ
  MSI,
  /// Machine timer IRQ
  MTI,
  /// Supervisor external IRQ
  SEI,
  /// Supervisor software IRQ
  SSI,
  /// Supervisor timer IRQ
  STI,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Exception {
  IllegalInstruction(VirtAddr, Bytecode, Bytecode16),
  LoadAccessFault(VirtAddr),
  StoreAccessFault(VirtAddr),
  LoadAddressMisaligned(VirtAddr),
  StoreAddressMisaligned(VirtAddr),
  Breakpoint,
}

impl RV64Cpu {
  pub fn pending_interrupt(&mut self) -> Option<IRQ> {
    // 3.1.6.1 Privilege and Global Interrupt-Enable Stack in mstatus register
    // Global interrupt-enable bits, MIE and SIE, are provided for M-mode and S-mode respectively.
    // When a hart is executing in privilege mode x, interrupts are globally enabled
    // when xIE=1 and globally disabled when xIE=0.
    // Interrupts for lower-privilege modes, w<x, are always globally disabled
    // regardless of the setting of any global wIE bit for the lower-privilege mode.
    // Interrupts for higher-privilege modes, y>x, are always globally enabled
    // regardless of the setting of the global yIE bit for the higher-privilege mode.

    // Higher-privilege-level code can use separate per-interrupt enable bits
    // to disable selected higher-privilege-mode interrupts before ceding control
    // to a lower-privilege mode.
    let irq_enabled = match self.mode {
      PrivilegeMode::Machine => self.csrs.is_machine_irq_enabled_globally(),
      PrivilegeMode::Supervisor => self.csrs.is_supervisor_irq_enabled_globally(),
      PrivilegeMode::User => true,
    };

    if !irq_enabled {
      return None;
    }

    // 3.1.9 Machine Interrupt Registers (mip and mie)
    // The machine-level interrupt fixed-priority ordering rules were developed with the following rationale:
    // 1. Interrupts for higher privilege modes must be serviced before interrupts
    //    for lower privilege modes to support preemption.
    // 2. The platform-specific machine-level interrupt sources in bits 16 and above have
    //    platform-specific priority, but are typically chosen to have the highest service priority
    //    to support very fast local vectored interrupts.
    // 3. External interrupts are handled before internal (timer/software) interrupts
    //    as external interrupts are usually generated by devices that might require
    //    low interrupt service times.
    // 4. Software interrupts are handled before internal timer interrupts, because internal
    //    timer interrupts are usually intended for time slicing, where time precision is less important,
    //    whereas software interrupts are used for inter-processor messaging.
    //    Software interrupts can be avoided when high-precision timing is required,
    //    or high-precision timer interrupts can be routed via a different interrupt path.

    // In our implementation, external devices is only UART, currently. We may support
    // VirtIO disks, which is also a external devices.

    // check builtin virtio disk
    let external_irq = match self.bus.virtio.pending_interrupt() {
      Some(virtio_irq) => {
        // TODO: replace with our own DMA implementation
        // TODO: exception handling
        Virtio::disk_access(self).expect("failed to access the disk");
        Some(virtio_irq)
      },
      _ => {
        let mut irq = None;
        // check other external devices like UART
        for dev in self.bus.devices.iter() {
          if let Some(irq_id) = dev.is_interrupting() {
            irq = Some(irq_id);
            break;
          }
        }
        irq
      }
    };

    if let Some(irq_id) = external_irq {
      // tell PLIC that we have an external irq
      self.bus.plic.update_pending(irq_id);
      // 3.1.9 Machine Interrupt Registers (mip and mie)
      // SEIP is writable in mip, and may be written by M-mode software to
      // indicate to S-mode that an external interrupt is pending. Additionally,
      // the platform-level interrupt controller may generate supervisor-level external interrupts.
      self.csrs.write_unchecked(MIP, self.csrs.read_unchecked(MIP) | SEIP_MASK);
    }

    // 3.1.9 Machine Interrupt Registers (mip and mie)
    // Multiple simultaneous interrupts destined for M-mode are handled
    // in the following decreasing priority order: MEI, MSI, MTI, SEI, SSI, STI.

    // only enable bit and pending bit are both set, an interrupt is treated as taken
    let mip = self.csrs.read_unchecked(MIP) & self.csrs.read_unchecked(MIE);

    // fast-path check
    if mip == 0 {
      return None;
    }

    if (mip & MEIP_MASK) != 0 {
      self.csrs.write_unchecked(MIP, self.csrs.read_unchecked(MIP) & !MEIP_MASK);
      return Some(IRQ::MEI);
    }

    if (mip & MSIP_MASK) != 0 {
      self.csrs.write_unchecked(MIP, self.csrs.read_unchecked(MIP) & !MSIP_MASK);
      return Some(IRQ::MSI);
    }

    if (mip & MTIP_MASK) != 0 {
      self.csrs.write_unchecked(MIP, self.csrs.read_unchecked(MIP) & !MTIP_MASK);
      return Some(IRQ::MTI);
    }

    if (mip & SEIP_MASK) != 0 {
      self.csrs.write_unchecked(MIP, self.csrs.read_unchecked(MIP) & !SEIP_MASK);
      return Some(IRQ::SEI);
    }

    if (mip & SSIP_MASK) != 0 {
      self.csrs.write_unchecked(MIP, self.csrs.read_unchecked(MIP) & !SSIP_MASK);
      return Some(IRQ::SSI);
    }

    if (mip & STIP_MASK) != 0 {
      self.csrs.write_unchecked(MIP, self.csrs.read_unchecked(MIP) & !STIP_MASK);
      return Some(IRQ::STI);
    }

    None
  }
}

impl IRQ {
  /// Just the corresponding bit in MIP register
  fn mcause(&self) -> u64 {
    match self {
      IRQ::SSI => 1,
      IRQ::MSI => 3,
      IRQ::STI => 5,
      IRQ::MTI => 7,
      IRQ::SEI => 9,
      IRQ::MEI => 11,
    }
  }

  pub fn handle(&self, cpu: &mut RV64Cpu) -> Result<(), ()> {
    trap_entry(self.mcause(), 0, true, cpu)
  }
}

impl Exception {
  pub fn mcause_mtval(&self) -> (u64, u64) {
    match self {
      Exception::IllegalInstruction(_, b, _) => (2, b.repr() as u64),
      Exception::Breakpoint => (3, 0),
      Exception::LoadAddressMisaligned(addr) => (4, addr.0),
      Exception::LoadAccessFault(addr) => (5, addr.0),
      Exception::StoreAddressMisaligned(addr) => (6, addr.0),
      Exception::StoreAccessFault(addr) => (7, addr.0),
    }
  }

  pub fn handle(&self, cpu: &mut RV64Cpu) -> Result<(), ()> {
    let (mcause, mtval) = self.mcause_mtval();
    trap_entry(mcause, mtval, false, cpu)
  }
}

pub fn trap_entry(mcause: u64, mtval: u64, is_interrupt: bool, cpu: &mut RV64Cpu) -> Result<(), ()> {
  // clear the wait-for-interrupt flag
  if is_interrupt { cpu.wfi = false; }

  let previous_pc = cpu.regs.pc.0;
  let previous_mode = cpu.mode;

  // 3.1.9 Machine Interrupt Registers (mip and mie)
  // An interrupt i will trap to M-mode (causing the privilege mode to change to M-mode) if all of the following are true:
  // (a) either the current privilege mode is M and the MIE bit in the mstatus register is set,
  //     or the current privilege mode has less privilege than M-mode;
  // (b) bit i is set in both mip and mie;
  // (c) if register mideleg exists, bit i is not set in mideleg/medeleg.

  // 4.1.3 Supervisor Interrupt Registers (sip and sie)
  // An interrupt i will trap to S-mode if both of the following are true:
  // (a) either the current privilege mode is S and the SIE bit in the sstatus register is set,
  //     or the current privilege mode has less privilege than S-mode;
  // (b) bit i is set in both sip and sie.

  // note: wo don't need to check the following, since we have
  // already checked it in `RV64Cpu::pending_interrupt`:
  // (a) the MIE bit in mstatus, or the SIE bit in sstatus
  // (b) the bit i in (mip & mie) or (sip & sie)

  // we only need to check: (c) bit i is not set in mideleg/medeleg.
  let not_delegated = match is_interrupt {
    true => ((cpu.csrs.read_unchecked(MIDELEG) >> mcause) & 1) == 0,
    false => ((cpu.csrs.read_unchecked(MEDELEG) >> mcause) & 1) == 0,
  };

  let trap_to_machine = previous_mode <= PrivilegeMode::Machine && not_delegated;

  match trap_to_machine {
    true => {
      // enter machine mode
      cpu.mode = PrivilegeMode::Machine;

      // find the trap-vector offset according to the mode saved in mtvec[1:0]
      // 3.1.7 Machine Trap-Vector Base-Address Register (mtvec)
      // When MODE=Direct, all traps into machine mode cause the pc to be set to the address in the BASE field.
      // When MODE=Vectored, all synchronous exceptions into machine mode cause the pc to be set to the address in the BASE field,
      // whereas interrupts cause the pc to be set to the address in the BASE field plus four times the interrupt cause number.
      // For example, a machine-mode timer interrupt (see Table 3.6 on page 39) causes the pc to be set to BASE+0x1c.
      let mode = cpu.csrs.read_unchecked(MTVEC) & 0b11;
      let offset = match (is_interrupt, mode) {
        (false, _) => 0,        // exceptions all cause the pc to the BASE field
        (true, 0) => 0,         // direct mode
        (true, 1) => 4 * mcause, // vectored mode, here we are handling interrupts
        _ => panic!("invalid machine trap-vector address mode in mtvec[1:0]: {}", mode),
      };

      cpu.regs.pc.0 = ((cpu.csrs.read_unchecked(MTVEC) & !(0b11)) + offset) as u64;

      // 3.1.14 Machine Exception Program Counter (mepc)
      // When a trap is taken into M-mode, mepc is written with the virtual address of the instruction
      // that was interrupted or that encountered the exception. Otherwise, mepc is never written
      // by the implementation, though it may be explicitly written by software.
      // The low bit of mepc (mepc[0]) is always zero.
      cpu.csrs.write_unchecked(MEPC, previous_pc & !1);

      // 3.1.15 Machine Cause Register (mcause)
      // When a trap is taken into M-mode, mcause is written with a code indicating the event
      // that caused the trap. Otherwise, mcause is never written by the implementation,
      // though it may be explicitly written by software.
      // The Interrupt bit (mcause[63]) in the mcause register is set if the trap was caused by an interrupt.
      // The Exception Code field(mcause[0:62]) contains a code identifying the last exception or interrupt.
      let interrupt_bit = is_interrupt as u64;
      cpu.csrs.write_unchecked(MCAUSE, interrupt_bit << 63 | mcause);

      // 3.1.16 Machine Trap Value Register (mtval)
      // When a trap is taken into M-mode, mtval is either set to zero or written with exception-specific information
      // to assist software in handling the trap. Otherwise, mtval is never written by the implementation,
      // though it may be explicitly written by software.
      cpu.csrs.write_unchecked(MTVAL, mtval);

      // set MPIE to MIE
      let mie = cpu.csrs.is_machine_irq_enabled_globally();
      cpu.csrs.write_bit(MSTATUS, 7, mie);
      // set MIE to 0 to disable interrupts
      cpu.csrs.write_bit(MSTATUS, 3, false);

      // set MPP to previous privileged mode
      match previous_mode {
        PrivilegeMode::User => {
          // mstatus[12:11] = 0b00
          cpu.csrs.write_bit(MSTATUS, 11, false);
          cpu.csrs.write_bit(MSTATUS, 12, false);
        }
        PrivilegeMode::Supervisor => {
          // mstatus[12:11] = 0b01
          cpu.csrs.write_bit(MSTATUS, 11, true);
          cpu.csrs.write_bit(MSTATUS, 12, false);
        }
        PrivilegeMode::Machine => {
          // mstatus[12:11] = 0b11
          cpu.csrs.write_bit(MSTATUS, 11, true);
          cpu.csrs.write_bit(MSTATUS, 12, true);
        }
      }

      Ok(())
    }

    false => {
      // Comments omitted since there's only register differences
      cpu.mode = PrivilegeMode::Supervisor;
      let mode = cpu.csrs.read_unchecked(STVEC) & 0b11;
      let offset = match (is_interrupt, mode) {
        (false, _) => 0,        // exceptions all cause the pc to the BASE field
        (true, 0) => 0,         // direct mode
        (true, 1) => 4 * mcause, // vectored mode, here we are handling interrupts
        _ => panic!("invalid machine trap-vector address mode in stvec[1:0]: {}", mode),
      };

      cpu.regs.pc.0 = ((cpu.csrs.read_unchecked(STVEC) & (!0b11)) + offset) as u64;
      cpu.csrs.write_unchecked(SEPC, previous_pc & !1);
      let interrupt_bit = is_interrupt as u64;
      cpu.csrs.write_unchecked(SCAUSE, interrupt_bit << 63 | mcause);
      cpu.csrs.write_unchecked(STVAL, mtval);

      // set SPIE to SIE
      let sie = cpu.csrs.is_supervisor_irq_enabled_globally();
      cpu.csrs.write_bit(SSTATUS, 5, sie);
      // set SIE to 0 to disable interrupts
      cpu.csrs.write_bit(SSTATUS, 1, false);

      // set SPP to previous privileged mode
      match previous_mode {
        PrivilegeMode::User => cpu.csrs.write_bit(SSTATUS, 8, false),
        PrivilegeMode::Supervisor => cpu.csrs.write_bit(SSTATUS, 8, true),
        PrivilegeMode::Machine => panic!("Machine interrupts cannot be handled in supervisor mode"),
      }

      Ok(())
    }
  }
}
