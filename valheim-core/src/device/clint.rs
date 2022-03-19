// https://github.com/qemu/qemu/blob/master/hw/intc/sifive_clint.c
// https://github.com/qemu/qemu/blob/master/include/hw/intc/sifive_clint.h

use crate::cpu::bus::CLINT_BASE;
use crate::cpu::csr::{CSRMap, CSRRegs};
use crate::cpu::irq::Exception;
use crate::cpu::csr::CSRMap::{*};
use crate::memory::{CanIO, VirtAddr};

/// machine timer register
const MTIME: u64 = CLINT_BASE + 0xbff8;
const MTIME_END: u64 = MTIME + 8;

/// machine timer compare register
/// 3.2.1 Machine Timer Registers (mtime and mtimecmp)
/// Lower privilege levels do not have their own timecmp registers.
/// Instead, machine-mode software can implement any number of virtual timers on a hart by multiplexing the next timer interrupt into the mtimecmp register.
const MTIMECMP: u64 = CLINT_BASE + 0x4000;
const MTIMECMP_END: u64 = MTIMECMP + 8;

/// machine software interrupt pending register
const MSIP: u64 = CLINT_BASE;
const MSIP_END: u64 = MSIP + 4;

pub struct Clint {
  msip: u32,
  mtimecmp: u64,
  mtime: u64,
}

impl Clint {
  pub fn new() -> Self {
    Self {
      msip: 0,
      mtimecmp: 0,
      mtime: 0,
    }
  }

  pub fn tick(&mut self, csrs: &mut CSRRegs) {
    let old_mtime = self.mtime;
    let old_csr_time = csrs.csrs[CSRMap::TIME as usize];
    debug_assert_eq!(old_mtime, old_csr_time);

    // 3.2.1 Machine Timer Registers (mtime and mtimecmp)
    // mtime must increment at constant frequency, and the platform must provide a mechanism for determining the period of an mtime tick.

    self.mtime = old_mtime.wrapping_add(1);
    csrs.csrs[CSRMap::TIME as usize] = old_csr_time.wrapping_add(1);

    // 3.2.1 Machine Timer Registers (mtime and mtimecmp)
    // A machine timer interrupt becomes pending whenever mtime contains a value greater than or
    // equal to mtimecmp, treating the values as unsigned integers. The interrupt remains posted
    // until mtimecmp becomes greater than mtime (typically as a result of writing mtimecmp).
    // The interrupt will only be taken if interrupts are enabled the MTIE bit is set in the mie register.

    // note: just set the pending bit, the CPU will check whether the interrupt should be taken
    // together with the enable bit.

    if (self.msip & 1) != 0 {
      csrs.write_unchecked(MIP, csrs.read_unchecked(MIP) | MSIP_MASK);
    }

    if self.mtime >= self.mtimecmp {
      // take the interrupt
      csrs.write_unchecked(MIP, csrs.read_unchecked(MIP) | MTIP_MASK);
    } else {
      // clear the interrupt
      csrs.write_unchecked(MIP, csrs.read_unchecked(MIP) & !MTIP_MASK);
    }
  }

  pub fn read<T: CanIO>(&self, addr: VirtAddr) -> Result<u64, Exception> {
    let (val, offset) = match addr.0 {
      MSIP..=MSIP_END => (self.msip as u64, addr.0 - MSIP),
      MTIMECMP..=MTIMECMP_END => (self.mtimecmp, addr.0 - MTIMECMP),
      MTIME..=MTIME_END => (self.mtime, addr.0 - MTIME),
      _ => return Err(Exception::LoadAccessFault(addr)),
    };

    let val = match std::mem::size_of::<T>() {
      1 => (val >> (offset * 8)) & 0xff,
      2 => (val >> (offset * 8)) & 0xffff,
      4 => (val >> (offset * 8)) & 0xffffffff,
      8 => val,
      _ => return Err(Exception::LoadAccessFault(addr)),
    };
    Ok(val)
  }

  pub fn write<T: CanIO>(&mut self, addr: VirtAddr, value: u64) -> Result<(), Exception> {
    let (mut old, offset) = match addr.0 {
      MSIP..=MSIP_END => (self.msip as u64, addr.0 - MSIP),
      MTIMECMP..=MTIMECMP_END => (self.mtimecmp, addr.0 - MTIMECMP),
      MTIME..=MTIME_END => (self.mtime, addr.0 - MTIME),
      _ => return Err(Exception::StoreAccessFault(addr)),
    };

    // Calculate the new value of the target register based on `size` and `offset`.
    match std::mem::size_of::<T>() {
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
    match addr.0 {
      MSIP..=MSIP_END => self.msip = old as u32,
      MTIMECMP..=MTIMECMP_END => self.mtimecmp = old,
      MTIME..=MTIME_END => self.mtime = old,
      _ => return Err(Exception::StoreAccessFault(addr)),
    }
    Ok(())
  }
}
