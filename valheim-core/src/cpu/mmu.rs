use std::fmt::Debug;

use crate::cpu::{PrivilegeMode, RV64Cpu};
use crate::cpu::csr::CSRMap::SATP;
use crate::cpu::irq::Exception;
use crate::debug::trace::{MemTrace, Trace};
use crate::memory::{CanIO, VirtAddr};

pub const PAGE_SHIFT: u64 = 12;
pub const PAGE_SIZE: u64 = 1 << PAGE_SHIFT; // 4096

// RV64 satp CSR field masks
pub const SATP64_MODE_MASK: u64 = 0xF000000000000000;
pub const SATP64_MODE_SHIFT: u64 = 60;
pub const SATP64_ASID_MASK: u64 = 0x0FFFF00000000000;
pub const SATP64_ASID_SHIFT: u64 = 44;
pub const SATP64_PPN_MASK: u64 = 0x00000FFFFFFFFFFF;
pub const SATP64_PPN_SHIFT: u64 = 0;

/* VM modes (satp.mode) privileged ISA V20211203 */
pub const VM_V20211203_MBARE: u8 = 0;
pub const VM_V20211203_SV39: u8 = 8;
pub const VM_V20211203_SV48: u8 = 9;
pub const VM_V20211203_SV57: u8 = 10;
pub const VM_V20211203_SV64: u8 = 11;

// Page table entry (PTE) fields
pub const PTE_V: u64 = 0; /* Valid */
pub const PTE_R: u64 = 1; /* Read */
pub const PTE_W: u64 = 2; /* Write */
pub const PTE_X: u64 = 3; /* Execute */
pub const PTE_U: u64 = 4; /* User */
pub const PTE_G: u64 = 5; /* Global */
pub const PTE_A: u64 = 6; /* Accessed */
pub const PTE_D: u64 = 7; /* Dirty */

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum VMMode {
  /// 4.1.11 Supervisor Address Translation and Protection (satp) Register
  /// When MODE=Bare, supervisor virtual addresses are equal to supervisor physical addresses,
  /// and there is no additional memory protection beyond the physical memory protection scheme.
  MBARE = VM_V20211203_MBARE,
  SV39 = VM_V20211203_SV39,
  SV48 = VM_V20211203_SV48,
  SV57 = VM_V20211203_SV57,
  SV64 = VM_V20211203_SV64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum AccessType {
  Fetch,
  Read,
  Write,
}

/// Compatibility name for existing callers. New MMU and JIT code should use `AccessType`.
pub type Reason = AccessType;

pub const TRANSLATION_READ: u8 = 1 << 0;
pub const TRANSLATION_WRITE: u8 = 1 << 1;
pub const TRANSLATION_EXECUTE: u8 = 1 << 2;

/// The physical endpoint selected by the common page-table walker.
///
/// Only ordinary guest DRAM returns a host pointer. All device, firmware-ROM and unmapped physical
/// addresses use `Mmio`, forcing the caller through `Bus::read`/`Bus::write` so device side effects
/// and access faults remain in the shared core path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslationTarget {
  Dram {
    paddr: VirtAddr,
    host_page: *mut u8,
    phys_page: u64,
    permissions: u8,
  },
  Mmio {
    paddr: VirtAddr,
    permissions: u8,
  },
}

impl TranslationTarget {
  #[inline(always)]
  pub fn paddr(self) -> VirtAddr {
    match self {
      TranslationTarget::Dram { paddr, .. } | TranslationTarget::Mmio { paddr, .. } => paddr,
    }
  }

  #[inline(always)]
  pub fn permissions(self) -> u8 {
    match self {
      TranslationTarget::Dram { permissions, .. } | TranslationTarget::Mmio { permissions, .. } => {
        permissions
      }
    }
  }

  /// Returns the exact host address for an access wholly contained in the translated DRAM page.
  /// Callers must translate the next guest page separately when an access crosses a page boundary.
  #[inline(always)]
  fn direct_host_addr(self, width: usize) -> Option<*mut u8> {
    let TranslationTarget::Dram {
      paddr,
      host_page,
      phys_page,
      ..
    } = self
    else {
      return None;
    };
    let page_offset = paddr.0.checked_sub(phys_page)?;
    let width = u64::try_from(width).ok()?;
    if page_offset.checked_add(width)? > PAGE_SIZE {
      return None;
    }
    Some(unsafe { host_page.add(page_offset as usize) })
  }
}

impl VMMode {
  /// returns Some((levels, ptidxbits, ptesize)) if the translation is supported.
  pub fn translation_args(&self) -> Option<(i64, u64, u64)> {
    match self {
      VMMode::MBARE => None, // MBARE does not need translation
      VMMode::SV39 => Some((3, 9, 8)),
      VMMode::SV48 => Some((4, 9, 8)),
      VMMode::SV57 => Some((5, 9, 8)),
      VMMode::SV64 => None,  // SV64 is not described in the spec Volume 2
    }
  }

  /// returns the vpn structure for the given vaddr.
  /// For Sv39, the result only contains 3 elements (vpn[0], vpn[1], vpn[2]), vpn[3,4] are 0.
  /// For Sv48, the result contains 4 elements (vpn[0], vpn[1], vpn[2], vpn[3]), vpn[4] is 0.
  /// For Sv57, the result contains 5 elements (vpn[0], vpn[1], vpn[2], vpn[3], vpn[4])
  /// For unsupported modes, the result contains all zero.
  pub fn vpn(&self, addr: u64) -> [u64; 5] {
    match self {
      VMMode::SV39 => {
        // addr[0:11] = page offset
        // addr[12:20] = vpn[0]
        // addr[21:29] = vpn[1]
        // addr[30:38] = vpn[2]
        [(addr >> 12) & 0x1ff, (addr >> 21) & 0x1ff, (addr >> 30) & 0x1ff, 0, 0]
      }
      VMMode::SV48 => {
        // addr[0:11] = page offset
        // addr[12:20] = vpn[0]
        // addr[21:29] = vpn[1]
        // addr[30:38] = vpn[2]
        // addr[39:47] = vpn[3]
        [(addr >> 12) & 0x1ff, (addr >> 21) & 0x1ff, (addr >> 30) & 0x1ff, (addr >> 39) & 0x1ff, 0]
      }
      VMMode::SV57 => {
        // addr[0:11] = page offset
        // addr[12:20] = vpn[0]
        // addr[21:29] = vpn[1]
        // addr[30:38] = vpn[2]
        // addr[39:47] = vpn[3]
        // addr[48:56] = vpn[4]
        [(addr >> 12) & 0x1ff, (addr >> 21) & 0x1ff, (addr >> 30) & 0x1ff, (addr >> 39) & 0x1ff, (addr >> 48) & 0x1ff]
      }
      _ => [0, 0, 0, 0, 0]
    }
  }

  /// returns the ppn fields of PTE.
  pub fn pte_ppn(&self, pte: u64) -> [u64; 5] {
    match self {
      VMMode::SV39 => {
        // pte[10:18] = ppn[0]
        // pte[19:27] = ppn[1]
        // pte[28:53] = ppn[2]
        [(pte >> 10) & 0x1ff, (pte >> 19) & 0x1ff, (pte >> 28) & 0x03ffffff, 0, 0]
      }
      VMMode::SV48 => {
        // pte[10:18] = ppn[0]
        // pte[19:27] = ppn[1]
        // pte[28:36] = ppn[2]
        // pte[37:53] = ppn[3]
        [(pte >> 10) & 0x1ff, (pte >> 19) & 0x1ff, (pte >> 28) & 0x1ff, (pte >> 37) & 0x1ffff, 0]
      }
      VMMode::SV57 => {
        // pte[10:18] = ppn[0]
        // pte[19:27] = ppn[1]
        // pte[28:36] = ppn[2]
        // pte[37:45] = ppn[3]
        // pte[46:53] = ppn[4]
        [(pte >> 10) & 0x1ff, (pte >> 19) & 0x1ff, (pte >> 28) & 0x1ff, (pte >> 37) & 0x1ff, (pte >> 46) & 0xff]
      }
      _ => [0, 0, 0, 0, 0],
    }
  }

  fn is_canonical(&self, addr: u64) -> bool {
    let address_bits = match self {
      VMMode::SV39 => 39,
      VMMode::SV48 => 48,
      VMMode::SV57 => 57,
      _ => return true,
    };
    let sign = (addr >> (address_bits - 1)) & 1;
    let upper = addr >> address_bits;
    let expected = if sign == 0 {
      0
    } else {
      (1_u64 << (64 - address_bits)) - 1
    };
    upper == expected
  }
}

impl AccessType {
  pub fn to_page_fault<T>(self, addr: VirtAddr) -> Result<T, Exception> {
    return match self {
      AccessType::Fetch => Err(Exception::InstructionPageFault(addr)),
      AccessType::Read => Err(Exception::LoadPageFault(addr)),
      AccessType::Write => Err(Exception::StorePageFault(addr)),
    };
  }

  fn access_fault(self, addr: VirtAddr) -> Exception {
    match self {
      AccessType::Fetch => Exception::InstructionAccessFault(addr),
      AccessType::Read => Exception::LoadAccessFault(addr),
      AccessType::Write => Exception::StoreAccessFault(addr),
    }
  }
}

impl RV64Cpu {
  #[inline(always)]
  fn first_page_fragment_len(addr: VirtAddr, width: usize) -> usize {
    let page_offset = addr.0 & (PAGE_SIZE - 1);
    let remaining = (PAGE_SIZE - page_offset) as usize;
    remaining.min(width)
  }

  #[inline(always)]
  fn read_translated_fragment(
    &mut self,
    target: TranslationTarget,
    guest_addr: VirtAddr,
    output: &mut [u8],
  ) -> Result<(), Exception> {
    if let Some(host_addr) = target.direct_host_addr(output.len()) {
      unsafe {
        std::ptr::copy_nonoverlapping(host_addr.cast_const(), output.as_mut_ptr(), output.len())
      };
      return Ok(());
    }

    for (offset, byte) in output.iter_mut().enumerate() {
      let fault_addr = VirtAddr(guest_addr.0.wrapping_add(offset as u64));
      *byte = self
        .bus
        .read::<u8>(VirtAddr(target.paddr().0.wrapping_add(offset as u64)))
        .map_err(|_| AccessType::Read.access_fault(fault_addr))?;
    }
    Ok(())
  }

  #[inline(always)]
  fn write_translated_fragment(
    &mut self,
    target: TranslationTarget,
    guest_addr: VirtAddr,
    input: &[u8],
  ) -> Result<(), Exception> {
    if let Some(host_addr) = target.direct_host_addr(input.len()) {
      unsafe { std::ptr::copy_nonoverlapping(input.as_ptr(), host_addr, input.len()) };
      return Ok(());
    }

    for (offset, byte) in input.iter().enumerate() {
      let fault_addr = VirtAddr(guest_addr.0.wrapping_add(offset as u64));
      self
        .bus
        .write::<u8>(VirtAddr(target.paddr().0.wrapping_add(offset as u64)), *byte)
        .map_err(|_| AccessType::Write.access_fault(fault_addr))?;
    }
    Ok(())
  }

  #[inline(always)]
  fn probe_translated_fragment_write(
    &self,
    target: TranslationTarget,
    guest_addr: VirtAddr,
    width: usize,
  ) -> Result<(), Exception> {
    if target.direct_host_addr(width).is_some() {
      return Ok(());
    }

    for offset in 0..width {
      let paddr = VirtAddr(target.paddr().0.wrapping_add(offset as u64));
      let fault_addr = VirtAddr(guest_addr.0.wrapping_add(offset as u64));
      self
        .bus
        .probe_write(paddr, std::mem::size_of::<u8>())
        .map_err(|_| AccessType::Write.access_fault(fault_addr))?;
    }
    Ok(())
  }

  #[inline(always)]
  fn read_cross_page<T: CanIO>(
    &mut self,
    first: TranslationTarget,
    second: TranslationTarget,
    addr: VirtAddr,
    first_len: usize,
  ) -> Result<T, Exception> {
    let width = std::mem::size_of::<T>();
    let mut value = std::mem::MaybeUninit::<T>::uninit();
    let bytes = unsafe {
      std::slice::from_raw_parts_mut(value.as_mut_ptr().cast::<u8>(), std::mem::size_of::<T>())
    };
    let second_addr = VirtAddr(addr.0.wrapping_add(first_len as u64));
    self.read_translated_fragment(first, addr, &mut bytes[..first_len])?;
    self.read_translated_fragment(second, second_addr, &mut bytes[first_len..width])?;
    Ok(unsafe { value.assume_init() })
  }

  #[inline(always)]
  fn write_cross_page<T: CanIO>(
    &mut self,
    first: TranslationTarget,
    second: TranslationTarget,
    addr: VirtAddr,
    first_len: usize,
    value: T,
  ) -> Result<(), Exception> {
    let width = std::mem::size_of::<T>();
    let bytes = unsafe {
      std::slice::from_raw_parts((&value as *const T).cast::<u8>(), std::mem::size_of::<T>())
    };
    let second_addr = VirtAddr(addr.0.wrapping_add(first_len as u64));
    self.write_translated_fragment(first, addr, &bytes[..first_len])?;
    self.write_translated_fragment(second, second_addr, &bytes[first_len..width])
  }

  #[inline(always)]
  fn read_fetch_parcel(
    &mut self,
    target: TranslationTarget,
    vaddr: VirtAddr,
  ) -> Result<u16, Exception> {
    match target.direct_host_addr(std::mem::size_of::<u16>()) {
      Some(host_addr) => Ok(unsafe { host_addr.cast::<u16>().read_unaligned() }),
      None => self
        .bus
        .read::<u16>(target.paddr())
        .map_err(|_| Exception::InstructionAccessFault(vaddr)),
    }
  }

  #[inline(always)]
  pub fn fetch_mem(&mut self, addr: VirtAddr) -> Result<u32, Exception> {
    if addr.0 & 1 != 0 {
      return Err(Exception::InstructionAddressMisaligned(addr));
    }

    let first = self.translate_to_host(addr, AccessType::Fetch)?;
    let low = self.read_fetch_parcel(first, addr)?;
    if low & 0b11 != 0b11 {
      return Ok(low as u32);
    }

    let second_addr = VirtAddr(addr.0.wrapping_add(std::mem::size_of::<u16>() as u64));
    let high = if Self::first_page_fragment_len(addr, std::mem::size_of::<u32>())
      == std::mem::size_of::<u32>()
    {
      match first.direct_host_addr(std::mem::size_of::<u32>()) {
        Some(host_addr) => unsafe { host_addr.add(2).cast::<u16>().read_unaligned() },
        None => self
          .bus
          .read::<u16>(VirtAddr(first.paddr().0.wrapping_add(2)))
          .map_err(|_| Exception::InstructionAccessFault(second_addr))?,
      }
    } else {
      let second = self.translate_to_host(second_addr, AccessType::Fetch)?;
      self.read_fetch_parcel(second, second_addr)?
    };
    Ok((low as u32) | ((high as u32) << 16))
  }

  #[inline(always)]
  pub fn read_mem<T: CanIO + Debug>(&mut self, addr: VirtAddr) -> Result<T, Exception> {
    let target = self.translate_to_host(addr, AccessType::Read)?;
    let paddr = target.paddr();
    let width = std::mem::size_of::<T>();
    let first_len = Self::first_page_fragment_len(addr, width);
    let val = if first_len == width {
      match target.direct_host_addr(width) {
        Some(host_addr) => Ok(unsafe { host_addr.cast::<T>().read_unaligned() }),
        None => self
          .bus
          .read::<T>(paddr)
          .map_err(|_| AccessType::Read.access_fault(addr)),
      }
    } else {
      let second_addr = VirtAddr(addr.0.wrapping_add(first_len as u64));
      let second = self.translate_to_host(second_addr, AccessType::Read)?;
      self.read_cross_page(target, second, addr, first_len)
    };
    self.journal.trace(|| {
      Trace::Mem(MemTrace::Read(
        addr,
        paddr,
        width,
        format!("{:?}", val),
      ))
    });
    val
  }

  #[inline(always)]
  pub fn write_mem<T: CanIO + Debug>(&mut self, addr: VirtAddr, val: T) -> Result<(), Exception> {
    let target = self.translate_to_host(addr, AccessType::Write)?;
    let paddr = target.paddr();
    let width = std::mem::size_of::<T>();
    let first_len = Self::first_page_fragment_len(addr, width);
    let res = if first_len == width {
      match target.direct_host_addr(width) {
        Some(host_addr) => {
          unsafe { host_addr.cast::<T>().write_unaligned(val) };
          Ok(())
        }
        None => self
          .bus
          .write::<T>(paddr, val)
          .map_err(|_| AccessType::Write.access_fault(addr)),
      }
    } else {
      // Complete both translations and side-effect-free Bus range probes before committing either
      // fragment. A fault on the second guest page or an unmapped endpoint therefore cannot
      // partially store data.
      let second_addr = VirtAddr(addr.0.wrapping_add(first_len as u64));
      let second = self.translate_to_host(second_addr, AccessType::Write)?;
      self.probe_translated_fragment_write(target, addr, first_len)?;
      self.probe_translated_fragment_write(second, second_addr, width - first_len)?;
      self.write_cross_page(target, second, addr, first_len, val)
    };
    self.journal.trace(|| {
      Trace::Mem(MemTrace::Write(
        addr,
        paddr,
        width,
        format!("{:?}", val),
      ))
    });
    res
  }

  pub fn sync_pagetable(&mut self) {
    let satp = self.csrs.read_unchecked(SATP);
    let ppn = (satp & SATP64_PPN_MASK) >> SATP64_PPN_SHIFT;
    let _asid = (satp & SATP64_ASID_MASK) >> SATP64_ASID_SHIFT;
    let mode = (satp & SATP64_MODE_MASK) >> SATP64_MODE_SHIFT;

    self.vmppn = ppn << PAGE_SHIFT;
    match mode as u8 {
      VM_V20211203_MBARE => self.vmmode = VMMode::MBARE,
      VM_V20211203_SV39 => self.vmmode = VMMode::SV39,
      VM_V20211203_SV48 => self.vmmode = VMMode::SV48,
      VM_V20211203_SV57 => self.vmmode = VMMode::SV57,
      VM_V20211203_SV64 => self.vmmode = VMMode::SV64,
      _ => unreachable!(),
    }
  }

  /// Translates a guest virtual address and classifies its physical endpoint for the interpreter
  /// and JIT slow paths. This is the only entry point that performs a guest page-table walk.
  pub fn translate_to_host(
    &mut self,
    addr: VirtAddr,
    access: AccessType,
  ) -> Result<TranslationTarget, Exception> {
    let (paddr, permissions) = self.translate_address(addr, access)?;
    Ok(match self.bus.dram_host_page(paddr) {
      Some(mapping) => TranslationTarget::Dram {
        paddr,
        host_page: mapping.host_page,
        phys_page: mapping.phys_page,
        permissions,
      },
      None => TranslationTarget::Mmio { paddr, permissions },
    })
  }

  /// Compatibility wrapper for code that only needs the translated physical address.
  pub fn translate(&mut self, addr: VirtAddr, access: AccessType) -> Result<VirtAddr, Exception> {
    Ok(self.translate_to_host(addr, access)?.paddr())
  }

  fn translate_address(
    &mut self,
    addr: VirtAddr,
    reason: AccessType,
  ) -> Result<(VirtAddr, u8), Exception> {
    if self.vmmode == VMMode::MBARE {
      return Ok((
        addr,
        TRANSLATION_READ | TRANSLATION_WRITE | TRANSLATION_EXECUTE,
      ));
    }

    // 3.1.6.3 Memory Privilege in mstatus Register
    // The MPRV (Modify PRiVilege) bit modifies the effective privilege mode,
    // i.e., the privilege level at which loads and stores execute.
    // When MPRV=0, loads and stores behave as normal, using the translation and protection
    // mechanisms of the current privilege mode.
    // When MPRV=1, load and store memory addresses are translated and protected,
    // and endianness is applied, as though the current privilege mode were set to MPP.
    // Instruction address-translation and protection are unaffected by the setting of MPRV.
    // MPRV is read-only 0 if U-mode is not supported.
    let eff_mode = match reason {
      Reason::Fetch => self.mode,
      _ => match self.csrs.read_mstatus_MPRV() {
        true => self.csrs.read_mstatus_MPP(),
        false => self.mode,
      }
    };

    if eff_mode == PrivilegeMode::Machine {
      return Ok((
        addr,
        TRANSLATION_READ | TRANSLATION_WRITE | TRANSLATION_EXECUTE,
      ));
    }

    // 3.1.6.3 Memory Privilege in mstatus Register
    // The MXR (Make eXecutable Readable) bit modifies the privilege with which loads access virtual memory.
    // When MXR=0, only loads from pages marked readable (R=1 in Figure 4.18) will succeed.
    // When MXR=1, loads from pages marked either readable or executable (R=1 or X=1) will succeed.
    // MXR has no effect when page-based virtual memory is not in effect.
    // MXR is read-only 0 if S-mode is not supported.
    let mxr = self.csrs.read_mstatus_MXR();

    // 3.1.6.3 Memory Privilege in mstatus Register
    // The SUM (permit Supervisor User Memory access) bit modifies the privilege with which S-mode loads and stores access virtual memory.
    // When SUM=0, S-mode memory accesses to pages that are accessible by U-mode (U=1 in Figure 4.18) will fault.
    // When SUM=1, these accesses are permitted.
    // SUM has no effect when page-based virtual memory is not in effect. Note that,
    // while SUM is ordinarily ignored when not executing in S-mode,
    // it is in effect when MPRV=1 and MPP=S.
    // SUM is read-only 0 if S-mode is not supported or if satp.MODE is read-only 0.
    let sum = self.csrs.read_mstatus_SUM();

    // 3.1.6.3 Memory Privilege in mstatus Register
    // The MXR and SUM mechanisms only affect the interpretation of permissions encoded
    // in page-table entries. In particular, they have no impact on whether access-fault
    // exceptions are raised due to PMAs or PMP.

    // 4.3.2 Virtual Address Translation Process
    let addr = addr.0;
    if !self.vmmode.is_canonical(addr) {
      return reason.to_page_fault(VirtAddr(addr));
    }
    let Some((levels, _ptidxbits, ptesize)) = self.vmmode.translation_args() else {
      return reason.to_page_fault(VirtAddr(addr));
    };
    let vpn: [u64; 5] = self.vmmode.vpn(addr);

    // 1. Let a be satp.ppn × PAGESIZE, and let i = LEVELS − 1.
    let mut a = self.vmppn;
    let mut i: i64 = levels - 1;
    let mut pte: u64;
    let mut ppn: [u64; 5];

    loop {
      loop {
        // 2. Let pte be the value of the PTE at address a + va.vpn[i] × PTESIZE.
        // If accessing pte violates a PMA or PMP check, raise an access exception
        // corresponding to the original access type.
        // TODO: PMA or PMP checks
        pte = self
          .bus
          .read::<u64>(VirtAddr(a + vpn[i as usize] * ptesize))
          .map_err(|_| reason.access_fault(VirtAddr(addr)))?;

        // 3. If pte.v = 0, or if pte.r = 0 and pte.w = 1, stop and raise a page-fault
        // exception corresponding to the original access type.
        let pte_v = (pte >> PTE_V) & 1;
        let pte_r = (pte >> PTE_R) & 1;
        let pte_w = (pte >> PTE_W) & 1;
        let pte_x = (pte >> PTE_X) & 1;
        if pte_v == 0 || (pte_r == 0 && pte_w == 1) {
          return reason.to_page_fault(VirtAddr(addr));
        }

        // 4. Otherwise, the PTE is valid. If pte.r = 1 or pte.x = 1, go to step 5.
        if pte_r == 1 || pte_x == 1 {
          break;
        }

        // Otherwise, this PTE is a pointer to the next level of the page table.
        // Let i = i − 1. If i < 0, stop and raise a page-fault exception corresponding
        // to the original access type.
        i = i - 1;
        if i < 0 {
          return reason.to_page_fault(VirtAddr(addr));
        }

        // Otherwise, let a = pte.ppn × PAGESIZE and go to step 2.
        // ppn = pte[10:53]
        let ppn = (pte >> 10) & 0x0fffffffffff;
        a = ppn * PAGE_SIZE;
      }

      // make it immutable explicitly
      let i = i;

      // 5. A leaf PTE has been found. Determine if the requested memory access
      // is allowed by the pte.r, pte.w, pte.x, and pte.u bits, given the current
      // privilege mode and the value of the SUM and MXR fields of the mstatus register.
      // If not, stop and raise a page-fault exception corresponding to the original access type.

      // check access rights according to 3.1.6.3 Memory Privilege in mstatus Register
      // see: https://github.com/qemu/qemu/blob/aea6e471085f39ada1ccd637043c3ee3dfd88750/target/riscv/cpu_helper.c#L952
      let pte_r = (pte >> PTE_R) & 1;
      let pte_w = (pte >> PTE_W) & 1;
      let pte_x = (pte >> PTE_X) & 1;
      let pte_u = (pte >> PTE_U) & 1;

      match (pte_r, pte_w, pte_x) {
        // Reserved leaf PTE flags: PTE_W
        (0, 1, 0) |
        // Reserved leaf PTE flags: PTE_W + PTE_X
        (0, 1, 1) => return reason.to_page_fault(VirtAddr(addr)),
        _ => (),
      }

      if (pte_u == 1) && ((eff_mode != PrivilegeMode::User) && (!sum || reason == Reason::Fetch)) {
        // User PTE flags when not U mode and mstatus.SUM is not set,
        // or the access type is an instruction fetch
        return reason.to_page_fault(VirtAddr(addr));
      }

      if (pte_u == 0) && (eff_mode != PrivilegeMode::Supervisor) {
        // Supervisor PTE flags when not S mode
        return reason.to_page_fault(VirtAddr(addr));
      }

      if reason == Reason::Read && !((pte_r == 1) || ((pte_x == 1) && mxr)) {
        // Read access check failed
        return reason.to_page_fault(VirtAddr(addr));
      } else if reason == Reason::Write && (pte_w == 0) {
        // Write access check failed
        return reason.to_page_fault(VirtAddr(addr));
      } else if reason == Reason::Fetch && (pte_x == 0) {
        // Fetch access check failed
        return reason.to_page_fault(VirtAddr(addr));
      }

      // 6. If i > 0 and pte.ppn[i−1 : 0] != 0, this is a misaligned superpage;
      // stop and raise a page-fault exception corresponding to the original access type.
      ppn = self.vmmode.pte_ppn(pte);
      if i > 0 {
        if ppn.split_at(i as usize).0.iter().any(|&x| x != 0) {
          return reason.to_page_fault(VirtAddr(addr));
        }
      }

      // 7. If pte.a = 0, or if the memory access is a store and pte.d = 0, either raise a page-fault exception
      // corresponding to the original access type, or:
      // - If a store to pte would violate a PMA or PMP check, raise an access-fault exception corresponding to the original access type.
      // - Perform the following steps atomically:
      //   - Compare pte to the value of the PTE at address a + va.vpn[i] × PTESIZE.
      //   - If the values match, set pte.a to 1 and, if the original memory access is a store, also set pte.d to 1.
      //   - If the comparison fails, return to step 2
      let pte_a = (pte >> PTE_A) & 1;
      let pte_d = (pte >> PTE_D) & 1;
      if pte_a == 0 || (reason == Reason::Write && pte_d == 0) {
        // Compare pte to the value of the PTE at address a + va.vpn[i] × PTESIZE.
        let pte_addr = VirtAddr(a + vpn[i as usize] * ptesize);
        let compare = self
          .bus
          .read::<u64>(pte_addr)
          .map_err(|_| reason.access_fault(VirtAddr(addr)))?;

        // If the values match, set pte.a to 1 and
        if compare == pte {
          pte = pte | (1 << PTE_A);
          // if the original memory access is a store, also set pte.d to 1.
          if reason == Reason::Write {
            pte = pte | (1 << PTE_D);
          }

          // TODO: PMA or PMP checks
          self
            .bus
            .write::<u64>(pte_addr, pte)
            .map_err(|_| reason.access_fault(VirtAddr(addr)))?;
          // note: and goto step 8
          break;
        } else {
          // If the comparison fails, return to step 2
          eprintln!("[Valheim] translate: compare pte failed: {} != {}, returning to step 2", compare, pte);
          continue;
        }
      } else {
        // note: step 7 checks successfully goto step 8
        break;
      }
    }

    // 8. The translation is successful. The translated physical address is given as follows:
    // - pa.pgoff = va.pgoff.
    // - If i > 0, then this is a superpage translation and pa.ppn[i−1:0] = va.vpn[i−1:0].
    // - pa.ppn[LEVELS−1:i] = pte.ppn[LEVELS−1:i].
    let page_offset = addr & 0xfff;
    let paddr = match i {
      0 => {
        // pa.ppn[LEVELS−1:0] = pte.ppn[LEVELS−1:0]
        let ppn = (pte >> 10) & 0x0fffffffffff;
        (ppn << 12) | page_offset
      }
      1 => {
        // pa.ppn[0:0] = va.vpn[0:0]
        // pa.ppn[LEVELS−1:1] = pte.ppn[LEVELS−1:1]
        match levels {
          3 => (ppn[2] << 30) | (ppn[1] << 21) | (vpn[0] << 12) | page_offset,
          4 => (ppn[3] << 39) | (ppn[2] << 30) | (ppn[1] << 21) | (vpn[0] << 12) | page_offset,
          5 => (ppn[4] << 48) | (ppn[3] << 39) | (ppn[2] << 30) | (ppn[1] << 21) | (vpn[0] << 12) | page_offset,
          _ => unreachable!(),
        }
      }
      2 => {
        // pa.ppn[1:0] = va.vpn[1:0]
        // pa.ppn[LEVELS−1:2] = pte.ppn[LEVELS−1:2]
        match levels {
          3 => (ppn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          4 => (ppn[3] << 39) | (ppn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          5 => (ppn[4] << 48) | (ppn[3] << 39) | (ppn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          _ => unreachable!()
        }
      }
      3 => {
        // pa.ppn[2:0] = va.vpn[2:0]
        // pa.ppn[LEVELS−1:3] = pte.ppn[LEVELS−1:3]
        match levels {
          3 => (vpn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          4 => (ppn[3] << 39) | (vpn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          5 => (ppn[4] << 48) | (ppn[3] << 39) | (vpn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          _ => unreachable!()
        }
      }
      4 => {
        // pa.ppn[3:0] = va.vpn[3:0]
        // pa.ppn[LEVELS−1:4] = pte.ppn[LEVELS−1:4]
        match levels {
          3 => (vpn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          4 => (vpn[3] << 39) | (vpn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          5 => (ppn[4] << 48) | (vpn[3] << 39) | (vpn[2] << 30) | (vpn[1] << 21) | (vpn[0] << 12) | page_offset,
          _ => unreachable!()
        }
      }
      _ => unreachable!(),
    };

    // 4.3.2 Virtual Address Translation Process
    // TODO: The results of implicit address-translation reads in step 2 may be held in a read-only,
    //  incoherent address-translation cache but not shared with other harts.
    let permissions = ((pte >> PTE_R) & 0b111) as u8;
    Ok((VirtAddr(paddr), permissions))
  }
}

#[cfg(test)]
mod tests {
  use std::sync::Arc;

  use super::*;
  use crate::cpu::bus::{CLINT_BASE, RV64_MEMORY_BASE, RV64_MEMORY_END};
  use crate::device::Device;
  use crate::memory::Memory;

  const TEST_VADDR: u64 = 0x1234_5000;
  const ROOT_PAGE: u64 = RV64_MEMORY_BASE + 0x1000;
  const DATA_PAGE: u64 = RV64_MEMORY_BASE + 0x10_000;
  const SECOND_DATA_PAGE: u64 = RV64_MEMORY_BASE + 0x20_000;

  const fn pte_flag(bit: u64) -> u64 {
    1 << bit
  }

  fn install_mapping(cpu: &mut RV64Cpu, mode: VMMode, leaf_flags: u64) -> VirtAddr {
    let (levels, _, pte_size) = mode.translation_args().unwrap();
    let vpn = mode.vpn(TEST_VADDR);
    let mut table = ROOT_PAGE;

    for level in (1..levels as usize).rev() {
      let next_table = table + PAGE_SIZE;
      let pte_addr = VirtAddr(table + vpn[level] * pte_size);
      let next_pte = ((next_table >> PAGE_SHIFT) << 10) | pte_flag(PTE_V);
      cpu.bus.write(pte_addr, next_pte).unwrap();
      table = next_table;
    }

    let leaf_addr = VirtAddr(table + vpn[0] * pte_size);
    let leaf_pte = ((DATA_PAGE >> PAGE_SHIFT) << 10) | leaf_flags;
    cpu.bus.write(leaf_addr, leaf_pte).unwrap();
    cpu
      .csrs
      .write_unchecked(
        SATP,
        ((mode as u64) << SATP64_MODE_SHIFT) | (ROOT_PAGE >> PAGE_SHIFT),
      )
      .unwrap();
    cpu.sync_pagetable();
    leaf_addr
  }

  fn mapped_cpu(mode: VMMode, leaf_flags: u64) -> (RV64Cpu, VirtAddr) {
    let mut cpu = RV64Cpu::new(None);
    cpu.mode = PrivilegeMode::Supervisor;
    let leaf_addr = install_mapping(&mut cpu, mode, leaf_flags);
    (cpu, leaf_addr)
  }

  fn install_adjacent_leaf(
    cpu: &mut RV64Cpu,
    first_leaf_addr: VirtAddr,
    paddr: u64,
    leaf_flags: u64,
  ) {
    let adjacent_leaf_addr = VirtAddr(first_leaf_addr.0 + std::mem::size_of::<u64>() as u64);
    let leaf_pte = ((paddr >> PAGE_SHIFT) << 10) | leaf_flags;
    cpu.bus.write(adjacent_leaf_addr, leaf_pte).unwrap();
  }

  fn assert_page_fault(cpu: &mut RV64Cpu, access: AccessType, expected: Exception) {
    assert_eq!(
      cpu.translate_to_host(VirtAddr(TEST_VADDR), access),
      Err(expected),
    );
  }

  #[test]
  fn inaccessible_page_table_root_uses_the_original_access_type_and_guest_address() {
    let mut cpu = RV64Cpu::new(None);
    let vaddr = VirtAddr(TEST_VADDR);
    cpu.mode = PrivilegeMode::Supervisor;
    cpu.vmmode = VMMode::SV39;
    cpu.vmppn = 0;

    assert_eq!(
      cpu.translate_to_host(vaddr, AccessType::Fetch),
      Err(Exception::InstructionAccessFault(vaddr)),
    );
    assert_eq!(
      cpu.translate_to_host(vaddr, AccessType::Read),
      Err(Exception::LoadAccessFault(vaddr)),
    );
    assert_eq!(
      cpu.translate_to_host(vaddr, AccessType::Write),
      Err(Exception::StoreAccessFault(vaddr)),
    );
  }

  struct ReadOnlyPteDevice {
    base: VirtAddr,
    pte_addr: VirtAddr,
    pte: u64,
  }

  impl Device for ReadOnlyPteDevice {
    fn name(&self) -> &'static str {
      "read-only-pte-test"
    }

    fn vendor_id(&self) -> u16 {
      0
    }

    fn device_id(&self) -> u16 {
      0
    }

    fn init(&self) -> Result<Vec<(VirtAddr, VirtAddr)>, ()> {
      Ok(vec![(self.base, self.base + VirtAddr(PAGE_SIZE))])
    }

    fn destroy(&self) -> Result<(), ()> {
      Ok(())
    }

    fn dma_read(&self, _: VirtAddr) -> Option<&Memory> {
      None
    }

    fn dma_write(&self, _: VirtAddr) -> Option<&mut Memory> {
      None
    }

    fn mmio_read(&self, addr: VirtAddr) -> Option<u8> {
      let offset = addr.0.checked_sub(self.pte_addr.0)?;
      if offset >= std::mem::size_of::<u64>() as u64 {
        return None;
      }
      Some((self.pte >> (offset * 8)) as u8)
    }

    fn mmio_write(&self, _: VirtAddr, _: u8) -> Result<(), ()> {
      Err(())
    }

    fn is_interrupting(&self) -> Option<u64> {
      None
    }
  }

  #[test]
  fn accessed_bit_write_failure_uses_the_original_fetch_address() {
    const PTE_DEVICE_BASE: u64 = 0x4000_0000;

    let mut cpu = RV64Cpu::new(None);
    let vaddr = VirtAddr(TEST_VADDR);
    let vpn = VMMode::SV39.vpn(vaddr.0);
    let pte_addr = VirtAddr(PTE_DEVICE_BASE + vpn[2] * std::mem::size_of::<u64>() as u64);
    let pte = ((RV64_MEMORY_BASE >> PAGE_SHIFT) << 10)
      | pte_flag(PTE_V)
      | pte_flag(PTE_R)
      | pte_flag(PTE_X);
    unsafe {
      cpu
        .bus
        .add_device(Arc::new(ReadOnlyPteDevice {
          base: VirtAddr(PTE_DEVICE_BASE),
          pte_addr,
          pte,
        }))
        .unwrap();
    }
    cpu.mode = PrivilegeMode::Supervisor;
    cpu.vmmode = VMMode::SV39;
    cpu.vmppn = PTE_DEVICE_BASE;

    assert_eq!(
      cpu.translate_to_host(vaddr, AccessType::Fetch),
      Err(Exception::InstructionAccessFault(vaddr)),
    );
  }

  #[test]
  fn common_translation_walks_sv39_sv48_and_sv57() {
    let permissions = pte_flag(PTE_V)
      | pte_flag(PTE_R)
      | pte_flag(PTE_W)
      | pte_flag(PTE_X)
      | pte_flag(PTE_A)
      | pte_flag(PTE_D);

    for mode in [VMMode::SV39, VMMode::SV48, VMMode::SV57] {
      let (mut cpu, _) = mapped_cpu(mode, permissions);
      let vaddr = VirtAddr(TEST_VADDR + 0x234);
      let target = cpu.translate_to_host(vaddr, AccessType::Read).unwrap();
      match target {
        TranslationTarget::Dram {
          paddr,
          host_page,
          phys_page,
          permissions,
        } => {
          assert_eq!(paddr, VirtAddr(DATA_PAGE + 0x234));
          assert_eq!(phys_page, DATA_PAGE);
          assert_eq!(
            host_page,
            cpu.bus.mem.to_phys(VirtAddr(DATA_PAGE)).unwrap().0,
          );
          assert_eq!(
            permissions,
            TRANSLATION_READ | TRANSLATION_WRITE | TRANSLATION_EXECUTE,
          );
        }
        TranslationTarget::Mmio { .. } => panic!("mapped DRAM was classified as MMIO"),
      }
    }
  }

  #[test]
  fn interpreter_memory_helpers_consume_the_common_translation() {
    let flags = pte_flag(PTE_V)
      | pte_flag(PTE_R)
      | pte_flag(PTE_W)
      | pte_flag(PTE_X)
      | pte_flag(PTE_A)
      | pte_flag(PTE_D);
    let (mut cpu, _) = mapped_cpu(VMMode::SV39, flags);
    let vaddr = VirtAddr(TEST_VADDR + 4);
    let paddr = VirtAddr(DATA_PAGE + 4);

    cpu.bus.write(paddr, 0x1122_3343_u32).unwrap();
    assert_eq!(cpu.fetch_mem(vaddr), Ok(0x1122_3343));
    assert_eq!(cpu.read_mem::<u32>(vaddr), Ok(0x1122_3343));
    assert_eq!(cpu.write_mem(vaddr, 0xaabb_ccdd_u32), Ok(()));
    assert_eq!(cpu.bus.read::<u32>(paddr), Ok(0xaabb_ccdd));
  }

  #[test]
  fn cross_page_instruction_fetch_uses_the_second_guest_mapping() {
    let flags = pte_flag(PTE_V) | pte_flag(PTE_X) | pte_flag(PTE_A);
    let (mut cpu, first_leaf_addr) = mapped_cpu(VMMode::SV39, flags);
    install_adjacent_leaf(&mut cpu, first_leaf_addr, SECOND_DATA_PAGE, flags);
    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 2);

    cpu
      .bus
      .write::<u16>(VirtAddr(DATA_PAGE + PAGE_SIZE - 2), 0x0093)
      .unwrap();
    cpu
      .bus
      .write::<u16>(VirtAddr(DATA_PAGE + PAGE_SIZE), 0xdead)
      .unwrap();
    cpu
      .bus
      .write::<u16>(VirtAddr(SECOND_DATA_PAGE), 0x0010)
      .unwrap();

    assert_eq!(cpu.fetch_mem(vaddr), Ok(0x0010_0093));
  }

  #[test]
  fn cross_page_instruction_fetch_faults_at_the_unmapped_second_parcel() {
    let flags = pte_flag(PTE_V) | pte_flag(PTE_X) | pte_flag(PTE_A);
    let (mut cpu, _) = mapped_cpu(VMMode::SV39, flags);
    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 2);
    let second_vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE);
    cpu
      .bus
      .write::<u16>(VirtAddr(DATA_PAGE + PAGE_SIZE - 2), 0x0093)
      .unwrap();

    assert_eq!(
      cpu.fetch_mem(vaddr),
      Err(Exception::InstructionPageFault(second_vaddr)),
    );
  }

  #[test]
  fn cross_page_instruction_fetch_checks_execute_permission_on_the_second_page() {
    let executable = pte_flag(PTE_V) | pte_flag(PTE_X) | pte_flag(PTE_A);
    let not_executable = pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_A);
    let (mut cpu, first_leaf_addr) = mapped_cpu(VMMode::SV39, executable);
    install_adjacent_leaf(
      &mut cpu,
      first_leaf_addr,
      SECOND_DATA_PAGE,
      not_executable,
    );
    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 2);
    let second_vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE);
    cpu
      .bus
      .write::<u16>(VirtAddr(DATA_PAGE + PAGE_SIZE - 2), 0x0093)
      .unwrap();

    assert_eq!(
      cpu.fetch_mem(vaddr),
      Err(Exception::InstructionPageFault(second_vaddr)),
    );
  }

  #[test]
  fn compressed_instruction_at_page_end_does_not_touch_the_next_page() {
    let flags = pte_flag(PTE_V) | pte_flag(PTE_X) | pte_flag(PTE_A);
    let (mut cpu, _) = mapped_cpu(VMMode::SV39, flags);
    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 2);
    cpu
      .bus
      .write::<u16>(VirtAddr(DATA_PAGE + PAGE_SIZE - 2), 0x0001)
      .unwrap();

    assert_eq!(cpu.fetch_mem(vaddr), Ok(0x0000_0001));
  }

  #[test]
  fn instruction_fetch_uses_instruction_specific_access_and_alignment_faults() {
    let mut cpu = RV64Cpu::new(None);
    let unmapped = VirtAddr(0);
    let misaligned = VirtAddr(1);

    assert_eq!(
      cpu.fetch_mem(unmapped),
      Err(Exception::InstructionAccessFault(unmapped)),
    );
    assert_eq!(
      cpu.fetch_mem(misaligned),
      Err(Exception::InstructionAddressMisaligned(misaligned)),
    );
    assert_eq!(
      Exception::InstructionAccessFault(unmapped).mcause_mtval(&cpu),
      (1, 0),
    );
    assert_eq!(
      Exception::InstructionAddressMisaligned(misaligned).mcause_mtval(&cpu),
      (0, 1),
    );
  }

  #[test]
  fn translated_endpoint_access_faults_use_the_guest_address() {
    let flags = pte_flag(PTE_V)
      | pte_flag(PTE_R)
      | pte_flag(PTE_W)
      | pte_flag(PTE_X)
      | pte_flag(PTE_A)
      | pte_flag(PTE_D);
    let (mut cpu, leaf_addr) = mapped_cpu(VMMode::SV39, flags);
    let vaddr = VirtAddr(TEST_VADDR);
    cpu
      .bus
      .write(
        leaf_addr,
        ((RV64_MEMORY_END >> PAGE_SHIFT) << 10) | flags,
      )
      .unwrap();

    assert_eq!(
      cpu.fetch_mem(vaddr),
      Err(Exception::InstructionAccessFault(vaddr)),
    );
    assert_eq!(
      cpu.read_mem::<u32>(vaddr),
      Err(Exception::LoadAccessFault(vaddr)),
    );
    assert_eq!(
      cpu.write_mem(vaddr, 0x1122_3344_u32),
      Err(Exception::StoreAccessFault(vaddr)),
    );
  }

  #[test]
  fn cross_page_endpoint_faults_use_the_second_guest_fragment() {
    let flags =
      pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_W) | pte_flag(PTE_A) | pte_flag(PTE_D);
    let (mut cpu, first_leaf_addr) = mapped_cpu(VMMode::SV39, flags);
    install_adjacent_leaf(&mut cpu, first_leaf_addr, RV64_MEMORY_END, flags);
    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 2);
    let second_vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE);
    let first_paddr = VirtAddr(DATA_PAGE + PAGE_SIZE - 2);
    cpu.bus.write(first_paddr, 0x55aa_u16).unwrap();

    assert_eq!(
      cpu.read_mem::<u32>(vaddr),
      Err(Exception::LoadAccessFault(second_vaddr)),
    );
    assert_eq!(
      cpu.write_mem(vaddr, 0xaabb_ccdd_u32),
      Err(Exception::StoreAccessFault(second_vaddr)),
    );
    assert_eq!(cpu.bus.read::<u16>(first_paddr), Ok(0x55aa));
  }

  #[test]
  fn cross_page_access_translates_each_guest_page_independently() {
    let flags =
      pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_W) | pte_flag(PTE_A) | pte_flag(PTE_D);
    let (mut cpu, first_leaf_addr) = mapped_cpu(VMMode::SV39, flags);
    install_adjacent_leaf(&mut cpu, first_leaf_addr, SECOND_DATA_PAGE, flags);
    let in_page = cpu
      .translate_to_host(VirtAddr(TEST_VADDR + 8), AccessType::Read)
      .unwrap();
    let cross_page = cpu
      .translate_to_host(VirtAddr(TEST_VADDR + PAGE_SIZE - 2), AccessType::Read)
      .unwrap();

    assert!(in_page.direct_host_addr(8).is_some());
    assert!(cross_page.direct_host_addr(4).is_none());

    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 2);
    let first_paddr = VirtAddr(DATA_PAGE + PAGE_SIZE - 2);
    let second_paddr = VirtAddr(SECOND_DATA_PAGE);
    cpu.bus.write(first_paddr, 0x3344_u16).unwrap();
    cpu.bus.write(second_paddr, 0x1122_u16).unwrap();
    assert_eq!(cpu.read_mem::<u32>(vaddr), Ok(0x1122_3344));
    assert_eq!(cpu.write_mem(vaddr, 0xaabb_ccdd_u32), Ok(()));
    assert_eq!(cpu.bus.read::<u16>(first_paddr), Ok(0xccdd));
    assert_eq!(cpu.bus.read::<u16>(second_paddr), Ok(0xaabb));
  }

  #[test]
  fn cross_page_load_reports_a_fault_on_the_second_guest_page() {
    let flags = pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_A);
    let (mut cpu, _) = mapped_cpu(VMMode::SV39, flags);
    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 2);
    let second_vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE);

    assert_eq!(
      cpu.read_mem::<u32>(vaddr),
      Err(Exception::LoadPageFault(second_vaddr)),
    );
  }

  #[test]
  fn cross_page_store_fault_does_not_modify_the_first_page() {
    let flags =
      pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_W) | pte_flag(PTE_A) | pte_flag(PTE_D);
    let (mut cpu, _) = mapped_cpu(VMMode::SV39, flags);
    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 2);
    let second_vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE);
    let first_paddr = VirtAddr(DATA_PAGE + PAGE_SIZE - 2);
    cpu.bus.write(first_paddr, 0x55aa_u16).unwrap();

    assert_eq!(
      cpu.write_mem(vaddr, 0xaabb_ccdd_u32),
      Err(Exception::StorePageFault(second_vaddr)),
    );
    assert_eq!(cpu.bus.read::<u16>(first_paddr), Ok(0x55aa));
  }

  #[test]
  fn bare_cross_page_store_access_fault_does_not_modify_dram() {
    let mut cpu = RV64Cpu::new(None);
    let sentinel_addr = VirtAddr(RV64_MEMORY_END - std::mem::size_of::<u64>() as u64);
    let store_addr = VirtAddr(RV64_MEMORY_END - 4);
    let sentinel = 0x1122_3344_5566_7788_u64;
    cpu.bus.write(sentinel_addr, sentinel).unwrap();

    assert_eq!(
      cpu.write_mem(store_addr, 0xcafe_babe_dead_beef_u64),
      Err(Exception::StoreAccessFault(VirtAddr(RV64_MEMORY_END))),
    );
    assert_eq!(cpu.bus.read::<u64>(sentinel_addr), Ok(sentinel));
  }

  #[test]
  fn cross_page_mmio_fragment_stays_on_the_bus_path() {
    let flags =
      pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_W) | pte_flag(PTE_A) | pte_flag(PTE_D);
    let (mut cpu, first_leaf_addr) = mapped_cpu(VMMode::SV39, flags);
    install_adjacent_leaf(&mut cpu, first_leaf_addr, CLINT_BASE, flags);
    let vaddr = VirtAddr(TEST_VADDR + PAGE_SIZE - 1);
    let first_paddr = VirtAddr(DATA_PAGE + PAGE_SIZE - 1);
    let mmio_paddr = VirtAddr(CLINT_BASE);
    cpu.bus.write(first_paddr, 0x34_u8).unwrap();
    cpu.bus.write(mmio_paddr, 0x12_u8).unwrap();

    assert_eq!(cpu.read_mem::<u16>(vaddr), Ok(0x1234));
    assert_eq!(cpu.write_mem(vaddr, 0xabcd_u16), Ok(()));
    assert_eq!(cpu.bus.read::<u8>(first_paddr), Ok(0xcd));
    assert_eq!(cpu.bus.read::<u8>(mmio_paddr), Ok(0xab));
  }

  #[test]
  fn bare_and_machine_mode_bypass_page_tables_but_not_bus_classification() {
    let mut cpu = RV64Cpu::new(None);
    let dram = VirtAddr(RV64_MEMORY_BASE + 0x40);
    let mmio = VirtAddr(CLINT_BASE);
    let mtime = VirtAddr(CLINT_BASE + 0xbff8);

    assert!(matches!(
      cpu.translate_to_host(dram, AccessType::Read),
      Ok(TranslationTarget::Dram { paddr, .. }) if paddr == dram
    ));
    assert!(matches!(
      cpu.translate_to_host(mmio, AccessType::Read),
      Ok(TranslationTarget::Mmio { paddr, .. }) if paddr == mmio
    ));
    assert!(cpu
      .translate_to_host(mmio, AccessType::Read)
      .unwrap()
      .direct_host_addr(1)
      .is_none());
    assert_eq!(cpu.write_mem(mtime, 42_u64), Ok(()));
    assert!(matches!(cpu.read_mem::<u64>(mtime), Ok(value) if value >= 42));

    cpu.vmmode = VMMode::SV39;
    cpu.vmppn = 0;
    cpu.mode = PrivilegeMode::Machine;
    assert_eq!(cpu.translate(mmio, AccessType::Read), Ok(mmio));
  }

  #[cfg(feature = "trace")]
  #[test]
  fn interpreter_common_path_keeps_virtual_and_physical_memory_trace() {
    use crate::debug::trace::{MemTrace, Trace};

    let flags = pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_A);
    let mut cpu = RV64Cpu::new(Some(String::new()));
    cpu.mode = PrivilegeMode::Supervisor;
    install_mapping(&mut cpu, VMMode::SV39, flags);
    let vaddr = VirtAddr(TEST_VADDR + 8);
    let paddr = VirtAddr(DATA_PAGE + 8);
    cpu.bus.write(paddr, 0x55aa_u16).unwrap();

    assert_eq!(cpu.read_mem::<u16>(vaddr), Ok(0x55aa));
    assert!(matches!(
      cpu.journal.traces.borrow().last(),
      Some(Trace::Mem(MemTrace::Read(traced_vaddr, traced_paddr, 2, _)))
        if *traced_vaddr == vaddr && *traced_paddr == paddr
    ));
  }

  #[test]
  fn user_and_supervisor_permissions_honor_sum() {
    let user_flags = pte_flag(PTE_V)
      | pte_flag(PTE_R)
      | pte_flag(PTE_W)
      | pte_flag(PTE_X)
      | pte_flag(PTE_U)
      | pte_flag(PTE_A)
      | pte_flag(PTE_D);
    let (mut cpu, leaf_addr) = mapped_cpu(VMMode::SV39, user_flags);
    let vaddr = VirtAddr(TEST_VADDR);

    cpu.mode = PrivilegeMode::User;
    assert!(cpu.translate_to_host(vaddr, AccessType::Read).is_ok());
    assert!(cpu.translate_to_host(vaddr, AccessType::Write).is_ok());
    assert!(cpu.translate_to_host(vaddr, AccessType::Fetch).is_ok());

    cpu.mode = PrivilegeMode::Supervisor;
    cpu.csrs.write_mstatus_SUM(false);
    assert_page_fault(&mut cpu, AccessType::Read, Exception::LoadPageFault(vaddr));
    cpu.csrs.write_mstatus_SUM(true);
    assert!(cpu.translate_to_host(vaddr, AccessType::Read).is_ok());
    assert!(cpu.translate_to_host(vaddr, AccessType::Write).is_ok());
    assert_page_fault(
      &mut cpu,
      AccessType::Fetch,
      Exception::InstructionPageFault(vaddr),
    );

    let supervisor_flags = user_flags & !pte_flag(PTE_U);
    cpu
      .bus
      .write(
        leaf_addr,
        ((DATA_PAGE >> PAGE_SHIFT) << 10) | supervisor_flags,
      )
      .unwrap();
    cpu.mode = PrivilegeMode::User;
    assert_page_fault(&mut cpu, AccessType::Read, Exception::LoadPageFault(vaddr));
  }

  #[test]
  fn access_permissions_and_mxr_produce_typed_page_faults() {
    let read_only = pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_A) | pte_flag(PTE_D);
    let (mut cpu, leaf_addr) = mapped_cpu(VMMode::SV39, read_only);
    let vaddr = VirtAddr(TEST_VADDR);

    assert!(cpu.translate_to_host(vaddr, AccessType::Read).is_ok());
    assert_page_fault(
      &mut cpu,
      AccessType::Write,
      Exception::StorePageFault(vaddr),
    );
    assert_page_fault(
      &mut cpu,
      AccessType::Fetch,
      Exception::InstructionPageFault(vaddr),
    );

    let execute_only = pte_flag(PTE_V) | pte_flag(PTE_X) | pte_flag(PTE_A);
    cpu
      .bus
      .write(leaf_addr, ((DATA_PAGE >> PAGE_SHIFT) << 10) | execute_only)
      .unwrap();
    assert!(cpu.translate_to_host(vaddr, AccessType::Fetch).is_ok());
    cpu.csrs.write_mstatus_MXR(false);
    assert_page_fault(&mut cpu, AccessType::Read, Exception::LoadPageFault(vaddr));
    cpu.csrs.write_mstatus_MXR(true);
    assert!(cpu.translate_to_host(vaddr, AccessType::Read).is_ok());
  }

  #[test]
  fn translation_updates_accessed_and_dirty_bits() {
    let initial = pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_W);
    let (mut cpu, leaf_addr) = mapped_cpu(VMMode::SV39, initial);
    let vaddr = VirtAddr(TEST_VADDR);

    cpu.translate_to_host(vaddr, AccessType::Read).unwrap();
    let after_read = cpu.bus.read::<u64>(leaf_addr).unwrap();
    assert_ne!(after_read & pte_flag(PTE_A), 0);
    assert_eq!(after_read & pte_flag(PTE_D), 0);

    cpu.translate_to_host(vaddr, AccessType::Write).unwrap();
    let after_write = cpu.bus.read::<u64>(leaf_addr).unwrap();
    assert_ne!(after_write & pte_flag(PTE_A), 0);
    assert_ne!(after_write & pte_flag(PTE_D), 0);
  }

  #[test]
  fn mprv_uses_mpp_for_data_but_never_for_fetch() {
    let supervisor_flags = pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_X) | pte_flag(PTE_A);
    let (mut cpu, _) = mapped_cpu(VMMode::SV39, supervisor_flags);
    let vaddr = VirtAddr(TEST_VADDR);
    cpu.mode = PrivilegeMode::Machine;
    cpu.csrs.write_mstatus_MPP(PrivilegeMode::Supervisor);
    cpu.csrs.write_mstatus_MPRV(true);

    assert_eq!(
      cpu.translate(vaddr, AccessType::Read),
      Ok(VirtAddr(DATA_PAGE))
    );
    assert_eq!(cpu.translate(vaddr, AccessType::Fetch), Ok(vaddr));

    cpu.csrs.write_mstatus_MPP(PrivilegeMode::User);
    assert_page_fault(&mut cpu, AccessType::Read, Exception::LoadPageFault(vaddr));
  }

  #[test]
  fn noncanonical_virtual_addresses_fault_by_access_type() {
    let flags = pte_flag(PTE_V) | pte_flag(PTE_R) | pte_flag(PTE_W) | pte_flag(PTE_X);
    let (mut cpu, _) = mapped_cpu(VMMode::SV39, flags);
    let noncanonical = VirtAddr(1_u64 << 39);

    assert_eq!(
      cpu.translate_to_host(noncanonical, AccessType::Fetch),
      Err(Exception::InstructionPageFault(noncanonical)),
    );
    assert_eq!(
      cpu.translate_to_host(noncanonical, AccessType::Read),
      Err(Exception::LoadPageFault(noncanonical)),
    );
    assert_eq!(
      cpu.translate_to_host(noncanonical, AccessType::Write),
      Err(Exception::StorePageFault(noncanonical)),
    );
  }
}
