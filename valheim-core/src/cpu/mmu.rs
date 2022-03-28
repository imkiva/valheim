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

// Page table PPN shift amount
const PTE_PPN_SHIFT: u64 = 10;
// Page table PPN mask
const PTE_PPN_MASK: u64 = 0x3FFFFFFFFFFC00;

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
pub enum Reason {
  Fetch,
  Read,
  Write,
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
}

impl Reason {
  pub fn to_page_fault<T>(self, addr: VirtAddr) -> Result<T, Exception> {
    return match self {
      Reason::Fetch => Err(Exception::InstructionPageFault(addr)),
      Reason::Read => Err(Exception::LoadPageFault(addr)),
      Reason::Write => Err(Exception::StorePageFault(addr)),
    };
  }
}

impl RV64Cpu {
  #[inline(always)]
  pub fn fetch_mem(&self, addr: VirtAddr) -> Result<u32, Exception> {
    let paddr = self.translate(addr, Reason::Fetch)?;
    self.bus.read::<u32>(paddr)
  }

  #[inline(always)]
  pub fn read_mem<T: CanIO + Debug>(&self, addr: VirtAddr) -> Result<T, Exception> {
    // TODO: mstatus MPRV
    let paddr = self.translate(addr, Reason::Read)?;
    let val = self.bus.read::<T>(paddr);
    self.journal.trace(|| Trace::Mem(MemTrace::Read(addr, paddr, std::mem::size_of::<T>(), format!("{:?}", val))));
    val
  }

  #[inline(always)]
  pub fn write_mem<T: CanIO + Debug>(&mut self, addr: VirtAddr, val: T) -> Result<(), Exception> {
    // TODO: mstatus MPRV
    let paddr = self.translate(addr, Reason::Write)?;
    let res = self.bus.write::<T>(paddr, val);
    self.journal.trace(|| Trace::Mem(MemTrace::Write(addr, paddr, std::mem::size_of::<T>(), format!("{:?}", val))));
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

  pub fn translate(&self, addr: VirtAddr, reason: Reason) -> Result<VirtAddr, Exception> {
    if self.vmmode == VMMode::MBARE || self.mode == PrivilegeMode::Machine {
      return Ok(addr);
    }

    // 4.3.2 Virtual Address Translation Process
    let addr = addr.0;
    let (levels, ptidxbits, ptesize) = self.vmmode.translation_args().unwrap();
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
        pte = self.bus.read::<u64>(VirtAddr(a + vpn[i as usize] * ptesize))?;

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

      // TODO: check access rights according to 3.1.6.3 Memory Privilege in mstatus Register

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
        // TODO: PMA or PMP checks

        // Compare pte to the value of the PTE at address a + va.vpn[i] × PTESIZE.
        let compare = self.bus.read::<u64>(VirtAddr(a + vpn[i as usize] * ptesize))?;

        // If the values match, set pte.a to 1 and
        if compare == pte {
          pte = pte | (1 << PTE_A);
          // if the original memory access is a store, also set pte.d to 1.
          if reason == Reason::Write {
            pte = pte | (1 << PTE_D);
          }
          // note: and goto step 8
          break;
        } else {
          // If the comparison fails, return to step 2
          eprint!("[Valheim] translate: compare pte failed: {} != {}, returning to step 2\n", compare, pte);
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
    Ok(VirtAddr(paddr))
  }
}