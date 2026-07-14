use std::fmt::{Display, Formatter};
use std::mem::{offset_of, transmute};

use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
  types, AbiParam, FuncRef, InstBuilder, MemFlags, Type, UserFuncName, Value,
};
use cranelift_codegen::isa::CallConv;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{default_libcall_names, FuncId, Linkage, Module};
use valheim_asm::isa::rv32::RV32Instr;
use valheim_asm::isa::rv64::RV64Instr;
use valheim_asm::isa::typed::{Instr, Rd, Reg, Rs1, Rs2};
use valheim_core::cpu::RV64Cpu;

use crate::atomic::{jit_atomic, AtomicOp};
use crate::block::{GuestBlock, GuestInst};
use crate::memory::{
  jit_tlb_fill, TlbEntry, TlbStats, EXIT_SLOW_MEMORY, FAULT_NONE, TLB_ACCESS_READ,
  TLB_ACCESS_WRITE, TLB_INDEX_MASK, WIDTH_16, WIDTH_32, WIDTH_64, WIDTH_8,
};

pub type BlockEntry = unsafe extern "C" fn(*mut JitFrame) -> u32;

#[repr(C)]
pub struct JitFrame {
  pub cpu: *mut RV64Cpu,
  pub xregs: *mut u64,
  pub next_pc: u64,
  pub fault_pc: u64,
  pub fault_addr: u64,
  pub raw_instr: u32,
  pub attempted: u32,
  pub exit_kind: u32,
  pub load_tlb: *mut TlbEntry,
  pub store_tlb: *mut TlbEntry,
  pub tlb_generation: u64,
  pub tlb_stats: *mut TlbStats,
}

impl JitFrame {
  pub fn new(
    cpu: &mut RV64Cpu,
    xregs: *mut u64,
    load_tlb: *mut TlbEntry,
    store_tlb: *mut TlbEntry,
    tlb_generation: u64,
    tlb_stats: *mut TlbStats,
  ) -> Self {
    let next_pc = cpu.read_pc().0;
    Self {
      cpu,
      xregs,
      next_pc,
      fault_pc: 0,
      fault_addr: 0,
      raw_instr: 0,
      attempted: 0,
      exit_kind: 0,
      load_tlb,
      store_tlb,
      tlb_generation,
      tlb_stats,
    }
  }
}

#[derive(Clone, Copy)]
pub struct CompiledBlock {
  pub entry: BlockEntry,
  pub code_size: u64,
}

#[derive(Debug)]
pub struct JitError(String);

impl Display for JitError {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    f.write_str(&self.0)
  }
}

impl std::error::Error for JitError {}

pub struct CraneliftBackend {
  module: JITModule,
  context: cranelift_codegen::Context,
  function_context: FunctionBuilderContext,
  tlb_fill_helper: FuncId,
  atomic_helper: FuncId,
  code_bytes: u64,
}

impl CraneliftBackend {
  pub fn new() -> Result<Self, JitError> {
    let mut builder = JITBuilder::with_flags(&[("opt_level", "speed")], default_libcall_names())
      .map_err(|error| JitError(error.to_string()))?;
    builder.symbol("valheim_jit_tlb_fill", jit_tlb_fill as *const u8);
    builder.symbol("valheim_jit_atomic", jit_atomic as *const u8);
    let mut module = JITModule::new(builder);

    let pointer_type = module.target_config().pointer_type();
    let mut tlb_fill_signature = module.make_signature();
    tlb_fill_signature.call_conv = CallConv::SystemV;
    tlb_fill_signature.params.push(AbiParam::new(pointer_type));
    tlb_fill_signature.params.push(AbiParam::new(types::I64));
    tlb_fill_signature.params.push(AbiParam::new(types::I32));
    tlb_fill_signature.returns.push(AbiParam::new(types::I64));
    let tlb_fill_helper = module
      .declare_function("valheim_jit_tlb_fill", Linkage::Import, &tlb_fill_signature)
      .map_err(|error| JitError(error.to_string()))?;

    let mut atomic_signature = module.make_signature();
    atomic_signature.call_conv = CallConv::SystemV;
    atomic_signature.params.push(AbiParam::new(pointer_type));
    atomic_signature.params.push(AbiParam::new(types::I64));
    atomic_signature.params.push(AbiParam::new(types::I64));
    atomic_signature.params.push(AbiParam::new(types::I32));
    atomic_signature.returns.push(AbiParam::new(types::I64));
    let atomic_helper = module
      .declare_function("valheim_jit_atomic", Linkage::Import, &atomic_signature)
      .map_err(|error| JitError(error.to_string()))?;

    let context = module.make_context();
    Ok(Self {
      module,
      context,
      function_context: FunctionBuilderContext::new(),
      tlb_fill_helper,
      atomic_helper,
      code_bytes: 0,
    })
  }

  pub fn compile(
    &mut self,
    block: &GuestBlock,
    collect_tlb_stats: bool,
  ) -> Result<CompiledBlock, JitError> {
    let pointer_type = self.module.target_config().pointer_type();
    let mut signature = self.module.make_signature();
    signature.call_conv = CallConv::SystemV;
    signature.params.push(AbiParam::new(pointer_type));
    signature.returns.push(AbiParam::new(types::I32));

    let function_id = self
      .module
      .declare_anonymous_function(&signature)
      .map_err(|error| JitError(error.to_string()))?;
    self.context.func.signature = signature;
    self.context.func.name = UserFuncName::user(0, function_id.as_u32());
    let tlb_fill_helper = self
      .module
      .declare_func_in_func(self.tlb_fill_helper, &mut self.context.func);
    let atomic_helper = self
      .module
      .declare_func_in_func(self.atomic_helper, &mut self.context.func);

    {
      let mut builder = FunctionBuilder::new(&mut self.context.func, &mut self.function_context);
      let entry = builder.create_block();
      builder.append_block_params_for_function_params(entry);
      builder.switch_to_block(entry);
      builder.seal_block(entry);
      let frame = builder.block_params(entry)[0];
      let flags = MemFlags::trusted();
      let xregs = builder.ins().load(
        pointer_type,
        flags,
        frame,
        offset_of!(JitFrame, xregs) as i32,
      );
      let load_tlb = builder.ins().load(
        pointer_type,
        flags,
        frame,
        offset_of!(JitFrame, load_tlb) as i32,
      );
      let store_tlb = builder.ins().load(
        pointer_type,
        flags,
        frame,
        offset_of!(JitFrame, store_tlb) as i32,
      );
      let tlb_generation = builder.ins().load(
        types::I64,
        flags,
        frame,
        offset_of!(JitFrame, tlb_generation) as i32,
      );
      let tlb_stats = if collect_tlb_stats {
        Some(builder.ins().load(
          pointer_type,
          flags,
          frame,
          offset_of!(JitFrame, tlb_stats) as i32,
        ))
      } else {
        None
      };
      let mut lowering = Lowering::new(
        &mut builder,
        frame,
        xregs,
        flags,
        load_tlb,
        store_tlb,
        tlb_generation,
        tlb_stats,
        tlb_fill_helper,
        atomic_helper,
      );
      for (index, instruction) in block.instructions.iter().enumerate() {
        lowering.lower(*instruction, index as u32 + 1)?;
      }
      lowering.finish(block)?;
      builder.seal_all_blocks();
      builder.finalize();
    }

    self
      .module
      .define_function(function_id, &mut self.context)
      .map_err(|error| JitError(error.to_string()))?;
    let code_size = self
      .context
      .compiled_code()
      .map(|code| code.code_info().total_size as u64)
      .unwrap_or(0);
    self.code_bytes = self.code_bytes.saturating_add(code_size);
    self.module.clear_context(&mut self.context);
    self
      .module
      .finalize_definitions()
      .map_err(|error| JitError(error.to_string()))?;
    let pointer = self.module.get_finalized_function(function_id);
    let entry = unsafe { transmute::<*const u8, BlockEntry>(pointer) };
    Ok(CompiledBlock { entry, code_size })
  }

  pub fn code_bytes(&self) -> u64 {
    self.code_bytes
  }

  /// Releases every native function in this module at a dispatcher safe point.
  ///
  /// The runtime clears all `CompiledBlock` pointers before calling this method and does not use
  /// block chaining, so no generated code can retain an address into the old module.
  pub unsafe fn free_memory(self) {
    self.module.free_memory();
  }
}

struct Lowering<'a, 'b> {
  builder: &'a mut FunctionBuilder<'b>,
  frame: Value,
  xregs: Value,
  flags: MemFlags,
  load_tlb: Value,
  store_tlb: Value,
  tlb_generation: Value,
  tlb_stats: Option<Value>,
  tlb_fill_helper: FuncRef,
  atomic_helper: FuncRef,
  values: [Option<Value>; 32],
  dirty: [bool; 32],
  next_pc: Option<Value>,
}

impl<'a, 'b> Lowering<'a, 'b> {
  fn new(
    builder: &'a mut FunctionBuilder<'b>,
    frame: Value,
    xregs: Value,
    flags: MemFlags,
    load_tlb: Value,
    store_tlb: Value,
    tlb_generation: Value,
    tlb_stats: Option<Value>,
    tlb_fill_helper: FuncRef,
    atomic_helper: FuncRef,
  ) -> Self {
    Self {
      builder,
      frame,
      xregs,
      flags,
      load_tlb,
      store_tlb,
      tlb_generation,
      tlb_stats,
      tlb_fill_helper,
      atomic_helper,
      values: [None; 32],
      dirty: [false; 32],
      next_pc: None,
    }
  }

  fn iconst(&mut self, value: u64) -> Value {
    self.builder.ins().iconst(types::I64, value as i64)
  }

  fn reg_index(reg: Reg) -> Result<Option<usize>, JitError> {
    match reg {
      Reg::ZERO => Ok(None),
      Reg::X(index) if index.value() == 0 => Ok(None),
      Reg::X(index) => Ok(Some(index.value() as usize)),
      _ => Err(JitError(format!("unsupported integer register {reg:?}"))),
    }
  }

  fn read(&mut self, reg: Reg) -> Result<Value, JitError> {
    let Some(index) = Self::reg_index(reg)? else {
      return Ok(self.iconst(0));
    };
    if let Some(value) = self.values[index] {
      return Ok(value);
    }
    let value = self.builder.ins().load(
      types::I64,
      self.flags,
      self.xregs,
      (index * std::mem::size_of::<u64>()) as i32,
    );
    self.values[index] = Some(value);
    Ok(value)
  }

  fn write(&mut self, reg: Reg, value: Value) -> Result<(), JitError> {
    let Some(index) = Self::reg_index(reg)? else {
      return Ok(());
    };
    self.values[index] = Some(value);
    self.dirty[index] = true;
    Ok(())
  }

  fn bool_to_u64(&mut self, value: Value) -> Value {
    self.builder.ins().uextend(types::I64, value)
  }

  fn branch_target(&mut self, inst: GuestInst, condition: Value, offset: i32) {
    let taken = self.iconst(inst.pc.wrapping_add(offset as i64 as u64));
    let fallthrough = self.iconst(inst.pc.wrapping_add(inst.len as u64));
    self.next_pc = Some(self.builder.ins().select(condition, taken, fallthrough));
  }

  fn store_frame_i32(&mut self, offset: usize, value: u32) {
    let value = self.builder.ins().iconst(types::I32, value as i64);
    self
      .builder
      .ins()
      .store(self.flags, value, self.frame, offset as i32);
  }

  fn store_frame_i64(&mut self, offset: usize, value: u64) {
    let value = self.iconst(value);
    self
      .builder
      .ins()
      .store(self.flags, value, self.frame, offset as i32);
  }

  fn prepare_memory_access(&mut self, inst: GuestInst, attempted: u32) {
    self.store_frame_i32(offset_of!(JitFrame, exit_kind), FAULT_NONE);
    self.store_frame_i64(offset_of!(JitFrame, fault_pc), inst.pc);
    self.store_frame_i32(offset_of!(JitFrame, raw_instr), inst.raw);
    self.store_frame_i32(offset_of!(JitFrame, attempted), attempted);
  }

  fn flush_dirty_registers(&mut self) {
    for index in 1..32 {
      if !self.dirty[index] {
        continue;
      }
      self.builder.ins().store(
        self.flags,
        self.values[index].expect("dirty register has no value"),
        self.xregs,
        (index * std::mem::size_of::<u64>()) as i32,
      );
    }
  }

  fn return_memory_exit(&mut self, attempted: u32) {
    self.flush_dirty_registers();
    let fault_pc = self.builder.ins().load(
      types::I64,
      self.flags,
      self.frame,
      offset_of!(JitFrame, fault_pc) as i32,
    );
    self.builder.ins().store(
      self.flags,
      fault_pc,
      self.frame,
      offset_of!(JitFrame, next_pc) as i32,
    );
    let attempted = self.builder.ins().iconst(types::I32, attempted as i64);
    self.builder.ins().return_(&[attempted]);
  }

  fn branch_after_fallible_helper(&mut self, attempted: u32) {
    let exit_kind = self.builder.ins().load(
      types::I32,
      self.flags,
      self.frame,
      offset_of!(JitFrame, exit_kind) as i32,
    );
    let failed = self.builder.ins().icmp_imm(IntCC::NotEqual, exit_kind, 0);
    let exit = self.builder.create_block();
    let resume = self.builder.create_block();
    self.builder.ins().brif(failed, exit, &[], resume, &[]);

    self.builder.switch_to_block(exit);
    self.return_memory_exit(attempted);

    self.builder.switch_to_block(resume);
  }

  fn increment_tlb_stat(&mut self, offset: usize) {
    let Some(tlb_stats) = self.tlb_stats else {
      return;
    };
    let old = self
      .builder
      .ins()
      .load(types::I64, self.flags, tlb_stats, offset as i32);
    let new = self.builder.ins().iadd_imm(old, 1);
    self
      .builder
      .ins()
      .store(self.flags, new, tlb_stats, offset as i32);
  }

  fn tlb_host_address(
    &mut self,
    inst: GuestInst,
    attempted: u32,
    address: Value,
    width: u32,
    access: u32,
  ) -> Value {
    self.prepare_memory_access(inst, attempted);

    let page_offset = self.builder.ins().band_imm(address, 0xfff);
    let width_bytes = 1_u64 << width;
    let last_direct_offset = 4096_u64 - width_bytes;
    let crosses_page = self.builder.ins().icmp_imm(
      IntCC::UnsignedGreaterThan,
      page_offset,
      last_direct_offset as i64,
    );
    let slow = self.builder.create_block();
    let lookup = self.builder.create_block();
    self
      .builder
      .ins()
      .brif(crosses_page, slow, &[], lookup, &[]);

    self.builder.switch_to_block(slow);
    self.store_frame_i32(offset_of!(JitFrame, exit_kind), EXIT_SLOW_MEMORY);
    self.increment_tlb_stat(offset_of!(TlbStats, slow_paths));
    self.return_memory_exit(attempted);

    self.builder.switch_to_block(lookup);
    let tag = self.builder.ins().ushr_imm(address, 12);
    let index = self.builder.ins().band_imm(tag, TLB_INDEX_MASK as i64);
    let entry_offset = self
      .builder
      .ins()
      .imul_imm(index, std::mem::size_of::<TlbEntry>() as i64);
    let table = match access {
      TLB_ACCESS_READ => self.load_tlb,
      TLB_ACCESS_WRITE => self.store_tlb,
      _ => unreachable!(),
    };
    let entry = self.builder.ins().iadd(table, entry_offset);
    let cached_tag = self.builder.ins().load(
      types::I64,
      self.flags,
      entry,
      offset_of!(TlbEntry, tag) as i32,
    );
    let cached_generation = self.builder.ins().load(
      types::I64,
      self.flags,
      entry,
      offset_of!(TlbEntry, generation) as i32,
    );
    let tag_matches = self.builder.ins().icmp(IntCC::Equal, tag, cached_tag);
    let generation_matches =
      self
        .builder
        .ins()
        .icmp(IntCC::Equal, self.tlb_generation, cached_generation);
    let hit = self.builder.ins().band(tag_matches, generation_matches);
    let hit_block = self.builder.create_block();
    let miss_block = self.builder.create_block();
    let ready = self.builder.create_block();
    let ready_host_page = self.builder.append_block_param(ready, types::I64);
    self
      .builder
      .ins()
      .brif(hit, hit_block, &[], miss_block, &[]);

    self.builder.switch_to_block(hit_block);
    self.increment_tlb_stat(offset_of!(TlbStats, hits));
    let host_page = self.builder.ins().load(
      types::I64,
      self.flags,
      entry,
      offset_of!(TlbEntry, host_page) as i32,
    );
    self.builder.ins().jump(ready, &[host_page]);

    self.builder.switch_to_block(miss_block);
    let access_value = self.builder.ins().iconst(types::I32, access as i64);
    let call = self
      .builder
      .ins()
      .call(self.tlb_fill_helper, &[self.frame, address, access_value]);
    let filled_host_page = self.builder.inst_results(call)[0];
    let exit_kind = self.builder.ins().load(
      types::I32,
      self.flags,
      self.frame,
      offset_of!(JitFrame, exit_kind) as i32,
    );
    let failed = self.builder.ins().icmp_imm(IntCC::NotEqual, exit_kind, 0);
    let miss_exit = self.builder.create_block();
    let miss_ready = self.builder.create_block();
    self
      .builder
      .ins()
      .brif(failed, miss_exit, &[], miss_ready, &[]);

    self.builder.switch_to_block(miss_exit);
    self.return_memory_exit(attempted);

    self.builder.switch_to_block(miss_ready);
    self.builder.ins().jump(ready, &[filled_host_page]);

    self.builder.switch_to_block(ready);
    self.builder.ins().iadd(ready_host_page, page_offset)
  }

  fn address(&mut self, rs1: Reg, immediate: i32) -> Result<Value, JitError> {
    let base = self.read(rs1)?;
    let offset = self.iconst(immediate as i64 as u64);
    Ok(self.builder.ins().iadd(base, offset))
  }

  fn signed_divrem(&mut self, ty: Type, lhs: Value, rhs: Value, remainder: bool) -> Value {
    let zero = self.builder.ins().icmp_imm(IntCC::Equal, rhs, 0);
    let min_value = match ty {
      types::I32 => i32::MIN as i64,
      types::I64 => i64::MIN,
      _ => unreachable!(),
    };
    let is_min = self.builder.ins().icmp_imm(IntCC::Equal, lhs, min_value);
    let is_negative_one = self.builder.ins().icmp_imm(IntCC::Equal, rhs, -1);
    let overflow = self.builder.ins().band(is_min, is_negative_one);
    let special = self.builder.ins().bor(zero, overflow);
    let one = self.builder.ins().iconst(ty, 1);
    let safe_rhs = self.builder.ins().select(special, one, rhs);
    let normal = if remainder {
      self.builder.ins().srem(lhs, safe_rhs)
    } else {
      self.builder.ins().sdiv(lhs, safe_rhs)
    };
    let divide_by_zero = if remainder {
      lhs
    } else {
      self.builder.ins().iconst(ty, -1)
    };
    let signed_overflow = if remainder {
      self.builder.ins().iconst(ty, 0)
    } else {
      lhs
    };
    let special_value = self
      .builder
      .ins()
      .select(zero, divide_by_zero, signed_overflow);
    self.builder.ins().select(special, special_value, normal)
  }

  fn unsigned_divrem(&mut self, ty: Type, lhs: Value, rhs: Value, remainder: bool) -> Value {
    let zero = self.builder.ins().icmp_imm(IntCC::Equal, rhs, 0);
    let one = self.builder.ins().iconst(ty, 1);
    let safe_rhs = self.builder.ins().select(zero, one, rhs);
    let normal = if remainder {
      self.builder.ins().urem(lhs, safe_rhs)
    } else {
      self.builder.ins().udiv(lhs, safe_rhs)
    };
    let divide_by_zero = if remainder {
      lhs
    } else {
      self.builder.ins().iconst(ty, -1)
    };
    self.builder.ins().select(zero, divide_by_zero, normal)
  }

  fn atomic(
    &mut self,
    inst: GuestInst,
    attempted: u32,
    rd: Reg,
    rs1: Reg,
    rs2: Option<Reg>,
    operation: AtomicOp,
  ) -> Result<(), JitError> {
    let address = self.read(rs1)?;
    let operand = match rs2 {
      Some(rs2) => self.read(rs2)?,
      None => self.iconst(0),
    };
    self.prepare_memory_access(inst, attempted);
    let operation = self
      .builder
      .ins()
      .iconst(types::I32, operation as u32 as i64);
    let call = self.builder.ins().call(
      self.atomic_helper,
      &[self.frame, address, operand, operation],
    );
    let result = self.builder.inst_results(call)[0];
    self.branch_after_fallible_helper(attempted);
    self.write(rd, result)
  }

  fn load(
    &mut self,
    inst: GuestInst,
    attempted: u32,
    rd: Reg,
    rs1: Reg,
    immediate: i32,
    width: u32,
    signed: bool,
  ) -> Result<(), JitError> {
    let address = self.address(rs1, immediate)?;
    let host_address = self.tlb_host_address(inst, attempted, address, width, TLB_ACCESS_READ);
    let load_type = match width {
      WIDTH_8 => types::I8,
      WIDTH_16 => types::I16,
      WIDTH_32 => types::I32,
      WIDTH_64 => types::I64,
      _ => return Err(JitError(format!("invalid load width {width}"))),
    };
    let value = self
      .builder
      .ins()
      .load(load_type, self.flags, host_address, 0);
    let value = match (width, signed) {
      (WIDTH_8 | WIDTH_16 | WIDTH_32, true) => self.builder.ins().sextend(types::I64, value),
      (WIDTH_8 | WIDTH_16 | WIDTH_32, false) => self.builder.ins().uextend(types::I64, value),
      (WIDTH_64, _) => value,
      _ => return Err(JitError(format!("invalid load width {width}"))),
    };
    self.write(rd, value)
  }

  fn store(
    &mut self,
    inst: GuestInst,
    attempted: u32,
    rs1: Reg,
    rs2: Reg,
    immediate: i32,
    width: u32,
  ) -> Result<(), JitError> {
    let address = self.address(rs1, immediate)?;
    let value = self.read(rs2)?;
    let host_address = self.tlb_host_address(inst, attempted, address, width, TLB_ACCESS_WRITE);
    let value = match width {
      WIDTH_8 => self.builder.ins().ireduce(types::I8, value),
      WIDTH_16 => self.builder.ins().ireduce(types::I16, value),
      WIDTH_32 => self.builder.ins().ireduce(types::I32, value),
      WIDTH_64 => value,
      _ => return Err(JitError(format!("invalid store width {width}"))),
    };
    self.builder.ins().store(self.flags, value, host_address, 0);
    Ok(())
  }

  fn lower(&mut self, inst: GuestInst, attempted: u32) -> Result<(), JitError> {
    use RV32Instr as I32;
    use RV64Instr as I64;

    let default_next = inst.pc.wrapping_add(inst.len as u64);
    self.next_pc = Some(self.iconst(default_next));
    match inst.decoded {
      Instr::NOP | Instr::RV32(I32::FENCE(..) | I32::FENCE_TSO) => (),
      Instr::RV32(I32::LUI(Rd(rd), imm)) => {
        let value = self.iconst(imm.decode() as i32 as i64 as u64);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::AUIPC(Rd(rd), imm)) => {
        let value = self.iconst(inst.pc.wrapping_add(imm.decode() as i32 as i64 as u64));
        self.write(rd, value)?;
      }
      Instr::RV32(I32::JAL(Rd(rd), imm)) => {
        let link = self.iconst(default_next);
        self.write(rd, link)?;
        self.next_pc = Some(self.iconst(inst.pc.wrapping_add(imm.decode_sext() as i64 as u64)));
      }
      Instr::RV32(I32::JALR(Rd(rd), Rs1(rs1), imm)) => {
        let base = self.read(rs1)?;
        let offset = self.iconst(imm.decode_sext() as i64 as u64);
        let target = self.builder.ins().iadd(base, offset);
        let target = self.builder.ins().band_imm(target, -2);
        let link = self.iconst(default_next);
        self.write(rd, link)?;
        self.next_pc = Some(target);
      }
      Instr::RV32(I32::BEQ(Rs1(a), Rs2(b), imm)) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let condition = self.builder.ins().icmp(IntCC::Equal, a, b);
        self.branch_target(inst, condition, imm.decode_sext());
      }
      Instr::RV32(I32::BNE(Rs1(a), Rs2(b), imm)) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let condition = self.builder.ins().icmp(IntCC::NotEqual, a, b);
        self.branch_target(inst, condition, imm.decode_sext());
      }
      Instr::RV32(I32::BLT(Rs1(a), Rs2(b), imm)) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let condition = self.builder.ins().icmp(IntCC::SignedLessThan, a, b);
        self.branch_target(inst, condition, imm.decode_sext());
      }
      Instr::RV32(I32::BGE(Rs1(a), Rs2(b), imm)) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let condition = self
          .builder
          .ins()
          .icmp(IntCC::SignedGreaterThanOrEqual, a, b);
        self.branch_target(inst, condition, imm.decode_sext());
      }
      Instr::RV32(I32::BLTU(Rs1(a), Rs2(b), imm)) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let condition = self.builder.ins().icmp(IntCC::UnsignedLessThan, a, b);
        self.branch_target(inst, condition, imm.decode_sext());
      }
      Instr::RV32(I32::BGEU(Rs1(a), Rs2(b), imm)) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let condition = self
          .builder
          .ins()
          .icmp(IntCC::UnsignedGreaterThanOrEqual, a, b);
        self.branch_target(inst, condition, imm.decode_sext());
      }
      Instr::RV32(I32::ADDI(Rd(rd), Rs1(rs1), imm)) => {
        let lhs = self.read(rs1)?;
        let rhs = self.iconst(imm.decode_sext() as i64 as u64);
        let value = self.builder.ins().iadd(lhs, rhs);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SLTI(Rd(rd), Rs1(rs1), imm)) => {
        let lhs = self.read(rs1)?;
        let rhs = self.iconst(imm.decode_sext() as i64 as u64);
        let value = self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs);
        let value = self.bool_to_u64(value);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SLTIU(Rd(rd), Rs1(rs1), imm)) => {
        let lhs = self.read(rs1)?;
        let rhs = self.iconst(imm.decode_sext() as i64 as u64);
        let value = self.builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs);
        let value = self.bool_to_u64(value);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::XORI(Rd(rd), Rs1(rs1), imm)) => {
        let lhs = self.read(rs1)?;
        let rhs = self.iconst(imm.decode_sext() as i64 as u64);
        let value = self.builder.ins().bxor(lhs, rhs);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::ORI(Rd(rd), Rs1(rs1), imm)) => {
        let lhs = self.read(rs1)?;
        let rhs = self.iconst(imm.decode_sext() as i64 as u64);
        let value = self.builder.ins().bor(lhs, rhs);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::ANDI(Rd(rd), Rs1(rs1), imm)) => {
        let lhs = self.read(rs1)?;
        let rhs = self.iconst(imm.decode_sext() as i64 as u64);
        let value = self.builder.ins().band(lhs, rhs);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SLLI(Rd(rd), Rs1(rs1), shamt))
      | Instr::RV64(I64::SLLI(Rd(rd), Rs1(rs1), shamt)) => {
        let value = self.read(rs1)?;
        let shift = self.iconst((shamt.0 & 63) as u64);
        let value = self.builder.ins().ishl(value, shift);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SRLI(Rd(rd), Rs1(rs1), shamt))
      | Instr::RV64(I64::SRLI(Rd(rd), Rs1(rs1), shamt)) => {
        let value = self.read(rs1)?;
        let shift = self.iconst((shamt.0 & 63) as u64);
        let value = self.builder.ins().ushr(value, shift);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SRAI(Rd(rd), Rs1(rs1), shamt))
      | Instr::RV64(I64::SRAI(Rd(rd), Rs1(rs1), shamt)) => {
        let value = self.read(rs1)?;
        let shift = self.iconst((shamt.0 & 63) as u64);
        let value = self.builder.ins().sshr(value, shift);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::ADD(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().iadd(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SUB(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().isub(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SLL(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let b = self.builder.ins().band_imm(b, 63);
        let value = self.builder.ins().ishl(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SLT(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().icmp(IntCC::SignedLessThan, a, b);
        let value = self.bool_to_u64(value);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SLTU(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().icmp(IntCC::UnsignedLessThan, a, b);
        let value = self.bool_to_u64(value);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::XOR(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().bxor(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SRL(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let b = self.builder.ins().band_imm(b, 63);
        let value = self.builder.ins().ushr(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::SRA(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let b = self.builder.ins().band_imm(b, 63);
        let value = self.builder.ins().sshr(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::OR(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().bor(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::AND(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().band(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::MUL(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().imul(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::MULH(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().smulhi(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::MULHSU(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let unsigned_high = self.builder.ins().umulhi(a, b);
        let sign_mask = self.builder.ins().sshr_imm(a, 63);
        let correction = self.builder.ins().band(sign_mask, b);
        let value = self.builder.ins().isub(unsigned_high, correction);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::MULHU(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let value = self.builder.ins().umulhi(a, b);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::DIV(Rd(rd), Rs1(a), Rs2(b)))
      | Instr::RV32(I32::REM(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let remainder = matches!(inst.decoded, Instr::RV32(I32::REM(..)));
        let value = self.signed_divrem(types::I64, a, b, remainder);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::DIVU(Rd(rd), Rs1(a), Rs2(b)))
      | Instr::RV32(I32::REMU(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let remainder = matches!(inst.decoded, Instr::RV32(I32::REMU(..)));
        let value = self.unsigned_divrem(types::I64, a, b, remainder);
        self.write(rd, value)?;
      }
      Instr::RV32(I32::LR_W(Rd(rd), Rs1(rs1), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, None, AtomicOp::LrW)?;
      }
      Instr::RV32(I32::SC_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::ScW)?;
      }
      Instr::RV32(I32::AMOSWAP_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::SwapW)?;
      }
      Instr::RV32(I32::AMOADD_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::AddW)?;
      }
      Instr::RV32(I32::AMOXOR_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::XorW)?;
      }
      Instr::RV32(I32::AMOAND_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::AndW)?;
      }
      Instr::RV32(I32::AMOOR_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::OrW)?;
      }
      Instr::RV32(I32::AMOMIN_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::MinW)?;
      }
      Instr::RV32(I32::AMOMAX_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::MaxW)?;
      }
      Instr::RV32(I32::AMOMINU_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::MinuW)?;
      }
      Instr::RV32(I32::AMOMAXU_W(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::MaxuW)?;
      }
      Instr::RV32(I32::LB(Rd(rd), Rs1(rs1), imm)) => {
        self.load(inst, attempted, rd, rs1, imm.decode_sext(), WIDTH_8, true)?;
      }
      Instr::RV32(I32::LH(Rd(rd), Rs1(rs1), imm)) => {
        self.load(inst, attempted, rd, rs1, imm.decode_sext(), WIDTH_16, true)?;
      }
      Instr::RV32(I32::LW(Rd(rd), Rs1(rs1), imm)) => {
        self.load(inst, attempted, rd, rs1, imm.decode_sext(), WIDTH_32, true)?;
      }
      Instr::RV32(I32::LBU(Rd(rd), Rs1(rs1), imm)) => {
        self.load(inst, attempted, rd, rs1, imm.decode_sext(), WIDTH_8, false)?;
      }
      Instr::RV32(I32::LHU(Rd(rd), Rs1(rs1), imm)) => {
        self.load(inst, attempted, rd, rs1, imm.decode_sext(), WIDTH_16, false)?;
      }
      Instr::RV64(I64::LWU(Rd(rd), Rs1(rs1), imm)) => {
        self.load(inst, attempted, rd, rs1, imm.decode_sext(), WIDTH_32, false)?;
      }
      Instr::RV64(I64::LD(Rd(rd), Rs1(rs1), imm)) => {
        self.load(inst, attempted, rd, rs1, imm.decode_sext(), WIDTH_64, false)?;
      }
      Instr::RV32(I32::SB(Rs1(rs1), Rs2(rs2), imm)) => {
        self.store(inst, attempted, rs1, rs2, imm.decode_sext(), WIDTH_8)?;
      }
      Instr::RV32(I32::SH(Rs1(rs1), Rs2(rs2), imm)) => {
        self.store(inst, attempted, rs1, rs2, imm.decode_sext(), WIDTH_16)?;
      }
      Instr::RV32(I32::SW(Rs1(rs1), Rs2(rs2), imm)) => {
        self.store(inst, attempted, rs1, rs2, imm.decode_sext(), WIDTH_32)?;
      }
      Instr::RV64(I64::SD(Rs1(rs1), Rs2(rs2), imm)) => {
        self.store(inst, attempted, rs1, rs2, imm.decode_sext(), WIDTH_64)?;
      }
      Instr::RV64(I64::MULW(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let a = self.builder.ins().ireduce(types::I32, a);
        let b = self.builder.ins().ireduce(types::I32, b);
        let value = self.builder.ins().imul(a, b);
        let value = self.builder.ins().sextend(types::I64, value);
        self.write(rd, value)?;
      }
      Instr::RV64(I64::DIVW(Rd(rd), Rs1(a), Rs2(b)))
      | Instr::RV64(I64::REMW(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let a = self.builder.ins().ireduce(types::I32, a);
        let b = self.builder.ins().ireduce(types::I32, b);
        let remainder = matches!(inst.decoded, Instr::RV64(I64::REMW(..)));
        let value = self.signed_divrem(types::I32, a, b, remainder);
        let value = self.builder.ins().sextend(types::I64, value);
        self.write(rd, value)?;
      }
      Instr::RV64(I64::DIVUW(Rd(rd), Rs1(a), Rs2(b)))
      | Instr::RV64(I64::REMUW(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let a = self.builder.ins().ireduce(types::I32, a);
        let b = self.builder.ins().ireduce(types::I32, b);
        let remainder = matches!(inst.decoded, Instr::RV64(I64::REMUW(..)));
        let value = self.unsigned_divrem(types::I32, a, b, remainder);
        let value = self.builder.ins().sextend(types::I64, value);
        self.write(rd, value)?;
      }
      Instr::RV64(I64::LR_D(Rd(rd), Rs1(rs1), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, None, AtomicOp::LrD)?;
      }
      Instr::RV64(I64::SC_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::ScD)?;
      }
      Instr::RV64(I64::AMOSWAP_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::SwapD)?;
      }
      Instr::RV64(I64::AMOADD_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::AddD)?;
      }
      Instr::RV64(I64::AMOXOR_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::XorD)?;
      }
      Instr::RV64(I64::AMOAND_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::AndD)?;
      }
      Instr::RV64(I64::AMOOR_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::OrD)?;
      }
      Instr::RV64(I64::AMOMIN_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::MinD)?;
      }
      Instr::RV64(I64::AMOMAX_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::MaxD)?;
      }
      Instr::RV64(I64::AMOMINU_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::MinuD)?;
      }
      Instr::RV64(I64::AMOMAXU_D(Rd(rd), Rs1(rs1), Rs2(rs2), _, _)) => {
        self.atomic(inst, attempted, rd, rs1, Some(rs2), AtomicOp::MaxuD)?;
      }
      Instr::RV64(I64::ADDIW(Rd(rd), Rs1(rs1), imm)) => {
        let lhs = self.read(rs1)?;
        let lhs = self.builder.ins().ireduce(types::I32, lhs);
        let rhs = self
          .builder
          .ins()
          .iconst(types::I32, imm.decode_sext() as i64);
        let value = self.builder.ins().iadd(lhs, rhs);
        let value = self.builder.ins().sextend(types::I64, value);
        self.write(rd, value)?;
      }
      Instr::RV64(I64::SLLIW(Rd(rd), Rs1(rs1), shamt)) => {
        let value = self.read(rs1)?;
        let value = self.builder.ins().ireduce(types::I32, value);
        let shift = self.builder.ins().iconst(types::I32, (shamt.0 & 31) as i64);
        let value = self.builder.ins().ishl(value, shift);
        let value = self.builder.ins().sextend(types::I64, value);
        self.write(rd, value)?;
      }
      Instr::RV64(I64::SRLIW(Rd(rd), Rs1(rs1), shamt)) => {
        let value = self.read(rs1)?;
        let value = self.builder.ins().ireduce(types::I32, value);
        let shift = self.builder.ins().iconst(types::I32, (shamt.0 & 31) as i64);
        let value = self.builder.ins().ushr(value, shift);
        let value = self.builder.ins().sextend(types::I64, value);
        self.write(rd, value)?;
      }
      Instr::RV64(I64::SRAIW(Rd(rd), Rs1(rs1), shamt)) => {
        let value = self.read(rs1)?;
        let value = self.builder.ins().ireduce(types::I32, value);
        let shift = self.builder.ins().iconst(types::I32, (shamt.0 & 31) as i64);
        let value = self.builder.ins().sshr(value, shift);
        let value = self.builder.ins().sextend(types::I64, value);
        self.write(rd, value)?;
      }
      Instr::RV64(I64::ADDW(Rd(rd), Rs1(a), Rs2(b)))
      | Instr::RV64(I64::SUBW(Rd(rd), Rs1(a), Rs2(b)))
      | Instr::RV64(I64::SLLW(Rd(rd), Rs1(a), Rs2(b)))
      | Instr::RV64(I64::SRLW(Rd(rd), Rs1(a), Rs2(b)))
      | Instr::RV64(I64::SRAW(Rd(rd), Rs1(a), Rs2(b))) => {
        let a = self.read(a)?;
        let b = self.read(b)?;
        let a = self.builder.ins().ireduce(types::I32, a);
        let b = self.builder.ins().ireduce(types::I32, b);
        let value = match inst.decoded {
          Instr::RV64(I64::ADDW(..)) => self.builder.ins().iadd(a, b),
          Instr::RV64(I64::SUBW(..)) => self.builder.ins().isub(a, b),
          Instr::RV64(I64::SLLW(..)) => self.builder.ins().ishl(a, b),
          Instr::RV64(I64::SRLW(..)) => self.builder.ins().ushr(a, b),
          Instr::RV64(I64::SRAW(..)) => self.builder.ins().sshr(a, b),
          _ => unreachable!(),
        };
        let value = self.builder.ins().sextend(types::I64, value);
        self.write(rd, value)?;
      }
      unsupported => {
        return Err(JitError(format!(
          "unsupported baseline instruction {unsupported:?}"
        )))
      }
    }
    Ok(())
  }

  fn finish(&mut self, block: &GuestBlock) -> Result<(), JitError> {
    self.flush_dirty_registers();
    let next_pc = match self.next_pc {
      Some(next_pc) => next_pc,
      None => self.iconst(block.start_pc),
    };
    self.builder.ins().store(
      self.flags,
      next_pc,
      self.frame,
      offset_of!(JitFrame, next_pc) as i32,
    );
    let attempted = block.instructions.len() as u32;
    let attempted_value = self.builder.ins().iconst(types::I32, attempted as i64);
    self.builder.ins().store(
      self.flags,
      attempted_value,
      self.frame,
      offset_of!(JitFrame, attempted) as i32,
    );
    self.builder.ins().return_(&[attempted_value]);
    Ok(())
  }
}
