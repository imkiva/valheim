use std::cell::Cell;
use std::rc::Rc;

use valheim_asm::asm::encode32::Encode32;
use valheim_asm::isa::data::Fin;
use valheim_asm::isa::rv32::RV32Instr;
use valheim_asm::isa::rv64::RV64Instr;
use valheim_asm::isa::typed::{Imm32, Rd, Reg, Rs1, Rs2};
use valheim_core::cpu::bus::{CLINT_BASE, RV64_MEMORY_BASE, RV64_MEMORY_END};
use valheim_core::cpu::csr::CSRMap::{
  MCAUSE, MEPC, MIE, MIP, MSTATUS, MTIE_MASK, MTIP_MASK, MTVEC, SATP, TIME,
};
use valheim_core::cpu::irq::Exception;
use valheim_core::cpu::RV64Cpu;
use valheim_core::interp::naive::NaiveInterpreter;
use valheim_core::interp::{ExecOutcome, RV64Executor};
use valheim_core::machine::Machine;
use valheim_core::memory::VirtAddr;
use valheim_jit::JitExecutor;

const PROGRAM_PC: u64 = RV64_MEMORY_BASE + 0x20_000;
const DATA_ADDR: u64 = RV64_MEMORY_BASE + 0x40_000;
const JAL_X0_TO_SELF: u32 = 0x0000_006f;
// Use the architectural encoding directly so this test cannot accidentally execute SRET.
const WFI: u32 = 0x1050_0073;

#[derive(Debug)]
struct Snapshot {
  outcome: ExecOutcome,
  pc: VirtAddr,
  xregs: [u64; 32],
  instr: u64,
  mstatus: u64,
  satp: u64,
  ram: Vec<Vec<u8>>,
}

struct ObservedJit {
  inner: JitExecutor,
  native_executions: Rc<Cell<u64>>,
}

impl RV64Executor for ObservedJit {
  fn execute(&mut self, cpu: &mut RV64Cpu, budget: u32) -> ExecOutcome {
    let outcome = self.inner.execute(cpu, budget);
    self
      .native_executions
      .set(self.inner.stats().native_executions);
    outcome
  }
}

fn x(index: u32) -> Reg {
  Reg::X(Fin::new(index))
}

fn cpu_with_program(program: &[u32]) -> RV64Cpu {
  let mut cpu = RV64Cpu::new(None);
  for (index, instruction) in program.iter().copied().enumerate() {
    cpu
      .bus
      .write::<u32>(VirtAddr(PROGRAM_PC + index as u64 * 4), instruction)
      .unwrap();
  }
  cpu.write_pc(VirtAddr(PROGRAM_PC));
  cpu
}

fn run_naive(cpu: &mut RV64Cpu, budget: u32) -> ExecOutcome {
  let mut naive = NaiveInterpreter::new();
  let mut attempted = 0;
  while attempted < budget {
    let outcome = naive.execute(cpu, 1);
    attempted += outcome.attempted;
    match outcome.result {
      Err(exception) => return ExecOutcome::new(attempted, Err(exception)),
      Ok(()) if outcome.attempted == 0 => break,
      Ok(()) => (),
    }
  }
  ExecOutcome::new(attempted, Ok(()))
}

fn capture_ram(cpu: &RV64Cpu, ranges: &[(VirtAddr, usize)]) -> Vec<Vec<u8>> {
  ranges
    .iter()
    .map(|(address, len)| {
      let start = usize::try_from(address.0 - cpu.bus.mem.memory_base.0).unwrap();
      cpu.bus.mem.memory[start..start + len].to_vec()
    })
    .collect()
}

fn snapshot(cpu: &RV64Cpu, outcome: ExecOutcome, ranges: &[(VirtAddr, usize)]) -> Snapshot {
  Snapshot {
    outcome,
    pc: cpu.read_pc(),
    xregs: cpu.regs.x,
    instr: cpu.instr,
    mstatus: cpu.csrs.read_unchecked(MSTATUS),
    satp: cpu.csrs.read_unchecked(SATP),
    ram: capture_ram(cpu, ranges),
  }
}

fn assert_snapshot_eq(label: &str, native: &Snapshot, naive: &Snapshot) {
  assert_eq!(native.outcome, naive.outcome, "{label}: execution outcome");
  assert_eq!(native.pc, naive.pc, "{label}: PC");
  assert_eq!(native.xregs, naive.xregs, "{label}: integer registers");
  assert_eq!(native.instr, naive.instr, "{label}: current instruction");
  assert_eq!(native.mstatus, naive.mstatus, "{label}: mstatus");
  assert_eq!(native.satp, naive.satp, "{label}: satp");
  assert_eq!(native.ram, naive.ram, "{label}: RAM ranges");
}

fn compare_native_with_naive(
  label: &str,
  program: &[u32],
  budget: u32,
  warm_setup: &dyn Fn(&mut RV64Cpu),
  initial_setup: &dyn Fn(&mut RV64Cpu),
  data_ranges: &[(VirtAddr, usize)],
) -> Snapshot {
  let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);
  let mut warm_cpu = cpu_with_program(program);
  warm_setup(&mut warm_cpu);
  let warm_outcome = jit.execute(&mut warm_cpu, budget);
  assert_eq!(
    warm_outcome,
    ExecOutcome::new(budget, Ok(())),
    "{label}: warm-up must execute the complete decoded block",
  );
  assert_eq!(
    jit.stats().compiled_blocks,
    1,
    "{label}: hot_threshold=1 must compile the warmed block",
  );
  drop(warm_cpu);

  let mut ranges = Vec::with_capacity(data_ranges.len() + 1);
  ranges.push((VirtAddr(PROGRAM_PC), program.len() * 4));
  ranges.extend_from_slice(data_ranges);

  let mut naive_cpu = cpu_with_program(program);
  initial_setup(&mut naive_cpu);
  let naive_outcome = run_naive(&mut naive_cpu, budget);
  let naive = snapshot(&naive_cpu, naive_outcome, &ranges);
  drop(naive_cpu);

  let mut native_cpu = cpu_with_program(program);
  initial_setup(&mut native_cpu);
  let native_before = jit.stats().native_executions;
  let native_outcome = jit.execute(&mut native_cpu, budget);
  assert_eq!(
    jit.stats().native_executions,
    native_before + 1,
    "{label}: comparison execution did not enter native code",
  );
  let native = snapshot(&native_cpu, native_outcome, &ranges);
  assert_snapshot_eq(label, &native, &naive);
  native
}

#[test]
fn jalr_reads_rs1_before_writing_the_same_rd() {
  let program = [
    RV32Instr::JALR(
      Rd(x(1)),
      Rs1(x(1)),
      Imm32::<11, 0>::from(3),
    )
    .encode32(),
  ];
  let setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), PROGRAM_PC + 0x81);
  };

  let native = compare_native_with_naive("jalr rd == rs1", &program, 1, &setup, &setup, &[]);
  assert_eq!(native.pc, VirtAddr(PROGRAM_PC + 0x84));
  assert_eq!(native.xregs[1], PROGRAM_PC + 4);
}

#[test]
fn load_to_x0_still_reports_a_fault() {
  let program = [
    RV64Instr::LD(
      Rd(Reg::ZERO),
      Rs1(x(1)),
      Imm32::<11, 0>::from(0),
    )
    .encode32(),
    JAL_X0_TO_SELF,
  ];
  let warm_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), DATA_ADDR);
    cpu
      .bus
      .write::<u64>(VirtAddr(DATA_ADDR), 0x0123_4567_89ab_cdef)
      .unwrap();
  };
  let fault_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), 0);
  };

  let native = compare_native_with_naive(
    "faulting load to x0",
    &program,
    2,
    &warm_setup,
    &fault_setup,
    &[],
  );
  assert_eq!(
    native.outcome,
    ExecOutcome::new(1, Err(Exception::LoadAccessFault(VirtAddr(0)))),
  );
  assert_eq!(native.pc, VirtAddr(PROGRAM_PC));
  assert_eq!(native.xregs[0], 0);
}

#[test]
fn faulting_load_does_not_write_its_destination() {
  let program = [
    RV64Instr::LD(Rd(x(2)), Rs1(x(1)), Imm32::<11, 0>::from(0)).encode32(),
    JAL_X0_TO_SELF,
  ];
  let warm_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), DATA_ADDR);
    cpu.bus.write::<u64>(VirtAddr(DATA_ADDR), 7).unwrap();
  };
  let fault_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), 0);
    cpu.write_reg(x(2), 0xfeed_face_cafe_beef);
  };

  let native = compare_native_with_naive(
    "faulting load preserves rd",
    &program,
    2,
    &warm_setup,
    &fault_setup,
    &[],
  );
  assert_eq!(
    native.outcome,
    ExecOutcome::new(1, Err(Exception::LoadAccessFault(VirtAddr(0)))),
  );
  assert_eq!(native.pc, VirtAddr(PROGRAM_PC));
  assert_eq!(native.xregs[2], 0xfeed_face_cafe_beef);
}

#[test]
fn faulting_store_does_not_partially_modify_ram() {
  let program = [
    RV64Instr::SD(Rs1(x(1)), Rs2(x(2)), Imm32::<11, 0>::from(0)).encode32(),
    JAL_X0_TO_SELF,
  ];
  let warm_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), DATA_ADDR);
    cpu.write_reg(x(2), 0x0123_4567_89ab_cdef);
  };
  let fault_address = RV64_MEMORY_END - 4;
  let sentinel_address = RV64_MEMORY_END - 8;
  let sentinel = 0x1122_3344_5566_7788_u64;
  let fault_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), fault_address);
    cpu.write_reg(x(2), 0xdead_beef_cafe_babe);
    cpu
      .bus
      .write::<u64>(VirtAddr(sentinel_address), sentinel)
      .unwrap();
  };

  let native = compare_native_with_naive(
    "faulting store is atomic with respect to RAM",
    &program,
    2,
    &warm_setup,
    &fault_setup,
    &[(VirtAddr(sentinel_address), 8)],
  );
  assert_eq!(
    native.outcome,
    ExecOutcome::new(
      1,
      Err(Exception::StoreAccessFault(VirtAddr(RV64_MEMORY_END))),
    ),
  );
  assert_eq!(native.pc, VirtAddr(PROGRAM_PC));
  assert_eq!(
    u64::from_le_bytes(native.ram[1].as_slice().try_into().unwrap()),
    sentinel,
  );
}

#[test]
fn div_and_rem_boundaries_match_naive() {
  let program = [
    RV32Instr::DIV(Rd(x(3)), Rs1(x(1)), Rs2(x(2))).encode32(),
    RV32Instr::REM(Rd(x(4)), Rs1(x(1)), Rs2(x(2))).encode32(),
    RV32Instr::DIVU(Rd(x(5)), Rs1(x(1)), Rs2(x(2))).encode32(),
    RV32Instr::REMU(Rd(x(6)), Rs1(x(1)), Rs2(x(2))).encode32(),
    RV64Instr::DIVW(Rd(x(7)), Rs1(x(8)), Rs2(x(9))).encode32(),
    RV64Instr::REMW(Rd(x(10)), Rs1(x(8)), Rs2(x(9))).encode32(),
    RV64Instr::DIVUW(Rd(x(11)), Rs1(x(8)), Rs2(x(9))).encode32(),
    RV64Instr::REMUW(Rd(x(12)), Rs1(x(8)), Rs2(x(9))).encode32(),
    JAL_X0_TO_SELF,
  ];
  let warm_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), 12);
    cpu.write_reg(x(2), 3);
    cpu.write_reg(x(8), 12);
    cpu.write_reg(x(9), 3);
  };
  let overflow_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), i64::MIN as u64);
    cpu.write_reg(x(2), u64::MAX);
    cpu.write_reg(x(8), 0x8000_0000);
    cpu.write_reg(x(9), u64::MAX);
  };
  let zero_setup = |cpu: &mut RV64Cpu| {
    cpu.write_reg(x(1), 0x1234_5678_9abc_def0);
    cpu.write_reg(x(2), 0);
    cpu.write_reg(x(8), 0x8123_4567);
    cpu.write_reg(x(9), 0);
  };

  let overflow = compare_native_with_naive(
    "DIV signed overflow",
    &program,
    program.len() as u32,
    &warm_setup,
    &overflow_setup,
    &[],
  );
  assert_eq!(overflow.xregs[3], i64::MIN as u64);
  assert_eq!(overflow.xregs[4], 0);
  assert_eq!(overflow.xregs[7], 0xffff_ffff_8000_0000);
  assert_eq!(overflow.xregs[10], 0);

  let zero = compare_native_with_naive(
    "DIV by zero",
    &program,
    program.len() as u32,
    &warm_setup,
    &zero_setup,
    &[],
  );
  assert_eq!(zero.xregs[3], u64::MAX);
  assert_eq!(zero.xregs[4], 0x1234_5678_9abc_def0);
  assert_eq!(zero.xregs[7], u64::MAX);
  assert_eq!(zero.xregs[10], 0xffff_ffff_8123_4567);
}

#[test]
fn native_tb_stops_at_the_machine_timer_deadline() {
  let program = [
    RV32Instr::ADDI(Rd(x(1)), Rs1(x(1)), Imm32::<11, 0>::from(1)).encode32(),
    RV32Instr::ADDI(Rd(x(1)), Rs1(x(1)), Imm32::<11, 0>::from(1)).encode32(),
    RV32Instr::ADDI(Rd(x(1)), Rs1(x(1)), Imm32::<11, 0>::from(1)).encode32(),
    JAL_X0_TO_SELF,
  ];
  let budget = program.len() as u32;
  let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);
  let mut warm_cpu = cpu_with_program(&program);
  assert_eq!(
    jit.execute(&mut warm_cpu, budget),
    ExecOutcome::new(budget, Ok(())),
  );
  assert_eq!(jit.stats().compiled_blocks, 1);
  drop(warm_cpu);

  let native_executions = Rc::new(Cell::new(0));
  let executor = ObservedJit {
    inner: jit,
    native_executions: native_executions.clone(),
  };
  let mut machine = Machine {
    cpu: cpu_with_program(&program),
    executor: Box::new(executor),
  };
  machine
    .cpu
    .bus
    .clint
    .write::<u64>(VirtAddr(CLINT_BASE + 0x4000), 5)
    .unwrap();

  assert!(machine.run_next());
  assert_eq!(native_executions.get(), 1);
  assert_eq!(machine.cpu.read_reg(x(1)), Some(3));
  assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 4);
  assert_eq!(machine.cpu.csrs.read_unchecked(MIP) & MTIP_MASK, 0);

  machine.cpu.bus.clint.tick(&mut machine.cpu.csrs);
  assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 5);
  assert_ne!(machine.cpu.csrs.read_unchecked(MIP) & MTIP_MASK, 0);
}

#[test]
fn wfi_is_woken_by_a_machine_timer_interrupt() {
  let handler = PROGRAM_PC + 0x100;
  let program = [WFI, JAL_X0_TO_SELF];
  let mut cpu = cpu_with_program(&program);
  cpu
    .bus
    .write::<u32>(VirtAddr(handler), RV32Instr::EBREAK.encode32())
    .unwrap();
  cpu.csrs.write_unchecked(MTVEC, handler).unwrap();
  cpu.csrs.write_unchecked(MIE, MTIE_MASK).unwrap();
  cpu
    .bus
    .clint
    .write::<u64>(VirtAddr(CLINT_BASE + 0x4000), 3)
    .unwrap();
  let mut machine = Machine {
    cpu,
    executor: Box::new(JitExecutor::new().unwrap().with_hot_threshold(1)),
  };

  assert!(machine.run_next());
  assert!(
    machine.cpu.wfi,
    "WFI did not wait: pc={:?}, instr={:#010x}, time={}, mip={:#x}",
    machine.cpu.read_pc(),
    machine.cpu.instr,
    machine.cpu.csrs.read_unchecked(TIME),
    machine.cpu.csrs.read_unchecked(MIP),
  );
  assert_eq!(machine.cpu.read_pc(), VirtAddr(PROGRAM_PC + 4));
  assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 1);

  assert!(!machine.run_next());
  assert!(!machine.cpu.wfi);
  assert_eq!(machine.cpu.read_pc(), VirtAddr(handler));
  assert_eq!(machine.cpu.csrs.read_unchecked(TIME), 3);
  assert_eq!(machine.cpu.csrs.read_unchecked(MEPC), PROGRAM_PC + 4);
  assert_eq!(machine.cpu.csrs.read_unchecked(MCAUSE), (1_u64 << 63) | 7);
}

struct XorShift64(u64);

impl XorShift64 {
  fn new(seed: u64) -> Self {
    assert_ne!(seed, 0);
    Self(seed)
  }

  fn next(&mut self) -> u64 {
    let mut value = self.0;
    value ^= value << 13;
    value ^= value >> 7;
    value ^= value << 17;
    self.0 = value;
    value
  }

  fn register(&mut self) -> Reg {
    x(1 + (self.next() % 27) as u32)
  }

  fn immediate(&mut self) -> Imm32<11, 0> {
    Imm32::from((self.next() & 0xfff) as u32)
  }
}

fn random_integer_instruction(rng: &mut XorShift64) -> u32 {
  let rd = Rd(rng.register());
  let rs1 = Rs1(rng.register());
  let rs2 = Rs2(rng.register());
  match rng.next() % 32 {
    0 => RV32Instr::ADD(rd, rs1, rs2).encode32(),
    1 => RV32Instr::SUB(rd, rs1, rs2).encode32(),
    2 => RV32Instr::SLL(rd, rs1, rs2).encode32(),
    3 => RV32Instr::SRL(rd, rs1, rs2).encode32(),
    4 => RV32Instr::SRA(rd, rs1, rs2).encode32(),
    5 => RV32Instr::SLT(rd, rs1, rs2).encode32(),
    6 => RV32Instr::SLTU(rd, rs1, rs2).encode32(),
    7 => RV32Instr::XOR(rd, rs1, rs2).encode32(),
    8 => RV32Instr::OR(rd, rs1, rs2).encode32(),
    9 => RV32Instr::AND(rd, rs1, rs2).encode32(),
    10 => RV32Instr::ADDI(rd, rs1, rng.immediate()).encode32(),
    11 => RV32Instr::XORI(rd, rs1, rng.immediate()).encode32(),
    12 => RV32Instr::ORI(rd, rs1, rng.immediate()).encode32(),
    13 => RV32Instr::ANDI(rd, rs1, rng.immediate()).encode32(),
    14 => RV32Instr::MUL(rd, rs1, rs2).encode32(),
    15 => RV32Instr::MULH(rd, rs1, rs2).encode32(),
    16 => RV32Instr::MULHSU(rd, rs1, rs2).encode32(),
    17 => RV32Instr::MULHU(rd, rs1, rs2).encode32(),
    18 => RV32Instr::DIV(rd, rs1, rs2).encode32(),
    19 => RV32Instr::DIVU(rd, rs1, rs2).encode32(),
    20 => RV32Instr::REM(rd, rs1, rs2).encode32(),
    21 => RV32Instr::REMU(rd, rs1, rs2).encode32(),
    22 => RV64Instr::ADDW(rd, rs1, rs2).encode32(),
    23 => RV64Instr::SUBW(rd, rs1, rs2).encode32(),
    24 => RV64Instr::SLLW(rd, rs1, rs2).encode32(),
    25 => RV64Instr::SRLW(rd, rs1, rs2).encode32(),
    26 => RV64Instr::SRAW(rd, rs1, rs2).encode32(),
    27 => RV64Instr::MULW(rd, rs1, rs2).encode32(),
    28 => RV64Instr::DIVW(rd, rs1, rs2).encode32(),
    29 => RV64Instr::DIVUW(rd, rs1, rs2).encode32(),
    30 => RV64Instr::REMW(rd, rs1, rs2).encode32(),
    _ => RV64Instr::REMUW(rd, rs1, rs2).encode32(),
  }
}

#[test]
fn deterministic_random_rv64i_m_blocks_match_naive() {
  for case in 0..24_u64 {
    let seed = 0x4d59_5df4_d0f3_3173_u64 ^ case.wrapping_mul(0x9e37_79b9_7f4a_7c15);
    let mut instruction_rng = XorShift64::new(seed);
    let mut program = Vec::with_capacity(16);
    program.push(
      RV64Instr::LD(Rd(x(27)), Rs1(x(30)), Imm32::<11, 0>::from(0)).encode32(),
    );
    for _ in 0..12 {
      program.push(random_integer_instruction(&mut instruction_rng));
    }
    program.push(
      RV64Instr::SD(Rs1(x(30)), Rs2(x(27)), Imm32::<11, 0>::from(8)).encode32(),
    );
    program.push(JAL_X0_TO_SELF);

    let mut state_rng = XorShift64::new(seed ^ 0xa076_1d64_78bd_642f);
    let mut registers = [0_u64; 31];
    for value in &mut registers {
      *value = state_rng.next();
    }
    let input = state_rng.next();
    let untouched = state_rng.next();
    let setup = |cpu: &mut RV64Cpu| {
      for (index, value) in registers.iter().copied().enumerate() {
        cpu.write_reg(x(index as u32 + 1), value);
      }
      cpu.write_reg(x(30), DATA_ADDR);
      cpu.bus.write::<u64>(VirtAddr(DATA_ADDR), input).unwrap();
      cpu
        .bus
        .write::<u64>(VirtAddr(DATA_ADDR + 8), untouched)
        .unwrap();
    };
    let label = format!("deterministic random block {case}, seed {seed:#018x}");

    compare_native_with_naive(
      &label,
      &program,
      program.len() as u32,
      &setup,
      &setup,
      &[(VirtAddr(DATA_ADDR), 16)],
    );
  }
}
