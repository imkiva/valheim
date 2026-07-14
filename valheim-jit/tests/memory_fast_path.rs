use valheim_asm::isa::data::Fin;
use valheim_asm::isa::typed::Reg;
use valheim_core::cpu::bus::RV64_MEMORY_BASE;
use valheim_core::cpu::RV64Cpu;
use valheim_core::interp::{ExecOutcome, RV64Executor};
use valheim_core::memory::VirtAddr;
use valheim_jit::JitExecutor;

fn run_three_times(jit: &mut JitExecutor, cpu: &mut RV64Cpu, pc: VirtAddr, budget: u32) {
  for _ in 0..3 {
    cpu.write_pc(pc);
    assert_eq!(jit.execute(cpu, budget), ExecOutcome::new(budget, Ok(())));
  }
}

#[test]
fn disabled_stats_keep_native_tlb_miss_and_hit_paths_counter_free() {
  let mut cpu = RV64Cpu::new(None);
  let pc = VirtAddr(RV64_MEMORY_BASE);
  let data = VirtAddr(RV64_MEMORY_BASE + 0x200);
  cpu.bus.write::<u32>(pc, 0x0000_b103).unwrap(); // ld x2, 0(x1)
  cpu
    .bus
    .write::<u32>(pc + VirtAddr(4), 0x0020_b423)
    .unwrap(); // sd x2, 8(x1)
  cpu
    .bus
    .write::<u32>(pc + VirtAddr(8), 0x0000_006f)
    .unwrap(); // jal x0, 0
  cpu.bus.write::<u64>(data, 0xfeed_face_cafe_beef).unwrap();
  cpu.write_reg(Reg::X(Fin::new(1)), data.0);
  let mut jit = JitExecutor::new()
    .unwrap()
    .with_hot_threshold(1)
    .with_stats_enabled(false);

  // Helpers use an unreported sink and the generated hit path must omit its RMW.
  run_three_times(&mut jit, &mut cpu, pc, 3);

  let stats = jit.stats();
  assert_eq!(stats.dispatches, 0);
  assert_eq!(stats.guest_instructions, 0);
  assert_eq!(stats.cache_hits, 0);
  assert_eq!(stats.cache_misses, 0);
  assert_eq!(stats.decoded_executions, 0);
  assert_eq!(stats.native_executions, 0);
  assert_eq!(stats.compiled_blocks, 0);
  assert_eq!(stats.successful_exits, 0);
  assert_eq!(stats.exception_exits, 0);
  assert_eq!(stats.tlb_misses, 0);
  assert_eq!(stats.tlb_hits, 0);
  assert_eq!(stats.memory_slow_paths, 0);
  assert_eq!(stats.memory_faults, 0);
  assert_eq!(
    cpu.bus.read::<u64>(data + VirtAddr(8)).unwrap(),
    0xfeed_face_cafe_beef
  );
}

#[test]
fn disabling_stats_after_compilation_keeps_old_native_counters_safe() {
  let mut cpu = RV64Cpu::new(None);
  let pc = VirtAddr(RV64_MEMORY_BASE);
  let data = VirtAddr(RV64_MEMORY_BASE + 0x300);
  cpu.bus.write::<u32>(pc, 0x0000_b103).unwrap(); // ld x2, 0(x1)
  cpu
    .bus
    .write::<u32>(pc + VirtAddr(4), 0x0000_006f)
    .unwrap(); // jal x0, 0
  cpu.bus.write::<u64>(data, 0x1234_5678).unwrap();
  cpu.write_reg(Reg::X(Fin::new(1)), data.0);
  let mut jit = JitExecutor::new().unwrap().with_hot_threshold(1);

  cpu.write_pc(pc);
  assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
  let before = jit.stats();

  jit = jit.with_stats_enabled(false);
  cpu.write_pc(pc);
  assert_eq!(jit.execute(&mut cpu, 2), ExecOutcome::new(2, Ok(())));
  assert_eq!(cpu.read_reg(Reg::X(Fin::new(2))), Some(0x1234_5678));
  assert_eq!(jit.stats(), before);
}
