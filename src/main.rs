use valheim_core::cpu::RV64Cpu;
use valheim_core::interp::naive::NaiveInterpreter;

mod repl;

fn main() {
  let code : Vec<u32> = vec![
    0x1bf520b7,  // 0:  lui x1, 114514
    0x77f08093,  // 4:  addi x1, x1, 1919
    0x000080b3,  // 8:  add x1, x1, x0
    0x001080b3,  // 12: add x1, x1, x1
    0x400080b3,  // 16: sub x1, x1, x0
    0x401080b3,  // 20: sub x1, x1, x1
    0xfe9ff06f,  // 24: jal x0, -24
    0x00100073,  // ebreak
  ];
  let mut cpu = RV64Cpu::new();
  cpu.reset(code.as_slice());
  let interp = NaiveInterpreter::new();
  cpu.run(&interp);
}
