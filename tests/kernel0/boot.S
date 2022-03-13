  .equ STACK_SIZE, 1024
  .equ MAX_CORE, 8
  .global _start
  .global kernel_halt

  .text
_start:
  csrr t0, mhartid           # read current hart id
  mv tp, t0                  # keep CPU's hartid in its tp for later usage.
  bnez t0, park              # park the hart if we're not on the hart 0

  # now we are on hart 0
  slli t0, t0, 10            # shift left the hart id by 1024
  la sp, stacks + STACK_SIZE # setup the kernel stack
  add sp, sp, t0             # move the current hart stack pointer
                             # to its place in the stack space
  j  kernel_main             # jump to kernel

park:
  # wfi                      # TODO: implement wfi
  j park

kernel_halt:
  ebreak                     # tell valheim emulator that we are done

stacks:
  .skip STACK_SIZE * MAX_CORE
  .end
