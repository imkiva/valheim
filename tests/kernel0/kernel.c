// tell valheim that we are done
__attribute__((noreturn)) extern void kernel_halt();

// the kernel entrace function called by boot.S
void kernel_main(void) {
  int sum = 0;
  for (int i = 0; i <= 100; ++i) {
    sum += i;
  }
  kernel_halt();
}
