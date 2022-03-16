#include "uart.h"

// tell valheim that we are done
__attribute__((noreturn)) extern void kernel_halt();

// the kernel entrance function called by boot.S
void kernel_main(void) {
  uart_init();
  uart_puts("hello kernel\n");
  kernel_halt();
}
