CROSS_COMPILE = riscv64-unknown-elf-
CFLAGS = -mcmodel=medany -nostdlib -fno-builtin -march=rv64gc -mabi=lp64d -g -Wall

QEMU = qemu-system-riscv64
QFLAGS = -nographic -smp 1 -machine virt -bios none

GDB = ${CROSS_COMPILE}gdb
CC = ${CROSS_COMPILE}gcc
OBJCOPY = ${CROSS_COMPILE}objcopy
OBJDUMP = ${CROSS_COMPILE}objdump

SRCS_ASM = $(wildcard *.S)
SRCS_C = $(wildcard *.c)

OBJS = $(SRCS_ASM:.S=.o) $(SRCS_C:.c=.o)

KERNEL_ELF := kernel.elf
KERNEL_BIN := $(KERNEL_ELF:.elf=.bin)

all: $(KERNEL_ELF)

$(KERNEL_ELF): ${OBJS}
	${CC} ${CFLAGS} -Ttext=0x80000000 -o $@ $^
	${OBJCOPY} -O binary $@ $(KERNEL_BIN)

%.o : %.c
	${CC} ${CFLAGS} -c -o $@ $<

%.o : %.S
	${CC} ${CFLAGS} -c -o $@ $<

.PHONY : clean disassemble
clean:
	rm -f $(OBJS) $(KERNEL_ELF) $(KERNEL_BIN)

disassemble: $(KERNEL_ELF)
	${OBJDUMP} -d $<

qemu: all
	@${QEMU} -M ? | grep virt >/dev/null || exit
	@${QEMU} ${QFLAGS} -kernel $(KERNEL_ELF)

.PHONY : debug
debug: all
	@${QEMU} ${QFLAGS} -kernel $(KERNEL_ELF) -s -S &
	@${GDB} os.elf -q -x ../gdbinit
