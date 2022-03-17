#!/usr/bin/env bash

cargo build --release

sudo dtrace -c \
  '../target/release/valheim --kernel tests/test-kernel.bin --bios tests/rustsbi-qemu.bin' \
  -o out.stacks \
  -n 'profile-997/execname == "valheim"/{@[ustack(500)] = count();}'

./stackcollapse.pl out.stacks | tee out.collapsed | ./flamegraph.pl > flamegraph.svg
