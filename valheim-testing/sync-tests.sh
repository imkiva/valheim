#!/usr/bin/env bash
set -e
set -x
set -o pipefail

function find_glob() {
  local pattern="$1"
  local dir="$2"
  find "$dir" -type f -name "$pattern" -exec basename {} \; \
    | grep -v "\\.dump$" \
    | grep -v "\\.bin$"  \
    | sort               \
    | uniq
}

SELF="$(dirname $0)"

ENABLED_TESTS_FILE="$SELF/enabled-tests.txt"
ISA_TEST_DIR="$SELF/target/share/riscv-tests/isa"

find_glob 'rv64ui-p-*' "$ISA_TEST_DIR" > "$ENABLED_TESTS_FILE"
