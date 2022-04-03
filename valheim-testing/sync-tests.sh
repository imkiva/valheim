#!/usr/bin/env bash
set -e
set -x
set -o pipefail

function find_glob() {
  local pattern="$1"
  local dir="$2"
  local disabled="$3"
  find "$dir" -type f -name "$pattern" -exec basename {} \; \
    | grep -v "\\.dump$" \
    | grep -v "\\.bin$"  \
    | grep -v -x -F -f "$disabled" \
    | sort               \
    | uniq
}

SELF="$(dirname $0)"

ENABLED_TESTS_FILE="$SELF/enabled-tests.txt"
DISABLED_TESTS_FILE="$SELF/disabled-tests.txt"
ISA_TEST_DIR="$SELF/target/share/riscv-tests/isa"

> "$ENABLED_TESTS_FILE"
find_glob 'rv64ui-p-*' "$ISA_TEST_DIR" "$DISABLED_TESTS_FILE" >> "$ENABLED_TESTS_FILE"
find_glob 'rv64um-p-*' "$ISA_TEST_DIR" "$DISABLED_TESTS_FILE" >> "$ENABLED_TESTS_FILE"
find_glob 'rv64ua-p-*' "$ISA_TEST_DIR" "$DISABLED_TESTS_FILE" >> "$ENABLED_TESTS_FILE"
find_glob 'rv64uf-p-*' "$ISA_TEST_DIR" "$DISABLED_TESTS_FILE" >> "$ENABLED_TESTS_FILE"
find_glob 'rv64ud-p-*' "$ISA_TEST_DIR" "$DISABLED_TESTS_FILE" >> "$ENABLED_TESTS_FILE"
find_glob 'rv64uc-p-*' "$ISA_TEST_DIR" "$DISABLED_TESTS_FILE" >> "$ENABLED_TESTS_FILE"
