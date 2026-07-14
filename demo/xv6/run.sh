#!/usr/bin/env bash

set -euo pipefail

readonly SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
readonly REPO_ROOT="$(cd -- "${SCRIPT_DIR}/../.." && pwd -P)"
readonly TARGET_DEMO_DIR="${REPO_ROOT}/target/demo"
readonly XV6_STATE_DIR="${TARGET_DEMO_DIR}/xv6"
readonly XV6_SOURCE="${XV6_STATE_DIR}/source"
readonly RUNTIME_DIR="${XV6_STATE_DIR}/runtime"
readonly RUNTIME_DISK="${RUNTIME_DIR}/fs.img"

readonly XV6_COMMIT="a1da53a5a12e21b44a2c79d962a437fa2107627c"
readonly XV6_REPOSITORY="https://github.com/mit-pdos/xv6-riscv.git"

readonly RISCV_TOOLCHAIN_DEFAULT="${TARGET_DEMO_DIR}/gcc-riscv64-elf-2022.03.09"
readonly RISCV_TOOLCHAIN="${VALHEIM_RISCV_TOOLCHAIN:-${RISCV_TOOLCHAIN_DEFAULT}}"
readonly RISCV_ARCHIVE_NAME="riscv64-elf-ubuntu-20.04-nightly-2022.03.09-nightly.tar.gz"
readonly RISCV_ARCHIVE="${TARGET_DEMO_DIR}/downloads/${RISCV_ARCHIVE_NAME}"
readonly RISCV_TOOLCHAIN_URL="https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2022.03.09/${RISCV_ARCHIVE_NAME}"
readonly RISCV_TOOLCHAIN_SHA256="6ec8ea11558f283aecd47c52a25c61a10c117ed703fee09c5a7dbfde3b522da1"
readonly RISCV_BIN="${RISCV_TOOLCHAIN}/riscv/bin"
readonly TOOLPREFIX="${RISCV_BIN}/riscv64-unknown-elf-"

readonly RUST_TOOLCHAIN="${VALHEIM_RUST_TOOLCHAIN:-nightly-2024-09-05}"
readonly RUSTUP_HOME="${TARGET_DEMO_DIR}/rustup"
readonly DEMO_CARGO_HOME="${TARGET_DEMO_DIR}/cargo-home"
readonly CARGO_TARGET_DIR="${TARGET_DEMO_DIR}/cargo"
export RUSTUP_HOME

die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

require_command() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

verify_riscv_archive() {
  printf '%s  %s\n' "${RISCV_TOOLCHAIN_SHA256}" "$1" |
    sha256sum --check --status
}

toolchain_is_expected() {
  local gcc="${TOOLPREFIX}gcc"
  local objcopy="${TOOLPREFIX}objcopy"
  local objcopy_version

  [[ -x "${gcc}" && -x "${objcopy}" ]] || return 1
  [[ "$("${gcc}" -dumpmachine)" == "riscv64-unknown-elf" ]] || return 1
  [[ "$("${gcc}" -dumpfullversion)" == "11.1.0" ]] || return 1
  objcopy_version="$("${objcopy}" --version)"
  [[ "${objcopy_version}" == *"2.37"* ]]
}

install_riscv_toolchain() {
  [[ "${RISCV_TOOLCHAIN}" == "${RISCV_TOOLCHAIN_DEFAULT}" ]] || die \
    "RISC-V toolchain not found at overridden path: ${RISCV_TOOLCHAIN}"

  mkdir -p "${TARGET_DEMO_DIR}/downloads"

  if [[ -f "${RISCV_ARCHIVE}" ]] && ! verify_riscv_archive "${RISCV_ARCHIVE}"; then
    printf 'Discarding RISC-V toolchain archive with an invalid SHA-256...\n' >&2
    rm -f -- "${RISCV_ARCHIVE}"
  fi

  if [[ ! -f "${RISCV_ARCHIVE}" ]]; then
    local partial_archive="${RISCV_ARCHIVE}.part"
    printf 'Downloading RISC-V GNU toolchain 2022.03.09...\n'
    rm -f -- "${partial_archive}"
    curl --fail --location --retry 3 \
      --output "${partial_archive}" \
      "${RISCV_TOOLCHAIN_URL}"
    if ! verify_riscv_archive "${partial_archive}"; then
      rm -f -- "${partial_archive}"
      die "RISC-V toolchain archive SHA-256 mismatch"
    fi
    mv -- "${partial_archive}" "${RISCV_ARCHIVE}"
  fi

  verify_riscv_archive "${RISCV_ARCHIVE}" || die \
    "RISC-V toolchain archive SHA-256 mismatch"

  printf 'Extracting RISC-V GNU toolchain to %s...\n' "${RISCV_TOOLCHAIN}"
  rm -rf -- "${RISCV_TOOLCHAIN}"
  mkdir -p "${RISCV_TOOLCHAIN}"
  tar -xzf "${RISCV_ARCHIVE}" -C "${RISCV_TOOLCHAIN}"

  toolchain_is_expected || die "unexpected compiler in ${RISCV_TOOLCHAIN}"
}

require_command curl
require_command dtc
require_command git
require_command make
require_command rustup
require_command sha256sum
require_command tar

[[ -f "${REPO_ROOT}/Cargo.toml" ]] || die \
  "could not find the Valheim repository root at ${REPO_ROOT}"
if [[ -n "${JOBS:-}" && ! "${JOBS}" =~ ^[1-9][0-9]*$ ]]; then
  die "JOBS must be a positive integer"
fi

mkdir -p \
  "${TARGET_DEMO_DIR}" "${RUSTUP_HOME}" "${DEMO_CARGO_HOME}" "${CARGO_TARGET_DIR}"

if ! toolchain_is_expected; then
  install_riscv_toolchain
fi

if ! rustup run "${RUST_TOOLCHAIN}" rustc --version >/dev/null 2>&1; then
  printf 'Installing Rust %s with the minimal profile...\n' "${RUST_TOOLCHAIN}"
  rustup toolchain install "${RUST_TOOLCHAIN}" --profile minimal
fi

mkdir -p "${XV6_STATE_DIR}"
if [[ -d "${XV6_SOURCE}/.git" ]] && \
   ! git -C "${XV6_SOURCE}" rev-parse --verify HEAD >/dev/null 2>&1; then
  printf 'Discarding an incomplete xv6 checkout: %s\n' "${XV6_SOURCE}" >&2
  rm -rf -- "${XV6_SOURCE}"
fi
if [[ ! -d "${XV6_SOURCE}/.git" ]]; then
  [[ ! -e "${XV6_SOURCE}" ]] || die \
    "${XV6_SOURCE} exists but is not a Git checkout"
  clone_dir="${XV6_SOURCE}.clone"
  printf 'Cloning xv6-riscv at %s...\n' "${XV6_COMMIT}"
  rm -rf -- "${clone_dir}"
  git init --quiet "${clone_dir}"
  git -C "${clone_dir}" remote add origin "${XV6_REPOSITORY}"
  git -C "${clone_dir}" fetch --depth=1 origin "${XV6_COMMIT}"
  git -C "${clone_dir}" checkout --detach FETCH_HEAD
  mv -- "${clone_dir}" "${XV6_SOURCE}"
fi

actual_commit="$(git -C "${XV6_SOURCE}" rev-parse HEAD)"
[[ "${actual_commit}" == "${XV6_COMMIT}" ]] || die \
  "xv6 checkout is ${actual_commit}; expected ${XV6_COMMIT}"

if ! git -C "${XV6_SOURCE}" diff --quiet || \
   ! git -C "${XV6_SOURCE}" diff --cached --quiet; then
  printf 'warning: xv6 source contains local changes\n' >&2
fi

if [[ -n "${JOBS:-}" ]]; then
  build_jobs="${JOBS}"
elif command -v nproc >/dev/null 2>&1; then
  build_jobs="$(nproc)"
else
  build_jobs=1
fi

printf 'Building xv6 (%s)...\n' "${XV6_COMMIT}"
PATH="${RISCV_BIN}:${PATH}" make \
  -C "${XV6_SOURCE}" \
  -j"${build_jobs}" \
  TOOLPREFIX="${TOOLPREFIX}" \
  kernel/kernel fs.img

"${TOOLPREFIX}objcopy" \
  -O binary \
  "${XV6_SOURCE}/kernel/kernel" \
  "${XV6_SOURCE}/kernel/kernel.bin"

mkdir -p "${RUNTIME_DIR}"
if [[ "${RESET_DISK:-0}" == "1" || ! -f "${RUNTIME_DISK}" ]]; then
  cp "${XV6_SOURCE}/fs.img" "${RUNTIME_DISK}"
fi

printf 'Building Valheim with Rust %s...\n' "${RUST_TOOLCHAIN}"
rustup run "${RUST_TOOLCHAIN}" env \
  CARGO_HOME="${DEMO_CARGO_HOME}" \
  CARGO_TARGET_DIR="${CARGO_TARGET_DIR}" \
  cargo build \
  --manifest-path "${REPO_ROOT}/Cargo.toml" \
  --release \
  --locked \
  --package valheim-cli

printf '\nStarting xv6; press Ctrl-C to stop Valheim.\n\n'
exec "${CARGO_TARGET_DIR}/release/valheim-cli" \
  --kernel "${XV6_SOURCE}/kernel/kernel.bin" \
  --disk "${RUNTIME_DISK}" \
  "$@"
