#!/usr/bin/env bash

set -euo pipefail

BUILD_ONLY=0
if [[ "${1:-}" == "--build-only" ]]; then
  BUILD_ONLY=1
  shift
fi
readonly BUILD_ONLY

readonly SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
readonly REPO_ROOT="$(cd -- "${SCRIPT_DIR}/../.." && pwd -P)"
readonly DEMO_TARGET_ROOT="${REPO_ROOT}/target/demo"
readonly WORK_DIR="${DEMO_TARGET_ROOT}/rustsbi"
readonly SOURCE_DIR="${WORK_DIR}/source"
readonly ARTIFACT_DIR="${WORK_DIR}/artifacts/valheim"
readonly RUNTIME_DIR="${WORK_DIR}/runtime"
readonly RUN_LOG="${RUNTIME_DIR}/last-run.log"
readonly RUSTSBI_CARGO_TARGET_DIR="${WORK_DIR}/cargo-target"
readonly VALHEIM_CARGO_TARGET_DIR="${DEMO_TARGET_ROOT}/cargo"
readonly VALHEIM_BIN="${VALHEIM_CARGO_TARGET_DIR}/release/valheim-cli"

# Keep downloaded Rust components and Cargo registries inside the repository's
# ignored target tree. The demo does not modify the user's normal Rust homes.
export RUSTUP_HOME="${DEMO_TARGET_ROOT}/rustup"
readonly DEMO_CARGO_HOME="${DEMO_TARGET_ROOT}/cargo-home"

readonly -a PATCH_FILES=(
  "${SCRIPT_DIR}/single-hart-valheim.patch"
  "${SCRIPT_DIR}/valheim-dtb-pointer.patch"
)
readonly RUSTSBI_REPOSITORY="https://github.com/rustsbi/rustsbi-qemu.git"
readonly RUSTSBI_COMMIT="999e3556fcfa1b0900dd797ae2186667af8d2dc6"
readonly RUST_TARGET="riscv64imac-unknown-none-elf"
readonly RUSTSBI_RUST="${RUSTSBI_RUST_TOOLCHAIN:-nightly-2022-02-14}"
readonly VALHEIM_RUST="${VALHEIM_RUST_TOOLCHAIN:-nightly-2024-09-05}"

readonly RISCV_TOOLCHAIN_DEFAULT="${DEMO_TARGET_ROOT}/gcc-riscv64-elf-2022.03.09"
readonly RISCV_TOOLCHAIN="${VALHEIM_RISCV_TOOLCHAIN:-${RISCV_TOOLCHAIN_DEFAULT}}"
readonly TOOLCHAIN_ARCHIVE_NAME="riscv64-elf-ubuntu-20.04-nightly-2022.03.09-nightly.tar.gz"
readonly TOOLCHAIN_ARCHIVE="${DEMO_TARGET_ROOT}/downloads/${TOOLCHAIN_ARCHIVE_NAME}"
readonly TOOLCHAIN_URL="https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2022.03.09/${TOOLCHAIN_ARCHIVE_NAME}"
readonly TOOLCHAIN_SHA256="6ec8ea11558f283aecd47c52a25c61a10c117ed703fee09c5a7dbfde3b522da1"
readonly RISCV_BIN="${RISCV_TOOLCHAIN}/riscv/bin"
readonly TOOLPREFIX="${RISCV_BIN}/riscv64-unknown-elf-"
readonly SUCCESS_LINE="<< Test-kernel: All hart SBI test SUCCESS, shutdown"

die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

if (( BUILD_ONLY )) && (( $# != 0 )); then
  die "--build-only does not accept additional arguments"
fi

require_command() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

file_has_sha256() {
  local file="$1"
  local expected="$2"
  [[ -f "${file}" ]] || return 1
  printf '%s  %s\n' "${expected}" "${file}" | sha256sum --check --status
}

download_checked() {
  local destination="$1"
  local expected="$2"
  local url="$3"
  local partial="${destination}.part"

  if file_has_sha256 "${destination}" "${expected}"; then
    printf 'Using verified download: %s\n' "${destination}"
    return
  fi

  if [[ -e "${destination}" ]]; then
    printf 'Discarding download with the wrong SHA-256: %s\n' "${destination}" >&2
    rm -f -- "${destination}"
  fi

  mkdir -p -- "$(dirname -- "${destination}")"
  printf 'Downloading %s...\n' "$(basename -- "${destination}")"
  if ! curl \
    --fail --location --retry 4 --retry-all-errors \
    --continue-at - --output "${partial}" "${url}"; then
    printf 'Resume failed; retrying from the beginning...\n' >&2
    rm -f -- "${partial}"
    curl \
      --fail --location --retry 4 --retry-all-errors \
      --output "${partial}" "${url}"
  fi

  file_has_sha256 "${partial}" "${expected}" || {
    rm -f -- "${partial}"
    die "SHA-256 mismatch for ${destination}; expected ${expected}"
  }
  mv -- "${partial}" "${destination}"
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

prepare_riscv_toolchain() {
  if toolchain_is_expected; then
    printf 'Using RISC-V bare-metal GNU toolchain: %s\n' "${RISCV_TOOLCHAIN}"
    return
  fi

  [[ "${RISCV_TOOLCHAIN}" == "${RISCV_TOOLCHAIN_DEFAULT}" ]] || die \
    "RISC-V toolchain not found at overridden path: ${RISCV_TOOLCHAIN}"
  download_checked "${TOOLCHAIN_ARCHIVE}" "${TOOLCHAIN_SHA256}" "${TOOLCHAIN_URL}"
  printf 'Extracting the RISC-V bare-metal GNU toolchain...\n'
  mkdir -p -- "${RISCV_TOOLCHAIN}"
  rm -rf -- "${RISCV_TOOLCHAIN}/riscv"
  tar -xzf "${TOOLCHAIN_ARCHIVE}" -C "${RISCV_TOOLCHAIN}"
  toolchain_is_expected || die "unexpected compiler in ${RISCV_TOOLCHAIN}"
}

ensure_rust_toolchain() {
  local toolchain="$1"

  if ! rustup run "${toolchain}" rustc --version >/dev/null 2>&1; then
    printf 'Installing Rust %s with the minimal profile...\n' "${toolchain}"
    rustup toolchain install "${toolchain}" --profile minimal
  fi
  rustup run "${toolchain}" rustc --version >/dev/null 2>&1 ||
    die "Rust toolchain ${toolchain} is unavailable after installation"
}

ensure_rust_target() {
  local toolchain="$1"
  local target="$2"

  if ! rustup target list --installed --toolchain "${toolchain}" | grep -Fxq "${target}"; then
    printf 'Installing Rust target %s for %s...\n' "${target}" "${toolchain}"
    rustup target add --toolchain "${toolchain}" "${target}"
  fi
}

require_command curl
require_command dtc
require_command git
require_command grep
require_command rustup
require_command sha256sum
require_command tar
require_command tee

if [[ -n "${JOBS:-}" && ! "${JOBS}" =~ ^[1-9][0-9]*$ ]]; then
  die "JOBS must be a positive integer"
fi

mkdir -p -- "${DEMO_TARGET_ROOT}" "${WORK_DIR}"
prepare_riscv_toolchain
ensure_rust_toolchain "${RUSTSBI_RUST}"
ensure_rust_target "${RUSTSBI_RUST}" "${RUST_TARGET}"
ensure_rust_toolchain "${VALHEIM_RUST}"

if [[ -d "${SOURCE_DIR}/.git" ]] && \
   ! git -C "${SOURCE_DIR}" rev-parse --verify HEAD >/dev/null 2>&1; then
  printf 'Discarding an incomplete RustSBI checkout: %s\n' "${SOURCE_DIR}" >&2
  rm -rf -- "${SOURCE_DIR}"
fi
if [[ ! -d "${SOURCE_DIR}/.git" ]]; then
  [[ ! -e "${SOURCE_DIR}" ]] || die "${SOURCE_DIR} exists but is not a Git checkout"
  clone_dir="${SOURCE_DIR}.clone"
  printf 'Cloning RustSBI-QEMU at %s...\n' "${RUSTSBI_COMMIT}"
  rm -rf -- "${clone_dir}"
  git init --quiet "${clone_dir}"
  git -C "${clone_dir}" remote add origin "${RUSTSBI_REPOSITORY}"
  git -C "${clone_dir}" fetch --depth=1 origin "${RUSTSBI_COMMIT}"
  git -C "${clone_dir}" checkout --detach FETCH_HEAD
  mv -- "${clone_dir}" "${SOURCE_DIR}"
fi

actual_commit="$(git -C "${SOURCE_DIR}" rev-parse HEAD)"
[[ "${actual_commit}" == "${RUSTSBI_COMMIT}" ]] || die \
  "RustSBI checkout is ${actual_commit}; expected ${RUSTSBI_COMMIT}"

for patch_file in "${PATCH_FILES[@]}"; do
  patch_name="$(basename -- "${patch_file}")"
  if git -C "${SOURCE_DIR}" apply --reverse --check "${patch_file}" >/dev/null 2>&1; then
    printf '%s is already applied.\n' "${patch_name}"
  elif git -C "${SOURCE_DIR}" apply --check "${patch_file}" >/dev/null 2>&1; then
    printf 'Applying %s...\n' "${patch_name}"
    git -C "${SOURCE_DIR}" apply "${patch_file}"
  else
    die "RustSBI source does not match ${patch_name}"
  fi
done

cargo_jobs=()
if [[ -n "${JOBS:-}" ]]; then
  cargo_jobs=(--jobs "${JOBS}")
fi

printf 'Building RustSBI-QEMU 0.1.0 / RustSBI 0.2.1...\n'
(
  cd "${SOURCE_DIR}/rustsbi-qemu"
  rustup run "${RUSTSBI_RUST}" env \
    CARGO_HOME="${DEMO_CARGO_HOME}" \
    CARGO_TARGET_DIR="${RUSTSBI_CARGO_TARGET_DIR}" \
    cargo build \
      --release --locked \
      --package rustsbi-qemu \
      --target "${RUST_TARGET}" \
      "${cargo_jobs[@]}"
)

printf 'Building the historical test kernel with the Valheim single-hart patch...\n'
(
  cd "${SOURCE_DIR}/test-kernel"
  rustup run "${RUSTSBI_RUST}" env \
    CARGO_HOME="${DEMO_CARGO_HOME}" \
    CARGO_TARGET_DIR="${RUSTSBI_CARGO_TARGET_DIR}" \
    cargo build \
      --release --locked \
      --package test-kernel \
      --target "${RUST_TARGET}" \
      "${cargo_jobs[@]}"
)

readonly DIST_DIR="${RUSTSBI_CARGO_TARGET_DIR}/${RUST_TARGET}/release"
mkdir -p -- "${ARTIFACT_DIR}" "${RUNTIME_DIR}"
"${TOOLPREFIX}objcopy" -O binary \
  "${DIST_DIR}/rustsbi-qemu" "${ARTIFACT_DIR}/rustsbi-qemu.bin"
"${TOOLPREFIX}objcopy" -O binary \
  "${DIST_DIR}/test-kernel" "${ARTIFACT_DIR}/test-kernel.bin"

printf 'Building Valheim with Rust %s...\n' "${VALHEIM_RUST}"
rustup run "${VALHEIM_RUST}" env \
  CARGO_HOME="${DEMO_CARGO_HOME}" \
  CARGO_TARGET_DIR="${VALHEIM_CARGO_TARGET_DIR}" \
  cargo build \
    --manifest-path "${REPO_ROOT}/Cargo.toml" \
    --release --locked --package valheim-cli \
    "${cargo_jobs[@]}"

if (( BUILD_ONLY )); then
  printf '\nBuild-only complete. RustSBI firmware: %s\n' \
    "${ARTIFACT_DIR}/rustsbi-qemu.bin"
  printf 'Historical test kernel: %s\n' "${ARTIFACT_DIR}/test-kernel.bin"
  exit 0
fi

printf '\nRunning the RustSBI test kernel...\n\n'
set +e
"${VALHEIM_BIN}" \
  --bios "${ARTIFACT_DIR}/rustsbi-qemu.bin" \
  --kernel "${ARTIFACT_DIR}/test-kernel.bin" \
  "$@" 2>&1 | tee "${RUN_LOG}"
valheim_status="${PIPESTATUS[0]}"
set -e

[[ "${valheim_status}" == "0" ]] || die "Valheim exited with status ${valheim_status}"
grep -Fq "${SUCCESS_LINE}" "${RUN_LOG}" || die \
  "test kernel stopped without the expected success line; see ${RUN_LOG}"

printf '\nVerified: %s\n' "${SUCCESS_LINE}"
