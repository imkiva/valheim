#!/usr/bin/env bash

set -euo pipefail

readonly SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
readonly REPO_ROOT="$(cd -- "${SCRIPT_DIR}/../.." && pwd -P)"
readonly DEMO_TARGET_DIR="${REPO_ROOT}/target/demo"
readonly WORK_DIR="${DEMO_TARGET_DIR}/linux"
readonly DOWNLOAD_DIR="${WORK_DIR}/downloads"
readonly SOURCE_DIR="${WORK_DIR}/source"
readonly BUILD_DIR="${WORK_DIR}/build"
readonly KERNEL_BUILD_DIR="${BUILD_DIR}/kernel"
readonly OVERLAY_INIT="${SCRIPT_DIR}/init"
readonly INITRAMFS_CPIO="${BUILD_DIR}/debian-13-slim-riscv64.cpio"
readonly INITRAMFS_STAMP="${BUILD_DIR}/debian-13-slim-riscv64.cpio.stamp"
readonly SOURCE_STAMP="${BUILD_DIR}/linux-source.stamp"

export RUSTUP_HOME="${DEMO_TARGET_DIR}/rustup"
readonly DEMO_CARGO_HOME="${DEMO_TARGET_DIR}/cargo-home"
readonly CARGO_TARGET_DIR="${DEMO_TARGET_DIR}/cargo"

readonly DEBIAN_INDEX_DIGEST="sha256:020c0d20b9880058cbe785a9db107156c3c75c2ac944a6aa7ab59f2add76a7bd"
readonly DEBIAN_MANIFEST_DIGEST="sha256:7244fbb388f7b59c9f584bb2bb7ef3a60b23aa1e55f1ad1d0641bd5ec12390f3"
readonly DEBIAN_LAYER_DIGEST="sha256:3ed37bd5491de4685b6418abd6b83c4b16cc06b7a51e46da7f154c5a149a41a5"
readonly DEBIAN_INDEX_SHA256="${DEBIAN_INDEX_DIGEST#sha256:}"
readonly DEBIAN_MANIFEST_SHA256="${DEBIAN_MANIFEST_DIGEST#sha256:}"
readonly DEBIAN_LAYER_SHA256="${DEBIAN_LAYER_DIGEST#sha256:}"
readonly DEBIAN_INDEX_FILE="${DOWNLOAD_DIR}/debian-13-slim.index.json"
readonly DEBIAN_MANIFEST_FILE="${DOWNLOAD_DIR}/debian-13-slim-riscv64.manifest.json"
readonly DEBIAN_LAYER_FILE="${DOWNLOAD_DIR}/debian-13-slim-riscv64-rootfs.tar.gz"
readonly DOCKER_TOKEN_URL="https://auth.docker.io/token?service=registry.docker.io&scope=repository:library/debian:pull"
readonly DOCKER_REGISTRY="https://registry-1.docker.io/v2/library/debian"

readonly LINUX_VERSION="5.17"
readonly LINUX_ARCHIVE="${DOWNLOAD_DIR}/linux-${LINUX_VERSION}.tar.xz"
readonly LINUX_URL="https://cdn.kernel.org/pub/linux/kernel/v5.x/linux-${LINUX_VERSION}.tar.xz"
readonly LINUX_SHA256="555fef61dddb591a83d62dd04e252792f9af4ba9ef14683f64840e46fa20b1b1"

readonly TOOLCHAIN_DEFAULT="${DEMO_TARGET_DIR}/gcc-riscv64-glibc-2022.03.09"
readonly TOOLCHAIN_DIR="${VALHEIM_LINUX_TOOLCHAIN:-${TOOLCHAIN_DEFAULT}}"
readonly TOOLCHAIN_ARCHIVE_NAME="riscv64-glibc-ubuntu-20.04-nightly-2022.03.09-nightly.tar.gz"
readonly TOOLCHAIN_ARCHIVE="${DEMO_TARGET_DIR}/downloads/${TOOLCHAIN_ARCHIVE_NAME}"
readonly TOOLCHAIN_URL="https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2022.03.09/${TOOLCHAIN_ARCHIVE_NAME}"
readonly TOOLCHAIN_SHA256="02b97cf3502d9542943b62c7470d99f97c0c9148be95e1277df96d4b5c2fdb41"
readonly TOOLCHAIN_BIN="${TOOLCHAIN_DIR}/riscv/bin"
readonly CROSS_COMPILE="${TOOLCHAIN_BIN}/riscv64-unknown-linux-gnu-"

readonly RUSTSBI_RUN="${REPO_ROOT}/demo/rustsbi/run.sh"
readonly RUSTSBI_BIOS="${DEMO_TARGET_DIR}/rustsbi/artifacts/valheim/rustsbi-qemu.bin"
readonly VALHEIM_RUST="${VALHEIM_RUST_TOOLCHAIN:-nightly-2024-09-05}"
readonly VALHEIM_BIN="${CARGO_TARGET_DIR}/release/valheim-cli"
readonly KERNEL_IMAGE="${KERNEL_BUILD_DIR}/arch/riscv/boot/Image"
readonly KERNEL_CMDLINE="console=ttyS0,115200 earlycon=sbi rdinit=/init highres=off swiotlb=noforce loglevel=7"

die() {
  printf 'error: %s\n' "$*" >&2
  exit 1
}

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
  shift 3
  local -a extra_curl_args=("$@")
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
    --continue-at - --output "${partial}" \
    "${extra_curl_args[@]}" "${url}"; then
    printf 'Resume failed; retrying %s from the beginning...\n' "$(basename -- "${destination}")" >&2
    rm -f -- "${partial}"
    curl \
      --fail --location --retry 4 --retry-all-errors \
      --output "${partial}" \
      "${extra_curl_args[@]}" "${url}"
  fi

  file_has_sha256 "${partial}" "${expected}" || {
    rm -f -- "${partial}"
    die "SHA-256 mismatch for ${destination}; expected ${expected}"
  }
  mv -- "${partial}" "${destination}"
}

prepare_debian_downloads() {
  if file_has_sha256 "${DEBIAN_INDEX_FILE}" "${DEBIAN_INDEX_SHA256}" && \
     file_has_sha256 "${DEBIAN_MANIFEST_FILE}" "${DEBIAN_MANIFEST_SHA256}" && \
     file_has_sha256 "${DEBIAN_LAYER_FILE}" "${DEBIAN_LAYER_SHA256}"; then
    printf 'Using verified Debian 13 slim OCI metadata and rootfs layer.\n'
  else
    local token
    token="$({
      curl --fail --silent --show-error --location --retry 4 --retry-all-errors \
        "${DOCKER_TOKEN_URL}"
    } | python3 -c 'import json, sys; print(json.load(sys.stdin)["token"])')"
    [[ -n "${token}" ]] || die "Docker Hub returned an empty registry token"

    download_checked \
      "${DEBIAN_INDEX_FILE}" "${DEBIAN_INDEX_SHA256}" \
      "${DOCKER_REGISTRY}/manifests/${DEBIAN_INDEX_DIGEST}" \
      --header "Authorization: Bearer ${token}" \
      --header 'Accept: application/vnd.oci.image.index.v1+json'
    download_checked \
      "${DEBIAN_MANIFEST_FILE}" "${DEBIAN_MANIFEST_SHA256}" \
      "${DOCKER_REGISTRY}/manifests/${DEBIAN_MANIFEST_DIGEST}" \
      --header "Authorization: Bearer ${token}" \
      --header 'Accept: application/vnd.oci.image.manifest.v1+json'
    download_checked \
      "${DEBIAN_LAYER_FILE}" "${DEBIAN_LAYER_SHA256}" \
      "${DOCKER_REGISTRY}/blobs/${DEBIAN_LAYER_DIGEST}" \
      --header "Authorization: Bearer ${token}"
  fi

  grep -Fq "${DEBIAN_MANIFEST_DIGEST}" "${DEBIAN_INDEX_FILE}" || die \
    "the pinned Debian index does not reference the pinned riscv64 manifest"
  grep -Fq '"architecture":"riscv64"' "${DEBIAN_INDEX_FILE}" || die \
    "the pinned Debian index has no riscv64 platform entry"
  grep -Fq "${DEBIAN_LAYER_DIGEST}" "${DEBIAN_MANIFEST_FILE}" || die \
    "the pinned Debian manifest does not reference the pinned rootfs layer"

  local os_release
  os_release="$(tar -xOzf "${DEBIAN_LAYER_FILE}" usr/lib/os-release)"
  grep -Fqx 'VERSION_ID="13"' <<<"${os_release}" || die \
    "the pinned Debian layer is not Debian 13"
  grep -Fqx 'VERSION_CODENAME=trixie' <<<"${os_release}" || die \
    "the pinned Debian layer is not trixie"
  grep -Fqx 'DEBIAN_VERSION_FULL=13.6' <<<"${os_release}" || die \
    "the pinned Debian layer is not the expected Debian 13.6 snapshot"
}

toolchain_is_expected() {
  local gcc="${CROSS_COMPILE}gcc"
  local ld="${CROSS_COMPILE}ld"
  local version
  local ld_version
  [[ -x "${gcc}" && -x "${ld}" ]] || return 1
  [[ "$("${gcc}" -dumpmachine)" == "riscv64-unknown-linux-gnu" ]] || return 1
  [[ "$("${gcc}" -dumpfullversion)" == "11.1.0" ]] || return 1
  version="$("${gcc}" --version)"
  [[ "${version}" == *g5964b5cd727* ]] || return 1
  ld_version="$("${ld}" --version)"
  [[ "${ld_version}" == *"2.37"* ]]
}

prepare_toolchain() {
  if toolchain_is_expected; then
    printf 'Using RISC-V Linux GNU toolchain: %s\n' "${TOOLCHAIN_DIR}"
    return
  fi

  [[ "${TOOLCHAIN_DIR}" == "${TOOLCHAIN_DEFAULT}" ]] || die \
    "RISC-V Linux toolchain not found at overridden path: ${TOOLCHAIN_DIR}"
  mkdir -p -- "${TOOLCHAIN_DIR}"
  download_checked \
    "${TOOLCHAIN_ARCHIVE}" "${TOOLCHAIN_SHA256}" "${TOOLCHAIN_URL}"

  printf 'Extracting the RISC-V Linux GNU toolchain...\n'
  rm -rf -- "${TOOLCHAIN_DIR}/riscv"
  tar -xzf "${TOOLCHAIN_ARCHIVE}" -C "${TOOLCHAIN_DIR}"
  toolchain_is_expected || die "unexpected compiler in ${TOOLCHAIN_DIR}"
}

linux_source_is_expected() {
  [[ -f "${SOURCE_DIR}/Makefile" ]] || return 1
  grep -Eq '^VERSION = 5$' "${SOURCE_DIR}/Makefile" && \
    grep -Eq '^PATCHLEVEL = 17$' "${SOURCE_DIR}/Makefile" && \
    grep -Eq '^SUBLEVEL = 0$' "${SOURCE_DIR}/Makefile"
}

prepare_linux_source() {
  download_checked "${LINUX_ARCHIVE}" "${LINUX_SHA256}" "${LINUX_URL}"

  local stamped=""
  if [[ -f "${SOURCE_STAMP}" ]]; then
    IFS= read -r stamped < "${SOURCE_STAMP}" || true
  fi
  if linux_source_is_expected && \
     { [[ "${stamped}" == "${LINUX_SHA256}" ]] || [[ -z "${stamped}" ]]; }; then
    printf '%s\n' "${LINUX_SHA256}" > "${SOURCE_STAMP}"
    printf 'Using Linux %s source: %s\n' "${LINUX_VERSION}" "${SOURCE_DIR}"
    return
  fi

  local temporary="${SOURCE_DIR}.extracting"
  printf 'Extracting Linux %s source...\n' "${LINUX_VERSION}"
  rm -rf -- "${temporary}"
  mkdir -p -- "${temporary}"
  tar -xJf "${LINUX_ARCHIVE}" --strip-components=1 -C "${temporary}"
  rm -rf -- "${SOURCE_DIR}"
  mv -- "${temporary}" "${SOURCE_DIR}"
  linux_source_is_expected || die "the extracted source is not Linux ${LINUX_VERSION}"
  printf '%s\n' "${LINUX_SHA256}" > "${SOURCE_STAMP}"
}

prepare_initramfs() {
  [[ -x "${OVERLAY_INIT}" ]] || die "missing executable overlay: ${OVERLAY_INIT}"

  local overlay_sha expected_stamp actual_stamp=""
  overlay_sha="$(sha256sum "${OVERLAY_INIT}" | awk '{print $1}')"
  expected_stamp="v2:${DEBIAN_LAYER_SHA256}:${overlay_sha}"
  if [[ -f "${INITRAMFS_STAMP}" ]]; then
    IFS= read -r actual_stamp < "${INITRAMFS_STAMP}" || true
  fi
  if [[ -f "${INITRAMFS_CPIO}" && "${actual_stamp}" == "${expected_stamp}" ]]; then
    printf 'Using cached official Debian rootfs initramfs: %s\n' "${INITRAMFS_CPIO}"
    return
  fi

  local work="${BUILD_DIR}/initramfs-work.$$"
  local temporary="${INITRAMFS_CPIO}.tmp.$$"
  local rootfs_stage="${work}/rootfs"
  local overlay_stage="${work}/overlay"
  rm -rf -- "${work}"
  rm -f -- "${temporary}"
  mkdir -p -- "${rootfs_stage}" "${overlay_stage}/dev"

  printf 'Packing the unmodified official Debian OCI layer with a separate init overlay...\n'
  if ! fakeroot -- bash -euo pipefail -c '
    layer=$1
    rootfs_stage=$2
    overlay_stage=$3
    overlay_init=$4
    output=$5

    tar --extract --gzip --file "${layer}" \
      --directory "${rootfs_stage}" \
      --numeric-owner --same-owner --preserve-permissions

    install -m 0755 "${overlay_init}" "${overlay_stage}/init"
    chmod 0755 "${overlay_stage}" "${overlay_stage}/dev"
    chown 0:0 "${overlay_stage}" "${overlay_stage}/dev" "${overlay_stage}/init"
    mknod -m 0600 "${overlay_stage}/dev/console" c 5 1
    mknod -m 0666 "${overlay_stage}/dev/null" c 1 3
    mknod -m 0666 "${overlay_stage}/dev/tty" c 5 0
    chown 0:0 \
      "${overlay_stage}/dev/console" \
      "${overlay_stage}/dev/null" \
      "${overlay_stage}/dev/tty"

    {
      cd "${rootfs_stage}"
      find . -xdev -print0 | LC_ALL=C sort -z | \
        cpio --null --create --format=newc --reproducible --quiet
      cd "${overlay_stage}"
      find . -xdev -print0 | LC_ALL=C sort -z | \
        cpio --null --create --format=newc --reproducible --quiet
    } > "${output}"
  ' _ \
    "${DEBIAN_LAYER_FILE}" "${rootfs_stage}" "${overlay_stage}" \
    "${OVERLAY_INIT}" "${temporary}"; then
    rm -rf -- "${work}"
    rm -f -- "${temporary}"
    die "failed to construct the Debian initramfs under fakeroot"
  fi

  mv -- "${temporary}" "${INITRAMFS_CPIO}"
  printf '%s\n' "${expected_stamp}" > "${INITRAMFS_STAMP}"
  rm -rf -- "${work}"
}

configure_kernel() {
  local config="${KERNEL_BUILD_DIR}/.config"
  local config_tool="${SOURCE_DIR}/scripts/config"

  printf 'Configuring Linux %s for Valheim...\n' "${LINUX_VERSION}"
  make -C "${SOURCE_DIR}" O="${KERNEL_BUILD_DIR}" \
    ARCH=riscv CROSS_COMPILE="${CROSS_COMPILE}" defconfig

  "${config_tool}" --file "${config}" \
    --enable CC_OPTIMIZE_FOR_SIZE \
    --disable SMP \
    --disable HOTPLUG_CPU \
    --disable MODULES \
    --disable VIRTUALIZATION \
    --disable NET \
    --disable PCI \
    --disable VIRTIO_BLK \
    --disable VIRTIO_MMIO \
    --disable SCSI \
    --disable ATA \
    --disable USB_SUPPORT \
    --disable DRM \
    --disable FB \
    --disable SOUND \
    --disable MMC \
    --disable EXT4_FS \
    --disable NFS_FS \
    --disable DEBUG_KERNEL \
    --disable KALLSYMS \
    --enable BLK_DEV_INITRD \
    --set-str INITRAMFS_SOURCE "${INITRAMFS_CPIO}" \
    --set-val INITRAMFS_ROOT_UID 0 \
    --set-val INITRAMFS_ROOT_GID 0 \
    --enable RD_GZIP \
    --enable INITRAMFS_COMPRESSION_GZIP \
    --disable INITRAMFS_COMPRESSION_BZIP2 \
    --disable INITRAMFS_COMPRESSION_LZMA \
    --disable INITRAMFS_COMPRESSION_XZ \
    --disable INITRAMFS_COMPRESSION_LZO \
    --disable INITRAMFS_COMPRESSION_LZ4 \
    --disable INITRAMFS_COMPRESSION_ZSTD \
    --disable INITRAMFS_COMPRESSION_NONE \
    --enable RISCV_SBI \
    --enable RISCV_SBI_V01 \
    --enable SOC_VIRT \
    --enable RISCV_ISA_C \
    --enable FPU \
    --enable DEVTMPFS \
    --enable DEVTMPFS_MOUNT \
    --enable TTY \
    --enable HVC_RISCV_SBI \
    --enable SERIAL_8250 \
    --enable SERIAL_8250_CONSOLE \
    --enable SERIAL_OF_PLATFORM \
    --enable SERIAL_EARLYCON \
    --enable SERIAL_EARLYCON_RISCV_SBI \
    --enable BINFMT_ELF \
    --enable PROC_FS \
    --enable SYSFS \
    --enable TMPFS \
    --enable PRINTK

  make -C "${SOURCE_DIR}" O="${KERNEL_BUILD_DIR}" \
    ARCH=riscv CROSS_COMPILE="${CROSS_COMPILE}" olddefconfig

  grep -Fqx "CONFIG_INITRAMFS_SOURCE=\"${INITRAMFS_CPIO}\"" "${config}" || die \
    "kernel config did not retain the requested initramfs"
  grep -Fqx 'CONFIG_INITRAMFS_COMPRESSION_GZIP=y' "${config}" || die \
    "kernel config did not enable gzip initramfs compression"
  grep -Fqx '# CONFIG_SMP is not set' "${config}" || die \
    "kernel config unexpectedly enabled SMP"
}

main() {
  local command
  for command in \
    awk bash bc bison cc chmod chown cpio curl dtc fakeroot find flex grep gzip install \
    make mkdir mknod mv perl python3 rm rustup sha256sum sort tar xz; do
    require_command "${command}"
  done
  [[ -x "${RUSTSBI_RUN}" ]] || die "RustSBI launcher not found: ${RUSTSBI_RUN}"

  local jobs="${JOBS:-}"
  if [[ -z "${jobs}" ]]; then
    if command -v nproc >/dev/null 2>&1; then
      jobs="$(nproc)"
    else
      jobs=1
    fi
  fi
  [[ "${jobs}" =~ ^[1-9][0-9]*$ ]] || die "JOBS must be a positive integer"

  mkdir -p -- \
    "${DOWNLOAD_DIR}" "${BUILD_DIR}" "${KERNEL_BUILD_DIR}" \
    "${RUSTUP_HOME}" "${DEMO_CARGO_HOME}" "${CARGO_TARGET_DIR}"
  prepare_debian_downloads
  prepare_toolchain
  prepare_linux_source
  prepare_initramfs
  configure_kernel

  printf 'Building Linux %s Image with %s jobs...\n' "${LINUX_VERSION}" "${jobs}"
  make -C "${SOURCE_DIR}" O="${KERNEL_BUILD_DIR}" \
    ARCH=riscv CROSS_COMPILE="${CROSS_COMPILE}" \
    -j"${jobs}" Image
  [[ -s "${KERNEL_IMAGE}" ]] || die "kernel Image was not produced: ${KERNEL_IMAGE}"

  printf 'Building the pinned RustSBI firmware...\n'
  JOBS="${jobs}" "${RUSTSBI_RUN}" --build-only
  [[ -s "${RUSTSBI_BIOS}" ]] || die "RustSBI BIOS was not produced: ${RUSTSBI_BIOS}"

  printf 'Building Valheim with Rust %s...\n' "${VALHEIM_RUST}"
  rustup run "${VALHEIM_RUST}" env \
    CARGO_HOME="${DEMO_CARGO_HOME}" \
    CARGO_TARGET_DIR="${CARGO_TARGET_DIR}" \
    cargo build \
      --manifest-path "${REPO_ROOT}/Cargo.toml" \
      --release --locked --package valheim-cli
  [[ -x "${VALHEIM_BIN}" ]] || die "Valheim CLI was not produced: ${VALHEIM_BIN}"

  printf '\nStarting Debian 13 on Linux %s. Wait for the debian13# prompt; press Ctrl-C to exit.\n\n' \
    "${LINUX_VERSION}"
  exec "${VALHEIM_BIN}" \
    --bios "${RUSTSBI_BIOS}" \
    --kernel "${KERNEL_IMAGE}" \
    --cmdline "${KERNEL_CMDLINE}" \
    "$@"
}

main "$@"
