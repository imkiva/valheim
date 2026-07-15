#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C

readonly SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)"
readonly REPO_ROOT="$(cd -- "${SCRIPT_DIR}/../.." && pwd -P)"
readonly DEMO_TARGET_DIR="${REPO_ROOT}/target/demo"
readonly WORK_DIR="${DEMO_TARGET_DIR}/linux"
readonly DOWNLOAD_DIR="${WORK_DIR}/downloads"
readonly SOURCE_DIR="${WORK_DIR}/source"
readonly BUILD_DIR="${WORK_DIR}/build"
readonly KERNEL_BUILD_DIR="${BUILD_DIR}/kernel"
readonly OVERLAY_INIT="${SCRIPT_DIR}/init"
readonly RUNTIME_DIR="${WORK_DIR}/runtime"
readonly ROOTFS_BASE_IMAGE="${BUILD_DIR}/debian-13-slim-riscv64.ext4"
readonly ROOTFS_BASE_STAMP="${BUILD_DIR}/debian-13-slim-riscv64.ext4.stamp"
readonly ROOTFS_RUNTIME_IMAGE="${RUNTIME_DIR}/rootfs.ext4"
readonly ROOTFS_RUNTIME_STAMP="${RUNTIME_DIR}/rootfs.ext4.schema"
readonly SOURCE_STAMP="${BUILD_DIR}/linux-source.stamp"

readonly ROOTFS_SCHEMA_VERSION="1"
readonly ROOTFS_SIZE_BYTES="$((256 * 1024 * 1024))"
readonly ROOTFS_BLOCK_SIZE="4096"
readonly ROOTFS_BLOCK_COUNT="$((ROOTFS_SIZE_BYTES / ROOTFS_BLOCK_SIZE))"
readonly ROOTFS_UUID="3f3434d2-6c1e-4f8b-98e8-4f525649534b"
readonly ROOTFS_LABEL="VALHEIMROOT"
readonly ROOTFS_FEATURES="has_journal,ext_attr,resize_inode,dir_index,filetype,extent,64bit,flex_bg,sparse_super,large_file,huge_file,dir_nlink,extra_isize,metadata_csum"

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
readonly KERNEL_CMDLINE="console=ttyS0,115200 earlycon=sbi root=/dev/vda rootfstype=ext4 rootwait rw init=/init highres=off swiotlb=noforce loglevel=7"

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

rootfs_schema() {
  local init_sha
  init_sha="$(sha256sum "${OVERLAY_INIT}" | awk '{print $1}')"
  printf 'v%s:layer=%s:init=%s:size=%s:block=%s:uuid=%s:label=%s:features=%s' \
    "${ROOTFS_SCHEMA_VERSION}" "${DEBIAN_LAYER_SHA256}" "${init_sha}" \
    "${ROOTFS_SIZE_BYTES}" "${ROOTFS_BLOCK_SIZE}" "${ROOTFS_UUID}" \
    "${ROOTFS_LABEL}" "${ROOTFS_FEATURES}"
}

debugfs_stat() {
  local image="$1"
  local path="$2"
  debugfs -R "stat ${path}" "${image}" 2>/dev/null
}

verify_rootfs_metadata() {
  local image="$1"
  local expected_init_sha="$2"
  local actual_features allocated_bytes image_size init_sha
  local stats init_stat console_stat null_stat tty_stat shadow_stat
  local perl_inode versioned_perl_inode

  [[ -f "${image}" ]] || return 1
  image_size="$(stat -c '%s' "${image}")"
  [[ "${image_size}" == "${ROOTFS_SIZE_BYTES}" ]] || return 1
  allocated_bytes="$(( $(stat -c '%b' "${image}") * 512 ))"
  (( allocated_bytes < ROOTFS_SIZE_BYTES )) || return 1
  e2fsck -fn "${image}" >/dev/null 2>&1 || return 1

  stats="$(debugfs -R stats "${image}" 2>/dev/null)" || return 1
  grep -Eq "^Filesystem volume name:[[:space:]]+${ROOTFS_LABEL}$" <<<"${stats}" || return 1
  grep -Eq "^Filesystem UUID:[[:space:]]+${ROOTFS_UUID}$" <<<"${stats}" || return 1
  grep -Eq '^Filesystem state:[[:space:]]+clean$' <<<"${stats}" || return 1
  grep -Eq "^Block size:[[:space:]]+${ROOTFS_BLOCK_SIZE}$" <<<"${stats}" || return 1
  grep -Eq "^Block count:[[:space:]]+${ROOTFS_BLOCK_COUNT}$" <<<"${stats}" || return 1
  actual_features="$(awk '
    /^Filesystem features:/ {
      sub(/^Filesystem features:[[:space:]]*/, "")
      print
      exit
    }
  ' <<<"${stats}")"
  [[ "${actual_features}" == "${ROOTFS_FEATURES//,/ }" ]] || return 1

  init_stat="$(debugfs_stat "${image}" /init)" || return 1
  grep -Fq 'Type: regular    Mode:  0755' <<<"${init_stat}" || return 1
  grep -Eq '^User:[[:space:]]+0[[:space:]]+Group:[[:space:]]+0([[:space:]]|$)' \
    <<<"${init_stat}" || return 1
  init_sha="$(debugfs -R 'cat /init' "${image}" 2>/dev/null | sha256sum | awk '{print $1}')"
  [[ "${init_sha}" == "${expected_init_sha}" ]] || return 1

  console_stat="$(debugfs_stat "${image}" /dev/console)" || return 1
  null_stat="$(debugfs_stat "${image}" /dev/null)" || return 1
  tty_stat="$(debugfs_stat "${image}" /dev/tty)" || return 1
  grep -Fq 'Type: character special    Mode:  0600' <<<"${console_stat}" || return 1
  grep -Fq 'Device major/minor number: 05:01' <<<"${console_stat}" || return 1
  grep -Fq 'Type: character special    Mode:  0666' <<<"${null_stat}" || return 1
  grep -Fq 'Device major/minor number: 01:03' <<<"${null_stat}" || return 1
  grep -Fq 'Type: character special    Mode:  0666' <<<"${tty_stat}" || return 1
  grep -Fq 'Device major/minor number: 05:00' <<<"${tty_stat}" || return 1

  shadow_stat="$(debugfs_stat "${image}" /etc/shadow)" || return 1
  grep -Fq 'Type: regular    Mode:  0640' <<<"${shadow_stat}" || return 1
  grep -Eq '^User:[[:space:]]+0[[:space:]]+Group:[[:space:]]+42([[:space:]]|$)' \
    <<<"${shadow_stat}" || return 1
  perl_inode="$(debugfs_stat "${image}" /usr/bin/perl | awk 'NR == 1 { print $2 }')"
  versioned_perl_inode="$(debugfs_stat "${image}" /usr/bin/perl5.40.1 | awk 'NR == 1 { print $2 }')"
  [[ -n "${perl_inode}" && "${perl_inode}" == "${versioned_perl_inode}" ]] || return 1
}

cached_rootfs_base_is_valid() {
  local expected_schema="$1"
  local expected_init_sha="$2"
  local line stamped_schema="" stamped_sha=""

  [[ -f "${ROOTFS_BASE_IMAGE}" && -f "${ROOTFS_BASE_STAMP}" ]] || return 1
  while IFS= read -r line; do
    case "${line}" in
      schema=*) stamped_schema="${line#schema=}" ;;
      sha256=*) stamped_sha="${line#sha256=}" ;;
    esac
  done < "${ROOTFS_BASE_STAMP}"
  [[ "${stamped_schema}" == "${expected_schema}" ]] || return 1
  [[ "${stamped_sha}" =~ ^[0-9a-f]{64}$ ]] || return 1
  file_has_sha256 "${ROOTFS_BASE_IMAGE}" "${stamped_sha}" || return 1
  verify_rootfs_metadata "${ROOTFS_BASE_IMAGE}" "${expected_init_sha}"
}

prepare_rootfs_base() {
  [[ -x "${OVERLAY_INIT}" ]] || die "missing executable overlay: ${OVERLAY_INIT}"

  local expected_schema init_sha image_sha
  local work="${BUILD_DIR}/ext4-work.$$"
  local rootfs_stage="${work}/rootfs"
  local temporary="${ROOTFS_BASE_IMAGE}.tmp.$$"
  local temporary_stamp="${ROOTFS_BASE_STAMP}.tmp.$$"
  expected_schema="$(rootfs_schema)"
  init_sha="$(sha256sum "${OVERLAY_INIT}" | awk '{print $1}')"

  if cached_rootfs_base_is_valid "${expected_schema}" "${init_sha}"; then
    chmod 0444 "${ROOTFS_BASE_IMAGE}"
    printf 'Using verified read-only Debian ext4 base: %s\n' "${ROOTFS_BASE_IMAGE}"
    return
  fi
  if [[ -e "${ROOTFS_BASE_IMAGE}" || -e "${ROOTFS_BASE_STAMP}" ]]; then
    printf 'Rebuilding stale or invalid read-only ext4 base.\n'
  fi

  rm -rf -- "${work}"
  rm -f -- "${temporary}" "${temporary_stamp}"
  mkdir -p -- "${rootfs_stage}"
  printf 'Building a sparse 256 MiB ext4 image from the official Debian OCI layer...\n'
  if ! fakeroot -- bash -euo pipefail -c '
    layer=$1
    rootfs_stage=$2
    overlay_init=$3
    output=$4
    size=$5
    block_size=$6
    uuid=$7
    label=$8
    features=$9

    tar --extract --gzip --file "${layer}" \
      --directory "${rootfs_stage}" \
      --numeric-owner --same-owner --preserve-permissions

    install -m 0755 "${overlay_init}" "${rootfs_stage}/init"
    mkdir -p "${rootfs_stage}/dev"
    chmod 0755 "${rootfs_stage}" "${rootfs_stage}/dev"
    chown 0:0 "${rootfs_stage}" "${rootfs_stage}/dev" "${rootfs_stage}/init"
    rm -f \
      "${rootfs_stage}/dev/console" \
      "${rootfs_stage}/dev/null" \
      "${rootfs_stage}/dev/tty"
    mknod -m 0600 "${rootfs_stage}/dev/console" c 5 1
    mknod -m 0666 "${rootfs_stage}/dev/null" c 1 3
    mknod -m 0666 "${rootfs_stage}/dev/tty" c 5 0
    chown 0:0 \
      "${rootfs_stage}/dev/console" \
      "${rootfs_stage}/dev/null" \
      "${rootfs_stage}/dev/tty"

    truncate -s "${size}" "${output}"
    mke2fs -q -t ext4 \
      -b "${block_size}" -I 256 -m 0 \
      -U "${uuid}" -L "${label}" \
      -O "none,${features}" \
      -E lazy_itable_init=0,lazy_journal_init=0,root_owner=0:0 \
      -d "${rootfs_stage}" "${output}"
  ' _ \
    "${DEBIAN_LAYER_FILE}" "${rootfs_stage}" "${OVERLAY_INIT}" "${temporary}" \
    "${ROOTFS_SIZE_BYTES}" "${ROOTFS_BLOCK_SIZE}" "${ROOTFS_UUID}" \
    "${ROOTFS_LABEL}" "${ROOTFS_FEATURES}"; then
    rm -rf -- "${work}"
    rm -f -- "${temporary}" "${temporary_stamp}"
    die "failed to construct the Debian ext4 base under fakeroot"
  fi

  if ! verify_rootfs_metadata "${temporary}" "${init_sha}"; then
    e2fsck -fn "${temporary}" || true
    rm -rf -- "${work}"
    rm -f -- "${temporary}" "${temporary_stamp}"
    die "the generated Debian ext4 base failed metadata verification"
  fi
  image_sha="$(sha256sum "${temporary}" | awk '{print $1}')"
  printf 'schema=%s\nsha256=%s\n' "${expected_schema}" "${image_sha}" > "${temporary_stamp}"
  chmod 0444 "${temporary}"
  mv -f -- "${temporary}" "${ROOTFS_BASE_IMAGE}"
  mv -f -- "${temporary_stamp}" "${ROOTFS_BASE_STAMP}"
  rm -rf -- "${work}"
  printf 'Verified ext4 UUID, label, features, ownership, hardlinks, device nodes, and fsck.\n'
}

prepare_runtime_rootfs() {
  local expected_schema actual_schema="" base_sha runtime_sha
  local temporary="${ROOTFS_RUNTIME_IMAGE}.tmp.$$"
  local temporary_stamp="${ROOTFS_RUNTIME_STAMP}.tmp.$$"
  expected_schema="$(rootfs_schema)"

  case "${RESET_DISK:-0}" in
    0 | 1) ;;
    *) die "RESET_DISK must be 0 or 1" ;;
  esac

  if [[ "${RESET_DISK:-0}" != 1 && \
        -f "${ROOTFS_RUNTIME_IMAGE}" && -f "${ROOTFS_RUNTIME_STAMP}" ]]; then
    IFS= read -r actual_schema < "${ROOTFS_RUNTIME_STAMP}" || true
    if [[ "${actual_schema}" == "schema=${expected_schema}" && \
          "$(stat -c '%s' "${ROOTFS_RUNTIME_IMAGE}")" == "${ROOTFS_SIZE_BYTES}" ]]; then
      printf 'Reusing writable Debian ext4 runtime: %s\n' "${ROOTFS_RUNTIME_IMAGE}"
      return
    fi
  fi

  if [[ "${RESET_DISK:-0}" != 1 && \
        ( -e "${ROOTFS_RUNTIME_IMAGE}" || -e "${ROOTFS_RUNTIME_STAMP}" ) ]]; then
    die "existing runtime rootfs has an unknown schema; preserve it, then use RESET_DISK=1 to replace it"
  fi

  mkdir -p -- "${RUNTIME_DIR}"
  rm -f -- "${temporary}" "${temporary_stamp}"
  printf 'Creating a writable runtime copy from the verified ext4 base...\n'
  cp --reflink=auto --sparse=always -- "${ROOTFS_BASE_IMAGE}" "${temporary}"
  chmod u+w "${temporary}"
  base_sha="$(sha256sum "${ROOTFS_BASE_IMAGE}" | awk '{print $1}')"
  runtime_sha="$(sha256sum "${temporary}" | awk '{print $1}')"
  if [[ "${runtime_sha}" != "${base_sha}" ]]; then
    rm -f -- "${temporary}" "${temporary_stamp}"
    die "the writable ext4 runtime copy did not match its verified base"
  fi
  printf 'schema=%s\n' "${expected_schema}" > "${temporary_stamp}"
  mv -f -- "${temporary}" "${ROOTFS_RUNTIME_IMAGE}"
  mv -f -- "${temporary_stamp}" "${ROOTFS_RUNTIME_STAMP}"
}

acquire_default_runtime_lock() {
  exec 8>>"${ROOTFS_RUNTIME_IMAGE}.lock" || die \
    "cannot open runtime lock: ${ROOTFS_RUNTIME_IMAGE}.lock"
  flock --exclusive --nonblock 8 || die \
    "the default Debian runtime is already in use by another process"
}

acquire_disk_lock() {
  local disk="$1"
  exec 9<>"${disk}" || die "cannot open writable disk for locking: ${disk}"
  flock --exclusive --nonblock 9 || die \
    "the disk is already in use by another process: ${disk}"
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
    --enable BLOCK \
    --enable VIRTIO \
    --enable VIRTIO_BLK \
    --enable VIRTIO_MMIO \
    --disable VIRTIO_PCI \
    --disable SCSI \
    --disable ATA \
    --disable USB_SUPPORT \
    --disable DRM \
    --disable FB \
    --disable SOUND \
    --disable MMC \
    --enable EXT4_FS \
    --enable EXT4_FS_POSIX_ACL \
    --disable NFS_FS \
    --disable DEBUG_KERNEL \
    --disable KALLSYMS \
    --enable BLK_DEV_INITRD \
    --set-str INITRAMFS_SOURCE "" \
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

  grep -Fqx 'CONFIG_INITRAMFS_SOURCE=""' "${config}" || die \
    "kernel config unexpectedly embeds an initramfs"
  grep -Fqx 'CONFIG_BLK_DEV_INITRD=y' "${config}" || die \
    "kernel config did not retain empty-initramfs support"
  for option in CONFIG_VIRTIO CONFIG_VIRTIO_BLK CONFIG_VIRTIO_MMIO CONFIG_EXT4_FS; do
    grep -Fqx "${option}=y" "${config}" || die \
      "kernel config did not build ${option} into the Image"
  done
  grep -Fqx '# CONFIG_SMP is not set' "${config}" || die \
    "kernel config unexpectedly enabled SMP"
}

main() {
  local command
  local argument
  local disk_path=""
  local disk_overridden=0
  local i
  local -a cli_args=("$@")
  local -a engine_args=(--engine jit)
  local -a cmdline_args=(--cmdline "${KERNEL_CMDLINE}")
  local -a disk_args=()
  for ((i = 0; i < ${#cli_args[@]}; i++)); do
    argument="${cli_args[i]}"
    case "${argument}" in
      --engine)
        engine_args=()
        ;;
      --engine=*)
        engine_args=()
        ;;
      --disk | -d)
        disk_overridden=1
        ((i + 1 < ${#cli_args[@]})) || die "${argument} requires a disk path"
        i=$((i + 1))
        disk_path="${cli_args[i]}"
        ;;
      --disk=*)
        disk_overridden=1
        disk_path="${argument#--disk=}"
        ;;
      -d=*)
        disk_overridden=1
        disk_path="${argument#-d=}"
        ;;
      -d?*)
        disk_overridden=1
        disk_path="${argument#-d}"
        ;;
      --cmdline | -c)
        cmdline_args=()
        ;;
      --cmdline=* | -c=* | -c?*)
        cmdline_args=()
        ;;
    esac
  done

  case "${RESET_DISK:-0}" in
    0 | 1) ;;
    *) die "RESET_DISK must be 0 or 1" ;;
  esac
  if [[ "${disk_overridden}" == 1 && "${RESET_DISK:-0}" == 1 ]]; then
    die "RESET_DISK=1 cannot be combined with an explicit --disk/-d"
  fi

  for command in \
    awk bash bc bison cc chmod chown cp curl debugfs dtc e2fsck fakeroot flex flock grep \
    gzip install make mkdir mke2fs mknod mv perl python3 rm rustup sha256sum stat \
    tar truncate xz; do
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
    "${DOWNLOAD_DIR}" "${BUILD_DIR}" "${KERNEL_BUILD_DIR}" "${RUNTIME_DIR}" \
    "${RUSTUP_HOME}" "${DEMO_CARGO_HOME}" "${CARGO_TARGET_DIR}"
  prepare_debian_downloads
  prepare_toolchain
  prepare_linux_source
  prepare_rootfs_base
  if [[ "${disk_overridden}" == 0 ]]; then
    acquire_default_runtime_lock
    prepare_runtime_rootfs
    acquire_disk_lock "${ROOTFS_RUNTIME_IMAGE}"
    disk_args=(--disk "${ROOTFS_RUNTIME_IMAGE}")
  else
    [[ -n "${disk_path}" ]] || die "--disk/-d requires a non-empty disk path"
    [[ -f "${disk_path}" ]] || die "disk image not found: ${disk_path}"
    if [[ "${disk_path}" -ef "${ROOTFS_BASE_IMAGE}" ]]; then
      die "refusing to use the verified read-only ext4 base as a writable runtime disk"
    fi
    if [[ -f "${ROOTFS_RUNTIME_IMAGE}" && "${disk_path}" -ef "${ROOTFS_RUNTIME_IMAGE}" ]]; then
      acquire_default_runtime_lock
    fi
    acquire_disk_lock "${disk_path}"
    printf 'Using the rootfs disk supplied on the command line.\n'
  fi
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
    "${engine_args[@]}" \
    --bios "${RUSTSBI_BIOS}" \
    --kernel "${KERNEL_IMAGE}" \
    "${disk_args[@]}" \
    "${cmdline_args[@]}" \
    "$@"
}

main "$@"
