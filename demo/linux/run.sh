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
readonly NOCLOUD_CHROOT_HELPER="${SCRIPT_DIR}/chroot-install-dev-packages.sh"
readonly RUNTIME_DIR="${WORK_DIR}/runtime"
readonly HOST_TOOLS_DIR="${WORK_DIR}/host-tools"
readonly PREPARE_LOCK="${WORK_DIR}/prepare.lock"
readonly SOURCE_STAMP="${BUILD_DIR}/linux-source.stamp"

readonly OCI_ROOTFS_BASE_IMAGE="${BUILD_DIR}/debian-13-slim-riscv64.ext4"
readonly OCI_ROOTFS_BASE_STAMP="${BUILD_DIR}/debian-13-slim-riscv64.ext4.stamp"
readonly OCI_ROOTFS_RUNTIME_IMAGE="${RUNTIME_DIR}/rootfs.ext4"
readonly OCI_ROOTFS_RUNTIME_STAMP="${RUNTIME_DIR}/rootfs.ext4.schema"
readonly OCI_ROOTFS_SCHEMA_VERSION="2"
readonly OCI_ROOTFS_SIZE_BYTES="$((256 * 1024 * 1024))"
readonly OCI_ROOTFS_BLOCK_SIZE="4096"
readonly OCI_ROOTFS_BLOCK_COUNT="$((OCI_ROOTFS_SIZE_BYTES / OCI_ROOTFS_BLOCK_SIZE))"
readonly OCI_ROOTFS_UUID="3f3434d2-6c1e-4f8b-98e8-4f525649534b"
readonly OCI_ROOTFS_LABEL="VALHEIMROOT"
readonly OCI_ROOTFS_FEATURES="has_journal,ext_attr,resize_inode,dir_index,filetype,extent,64bit,flex_bg,sparse_super,large_file,huge_file,dir_nlink,extra_isize,metadata_csum"

readonly NOCLOUD_BUILD_ID="20260712-2537"
readonly NOCLOUD_ARCHIVE_NAME="debian-13-nocloud-riscv64-${NOCLOUD_BUILD_ID}.tar.xz"
readonly NOCLOUD_METADATA_NAME="debian-13-nocloud-riscv64-${NOCLOUD_BUILD_ID}.json"
readonly NOCLOUD_BASE_URL="https://cloud.debian.org/images/cloud/trixie/${NOCLOUD_BUILD_ID}"
readonly NOCLOUD_ARCHIVE="${DOWNLOAD_DIR}/${NOCLOUD_ARCHIVE_NAME}"
readonly NOCLOUD_METADATA="${DOWNLOAD_DIR}/${NOCLOUD_METADATA_NAME}"
readonly NOCLOUD_ARCHIVE_SHA512="65f4c937175e6f096e697f671b8bbd745f1a6025f610343e1d00bfd4e0bbe475b27a8cc77c0f22f83499242d78be2070769c24da11ee4c73e37072eab8659783"
readonly NOCLOUD_METADATA_SHA512="023206bfb347bc1f2c1b64ae1d42f7c1ccc2b42fb48b7f2bdd2e8c7750d06bdee0ffc2248d5df054d40803aefc8abc2d5318098fee42202d41dbfe1eb6b28412"
readonly NOCLOUD_ROOTFS_BASE_IMAGE="${BUILD_DIR}/debian-13-nocloud-riscv64.ext4"
readonly NOCLOUD_ROOTFS_BASE_STAMP="${BUILD_DIR}/debian-13-nocloud-riscv64.ext4.stamp"
readonly NOCLOUD_ROOTFS_RUNTIME_IMAGE="${RUNTIME_DIR}/nocloud-rootfs.ext4"
readonly NOCLOUD_ROOTFS_RUNTIME_STAMP="${RUNTIME_DIR}/nocloud-rootfs.ext4.schema"
readonly NOCLOUD_ROOTFS_SCHEMA_VERSION="3"
readonly NOCLOUD_DISK_SIZE_BYTES="$((3 * 1024 * 1024 * 1024))"
readonly NOCLOUD_SECTOR_SIZE="512"
readonly NOCLOUD_ROOT_START_SECTORS="262144"
readonly NOCLOUD_ROOT_SIZE_SECTORS="6027264"
readonly NOCLOUD_ROOTFS_SIZE_BYTES="$((NOCLOUD_ROOT_SIZE_SECTORS * NOCLOUD_SECTOR_SIZE))"
readonly NOCLOUD_ROOTFS_BLOCK_SIZE="4096"
readonly NOCLOUD_ROOTFS_BLOCK_COUNT="$((NOCLOUD_ROOTFS_SIZE_BYTES / NOCLOUD_ROOTFS_BLOCK_SIZE))"
readonly NOCLOUD_ROOTFS_UUID="b0d27fb7-a963-466f-9a17-bc634e1aff15"
readonly NOCLOUD_ROOT_PARTITION_TYPE="72EC70A6-CF74-40E6-BD49-4BDA08E8F224"
readonly NOCLOUD_ROOTFS_FEATURES="has_journal ext_attr resize_inode dir_index orphan_file filetype extent 64bit flex_bg metadata_csum_seed sparse_super large_file huge_file dir_nlink extra_isize metadata_csum"
readonly NOCLOUD_DEV_PROFILE_VERSION="1"
readonly NOCLOUD_DEBIAN_SNAPSHOT="20260712T202631Z"
readonly NOCLOUD_SECURITY_SNAPSHOT="20260712T194830Z"
readonly -a NOCLOUD_DEV_PACKAGES=(
  build-essential
  git
  pkg-config
  cmake
  ninja-build
  meson
  autoconf
  automake
  libtool
  bison
  flex
  patch
  gdb
  strace
  lsof
  jq
  rsync
  wget
  fakeroot
  manpages-dev
  bzip2
  unzip
  zip
  tree
  python3-pip
  python3-venv
  python3-dev
)
readonly NOCLOUD_ENRICHED_PACKAGE_COUNT="392"
readonly NOCLOUD_ENRICHED_MANIFEST_SHA256="051c1d9de5b0b1eb38442edcab296835363fcdbbe806f4335888de1e11d6672b"

readonly QEMU_USER_STATIC_VERSION="6.2+dfsg-2ubuntu6.31"
readonly QEMU_USER_STATIC_ARCHIVE_NAME="qemu-user-static_${QEMU_USER_STATIC_VERSION}_amd64.deb"
readonly QEMU_USER_STATIC_ARCHIVE="${DOWNLOAD_DIR}/${QEMU_USER_STATIC_ARCHIVE_NAME}"
readonly QEMU_USER_STATIC_URL="https://archive.ubuntu.com/ubuntu/pool/universe/q/qemu/${QEMU_USER_STATIC_ARCHIVE_NAME}"
readonly QEMU_USER_STATIC_SHA256="2d22939f98f2ee8b84c5cc53b01082a4a937cfc7b4a8aa432788b9eaf4a14a41"
readonly QEMU_RISCV64_STATIC_SHA256="ee063e5feaae2475b1eabe82ead98574c94fbbdbf6e0131379ea686ab6e3b437"
readonly QEMU_USER_STATIC_DIR="${HOST_TOOLS_DIR}/qemu-user-static-${QEMU_USER_STATIC_VERSION}"
readonly QEMU_RISCV64_STATIC="${QEMU_USER_STATIC_DIR}/qemu-riscv64-static"

readonly E2FSPROGS_VERSION="1.47.2"
readonly E2FSPROGS_ARCHIVE_NAME="e2fsprogs-${E2FSPROGS_VERSION}.tar.xz"
readonly E2FSPROGS_ARCHIVE="${DOWNLOAD_DIR}/${E2FSPROGS_ARCHIVE_NAME}"
readonly E2FSPROGS_URL="https://cdn.kernel.org/pub/linux/kernel/people/tytso/e2fsprogs/v${E2FSPROGS_VERSION}/${E2FSPROGS_ARCHIVE_NAME}"
readonly E2FSPROGS_SHA256="08242e64ca0e8194d9c1caad49762b19209a06318199b63ce74ae4ef2d74e63c"
readonly E2FSPROGS_SOURCE_DIR="${HOST_TOOLS_DIR}/e2fsprogs-${E2FSPROGS_VERSION}-source"
readonly E2FSPROGS_BUILD_DIR="${HOST_TOOLS_DIR}/e2fsprogs-${E2FSPROGS_VERSION}-build"
readonly E2FSPROGS_E2FSCK="${E2FSPROGS_BUILD_DIR}/e2fsck/e2fsck"
readonly E2FSPROGS_DEBUGFS="${E2FSPROGS_BUILD_DIR}/debugfs/debugfs"
readonly E2FSPROGS_BUILD_STAMP="${E2FSPROGS_BUILD_DIR}/valheim-build.stamp"
readonly E2FSPROGS_BUILD_SCHEMA="v1:sha256=${E2FSPROGS_SHA256}:private-libuuid:private-libblkid"

readonly PASST_VERSION="2026_06_11.a9c61ff"
readonly PASST_ARCHIVE_NAME="passt-${PASST_VERSION}.tar.xz"
readonly PASST_ARCHIVE="${DOWNLOAD_DIR}/${PASST_ARCHIVE_NAME}"
readonly PASST_URL="https://passt.top/passt/snapshot/${PASST_ARCHIVE_NAME}"
readonly PASST_SHA256="b94b235cb96ce1b7aeab6552b7e0b4c9a780e5d700ced500c65e429b2d8b8450"
readonly PASST_SOURCE_DIR="${HOST_TOOLS_DIR}/passt-${PASST_VERSION}-source"
readonly PASST_SOURCE_STAMP="${PASST_SOURCE_DIR}/valheim-source.stamp"
readonly PASST_BUILD_DIR="${HOST_TOOLS_DIR}/passt-${PASST_VERSION}-build"
readonly PASST_BINARY="${PASST_BUILD_DIR}/passt"
readonly PASST_AVX2_BINARY="${PASST_BUILD_DIR}/passt.avx2"
readonly PASST_BUILD_STAMP="${PASST_BUILD_DIR}/valheim-build.stamp"
readonly PASST_BUILD_SCHEMA="v2:version=${PASST_VERSION}:sha256=${PASST_SHA256}:x86_64-avx2-dispatch"

ROOTFS_SOURCE=""
ROOTFS_BASE_IMAGE=""
ROOTFS_BASE_STAMP=""
ROOTFS_RUNTIME_IMAGE=""
ROOTFS_RUNTIME_STAMP=""
ROOTFS_SIZE_BYTES=""
ROOTFS_BLOCK_SIZE=""
ROOTFS_E2FSCK=""
ROOTFS_DEBUGFS=""
TEMPORARY_PATHS=()

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

cleanup_temporary_paths() {
  local path
  for path in "${TEMPORARY_PATHS[@]}"; do
    [[ -n "${path}" ]] && rm -rf -- "${path}"
  done
}

trap cleanup_temporary_paths EXIT

require_command() {
  command -v "$1" >/dev/null 2>&1 || die "required command not found: $1"
}

select_rootfs_source() {
  case "$1" in
    nocloud)
      ROOTFS_SOURCE="nocloud"
      ROOTFS_BASE_IMAGE="${NOCLOUD_ROOTFS_BASE_IMAGE}"
      ROOTFS_BASE_STAMP="${NOCLOUD_ROOTFS_BASE_STAMP}"
      ROOTFS_RUNTIME_IMAGE="${NOCLOUD_ROOTFS_RUNTIME_IMAGE}"
      ROOTFS_RUNTIME_STAMP="${NOCLOUD_ROOTFS_RUNTIME_STAMP}"
      ROOTFS_SIZE_BYTES="${NOCLOUD_ROOTFS_SIZE_BYTES}"
      ROOTFS_BLOCK_SIZE="${NOCLOUD_ROOTFS_BLOCK_SIZE}"
      ROOTFS_E2FSCK="${E2FSPROGS_E2FSCK}"
      ROOTFS_DEBUGFS="${E2FSPROGS_DEBUGFS}"
      ;;
    oci)
      ROOTFS_SOURCE="oci"
      ROOTFS_BASE_IMAGE="${OCI_ROOTFS_BASE_IMAGE}"
      ROOTFS_BASE_STAMP="${OCI_ROOTFS_BASE_STAMP}"
      ROOTFS_RUNTIME_IMAGE="${OCI_ROOTFS_RUNTIME_IMAGE}"
      ROOTFS_RUNTIME_STAMP="${OCI_ROOTFS_RUNTIME_STAMP}"
      ROOTFS_SIZE_BYTES="${OCI_ROOTFS_SIZE_BYTES}"
      ROOTFS_BLOCK_SIZE="${OCI_ROOTFS_BLOCK_SIZE}"
      ROOTFS_E2FSCK="e2fsck"
      ROOTFS_DEBUGFS="debugfs"
      ;;
    *)
      die "unknown rootfs source: $1"
      ;;
  esac
}

file_has_checksum() {
  local file="$1"
  local expected="$2"
  local algorithm="$3"
  local checksum_tool="${algorithm}sum"
  [[ -f "${file}" ]] || return 1
  printf '%s  %s\n' "${expected}" "${file}" | "${checksum_tool}" --check --status
}

file_has_sha256() {
  file_has_checksum "$1" "$2" sha256
}

file_has_sha512() {
  file_has_checksum "$1" "$2" sha512
}

download_checked_with() {
  local destination="$1"
  local expected="$2"
  local algorithm="$3"
  local url="$4"
  shift 4
  local -a extra_curl_args=("$@")
  local partial="${destination}.part"

  if file_has_checksum "${destination}" "${expected}" "${algorithm}"; then
    printf 'Using verified download: %s\n' "${destination}"
    return
  fi

  if [[ -e "${destination}" ]]; then
    printf 'Discarding download with the wrong %s: %s\n' \
      "${algorithm^^}" "${destination}" >&2
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

  file_has_checksum "${partial}" "${expected}" "${algorithm}" || {
    rm -f -- "${partial}"
    die "${algorithm^^} mismatch for ${destination}; expected ${expected}"
  }
  mv -- "${partial}" "${destination}"
}

download_checked() {
  local destination="$1"
  local expected="$2"
  local url="$3"
  shift 3
  download_checked_with "${destination}" "${expected}" sha256 "${url}" "$@"
}

download_checked_sha512() {
  local destination="$1"
  local expected="$2"
  local url="$3"
  shift 3
  download_checked_with "${destination}" "${expected}" sha512 "${url}" "$@"
}

prepare_oci_downloads() {
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

prepare_nocloud_downloads() {
  download_checked_sha512 \
    "${NOCLOUD_ARCHIVE}" "${NOCLOUD_ARCHIVE_SHA512}" \
    "${NOCLOUD_BASE_URL}/${NOCLOUD_ARCHIVE_NAME}"
  download_checked_sha512 \
    "${NOCLOUD_METADATA}" "${NOCLOUD_METADATA_SHA512}" \
    "${NOCLOUD_BASE_URL}/${NOCLOUD_METADATA_NAME}"

  grep -Fq '"arch": "riscv64"' "${NOCLOUD_METADATA}" || die \
    "the pinned Debian NoCloud image is not riscv64"
  grep -Fq '"release": "trixie"' "${NOCLOUD_METADATA}" || die \
    "the pinned Debian NoCloud image is not trixie"
  grep -Fq '"release_id": "13"' "${NOCLOUD_METADATA}" || die \
    "the pinned Debian NoCloud image is not Debian 13"
  grep -Fq '"type": "official"' "${NOCLOUD_METADATA}" || die \
    "the pinned Debian NoCloud image is not marked official"
  grep -Fq '"vendor": "nocloud"' "${NOCLOUD_METADATA}" || die \
    "the pinned Debian cloud image is not the NoCloud variant"
  grep -Fq "\"version\": \"${NOCLOUD_BUILD_ID}\"" "${NOCLOUD_METADATA}" || die \
    "the pinned Debian NoCloud metadata has an unexpected build id"

  local package
  local package_count
  package_count="$(grep -c '"name":' "${NOCLOUD_METADATA}")"
  [[ "${package_count}" == 271 ]] || die \
    "the pinned Debian NoCloud metadata does not contain the expected 271 packages"
  for package in \
    bash bash-completion ca-certificates coreutils curl e2fsprogs file less man-db manpages \
    nano procps psmisc python3 screen vim vim-tiny xz-utils zstd; do
    grep -Fq "\"name\": \"${package}\"" "${NOCLOUD_METADATA}" || die \
      "the pinned Debian NoCloud metadata is missing package ${package}"
  done
  printf 'Using verified Debian 13 NoCloud riscv64 build %s (%s packages).\n' \
    "${NOCLOUD_BUILD_ID}" "${package_count}"
}

prepare_rootfs_downloads() {
  case "${ROOTFS_SOURCE}" in
    nocloud) prepare_nocloud_downloads ;;
    oci) prepare_oci_downloads ;;
    *) die "rootfs source was not selected" ;;
  esac
}

nocloud_e2fsprogs_is_expected() {
  local e2fsck_version debugfs_version
  [[ -x "${E2FSPROGS_E2FSCK}" && -x "${E2FSPROGS_DEBUGFS}" && \
     -f "${E2FSPROGS_BUILD_STAMP}" ]] || return 1
  grep -Fqx "${E2FSPROGS_BUILD_SCHEMA}" "${E2FSPROGS_BUILD_STAMP}" || return 1
  e2fsck_version="$("${E2FSPROGS_E2FSCK}" -V 2>&1)" || return 1
  debugfs_version="$("${E2FSPROGS_DEBUGFS}" -V 2>&1)" || return 1
  [[ "${e2fsck_version}" == *"e2fsck ${E2FSPROGS_VERSION}"* ]] &&
    [[ "${debugfs_version}" == *"debugfs ${E2FSPROGS_VERSION}"* ]]
}

prepare_nocloud_e2fsprogs() {
  local jobs="$1"
  local temporary_source="${E2FSPROGS_SOURCE_DIR}.tmp.$$"
  local temporary_build="${E2FSPROGS_BUILD_DIR}.tmp.$$"

  if nocloud_e2fsprogs_is_expected; then
    printf 'Using pinned host e2fsprogs %s for the NoCloud ext4 image.\n' \
      "${E2FSPROGS_VERSION}"
    return
  fi

  download_checked "${E2FSPROGS_ARCHIVE}" "${E2FSPROGS_SHA256}" "${E2FSPROGS_URL}"
  TEMPORARY_PATHS=("${temporary_source}" "${temporary_build}")
  rm -rf -- "${temporary_source}" "${temporary_build}"
  mkdir -p -- "${temporary_source}" "${temporary_build}"
  printf 'Building pinned host e2fsprogs %s for orphan_file support...\n' \
    "${E2FSPROGS_VERSION}"
  tar -xJf "${E2FSPROGS_ARCHIVE}" --strip-components=1 -C "${temporary_source}"
  (
    cd -- "${temporary_build}"
    "${temporary_source}/configure" \
      --disable-nls --disable-defrag --disable-fuse2fs --disable-uuidd \
      --enable-libuuid --enable-libblkid
    make -j"${jobs}"
  )
  [[ -x "${temporary_build}/e2fsck/e2fsck" && \
     -x "${temporary_build}/debugfs/debugfs" ]] || die \
    "the pinned host e2fsprogs build did not produce e2fsck and debugfs"
  printf '%s\n' "${E2FSPROGS_BUILD_SCHEMA}" > \
    "${temporary_build}/valheim-build.stamp"

  rm -rf -- "${E2FSPROGS_SOURCE_DIR}" "${E2FSPROGS_BUILD_DIR}"
  mv -- "${temporary_source}" "${E2FSPROGS_SOURCE_DIR}"
  mv -- "${temporary_build}" "${E2FSPROGS_BUILD_DIR}"
  TEMPORARY_PATHS=()
  nocloud_e2fsprogs_is_expected || die \
    "the pinned host e2fsprogs build has an unexpected version"
}

passt_is_expected() {
  local version
  [[ -d "${PASST_SOURCE_DIR}" && -f "${PASST_SOURCE_STAMP}" && \
     -x "${PASST_BINARY}" && ! -L "${PASST_BINARY}" && \
     -x "${PASST_AVX2_BINARY}" && ! -L "${PASST_AVX2_BINARY}" && \
     -f "${PASST_BUILD_STAMP}" ]] || return 1
  grep -Fqx "${PASST_BUILD_SCHEMA}" "${PASST_SOURCE_STAMP}" || return 1
  grep -Fqx "${PASST_BUILD_SCHEMA}" "${PASST_BUILD_STAMP}" || return 1
  version="$("${PASST_BINARY}" --version 2>&1)" || return 1
  grep -Fq "${PASST_VERSION}" <<<"${version}"
}

prepare_passt() {
  local jobs="$1"
  local temporary_source="${PASST_SOURCE_DIR}.tmp.$$"
  local temporary_build="${PASST_BUILD_DIR}.tmp.$$"

  if passt_is_expected; then
    printf 'Using pinned passt %s: %s\n' "${PASST_VERSION}" "${PASST_BINARY}"
    return
  fi

  download_checked "${PASST_ARCHIVE}" "${PASST_SHA256}" "${PASST_URL}"
  TEMPORARY_PATHS=("${temporary_source}" "${temporary_build}")
  rm -rf -- "${temporary_source}" "${temporary_build}"
  mkdir -p -- "${temporary_source}" "${temporary_build}"
  printf 'Building pinned passt %s for unprivileged NAT...\n' "${PASST_VERSION}"
  tar -xJf "${PASST_ARCHIVE}" --strip-components=1 -C "${temporary_source}"
  cp -a -- "${temporary_source}/." "${temporary_build}/"
  make -C "${temporary_build}" VERSION="${PASST_VERSION}" -j"${jobs}" passt passt.avx2
  [[ -x "${temporary_build}/passt" && ! -L "${temporary_build}/passt" && \
     -x "${temporary_build}/passt.avx2" && ! -L "${temporary_build}/passt.avx2" ]] || die \
    "the pinned passt build did not produce its baseline and AVX2 executables"
  printf '%s\n' "${PASST_BUILD_SCHEMA}" > "${temporary_source}/valheim-source.stamp"
  printf '%s\n' "${PASST_BUILD_SCHEMA}" > "${temporary_build}/valheim-build.stamp"

  rm -rf -- "${PASST_SOURCE_DIR}" "${PASST_BUILD_DIR}"
  mv -- "${temporary_source}" "${PASST_SOURCE_DIR}"
  mv -- "${temporary_build}" "${PASST_BUILD_DIR}"
  TEMPORARY_PATHS=()
  passt_is_expected || die "the pinned passt build has an unexpected version"
}

nocloud_dev_profile_sha256() {
  printf '%s\n' "${NOCLOUD_DEV_PACKAGES[@]}" | sha256sum | awk '{print $1}'
}

qemu_riscv64_static_is_expected() {
  local version
  [[ -f "${QEMU_RISCV64_STATIC}" && ! -L "${QEMU_RISCV64_STATIC}" &&
     -x "${QEMU_RISCV64_STATIC}" ]] || return 1
  file_has_sha256 "${QEMU_RISCV64_STATIC}" "${QEMU_RISCV64_STATIC_SHA256}" || return 1
  version="$("${QEMU_RISCV64_STATIC}" --version | head -n 1)" || return 1
  [[ "${version}" == \
    "qemu-riscv64 version 6.2.0 (Debian 1:${QEMU_USER_STATIC_VERSION})" ]]
}

prepare_nocloud_qemu_static() {
  local work="$1"
  local extracted="${work}/qemu-user-static-extracted"
  local staged="${work}/qemu-user-static-staged"
  local candidate="${extracted}/usr/bin/qemu-riscv64-static"

  if qemu_riscv64_static_is_expected; then
    printf 'Using pinned static qemu-riscv64 for the package-install chroot.\n'
    return
  fi

  require_command dpkg-deb
  download_checked \
    "${QEMU_USER_STATIC_ARCHIVE}" "${QEMU_USER_STATIC_SHA256}" \
    "${QEMU_USER_STATIC_URL}"
  rm -rf -- "${extracted}" "${staged}"
  mkdir -p -- "${extracted}" "${staged}"
  dpkg-deb --extract "${QEMU_USER_STATIC_ARCHIVE}" "${extracted}"
  [[ -f "${candidate}" && ! -L "${candidate}" ]] || die \
    "the pinned qemu-user-static package did not contain qemu-riscv64-static"
  file_has_sha256 "${candidate}" "${QEMU_RISCV64_STATIC_SHA256}" || die \
    "the qemu-riscv64-static binary had an unexpected checksum"
  install -m 0755 "${candidate}" "${staged}/qemu-riscv64-static"

  mkdir -p -- "${HOST_TOOLS_DIR}"
  rm -rf -- "${QEMU_USER_STATIC_DIR}"
  mv -- "${staged}" "${QEMU_USER_STATIC_DIR}"
  qemu_riscv64_static_is_expected || die \
    "the extracted qemu-riscv64-static binary had an unexpected version"
  printf 'Prepared pinned static qemu-riscv64 %s.\n' "${QEMU_USER_STATIC_VERSION}"
}

install_nocloud_dev_packages() {
  local image="$1"
  local work="$2"
  local mount_dir="${work}/chroot-root"
  local binfmt_state="${work}/binfmt-state"
  local command
  local -a root_command=(
    unshare --mount --pid --propagation private --fork --kill-child=SIGTERM
    /bin/bash "${NOCLOUD_CHROOT_HELPER}"
    "${image}" "${mount_dir}" "${QEMU_RISCV64_STATIC}" "${binfmt_state}"
    "${NOCLOUD_DEBIAN_SNAPSHOT}" "${NOCLOUD_SECURITY_SNAPSHOT}"
    "${NOCLOUD_DEV_PACKAGES[@]}"
  )

  [[ -f "${NOCLOUD_CHROOT_HELPER}" ]] || die \
    "missing NoCloud chroot helper: ${NOCLOUD_CHROOT_HELPER}"
  for command in chroot flock mount sync umount unshare update-binfmts; do
    require_command "${command}"
  done
  if (( EUID != 0 )); then
    require_command sudo
  fi
  prepare_nocloud_qemu_static "${work}"
  mkdir -p -- "${mount_dir}" "${binfmt_state}"

  if (( EUID == 0 )); then
    printf 'Installing the NoCloud development profile in an isolated mount/chroot as root.\n'
    "${root_command[@]}"
  else
    printf 'The first NoCloud base build needs sudo for an isolated mount/chroot.\n'
    sudo "${root_command[@]}"
  fi
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

oci_rootfs_schema() {
  local init_sha="$1"
  printf 'v%s:layer=%s:init=%s:size=%s:block=%s:uuid=%s:label=%s:features=%s' \
    "${OCI_ROOTFS_SCHEMA_VERSION}" "${DEBIAN_LAYER_SHA256}" "${init_sha}" \
    "${OCI_ROOTFS_SIZE_BYTES}" "${OCI_ROOTFS_BLOCK_SIZE}" "${OCI_ROOTFS_UUID}" \
    "${OCI_ROOTFS_LABEL}" "${OCI_ROOTFS_FEATURES}"
}

rootfs_schema() {
  local chroot_helper_sha dev_profile_sha init_sha
  init_sha="$(sha256sum "${OVERLAY_INIT}" | awk '{print $1}')"
  case "${ROOTFS_SOURCE}" in
    nocloud)
      chroot_helper_sha="$(sha256sum "${NOCLOUD_CHROOT_HELPER}" | awk '{print $1}')"
      dev_profile_sha="$(nocloud_dev_profile_sha256)"
      printf 'v%s:source=nocloud:build=%s:archive=%s:metadata=%s:host-e2fsprogs=%s:init=%s:size=%s:block=%s:start-sector=%s:sectors=%s:dev-profile=%s:packages=%s:package-count=%s:manifest=%s:debian-snapshot=%s:security-snapshot=%s:qemu=%s:chroot-helper=%s' \
        "${NOCLOUD_ROOTFS_SCHEMA_VERSION}" "${NOCLOUD_BUILD_ID}" \
        "${NOCLOUD_ARCHIVE_SHA512}" "${NOCLOUD_METADATA_SHA512}" \
        "${E2FSPROGS_SHA256}" "${init_sha}" \
        "${NOCLOUD_ROOTFS_SIZE_BYTES}" "${NOCLOUD_ROOTFS_BLOCK_SIZE}" \
        "${NOCLOUD_ROOT_START_SECTORS}" "${NOCLOUD_ROOT_SIZE_SECTORS}" \
        "${NOCLOUD_DEV_PROFILE_VERSION}" "${dev_profile_sha}" \
        "${NOCLOUD_ENRICHED_PACKAGE_COUNT}" "${NOCLOUD_ENRICHED_MANIFEST_SHA256}" \
        "${NOCLOUD_DEBIAN_SNAPSHOT}" "${NOCLOUD_SECURITY_SNAPSHOT}" \
        "${QEMU_USER_STATIC_SHA256}" "${chroot_helper_sha}"
      ;;
    oci)
      oci_rootfs_schema "${init_sha}"
      ;;
    *)
      die "rootfs source was not selected"
      ;;
  esac
}

debugfs_stat() {
  local image="$1"
  local path="$2"
  "${ROOTFS_DEBUGFS}" -R "stat ${path}" "${image}" 2>/dev/null
}

verify_rootfs_metadata() {
  local image="$1"
  local expected_init_sha="$2"
  local actual_features allocated_bytes image_size init_sha os_release package package_count
  local installed_count package_manifest_sha package_status
  local stats init_stat console_stat null_stat tty_stat shadow_stat device_stat
  local perl_inode resolv_stat tool tool_stat versioned_perl_inode

  [[ -f "${image}" ]] || return 1
  image_size="$(stat -c '%s' "${image}")"
  [[ "${image_size}" == "${ROOTFS_SIZE_BYTES}" ]] || return 1
  allocated_bytes="$(( $(stat -c '%b' "${image}") * 512 ))"
  (( allocated_bytes < ROOTFS_SIZE_BYTES )) || return 1
  "${ROOTFS_E2FSCK}" -fn "${image}" >/dev/null 2>&1 || return 1

  stats="$("${ROOTFS_DEBUGFS}" -R stats "${image}" 2>/dev/null)" || return 1
  grep -Eq '^Filesystem state:[[:space:]]+clean$' <<<"${stats}" || return 1
  grep -Eq "^Block size:[[:space:]]+${ROOTFS_BLOCK_SIZE}$" <<<"${stats}" || return 1
  actual_features="$(awk '
    /^Filesystem features:/ {
      sub(/^Filesystem features:[[:space:]]*/, "")
      print
      exit
    }
  ' <<<"${stats}")"

  init_stat="$(debugfs_stat "${image}" /init)" || return 1
  grep -Fq 'Type: regular    Mode:  0755' <<<"${init_stat}" || return 1
  grep -Eq '^User:[[:space:]]+0[[:space:]]+Group:[[:space:]]+0([[:space:]]|$)' \
    <<<"${init_stat}" || return 1
  init_sha="$(
    "${ROOTFS_DEBUGFS}" -R 'cat /init' "${image}" 2>/dev/null | sha256sum | awk '{print $1}'
  )"
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
  for device_stat in "${console_stat}" "${null_stat}" "${tty_stat}"; do
    grep -Eq '^User:[[:space:]]+0[[:space:]]+Group:[[:space:]]+0([[:space:]]|$)' \
      <<<"${device_stat}" || return 1
  done

  shadow_stat="$(debugfs_stat "${image}" /etc/shadow)" || return 1
  grep -Fq 'Type: regular    Mode:  0640' <<<"${shadow_stat}" || return 1
  grep -Eq '^User:[[:space:]]+0[[:space:]]+Group:[[:space:]]+42([[:space:]]|$)' \
    <<<"${shadow_stat}" || return 1

  case "${ROOTFS_SOURCE}" in
    nocloud)
      grep -Eq "^Filesystem UUID:[[:space:]]+${NOCLOUD_ROOTFS_UUID}$" <<<"${stats}" || return 1
      grep -Eq "^Block count:[[:space:]]+${NOCLOUD_ROOTFS_BLOCK_COUNT}$" \
        <<<"${stats}" || return 1
      [[ "${actual_features}" == "${NOCLOUD_ROOTFS_FEATURES}" ]] || return 1
      os_release="$(
        "${ROOTFS_DEBUGFS}" -R 'cat /usr/lib/os-release' "${image}" 2>/dev/null
      )" || return 1
      grep -Fqx 'VERSION_ID="13"' <<<"${os_release}" || return 1
      grep -Fqx 'VERSION_CODENAME=trixie' <<<"${os_release}" || return 1
      package_status="$(
        "${ROOTFS_DEBUGFS}" -R 'cat /var/lib/dpkg/status' "${image}" 2>/dev/null
      )" || return 1
      package_count="$(awk '/^Package: / { count++ } END { print count + 0 }' \
        <<<"${package_status}")"
      installed_count="$(awk '
        /^Status: install ok installed$/ { count++ }
        END { print count + 0 }
      ' <<<"${package_status}")"
      [[ "${package_count}" == "${NOCLOUD_ENRICHED_PACKAGE_COUNT}" ]] || return 1
      [[ "${installed_count}" == "${NOCLOUD_ENRICHED_PACKAGE_COUNT}" ]] || return 1
      package_manifest_sha="$(
        awk '
          /^Package: / { package=$2 }
          /^Status: / { status=$0 }
          /^Architecture: / { architecture=$2 }
          /^Version: / { version=$2 }
          /^$/ {
            if (status == "Status: install ok installed") {
              print package "\t" architecture "\t" version
            }
            package=architecture=version=status=""
          }
          END {
            if (status == "Status: install ok installed") {
              print package "\t" architecture "\t" version
            }
          }
        ' <<<"${package_status}" | LC_ALL=C sort | sha256sum | awk '{print $1}'
      )"
      [[ "${package_manifest_sha}" == "${NOCLOUD_ENRICHED_MANIFEST_SHA256}" ]] || \
        return 1
      for package in \
        bash bash-completion ca-certificates coreutils curl e2fsprogs file less man-db manpages \
        nano procps psmisc python3 screen vim vim-tiny xz-utils zstd; do
        grep -Fqx "Package: ${package}" <<<"${package_status}" || return 1
      done
      for package in "${NOCLOUD_DEV_PACKAGES[@]}"; do
        grep -Fqx "Package: ${package}" <<<"${package_status}" || return 1
      done
      for tool in \
        /usr/bin/gcc /usr/bin/g++ /usr/bin/make /usr/bin/git /usr/bin/pkg-config \
        /usr/bin/cmake /usr/bin/ninja /usr/bin/meson /usr/bin/autoconf \
        /usr/bin/automake /usr/bin/libtoolize /usr/bin/bison /usr/bin/flex \
        /usr/bin/patch /usr/bin/gdb /usr/bin/strace /usr/bin/lsof /usr/bin/jq \
        /usr/bin/rsync /usr/bin/wget /usr/bin/fakeroot /usr/bin/unzip \
        /usr/bin/zip /usr/bin/tree /usr/bin/pip3 /usr/bin/python3-config; do
        tool_stat="$(debugfs_stat "${image}" "${tool}")" || return 1
        [[ -n "${tool_stat}" ]] || return 1
      done
      resolv_stat="$(debugfs_stat "${image}" /etc/resolv.conf)" || return 1
      grep -Fq 'Type: symlink' <<<"${resolv_stat}" || return 1
      grep -Fq 'Fast link dest: "../run/systemd/resolve/stub-resolv.conf"' \
        <<<"${resolv_stat}" || return 1
      [[ -z "$(debugfs_stat "${image}" /usr/sbin/policy-rc.d)" ]] || return 1
      ;;
    oci)
      grep -Eq "^Filesystem volume name:[[:space:]]+${OCI_ROOTFS_LABEL}$" \
        <<<"${stats}" || return 1
      grep -Eq "^Filesystem UUID:[[:space:]]+${OCI_ROOTFS_UUID}$" <<<"${stats}" || return 1
      grep -Eq "^Block count:[[:space:]]+${OCI_ROOTFS_BLOCK_COUNT}$" \
        <<<"${stats}" || return 1
      [[ "${actual_features}" == "${OCI_ROOTFS_FEATURES//,/ }" ]] || return 1
      perl_inode="$(debugfs_stat "${image}" /usr/bin/perl | awk 'NR == 1 { print $2 }')"
      versioned_perl_inode="$(
        debugfs_stat "${image}" /usr/bin/perl5.40.1 | awk 'NR == 1 { print $2 }'
      )"
      [[ -n "${perl_inode}" && "${perl_inode}" == "${versioned_perl_inode}" ]] || return 1
      ;;
    *)
      return 1
      ;;
  esac
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

prepare_oci_rootfs_base() {
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

  TEMPORARY_PATHS=("${work}" "${temporary}" "${temporary_stamp}")
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
    "${OCI_ROOTFS_SIZE_BYTES}" "${OCI_ROOTFS_BLOCK_SIZE}" "${OCI_ROOTFS_UUID}" \
    "${OCI_ROOTFS_LABEL}" "${OCI_ROOTFS_FEATURES}"; then
    rm -rf -- "${work}"
    rm -f -- "${temporary}" "${temporary_stamp}"
    die "failed to construct the Debian ext4 base under fakeroot"
  fi

  if ! verify_rootfs_metadata "${temporary}" "${init_sha}"; then
    "${ROOTFS_E2FSCK}" -fn "${temporary}" || true
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
  TEMPORARY_PATHS=()
  printf 'Verified ext4 UUID, label, features, ownership, hardlinks, device nodes, and fsck.\n'
}

write_nocloud_overlay() {
  local image="$1"

  debugfs_stat "${image}" /dev >/dev/null || return 1
  "${ROOTFS_DEBUGFS}" -w -R 'rm /init' "${image}" >/dev/null 2>&1 || true
  "${ROOTFS_DEBUGFS}" -w -R "write \"${OVERLAY_INIT}\" /init" \
    "${image}" >/dev/null 2>&1 || return 1
  "${ROOTFS_DEBUGFS}" -w -R 'set_inode_field /init mode 0100755' \
    "${image}" >/dev/null 2>&1 || return 1
  "${ROOTFS_DEBUGFS}" -w -R 'set_inode_field /init uid 0' \
    "${image}" >/dev/null 2>&1 || return 1
  "${ROOTFS_DEBUGFS}" -w -R 'set_inode_field /init gid 0' \
    "${image}" >/dev/null 2>&1 || return 1

  printf '%s\n' \
    'cd /dev' \
    'rm console' \
    'rm null' \
    'rm tty' \
    'mknod console c 5 1' \
    'mknod null c 1 3' \
    'mknod tty c 5 0' | "${ROOTFS_DEBUGFS}" -w "${image}" >/dev/null 2>&1 || return 1
  "${ROOTFS_DEBUGFS}" -w -R 'set_inode_field /dev/console mode 020600' \
    "${image}" >/dev/null 2>&1 || return 1
  "${ROOTFS_DEBUGFS}" -w -R 'set_inode_field /dev/null mode 020666' \
    "${image}" >/dev/null 2>&1 || return 1
  "${ROOTFS_DEBUGFS}" -w -R 'set_inode_field /dev/tty mode 020666' \
    "${image}" >/dev/null 2>&1 || return 1
  local device
  for device in console null tty; do
    "${ROOTFS_DEBUGFS}" -w -R "set_inode_field /dev/${device} uid 0" \
      "${image}" >/dev/null 2>&1 || return 1
    "${ROOTFS_DEBUGFS}" -w -R "set_inode_field /dev/${device} gid 0" \
      "${image}" >/dev/null 2>&1 || return 1
  done
}

prepare_nocloud_rootfs_base() {
  [[ -x "${OVERLAY_INIT}" ]] || die "missing executable overlay: ${OVERLAY_INIT}"

  local expected_schema init_sha image_sha partition_dump root_end_bytes
  local work="${BUILD_DIR}/nocloud-ext4-work.$$"
  local raw_disk="${work}/disk.raw"
  local temporary="${ROOTFS_BASE_IMAGE}.tmp.$$"
  local temporary_stamp="${ROOTFS_BASE_STAMP}.tmp.$$"
  expected_schema="$(rootfs_schema)"
  init_sha="$(sha256sum "${OVERLAY_INIT}" | awk '{print $1}')"

  if cached_rootfs_base_is_valid "${expected_schema}" "${init_sha}"; then
    chmod 0444 "${ROOTFS_BASE_IMAGE}"
    printf 'Using verified read-only Debian NoCloud ext4 base: %s\n' "${ROOTFS_BASE_IMAGE}"
    return
  fi
  if [[ -e "${ROOTFS_BASE_IMAGE}" || -e "${ROOTFS_BASE_STAMP}" ]]; then
    printf 'Rebuilding stale or invalid Debian NoCloud ext4 base.\n'
  fi

  TEMPORARY_PATHS=("${work}" "${temporary}" "${temporary_stamp}")
  rm -rf -- "${work}"
  rm -f -- "${temporary}" "${temporary_stamp}"
  mkdir -p -- "${work}"
  printf 'Extracting the sparse Debian NoCloud disk and its ext4 root partition...\n'
  if ! tar --extract --xz --sparse --no-same-owner --file "${NOCLOUD_ARCHIVE}" \
      --directory "${work}" -- disk.raw; then
    rm -rf -- "${work}"
    die "failed to extract disk.raw from ${NOCLOUD_ARCHIVE}"
  fi
  [[ -f "${raw_disk}" ]] || {
    rm -rf -- "${work}"
    die "the Debian NoCloud archive did not contain disk.raw"
  }
  [[ "$(stat -c '%s' "${raw_disk}")" == "${NOCLOUD_DISK_SIZE_BYTES}" ]] || {
    rm -rf -- "${work}"
    die "the Debian NoCloud raw disk has an unexpected size"
  }
  sfdisk --verify "${raw_disk}" >/dev/null 2>&1 || {
    rm -rf -- "${work}"
    die "the Debian NoCloud GPT failed verification"
  }
  partition_dump="$(sfdisk --dump "${raw_disk}")" || {
    rm -rf -- "${work}"
    die "failed to inspect the Debian NoCloud GPT"
  }
  grep -Fqx 'label: gpt' <<<"${partition_dump}" || {
    rm -rf -- "${work}"
    die "the Debian NoCloud image does not use GPT"
  }
  grep -Eq \
    "start=[[:space:]]*${NOCLOUD_ROOT_START_SECTORS},[[:space:]]*size=[[:space:]]*${NOCLOUD_ROOT_SIZE_SECTORS},[[:space:]]*type=${NOCLOUD_ROOT_PARTITION_TYPE}" \
    <<<"${partition_dump}" || {
      rm -rf -- "${work}"
      die "the Debian NoCloud root partition layout is unexpected"
    }
  root_end_bytes="$((
    (NOCLOUD_ROOT_START_SECTORS + NOCLOUD_ROOT_SIZE_SECTORS) * NOCLOUD_SECTOR_SIZE
  ))"
  (( root_end_bytes <= NOCLOUD_DISK_SIZE_BYTES )) || {
    rm -rf -- "${work}"
    die "the Debian NoCloud root partition extends beyond disk.raw"
  }

  if ! dd if="${raw_disk}" of="${temporary}" bs=65536 skip=2048 count=47088 \
      conv=sparse status=none; then
    rm -rf -- "${work}"
    rm -f -- "${temporary}" "${temporary_stamp}"
    die "failed to extract the Debian NoCloud ext4 root partition"
  fi
  [[ "$(stat -c '%s' "${temporary}")" == "${NOCLOUD_ROOTFS_SIZE_BYTES}" ]] || {
    rm -rf -- "${work}"
    rm -f -- "${temporary}" "${temporary_stamp}"
    die "the extracted Debian NoCloud ext4 root partition has an unexpected size"
  }
  "${ROOTFS_E2FSCK}" -fn "${temporary}" >/dev/null 2>&1 || {
    rm -rf -- "${work}"
    rm -f -- "${temporary}" "${temporary_stamp}"
    die "the extracted Debian NoCloud ext4 root partition failed its initial fsck"
  }
  install_nocloud_dev_packages "${temporary}" "${work}"
  if ! write_nocloud_overlay "${temporary}" || \
     ! verify_rootfs_metadata "${temporary}" "${init_sha}"; then
    "${ROOTFS_E2FSCK}" -fn "${temporary}" || true
    rm -rf -- "${work}"
    rm -f -- "${temporary}" "${temporary_stamp}"
    die "the adapted Debian NoCloud ext4 base failed metadata verification"
  fi

  image_sha="$(sha256sum "${temporary}" | awk '{print $1}')"
  printf 'schema=%s\nsha256=%s\n' "${expected_schema}" "${image_sha}" > "${temporary_stamp}"
  chmod 0444 "${temporary}"
  mv -f -- "${temporary}" "${ROOTFS_BASE_IMAGE}"
  mv -f -- "${temporary_stamp}" "${ROOTFS_BASE_STAMP}"
  rm -rf -- "${work}"
  TEMPORARY_PATHS=()
  printf 'Verified NoCloud GPT/p1, development packages, overlay, device nodes, and fsck.\n'
}

prepare_rootfs_base() {
  case "${ROOTFS_SOURCE}" in
    nocloud) prepare_nocloud_rootfs_base ;;
    oci) prepare_oci_rootfs_base ;;
    *) die "rootfs source was not selected" ;;
  esac
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
  TEMPORARY_PATHS=("${temporary}" "${temporary_stamp}")
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
  TEMPORARY_PATHS=()
}

acquire_prepare_lock() {
  exec 7>>"${PREPARE_LOCK}" || die "cannot open Linux demo prepare lock: ${PREPARE_LOCK}"
  flock --exclusive --nonblock 7 || die \
    "another Linux demo process is preparing shared downloads or build artifacts"
}

release_prepare_lock() {
  flock --unlock 7
  exec 7>&-
}

acquire_managed_runtime_lock() {
  local runtime="$1"
  exec 8>>"${runtime}.lock" || die \
    "cannot open runtime lock: ${runtime}.lock"
  flock --exclusive --nonblock 8 || die \
    "the managed Debian runtime is already in use by another process: ${runtime}"
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
    --enable NET \
    --enable PACKET \
    --enable UNIX \
    --enable INET \
    --disable IPV6 \
    --enable NETDEVICES \
    --enable IP_PNP \
    --enable IP_PNP_DHCP \
    --disable PCI \
    --enable BLOCK \
    --enable VIRTIO \
    --enable VIRTIO_BLK \
    --enable VIRTIO_NET \
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
  for option in \
    CONFIG_NET CONFIG_PACKET CONFIG_UNIX CONFIG_INET CONFIG_NETDEVICES \
    CONFIG_IP_PNP CONFIG_IP_PNP_DHCP \
    CONFIG_VIRTIO CONFIG_VIRTIO_BLK CONFIG_VIRTIO_NET CONFIG_VIRTIO_MMIO CONFIG_EXT4_FS; do
    grep -Fqx "${option}=y" "${config}" || die \
      "kernel config did not build ${option} into the Image"
  done
  grep -Fqx '# CONFIG_IPV6 is not set' "${config}" || die \
    "kernel config unexpectedly enabled IPv6"
  grep -Fqx '# CONFIG_SMP is not set' "${config}" || die \
    "kernel config unexpectedly enabled SMP"
}

main() {
  local command
  local argument
  local disk_path=""
  local disk_overridden=0
  local i
  local managed_path
  local net_mode="none"
  local net_subnet_overridden=0
  local passt_overridden=0
  local rootfs_source="nocloud"
  local -a cli_args=("$@")
  local -a passthrough_args=()
  local -a engine_args=(--engine jit)
  local -a cmdline_args=(--cmdline "${KERNEL_CMDLINE}")
  local -a disk_args=()
  local -a passt_args=()
  for ((i = 0; i < ${#cli_args[@]}; i++)); do
    argument="${cli_args[i]}"
    case "${argument}" in
      --from-oci)
        rootfs_source="oci"
        continue
        ;;
      --from-oci=*)
        die "--from-oci does not take a value"
        ;;
    esac
    passthrough_args+=("${argument}")
    case "${argument}" in
      --engine)
        engine_args=()
        ;;
      --engine=*)
        engine_args=()
        ;;
      --net)
        ((i + 1 < ${#cli_args[@]})) || die "--net requires a mode"
        i=$((i + 1))
        net_mode="${cli_args[i]}"
        passthrough_args+=("${net_mode}")
        ;;
      --net=*)
        net_mode="${argument#--net=}"
        ;;
      --net-subnet)
        net_subnet_overridden=1
        ((i + 1 < ${#cli_args[@]})) || die "--net-subnet requires an IPv4 CIDR"
        i=$((i + 1))
        [[ -n "${cli_args[i]}" ]] || die "--net-subnet requires a non-empty IPv4 CIDR"
        passthrough_args+=("${cli_args[i]}")
        ;;
      --net-subnet=)
        die "--net-subnet requires a non-empty IPv4 CIDR"
        ;;
      --net-subnet=*)
        net_subnet_overridden=1
        ;;
      --passt)
        passt_overridden=1
        ((i + 1 < ${#cli_args[@]})) || die "--passt requires a path"
        i=$((i + 1))
        [[ -n "${cli_args[i]}" ]] || die "--passt requires a non-empty path"
        passthrough_args+=("${cli_args[i]}")
        ;;
      --passt=)
        die "--passt requires a non-empty path"
        ;;
      --passt=*)
        passt_overridden=1
        ;;
      --disk | -d)
        disk_overridden=1
        ((i + 1 < ${#cli_args[@]})) || die "${argument} requires a disk path"
        i=$((i + 1))
        disk_path="${cli_args[i]}"
        passthrough_args+=("${disk_path}")
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
  case "${net_mode}" in
    none)
      if [[ "${net_subnet_overridden}" == 1 || "${passt_overridden}" == 1 ]]; then
        die "--net-subnet and --passt require --net nat"
      fi
      ;;
    nat)
      if [[ ${#cmdline_args[@]} -ne 0 ]]; then
        cmdline_args=(--cmdline "${KERNEL_CMDLINE} ip=dhcp")
      fi
      ;;
    *) die "--net mode must be none or nat" ;;
  esac
  select_rootfs_source "${rootfs_source}"

  case "${RESET_DISK:-0}" in
    0 | 1) ;;
    *) die "RESET_DISK must be 0 or 1" ;;
  esac
  if [[ "${disk_overridden}" == 1 && "${RESET_DISK:-0}" == 1 ]]; then
    die "RESET_DISK=1 cannot be combined with an explicit --disk/-d"
  fi

  for command in \
    awk bash bc bison cc chmod cp curl dtc flex flock grep gzip install make mkdir mv \
    perl python3 rm rustup sha256sum stat tar xz; do
    require_command "${command}"
  done
  if [[ "${net_mode}" == "nat" && "${passt_overridden}" == 0 ]]; then
    require_command getconf
  fi
  if [[ "${disk_overridden}" == 0 ]]; then
    if [[ "${ROOTFS_SOURCE}" == "nocloud" ]]; then
      for command in dd sfdisk sha512sum; do
        require_command "${command}"
      done
    else
      for command in chown debugfs e2fsck fakeroot mke2fs mknod truncate; do
        require_command "${command}"
      done
    fi
  fi
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
  acquire_prepare_lock
  prepare_toolchain
  prepare_linux_source
  if [[ "${net_mode}" == "nat" && "${passt_overridden}" == 0 ]]; then
    prepare_passt "${jobs}"
    passt_args=(--passt "${PASST_BINARY}")
  fi
  if [[ "${disk_overridden}" == 0 ]]; then
    if [[ "${ROOTFS_SOURCE}" == "oci" ]]; then
      printf 'Using the pinned Debian 13 slim OCI rootfs source (--from-oci).\n'
    else
      printf 'Using the pinned Debian 13 NoCloud rootfs source (default).\n'
    fi
    acquire_managed_runtime_lock "${ROOTFS_RUNTIME_IMAGE}"
    if [[ "${ROOTFS_SOURCE}" == "nocloud" ]]; then
      prepare_nocloud_e2fsprogs "${jobs}"
    fi
    prepare_rootfs_downloads
    prepare_rootfs_base
    prepare_runtime_rootfs
    acquire_disk_lock "${ROOTFS_RUNTIME_IMAGE}"
    disk_args=(--disk "${ROOTFS_RUNTIME_IMAGE}")
  else
    [[ -n "${disk_path}" ]] || die "--disk/-d requires a non-empty disk path"
    [[ -f "${disk_path}" ]] || die "disk image not found: ${disk_path}"
    for managed_path in "${OCI_ROOTFS_BASE_IMAGE}" "${NOCLOUD_ROOTFS_BASE_IMAGE}"; do
      if [[ -f "${managed_path}" && "${disk_path}" -ef "${managed_path}" ]]; then
        die "refusing to use a verified read-only ext4 base as a writable runtime disk"
      fi
    done
    for managed_path in "${OCI_ROOTFS_RUNTIME_IMAGE}" "${NOCLOUD_ROOTFS_RUNTIME_IMAGE}"; do
      if [[ -f "${managed_path}" && "${disk_path}" -ef "${managed_path}" ]]; then
        acquire_managed_runtime_lock "${managed_path}"
        break
      fi
    done
    acquire_disk_lock "${disk_path}"
    printf 'Using the rootfs disk supplied on the command line; managed rootfs preparation is skipped.\n'
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
  release_prepare_lock

  printf '\nStarting Debian 13 on Linux %s. Wait for the debian13# prompt; press Ctrl-C to exit.\n\n' \
    "${LINUX_VERSION}"
  exec "${VALHEIM_BIN}" \
    "${engine_args[@]}" \
    --bios "${RUSTSBI_BIOS}" \
    --kernel "${KERNEL_IMAGE}" \
    "${disk_args[@]}" \
    "${cmdline_args[@]}" \
    "${passt_args[@]}" \
    "${passthrough_args[@]}"
}

main "$@"
