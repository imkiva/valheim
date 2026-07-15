#!/usr/bin/env bash

set -euo pipefail

export LC_ALL=C

die() {
  printf 'chroot package install error: %s\n' "$*" >&2
  exit 1
}

[[ "${EUID}" == 0 ]] || die "this helper must run as root"
(( $# >= 6 )) || die \
  "usage: $0 IMAGE MOUNT_DIR QEMU_STATIC BINFMT_STATE_DIR DEBIAN_SNAPSHOT SECURITY_SNAPSHOT PACKAGE..."

readonly IMAGE="$1"
readonly MOUNT_DIR="$2"
readonly QEMU_STATIC="$3"
readonly BINFMT_STATE_DIR="$4"
readonly DEBIAN_SNAPSHOT="$5"
readonly SECURITY_SNAPSHOT="$6"
shift 6
readonly -a PACKAGES=("$@")

readonly BINFMT_MAGIC='\x7f\x45\x4c\x46\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xf3\x00'
readonly BINFMT_MASK='\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff'
readonly BINFMT_MAGIC_HEX='7f454c460201010000000000000000000200f300'
readonly BINFMT_MASK_HEX='ffffffffffffff00fffffffffffffffffeffffff'
readonly BINFMT_NAME="valheim-riscv64-${BASHPID}-${RANDOM}"
readonly BINFMT_ENTRY="/proc/sys/fs/binfmt_misc/${BINFMT_NAME}"

root_mounted=0
run_mounted=0
dev_mounted=0
proc_mounted=0
resolver_mounted=0
policy_created=0
binfmt_created=0

binfmt_entry_is_expected() {
  local entry="$1"
  [[ -f "${entry}" ]] || return 1
  grep -Fqx enabled "${entry}" &&
    grep -Fqx "interpreter ${QEMU_STATIC}" "${entry}" &&
    grep -Eq '^flags: .*F' "${entry}" &&
    grep -Fqx 'offset 0' "${entry}" &&
    grep -Fqx "magic ${BINFMT_MAGIC_HEX}" "${entry}" &&
    grep -Fqx "mask ${BINFMT_MASK_HEX}" "${entry}"
}

remove_owned_binfmt() {
  [[ "${binfmt_created}" == 1 ]] || return 0
  if [[ ! -e "${BINFMT_ENTRY}" ]]; then
    binfmt_created=0
    return 0
  fi
  if ! binfmt_entry_is_expected "${BINFMT_ENTRY}"; then
    printf 'warning: refusing to remove changed binfmt entry: %s\n' \
      "${BINFMT_ENTRY}" >&2
    return 1
  fi

  update-binfmts \
    --admindir "${BINFMT_STATE_DIR}" \
    --remove "${BINFMT_NAME}" "${QEMU_STATIC}" >/dev/null 2>&1 || true
  if [[ -e "${BINFMT_ENTRY}" ]]; then
    if ! binfmt_entry_is_expected "${BINFMT_ENTRY}"; then
      printf 'warning: refusing to remove changed binfmt entry: %s\n' \
        "${BINFMT_ENTRY}" >&2
      return 1
    fi
    if ! printf '%s\n' -1 > "${BINFMT_ENTRY}"; then
      printf 'warning: failed to remove binfmt entry: %s\n' "${BINFMT_ENTRY}" >&2
      return 1
    fi
  fi
  if [[ -e "${BINFMT_ENTRY}" ]]; then
    printf 'warning: binfmt entry remained after removal: %s\n' "${BINFMT_ENTRY}" >&2
    return 1
  fi
  binfmt_created=0
}

cleanup() {
  local rc=$?
  local cleanup_failed=0
  trap - EXIT INT TERM HUP
  set +e

  if [[ "${policy_created}" == 1 && "${root_mounted}" == 1 ]]; then
    rm -f -- "${MOUNT_DIR}/usr/sbin/policy-rc.d" || cleanup_failed=1
    policy_created=0
  fi
  if [[ "${root_mounted}" == 1 ]]; then
    sync "${MOUNT_DIR}" >/dev/null 2>&1 || cleanup_failed=1
  fi
  if [[ "${resolver_mounted}" == 1 ]]; then
    umount "${MOUNT_DIR}/run/systemd/resolve/stub-resolv.conf" || cleanup_failed=1
    resolver_mounted=0
  fi
  if [[ "${proc_mounted}" == 1 ]]; then
    umount "${MOUNT_DIR}/proc" || cleanup_failed=1
    proc_mounted=0
  fi
  if [[ "${dev_mounted}" == 1 ]]; then
    umount "${MOUNT_DIR}/dev" || cleanup_failed=1
    dev_mounted=0
  fi
  if [[ "${run_mounted}" == 1 ]]; then
    umount "${MOUNT_DIR}/run" || cleanup_failed=1
    run_mounted=0
  fi
  if [[ "${root_mounted}" == 1 ]]; then
    umount "${MOUNT_DIR}" || cleanup_failed=1
    root_mounted=0
  fi
  remove_owned_binfmt || cleanup_failed=1

  if [[ "${cleanup_failed}" == 1 ]]; then
    printf 'warning: chroot package install cleanup was incomplete\n' >&2
    [[ "${rc}" != 0 ]] || rc=1
  fi
  exit "${rc}"
}

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP

[[ -f "${IMAGE}" && ! -L "${IMAGE}" ]] || die "invalid ext4 image: ${IMAGE}"
[[ -x "${QEMU_STATIC}" && ! -L "${QEMU_STATIC}" ]] || die \
  "invalid static qemu-riscv64: ${QEMU_STATIC}"
[[ ${#PACKAGES[@]} -gt 0 ]] || die "the development package list is empty"
[[ -d /proc/sys/fs/binfmt_misc ]] || die "binfmt_misc is not mounted"
grep -Fqx enabled /proc/sys/fs/binfmt_misc/status || die "binfmt_misc is disabled"

mkdir -p -- "${MOUNT_DIR}" "${BINFMT_STATE_DIR}" /run/lock
exec 9>/run/lock/valheim-riscv64-binfmt.lock
flock --exclusive --wait 30 9 || die \
  "timed out waiting for another riscv64 binfmt operation"

# Recover only stale entries created by an interrupted invocation of this helper.
for stale_entry in /proc/sys/fs/binfmt_misc/valheim-riscv64-*; do
  [[ -e "${stale_entry}" ]] || continue
  if grep -Fqx "interpreter ${QEMU_STATIC}" "${stale_entry}" &&
     grep -Fqx "magic ${BINFMT_MAGIC_HEX}" "${stale_entry}" &&
     grep -Fqx "mask ${BINFMT_MASK_HEX}" "${stale_entry}"; then
    printf 'Removing stale Valheim binfmt entry: %s\n' "${stale_entry##*/}"
    printf '%s\n' -1 > "${stale_entry}"
  else
    die "found a foreign binfmt entry using Valheim's reserved prefix: ${stale_entry}"
  fi
done

# binfmt_misc checks its newest entry first. Always add our pinned interpreter so
# a pre-existing host qemu registration cannot change this chroot's behavior.
binfmt_created=1
update-binfmts \
  --admindir "${BINFMT_STATE_DIR}" \
  --install "${BINFMT_NAME}" "${QEMU_STATIC}" \
  --magic "${BINFMT_MAGIC}" --mask "${BINFMT_MASK}" \
  --fix-binary yes --preserve no --credentials no >/dev/null
binfmt_entry_is_expected "${BINFMT_ENTRY}" || die \
  "the temporary riscv64 binfmt entry did not match the requested format"
printf 'Registered temporary riscv64 binfmt entry: %s\n' "${BINFMT_NAME}"

if ! mount -o loop,rw,nosuid,nodev -- "${IMAGE}" "${MOUNT_DIR}"; then
  die "failed to loop-mount the NoCloud p1; the host kernel must support its ext4 features and provide loop/CAP_SYS_ADMIN access"
fi
root_mounted=1
mount -t tmpfs -o nosuid,nodev,noexec,mode=0755 tmpfs "${MOUNT_DIR}/run"
run_mounted=1
mount --bind /dev "${MOUNT_DIR}/dev"
dev_mounted=1
mount -t proc -o nosuid,nodev,noexec proc "${MOUNT_DIR}/proc"
proc_mounted=1

mkdir -p -- "${MOUNT_DIR}/run/systemd/resolve"
install -m 0644 /dev/null "${MOUNT_DIR}/run/systemd/resolve/stub-resolv.conf"
mount --bind /etc/resolv.conf "${MOUNT_DIR}/run/systemd/resolve/stub-resolv.conf"
resolver_mounted=1
mount -o remount,bind,ro "${MOUNT_DIR}/run/systemd/resolve/stub-resolv.conf"

[[ ! -e "${MOUNT_DIR}/usr/sbin/policy-rc.d" &&
   ! -L "${MOUNT_DIR}/usr/sbin/policy-rc.d" ]] || die \
  "the pinned rootfs unexpectedly already contains /usr/sbin/policy-rc.d"
printf '#!/bin/sh\nexit 101\n' > "${MOUNT_DIR}/usr/sbin/policy-rc.d"
policy_created=1
chmod 0755 "${MOUNT_DIR}/usr/sbin/policy-rc.d"

cat > "${MOUNT_DIR}/run/valheim-snapshot.sources" <<EOF
Types: deb
URIs: https://snapshot.debian.org/archive/debian/${DEBIAN_SNAPSHOT}/
Suites: trixie trixie-updates
Components: main
Architectures: riscv64
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg
Check-Valid-Until: no

Types: deb
URIs: https://snapshot.debian.org/archive/debian-security/${SECURITY_SNAPSHOT}/
Suites: trixie-security
Components: main
Architectures: riscv64
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg
Check-Valid-Until: no
EOF

readonly -a CHROOT_ENV=(
  /usr/bin/env -i
  HOME=/root
  PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
  LC_ALL=C.UTF-8
  DEBIAN_FRONTEND=noninteractive
  DEBCONF_NONINTERACTIVE_SEEN=true
  APT_LISTCHANGES_FRONTEND=none
  SYSTEMD_OFFLINE=1
  container=chroot
)
readonly -a APT_OPTIONS=(
  -o Dir::Etc::sourcelist=/run/valheim-snapshot.sources
  -o Dir::Etc::sourceparts=-
  -o Acquire::Check-Valid-Until=false
  -o Acquire::Languages=none
  -o Acquire::Retries=4
  -o Dpkg::Use-Pty=0
)

chroot_run() {
  chroot "${MOUNT_DIR}" "${CHROOT_ENV[@]}" "$@"
}

[[ "$(chroot_run /usr/bin/dpkg --print-architecture)" == riscv64 ]] || die \
  "the mounted rootfs is not a riscv64 dpkg installation"

printf 'Updating APT metadata from the fixed Debian snapshots...\n'
chroot_run /usr/bin/apt-get "${APT_OPTIONS[@]}" update
printf 'Installing %s top-level development packages in the NoCloud rootfs...\n' \
  "${#PACKAGES[@]}"
chroot_run /usr/bin/apt-get "${APT_OPTIONS[@]}" \
  --yes --no-install-recommends --no-install-suggests install "${PACKAGES[@]}"

dpkg_audit="$(chroot_run /usr/bin/dpkg --audit)"
[[ -z "${dpkg_audit}" ]] || die "dpkg --audit reported incomplete package state"
chroot_run /usr/bin/dpkg-query -W "${PACKAGES[@]}" >/dev/null

upgrade_simulation="$(chroot_run /usr/bin/apt-get "${APT_OPTIONS[@]}" --simulate upgrade)"
grep -Fq '0 upgraded, 0 newly installed, 0 to remove and 0 not upgraded.' \
  <<<"${upgrade_simulation}" || {
  printf '%s\n' "${upgrade_simulation}" >&2
  die "the fixed snapshots would upgrade packages in the pinned NoCloud image"
}

chroot_run /bin/bash -euc '
  work="$(mktemp -d /tmp/valheim-dev-smoke.XXXXXX)"
  trap '\''rm -rf -- "${work}"'\'' EXIT
  printf '\''#include <stdio.h>\nint main(void) { puts("VALHEIM_CC_OK"); return 0; }\n'\'' \
    > "${work}/smoke.c"
  gcc -O2 -Wall -Wextra -Werror "${work}/smoke.c" -o "${work}/smoke"
  [[ "$("${work}/smoke")" == VALHEIM_CC_OK ]]
  [[ "$(gcc -dumpmachine)" == riscv64-linux-gnu ]]
'

chroot_run /usr/bin/apt-get "${APT_OPTIONS[@]}" clean
rm -rf -- \
  "${MOUNT_DIR}/var/lib/apt/lists/"* \
  "${MOUNT_DIR}/var/cache/apt/"*.bin
rm -f -- "${MOUNT_DIR}/usr/sbin/policy-rc.d"
policy_created=0
sync "${MOUNT_DIR}"
printf 'The fixed Debian development package profile passed dpkg and compiler checks.\n'
