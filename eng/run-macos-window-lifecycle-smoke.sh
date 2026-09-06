#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifact_dir="$repo_root/target/ci/results/ecosystem"
if [ "$#" -gt 1 ]; then
  echo "usage: $0 [artifact-name.json]" >&2
  exit 2
fi
artifact_name=${1:-macos-window-lifecycle-smoke.json}
case "$artifact_name" in
  ""|*[!A-Za-z0-9._-]*)
    echo "invalid artifact name: $artifact_name" >&2
    exit 2
    ;;
esac
artifact="$artifact_dir/$artifact_name"
report_tmp=$(mktemp "${TMPDIR:-/tmp}/macos-window-lifecycle-smoke.XXXXXX")

cleanup() {
  rm -f "$report_tmp"
}
trap cleanup EXIT INT TERM

mkdir -p "$artifact_dir"
(
  cd "$repo_root"
  cargo run \
    -p vo-app-host-native \
    --example macos_window_lifecycle_smoke \
    --features macos-gpu \
    --locked
) >"$report_tmp"

if ! grep -q '"passed":true' "$report_tmp"; then
  cat "$report_tmp" >&2
  exit 1
fi
install -m 0644 "$report_tmp" "$artifact"
cat "$artifact"
