#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifact_dir="$repo_root/target/rewrite-validation"
artifact="$artifact_dir/release-matrix.json"
report_tmp=$(mktemp "${TMPDIR:-/tmp}/rewrite-release-matrix.XXXXXX")

cleanup() {
  rm -f "$report_tmp"
}
trap cleanup EXIT INT TERM

mkdir -p "$artifact_dir"
(
  cd "$repo_root"
  cargo run -q -p vo-dev --locked -- release matrix
) >"$report_tmp"

if ! grep -q '"include"' "$report_tmp" \
  || ! grep -q '"aarch64-apple-darwin"' "$report_tmp" \
  || ! grep -q '"x86_64-unknown-linux-gnu"' "$report_tmp"; then
  cat "$report_tmp" >&2
  exit 1
fi
install -m 0644 "$report_tmp" "$artifact"
cat "$artifact"
