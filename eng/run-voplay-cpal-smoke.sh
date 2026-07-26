#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifact_dir="$repo_root/target/rewrite-validation"
artifact="$artifact_dir/voplay-cpal-smoke.json"
report_tmp=$(mktemp "${TMPDIR:-/tmp}/voplay-cpal-smoke.XXXXXX")

cleanup() {
  rm -f "$report_tmp"
}
trap cleanup EXIT INT TERM

mkdir -p "$artifact_dir"
(
  cd "$repo_root"
  cargo run \
    --manifest-path ../voplay/rust/Cargo.toml \
    -p voplay-audio \
    --example cpal_smoke \
    --features native-cpal \
    --locked
) >"$report_tmp"

if ! grep -q '"passed":true' "$report_tmp"; then
  cat "$report_tmp" >&2
  exit 1
fi
install -m 0644 "$report_tmp" "$artifact"
cat "$artifact"
