#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifact_dir="$repo_root/target/rewrite-validation"
report_tmp=$(mktemp "${TMPDIR:-/tmp}/rewrite-stable-benchmark.XXXXXX")

cleanup() {
  rm -f "$report_tmp"
}
trap cleanup EXIT INT TERM

mkdir -p "$artifact_dir"
case "${1:-}" in
  voplay)
    artifact="$artifact_dir/voplay-stable-scene-benchmark.json"
    expected_name="voplay-stable-scene-single-object"
    (
      cd "$repo_root"
      cargo bench \
        --manifest-path ../voplay/rust/Cargo.toml \
        -p voplay-runtime \
        --bench stable_scene \
        --locked
    ) >"$report_tmp"
    ;;
  vogui)
    artifact="$artifact_dir/vogui-stable-ui-benchmark.json"
    expected_name="vogui-stable-ui-small-scope"
    (
      cd "$repo_root"
      cargo bench \
        --manifest-path ../vogui/rust/Cargo.toml \
        -p vogui-runtime \
        --bench stable_ui \
        --locked
    ) >"$report_tmp"
    ;;
  *)
    echo "usage: $0 voplay|vogui" >&2
    exit 2
    ;;
esac

if ! grep -q "\"name\":\"$expected_name\"" "$report_tmp" \
  || ! grep -q '"runs":' "$report_tmp" \
  || ! grep -q '"p95_ns":' "$report_tmp"; then
  cat "$report_tmp" >&2
  exit 1
fi
install -m 0644 "$report_tmp" "$artifact"
cat "$artifact"
