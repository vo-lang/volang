#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifact_dir="$repo_root/target/rewrite-validation"
artifact="$artifact_dir/rewrite-consumer-smoke.json"
log_artifact="$artifact_dir/rewrite-consumer-smoke.log"
report_tmp=$(mktemp "${TMPDIR:-/tmp}/rewrite-consumer-smoke.XXXXXX")
summary_tmp=$(mktemp "${TMPDIR:-/tmp}/rewrite-consumer-smoke-summary.XXXXXX")

cleanup() {
  rm -f "$report_tmp" "$summary_tmp"
}
trap cleanup EXIT HUP INT TERM

mkdir -p "$artifact_dir"
(
  cd "$repo_root"
  sh eng/run-rewrite-consumer-smoke.sh
) >"$report_tmp"

if ! grep -q '"passed":true' "$report_tmp" \
  || ! grep -q '"vogui_profile":"headless"' "$report_tmp" \
  || ! grep -q '"voplay_profile":"core"' "$report_tmp" \
  || ! grep -q '"generated_outputs_written_to_app":false' "$report_tmp"; then
  cat "$report_tmp" >&2
  exit 1
fi
tail -n 1 "$report_tmp" >"$summary_tmp"
install -m 0644 "$summary_tmp" "$artifact"
install -m 0644 "$report_tmp" "$log_artifact"
cat "$artifact"
