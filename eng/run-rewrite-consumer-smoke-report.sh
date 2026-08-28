#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifact_dir="$repo_root/target/rewrite-validation"
if [ "$#" -gt 1 ]; then
  echo "usage: $0 [artifact-stem]" >&2
  exit 2
fi
artifact_stem=${1:-rewrite-consumer-smoke}
case "$artifact_stem" in
  ""|*[!A-Za-z0-9._-]*)
    echo "invalid artifact stem: $artifact_stem" >&2
    exit 2
    ;;
esac
artifact="$artifact_dir/$artifact_stem.json"
log_artifact="$artifact_dir/$artifact_stem.log"
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
  || ! grep -q '"voplay_profile":"core"' "$report_tmp" \
  || ! grep -q '"generated_outputs_written_to_app":false' "$report_tmp"; then
  cat "$report_tmp" >&2
  exit 1
fi
tail -n 1 "$report_tmp" >"$summary_tmp"
install -m 0644 "$summary_tmp" "$artifact"
install -m 0644 "$report_tmp" "$log_artifact"
cat "$artifact"
