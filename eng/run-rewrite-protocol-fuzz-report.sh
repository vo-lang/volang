#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
  echo "usage: $0 app|framework" >&2
  exit 2
fi

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifact_dir="$repo_root/target/rewrite-validation"
summary="$artifact_dir/$1-protocol-fuzz-summary.json"
campaign_log="$artifact_dir/$1-protocol-fuzz.log"
summary_tmp=$(mktemp "${TMPDIR:-/tmp}/rewrite-protocol-fuzz-summary.XXXXXX")
log_tmp=$(mktemp "${TMPDIR:-/tmp}/rewrite-protocol-fuzz-log.XXXXXX")

cleanup() {
  rm -f "$summary_tmp" "$log_tmp"
}
trap cleanup EXIT INT TERM

mkdir -p "$artifact_dir"
(
  cd "$repo_root"
  sh eng/run-rewrite-protocol-fuzz.sh "$1"
) >"$summary_tmp" 2>"$log_tmp"

if ! grep -q "\"passed\":true,\"suite\":\"$1\",\"runs\":100000" "$summary_tmp" \
  || ! grep -q '#100000' "$log_tmp"; then
  cat "$summary_tmp" >&2
  cat "$log_tmp" >&2
  exit 1
fi
install -m 0644 "$summary_tmp" "$summary"
install -m 0644 "$log_tmp" "$campaign_log"
cat "$summary"
