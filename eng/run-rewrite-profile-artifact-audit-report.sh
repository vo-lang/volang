#!/bin/sh
set -eu

if [ "$#" -ne 1 ]; then
  echo "usage: $0 voplay|vogui" >&2
  exit 2
fi

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
artifact_dir="$repo_root/target/rewrite-validation"
artifact="$artifact_dir/$1-profile-artifact-audit.jsonl"
report_tmp=$(mktemp "${TMPDIR:-/tmp}/rewrite-profile-artifact-audit.XXXXXX")

cleanup() {
  rm -f "$report_tmp"
}
trap cleanup EXIT INT TERM

mkdir -p "$artifact_dir"
(
  cd "$repo_root"
  sh eng/run-rewrite-profile-artifact-audit.sh "$1"
) >"$report_tmp"

if ! grep -q "\"passed\":true,\"framework\":\"$1\"" "$report_tmp" \
  || ! grep -q '"artifact_sha256":' "$report_tmp" \
  || ! grep -q '"dependency_tree_sha256":' "$report_tmp"; then
  cat "$report_tmp" >&2
  exit 1
fi
install -m 0644 "$report_tmp" "$artifact"
cat "$artifact"
