#!/bin/sh
set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
studio_root="$repo_root/apps/studio"
manifest="$studio_root/src-tauri/Cargo.toml"
build_target="$repo_root/target/studio-webview-native-smoke"
binary="$build_target/debug/studio"
artifact_dir="$repo_root/target/rewrite-validation"
artifact="$artifact_dir/webview-native-smoke.json"
report_tmp=$(mktemp "${TMPDIR:-/tmp}/studio-webview-native-smoke.XXXXXX")
pid=

cleanup() {
  if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
    kill "$pid" 2>/dev/null || true
  fi
  rm -f "$report_tmp"
}
trap cleanup EXIT INT TERM

mkdir -p "$artifact_dir"
(
  cd "$studio_root"
  npm run build
)
cargo build \
  --manifest-path "$manifest" \
  --bin studio \
  --locked \
  --target-dir "$build_target"

STUDIO_WEBVIEW_NATIVE_SMOKE_OUTPUT="$report_tmp" "$binary" &
pid=$!

attempt=0
while kill -0 "$pid" 2>/dev/null; do
  if [ -s "$report_tmp" ]; then
    break
  fi
  attempt=$((attempt + 1))
  if [ "$attempt" -ge 600 ]; then
    echo "Studio WebView native smoke timed out after 60 seconds" >&2
    exit 1
  fi
  sleep 0.1
done

wait "$pid"
pid=
if [ ! -s "$report_tmp" ]; then
  echo "Studio exited without a WebView native smoke report" >&2
  exit 1
fi
if ! grep -q '"result": "passed"' "$report_tmp"; then
  cat "$report_tmp" >&2
  exit 1
fi
install -m 0644 "$report_tmp" "$artifact"
cat "$artifact"
