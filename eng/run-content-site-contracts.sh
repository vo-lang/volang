#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
vo_binary="${VO_UI_BIN:-$repo_root/target/debug/vo}"
site="$repo_root/ui/showcases/content-site"
snapshot="$(mktemp "${TMPDIR:-/tmp}/volang-content-site.XXXXXX")"
trap 'rm -f "$snapshot"' EXIT

actions=(
  "--click=Wasm AOT"
  "--wait-text=Compilation"
  "--click=Concurrency"
  "--wait-text=Scoped lifetime"
  "--click=Search"
  "--input=Search articles=goroutines"
  "--wait-text=Scoped lifetime, deterministic delivery, and bounded backpressure."
  "--wait-absent-text=Compilation"
  "--click=Subscribe"
  "--click=Join Field Notes"
  "--wait-text=Email address is required. Display name is required"
  "--input=Email address=ada"
  "--wait-text=Enter a valid email address. Display name is required"
  "--input=Email address=ada@example.test"
  "--input=Display name=Ada"
  "--wait-absent-text=Enter a valid email address. Display name is required"
  "--click=Join Field Notes"
  "--wait-text=Subscription confirmed"
  "--click=Home"
  "--wait-text=A content site with zero JavaScript application code"
)

"$vo_binary" ui test "$site" --mode=vm \
  "${actions[@]}" --snapshot="$snapshot" --update
"$vo_binary" ui test "$site" --mode=jit \
  "${actions[@]}" --snapshot="$snapshot"
