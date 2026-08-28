#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
vo_binary="${VO_UI_BIN:-$repo_root/target/debug/vo}"
gallery="$repo_root/ui/showcases/component-gallery"
snapshot="$(mktemp "${TMPDIR:-/tmp}/volang-uikit-gallery.XXXXXX")"
trap 'rm -f "$snapshot"' EXIT

actions=(
  "--input=Release notes=AOT contract text"
  "--click=+  Release targets"
  "--wait-text=The same UIKit tree renders on Web and desktop."
  "--click=Open drawer"
  "--wait-text=Release inspector"
  "--key=Release inspector=Escape"
  "--wait-absent-text=Release inspector"
  "--click=Show toast"
  "--wait-text=Release ready"
  "--click=Dismiss"
  "--wait-absent-text=Release ready"
  "--click=Open commands"
  "--input=Command palette query=publish"
  "--key=Command palette query=Enter"
  "--wait-text=Command executed: release.publish"
)

"$vo_binary" ui test "$gallery" --mode=vm \
  "${actions[@]}" --snapshot="$snapshot" --update
"$vo_binary" ui test "$gallery" --mode=jit \
  "${actions[@]}" --snapshot="$snapshot"
