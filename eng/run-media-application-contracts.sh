#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
vo_binary="${VO_UI_BIN:-$repo_root/target/debug/vo}"
application="$repo_root/ui/showcases/media-application"
snapshot="$(mktemp "${TMPDIR:-/tmp}/volang-media-application.XXXXXX")"
trap 'rm -f "$snapshot"' EXIT

actions=(
  "--click=Play media"
  "--wait-text=Playback is running"
  "--click=Seek to 30 seconds"
  "--wait-text=Playback position is 30 seconds"
  "--click=Set volume to 50 percent"
  "--wait-text=Playback volume is 50 percent"
  "--click=Set speed to 1.5 times"
  "--wait-text=Playback speed is 1.5 times"
  "--toggle=Show visualizations=false"
  "--wait-absent-text=Audio spectrum visualization"
  "--click=Capture"
  "--wait-text=Camera capture with explicit recovery"
  "--click=Request camera permission"
  "--wait-text=Camera permission granted"
  "--click=Start camera capture"
  "--wait-text=Camera is capturing showcase-camera"
  "--click=Stop camera capture"
  "--wait-text=Camera capture stopped"
  "--click=Test denied permission"
  "--wait-text=Camera failed: media capture permission denied"
  "--click=Recover camera session"
  "--wait-text=Camera session recovered; permission is ready to request"
  "--click=Offline"
  "--wait-text=Media controls remain available offline"
)

"$vo_binary" ui test "$application" --mode=vm \
  "${actions[@]}" --snapshot="$snapshot" --update
"$vo_binary" ui test "$application" --mode=jit \
  "${actions[@]}" --snapshot="$snapshot"
