#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
vo_binary="${VO_UI_BIN:-$repo_root/target/debug/vo}"
application="$repo_root/ui/showcases/data-application"
snapshot="$(mktemp "${TMPDIR:-/tmp}/volang-data-application.XXXXXX")"
trap 'rm -f "$snapshot"' EXIT

actions=(
  "--click=Member 0"
  "--wait-text=member-0"
  "--click=Archive selected"
  "--wait-text=Archiving member-0"
  "--click=Commit optimistic change"
  "--wait-text=Archived member-0"
  "--click=2"
  "--wait-text=Member 1000"
  "--input=Filter members=active"
  "--wait-text=250"
  "--click=Member"
  "--wait-text=Member 249"
  "--toggle=Work offline=true"
  "--wait-text=offline cache"
  "--click=Open commands"
  "--input=Command palette query=commit"
  "--key=Command palette query=Enter"
  "--wait-absent-text=Command palette"
  "--click=Settings"
  "--wait-text=Back to dashboard"
  "--input=Workspace name="
  "--click=Save settings"
  "--wait-text=Workspace name is required"
  "--input=Workspace name=Operations"
  "--click=Save settings"
  "--click=Back to dashboard"
  "--wait-text=Operations dashboard"
)

"$vo_binary" ui test "$application" --mode=vm \
  "${actions[@]}" --snapshot="$snapshot" --update
"$vo_binary" ui test "$application" --mode=jit \
  "${actions[@]}" --snapshot="$snapshot"
