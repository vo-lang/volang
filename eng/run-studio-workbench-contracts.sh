#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
vo_binary="${VO_UI_BIN:-$repo_root/target/debug/vo}"
application="$repo_root/ui/showcases/studio-workbench"
snapshot="$(mktemp "${TMPDIR:-/tmp}/volang-studio-workbench.XXXXXX")"
trap 'rm -f "$snapshot"' EXIT

edited_module=$'module example.com/edited-workbench\n\nvo 0.1\n\nrequire github.com/vo-lang/ui v1.0.0\n'
edited_source=$'package main\n\nimport "github.com/vo-lang/ui"\nimport "github.com/vo-lang/ui/kit"\n\nfunc App() ui.View {\n\treturn kit.Page(kit.Title("Edited in Volang Studio"))\n}\n\nfunc main() {\n\tif err := ui.Mount(App); err != nil { panic(err.Error()) }\n}\n'

actions=(
  "--wait-text=Workspace ready"
  "--wait-text=✓ VM, JIT, Wasm AOT, and Native AOT targets discovered"
  "--click=vo.mod"
  "--wait-text=require github.com/vo-lang/ui v1.0.0"
  "--input=vo.mod code editor=$edited_module"
  "--wait-text=Unsaved changes in vo.mod at version 1"
  "--click=Save active file"
  "--wait-text=Saved vo.mod at version 1"
  "--click=main.vo"
  "--input=main.vo code editor=$edited_source"
  "--wait-text=Unsaved changes in main.vo at version 1"
  "--click=Undo edit"
  "--wait-text=Undid the last edit in main.vo"
  "--click=Redo edit"
  "--wait-text=Redid the last edit in main.vo"
  "--click=Wasm AOT release"
  "--wait-text=Target changed to Wasm AOT release"
  "--click=Run project"
  "--wait-text=Run 1 completed with Wasm AOT release"
  "--wait-text=Preview synchronized after run 1"
  "--click=Show Console"
  "--wait-text=✓ Run 1 completed with Wasm AOT release"
  "--click=Show Documentation"
  "--wait-text=Build once, run everywhere"
  "--click=Open command palette"
  "--input=Command palette query=native"
  "--click=Use Native AOT Release"
  "--wait-text=Target changed to Native AOT release"
  "--click=Reset Workspace Layout"
  "--wait-text=Workspace layout restored"
  "--click=README.md"
  "--wait-text=The same component tree runs in VM/JIT development, Wasm AOT, and Native AOT releases."
  "--click=Toggle Explorer"
  "--wait-text=Explorer layout updated"
)

"$vo_binary" ui test "$application" --mode=vm \
  "${actions[@]}" --snapshot="$snapshot" --update
"$vo_binary" ui test "$application" --mode=jit \
  "${actions[@]}" --snapshot="$snapshot"
