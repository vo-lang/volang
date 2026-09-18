# Volang UI Authoring

This extension supplies compiler-backed completion, definition navigation and
diagnostics, along with `.vo` syntax, editor configuration and UI snippets. It
starts the installed toolchain with `vo lsp --stdio` using the official VS Code
language client. Application projects keep their pure-Vo dependency model.

Use VS Code 1.90 or newer and a Volang toolchain that supports `vo lsp`. Set
`volang.server.path` when `vo` is unavailable on PATH. An empty `volang.workspace`
inherits normal workspace discovery and VOWORK; set it to `off` or an absolute
`vo.work` path to override that selection. Configuration changes restart the
server. **Volang: Restart Language Server** retries after installing a toolchain.
Untrusted workspaces retain syntax and snippets; compiler analysis starts after
workspace trust is granted.

Ctrl+Space requests scope or member completion; F12 follows compiler definitions.
Native sources open as files, and embedded standard-library definitions open as
read-only captured documents. Unsaved buffers in the current module and selected
workspace modules participate in analysis. Closing a buffer restores its disk
source. Save a newly created file once before requesting project analysis.
Errors and successful-check warnings appear in Problems with exact UTF-16 ranges,
including Unicode and mixed line endings. Standard pull diagnostics cancel
outdated requests and check the document version before accepting results.
Dependency changes trigger a coalesced refresh of affected open documents.

Analysis uses existing locked dependencies and never installs them, builds native
extensions or runs the program. Missing dependencies produce an actionable
compiler diagnostic. Completion, definitions and diagnostics reuse one immutable
snapshot. This first implementation performs full
package analysis; incremental checking, automatic imports, rename and reference
search remain outside its advertised capabilities.

For extension development, run `npm ci --ignore-scripts` in this directory and
open it as an extension development project. `npm test` runs a separate VS Code
1.90.2 instance against `target/debug/vo`; build that CLI first. Override
`VO_LSP_BINARY` or `VO_VSCODE_VERSION` for another candidate. Test workspaces and
reports are retained under `target/ui-next/native-authoring-stage`; the user's
editor profile is not used.

The portable toolchain includes `editors/volang-ui-authoring.vsix`. In VS Code,
choose **Extensions: Install from VSIX**, then select that file. Set
`volang.server.path` to the compiler in the same toolchain. Its language client is
bundled, so extension users do not run npm. From a checkout, `npm run package --
/absolute/new/output-directory` produces the same package with retained third-party
licenses. Embedded VSIX timestamps use the fixed ZIP epoch so independent
toolchain packages contain identical extension bytes. Packaging tools require
Node.js 24 in this repository.

The new Web framework uses `github.com/vo-lang/ui/next`. Its snippets have
explicit `vui-web-` prefixes:

| Prefix | Use |
| --- | --- |
| `vui-web-app` | A complete single-file application using `host.Run` |
| `vui-web-component` | A component with a package-level type and typed title input |
| `vui-web-state` | Named integer state inside a component render |
| `vui-web-resource` | Text loading with pending, timeout, error and retry behavior |

Component/state snippets expect `ui/next` imported as `ui`; the resource snippet
also needs `ui/next/web`. Definitions belong at package scope. State and resource
declarations belong inside a component's render function. Choose unique local
keys when inserting several declarations.

In a project created with `vo ui create`, edit its shared `app/app.vo` and retain
`View(initial string)`; the complete application snippet is for a separate
single-file entry. See [First steps](../../next/guides/first-steps.md),
[state](../../next/guides/state.md) and [requests](../../next/guides/lifecycle.md).
