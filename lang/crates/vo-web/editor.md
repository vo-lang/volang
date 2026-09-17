# Browser editor snapshots

Compiler builds export `createEditorSource(source, filename, revision)` for an
isolated stdlib source and `createEditorProject(entry, projectRoot, revision,
modRoot?, overlayPath?, overlayText?)` for a captured browser project. They return
an `EditorAnalysis` which the caller must `free()` when replaced or disposed.
The overlay replaces an existing project file; it does not persist the draft.
Project identity, workspace sources, lock validation and dependency readiness use
the same preparation as normal compilation. Queries never install dependencies.

A revision is a caller-owned unsigned 32-bit integer identifying that entire
source view. The snapshot is immutable. A query using another revision returns
undefined. New snapshots do not mutate older snapshots or saved project files.

- `complete` indicates whether syntax and type checking completed without errors.
- `diagnosticsJson` uses the [diagnostic schema](diagnostics.md), including warnings
  from successful analysis. `dependencyError` describes an unavailable or invalid
  dependency; root semantic queries stay unavailable in that case.
- `completionsJson(revision, file, line, character)` returns version 1, revision,
  `positionEncoding: "utf-16"`, a `replace` location and `items`. Each item contains
  `label`, `kind`, `detail`, and an optional `definition` location. Suggestions
  follow lexical visibility, declaration positions, exported names, pointer
  receivers, promoted members and ambiguity rules from the compiler.
- `definitionJson(revision, file, line, character)` returns version 1, revision,
  `positionEncoding: "utf-16"`, and `location`. Builtins without source definitions,
  unknown names and invalid positions return undefined.
- `sourceText(revision, file)` reads only files already captured in this snapshot,
  for example to display an imported definition. It performs no filesystem read.

Locations share the diagnostic schema: exact source path, zero-based UTF-16 line
and character positions, and exclusive original UTF-8 byte ranges. Queries reject
unknown paths, out-of-line positions and split surrogate pairs. CRLF coordinates
exclude the line terminator and preserve the original bytes. Resolve returned
paths exactly; unrelated files can have identical basenames or contents.

Recovered syntax retains unfinished selectors and blocks while emitting errors.
Editor analysis preserves partial root facts in a separate non-executable type;
normal compilation still rejects the same invalid sources. Dependencies must be
fully checked before the root uses their types. Ordinary compilation does not
build this optional query index.

Run analysis in a Worker. Free the old snapshot when its source changes, propagate
request cancellation, and terminate the Worker to interrupt synchronous Wasm
analysis. Recheck source, selection and owner identity before applying a response.
The Studio service demonstrates these lifetimes and keeps its UI workspace in
memory. This API provides semantic primitives; it does not implement an LSP
transport, persistent incremental project cache or automatic dependency editing.
