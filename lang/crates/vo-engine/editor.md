# Native authoring snapshots

`vo_engine::editor::snapshot_path_with_options` analyzes one real package using
the ordinary compiler's project authority, locked graph, workspace selection,
input capture, module-cache read lease and generation validation. The convenience
`snapshot_path` variant inherits the environment's workspace policy.

Pass existing native `.vo` files as `SourceBuffer` values and assign a revision.
Buffers in the active project and selected workspace modules overlay only files
already captured by that graph; unrelated project buffers are ignored. Duplicate
buffer identities are rejected. `SourceBuffer::unavailable` retains a transport
failure for a source until synchronization recovers; only projects containing
that source are rejected, and its disk contents never silently replace the buffer.
No source is written, dependency installed,
native extension prepared or executable emitted. Missing or invalid project
inputs return `CompileError`; source syntax and type errors remain in the
returned diagnostic-bearing `EditorSnapshot` with available recovered facts.

Native files have absolute source paths. Embedded standard-library sources have
relative virtual paths and are read through the snapshot. All queries use exact
captured UTF-8 byte offsets and the snapshot revision. A later file edit never
changes an existing snapshot. A live input change during capture/analysis rejects
that generation. Ordinary compilation continues to require complete checked
projects and allocates no editor index.

The CLI's `lsp` module owns standard LSP synchronization, UTF-16 coordinates,
pull diagnostics, coalesced dependency refreshes, bounded open buffers and one
cached semantic snapshot. Clients without pull support receive versioned push
diagnostics. VS Code cancels outdated pulls and checks the document version
before accepting a report.
Standard-library navigation uses content-addressed `volang-source` documents;
`volang/source` returns only retained captured bytes. The VS Code extension
provides that read-only document viewer and uses the official language client.
There is no second resolver or type checker in either transport.

Focused regressions live in `compile/tests/editor.rs` and `cmd/vo/src/lsp/tests.rs`.
The VS Code extension's `test` directory exercises the actual extension host,
including unsaved imports, warning/error ranges, source views and restart.
