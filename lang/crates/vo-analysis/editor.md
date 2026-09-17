# Semantic snapshots for authoring

`editor::analyze` consumes a captured FileSet, Resolver, explicit PackageIdentity
and caller-owned revision, returning an immutable EditorSnapshot. Files and module
metadata should come from the same view. VFS clients can prepare a captured
context with `prepare_file_set_with_package_identity`, then consume it with
`editor(revision)` or `check()`; these paths share identity and module validation.

A snapshot owns recovered root syntax, available TypeInfo, source bytes and object
arenas. Successfully checked dependencies stay paired with their syntax. Invalid
or unavailable dependencies retain diagnostics and disable root semantic queries.
Admission failures return AnalysisError. Partial facts have no conversion into
Project and cannot be passed to code generation. Normal compilation retains strict
syntax/type rejection and does not allocate a query index.

Definition and completion methods take the snapshot revision, exact source path
and a UTF-8 byte offset. SourceRange carries that revision and an exclusive byte
range. Root paths are joined to the captured FileSet root, and dependency paths
come from their resolver filesystem. Native project files therefore remain
distinct from embedded standard-library files with the same relative name.
Queries reject stale revisions, ambiguous paths, unknown files and positions
inside a UTF-8 scalar. The Web adapter converts UTF-16 positions at its boundary.

The index uses the canonical syntax visitor with each package's defs, uses,
implicit import bindings and selections. Name completion uses position-aware scope
lookup. Member enumeration discovers names, then the existing selector lookup
resolves visibility, pointer receivers, embedding and ambiguity. No second type
resolver or execution frontend is introduced. Builtins can have completion entries
without source definitions.

Syntax recovery covers missing selector names and unfinished blocks. Recovery is
best effort; a discarded invalid declaration has no invented semantic facts.
Lexical completions exclude comments and literals. Qualified type completions
exclude values. Source-based queries intentionally make no claim about incremental
checking, automatic imports or rename/reference search. The native engine's
`editor::snapshot_path_with_options` owns real-project capture, workspace selection
and unsaved buffers; `vo lsp --stdio` adapts those snapshots to editor requests.
