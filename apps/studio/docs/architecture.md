# Studio architecture

The mounted UI Island is the product's single writer. Event handlers publish
small state transitions. Potentially slow work runs in generation-scoped
goroutines and returns typed messages through the capability boundary.

Each project session supervises these independent workers:

- a latest-wins analyzer for the active document;
- a run stream with bounded output and explicit cancellation;
- an isolated preview child session;
- filesystem and remote synchronization workers;
- credential access owned by the platform host.

The editor owns a bounded map of live documents. Switching tabs preserves each
document's selection and undo history. Save and compile operations capture
immutable `(text, version)` snapshots, so a late completion cannot clear a
newer dirty generation. Workspace search cancels its predecessor, scans a
bounded immutable file list, and publishes only the latest generation.

The starter catalog lives in pure Volang and sends one bounded project-file
snapshot through the project capability. Hosts validate normalized paths,
duplicate names, per-file size, aggregate size, and manifest ownership before
publishing `vo.mod` last. Opening a starter again reuses the user's existing
project, preserving edits. Project rename, deletion, and non-destructive
forget operations remain host-owned transactions; the product supplies
accessible dialogs and updates the active session only after host publication
succeeds.

Opening another project or starting another run advances its generation and
cancels the previous context. Every completion checks both generation and
document version before publishing. Preview applications mount in a separate
Island so guest rendering, panics, and event loops cannot mutate Studio's tree.
The Web host projects that child Island into the preview panel. The Native host
serializes the verified generation and launches it in a supervised preview
window; replacement, project switching, and host shutdown terminate the prior
child and remove its temporary artifact.

The `protocol` package contains capability interfaces instead of one broad
backend object. Native and Web launchers may implement them differently while
the product, recovery behavior, and tests remain shared.

On Web, the page is the sole durable OPFS writer. Compiler and run Workers use
memory-only VFS instances populated from bounded snapshots. A release packages
the reachable official workspace modules with sizes and SHA-256 digests; the
host verifies them, writes a local `vo.work`, and asks the compiler runtime to
derive a deterministic workspace-origin `vo.lock` without registry authority.

Native AOT links the compiled Studio image against the dedicated
`vo-studio-aot-runtime-native` static runtime. It reuses the capability host
from `vo-studio-native` and installs the same versioned invocation handler as
the VM/JIT launcher before entering the desktop event loop, preserving host
capabilities across development and release backends. The packaged executable
also recognizes the bounded `--studio-preview-artifact` child mode, verifies
the serialized module, and starts only the isolated preview shell. Temporary
artifact paths are canonicalized after the parent writes them, while arbitrary
external link traversal remains denied by the compiler input boundary.

Native UI revisions atomically reconcile their pending input queue. An input
captured against a listener that was replaced by an asynchronous Studio
revision is discarded before renderer polling; the reserved invalidation event
and every listener still matching the committed tree retain their original
monotonic sequence. The renderer continues to reject forged epochs, replayed
sequences, missing targets, and mismatched listener identities.

`volang.studio.host.v1` uses UTF-8 JSON payloads with lowerCamel field names.
Every request is bounded by the generic UI system limits. Responses may evolve
by adding fields; changing or removing a field requires a new protocol version.
