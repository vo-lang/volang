# Application and data platform

The E4 public surface is split by ownership so an application can replace one
policy without replacing the renderer or UIKit.

- `ui/task` owns component-scoped goroutines, cancellation generations,
  bounded completion mailboxes, streams, and post-commit effects.
- `ui/resource` adds cache joining, latest/FIFO admission, deterministic retry,
  optimistic scalar mutation, page/cursor models, and typed query adapters.
- `ui/persistence` owns versioned records, explicit migrations, injected
  backends, and bounded serializable undo/redo history.
- `ui/navigation` owns named path patterns, typed parameters and query values,
  lazy routes, guards, redirects, history actions, and focus/scroll restoration.
- `ui/forms` owns scoped field state, cross-field validation, cancellation-aware
  submission, reset, error focus, and accessible field projection.
- `ui/commands` gives shortcuts, menus, toolbars, context surfaces and palettes
  one validated identity and enablement source.
- `ui/platform` declares support, authority, permission, quota, lifetime,
  sandbox, ABI and packaging policy for network, storage, files, credentials,
  processes and plugins.
- `ui/kit/data` provides sortable grids, virtual provider tables and trees, and
  accessible dashboard charts. Provider APIs construct only the visible range.

Worker goroutines publish typed mailbox values and request invalidation. The UI
Island applies mounted state during its next owned turn. Cache and platform
models may use mutexes for cross-goroutine metadata; renderer nodes and reactive
state remain under the single UI writer.

The data-application showcase composes these packages without a JavaScript
application graph. Its 16-step VM and JIT business flows produce a byte-exact
snapshot, the logical 100,000-row table stays bounded, and the same source runs
as a real-browser Web Wasm AOT application and a packaged Native AOT window on
Linux, macOS, and Windows. The flow covers selection, filtering, sorting,
pagination, optimistic commit, offline resources, a modal Ctrl+K command
palette, typed routing, accessible validation, async submission, and return
navigation.
