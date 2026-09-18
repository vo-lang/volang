# Framework design

The public authoring surface is ordinary Vo: typed props, component functions and
views, root-owned state, effects, tasks and data. The same component implementation
renders in a browser, on the server and inside a desktop WebView. The
[authoring guide](guides/first-steps.md), [API reference](README.md) and
[component library](kit/README.md) describe the supported APIs.

## Ownership

| Owner | Responsibility |
| --- | --- |
| `view.vo`, `state.vo`, `root.vo` | Component definitions, instance state, subscriptions and scheduling |
| `reconcile.vo`, `effect.vo` | Stable identity, tree differences and post-commit lifecycle |
| `boundary.vo` | Render failure boundaries and instance diagnostics |
| `derived.vo`, `context.vo` | Cached derivations and ancestor-scoped values |
| Tasks, resources, watches and `data` | Cancellation, result ownership, caching and initial data |
| `html.vo`, `document` | Deterministic HTML and prepared semantic documents |
| `wire.schema.json` | Bounded guest/host identities, messages and generated codecs |
| `host/host.vo`, `vo-ui-bridge` | VM exchange, commit acknowledgement and input delivery |
| `vo-web/js/ui_next` | DOM, hydration, input/IME, browser services and widgets |
| `vo-ui-native`, `vo-ui-webview`, `vo-ui-desktop-runtime` | Native execution and system WebView delivery |
| `eng/ui-next` | Project tooling, packaging and executable acceptance |
| `apps/studio/next` | Gallery, Docs and Playground |

The guest owns component state and reconciliation. The document thread owns DOM,
native input, focus and browser services. Web applications can run the guest in a
Worker or on the page; both use the same serialized exchange. Studio's production
entry runs its guest in a Worker. Desktop delivery uses native VM/JIT or Native
AOT with the same browser renderer in the system WebView.

## Updates and lifecycle

1. Native events enter a bounded queue with root-local sequence numbers. A live
   target invokes its current handler; stale or unloaded targets cannot receive
   events. Delivery waits until native propagation and default actions complete.
2. State writes in one input batch coalesce. Dynamic reads replace subscriptions;
   dirty components run in mount order with parents before children. Rendering
   cannot write state.
3. A flush produces a revisioned batch. The next exchange waits for its exact
   acknowledgement, including an input batch that makes no DOM changes.
4. The host validates references, topology, attributes and operations before
   applying the batch. Commit acknowledgement permits cleanup and new effect
   setup. State changes from effects enter the following update.
5. Unmount invalidates scopes and pending results before resource cleanup. Roots
   release event listeners, ports, requests, watches, widgets and owned workers.

Sibling keys are parent-local. Moving a retained range preserves its DOM objects.
Controlled input reconciliation considers the latest native input sequence, so an
older guest update cannot overwrite newer editing. Composition, selection and
focus have explicit host ownership and browser regressions.

Effects start after commit. Replacing a request, cancelling a task or unmounting
its owner removes the result handler and cancels host work. Late completions are
discarded even when the provider ignores cancellation. Error boundaries recover
render/reconciliation failures; event, service and cleanup failures retain their
own reporting paths. Batch validation does not promise rollback of arbitrary
browser or third-party widget side effects.

## Rendering and application services

SSR flushes and serializes without running browser effects. Hydration verifies
server markers and adopts existing nodes. Versioned initial data and deterministic
inputs keep server and client output aligned; early native editing is retained.
Each server request uses its own execution state.

Navigation preserves ordinary links and handles client transitions through a
root-owned adapter. Semantic documents validate bounded content, cache prepared
views and expose a heading outline. Applications can attach heading links and code
controls without giving the document renderer clipboard or storage ownership.

Widgets own explicit subtrees with setup, update and disposal. Optional editor,
plot and canvas packages load only when used. Browser services remain adapters;
component state and application decisions stay in Vo.

## Studio delivery

Studio builds both a request-rendered server distribution and a static site.
Gallery, Docs and Playground have independent page artifacts. Maintained Markdown
is generated into digest-checked chapter assets; component APIs are searchable.
The source editor, online compiler and preview are loaded on demand. Drafts use
IndexedDB. Program execution uses a fresh, cancellable Worker and separate loading,
compilation and execution deadlines. UI previews have their own document and
Worker lifecycle.

The static server supports precompressed assets, explicit route metadata and
rendered 404 pages. The asset-cache retirement worker only removes its named asset
cache and unregisters itself; it never accesses user storage.

## Verification and limits

[Certification declarations](../certification.toml) select the Web and native
platform tasks defined in `eng/ci.toml`. Local declaration checks do not certify a
product: release evidence must match the source commit, configuration and artifacts.
Browser contracts cover input, lifecycle, services, hydration, distribution and
Studio flows. Desktop contracts cover the current native execution adapters.

Synthetic browser checks do not replace real IME, assistive-technology and mobile
device testing. Performance claims require measured builds and stated conditions;
see [the performance report](performance.md). New runtime complexity should be
justified by useful application measurements and maintained regression coverage.
