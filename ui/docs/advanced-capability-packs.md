# Advanced capability packs

E6 extends Volang UI through opt-in packages. Applications pay for a package
only when it appears in the compiled import graph, and every package keeps its
portable state machine in Volang source. Renderer adapters consume bounded,
versioned projections.

## Package boundaries

| Package | Public responsibility | Host boundary |
| --- | --- | --- |
| `ui/graphics` | vector commands, paths, transforms, canvas | VGC1 replay |
| `ui/assets` | typed sources, provenance, resolution and cache accounting | resolver |
| `ui/animation` | timelines, presence and reduced motion | existing motion clock |
| `ui/chart` | bounded series and semantic summaries | graphics canvas |
| `ui/media` | playback, tracks, capture, permission and teardown state | media and capture hosts |
| `ui/document` | Unicode text, selection, history, line index, rich spans and Markdown | none |
| `ui/language` | snapshot-versioned diagnostics, completion, hover, symbols and formatting | language service |
| `ui/editor` | document-backed editing, viewport, tokens, folds and language results | text input/editor projection |
| `ui/workspace` | panels, tabs, splits, stable IDs and restoration snapshot | window layout policy |

`ui`, `ui/kit`, and application packages do not depend on these packs. A media
application can import media without linking the editor; Studio can import the
document and editor without linking capture.

## Graphics and assets

`graphics.Program` validates finite coordinates, command count, path segments,
stroke width, payload size and encoded byte size before producing VGC1. VGC1
is a deterministic command stream shared by Web Canvas, native paint, SSR
metadata and replay tests. Both renderers decode the stream again at their
trust boundary and reject malformed commands.

Assets carry kind, media type, byte count, cache policy and optional SHA-256
integrity. Application-relative and HTTPS sources are accepted; executable and
local-file schemes are rejected. The registry accounts replacement before
mutation, so quota failures preserve the prior entry and byte total.

Charts retain the source series and emit a semantic table summary alongside
the visual canvas. Pixel output is never the only representation of data.

## Animation

Timelines are sorted, finite and bounded. `UseTarget` and `UsePresence` reuse
the component-owned motion generation: starting a newer animation prevents an
older goroutine from publishing. Reduced-motion environments commit the final
target in one UI turn while preserving the same component structure.

## Media and capture

`media.Session` owns a locked playback state machine. Host calls happen outside
the lock, carry a context, and publish only when the captured session generation
is still current. Closing a session advances the generation before host
teardown, so an open or control call that finishes late cannot revive it.

Capture is expressed through a narrow permission/start/stop host interface.
Requests and returned streams are validated for kind, dimensions, frame rate,
device identity and track count. Platform support is queried explicitly; an
unavailable capability remains a typed result.

## Document, language and editor

Document offsets use Unicode scalar indices. UTF-16 conversion is available at
the browser, IME and language-server boundary. Edits update only the affected
line-index interval, then atomically publish text, selection and version.
Undo/redo stores bounded inverse changes. Find results, history, rich spans,
Markdown blocks and total runes all have limits.

Language services receive immutable URI/language/version snapshots and a
cancellable context. Results are validated and normalized. The editor checks
the document version immediately before publishing, which discards a stale
goroutine completion without disturbing newer diagnostics or completions.

Editable documents up to one million runes use the native controlled textarea
path, including browser IME and selection behavior. Larger documents expose a
bounded overscanned line projection and keep the full document in the shared
model. Two editor models may safely share one document while retaining
independent viewport, focus, command and window scopes.

## Workspace and goroutine ownership

Workspace nodes use stable IDs and a bounded recursive tree. Split ratios, tab
indices, depth, node count and panel references are validated before render or
snapshot. A restored tree therefore cannot manufacture missing panels or
unbounded recursion.

Advanced packs follow the framework's structured-concurrency rule:

1. a component, session or service call owns the context and generation;
2. background work may calculate, decode or call a host without holding UI
   locks;
3. completion validates cancellation, generation and document/session version;
4. accepted results enter the single UI writer in one bounded commit;
5. disposal or close advances ownership before releasing external resources.

This makes goroutines useful for language analysis, asset resolution, media
control and document work while keeping renderer state deterministic.

## Permanent evidence

- `ui/tests/advanced-packs` runs document, language, graphics, assets, chart,
  animation, media and workspace behavior through VM, JIT, Core Wasm AOT and
  Native AOT.
- `ui/tests/multi-window-editor` shares a document across independent editor
  and desktop window scopes and verifies close isolation.
- `ui/showcases/media-application` and `ui/showcases/studio-workbench` use only
  public packages.
- `vo-ui-paint` and the Web DOM adapter replay the same bounded VGC1 stream.
