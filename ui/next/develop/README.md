# Development reload

`develop.Run(view)` and `develop.RunWithData(render)` combine optional inspection
and state reload. Use them from a separate development entry. Production keeps
`host.Run`/`RunWithData`. New starters and Studio provide both entry points; the
project builder aliases `@volang/ui-next` to `development-mount` only in development.

After a successful Vo rebuild, the browser loads a replacement runtime and obtains
a checkpoint from the old root after queued input and effects have settled. It
validates the replacement's initial DOM batch before closing the old host, creates
the new root and establishes its owned subscriptions. Failed loading, capture,
envelope validation or initial rendering preserves the working page and displays a diagnostic.
Vo errors caught by an application error boundary follow that boundary's policy.
Failures after activation use the ordinary application failure path.

CSS updates retain the existing DOM. Vo updates rebuild the root while preserving
compatible data, native input and focus. JavaScript, HTML and configuration changes
restart the page. Active composition defers the update. Browser reload requests
are serialized and coalesced; staging has a 15-second deadline, captures have a
five-second deadline, and closing the application cancels staging. A development
notice reports the number of states retained and reset.

## Identity and data

`Int`, `String` and `Bool` states are copied automatically. Identity uses the
component definition name, tree structure, sibling key or unkeyed position, and
state key/type. Give definitions unique qualified names such as `example.App`.
Names remain diagnostic metadata during ordinary rendering; reload additionally
requires that different definitions in one root have different names. Ambiguous
names reject a capture. Keyed siblings retain data when reordered. Renamed keys,
changed paths/types and removed declarations reset their affected states.

Snapshots are consumed by the initial render. Mounting a previously removed
conditional child later initializes fresh state. Derived values are recomputed;
tasks, subscriptions, refs and lifecycle handles receive new owners. The optional
checkpoint has a 4 MiB encoded limit, 2,048 states, 20,000 visited nodes and an
8,192-byte identity-path limit. Capture performs no derived computations or
tracked reads and rejects mutation from a codec.

Application `Store` values can implement `ui.ReloadValue`:

```vo
ReloadSchema() string
SaveReload() (string, error)
RestoreReload(string) (any, error)
```

The methods are pure. A schema is a nonempty versioned identifier of at most
256 bytes. `SaveReload` returns a data representation; `RestoreReload` runs on the
fresh initializer value and must return a compatible value with fresh lifecycle
metadata. Change the schema when the representation becomes incompatible. A
schema mismatch or decoder error initializes fresh state; decoder diagnostics
appear in the report. Stores without this contract reset. Keep application
services, closures and task handles out of serialized data.

Keep execution-only flags with their task or widget in an opaque Store. For
example, `{task, output}` or `{active, busy, source, status}` should start fresh
when their owner is replaced. A separately persisted `busy` scalar can otherwise
claim that cancelled work is still running. Studio uses this grouping for console
execution and UI previews while its editor draft remains restorable. An explicit
new Run starts new work after reload.

`forms.Use` implements this contract. It retains values and the saved baseline,
including repeated values and edits made during a save. Current source defines
the fields and reset defaults: new fields receive new defaults and removed fields
are dropped. Validation/touched/message/pending state starts fresh. An old
submission is cancelled locally and is never replayed by restoration.

Native uncontrolled text, checkbox and select values can survive with unique,
stable explicit IDs and matching control types. Text selection, focused IDs and
page/identified-container scroll are restored. Application-controlled values follow the typed guest
checkpoint. File selections, inputs without unambiguous IDs and private widget/
shadow DOM state currently reset; widgets remount with fresh ownership. Full real
IME, device and assistive-technology certification remains open.

## Root-bound services

Pass a `services(container)` factory to `mountUi` when an adapter captures a root
element. The factory creates the navigation/widget/service declarations for each
replacement's actual container. Providers install work lazily when called and
release it when their signal aborts. Plain service objects remain supported for
providers independent of a particular container. An asynchronous `initialData`
provider can read the current URL when a replacement starts.

For native or custom development adapters, `Root.SaveReload()` captures an idle
committed root. `freshRoot.RestoreReload(encoded)` validates the envelope before
mounting and returns a report finalized by the first `Flush`. Neither call starts
effects or transfers ownership. The normal commit/cancel/cleanup protocol still
applies. `host.RunRootWithIdle` provides a committed capture point for the optional
development transport.

Inspection and reload subscriptions attach through `Root.OnMount` and the root's
task methods. They add no synthetic component wrappers, so server HTML and the
development client retain the same node identities and early input. Closing the
inspection session cancels its subscription even while the application stays
mounted. Checkpoints made by older experimental builds containing inspection or
reload wrapper components may reset state when upgrading to this structure; the
normal restoration report identifies those resets.

Run `node eng/ui-next/reload-contracts.mjs` for actual VM/Wasm VM replacement
on Chromium, Firefox and WebKit. The fixture covers keyed state, form drafts,
native edits, focus/caret, failed replacement, composition deferral, incompatible
state and old result/subscription isolation. These are development contracts,
separate from the inspection panel's truncated display snapshots.
