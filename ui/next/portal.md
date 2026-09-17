# Root-owned Portal placement

`View.Portal(target)` places an HTML element inside another element in the same
UI root. It preserves the source component's state, context, effects, tasks and
keyed identity. Application code uses ordinary `ElementRef` values; native DOM
handles stay in the renderer.

```vo
destination := ui.Ref(scope, "overlays")
return ui.Element("main",
    ui.Element("section",
        ui.Element("aside", ui.Text("An idea with room to grow.")).Portal(destination),
    ).Class("clipped-content"),
    ui.Element("div").Ref(destination).Class("overlay-container"),
)
```

The target may appear later in the same render. A nil or currently unbound target
keeps the element at its original inline position. Removing the target restores
the retained element there; a later binding moves it again. Removing the Portal
decoration also restores inline placement without replacing the element or its
component state. Removing its logical owner disposes the whole owned subtree,
including any content physically placed elsewhere.

Only normal HTML elements can be sources. Targets must be HTML elements that
accept children; text, fragments, widgets, SVG, void elements, textarea, style and title targets
are rejected. Refs must belong to this root. A target cannot be the source or one
of its logical descendants, and the complete physical parent graph must remain
acyclic. A root supports up to 64 Portal declarations. The browser validates the
whole batch before any DOM mutation, including cycles introduced by later moves.

Native events bubble and capture through **destination DOM ancestors**. CSS
inheritance, scope selectors, accessibility order and native form participation
also follow that physical tree. Use explicit `form="form-id"` when a relocated
control should retain a particular native form association. Component context
continues to come from its source scopes. No inherited styles or event handlers
are copied between trees.

A small comment retains each source position. Managed children at the destination
come first; Portals sharing it appear in creation order. Reordering source siblings
moves their source positions while retaining that destination order. Unchanged
placements do not move DOM nodes. Input identity, value, focus and selection use
the existing commit boundary. Widget ownership and cancellation remain unchanged.

Server HTML renders the element inline. Before activation it remains readable and
its native inputs remain usable. Activation adopts those exact nodes and early
edits, then applies placement. Initial layout can therefore change on activation;
keep inline content useful and avoid relying on a client-only overlay for the
meaning of server-rendered text.

Portal controls placement. Compose `Modal`/kit Dialog or `Popover` for presentation,
focus, keyboard dismissal and positioning. The [pure Vo example](examples/portal/app/app.vo)
moves an editable note between two destinations and a native dialog while keeping
the notebook context and component state. Alt + Left/Right moves the focused note
between the ordinary destinations. No selectors, cross-root placement or direct
document-body mounting are exposed by this API.

Reproduce the native/guest checks through the regular build and runtime suite.
`node eng/ui-next/portal-dom-contracts.mjs` checks real DOM placement and ownership
on all three engines; `node eng/ui-next/portal-project-contracts.mjs` creates and
builds an ordinary project, exercises VM, server HTML input adoption and a real
source reload, and preserves its distribution and browser evidence under
`target/ui-next/portal-project`. Browser tests do not establish real OS IME or
assistive-technology conformance.
