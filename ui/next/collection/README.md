# Keyed collections and windowing

`collection.New([]collection.Item)` copies and validates unique nonempty keys and
labels once. Keep the returned `*collection.Items` in caller state, replacing the
snapshot when data changes. `At`, `Index`, `Next` and `Search` expose value reads,
enabled-item navigation and Unicode lowercase prefix search. Search wraps once;
directional navigation stops at the edge. Locale-specific collation is outside
the current search contract.

`collection.Virtual(scope, key, WindowOptions)` returns a `Window` plus a callback
for the scroll container's `OnViewport`. This primitive is independent of kit
recipes. Render `Start <= index < End` with stable keys inside a spacer of `Total`
CSS pixels, positioning each row at `index * RowHeight`. `WindowFor` exposes the
same calculation without component state. Each helper reserves `key + "/row"`
and `key + "/height"` in its scope.

```vo
window, observe := collection.Virtual(scope, "notes", collection.WindowOptions{
    Count: items.Len(), RowHeight: 40, Height: 240, Overscan: 2,
})
rows := []ui.View{}
for index := window.Start; index < window.End; index++ {
    item := items.At(index)
    rows = append(rows, ui.Element("div", ui.Text(item.Label)).Key(item.Key).
        Attr("style", fmt.Sprint("position:absolute;inset-inline:0;top:",
            index * window.RowHeight, "px;height:", window.RowHeight, "px")))
}
spacer := ui.Element("div", rows...).Attr("style",
    fmt.Sprint("position:relative;height:", window.Total, "px"))
return ui.Element("div", spacer).OnViewport(observe).
    Attr("style", "height:240px;overflow:auto;overflow-anchor:none")
```

`Height` supplies the initial SSR and hidden-container estimate. Native client
height takes over after observation. The supported extent is at most 8,000,000 CSS
pixels, `RowHeight` and `Height` must be positive, and `Overscan` is 0–64 rows.
One extra row covers fractional scrolling. The mounted range is bounded by
`ceil(clientHeight / RowHeight) + 1 + 2 * Overscan`, clipped to the data length.
The helper retains two scalars; scrolling within the same row does not rerender.
Overscroll and collection shrinkage clamp the range to valid data.

Rows leaving the window unmount normally: their effects, tasks and local state
are disposed. Put data that must survive scrolling in the caller's keyed model.
The primitive leaves semantic roles and focus policy to its consumer. Keep a
keyboard-active row mounted as needed; `kit.Listbox` implements this policy for
noninteractive options. Arbitrary interactive rows require an appropriate focus
and keyboard model. Measured vertical rows use `Variable`, described below. Horizontal windowing
and tree/grid interaction are not implemented yet.

`kit.Listbox` supports single or multiple selection. `Selected` reads the owner's
current keys, and `OnSelectionChange` receives a fresh slice. Arrow keys, Home,
End and prefix typing move the active option; Space/Enter selects it, toggling
it in multiple mode. Disabled options are skipped. Selection keys absent from a
filtered snapshot stay in the owner and reappear when the data returns. Single
mode permits at most one key; selected keys must be unique and nonempty.

DOM focus stays on the listbox. The active option remains mounted, including
outside the visible window, and `aria-posinset`/`aria-setsize` describe the full
collection. Custom option content must be noninteractive. This follows the
[APG Listbox pattern](https://www.w3.org/WAI/ARIA/apg/patterns/listbox/).
It does not provide automatic form submission; use the owner's selection in the
form model or explicit hidden fields when native `FormData` is needed.

The interaction example grows from 1,000 to 100,000 records on demand. Collection
creation remains proportional to data size; windowing bounds rendered work and
does not eliminate the application's data or index storage.

## Measured vertical windows

`collection.Variable(scope, key, VariableOptions)` returns a `VariableWindow`
and the scroll container's `OnViewport` callback. Supply an immutable `Items`
snapshot, a positive `Estimate` for unknown row heights, initial `Height`,
`Overscan` (0–64), and a bound scroll `Ref`. The result's `Rows` contain immutable
`Index`, `Key` and `Top` values; `Total` is the spacer height. Replacing or editing
the returned rows does not mutate the private layout cache.

```vo
ref := ui.Ref(scope, "scroll")
window, observe := collection.Variable(scope, "notes", collection.VariableOptions{
    Items: items, Estimate: 80, Height: 300, Overscan: 3, Ref: ref,
    PinnedKey: focusedKey,
})
rows := []ui.View{}
for _, position := range window.Rows {
    itemKey := position.Key
    rows = append(rows, ui.Element("div", renderItem(items.At(position.Index))).
        Key(itemKey).
        Attr("style", fmt.Sprint("position:absolute;inset-inline:0;top:",
            position.Top, "px;display:flow-root;min-block-size:1px")).
        OnSize(func(size ui.Size) { window.Measure(itemKey, size) }))
}
spacer := ui.Element("div", rows...).Attr("style",
    fmt.Sprint("position:relative;height:", window.Total, "px"))
return ui.Element("div", spacer).Ref(ref).OnViewport(observe).
    Attr("style", "height:300px;overflow:auto;overflow-anchor:none;padding:0")
```

Use horizontal writing mode and wrappers that all occupy the scroll content's
full inline size. Keep padding, borders and spacing inside each measured wrapper;
`display:flow-root` contains descendant margins. Row heights must be at least one
CSS pixel. Hidden zero sizes and measurements for removed keys are ignored.
Negative/nonfinite sizes and total extents above 8,000,000 pixels are rejected.
An invalid update does not publish its cached height. Positioning follows native
browser precision; at multi-million-pixel offsets, subpixel geometry may snap
within one CSS pixel.

The first row-size event at a new fractional inline size rebuilds the layout
index once. Subsequent measurements update logarithmic prefix sums. This retains
fractional widths that integer clientWidth cannot represent. A changed Items
snapshot or Estimate also rebuilds once, keeping applicable keyed measurements.
Keep Items stable between data changes; constructing a new snapshot on every
render repeats that work. Only returned rows carry observers, including at most
one extra `PinnedKey`. A 100,000-item model still owns its data and layout index;
windowing bounds rendered work, without removing the model's linear storage.

A matching first visible key anchors measurement, width and collection changes.
Its prior height remains an estimate until remeasurement so a deep inset is not
clamped to the generic Estimate. Actual row shrinkage clamps the inset inside the
row. If the anchor key disappears, native position and extent clamping determine
the next window. Conditional scroll correction preserves a newer native scroll.

Rows outside the range unmount normally. Keep drafts and other persistent data
in the caller's model. `PinnedKey` lets the caller retain one keyboard-active or
focused row; absent keys are ignored. The helper supplies no roles, focus movement
or selection model. Give each row appropriate semantics and full collection
position information. The standalone example retains an editor's identity,
focus, text and selection across scrolling and reordering.

The scope reserves `key`, `key + "/bookmark"` and an internal replacement effect.
A compact existing StringState bookmark preserves the visible key/inset during
source reload; derived measurements reset and are rebuilt. Restoration checks
the saved native position before applying a correction. The ordinary handler,
post-commit and disposal rules also apply to the returned callbacks.

See the [complete application](../examples/variable-list/app.vo) and its
[styles](../examples/variable-list/app.css). Public tests cover three engines,
VM, SSR/client mounting, fractional width changes, 100,000 rows, reorder,
shrinkage and pinned editing. Actual source reload also checks a deep inset,
changed width, later measurement, and retained editor focus/selection. Physical
input and assistive-technology acceptance remain in the platform plan.
