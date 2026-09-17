# State that stays close.

A component scope owns its state and lifetime. Local names identify state within
that scope; a component's definition and sibling key identify the instance.
Choose those identities from the application's meaning so updates remain
predictable when the page changes.

## Read and update

Use `ui.Int`, `ui.String` and `ui.Bool` for scalar state. Declare a state during
render, read it with `Get`, then call `Set` from an event handler or post-commit
work. Reading subscribes the current computation. Unrelated components keep
their existing views when a value changes.

```vo
count := ui.Int(scope, "count", 0)
button := kit.Button(kit.ButtonProps{
    OnPress: func() { count.Set(count.Get() + 1) },
}, ui.Text(fmt.Sprint(count.Get())))
```

Render describes a view. Updating state during render is rejected. Keep state
names stable and use distinct names for different declarations. A name cannot
change from one state type to another on a later render.

## Preserve an instance through movement

Give movable siblings a stable key:

```vo
for _, item := range items {
    children = append(children, Row(item).Key(item.ID))
}
```

The same definition and key preserve the component scope and its retained DOM
through a reorder. Removing it disposes its effects and tasks. Adding an instance
later creates fresh local state. Avoid an array position as the identity when
insertion, removal or sorting can change which item occupies that position.

Keys are local to the parent. They do not find an instance in another root or
turn two separate component declarations into shared state. Native focus and
selection follow retained elements; give interactive lists stable identities.

## Use snapshots for structured values

`ui.State` stores an application-defined value. Its initializer runs for the new
state, and `Update` publishes the replacement. Copy mutable collections before
changing them:

```vo
items := ui.State(scope, "items", func() any { return []string{} })
// In an event handler:
items.Update(func(previous any) any {
    next := append([]string{}, previous.([]string)...)
    return append(next, "A new thought")
})
```

Editing a map or slice returned by `Get` in place does not notify readers. A
snapshot discipline keeps previous values meaningful and makes updates easier
to reason about. Store temporary task handles separately from the reactive
status that the interface displays.

## Derive and share

`ui.Derived` caches a computation and follows the state it actually reads. When
an input changes, the result becomes pending until it is needed again. Keep the
computation pure; start external work through effects or requests.

```vo
doubled := ui.Derived(scope, "doubled", func() any {
    return count.Get() * 2
})
label := ui.Text(fmt.Sprint(doubled.Get().(int)))
```

Define a context once with `ui.NewContext`. An ancestor supplies a value using
`ui.Provide`; descendants read the nearest provider with `ui.UseContext`. Context
ownership remains local to the root, including independent server requests.
Prefer local state until several descendants need the same owner.

## Pass ordinary typed inputs

Pass strings, structs, callbacks and views as ordinary function arguments. The
render closure reads those typed inputs. A parent render supplies the new inputs
while the child's retained scope keeps its own state.

```vo
func NamedCounter(name string) ui.View {
    return ui.Component(counterType, func(scope *ui.Scope) ui.View {
        ui.InspectProp(scope, "name", name)
        count := ui.Int(scope, "count", 0)
        return ui.Element("p", ui.Text(name + ": " + fmt.Sprint(count.Get())))
    })
}
```

`InspectProp` records an explicitly declared scalar preview while an inspector is
attached. It adds no dependency and invokes no application formatting method.
Declare useful scalar fields of a structured input individually. Declarations
from a previous render disappear when the new render omits them.

## Find why an update happened

Capture the development inspector after a visible update. It distinguishes local
state invalidation, parent rendering, mount and fallback. Derived dependencies
show the source state behind a computed value. Props describe the latest observed
render attempt; a dirty state can precede the next render. Inspection does not
rerun a component just to discover its earlier inputs.

See [lifecycle and requests](lifecycle.md) for work owned by the same scope.

## Selective updates and stable inputs

Ordinary components refresh their captures whenever their parent renders. Add
`.Memo(inputs...)` to a component when all ordinary captures can be described by
immutable comparable values. Include every prop, child description dependency and
callback dependency. A pointer is suitable for a stable reactive handle; a mutable
object also needs a version input when its changes do not publish reactive writes.
Unlisted changing captures would leave rendered output and callbacks stale.

```vo
return ui.Component(rowType, func(scope *ui.Scope) ui.View {
    count := ui.Int(scope, "count", 0)
    return ui.Text(fmt.Sprint(id, ": ", count.Get()))
}).Memo(id).Key(fmt.Sprint(id))
```

State and context reads continue to invalidate consumers independently. Adding or
removing a context override reaches consumers inside skipped subtrees. Cleanup,
error boundaries and source reload retain their normal ownership rules.

`ui.DerivedMemo(scope, key, compute, inputs...)` uses the same capture contract for
a lazy derived value. It keeps its cache through unrelated owner renders and still
tracks the reactive state read by `compute`. `Derived` keeps its existing behavior
of refreshing on every declaration when captures are not explicitly described.

`store.Select(func(snapshot any) any { ... })` subscribes to a comparable projection
of immutable state. `SelectEqual(project, equal)` supports collections with an
explicit equality function. These callbacks are pure and depend only on their
arguments; equality must cover the whole selected result. `Get` subscribes to all
writes. `Peek` reads without subscribing and is for code that deliberately does
not derive render output from that read. Disposal removes every selector
subscription, and selected reads inside a derived value follow its lifetime.
