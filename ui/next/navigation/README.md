# Navigation and nested routes

Optional pure-Vo route matching and scoped browser history. Applications may use
the history layer by itself or add a route tree. Import
`github.com/vo-lang/ui/next/navigation`.

```vo
var routes = navigation.NewRoutes([]navigation.Route{
    {ID: "workspace", Path: "workspace", Render: Layout, Children: []navigation.Route{
        {ID: "overview", Render: Overview},
        {ID: "person", Path: "people/:person", Render: Person},
        {ID: "missing", Path: "*rest", Render: Missing},
    }},
})

func Layout(scope *ui.Scope, match navigation.Match, outlet ui.View) ui.View {
    note := ui.String(scope, "note", "")
    return ui.Element("section", NoteField(note), outlet)
}

func Person(scope *ui.Scope, match navigation.Match, outlet ui.View) ui.View {
    return ui.Element("h1", ui.Text(match.Param("person")))
}
```

Define the route tree once at package scope, like component definitions. Its
constructor copies the descriptions, validates them and creates stable component
identities. `Render` receives its own scope, the current match and the nested child
view. A parent may omit `Render` to contribute only a path prefix and scope.
Parents with children match through those children; add an empty-path leaf for
the default page. Every leaf needs a render function. IDs must be globally unique
within the tree. Replacing the tree creates new route component identities.

Use `router := navigation.Use(scope, "router", initialURL)` during render, then
`routes.View(router.Current(), notFoundView)`. The initial URL must match server
rendering; the host emits its actual URL after the first commit. `Parse(rawURL)`
also creates a `Location` for SSR or tests and reports malformed URL escapes.
`Routes.Match(location)` returns `(Match, bool)` when applications need explicit
control over the fallback. `match.View()` builds the selected nested component
tree; `Match.ID` always names the selected leaf, including in ancestor layouts.

Paths are relative, case-sensitive segments without leading/trailing slashes,
query strings, fragments or dot segments. An empty path adds no segment.
`:name` matches one nonempty segment; a terminal `*rest` matches zero or more.
Parameter names use ASCII letters/underscore followed by letters/digits/underscore
and must be unique across the branch. Static text is written as decoded text.
Optional segments and regular-expression patterns are not supported.

Precedence compares segments from left to right: static, named parameter, rest.
An exact endpoint outranks an empty rest capture. The order of declarations does
not select the winner. Equivalent patterns such as `people/:id` and
`people/:name` are rejected at construction. Nesting is bounded to 64 layouts.
Matching currently scans the compiled branches; no trie or code generation is
needed for the small application route tables this implementation targets.

Locations accept one optional trailing slash. Repeated separators do not match.
Matching splits the escaped path and then decodes each segment exactly once:
`people/a%2Fb` produces one `person` value `a/b`, while `people/a/b` has two segments.
`+` stays a plus in path parameters. `Location.Query` uses normal query decoding
and preserves repeated values; `Fragment` is available independently. Treat
locations returned by `Parse` and `Current` as snapshots; parse a new URL when
changing the path. Each layout receives an independent query map.

Layout state, effects, refs and DOM survive child navigation. Changing parameters
or query values updates an existing route's props. Use a keyed inner component
when an entity change should reset its local state. Leaving a branch disposes its
scopes, tasks and effects; returning mounts fresh scopes. Route rendering works
inside ordinary `ErrorBoundary` views. A leaf can declare `data.Use` against an
ancestor-owned client to share requests through navigation.

`Link(href, children...)` emits a native anchor. Browser services intercept
ordinary internal clicks after subscription; modified clicks, new tabs, downloads,
external URLs and same-page fragments keep native behavior. `Navigate` pushes a
history entry, `Replace` replaces it, `Error` exposes host failures and `SetTitle`
updates the document title from a handler/effect. Imperative navigation accepts
same-origin HTTP URLs. Back/forward and hash changes reach all live root observers.

Declare `navigation.Restore(scope, "viewport", router, "main-content")` beside
the router to own window scroll and focus restoration. The named content element
should be focusable, for example `<main id="main-content" tabindex="-1">`.
Use an empty ID to omit the default focus target. Initial mounting preserves the
existing viewport and focus, including input edited before hydration. Subsequent
navigation acts only after the new URL's view has committed; obsolete and repeated
commit notifications have no effect.

New routes scroll to the top or a fragment target and focus the main
content. Back/forward restores the recorded position and an element with the
previously focused ID when available, otherwise the main content. Native same-page
anchors retain the browser's own scrolling and focus behavior. For query changes
which should keep the current viewport, use
`router.NavigateWith(href, navigation.NavigateOptions{Replace: true, PreserveScroll: true})`.

For filters, `router.UpdateQuery(url.Values{"q": {text}, "page": nil}, options)`
patches just those fields against the browser's current URL. Repeated values retain
their order; nil or an empty slice removes the field. Independent rapid updates
compose in task order without reading an outdated rendered URL. Other fields,
the path and the fragment remain intact. The adapter validates at most 128 fields
and values and an 8 KiB encoded final URL before changing history.

Set `PreserveFocus: true` for query updates that should retain the active input
and its selection. This acts independently of `PreserveScroll`; if the focused
element is removed by the new view, restoration falls back to the content target.
The [Fieldnotes template](../templates/fieldnotes/README.md) uses these options for
URL-backed search, topic and sort, with ordinary navigation for pagination.

There is one viewport owner per window; other roots may keep independent history
subscriptions without declaring restoration. Removing that declaration or closing
its root releases listeners and restores the prior browser scroll policy. A failed
installation also releases its partial resources. History state must be null or an
object: object fields are preserved, and `__volangUiNavigation` is reserved for
entry identity and saved position. Other state shapes are rejected before mutation.
New pushed entries get a fresh state object; replace preserves other object fields.

The owner retains at most 128 recently visited positions in memory, and saves the
outgoing position in its history entry before managed navigation. If a returning
page first renders a shorter loading view, restoration follows layout growth for
up to three seconds. Wheel, touch, pointer or keyboard input cancels that follow-up;
another navigation or owner disposal also cancels it. This policy covers the window
viewport. Missing fragment targets also receive a bounded three-second wait for
document insertion, including an initial client-rendered deep link. Existing
initial targets retain native positioning; input, navigation and disposal stop
the wait. Nested scrolling panes, history across a full reload and virtualized
list restoration need application-specific integration.

See [Studio's route tree](../../../apps/studio/next/app/routes.vo) and
[native route contracts](../tests/runtime/routes.vo). Nested documentation pages
provide a real consumer with a persistent layout and dynamic chapter parameters.

Route-level lazy loading, SSR data prefetch and deployment fallback adapters remain
separate work. A static
host must serve the application shell for direct route requests; matching alone
does not configure the server. Current Studio SSR uses the actual requested URL.

Requirement reference: [React Router's routing guide](https://reactrouter.com/start/declarative/routing)
describes nested layouts, default children and dynamic segments. The APIs and
precedence rules above are this package's explicit contract.
