# UI rewrite laboratory

Experimental implementation of the [Web-first rewrite plan](../../docs/ui-platform-rewrite-plan-20260913.md).
The public API and transport are provisional. This directory provides executable
architecture evidence and the default Studio Web site candidate. Existing stable
UI packages remain available during migration; the replacement is not yet
product-certified.

Start with [First steps](guides/first-steps.md), then read
[State & identity](guides/state.md) and [Lifecycle & requests](guides/lifecycle.md).
For existing screens, use [Migrate a Web screen](guides/migration.md).
Both complete guide applications are tested directly from the delivered source
through the public CLI on the Web VM and all three browser engines.

Eight complete [project templates](toolchain.md) are delivered with the toolkit.
For example, `vo ui create my-garden --template plot` creates a chart application
with a native data table and working browser tests. Canvas, scrolling and measured
list examples use the same public creation and testing commands.
Inside a project containing `ui-next.json`, run `vo ui dev`, `vo ui build` or
`vo ui test` directly. These commands also accept a project directory;
`--project <directory>` remains available for explicit selection.
With the optional [desktop SDK](desktop.md), `vo ui run` opens the same project
using native JIT and `vo ui package` produces a standalone Native AOT application.
Both commands also accept explicit VM/JIT/Native AOT selection.

The optional [development inspector](inspect/README.md) shows component state,
declared props, update causes and individual request/subscription lifetimes.
`ui.InspectProp(scope, "name", name)` exposes a typed render input without changing
the closure-based component API. New projects demonstrate this with their initial
input; production entries omit the optional collector and panel.

## What runs

Components, state ownership, dependency tracking, reconciliation, effects and HTML
serialization execute in Vo. The Wasm VM runs the application bytecode. Browser TypeScript manages DOM identities, native input and
transport. The Rust adapter only transports bytes and suspends/resumes the VM.

The kit currently includes native controls, feedback, ToastRegion, Presence, Dialog, AlertDialog, Breadcrumb, Tabs, Accordion, Popover, Tooltip, Menu, Combobox, Listbox, Table and Pagination; each
interactive recipe is exercised on the Web VM and all three browser engines.
The [workbench](examples/workbench/main.vo) combines two instances of an imported
filterable list, caller-provided row content, inherited context, a settings form,
cancellable saves/search and a real uPlot chart. `?example=workbench` selects it.

The new [Studio](../../apps/studio/next/README.md) consumes this API at
`/studio/gallery`, `/studio/docs` and `/studio/playground`. Gallery and Docs do not
load the compiler. Playground runs standard-library examples in a disposable
worker and preserves a local draft. `/studio/playground/ui` also compiles and runs
a UI component in an isolated preview document, with an independent draft. The
new static distribution owns the default CI site candidate; hosted promotion and
the remaining native consumers have separate migration acceptance.

The optional [navigation package](navigation/README.md) supplies nested layouts,
default pages, dynamic parameters and explicit 404 views. Studio uses a shared
shell and a persistent documentation layout; chapter navigation updates its leaf
while retaining the layout's search state and DOM.

The optional [source editor](web/editor/README.md) enhances a native textarea with
an independently loaded CodeMirror widget. Studio keeps drafts, forms, early SSR
editing and input acknowledgements on the same native control. Gallery and Docs
do not load the editor library. Unavailable enhancements retain usable native input.
Ordinary projects add `"features": ["editor"]` to their existing `ui-next.json`;
the generated boot file stays unchanged. The toolchain installs the same optional
provider, splits the pinned library and includes its dependency versions and
licenses in the distribution. Projects without the feature omit that dependency.

The optional [Canvas bitmap pack](web/canvas/README.md) accepts immutable RGBA8
snapshots computed in Vo. Enable `"features": ["canvas"]` to install its small
native host through the same project tools. It retains the canvas through pixel
updates, releases backing storage on disposal and supplies a labelled SSR
placeholder. The [pixel landscape example](examples/canvas/app.vo) exercises
palette changes, hiding, failure recovery and source reload through public tools.

[Portal placement](portal.md) moves an HTML element into another element in the
same root while retaining its component state, context and lifetime. Missing
targets fall back inline; server HTML remains readable and early input is adopted
before placement. Native events, forms and CSS follow the destination DOM.

[Element scrolling](scrolling.md) adds instant, post-commit positioning and an
optional native-position guard for automatic corrections. It shares ref ownership
and operation order with focus/reveal requests and preserves newer user scrolling.

The optional [document package](document/README.md) renders bounded semantic
content through normal Vo views. Studio loads maintained Web UI and language chapters as
individual JSON assets, shares them through the data cache, and transfers the
selected server result with its HTML for hydration without a duplicate request.
The same semantic content supplies a separate index loaded on the first search.
Queries match all words across titles and body text, including code examples;
unavailable text search leaves title filtering and a local retry available.

```vo
import (
    "fmt"
    ui "github.com/vo-lang/ui/next"
)

var counterType = ui.Define("example.Counter")

func Counter(name string) ui.View {
    return ui.Component(counterType, func(scope *ui.Scope) ui.View {
        count := ui.Int(scope, "count", 0)
        return ui.Element("button", ui.Text(name)).On("click", func(event ui.Event) {
            count.Set(count.Get() + 1)
        }).Attr("data-count", fmt.Sprint(count.Get()))
    })
}
```

The complete application is [examples/interaction/main.vo](examples/interaction/main.vo). Its props use an
ordinary typed struct and wrapper function. Component definitions are package-level
singletons; mounted state belongs to each instance. Sibling keys preserve identity
under reordering. Changing type or key creates a new instance.

`Int`, `String` and `Bool` expose typed state. `State` holds application-defined
snapshots through `any`; use a typed application wrapper and replace/copy mutable
collections in `Update`. In-place mutation of an object returned by `Get` is not
tracked. There are no generics in the current language.

`Effect(scope, key, setup, dependencies...)` uses explicit comparable snapshots.
It runs after the DOM commit acknowledgement. An empty dependency list runs once
per mount. A changed dependency cleans up the previous resource before setup;
removing the declaration or component also cleans up. Return a cleanup function
from setup. Mutable slices/maps cannot serve as dependencies. Cleanup should use
captured resource handles: state access after its component is disposed fails.
`Derived` provides lazy, cached computations, dynamic dependencies and cycle
diagnostics. Its capture is refreshed on every owner render; this can recompute
when ordinary props change. `NewContext`, `Provide` and `UseContext` implement
ancestor snapshots; typed application wrappers keep casts at the package boundary.
Context consumption reads the nearest ancestor, allowing a provider to derive an
override from its parent's value. Mutable fallback objects must not be shared.

`Resource` reports pending/value/error and cancels replaced loads. `Reload` retries
the current request. `web.GetText` and `web.Delay` are typed convenience functions.
`Start` supports lower-level effect/handler requests and returns a cancellable task.
Completions return through the root's event queue; a cancelled/disposed/completed
task cannot call its guest handler. These services cover asynchronous host I/O and
timers. Guest goroutine task composition remains separate; complete VM roots can
also execute in a dedicated Worker through the host adapter described below. State
publication must remain on the root's UI writer.

The optional [data package](data/README.md) shares identical requests across
components and retains bounded inactive results. It provides freshness, explicit
invalidation, stale data during refresh, and last-observer cancellation. The
workbench exercises actual request deduplication and cache reuse on Wasm VM
in Chromium, Firefox and WebKit.

`Subscribe` receives successive values until cancellation or a terminal error.
`Watch(scope, key, request, receive)` declares that lifetime during render. It
refreshes the callback on rerender without reinstalling an unchanged source.
Changed requests, removed declarations and scope disposal cancel it. A timeout
bounds the whole subscription lifetime. Host `WatchProviders` install a source
and release their resources on AbortSignal; an error ends the source.

`web.PageActivity(scope, key)` returns a PageState whose Active reader follows
the owning document's visibility and focus. It initially returns false, including
SSR, and publishes distinct native changes through a scoped subscription. This
can pause time-sensitive feedback while the page is inactive. ToastRegion uses
the same service; no page listeners are installed unless an observer is declared.

`web.MotionFinished(id, timeoutMilliseconds)` observes current finite animation
on one element inside the root. Start it in a post-commit effect after updating
CSS state; no animation or missing elements complete immediately. Cancelling
releases the request’s animation subscriptions. The explicit timeout (1..60000ms) bounds custom
paused animation. Child/infinite animations are excluded. `kit.Presence` composes
this task with scope disposal, interrupted exits and logical focus return.

`ErrorBoundary(content, fallback)` catches render/reconciliation panics inside its
content. The fallback receives a `*RenderError` with `Value` and an idempotent
`Retry()` method for a native handler. A failure disposes that content, cancels its
tasks, and publishes the fallback in the same batch. Other boundaries, siblings
and ancestors retain their state and DOM. Retry mounts fresh content; callbacks
from older failures or disposed boundaries have no effect. Fallback failures reach
the next outer boundary. Handler, effect, cleanup and host exceptions are outside
this API. See [the recovery example](examples/interaction/recovery.vo).

`Components()` returns an independent diagnostic snapshot, innermost first, with
component names, sibling keys and definition file/line when available. An uncaught
component panic is a `*RenderFailure`; its `Value` retains the original panic and
its `Error()` includes the component trace. Source locations identify definitions,
not the exact statement which panicked.

`Request.WithTimeout(milliseconds)` sets a deadline from the committed host start;
zero disables it. Timeout aborts host I/O and arrives as a normal local result
error. Retry gets a fresh deadline; completion, replacement and disposal release
the timer. `ui.Resource(scope, key, web.FetchText(url).WithTimeout(5000))` applies
this to a text request. A provider must cooperate with its AbortSignal; late
results are rejected even if it ignores cancellation.

`web.HTTP(url, web.HTTPOptions{...})` adds methods, text bodies, credentials and
canonical headers. `web.DecodeHTTP` returns readable status, headers and body,
including structured 422 errors for forms. Both HTTP and `FetchText` use a
cancellable 2 MiB response reader; the native server can return typed JSON.
See [browser requests](web/README.md) for limits and composition with query
caching and form submission.

Each root's input queue owns immutable encoded events within 4096 events and
64 MiB. Completions split into FIFO frames within 16 MiB and 128 events;
backlog batches yield through host tasks before the next delivery.
single oversized events and queue exhaustion produce a stable root failure.
Native dispatch still finishes before the guest receives its first event batch.

HTML attribute names are case-insensitive. The reconciler canonicalizes them
after resolving ancestry, so alias casing changes do not remove a live binding.
When aliases overlap, the last assignment wins. SVG attributes remain
case-sensitive. Use canonical lowercase names with Class and DescribedBy helpers.

`Element("svg", ...)` derives SVG namespaces through ordinary components and
fragments, preserves case-sensitive tags/attributes, and returns to HTML inside
`foreignObject`. `Element("textarea").Attr("value", text)` is controlled and
supports SSR, early editing, composition, and immediate form submission. Textarea
content belongs to its value binding; child views are rejected. Unbound textareas
keep their native value. `DefaultValue(text)` initializes an uncontrolled input or
textarea; `DefaultChecked(bool)` initializes checkbox/radio defaults. The browser
owns live edits. Changing a default updates its native reset baseline, and removing
the declaration clears that baseline. Pristine controls follow native default
updates; edited text/checkbox state stays intact. Radio groups retain the browser's
group selection rules. These methods cannot be combined with a controlled value
or checked binding for the same property. For a native select, option `selected`
attributes provide reset defaults and a select `value` binding controls live state.
SSR includes native defaults and captures early input without turning it into a
controlled field. File inputs cannot receive a nonempty default value.

`SelectedValues([]string)` controls a native `<select multiple>` and copies its
values. It owns the `multiple` attribute and cannot share a scalar `value` binding.
An empty list clears selection; omitting the binding on a later view releases
control. Input/change events expose all `SelectedValues` in option order, including
selected disabled options; `Value` remains the first native value. SSR retains
edits to any selected option made before startup. Native FormData independently
applies successful-control rules. Lists are bounded to 4096 values and the frame
byte limit. Each matching option is selected, including duplicate/empty values;
the higher-level UIKit requires unique declared choices.
For native audio/video, `Attr("muted", "true")` sets both the initial
declaration (`defaultMuted`) and live mute. A changed declaration or its removal
updates live mute. Adoption of an unchanged server default preserves any native
mute choice made before boot; unrelated renders also preserve native choices.
This follows the separate [live and default media properties](https://html.spec.whatwg.org/multipage/media.html#dom-media-defaultmuted).
HTML `title` and `style` now accept zero or one direct `Text` child, using the
same text identity and updates in client and adopted roots. See [native HTML
text, JSON data and media](native-html.md) for escaping, parser limits and ownership.
`ui.DataBlock` provides inline JSON and JSON-LD snapshots with ordinary text
identity. Executable scripts and specialized parsing contexts require their
separate resource/content bindings; the supported boundaries are documented.

Controlled select values also apply after option changes. SSR selects the matching
option, including implicit option text values. Native form reset settles controlled
fields from component state and leaves unbound fields to native defaults. Input
delivery uses a posted task, because microtasks can run before a native event's
default action. Nonempty controlled file values are rejected before DOM mutation.

`OnWith(kind, EventOptions, handler)` declares capture, prevent-default,
stop-propagation and passive behavior. Native actions happen synchronously;
guest callbacks follow in the input queue. A node can have one handler per kind
and phase. Passive plus prevent-default is rejected. `On` uses bubbling, with
default prevention for managed `submit`; explicit `OnWith` options replace those
defaults. Event payloads include capture phase, modifier keys, repeat/composition
state and mouse button. `EventOptions.Keys` limits keyboard listeners to exact
key values. Unrelated keys and composing input bypass the binding, including its
default prevention. `EventOptions.Modifiers` declares exact alternatives, for
example `[]ui.KeyboardModifiers{{Ctrl: true}, {Meta: true}}` with `Keys: []string{"Enter"}`
matches Ctrl+Enter or Cmd+Enter. Additional modifiers, including Ctrl+Alt, bypass
that binding. Nil or an empty slice accepts any modifiers; `[]ui.KeyboardModifiers{{}}`
accepts only an unmodified key. Modifier-only filters are supported. Both arrays
are copied; changes rebind the native listener. Composition state follows the
event target, including keyboard handlers on ancestors. `Event.Repeat` remains
available to handlers that should ignore key repeat.
Normal links retain native behavior unless interception
is explicitly declared.

For `input` and `change`, `Event.Value`, `Checked` and `SelectedValues` describe
the originating native control, including when a wrapper owns the handler.
`Event.Target` continues to identify the handler owner. Other events retain the
owner's value, so a click on content inside a button still reports that button's
value. During HTML adoption, an edited control replays through its nearest
declared input/change handler using native capture and bubbling. Input
acknowledgements protect the originating control until the last corresponding
handler has been processed. Composition completion follows the same input route.

[Pointer interaction](pointer.md) adds typed coordinates/device data and native
capture for dragging. Capture belongs to the mounted element, survives keyed
placement, and is released with its declaration or root. The resizable preview
example combines pointer movement with keyboard controls.

`Ref(scope, key)` creates an `*ElementRef`; attach it with `view.Ref(ref)` and call
`ref.Focus()` from a handler or post-commit effect. It can be forwarded to child
components. Requests resolve after reconciliation, so showing a conditional
element and requesting focus in the same handler works. Missing/disposed targets
are ignored; the bool result means queued, not a guarantee that a disabled or
unfocusable element gained focus. A ref can bind one live element in its root;
duplicates fail before publishing a batch. Replacement and removal clear old
bindings. Focus applies after native input settlement and lets the browser scroll
the requested control into view. Restoration after a keyed move preserves scroll.

`ref.Measure(func(result ui.Measurement) { ... })` queues a one-shot Web geometry
read after the next DOM commit. It resolves newly shown/replaced elements without
exposing renderer IDs. The result contains Found, X, Y, Width, Height and Error;
coordinates are fractional CSS pixels relative to the element's own document
viewport, including transforms. This follows the browser's
[bounding-rectangle semantics](https://drafts.csswg.org/cssom-view/#dom-element-getboundingclientrect).
Missing bindings report Found=false; a hidden element can report Found=true with
a zero-sized rectangle. Results are snapshots and do not subscribe to scrolling
or animation. An explicit measurement can force layout; use CSS/container queries
for presentation and `OnViewport` for ongoing client-size/scroll observation.

The returned task can be cancelled; component disposal discards late results.
Nil/disposed refs return nil. Call from handlers or post-commit effects, and cancel
in an effect's cleanup when the result becomes obsolete. SSR renders do not run
effects or synthesize a geometry result. Native platform measurement is a later
adapter capability; the current provider serves the Web VM.

`navigation.Use` observes path/query/fragment and exposes `Navigate`/`Replace`.
`navigation.Link` keeps native modifier/new-tab/download/external-link behavior.
Install `createNavigationServices(container)` in the host's optional services.
History changes reach active root subscriptions; disposal removes their listeners.
`navigation.SetTitle` updates the document title. Studio focuses its main content
on subsequent route changes while preserving early SSR focus on first activation.
Same-page fragment links retain native scrolling and focus. The optional router
adds nested layouts and route matching; `navigation.Restore` owns window history
restoration. The data package provides shared caching separately from navigation.
See the navigation guide for restoration bounds and deployment requirements.

`host.RunWithData(render)` reads bounded host-supplied initial data before creating
the view. The server constructs its view from the same data; pending early input
remains queued through this handshake. Studio uses it for deep-link SSR.
`host.HTML(view)` creates and closes an isolated server root without starting
effects. The transport and HTML helpers format uncaught component failures with
their component trace; the core Root API retains the typed failure.

`Widget` gives a registered extension exclusive subtree ownership. Optional host
providers implement mount/update/dispose with an AbortSignal and queued events.
The typed `web/plot` wrapper uses [uPlot](https://github.com/leeoniya/uPlot), pinned
to 1.6.32 in the toolkit lockfile. Ordinary projects enable it through
`features: ["plot"]`; the [plot guide](web/plot/README.md) covers typed data, lazy
loading, local retry and accessible server-rendered alternatives. Chart updates reuse its canvas; ResizeObserver and
chart listeners are released at unmount. Widgets are created after DOM mutation,
then the guest receives the commit acknowledgement. SSR emits a placeholder and
hydration mounts the widget once. Providers must arrange cleanup for partial
initialization failures; arbitrary external side effects cannot be rolled back.
An integration may call `context.fail(message)` for an asynchronous failure;
this aborts/disposes its instance and delivers the existing local `Result.Error`.
Late events and failures from removed instances are ignored.

The optional `createLazyWidget(load, options)` export from `mount.js` defers a
browser integration until its first mounted use. Its loader receives the root's
Document and AbortSignal and returns a synchronous WidgetProvider. While pending,
only the latest input is retained; unrelated application interaction and `ready`
continue. The default loading deadline is 15 seconds, configurable from 1 to
60000 milliseconds. Once mounted, it follows normal update/disposal ownership.
Removal or failure releases subscriptions, cancels the signal and prevents late
installation. Native module import caching supplies module reuse; the adapter
adds no permanent module cache. A changed payload/remount can invoke the loader
again after failure, subject to the browser's cached module failure behavior.

```js
import { mountUi, createLazyWidget } from '@volang/ui-next';
const widgets = {
  chart: createLazyWidget(async (document, signal) => {
    const { createChartWidget } = await import('./chart-adapter.js');
    signal.throwIfAborted();
    return createChartWidget(document);
  }),
};
// Supply widgets through mountUi(..., { services: { widgets } }).
```

Application state, placeholders and retry controls stay in the typed Vo wrapper.
Loaders own module/CSS registration and cleanup of any partially acquired native
resources. Standard dynamic imports may continue downloading after cancellation;
the disposed integration cannot mount or publish a result. This capability loads
optional browser integrations; independently compiled Vo route entries remain
separate work.

The optional [`web/custom`](web/custom/README.md) wraps registered Custom Elements
with declared object properties, string attributes and JSON event details. Its
managed adapter retains the native instance and cleans up listeners on removal.
The workbench demonstrates both uPlot and a native component with shadow DOM.

The optional [`data`](data/README.md) shares requests, bounds inactive caching,
and transfers successful server reads into the initial browser cache. Fresh
initial data avoids duplicate requests; stale data refreshes after commit while
preserving the rendered value and server DOM.

Optional [`inspect`](inspect/README.md) provides component/state/dependency
snapshots and a bounded history of render causes and timings. Its explicit
development entry and browser panel use the existing task transport; ordinary
application builds do not import the collector or panel. Open the independent
lab at `/?example=inspection&backend=vm` after building the examples.

Advanced host tools can register `Root.OnMount(setup)` before the first render,
then use `Root.Start` / `Root.Subscribe` after commit. These services use the
ordinary task queue and root cancellation without creating component or DOM
nodes. Setup runs once after a successful first commit, and returned cleanups run
after cancellation. SSR and abandoned renders never start a root service.
Inspection and development reload use this boundary, preserving the application's
production HTML structure and component identities during development activation.

The optional [`kit`](kit/README.md) includes native controls, feedback and
keyboard-operated collections and layers. [`forms`](forms/README.md) owns field snapshots,
validation and submission lifetimes. `forms.Data` carries text fields and typed
native file selections through the same validation/submission snapshot. Text values use the standard ordered
`net/url.Values` model; `All`/`ChangeAll` connect repeated choices such as
`kit.CheckboxGroup`, `kit.MultiSelect` and `form.MultipleSelect` through the same
validation and save lifecycle.
`Options.Arrays`, `WithArray` and `form.Array` add stable nested row groups to
that same model. Insert/remove/move retain row keys, and reset, revert, native
submission and development reload include row shape. See the
[address-book example](examples/field-array/README.md) and the array contracts in
the [forms guide](forms/README.md).

[`forms/schema`](forms/schema/README.md) adapts structured validation issues to
the same field errors. A captured value snapshot maps array positions to stable
row names; repeated scalar values have a separate index operation. Applications
or independent libraries provide the validation rules and decoding.

[`files`](files/README.md) enhances native file inputs, adopts early selections and
provides bounded UTF-8/binary reads through scoped tasks. `form.FileInput` owns
selections with the form. [`web/upload`](web/upload/README.md) sends native
multipart bodies using the existing HTTP response and cancellation contract. See
the [delivery example](examples/files/README.md) and the callback migration in
the forms guide. File references remain local to one root and reset across reloads.
`Class` and `DescribedBy` compose caller
tokens instead of discarding existing customization.

[`Styling`](styling.md) documents `StyleScope`, native CSS scope boundaries,
theme inheritance and application overrides. UIKit defaults live in the `vui`
cascade layer; ordinary application CSS can override them. The scoped starter
and `/?example=styling` demonstrate the same pattern without a style runtime.

[`collection`](collection/README.md) provides immutable keyed snapshots and an
independent fixed-row virtual window. `View.OnViewport` reports native client
geometry through the writer, coalescing scroll and resize delivery per frame.
`kit.Listbox` adds single/multiple selection, disabled items, typeahead and a pinned
active option; its interaction example grows to 100,000 records on demand.

Both browser host factories expose `ready: Promise<boolean>`. It resolves true
when the guest has committed its initial view/effect commands and first waits for
input; early closure resolves false. It does not wait for pending network data.
Use it to distinguish host creation from activation; native SSR links remain
usable before activation.

## Reproduce

Run from the repository root. Prerequisites are the repository's pinned Rust/Node
tools, installed `vo-web` and browser-test dependencies, and Chromium, Firefox and
WebKit installed through the locked Playwright version (currently 1.63.0);
results and screenshots remain in separate per-engine directories. Build the CLI and Web packages
from the same checkout; serialize Cargo/wasm-pack operations in this target tree.

```sh
cargo build -p vo --locked
npm --prefix lang/crates/vo-web run build:wasm:release
node eng/ui-next/build-runtime.mjs
node eng/ui-next/build-runtime.mjs --compiler
npm --prefix lang/crates/vo-web run build:js
npm --prefix eng/ui-next ci --ignore-scripts
npm --prefix ui/editors/vscode ci --ignore-scripts
node eng/ui-next/generate.mjs --check
node eng/ui-next/build.mjs
node --test eng/ui-next/runtime.test.mjs eng/ui-next/codec.test.mjs eng/ui-next/inspection.test.mjs eng/ui-next/prerender.test.mjs eng/ui-next/reload.test.mjs
node eng/ui-next/check.mjs
UI_NEXT_BROWSER=firefox node eng/ui-next/check.mjs
UI_NEXT_BROWSER=webkit node eng/ui-next/check.mjs
target/debug/vo fmt --check ui/next
target/debug/vo-dev lint artifacts
```

The existing `vo-dev` binary is required for the last command; build it with
`cargo build -p vo-dev --locked` if needed. This example uses the Web VM
module imports; the standalone application check separately executes regexp support. The build script resolves the compiler through the repository's
`test_compiler.mjs` helper. Framework probes disable workspace overrides;
Studio explicitly uses the repository's `vo.work` to consume the new local UI.

The execution-only and Playground compiler builds include the isolated
`vo-ui-bridge` transport and check their Wasm dependency graphs for legacy UI
kernels. Their outputs live in `target/ui-next/wasm-runtime` and
`target/ui-next/wasm-compiler`. Studio downloads the compiler only when running
an example; it uses the same isolated compiler in development and deployment. Standard
`vo-web` builds keep the previous UI through the default `legacy-ui` feature;
old execution-only embedders can select `--no-default-features --features legacy-ui`.
The new framework uses generic Island exchange and owns its own component state
and reload, independently of the old UI arena and browser methods.

`build.mjs` builds bytecode, executes the
native contracts, serializes server HTML and records artifact sizes/digests.
`runtime.test.mjs` executes that bytecode on the Wasm VM. It also tests the bounded input queue, including overload.
`check.mjs` starts a temporary local server and the selected browser engine, verifies DOM
boundary contracts, and executes the Wasm VM with client rendering and hydration.
This now includes the workbench's form, real HTTP cancellation and uPlot lifecycle.
It also runs new Studio navigation, theme, worker execution/cancellation, local
draft, mobile overflow and deep-link SSR contracts on the Web VM.
The server and browser close when the check finishes.

The unified entry runs the same build and checks with
`node eng/ui-next/cli.mjs check`. It checks all three browser engines, Studio development
recovery and the standalone application workflow. Use `build` for artifacts, `preview` for a local
server, or `dev` for Studio VM rebuilds and live CSS. Compilation errors preserve
the running page; correcting Vo source restores compatible local state. CSS
changes preserve the DOM. See [development reload](develop/README.md) for state,
input, service ownership and reset rules. Projects with `ui-next.json` select
these tools through the public `vo ui` commands; other projects retain their
compatibility command path.

## Create an independent application

After building the prerequisites above:

```sh
node eng/ui-next/cli.mjs create target/my-ui-app
node eng/ui-next/cli.mjs dev --project target/my-ui-app
node eng/ui-next/cli.mjs check --project target/my-ui-app
node eng/ui-next/cli.mjs build --project target/my-ui-app
node eng/ui-next/cli.mjs preview --project target/my-ui-app
```

`create` requires a new directory and writes a small Vo application plus `web/`
assets. `vendor/ui` contains the current framework source snapshot, selected by
`vo.work` and a compiler-generated `vo.lock`; builds never rewrite that lock.
`ui-next.json` binds the experiment's wire version. These commands also ship in
the [portable toolchain preview](toolchain.md), available through `vo ui create`
and `vo ui <command> --project <directory>`. The earlier `vo ui web` spelling
continues to use the same tools.
Projects and tools can move independently; generated browser fixtures use the
active runner. Existing positional UI commands retain their project behavior.
Release publication and the remaining default Web migration continue separately.
Use [`vo ui doctor`](diagnosis.md) for configuration and installation diagnostics.
`check` validates every declared source entry and host import without executing
prerender code or generating a distribution.

For a routed application with a native server, use
`node eng/ui-next/cli.mjs create target/my-library --template fieldnotes`.

For independent static page programs, use
`node eng/ui-next/cli.mjs create target/small-pages --template pages`.
Named `pageEntries` bind each page to its own Vo image while sharing host assets.
For request-time SSR, combine them with `serverEntry` and select the declared
image through `server.Page.Entry`; an empty entry uses the default image for HTML.
See [page entries](page-entries.md) for configuration, development behavior,
document navigation and the current delivery limits.
The [Fieldnotes template](templates/fieldnotes/README.md) includes a reading library,
URL filters, nested detail routes, pagination and preferences with shared Vo
validation. The server owns the dataset and a small preferences cookie, returns
page-specific HTML and query data, and supports standard POST as well as JSON
writes. It uses the same check/build/dev/test commands as the default starter.
The optional browser navigation service is explicitly imported from
`@volang/ui-next` and installed for each mounted root.

The starter's shared `app.View(initial string)` lives in `app/app.vo`. Its small
`main.vo` production entry uses `host.RunWithData`; `development/main.vo` uses
`develop.RunWithData`; `prerender/main.vo` uses `prerender.Run(app.View)` at build
time. All three entries receive the same initial data string.
`developmentEntry` in `ui-next.json` selects the development entry only for `dev --project`.
The development page offers **Inspect components**, with one panel per document
and an application selector when multiple roots are mounted. Captures show named
state, dependencies and recent render causes without rerendering the application.
Production resolves the regular browser mount entry and omits both the Vo
collector and JavaScript panel. Existing configurations without `developmentEntry`
keep their regular application entry.

For a custom development entry, name a source file or directory within the
project, call `develop.Run` there and share application components through an
ordinary Vo package. Production, development and prerender entries are type-checked.
The toolkit does not rewrite arbitrary application source to add hooks.

`check --project` validates formatting and types, then builds the Web VM.
`build --project` writes `target/ui-next/dist` inside the application. Copy its
contents to a static HTTP host, including a subdirectory ending in `/`. Relative
assets and an execution-only VM ship in
the result. It has no compiler endpoint or Studio service dependency. The starter
includes static HTML for declared pages. Projects with `serverEntry`, including
Fieldnotes, emit a private native server image and a relocatable Node entry beside
their public assets; use that entry to serve request-time HTML and JSON.
The output names `assets/`, `theme.css` and `build-report.json` are reserved;
put authored assets elsewhere under `web/`.

Production builds also generate `.br` and `.gz` representations for compressible
public files of at least 1 KiB, retaining each only when it saves more than 64
bytes. The build report records their sizes and hashes. Matching authored
sidecars cause a build diagnostic; compression always runs in the unpublished
build directory. Static preview and the request-time server negotiate the client's
accepted encoding, keep the original MIME type, and validate caches with ETag.
Files stream with backpressure; HEAD and 304 responses avoid reading the body.
Development serves original files with caching disabled. External static hosts
must enable precompressed-file negotiation to use these representations.

Static preview and the native server stream a single requested byte range with
206 and `Content-Range`; suffix/open-ended ranges work, and unsatisfiable ranges
return 416. Ranges refer to the selected encoded representation. HEAD, empty
files, unsupported/multiple ranges and `If-Range` requests use a full response;
the current filesystem metadata validators are weak, so they cannot authenticate
partial resumption. A matching conditional cache request still returns 304.
The [listening template](templates/listening/README.md) exercises native audio,
early playback, seeking, theme updates and removal without a player dependency.

`prerenderEntry` selects a Vo source file or directory inside the project. The
builder compiles it once, then runs a fresh native process for each page.
[`prerender.Run`](prerender/README.md) reads initial data from stdin and prints
only the rendered root HTML. The server view must match the client's initial
view. Client effects remain inactive; each built document is shared across visits.
Each page is limited to 30 seconds and 16 MiB of UTF-8 HTML; the generated
document set is limited to 64 MiB. Failure or cancellation preserves the previous
complete distribution. Legacy entries printing only `host.HTML(view)` with
`fmt.Print` remain supported for a root page without initial data.

By default the builder emits `/` with empty initial data. Set `prerenderPages`
in `ui-next.json` for up to 256 directory-index pages, including the root:

```json
"prerenderPages": [
  { "path": "/", "data": "" },
  { "path": "/people/grace/", "data": "Grace", "title": "Meet Grace" }
]
```

Each data value is a Unicode string of at most 1 MiB; applications can decode
their own JSON structure. Paths use portable directory names and are unique
across case and Unicode normalization. The build report records each page's
HTML and data hashes. Generated pages cannot replace authored `web/` HTML.
The config's `document` object supplies shared `title` and `description` defaults;
each page can override them. Titles are bounded to 4 KiB and descriptions to
8 KiB. They appear immediately in server HTML and development pages.

The starter's `web/index.html` has one `<!--ui-next:content-->` inside the root
and one `<!--ui-next:mode-->` in its `ui-next-render` meta tag. One
`<!--ui-next:data-->` in the inert `ui-next-data` JSON script carries the initial
string, decoded by `services.initialData` in `web/boot.js`. Prefix generated
asset URLs with `<!--ui-next:assets-->` so nested pages resolve their assets
within the deployment directory. The builder fills the content and mode with
HTML and `server`; its browser entry opts into hydration accordingly.
`<!--ui-next:title-->` inside the document title and
`<!--ui-next:description-->` inside the quoted meta attribute receive escaped
text. Legacy templates retain their authored metadata when configuration omits
`document` and per-page metadata fields.
The loaded application retains the server nodes and native input made before
activation. Development uses the same page paths/data with empty content and
`client`. Removing both `prerenderPages` and `prerenderEntry`
keeps client rendering; older projects without markers continue to build in that
mode. Keep both markers exactly once when enabling prerendering in a custom page.

Development output lives separately under `target/ui-next/dev`. Successful builds
replace it only after compilation and bundling finish. CSS edits preserve the
live DOM/state, compilation errors preserve the last working page, and new tabs
receive the current diagnostic. A failed first build still opens a diagnostic
page, including a direct visit to a nested page, and recovers at that URL.
Corrected Vo source restores compatible state; JavaScript/configuration changes restart the page. Stop cancels
the compiler and releases the watcher, event streams and server.

`node eng/ui-next/project-contracts.mjs` reproduces creation, source checks,
failed-build preservation, three-engine Wasm VM static deployment, optional
regexp loading, and development recovery. Its evidence is `project-report.json`.
`node eng/ui-next/prerender-pages-contracts.mjs` covers multi-page initial data,
three-browser VM activation, Unicode paths, subdirectory assets and nested
development recovery. Its evidence is `prerender-pages-report.json`.

`mountUi`, available through the preview `vo-web/ui/next` package entry, owns
loading and execution. The entry also exports the host/provider TypeScript
contracts and optional navigation/lazy-widget factories. See the
[embedding guide](../../lang/crates/vo-web/ui-next.md) for external package use.
Call it with
an artifact URL, `backend`, optional `services`, and a `loadVm` function for VM.
Its `ready` resolves after initial effects; failures and shutdown resolve false
if activation has not happened. `done` rejects on loading/execution failure;
`close()` cancels startup or closes the root, and releases the VM exactly once.
Studio and the starter share this entry. Advanced adapters can still use the
lower-level `createVmUi` host.

Mounts sharing a VM module join one initialization, including wrappers exposing
the same initializer. A failed load permits a later retry; closing one waiting
root leaves other roots' initialization and execution intact. Each mounted root
owns its Island, component state, providers and cleanup.

Application mounts own their document-exit cleanup. A `pagehide` event with
`persisted=true` keeps the application and its state alive for a possible browser
history-cache restore; a discarded document closes its roots and workers. Do
not add an unconditional pagehide close around these mount helpers. Real history
tests record whether each browser actually restored a cached page or reloaded it;
cache eligibility remains a browser decision. Development pages reload the latest
compiled version when restored from that cache, reconnecting their update stream.

`createWorkerUi(container, worker, options)` in `ui_next/worker-host.ts` owns a
dedicated Worker and the same renderer/task boundary. Register it before sending
the application's startup message. It exposes the same ready/done/close lifecycle;
close terminates computation and releases DOM, listeners and tasks. Startup has a
30-second deadline and each guest computation turn has a 10-second deadline.
Options can set each deadline within 1..120000ms. An interactive root waiting for
input has no computation deadline. Overlapping, oversized or out-of-sequence
exchanges fail the connection. This is a separate execution option; ordinary
mountUi applications retain their existing backend selection.

Inside the Worker, call `runWorkerVm(vm, self)` from `ui_next/worker-vm.ts`, then
free the Island in finally. Its shared VM execution loop also drives createVmUi.
Send `{kind: 'ui-exit', error: message}` for loading/compilation failures that occur
before entering the loop. Transferred byte frames use the existing wire codec;
component state and handlers stay with the VM while DOM and native services stay
on the document thread. `createUiTransport` exposes that serialized exchange for
other execution adapters. Studio's UI preview uses this Worker path with a
separate document and a memory-only, explicitly packaged compilation workspace.

To explore the integration fixtures manually:

```sh
node eng/ui-next/server.mjs
```

Open the printed URL. `?backend=vm` selects the Wasm VM; VM is the default.
Add `&ssr` to test server HTML adoption. Stop the server when finished.
Use `?example=workbench&backend=vm&ssr` for the combined example. The local server
supplies a deterministic search fixture; query `error` returns a load failure and
queries beginning with `slow` respond slowly enough to exercise cancellation.

Outputs under `target/ui-next/` are disposable: `build-report.json`,
`browser-report.json`, backend screenshots, compiled images and server HTML.
Generated checked-in wire files are governed by `eng/artifacts.toml`; update their
schema with `node eng/ui-next/generate.mjs --write` and verify with `--check`.

## Limits of this evidence

The examples and independent applications exercise T1–T6, with the remaining
product acceptance tracked in the [rewrite plan](../../docs/ui-platform-rewrite-plan-20260913.md).
Chromium 153.0.8010.12, Firefox 155.0 and WebKit 26.6 have passed the Wasm VM
browser checks. Composition-event tests are synthetic; real IME, screen-reader
and mobile-device validation remain. Full HTML/property and raw-text hydration
coverage, guest goroutine task composition, runtime linking into an existing
root, native multipart upload endpoints, complete UIKit/device acceptance and
formal performance budgets remain open.

Studio provides Gallery, Playground and Docs, with 19 maintained language/toolchain
chapters, four UI guides, optional editing, local drafts and old-project export.
Its independent native and static distributions include rendered content and
versioned initial data; old links and cache retirement have separate upgrade
contracts. The single-file UI source preview uses the Wasm VM; package installation
is not supplied. Native VM/JIT tests
establish headless core semantics. Desktop windows and complete native host/Native
AOT UI execution have not been established here.

The same 1,000-row comparison is implemented for Vue, Svelte and Vo Wasm VM. After building the guest artifacts above, reproduce it with:

```sh
node eng/ui-next/benchmark/build.mjs
node eng/ui-next/benchmark/run.mjs
node eng/ui-next/benchmark/phases.mjs
```

The comparison uses production/minified bundles, three rotated rounds and 60
measured updates per scenario. Reports under `target/ui-next/benchmark/` retain
raw samples, artifact hashes, total raw/gzip bytes, DOM/host work and available
memory counters. `phases.mjs` separately times guest rendering and wire encoding in
Node, using the same app definitions in an instrumented image. These measurements
cover a narrow workload; mobile/network/paint and long-term resource evidence are
still pending. See [performance.md](performance.md) for the recorded results.

New starters include [application browser tests](testing.md). Run
`node eng/ui-next/cli.mjs test --project <directory>` to check/build a production
application and exercise its semantic Playwright tests on three engines and both
Web backends. Failed runs retain screenshots, traces and a separate report.

Production VM pages use the execution-only runtime built by `build-runtime.mjs`;
Playground uses the separately built `--compiler` package. Host loader types
require only initialization, VM construction, exchange and disposal, so either
actual Wasm package can be passed from TypeScript without a legacy API dependency.
Wasm VM UI roots use an isolated host session. Browser persistence belongs to
explicit application services; opening a new UI root does not initialize the old
Studio's OPFS namespace.
Applications can still pull in formatting, JSON and SSR through their own imports.
New Web packages register `vo-ui-bridge` directly. The public APIs remain
experimental while migration and product acceptance continue.

The optional [`server`](server/README.md) package and `serverEntry` project setting
provide request-time HTML delivery through a prepared native entry and a bundled
Node host. Route/data decisions stay in Vo; the host owns request cancellation,
admission and document assembly. See its guide for deployment and current limits.

Experimental wire v25 uses generated binary codecs on both sides. A frame contains
`VUI`, a one-byte version and message kind, signed little-endian 64-bit integers
restricted to JavaScript's safe range, strict booleans, operation codes, and
32-bit length-prefixed strings/arrays. Array length `0xffffffff` denotes nil.
Optional records use a strict boolean presence byte; coordinates use finite
IEEE754 little-endian float64 values.
Frames, counts, integers and complete consumption are checked before use; DOM
topology is still preflighted as a whole batch. Web strings use UTF-8 replacement
for invalid guest bytes and preserve a leading Unicode BOM. There is one schema
and one generated codec path, with no generic JSON transport dependency.
Regenerate and rebuild all guest and host artifacts together. The current schema
includes request timeouts, event options/payloads, bounded keyboard filters, focus
and modal mutations, bounded Portal placement, complete native multiple-selection values, and the initial-data handshake. Earlier
hosts reject the new version. The recorded binary performance slice uses v3.
This is an unreleased experimental protocol.
See [design.md](design.md) for current decisions and gaps.

See [native size observations](size.md) and [measured collections](collection/README.md).
