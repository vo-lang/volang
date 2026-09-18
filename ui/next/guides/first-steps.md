# A small idea, brought to life.

Build a Web interface with ordinary typed Volang code. Components describe the
page, state records what can change, and the browser keeps native HTML behavior.
The Web application runs with the Wasm VM.

This guide uses the new Web UI preview in `github.com/vo-lang/ui/next`. Use a
matching toolchain that includes the Web UI tools. A toolchain with the native
desktop SDK can also run and package
this application with `vo ui run` and `vo ui package`; see the desktop preview
guide in the toolkit for platform requirements.

## Create your first application

Check the installation and create a small project:

```sh
vo ui verify
vo ui create my-app
vo ui dev --project my-app
```

Open the local address printed by the development server. Change a name, press
the button, then edit the source and see the result. Node.js 24 or newer runs the
packaged tools; the application needs no npm manifest or JavaScript application
code. The toolkit contains its matching compiler, framework and build libraries.

The project has a small set of files with clear jobs:

| File | Purpose |
| --- | --- |
| `app/app.vo` | Shared component code used by development, production and server rendering |
| `web/app.css` | Your application's appearance |
| `main.vo` | Production entry |
| `development/main.vo` | Development entry with the inspector and reload support |
| `prerender/main.vo` | Server HTML entry using the same application |
| `ui-next.json` | Build, entry and optional feature settings |
| `vo.mod`, `vo.lock`, `vendor/ui` | The exact framework source selected for this project |
| `tests/browser` | Browser tests using labels, roles and visible results |

## Start with a component

Replace `app/app.vo` with this complete shared application:

```vo
package app

import (
	"fmt"
	ui "github.com/vo-lang/ui/next"
	"github.com/vo-lang/ui/next/kit"
)

var counterType = ui.Define("guide.Counter")

func View(initial string) ui.View {
	return ui.Component(counterType, func(scope *ui.Scope) ui.View {
		ui.InspectProp(scope, "initial", initial)
		count := ui.Int(scope, "count", 0)
		return ui.Element("main", ui.Element("h1", ui.Text("A little progress.")), kit.Button(kit.ButtonProps{
			OnPress: func() {
				count.Set(count.Get() + 1)
			},
		}, ui.Text("Take a step")), ui.Element("output", ui.Text("Steps taken: " + fmt.Sprint(count.Get()))).Attr("aria-label", "Progress").Attr("aria-live", "polite")).Class("vui", "starter").StyleScope("starter-app")
	})
}
```

Define the component type once at package scope. Each mounted instance gets its
own scope, so two instances can keep independent counts. `Int` finds the same
state by its local name on each render. `Get` reads it; `Set` notifies the
components that depend on it. The button runs its action after a browser click.

`Element` and `Text` compose semantic HTML. `kit.Button` adds shared presentation
and button behavior. Pass typed values, callbacks and child views through ordinary
Vo functions when extracting your own components. `InspectProp` optionally shows
the `initial` input in the development panel without changing how it is passed.

Keep the shared `View(initial string)` entry when changing the application. That
lets server rendering and browser activation agree about their first view.
Update the starter's browser test when you change its labels or behavior.

## Check, test and build

```sh
vo ui check --project my-app
vo ui browsers install
vo ui test --project my-app
vo ui build --project my-app
vo ui preview --project my-app
```

Checking reports source and formatting errors. Testing builds the actual
application and runs its browser tests against Wasm VM in Chromium, Firefox
and WebKit. The browser installation is needed once for the matching toolkit.
Use `UI_NEXT_BROWSER=chromium` for a focused local run.

The default build includes static server HTML, application bytecode and the
shared Wasm VM runtime. The output is under `my-app/target/ui-next/dist`; preview
serves that output. Omit `defaultBackend` or set it to `vm` in `ui-next.json`.
Development uses the same VM with rebuilds and diagnostics.

## Inspect an update

Open **Inspect components** in the development page. Expand a component to see
its declared props, scalar state, dependencies and latest update cause. Press the
button, wait for the new count, then capture again. Requests and subscriptions
show their owner, status, replies, timeout and observed duration.

The panel samples only when requested. Its timing includes queueing and rendering
work; use a production benchmark for performance comparisons. Production entries
omit the optional collector and panel. Objects and undeclared closure captures
remain opaque, and request bodies are not recorded.

Source errors keep the last working application visible. Fix the source to
resume development. Compatible reloads restore supported state and native input;
when a change cannot preserve state, the development message explains the reset.

Continue with [state and identity](state.md) and
[lifecycle and requests](lifecycle.md).
