# Move a legacy Web screen to the new component API

Start a new project with `vo ui create my-app`. Keep its generated development,
production and prerender entrypoints, and replace `app/app.vo` with the migrated
component. `vo ui check --project my-app` compiles the real application paths;
`vo ui test --project my-app` exercises its browser behavior.

The old public source packages remain available for compatibility and native
migration. The replacement uses `github.com/vo-lang/ui/next` with preview API
stability. Migrate a screen's state and lifecycle together, then test its user
interactions before changing an existing deployment.

## Project and command migration

Create the replacement beside the existing project. Copy domain logic and assets,
then move one complete screen at a time. The generated `vo.mod`, vendored UI source
and entrypoints belong to the new project; adding `ui-next.json` to a legacy
project alone cannot convert its component model. Keep its authenticated
`vo.lock` consistent through the normal module commands. Builds and checks do not
rewrite that lockfile or substitute another UI package.

| Compatibility command/configuration | Replacement project |
| --- | --- |
| `vo ui new` and legacy templates | `vo ui create`; choose one of the delivered templates |
| `ui.web.toml`, `public/` | `ui-next.json`, `web/index.html`, `web/boot.js` and `web/` assets |
| `vo ui dev`, `build`, `test` without a new manifest | Same command in the new project, or explicit `--project <directory>` |
| `vo ui doctor --format=json` | `vo ui doctor --json`; add `--target desktop` to check native prerequisites |
| `vo ui inspect` | New development inspector, component props and owned-request diagnostics |
| `vo ui run --mode=vm|jit` | `vo ui run --backend vm|jit|aot`, with the matching desktop SDK |
| `ui.desktop.toml`, old native package runtime | `desktop` in `ui-next.json`, then `vo ui package` |
| Saved native/headless snapshots and input scripts | New project browser tests plus native window checks for desktop delivery |

The manifest selects these tools. `vo ui check` checks all declared entrypoints,
HTML and host imports without executing guest code or writing build outputs.
Run `vo ui build` explicitly when a workflow needs prerendered pages or bytecode.
The new `test` command runs browser interactions, so migrate assertions about
labels, focus, form behavior and state rather than copying old renderer snapshots.

Web applications execute with Wasm VM. Remove `"backend": "aot"` from old preview
Web configuration, or set it to `"vm"`. Native VM, JIT and Native AOT remain
available. Web and desktop builds share component code, with platform services
declared in the authored boot. Desktop uses a single system WebView window;
HTTP(S) links open in the system browser. See the [desktop guide](../desktop.md)
for bundle layout, prerequisites and current platform evidence.

### Desktop preview storage migration

The application manifest advances from v1 to v2 to carry a stable application
identifier. The desktop SDK uses v3 to carry authenticated native import libraries
needed for standalone Windows AOT linking. Rebuild the matching SDK/toolchain and repackage each
application. Set `desktop.identifier` once in `ui-next.json` (for example
`dev.example.my-app`); new single-application templates generate a unique value.
Retain it across updates and relocation. Give independent applications different
identifiers. macOS persistent profiles require macOS 14 or newer.

Earlier desktop previews used platform default browser profiles. Export or copy
important drafts from the old application before replacing it. The new identified
profile starts empty and does not delete or automatically import old storage.
Existing old bundles keep their previous runtime and profile behavior. Web-origin
storage profiles are unchanged by this desktop migration. New desktop profiles
keep localStorage and IndexedDB independently of the executable's installation
path. For acknowledged saves, use `createPersistentStorage` from `@volang/ui-next`:
its writes resolve after an IndexedDB transaction commits. Browser localStorage
can flush later than `setItem` returns and can lose the last write on immediate
process exit. Studio now uses committed transactions and imports an old draft
only when its new key is absent; it leaves the original localStorage value intact.

| Previous pattern | Replacement |
| --- | --- |
| `ui.Mount(App)` | Generated project entrypoints own `host.Run`, development and SSR |
| `func App() ui.View` with implicit component state | Define a component once and render it with an explicit `*ui.Scope` |
| `ui.UseStringState` and `ui.StringStateValue` | `ui.String(scope, "name", initial)` and `name.Get()` |
| `ui.SetStringState` and `ui.SetBoolState` | `state.Set(value)` from handlers or post-commit work |
| `event.Text` for native text input | `event.Value`, or a typed control callback |
| Positional theme/control arguments | Named kit props and `.Class`/scoped styles |
| Background work owned by the whole application | `Effect`, `Watch`, `Start` and resources owned by the declaring scope |

## Package and ownership migration

| Existing package or responsibility | Replacement and ownership |
| --- | --- |
| `ui`, implicit hooks and mounted handles | `ui/next`: explicit component definitions, keys, scopes, state and refs |
| `ui/kit`, commands, gestures and theme | `next/kit`, semantic HTML, typed events and scoped CSS; shortcuts stay with the owning view |
| `ui/task`, `resource` and application cache | Scoped `Effect`, `Start`, `Resource`, `Watch` and optional `next/data`; all publications return to the root writer |
| `ui/forms` | `next/forms`, native forms and typed kit controls; retain native names, labels and submit order |
| `ui/navigation`, route loaders | `next/navigation`, optional `next/data` and managed route links |
| `ui/document`, chart, editor and simple graphics | `next/document`, optional `next/web/plot`, `next/web/editor` and `next/web/canvas` |
| `ui/media`, animation and motion | Native media elements, CSS animation, `Presence` and scoped completion/activity subscriptions |
| Persistence, platform and custom JavaScript integration | Explicit boot services or scoped widgets; release listeners, observers and subscriptions on cancellation |
| Workspace, Git, accounts and remote repository UI | Removed from the new Studio product; export old projects before moving |

Migration changes the component and ownership model as well as imports. Keep mutable
collections in state snapshots, use stable item keys, and replace snapshots when
updating them. Create component definitions once. State/effect/task keys stay
stable within their owner. Each effect captures the handles needed for cleanup;
cleanup must not read disposed component state. Stale asynchronous completions
cannot publish after cancellation. SSR render functions must remain replayable:
move network, persistence and platform work into owned requests or effects.

Professional media editing, old workspace services, native multi-window control
and renderer-specific drawing APIs have no automatic replacement. Keep such
consumers on the compatibility implementation until their actual requirements
are implemented and validated. Basic media, Canvas and plot projects have public
templates and browser tests. The package API guides describe their supported
operations and disposal contracts.

## Compatibility and removal schedule

Legacy commands identify the compatibility path on stderr while keeping stdout
available for machine-readable results. The old stable source APIs continue to
compile; their removal has no release date. This migration guide and the preview
do not retroactively start a published deprecation period.

A release that deprecates a stable API must name its working replacement in
diagnostics and release notes, preserve it for at least one minor release, and
remove it only in an eligible major release. Before that removal, all retained
consumers and required Web/Linux/macOS/Windows acceptance must pass against the
same candidate. A local preview receipt cannot replace product certification.
See the [release policy](../../docs/release-policy.md).

The new Studio uses separate drafts and retains a legacy project export route.
Its old service-worker retirement preserves OPFS and user data. Save edits in
old tabs before refreshing, export important projects, and confirm the new
Gallery, Docs and Playground before changing the deployment. Keep old recovery
routes and the retirement worker available for returning users.

## A complete settings component

This replaces the local state behavior of the former settings example. Save
records the local UI state; it does not persist settings to a server or disk.
To add persistence, start an owned request and publish success from its result.

```vo
package app

import (
	ui "github.com/vo-lang/ui/next"
	"github.com/vo-lang/ui/next/kit"
)

var settingsType = ui.Define("settings.Form")

func View(initial string) ui.View {
	return ui.Component(settingsType, func(scope *ui.Scope) ui.View {
		name := ui.String(scope, "name", "Volang")
		notifications := ui.Bool(scope, "notifications", true)
		saved := ui.Bool(scope, "saved", false)
		nameError := ""
		if name.Get() == "" {
			nameError = "Display name is required"
		}
		status := "Changes have not been saved"
		if saved.Get() {
			status = "Settings saved"
		}
		input := ui.Element("input").Attr("value", name.Get()).Attr("name", "display-name").On("input", func(event ui.Event) {
			name.Set(event.Value)
			saved.Set(false)
		})
		return ui.Element("main", ui.Element("h1", ui.Text("Settings")), ui.Element("p", ui.Text(status)).Attr("role", "status"), kit.FormField(kit.FieldProps{
			ID: "settings-name", Label: "Display name", Error: nameError,
		}, input), kit.Switch(kit.SwitchProps{
			ID: "settings-notifications", Label: "Notifications", Checked: notifications.Get,
			OnChange: func(value bool) {
				notifications.Set(value)
				saved.Set(false)
			},
		}), kit.Button(kit.ButtonProps{
			Disabled: saved.Get() || nameError != "",
			OnPress: func() {
				if name.Get() != "" {
					saved.Set(true)
				}
			},
		}, ui.Text("Save"))).Class("vui", "starter")
	})
}
```

Define `settingsType` once. Separate mounted instances have separate state even
when they share that definition. State names stay local to their component;
movable siblings also need stable keys based on item identity. Removing a
component disposes its owned effects, tasks and subscriptions.

Keep state reads live in callbacks that depend on current state. The save action
rechecks the current name, including input queued before the next DOM commit.
The switch uses its typed boolean callback; the text input reads the native value.
FormField owns label, error description and invalid-state attributes.

IDs are document-wide. These IDs work for one settings screen; when repeating the
screen or embedding several roots, pass a distinct ID prefix through props or
initial data. Use the same prefix for SSR and client activation. Component keys
preserve instance identity and do not replace HTML IDs.

## Verify the behavior before switching

Check initial values and label associations, Unicode typing, empty-name errors,
save enablement, switch updates and resetting the saved indicator after an edit.
Delay startup once and type into the server HTML while the application is loading.
After the host starts, declared events queue until the application can process
them; input followed by Save must preserve that order. Before the host script
loads, native control values survive, and normal links and forms keep their
browser behavior. Run the project's Wasm VM browser tests in all three engines.

For external JavaScript hosts, use `vo-web/ui/next` and the package embedding
guide. Each mount owns an Island and its providers. Multiple mounts share module
initialization, while closing one root leaves the others running.

The portable-toolchain regression extracts this exact program from the delivered
guide, builds it with the public project commands and runs its interaction tests
with the Wasm VM in Chromium, Firefox and WebKit.
