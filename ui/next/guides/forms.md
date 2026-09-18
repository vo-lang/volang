# Build a settings form

This complete component demonstrates validation, typed controls and local state.
Save records the local UI state; it does not persist settings to a server or disk.
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

## Verify the behavior

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
