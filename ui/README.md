# Volang UI

Build Web interfaces with typed Volang components, owned state and native HTML.
The new framework lives in [`next/`](next/README.md). Vo owns rendering decisions,
reactivity and lifecycle; a small browser adapter applies DOM and platform work.
The same application runs on the Wasm VM.

The Web rewrite is a preview. It has real application, browser and distribution
checks; device input, assistive technology, performance and migration acceptance
remain in [the plan](../docs/ui-platform-rewrite-plan-20260913.md). The
[desktop preview](next/desktop.md) shares the same application with native
VM/JIT/Native AOT and standalone packaging. The previous implementation retains its own
[compatibility overview](legacy.md) and certification declarations.

## Start a Web application

Use a matching installation containing the Web UI tools and Node.js 24 or newer:

```sh
vo ui verify
vo ui create hello-ui
vo ui dev --project hello-ui
```

Follow [First steps](next/guides/first-steps.md) for a complete application, then
read [State & identity](next/guides/state.md) and
[Lifecycle & requests](next/guides/lifecycle.md). Applications keep ordinary
`.vo` files, `vo.mod`, `vo.lock` and their exact framework snapshot. They need no
npm application manifest or JavaScript components.

```sh
vo ui check --project hello-ui
vo ui browsers install
vo ui test --project hello-ui
vo ui build --project hello-ui
vo ui preview --project hello-ui
```

The Web build includes initial HTML and the Wasm VM runtime. Public tests exercise
the built application in Chromium, Firefox and WebKit. The
[toolchain guide](next/toolchain.md) covers installation, templates and movable
packages; [testing](next/testing.md) covers application tests and reports.

## Build on the framework

| Task | Package or guide |
| --- | --- |
| Components, state, effects and native HTML | [`ui/next`](next/README.md) |
| Controls, themes and custom content | [`next/kit`](next/kit/README.md), [styling](next/styling.md) |
| Forms and validation | [`next/forms`](next/forms/README.md), [schema issues](next/forms/schema/README.md) |
| Routes, cached data and server HTML | [`next/navigation`](next/navigation/README.md), [`next/data`](next/data/README.md), [`next/server`](next/server/README.md) |
| Large collections | [`next/collection`](next/collection/README.md) |
| Platform extensions and optional tools | [`next/web`](next/web/README.md), [editor](next/web/editor/README.md), [Canvas](next/web/canvas/README.md) |
| Development and diagnosis | [project checks](next/diagnosis.md), [reload](next/develop/README.md), [inspection](next/inspect/README.md) |

[Studio](../apps/studio/next/README.md) demonstrates Gallery, Docs and Playground.
It uses the framework's public components, navigation and data packages. Its
static distribution owns the default CI site candidate. The
[performance report](next/performance.md) records measured costs and remaining
limits; functional browser checks do not establish performance or certification.

## Compatibility and contribution

Existing `github.com/vo-lang/ui` APIs, positional UI commands and native consumers
remain available through the [migration period](next/migration.md). New Web
projects use `github.com/vo-lang/ui/next` and explicit `--project` commands.
The [release policy](docs/release-policy.md) defines stable API compatibility.

Read the [architecture](docs/architecture.md), [framework development guide](next/README.md#reproduce)
and [CI guide](../docs/ci.md) when contributing. The existing E0–E8 declarations
describe the compatibility implementation. Its `ui-certify --check` result does
not certify the replacement or establish real-platform evidence.
