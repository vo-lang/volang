# UI architecture

The framework lives in `ui/next`. Components construct typed views; root-owned
state, reconciliation, effects, tasks and watches produce bounded wire frames.
The wire schema in `ui/next/wire.schema.json` owns the guest/host protocol.
Generated Vo and TypeScript codecs must agree on identities and bounds.

The browser host in `lang/crates/vo-web/js/ui_next` owns DOM mutations, browser
services, input/IME, focus and accessibility. It supports both page-owned and
Worker-owned VM execution. Worker execution keeps guest turns off the main
thread; the page retains DOM and service ownership. Every root must release its
listeners, requests, watches, workers and VM when closed.

`vo-ui-bridge` connects the VM to this exchange. Its optional `toolchain` feature
registers the same provider for CLI execution, SSR and AOT lowering; execution-only
Web and desktop packages omit that feature. `vo-ui-native` owns native guest
execution. `vo-ui-webview` hosts the same application in the system WebView;
`vo-ui-desktop-runtime` supplies its packaged native runtime. Framework state
and application business logic remain in Vo across Web and desktop.

`eng/ui-next` builds projects, packages the toolchain, and exercises browser,
static-page, desktop and distribution contracts. `apps/studio/next` is the
Gallery, documentation and Playground application. Its code execution and
language service use dedicated workers with bounded lifecycle ownership.

Start with [the framework design](../next/design.md) and
[authoring guide](../next/guides/first-steps.md). Acceptance declarations live in
`ui/certification.toml`; executable task definitions live in `eng/ci.toml`.
