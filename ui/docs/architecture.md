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

Studio's static export uses the native Vo renderer for all page HTML. Exported
Docs pages, including the default landing page, have a single lightweight
browser owner for search, clipboard and theme controls; they omit the guest snapshot and runtime boot. Their chapter
links use native document navigation. Gallery and Playground retain the Vo
application and its hydration contracts, and navigate to exported Docs through
native links. The Node and development hosts continue exercising Vo Docs and
nested routing. Do not attach the content entry and the VM to the same root.

Interactive static pages preload their build-derived synchronous module graph
and core runtime resources. The Studio Worker bundles its bindings and host;
compiler and editor imports remain lazy. The content script graph is limited to
20 KiB gzip by the build. `node eng/ui-next/studio-startup-benchmark.mjs` measures
10 cold content samples at 1 Mbps/200 ms and 10 cold Gallery samples at
10 Mbps/100 ms, plus 10 cold root-to-chapter navigations at 1 Mbps/200 ms.
It checks actual control readiness and duplicate fetches.
It writes `target/ui-next/startup-benchmark.json`; this local delivery model
excludes DNS, TLS, packet loss and variability of public networks.
