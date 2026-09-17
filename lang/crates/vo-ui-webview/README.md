# System WebView adapter

This experimental P7 adapter joins the canonical DOM host in
`vo-web/js/ui_next/desktop.ts` to `vo-ui-native::executor`. Vo owns components,
state and reconciliation; the JavaScript entry owns only transport and the
existing renderer. The main thread owns the window and system WebView. Its VM
factory, native execution, I/O polling and VM disposal run on a dedicated thread.
The window adapter has no `vo-engine`, Vo codegen or previous UI kernel
dependency. Its Native AOT build also excludes the Cranelift compiler; JIT builds
enable that compiler explicitly. The shared native stdlib still brings the
module resolver and `vo-analysis`.

The default feature set validates resources and IPC without requiring desktop
system libraries. `window` enables Wry/Tao, `jit` enables strict native JIT, and
`aot` enables a linked static native image. Application callers provide a
verified, loaded, unstarted VM with `vo-ui-bridge` already registered; compiler
and engine composition remain outside this crate.

## Window and resource ownership

`Assets` validates an immutable bundle before creating a window: at most 1,024
files, 16 MiB per source asset and 64 MiB total, excluding the small generated
bootstrap. Its HTML contains exactly one `BOOTSTRAP_MARKER`; its supplied host
script is built from the canonical framework entry. The custom protocol reads
only this bundle. Paths have canonical ASCII segments and reject traversal,
encoded aliases, duplicates and reserved entrypoints. Each document receives
its own random bootstrap identity, removed from the DOM after the host starts.

The IPC parser checks that identity, an exact monotonically increasing safe
integer sequence, envelope/frame limits and a single pending reply. The native
executor independently validates the opaque session capability. IPC ingress and
executor egress use fixed-capacity mailboxes with coalesced notifications.
The application document stays within its origin. HTTP(S) navigation and popup
links are handed to the system browser by a bounded worker; other external
schemes are rejected. The callback is replaceable or disabled by embedders,
and closing discards pending launches. Additional native windows are unavailable.

Closing asks the canonical host to cancel resources and run normal guest
cleanup. A configurable deadline then requests interruption. The window thread
never waits for an unresponsive VM or foreign call. Fatal guest diagnostics are
kept visible until the user closes the window; embedding probes may select
immediate error return. macOS installs standard editing shortcuts and routes
Command-Q through the same orderly close path. Clipboard access is enabled on
the system WebView, including Linux and Windows.

The bundled-resource protocol serves exact byte ranges with `206`,
`Content-Range` and `Content-Length` so native media can seek forward and back.
HEAD keeps the full resource length without a body; unsatisfiable ranges return
`416`. Unsupported range forms use the complete resource. Linux's GStreamer
backend additionally uses native data URLs for bounded bundled media, because it
cannot read the application's custom scheme.

`WindowOptions.application_id` selects an application-specific persistent browser
profile. `ApplicationId` validates the portable identifier and derives stable,
versioned storage keys; the identifier must survive repackaging and relocation.
Windows/Linux keep the profile below the user's local data directory. macOS uses
a dedicated website data store and rejects persistent windows before macOS 14.
An omitted identity selects ephemeral storage. The `WebContext` outlives its
WebView; no application profile is removed on window close.

`run` owns one application's main event loop, including the macOS menu. It is
not a nested window-loop API for an already running native application. The
returning event loop permits the embedding/AOT caller to dispose its window
resources without calling process exit.

## Local checks and development entry

From the repository root:

```sh
VOWORK=off cargo test -p vo-ui-native --features jit --locked
VOWORK=off cargo test -p vo-ui-webview --locked
VO_TEST_PROFILE=release node eng/ui-next/desktop-build.mjs
node eng/ui-next/desktop-contracts.mjs
node eng/ui-next/desktop-browser-contracts.mjs
target/ui-next/desktop/preview vm target/ui-next/desktop/interaction.vob target/ui-next/desktop/manual
```

The build needs the repository Node dependencies and platform system WebView
development libraries. On Linux this includes WebKitGTK 4.1/GTK 3; Windows uses
WebView2, and macOS uses the system WebKit. Dependencies are exact-pinned and
Cargo operations use the lockfile. The build's isolated Unix Native AOT fixture
consumes `rustc --print native-static-libs` and real CLI-generated objects.
The production desktop SDK owns Windows Native AOT packaging and MSVC linking;
the older development fixture above only links Native AOT on Unix.
The `preview` example compiles/loads through `vo-engine` on the worker thread;
that development-only dependency does not enter the window runtime.

The system-window probe exercises VM/JIT/Native AOT interaction and fatal guest
diagnostics. It deliberately records DOM/state conformance separately from
paint, physical input, real IME and assistive-technology acceptance. The fixture
assets and debug executables are not production installation packages or size
measurements. The `vo-ui-desktop-runtime` distribution crate now composes verified bytecode
loading, resource manifests and the Native AOT process entry. Project
`vo ui run/package` commands consume a separately built desktop SDK; see the
[desktop guide](../../../../ui/next/desktop.md). Studio desktop assembly uses the
same package path; macOS VM/JIT/Native AOT have local window evidence. Actual
Linux/Windows, physical input and assistive-technology acceptance remain separate.

`desktop-package-contracts.mjs` also builds the development preview and exercises
real-window external navigation, popup links and continued application
interaction. Its `--check-shell` callback records hand-offs without opening a
browser; that test callback is absent from production SDK launchers. Rust tests
cover URL normalization, scheme/size limits, queue capacity and close cancellation.

The default Rust lane covers resource/IPC contracts and the independent native
JIT session. The macOS platform lane additionally compiles the actual window
feature and runs its Rust contracts. System-window interaction receipts remain
separate from these unit checks and are not inferred from CI declarations.
