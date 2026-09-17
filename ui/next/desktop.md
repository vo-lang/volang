# Native desktop preview

The same Vo application, HTML, styles, host services and optional widgets can run
with a native VM, JIT or Native AOT behind the system WebView. The browser host
continues to own DOM, input and accessibility semantics. Application state and
rendering run on the native session's owner thread.

Install the matching UI toolchain **with its desktop SDK**, then inside a project:

```sh
vo ui run
vo ui run --backend vm
vo ui package
vo ui package --backend jit
```

`run` builds and opens a JIT application. `package` defaults to Native AOT;
`--backend vm|jit|aot` selects the execution mode. Directory arguments and
`--project <directory>` also work. Native AOT linking requires the platform C
linker on the build machine: Xcode command-line tools on macOS, a C toolchain and
WebKitGTK development libraries on Linux, or Visual Studio C++ Build Tools and
the Windows SDK on Windows. The compiler discovers MSVC's linker and library
environment automatically. SDKs include Native AOT on all three platforms;
macOS has local application evidence, while Linux/Windows execution awaits their
hosted CI runs. Windows applications use the GUI subsystem without an extra
console window.
Web execution continues to use Wasm VM.

Outputs are under `target/ui-desktop/dist-<backend>`. macOS produces an unsigned
`Application.app`; launch it normally or run `Contents/MacOS/application` for
terminal diagnostics. Linux and Windows produce a portable directory containing
`application`/`application.exe` and `resources/`. Move the whole application as one
unit. Execution needs the system WebView, and needs no Node, Cargo, compiler,
source checkout or local web server. Linux requires WebKitGTK 4.1/GTK 3; Windows
requires WebView2. Persistent macOS applications require macOS 14 or newer. Signing, notarization and system installer creation are release
steps outside this preview command.

Desktop packaging requires a stable `desktop.identifier` in `ui-next.json`.
Single-application templates generate a unique identifier once at creation;
keep it in source control. The object also accepts:

```json
{
  "desktop": {
    "title": "My application",
    "identifier": "dev.example.my-application",
    "width": 1080,
    "height": 800,
    "entry": ".",
    "data": ""
  }
}
```

`entry` names a Vo package inside the project. `data` supplies the same initial
page data used by the authored boot. Defaults reuse the ordinary application
entry and document title. Use a distinct identifier for each independent application;
keep it unchanged across versions, backend changes and installation moves.
Server and multi-page projects must declare their desktop entry explicitly and
adapt any server-only services; this command does not embed the Web server.

The identity selects a persistent browser profile, including localStorage and
IndexedDB. On Windows and Linux it lives below the user's local data directory,
under `Volang/UI/<identity-hash>`, independently of the installed executable.
macOS selects a dedicated WebKit website data store by the same stable identity.
Changing the identifier creates a separate profile; it never imports or deletes
another application's data. Sharing an identifier deliberately shares its profile.
Embedders that omit `WindowOptions.application_id` use an ephemeral WebView.

The desktop SDK uses format v3, with authenticated native import libraries; the
application manifest uses v2. Rebuild the matching SDK/toolchain and repackage
applications together; older SDK formats are rejected. Earlier preview bundles
used platform default storage with no application identity. Export important drafts
from the old application before upgrading. The new profile starts empty and leaves
old storage untouched; see [migration notes](guides/migration.md).

For saved drafts and preferences, import `createPersistentStorage` from
`@volang/ui-next` in the authored boot and pass the owned task's cancellation
signal to `get`, `set` or `remove`. The same optional adapter works on Web and
desktop. It uses IndexedDB transactions with strict durability; `set` resolves
after the transaction commits, including when the window immediately closes.
Cancelled operations abort uncommitted writes and release their connections.

```js
import {createPersistentStorage} from '@volang/ui-next';
const drafts = createPersistentStorage('my-application.drafts.v1');
const tasks = {
  'draft.read': async (_value, signal) => (await drafts.get('current', signal)) ?? '',
  'draft.write': async (value, signal) => { await drafts.set('current', value, signal); return ''; },
};
```

The system's `localStorage` follows its browser's asynchronous disk-flush policy;
a completed `setItem` does not acknowledge durable storage before process exit.
Studio uses the transaction adapter and imports existing localStorage drafts only
when the corresponding committed key is absent. Existing browser profiles and
legacy draft values remain available.

The build reuses `web/index.html` and its canonical
`<script type="module" src="<!--ui-next:assets-->assets/app.js"></script>` slot.
It bundles the existing `web/boot.js` against the native mount adapter, retaining
its service factories. `loadVm` and Web bytecode loading are owned by the native
launcher. Only one application root can mount in the desktop document. The
`plot`, `canvas` and `editor` features use the same optional providers as Web.
The native entry preserves JavaScript module semantics, including `import.meta.url`
and asynchronous startup. Static resources use canonical ASCII paths and supported
HTML/JS/CSS/JSON/text/image/font/audio/video/Wasm types. The build rejects unsupported
resources with their file name.

The native navigation adapter shares history, query updates and scroll restoration
with Web. It accepts paths within the application's fixed asset authority. Use
managed route links for application navigation. A full document load ends the
native session; reopen the application to restart it. Leaving the document
releases pending work, and a terminated macOS rendering process closes its shell.

HTTP(S) links, including `target="_blank"`, open in the system browser while
the application retains its current document and state. The shell accepts a
bounded URL and queues at most eight pending browser launches outside the GUI
thread. Closing cancels launches that have not started. Other external schemes
and additional native windows are unavailable. Embedders can replace or disable
the handler through `WindowOptions.open_external`.
Linux browser hand-off also requires `xdg-open` from the system's `xdg-utils`
package and a configured default browser.

Native editing uses the system WebView clipboard capability; macOS also installs
the standard editing menu and shortcuts. Actual keyboard, IME and assistive
technology behavior still requires the platform acceptance described below.

Builds stage all resources and verify the executable before replacing an earlier
output. `desktop.json` records each resource's size and SHA-256; the launcher
checks bounds, identities, backend and bytecode before creating a window.
`application --check` verifies an existing bundle without opening a window;
`--diagnostics` additionally prints native execution statistics on exit. Automated
window checks can use `--exit-on-failure` to return a nonzero status immediately.
Resource failures produce a terminal diagnostic; running application failures
remain visible in the window. The report records the build profile: local debug
SDKs must not be used to make production size or startup claims.

## Building the SDK

Distributors build the native SDK once, then include it in the ordinary portable
toolchain. Applications never build Rust dependencies themselves.

```sh
node eng/ui-next/desktop-sdk.mjs target/ui-next/desktop-sdk
node eng/ui-next/cli.mjs package /new/toolchain --desktop target/ui-next/desktop-sdk
```

The SDK builder defaults to `release-native`. `--profile dev` is available for
local contract checks. It uses locked dependencies and Rust's emitted native
link requirements. The Native AOT dependency graph excludes the JIT compiler and
previous UI runtime. SDK resources join the toolchain's existing per-file
inventory and `vo ui verify` contract. Web-only toolchains remain valid.
The builder uses Cargo's local dependency cache; run `cargo fetch --locked` when
provisioning a new distributor machine. Checkout drivers can select a prepared SDK
with `VO_UI_DESKTOP_SDK`; installed toolchains use their authenticated SDK path.
The SDK's `.a`/`.lib` runtime and system library arguments are passed to the
compiler's native linker; project tooling does not implement a second platform
linker or require a developer command prompt.

`eng/ui-next/desktop-package-contracts.mjs` builds a complete toolkit, moves it,
creates projects through the public CLI, packages VM/JIT/AOT applications, and
moves/runs them independently. It verifies storage persistence after repackaging
and relocation, and isolation between application identities. It also checks corruption diagnostics, failed
build preservation, optional pack mounting and actual system WebView DOM
interaction. Headless browser tests cover the shared host's failure and close
paths. These checks do not establish physical input, paint, real IME or assistive
technology acceptance. Cross-platform installer validation remains tracked
separately in the rewrite plan.
The three `ui-desktop-rewrite-*` CI tasks run these checks and the complete Studio
backend matrix. See [desktop CI](../../docs/ci.md) for prerequisites and receipts.

## Studio

`node eng/ui-next/studio-desktop.mjs --backend jit` assembles Studio using the same
project packager. Select `vm` or `aot` explicitly for the other native backends.
The main UI runs natively; the optional Playground compiler and examples run in
cancellable Wasm VM workers. The bundle includes the 23 generated chapters, editor
and preview assets and runs without a local server. Build the matching Web runtime,
compiler and generated documents before assembling it from a checkout.

`node eng/ui-next/studio-desktop-contracts.mjs` builds test-only variants and checks
navigation, offline documentation, editor enhancement, draft saving, compile errors,
run cancellation and interactive UI previews in actual system windows. An external
deadline also catches document/renderer failures that prevent in-page diagnostics.
Test builds go to `target/ui-next/studio-desktop/check-<backend>`. Builds without
`--check` go to `dist-<backend>` and open a normal, persistent Studio window;
running acceptance checks never replaces that application.
