# Portable Web UI toolchain preview

The Web UI rewrite has an independent, movable toolchain package. It contains
the matching `vo` compiler, UI source/templates, browser VM,
project tools, and pinned build/test libraries. Applications keep ordinary Vo
source, `vo.mod`, `vo.lock`, and a vendored UI snapshot. They need no npm project.

Node.js 24 or newer is required. Build the package on its target platform from
a prepared checkout, after building the browser runtime and compiler:

```sh
node eng/ui-next/cli.mjs package target/ui-next/my-toolchain
```

The default uses the checkout's selected compiler, which normally is a debug
build for local checks. For a compact toolchain, build the optimized native CLI
from the same source and select it explicitly:

```sh
cargo build -p vo --release --no-default-features --features jit --locked
node eng/ui-next/cli.mjs package target/ui-next/my-release-toolchain --compiler target/release/vo
```

On Windows, the executable is `target/release/vo.exe`. The selected compiler must
match the framework and browser artifacts; packaging preserves its bytes and
executable mode in the inventory. `package --help` describes the arguments and
creates no files. A missing or non-executable compiler fails before staging.

The destination must be new. Packaging stages and verifies a complete inventory
before publishing that directory; existing files remain intact. This command
does not install dependencies, download browser engines, or publish a release.

Add the package's `bin` directory to PATH and use its compiler:

```sh
vo ui verify
vo ui create my-app --template pages
vo ui dev --project my-app
vo ui check --project my-app
vo ui doctor --project my-app
vo ui build --project my-app
vo ui preview --project my-app
vo ui test --project my-app
```

The matching toolkit provides these complete project templates. Each includes
its source, styles and public browser tests; a repository checkout is unnecessary.

| Template | Application |
| --- | --- |
| `default` | An interactive starter with a name and counter |
| `pages` | Separate home and notebook programs with static HTML |
| `fieldnotes` | A reading library with a request-time server and settings |
| `listening` | Audio playback and a local note |
| `canvas` | A landscape calculated as a Vo bitmap |
| `plot` | A growing-plant journal with an optional chart and native data table |
| `scroll-position` | A landscape that remembers its scroll position |
| `variable-list` | A measured list that preserves its anchor and active editor |

For example, `vo ui create my-garden --template plot` creates the chart application
with its optional feature already selected. The common `check`, `dev`, `build`,
`preview` and `test` commands work immediately on every template.

Compiler diagnostics identify the source file, line, column, severity and error
code. Compilation failures list errors before warnings and count them separately;
imports used only in type declarations count as used. During development, a failed
edit keeps the last working page interactive. Fixing the source clears the message
and applies the next successful build. A newly opened tab receives the current
diagnostic too.

`check` validates source entries, document configuration and host imports without
running prerendering or creating a distribution. Run `build` before `preview`.
`doctor` diagnoses configuration and installation issues without changing the
project; `--target desktop` checks the native SDK and `--json` emits a structured
report. See [project diagnosis](diagnosis.md).

The original `default`, `pages`, `fieldnotes`, and `listening` templates cover a small
interactive page, separate page programs, an application with a request-time
server, and native media playback. The listening room includes an original
recording, server HTML, browser controls, theme changes and a local note. Its
player keeps playback through activation and ordinary updates. The
`features: ["editor"]` option uses the packaged editor dependency and retains its
licenses in the application distribution. Basic pages omit that dependency.
The optional `features: ["canvas"]` installs the [Canvas bitmap pack](web/canvas/README.md)
without adding a third-party library. `features: ["plot"]` installs the
[optional line chart](web/plot/README.md), loading its pinned library and original
stylesheet when a chart mounts. All three names may be declared together; omitted
packs stay outside an application's host bundle. Explicit host providers retain
their normal override precedence.

Web projects use Wasm VM in development and production. `defaultBackend` may be
omitted or set to `"vm"`. The generated entry loads `app.vob` with a matching
execution runtime.

The HTML slot `<meta name="ui-next-backend" content="<!--ui-next:backend-->">`
resolves to `vm`. This applies to static pages and native SSR.

`vo ui` discovers tools beside the installed executable. Current-directory and
positional commands select a project; `--project` selects one explicitly.
Projects declare their configuration in `ui-next.json`. Command help is available
without installing the tools or Node.
Set
`VO_UI_TOOLCHAIN` to a complete package directory to select another installation;
set `VO_UI_NODE` to a Node executable if it is absent from PATH. Direct
`node /path/to/toolchain/ui.mjs ...` commands use the same implementation. The
checkout's `eng/ui-next/cli.mjs` remains available for framework development.

Browser testing uses the packaged Playwright runner. Install its browser engines
with `vo ui browsers install`, optionally naming `chromium`, `firefox`, or
`webkit`; an existing matching `PLAYWRIGHT_BROWSERS_PATH` is also supported. The
engines live outside the package. Tests preserve production artifact identity,
reports, and failure traces under the project's `target/ui-next/browser-tests`.
Generated fixtures use the current runner, so moving a project or toolkit does
not require editing a saved source path.

`tools/toolchain.json` records platform, architecture, Node requirement, UI wire,
private server protocol, resource paths, dependency versions, and every file's
size/SHA-256. `verify` checks missing, changed, extra, and linked files. Startup
validates the package layout; it does not rehash the entire compiler on every
command. A missing or incompatible manifest has an explicit diagnostic. Keep
the compiler, framework, browser host and tools together when upgrading; project
builds consume their lockfile without rewriting it.

Native `vo ui run/package` commands use the optional [desktop SDK](desktop.md).
The same package can carry Web tools and native launch/link artifacts; desktop
applications run independently after building.

The package builder selects its current platform; cross-platform packaging
requires building and testing on that target. Release publication follows
[the release policy](../docs/release-policy.md), with verified CI evidence for
the source commit and every required platform.
