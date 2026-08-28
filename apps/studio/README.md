# Volang Studio

Volang Studio is the first production dogfood application for Volang UI. Its
product logic, state, concurrency, editor composition, workspace, preview, and
tests are written in Volang. Browser and desktop launchers provide versioned
host capabilities; they do not own application business logic.

## Source structure

```text
app/                 product state, goroutine supervisors, and views
design/              Studio-specific semantic visual recipes
documentation/       generated mirror of canonical language and UI docs
domain/              host-independent project/editor/run vocabulary
examples/            curated editable language and UIKit starter catalog
protocol/            versioned, narrow Native/Web service interfaces
services/memory/     deterministic test and recovery host
services/host/       pure Volang JSON adapter for application host calls
entry/host/          post-commit production bootstrap entry
entry/memory/        deterministic product-test and recovery entry
host-native/         native files/compiler/VM/JIT/Git host and launcher
public/              narrow browser OPFS/compiler/preview host adapters
```

The UI framework remains in `ui/` and has no Studio dependency. Studio can use
compiler/runtime capabilities through its host protocol while reusable layout,
editor, system, accessibility, and presentation contracts stay generic.

The documentation center reads the generated Volang package in
`documentation/`. Its source catalog lives at `lang/docs/catalog.toml`;
language guides and specifications remain under `lang/docs`, while official UI
pages remain under `ui/docs`. Every rendered page shows its canonical source
path. The center projects the catalog as a collapsible section tree and gives
each article a page outline, hierarchy breadcrumb, and previous/next controls.
`vo-dev lint docs` rejects missing, undersized, malformed, or stale generated
documentation.

## Try it

From the repository root, build the current CLI and start the VM development
server:

```sh
cargo build -p vo --locked
./target/debug/vo ui dev apps/studio --open
```

Open a starter from Home, edit `main.vo`, choose **Save File**, then use **Run
VM**. UIKit starters also expose **Open Preview**. Browser projects persist in
OPFS for that origin. Run `npm --prefix lang/crates/vo-web run build` first when
the local Web runtime artifacts have not been built yet.

Use **Share** after saving to create two immutable snapshot links. The Studio
link opens the project for editing; the Runner link restores, compiles, and
mounts the application without workspace chrome. Small source projects travel
inside the link and cold-start without a server-side project database. Larger
projects receive an explicit deployment recommendation.

## Development

```sh
vo ui test apps/studio/entry/memory --mode=vm
vo ui test apps/studio/entry/memory --mode=jit
cargo run -p vo-studio-native
vo ui build apps/studio -o dist/studio-web
cargo build -p vo-studio-aot-runtime-native --release
vo ui package apps/studio -o dist/studio-native --runtime=target/release/libvo_studio_aot_runtime_native.a
```

The Web release gate builds the complete Core Wasm AOT Studio and drives a
fresh browser through project open, source editing, save, VM execution,
command dispatch, UIKit preview mounting, and stateful interaction:

```sh
vo ui build apps/studio -o target/studio-aot --runtime-dir=lang/crates/vo-web
npm --prefix lang/crates/vo-web run test:studio-aot-browser
```

`VOLANG_STUDIO_WORKSPACE` selects the project discovery root for the native
launcher. `VOLANG_STUDIO_APP` can select another compatible pure Volang Studio
entry during host development.

The in-memory host keeps product tests deterministic. The Native host uses real
disk projects, atomic saves, immutable editor overlays, the production
compiler, VM/JIT run workers, bounded output streams, and Git commands. The Web
host owns one durable OPFS catalog on the page, compiles immutable snapshots in
memory-only Workers, provisions the packaged official UI workspace with a
verified lock, and mounts previews in isolated child Islands. `product.toml`
records the native isolated-window preview and the Studio-specific Native AOT
host. `ui.desktop.toml` owns the desktop application identity and package
policy.

The Home surface exposes fourteen maintained starters. Selecting one
transactionally creates a persistent project containing the exact catalog
files, opens `main.vo`, and keeps the normal editor, analysis, save, VM/JIT,
console, and preview paths active. Project cards support open, rename, and
confirmed deletion through the same host protocol on Web and desktop. Native
projects opened outside the managed workspace use a non-destructive Forget
action that leaves their folders untouched.

Native Source Control reports branch, dirty, ahead/behind, divergence,
conflicts, staged state, and per-file unified diffs. Pull and Commit/Push have
progress, cancellation, recovery, and conflict guards. Each synchronization
runs as a bounded host session: the Volang UI receives phase updates, context
cancellation calls `remote.stop`, and the Native host terminates the active Git
child process before releasing the session. A cancelled local commit remains
visible as an ahead change and can be pushed safely on the next attempt.
GitHub identity stays
inside the platform host: Browser Studio keeps a verified token only for the
page session, while Native Studio reads the authenticated GitHub CLI identity.
The pure Volang application receives display-only account data. Repository
settings keep four authorities explicit: managed local deletion, external
catalog forget, GitHub-only deletion, and GitHub plus local removal. Cloud
deletion requires typing the exact `owner/repository`; the Native host checks
the current origin again before asking GitHub CLI to delete it.
