# New Studio

The initial Web consumer of `ui/next`. Application state and views live in Vo;
the browser entry installs navigation, local-draft and worker-run services.
This is the preferred Web development entry and the default CI site candidate.
Hosted publication and platform certification remain tracked in the main plan.
The [desktop assembler](../../../ui/next/desktop.md) delivers this same application
with native VM/JIT/Native AOT, offline documentation and optional Wasm Playground
workers. Use `node eng/ui-next/studio-desktop.mjs --backend jit` after preparing
the desktop SDK and Web runtime/compiler assets.

The footer links to `/studio/recover`, a small read-only export page for projects
saved by the previous browser Studio. It opens storage only after **Find browser
projects**. The ZIP library loads only after **Prepare download**; the final native
download link remains available until cancellation, project replacement, navigation
away or development reload. The new execution runtime does not initialize the compatibility runtime's persistent browser filesystem.

- **Gallery** uses the new scoped kit recipes and native controls; local counter,
  input, switch, theme, filtering and code disclosure have real behavior. Dialog,
  Tabs, Accordion, Popover, Menu and the reusable form package exercise focus,
  keyboard and async lifetimes. Combobox runs inside a Dialog and participates in
  the same form validation, reset and submission flow as ordinary inputs.
  Checkbox, RadioGroup and Select demonstrate native required validation,
  keyboard selection, disabled options and browser-owned form reset defaults.
  Slider, Progress, Spinner and Alert demonstrate native range interaction,
  determinate/loading states, scoped work and dismissible feedback.
  Listbox browses 1,000 thoughts with a virtual window, keyboard navigation and
  explicit selection while keeping the active option available to accessibility APIs.
  Table and Pagination sort and browse a small idea library, keeping editable
  notes with their records across page changes.
  Tooltip describes an action on hover/focus and also runs inside Quick settings,
  retaining the trigger's existing action and descriptions.
  AlertDialog confirms a fresh start inside another modal, initially focusing
  the cancel action and retaining the user's name when cancelled.
  ToastRegion queues notifications, demonstrates a persistent Undo action, and
  pauses temporary notes while they are hovered, focused or in an inactive page.
  Presence fades an editable thought, retains it when reopened during exit and
  starts fresh after completed dismissal. Reduced-motion settings disable the fade.
- **Docs** provides 23 maintained chapters: four Web UI guides and 19 language,
  toolchain and reference chapters, all generated from the canonical catalog.
  Search matches words in titles, summaries, sections and body text, including code.
  Its separate index loads on the first nonempty query and is reused across edits
  and chapter navigation. A failed index keeps title filtering and a local retry.
  Each chapter body loads separately and shares a bounded cache across chapter
  navigation. Errors and invalid documents have local retry; leaving Docs cancels
  its requests and releases its cache. Previous `?topic=` links remain usable.
  Breadcrumb navigation identifies the current chapter and retains layout state
  when following a parent link.
- **Playground** edits and runs standard-library Vo examples. Each run gets an
  isolated worker. Stop, timeout and route disposal terminate it, including an
  infinite loop. Compilation errors and output return to the same UI writer.
  The compiler downloads on the first run or explicit code-information query.
  The [source editor](../../../ui/next/web/editor/README.md) adds Vo lexical
  highlighting, line numbers, search and undo through an optional CodeMirror
  chunk. Gallery/Docs omit it. A focused native textarea keeps the current input
  session when the library arrives; enhancement follows a natural blur. Failed
  loading leaves native editing, saved drafts and Run available.
  In the enhanced editor, Ctrl+Space requests compiler-backed completion and F12
  selects a local definition or opens a read-only imported definition. Both console
  and UI programs use the compiler's existing module and type rules. A separate
  lazy Worker owns immutable source snapshots; source edits, selection changes,
  blur and disposal cancel pending queries. Requests never run the draft. A failed
  query keeps editing available and can be retried with the same shortcut.
  Semantic shortcut declarations belong to the enhanced surface; the native
  fallback continues to declare its run shortcuts only.
  Restored and replaced drafts normalize CRLF and bare CR to LF to match native
  source controls. Diagnostics and semantic queries use that exact stored text.
  Successful compiler warnings remain selectable alongside visible program output
  or preview status, including subsequent runtime failures. Compiler errors keep
  their full message in a disclosure below the problem list.
  Its curated selector offers six console programs and three UI examples. Selecting
  a title leaves the editor alone; Open example stops current work and opens the
  chosen source. Restore previous draft recovers the text replaced by the last
  open/reset action, including an empty draft. The previous draft survives a
  compatible development reload and belongs to the current editor route.
  **UI preview** at `/studio/playground/ui` edits and compiles a single-file
  component against the packaged experimental UI module. A dedicated Worker
  executes the preview VM; the containing document remains available to stop a
  stuck event handler. Each run starts fresh. Errors retain the edited source,
  and rerun/Stop/route disposal release the old frame and Worker. A separate
  preview document keeps CSS, IDs and modal focus local and follows Studio's
  theme. UI drafts use their own storage key; console drafts remain independent.

The new source does not use the old Studio application, account or Git services.
It stores console and UI drafts at `volang.studio.next.draft.v1` and
`volang.studio.next.ui-draft.v1` in localStorage, including deliberately empty drafts. Existing OPFS
projects, Git data and prior drafts remain untouched; their export/migration path
must be completed before the old distribution is removed.

From the repository root, follow the runtime prerequisites in
[`ui/next/README.md`](../../../ui/next/README.md), then:

```sh
node eng/ui-next/cli.mjs check
node eng/ui-next/cli.mjs dev
```

`dev` prints a VM preview address. Vo changes rebuild the independent development
image and restore compatible state; errors leave the last good page running.
Stylesheets update without restarting components. JavaScript/HTML changes restart
the page. See [reload contracts](../../../ui/next/develop/README.md). Use `preview` to inspect the already-built
application. Studio uses the Wasm VM. `&ssr` requests server-rendered HTML.
The local SSR server passes the URL and selected chapter cache through a bounded
stdin stream to native Vo. Its HTML contains the full semantic body, title and
description; the same inert initial data reaches the browser, avoiding a repeated
chapter fetch. The local server remains an integration fixture; the independent
production distribution below serves native HTML on every document request.

Web execution now uses the Wasm VM exclusively. Earlier backend comparisons
remain historical evidence. See the [performance report](../../../ui/next/performance.md)
for raw samples, cache conditions, and the remaining large-list limits.

Shared Vo application code lives in `app/`, with separate production `main.vo`
and `development/main.vo` entries. `app/app.vo` owns shell/navigation; the remaining
files separate routes, Gallery recipes, Docs and both Playgrounds. Browser
`application.js` creates navigation, draft and worker services for each actual
root; `boot.js` and `development-boot.js` select their mount entry. Development
uses `studio-dev.vob` and preserves production artifacts. Development renders on
the client; preview provides the production SSR path.
`app/editor.vo` shares editing and draft lifetimes with `app/ui-playground.vo`.
Failed draft reads leave stored content untouched until an explicit edit is made;
readiness and transient read failures follow the current storage request.
`runner.js` compiles/runs one console program. `preview-widget.js` owns the
preview document and its `preview.js` session; `ui-runner.js` materializes a
memory-only workspace, generates its explicit workspace lock with the compiler's
module API, compiles, and enters the shared Worker UI runtime. Builds and dev
reloads refresh `target/ui-next/playground-ui.json`; it downloads only on UI Run.
The source preview and the Studio shell both use Wasm VM.
Imports cover the standard library and packaged UI, with no registry installation.
Reusable UI stays in the
framework's `kit` and `navigation` packages.

Chromium, Firefox and WebKit regression evidence covers the Web VM, real route/history changes,
theme and native keyboard controls, Unicode execution, errors, cancellation,
draft restoration, 390px layout and deep-link SSR. Screenshots and results are
under `target/ui-next`. The first-party replacement inventory is in
[`migration.md`](../../../ui/next/migration.md). Full keyboard/assistive-technology review,
richer editor behavior, broader HMR/device coverage,
public release integration and migration remain in the rewrite plan.

## Independent production distribution

After the same runtime prerequisites, build the standalone Studio:

```sh
node eng/ui-next/cli.mjs build --studio
```

Copy `target/ui-next/studio-distribution` to the deployment machine. With Node 24
and a matching Volang CLI installed, run `node server/entry.mjs` from that
directory. `HOST`, `PORT` and `VO_EXECUTABLE` configure the listener and executable.
The default page is `/studio/gallery`; `/` redirects there. This build owns an
origin-root deployment. A non-root `BASE_PATH` is rejected with a diagnostic.
Use a separate origin when an existing site's path layout differs.

The complete directory includes bundled browser adapters, VM Studio images,
the native request bytecode, all 22 chapter assets and the separate search index. The browser compiler and
the UI source snapshot load only on Run. It needs no source checkout or
`node_modules` at runtime. The artifact report records every delivered file's
size and SHA-256. It also records included third-party package versions and ships
their licenses in `THIRD_PARTY_NOTICES.txt`. Builds validate generated documentation and publish the whole
result together; failed or cancelled builds retain the previous distribution.
Keep each running distribution immutable and restart to adopt another version.

The host automatically negotiates the prebuilt Brotli/gzip assets and sends them
as bounded streams. ETags avoid retransmitting unchanged files on revalidation.
Both the initial Studio images and later compiler/preview resources use the same
delivery path. See the [measured transfer sizes](../../../ui/next/performance.md);
application execution and rendering budgets are tracked separately.

The pure-Vo `server/` entry shares route metadata and document request identities
with `app/`. Every request starts with an isolated native process; chapters are
read from the deployed public directory before rendering and transferred through
the initial query cache. Unknown pages/chapters return 404, unsupported text POST
returns 405, and a broken document affects only its request. Missing asset paths
return 404 directly. Admission, deadlines and shutdown follow the framework's
[server adapter](../../../ui/next/server/README.md).

`node eng/ui-next/studio-distribution-contracts.mjs` moves a built copy into a
temporary directory and verifies its artifacts, all native chapters and the
Gallery/Docs/Playground browser contracts. The new distribution remains an
experimental entry; the public Studio release is a separate step in the rewrite
plan. Same-origin project export and the upgrade path below accompany migration.

## Static deployment

`node eng/ui-next/cli.mjs build --studio --static` first builds the native
distribution, then exports `target/ui-next/studio-static`. Deploy that entire
directory at the origin root of a static HTTP host. No application server,
Volang CLI or Node runtime is needed on the deployment machine. The host must
serve directory indexes, JavaScript modules and `application/wasm`, and use
`404.html` for missing pages while retaining HTTP 404. `.nojekyll` is included.

The exporter asks the compiled application for its 28 public page paths and
uses the same native page renderer for Gallery, both Playgrounds, recovery and
all 22 guides. It includes 36 legacy redirects and a missing-page document. Chapter
HTML and metadata are available without JavaScript. The shared 404 page includes
a native return link and starts no application runtime. Older `?topic=` chapter
links redirect to canonical chapter paths when JavaScript starts. Interactive
examples and previews compile in local browser workers; browser drafts and
project recovery retain the same origin storage behavior.

All files are inventoried, including precompressed HTML. The export verifies
its source distribution before and after rendering and replaces the previous
static directory only after success. Deploy all files together and revalidate
stable asset URLs. Brotli/gzip siblings are optional; configure the matching
`Content-Encoding` when using them. This build requires origin-root deployment.

## Upgrading the previous Studio

Publish the entire directory on the previous origin, including the replacement
`/service-worker.js`. It retires the previous Studio registration and removes
only its asset-cache namespace. Other registrations and caches, OPFS project
files and localStorage remain intact. It does not navigate open tabs: save any
unsaved editor changes before refreshing them. No new offline cache is installed.
Keep this retirement file available for returning users; deleting it would leave
old cached applications waiting for an update that cannot succeed.

If a returning browser still opens the cached shell, open `/?studio-next` in a
separate tab. The previous worker passes query-bearing requests through to the
network, and the new application requests its retirement update. The original
tab stays available for saving. Project recovery requires the same origin and
browser profile described below.

The application owns one redirect catalog for both native and static delivery.
Old language chapters keep their chapter identity; `/runner` opens Playground;
`/workspace`, `/search` and `/source-control` open project recovery. Retired UI
guides lead to the new UI guides. Redirects preserve query parameters and URL
fragments in browsers with JavaScript; no-script HTML provides a direct link and
refresh to the canonical page. Native requests return 307, and unsupported
methods return 405 with `Allow: GET, HEAD`.

Startup and runtime failures show a short message, a **Reload Studio** button
and collapsed diagnostic details. Unknown pages remain readable without running
the application. `studio-upgrade-contracts.mjs` verifies old-cache retirement,
open-tab preservation, project storage, old links and VM startup retries using
an archived copy of the previous worker in temporary browser profiles.

## Recovering saved browser projects

Serve `/studio/recover` on the previous Studio's exact origin: protocol, hostname
and port, using the same browser profile. Another address cannot access those
projects. Save all changes in any old Studio tabs and close those tabs before
exporting; editor changes that only existed in a closed tab's memory cannot be
recovered. The new Playground drafts use separate localStorage keys.

Recovery reads `vo-web-vfs-v1/data/workspace` directly without starting the old
Studio host or VFS. The project catalog is optional: unfinished and uncatalogued
directories remain selectable. Each ZIP preserves file bytes, Unicode paths,
hidden files and empty directories under the selected project's name. It uses
browser file timestamps within ZIP's supported date range; legacy VFS Unix modes
and its metadata sidecar are not reconstructed. Save the completed ZIP locally
before navigating away. Original files are never removed by this page.

Each preparation has a 30-second deadline and accepts up to 128 MiB of source,
32,768 entries, depth 256 and 4,096 UTF-8 bytes per archive path. ZIP output is
bounded at 160 MiB; discovery accepts up to 4,096 top-level entries. Exceeding a
limit, cancellation, read failure or a detected file change discards the whole
preparation. Keep other writers closed: individual file checks cannot provide an
atomic snapshot of a workspace modified by another tab.

`recovery.test.mjs` exercises storage and subscription ownership without browser
profile access. `studio-recovery-contracts.mjs` uses isolated synthetic OPFS data
and real downloads on the Wasm VM; the development contract checks that hot
reload releases a prepared URL and resets its transient state. Default deployment
replacement remains a separate migration step.

## Documentation sources

Chapter source ownership stays in `lang/docs/catalog.toml` and its Markdown files.
After changing them, run `cargo run -q -p vo-dev --locked -- generate studio-docs --write`.
The generator emits `documentation/catalog.vo` for the guest, `index.json` for the
local server, individual semantic JSON bodies, and provenance. CommonMark parsing
runs in build tooling; the browser renders through the optional pure-Vo `document`
package. Studio build/dev checks reject stale generated files. After regeneration,
the development watcher reloads the catalog and body assets. The existing released
UI chapters continue to document their own product; the rewrite uses its new UI
introductions. Console and UI compilers remain lazy and independent of Docs.

## Curated examples

`examples/catalog.vo` owns the editable sample sources and short descriptions.
It uses the new UI packages and has no dependency on legacy Studio project types.
Console examples cover loops, channels, closures, errors/cleanup, interfaces and
dynamic JSON; UI examples cover local state, filtering and independent checkboxes.
Builds extract this catalog through Vo, compile every source in an isolated starter
workspace, and check all six deterministic console outputs. Browser contracts run
the selected programs in the real compiler workers and interact with each UI
preview. Keep this small catalog focused on clear, portable examples.
## Execution and development reload

Editor drafts remain compatible reload state. Console execution and UI previews
keep their task/widget, busy flags and result status in one opaque application
snapshot. Reload cancels the old execution and returns Run/Stop to their idle
state; it does not replay the user's program. Running again creates a new owned
execution. Draft storage readiness also follows the new read request before any
write begins. `development-contracts.mjs` covers reload during a running console
program and an active UI preview, retained drafts and a subsequent fresh run.

Both source editors expose Ctrl+Enter / Cmd+Enter to run the current draft.
The framework matches exact keys and modifiers before native default actions;
ordinary Enter, additional modifiers and composition keep their native behavior.
Run callbacks ignore repeat and share the buttons' live execution guard. Keyboard
shortcuts stay local to the editor, with an `aria-keyshortcuts` declaration and a
visible hint. The same binding is serialized into SSR HTML and adopted on startup.


## Certified site candidate

After the complete UI gate exports its verified static directory, CI prepares
and checks the default Pages candidate with:

```sh
node eng/ui-next/studio-site-cli.mjs stage --source target/ui-next/studio-static --output target/ci/artifacts/site --domain volang.dev
node eng/ui-next/studio-site-cli.mjs check --directory target/ci/artifacts/site --output target/ci/results/ui-web-site
```

Staging preserves the static build report and all application bytes. Hosting
metadata records its build identity and domain; it does not configure DNS or
publish anything. Output must be separate from the input. Existing owned site
outputs can be replaced or repaired; unrelated directories are retained.

`verify --directory <site>` checks exact local files and the deployment size
budgets. Add `--origin <URL>` to compare HTTP representations. `check` also runs
the shared Gallery/Docs/Playground browser journey with Wasm VM and checks
HTTP identities again afterward. Reports must live outside the candidate.
The origin must be the site root, with no subdirectory prefix.

The main CI task and the Site workflow consume these commands and the same
candidate bytes. The former Studio directory lives at
`target/ci/artifacts/legacy-studio` for compatibility checks. See
[CI ownership and promotion](../../../docs/ci.md#site).
