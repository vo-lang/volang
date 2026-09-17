# Your Volang UI application

This starter uses the experimental UI rewrite. The framework snapshot in
`vendor/ui` is selected by `vo.work` and `vo.lock`; commit those files with your
application. Builds consume the lock without changing it. Use a matching UI toolchain with the wire version declared in `ui-next.json`.
Add its `bin` directory to PATH, or set `VO_UI_TOOLCHAIN` to the package root
when using another matching `vo` executable. Browser tests use the active
toolchain, without a saved machine-specific path.

From this directory:

```sh
vo ui dev --project .
vo ui check --project .
vo ui test --project .
vo ui build --project .
vo ui preview --project .
```

Edit `app/app.vo` and `web/app.css`. Production, development and prerender entries
share `app.View(initial string)`. The starter interprets initial data as a name;
an application can decode its own JSON structure instead. Production uses
`host.RunWithData`, development uses `develop.RunWithData`, and the native build
entry uses `prerender.Run`.

Open **Inspect components** on a development page to inspect state and update
causes. CSS changes retain the DOM. Vo changes restore compatible state and
native input; JavaScript, HTML and configuration changes restart the page.
Compilation errors leave the last working page available. Development and
production use Wasm VM.

## More starting points

The toolkit includes `default`, `pages`, `fieldnotes`, `listening`, `canvas`,
`plot`, `scroll-position` and `variable-list`. For a separate project, run
`vo ui create ../my-garden --template plot`. Every template includes matching
application source, styles and public browser tests. They work without a checkout.

## Optional source editor

Add `"features": ["editor"]` to the existing `ui-next.json` and use
`github.com/vo-lang/ui/next/web/editor` in your Vo component. `editor.Enhance`
wraps a native textarea; its `value` attribute and ordinary `input` handler continue
to own the draft. See `vendor/ui/next/web/editor/README.md` for the component API.
The generated boot file works unchanged. The toolchain bundles the pinned editor
in separate chunks, loads it when a control mounts and emits its actual dependency
versions and licenses in `THIRD_PARTY_NOTICES.txt` and `build-report.json`.
Retain the notices when distributing your build.

Native editing remains available before startup or if the optional library fails.
Removing the feature omits the editor dependency from the host bundle. Feature
names must be supported and unique. `canvas` installs the Vo bitmap adapter
without a third-party dependency. `plot` adds the pinned line-chart library,
loaded when a chart mounts; the last chart releases its shared stylesheet.
The three packs can be selected together, for example
`"features": ["canvas", "editor", "plot"]`. See the `web/canvas` and `web/plot`
guides under `vendor/ui/next` for data, accessible alternatives and lifecycle.

## Application tests

The test command checks and builds production artifacts, then runs
`tests/browser/*.test.mjs` with standard Playwright fixtures on Chromium,
Firefox and WebKit with Wasm VM. Import `test` and `expect` from the
generated `fixtures.mjs`; use `page`, `appURL` and `backend` with semantic browser
locators. `UI_NEXT_BROWSER=chromium` selects one engine.

Each run preserves its build identity, JSON report and failure screenshots/traces
under `target/ui-next/browser-tests/run-*/`. Fixtures use the active toolchain.
Test tools are excluded from deployed pages.

## Static delivery

For separate page programs, configure `pageEntries` and select an `entry` in
`prerenderPages`. This template's HTML and boot file already select the correct
image. See `vendor/ui/next/page-entries.md` for the source wrappers and limits.
The `pages` starter demonstrates a home page and a separately compiled notebook.

Upload the contents of `target/ui-next/dist` to a static HTTP host. Production
includes native-rendered HTML before the client loads. The client adopts those
nodes and preserves edits entered before startup. Rendering uses one compiled
prerender image and a fresh process per page. Each process has a 30-second and
16 MiB HTML limit; the entire generated HTML set has a 64 MiB limit. A failure
preserves the previous complete distribution.

The default is one root page with empty initial data. Add a `prerenderPages`
field to the existing `ui-next.json` to generate more pages:

```json
"prerenderPages": [
  { "path": "/", "data": "" },
  { "path": "/hello/", "data": "Ada" },
  { "path": "/people/grace/", "data": "Grace", "title": "Meet Grace", "description": "A little introduction to Grace." }
]
```

Keep the root page. Paths map to directory-index HTML files; a missing trailing
slash is normalized. There may be up to 256 pages. Paths must be unique across
case and Unicode normalization, use portable directory names and avoid generated
output names. Initial data is a Unicode string of at most 1 MiB. It is sent to
the native entry on stdin and embedded as inert JSON for the same client view.
Native stdout belongs to HTML; write diagnostics to stderr.

The `document` object in `ui-next.json` supplies shared `title` and `description`
defaults. Each page can override either field, including with an empty string.
Titles are limited to 4 KiB and descriptions to 8 KiB of valid Unicode text.
Both appear in the initial HTML and development pages, with HTML escaping;
they are ordinary text, independent of the application's initial data.

Development emits the same paths and initial data with client rendering.
Remove both `prerenderPages` and `prerenderEntry` to use a client-only build.
Legacy root-page prerender entries that print only `host.HTML(view)` remain
supported when they do not require initial data.

Keep these markers when customizing `web/index.html`:

- One `<!--ui-next:content-->` for the rendered root.
- One `<!--ui-next:mode-->` for the client/server mode.
- One `<!--ui-next:data-->` inside the `ui-next-data` JSON script.
- `<!--ui-next:assets-->` before each generated stylesheet/script URL.
- One `<!--ui-next:title-->` inside the document title.
- One `<!--ui-next:description-->` inside the quoted description meta attribute.
- One `<!--ui-next:entry-->` inside the `ui-next-entry` meta attribute.
- One `<!--ui-next:backend-->` inside the `ui-next-backend` meta attribute.

Development and production use Wasm VM. Omit `defaultBackend` or set it to
`"vm"` in `ui-next.json`. Keep the backend slot when customizing the document so
static pages and native rendering receive the configured build choice.

Custom legacy templates with authored titles can omit the metadata markers when
their configuration omits `document` and per-page title/description fields.

The assets marker makes nested pages and subdirectory deployments work without
changing the base used by ordinary application links. Relative application
links keep normal HTML URL resolution. Deploy under a URL ending in `/` using a
host with directory-index support. Generated output names `assets/`, `theme.css`,
`build-report.json` and `THIRD_PARTY_NOTICES.txt` are reserved; put authored assets
elsewhere in `web/`.
An authored HTML file cannot collide with a declared generated page.

This is build-time static rendering. Request-time rendering, server loaders and
history-route fallback policy remain separate application adapters. Keep the
experimental host and vendor snapshot on the same wire version. The portable
toolchain is a local preview; release publication and product certification
remain separate.
