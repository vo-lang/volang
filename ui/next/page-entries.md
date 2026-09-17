# Independent page programs

Create a small static site with separate home and notebook programs:

```sh
node eng/ui-next/cli.mjs create target/small-pages --template pages
node eng/ui-next/cli.mjs check --project target/small-pages
node eng/ui-next/cli.mjs preview --project target/small-pages
```

The home page downloads its own program. Opening the notebook loads the notebook
program. Both use one browser host, one set of VM support files and shared CSS.
`/notes/` and `/notes/morning/` reuse the same notebook program with different
initial data. The generated project includes a browser test and a development
entry for each program.

## Declare entries and pages

An existing single-entry project keeps its current configuration. Add named
entries and select them on static pages when there is a useful document boundary:

```json
{
  "developmentEntry": "development",
  "prerenderEntry": "prerender",
  "pageEntries": {
    "notes": {
      "entry": "notes",
      "developmentEntry": "notes/development",
      "prerenderEntry": "notes/prerender"
    }
  },
  "prerenderPages": [
    { "path": "/", "data": "" },
    { "path": "/notes", "entry": "notes", "data": "Your first note" },
    { "path": "/notes/morning", "entry": "notes", "data": "A new day" }
  ]
}
```

Merge these fields into `ui-next.json`, retaining its `format`, `wireVersion`,
document metadata and optional features. The data format belongs to each entry's
Vo application; the generated `pages` template uses JSON with `note` and `home`
fields. The same string goes to native rendering and browser startup.

`default` identifies the project root's `main` package. Omitting a page's `entry`
selects it. A named entry points to an ordinary Vo main package or file. Its
production wrapper calls `host.RunWithData`, its development wrapper calls
`develop.RunWithData`, and its native renderer calls `prerender.Run`. All three
import the same page component. Keep shared Vo code in ordinary imported
packages. Independent programs each link the dependencies they use.

Only entries selected by a page are compiled into a static distribution. Each
selected entry is compiled once per backend and has one prepared native render
image. Each page renders in its own fresh process. `check --project` checks every
declared source entry, including unused entries, before building. Source paths
must remain within the project after symlink resolution.

## Browser and development behavior

Use native `a` elements to cross entry boundaries. A click opens a document with
the destination's program, initial data and title. Browser back/forward behavior,
modified clicks and no-JavaScript navigation remain native. A saved browser page
may retain its state; a fresh load starts from its declared initial data. Persist
any draft that needs to survive a new document independently of component state.

Within one entry, the existing Vo navigation/router APIs continue to work. This
delivery mechanism partitions complete page programs. It does not link a new
component module into an already running VM root. Shared Vo dependencies can
appear in several images, so total site size may grow while individual pages
download less code. Profile actual entry sizes before introducing more splits.

Development emits the same page paths, data and selected VM image with client
rendering. Source reload fetches the active entry's replacement image and keeps
compatible state, draft, focus and selection. All entries must use the same
inspection policy: configure `developmentEntry` everywhere, or omit it everywhere
and use full document reload. Configuration and host asset changes reload the
document. Compilation failures leave the previous complete output available.

## Output and migration

The default program is `assets/app.vob`. Named images use
`assets/entries/<name>/app.vob`. All pages share `assets/app.js`, runtime
support, optional host chunks and styles. The report lists page-to-entry selection,
entry sources, prepared renderer identities, and the complete artifact digests.
Native render bytecode is temporary and is removed on success, failure or
cancellation. Any failed entry or page keeps the previous complete distribution.

New project templates already include:

```html
<meta name="ui-next-entry" content="<!--ui-next:entry-->">
```

An older project opting into named entries must add that marker to `web/index.html`
and update `web/boot.js` to the current template's entry-aware artifact selection.
The build diagnoses a missing marker. Custom boot code must read this identity
and resolve the selected image relative to the shared host script; it must not
hard-code `assets/app.vob`. Old single-entry HTML and boot files remain supported.

Entry names contain 1–64 lowercase ASCII letters, digits and hyphens, start with a
letter, and exclude `default` and portable device names. Up to 32 named entries
and 256 static pages are supported. The existing 1 MiB data, 16 MiB per-page HTML,
64 MiB combined HTML and 30-second per-page rendering limits apply. The root page
is required and may itself select a named entry. Relative assets work beneath a
deployment subdirectory.

## Request-time page selection

`pageEntries` also works with `serverEntry`. Choose either `serverEntry` or
`prerenderPages` for a project. A request-time build compiles every declared client
entry, since its handler can select them dynamically. It does not execute the
per-entry `prerenderEntry`; those declarations can be omitted. Development entries
keep the same all-or-none inspection policy as static pages.

The native handler pairs its HTML/render function, startup data and client image:

```vo
return server.Page{
    Entry: "notes",
    Render: notes.View,
    InitialData: encodedNote,
    Title: "Your notebook",
}, nil
```

The `notes` entry must use that same application view in its browser entry.
Empty `Page.Entry` selects `default` for rendered HTML. JSON, redirects without
HTML and bodyless statuses keep it empty. A server may import every view it needs
for SSR; browser images retain their independent module graphs.

The immutable build writes its admitted entry IDs to `server/entries.json`.
Production and development read the same list and validate template outlets before
accepting requests. An undeclared response entry fails before any HTML is sent.
Each document's boot reads its entry identity and downloads only that image.
Versioned development asset URLs retain the selected entry across source reloads.
Native links/back navigation cross document boundaries; this does not add dynamic
component linking inside a mounted root.

This extends the private server protocol to version 4. Upgrade the experimental UI
module and toolchain together, then rebuild the complete server distribution.
Static entry configuration and image paths retain their existing contract.

## Validation

`node --test eng/ui-next/project-entries.test.mjs` checks declarations, path
ownership, entry selection and template compatibility.
`node eng/ui-next/page-entries-contracts.mjs` builds a real ordinary project and
checks the Web VM in Chromium, Firefox and WebKit: native HTML, per-page
data, early input adoption, selected-image downloads, document navigation,
history, teardown, failed-build preservation and nested development reload.
Its report and copied production distribution are under
`target/ui-next/page-entries/`.

`node eng/ui-next/server-page-entries-contracts.mjs` additionally exercises actual
request-time selection, HTML/data/image agreement, cross-document navigation,
early input, declared-image admission, failed-build preservation and named-entry
source reload. Its report/distribution are under `target/ui-next/server-page-entries/`.
