# Small pages

A home page and a notebook, written in Vo. The two pages build into independent
programs. `/notes/` and `/notes/morning/` share the notebook image and receive
different initial data. Ordinary links navigate between documents; browser
history works with its native page and scroll behavior.

Use the matching UI toolchain, with its `bin` directory on PATH. Set
`VO_UI_TOOLCHAIN` to the package root when using another matching `vo` executable.

```sh
vo ui dev --project .
vo ui check --project .
vo ui test --project .
vo ui build --project .
vo ui preview --project .
```

Edit `app/app.vo` for the home page, `notes/app/app.vo` for the notebook, and
`web/app.css` for their shared styles. Notebook data in `ui-next.json` supplies
the note text and the relative link back home. The draft lives in the current
page; this example has no persistent storage.

The home page declares WebPage JSON-LD with `ui.DataBlock`; it is readable in
the initial HTML. The notebook exposes its saved note as an ordinary JSON data
block. Saving updates that snapshot while retaining its native text identity;
source reload preserves the saved state and any newer unsaved draft. Encoding
escapes HTML parser tokens and keeps the original JSON values intact.

`pageEntries.notes` defines its production, development and prerender entry
directories. Each wrapper imports the same notebook component. The root entry
is named `default`; each `prerenderPages` item chooses an entry and supplies its
initial data. The template includes an entry marker and entry-aware boot file.

Development runs VM with state inspection and compatible state/input reload on
both entries. Production renders each page in a fresh native process and builds
Wasm VM images. Each page downloads only its own program; host
JavaScript, VM support and CSS are shared. Both development and production use
Wasm VM. The build report records every
page, entry and artifact digest. A failed build preserves the previous output.

Deploy `target/ui-next/dist` on a static HTTP host, retaining its notices.
Relative assets and links also work beneath a deployment subdirectory. See
`vendor/ui/next/page-entries.md` for limits, migration and the delivery boundary.
The framework is the experimental snapshot selected by `vo.work` and `vo.lock`;
project tools come from the selected portable UI toolchain preview.
