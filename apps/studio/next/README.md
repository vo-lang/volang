# Volang Studio

Studio demonstrates the UI framework through a Gallery, searchable documentation
and a Playground. It shares its Vo application across Web and system WebView
desktop delivery.

## Pages

- `/studio/gallery` demonstrates controls, overlays, forms, collections and
  feedback. Each card includes a code snippet, copying and a link to complete
  Playground examples.
- `/studio/docs` provides 24 maintained chapters: framework guides, component API,
  language guides and specifications. Chapters support content search, a local
  outline, heading links and code copying.
- `/studio/playground` runs complete language examples and displays diagnostics.
- `/studio/playground/ui` compiles a component into an isolated live preview.
- `/` redirects to Gallery. Unknown pages return a missing-page document.

## Runtime ownership

Production Web pages run the Studio guest in a dedicated Worker. The page owns
DOM, browser services and widgets. Gallery and Docs use the execution runtime;
they do not load the compiler or editor library. The editor enhances a native
textarea, preserving early input, composition and selection.

Each console run owns a disposable compiler worker. Loading, compilation and
execution have separate deadlines. Stop, route changes and disposal terminate
the worker even when guest code loops synchronously. UI previews own an isolated
iframe and worker. Language-service requests are bounded and cancelled with the
editor that owns them.

The console and UI editors keep independent drafts in IndexedDB. Saves resolve
after their transactions commit. Late reads cannot overwrite active typing;
a failed read cannot silently replace a stored draft with the default example.
Selecting an example retains a one-step Restore action for the previous draft.

The cache-retirement helper invalidates only the known obsolete asset cache and
unregisters its service worker. It does not read, migrate or delete user storage.

## Development and delivery

Run repository commands from the repository root with the pinned Node toolchain,
compiler and browser prerequisites installed:

```sh
node eng/ui-next/cli.mjs dev --studio
node eng/ui-next/cli.mjs build --studio
node eng/ui-next/cli.mjs build --studio --static
node eng/ui-next/studio-distribution-contracts.mjs
node eng/ui-next/studio-static-contracts.mjs
```

The request-time distribution is written to `target/ui-next/studio-distribution`.
The static website is written to `target/ui-next/studio-static`; it includes
prerendered pages, a missing-page document, compressed assets and a content
inventory. The deployment pipeline validates the exact staged artifact.

Authored documentation lives in `lang/docs` and `ui/next`.
`lang/docs/catalog.toml` selects the chapters. Regenerate their semantic documents
with `vo-dev generate studio-docs --write`; do not edit `documentation/` by hand.

The application code lives in `app/`, runnable examples in `examples/`, and
browser-owned adapters beside this README. Browser contracts in `eng/ui-next`
cover navigation, SSR adoption, controls, dialogs, documents, early input, drafts,
worker cancellation, mobile overflow and resource cleanup. Desktop delivery uses
the framework's matching SDK and platform acceptance checks.

Performance measurements use `eng/ui-next/benchmark/studio.mjs`. Report the actual
build identity, environment, sample counts and limitations with any comparison.
Local browser results do not establish cross-platform release certification.
