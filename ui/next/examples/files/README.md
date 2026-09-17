# A little context

A pure Vo file-delivery example with labelled native input, bounded text preview,
form validation, multipart submission and file-aware reset. Copy `app/app.vo` and
`styles.css` into an ordinary UI project's `app/app.vo` and `web/app.css`.
Provide a multipart endpoint at `./api/upload` that returns a successful HTTP
status after accepting a delivery; the example maps failures to its file field.

`node eng/ui-next/file-project-contracts.mjs` creates the ordinary project, builds
VM/Wasm VM and prepared SSR artifacts, and runs it against a real local HTTP
upload endpoint. It covers three browsers, the Wasm VM, empty-root/client and
hydrated startup, exact multipart bytes, early selection, edits while saving,
successful clear, failed delivery, native reset and source reload.
