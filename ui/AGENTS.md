# Volang UI workspace rules

Run repository commands from the root. Read `ui/docs/architecture.md` before
changing framework boundaries.

- `ui/next` owns typed Vo component, state, effects, task, data and rendering APIs.
- `lang/crates/vo-ui-bridge` owns the VM exchange boundary. Browser hosts live in
  `lang/crates/vo-web/js/ui_next`; native execution and system WebView hosts live
  in `vo-ui-native`, `vo-ui-webview`, and `vo-ui-desktop-runtime`.
- Keep one writer per root, parent-local sibling keys, generation-checked events,
  bounded frames, cancellation before disposal and deterministic cleanup.
- Effects belong in handlers, tasks or post-commit work. Rendering must remain
  replayable. Preserve input selection, composition and accessibility semantics.
- Update wire producers, generated codecs and consumers together. Generate
  checked-in protocol outputs with `node eng/ui-next/generate.mjs --write`.
- `eng/ui-next` owns tooling and executable acceptance contracts. Keep browser,
  desktop, packaged toolchain, static page and Studio checks aligned with changes.
- `ui/certification.toml` lists the required CI tasks and declaration sources.
  Declaration validation does not establish product certification; release
  evidence must be bound to the source commit and complete CI bundle.
- Follow `ui/docs/release-policy.md`. Measure performance on real interactions;
  do not infer latency or memory improvements from implementation changes alone.
