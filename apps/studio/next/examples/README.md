# Editable examples

This package owns the rewritten Studio's six console examples and three UI
examples. Sources are ordinary, standalone programs; the UI entries use
`github.com/vo-lang/ui/next` and its public host/kit packages. The old Studio's
project files, desktop assumptions and application domain are independent.

`Console()` and `UI()` return fresh metadata slices. Source strings remain
immutable and are copied into the editor only after Open example. Keep samples
small enough to read, edit, and run without installing additional modules.

Run `node eng/ui-next/studio-examples.mjs` to compile all nine programs in an
isolated starter and verify the console outputs. The normal Studio build includes
this check. `studio-examples-contracts.mjs` exercises the same catalog through the
real browser worker and UI preview, including cancellation, draft restoration,
storage failures and server-rendered editor adoption.
