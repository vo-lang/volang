# An address book that grows

This ordinary Vo application demonstrates the experimental form array API:
stable row identities, nested phone fields, validation, cancellable saves,
saved-baseline revert and native reset. Alt + Up/Down in a name field moves the
person while keeping the focused input and selection. Each person's check count
shows local component state surviving a move.

Its validation rules return structured positional issues through `forms/schema`.
The adapter maps those positions to the current snapshot's stable field names;
moving an invalid row keeps the error attached to that person.

Import `forms`, declare `Options.Arrays`, initialize with `WithArray`, and bind
fields through `row.Field(...)`. Give each row its `row.Key`; render `OrderInputs`
for every native array order field. The [forms guide](../../forms/README.md)
describes the values, submission and reload contracts.

The save operation is a scoped timer for demonstration. The application contains
no account, remote workspace or persistence implementation.

`eng/ui-next/field-array-project-contracts.mjs` creates an ordinary project from
the shared starter, copies `app/app.vo` and `styles.css`, then builds the same Vo
application for VM and server HTML. It exercises three browsers, early edits,
focused moves, native field order, nested edits and actual development reload.
The test bootstrap explicitly empties the server root before the client-only
startup cases; normal deployment uses the generated server document.
