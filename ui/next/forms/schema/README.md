# Structured validation issues

Import `github.com/vo-lang/ui/next/forms/schema` to connect a Vo validator or
schema library to the existing form lifecycle. The adapter owns path mapping and
error presentation data. Your validator owns its rules, decoding, refinement,
locale and messages. It adds no form state, task scheduler or validation DSL.

```vo
func validateProfile(data forms.Data) []schema.Issue {
    values := data.Values
    issues := []schema.Issue{}
    if strings.TrimSpace(values.Get("name")) == "" {
        issues = append(issues, schema.Issue{
            Path: schema.Field("name"), Message: "Enter your name.",
        })
    }
    return issues
}

form := forms.Use(scope, "profile", forms.Options{
    Initial: forms.Values{"name": []string{""}},
    Validate: schema.Adapt(validateProfile),
})
```

Translate a library's result to `[]schema.Issue` in that callback. The adapter
returns the normal `func(forms.Data) forms.Errors`, so touched fields, submit
validation, errors, async work, reset and reload keep their existing behavior.
Validators receive only participating fields; decide explicitly how your schema
treats an omitted conditional field. Keep validation free of external effects.
An input transformation does not publish a form edit; use `Change`/`ChangeAll`
for an intentional application edit. The Form supplies an independent input
copy, and the adapter freezes path identities before invoking the validator.

## Stable paths for dynamic arrays

A path uses literal field names and zero-based array positions:

```vo
schema.Field("email")
schema.Field("address").Field("city")
schema.Field("people").Index(0).Field("name")
schema.Field("people").Index(1).Field("phones").Index(0).Field("number")
schema.Field("interests").Value(1)
```

`Field("address.city")` is also a literal full field name. Consecutive `Field`
segments join with a dot, matching the native form naming convention. Each
`Index` addresses a `forms.WithArray` group in the validation snapshot. It uses
that group's ordered row identities, then `Field` selects the row's relative
field. A top-level array error can target `Field("people")` directly. An index
without a following row field is unresolved; the current form model has no
separate row-object error field.

`Value(index)` addresses one item in an ordered string field such as a multiple
select or checkbox group. It must finish the path and reports the issue on that
whole field. Use `Index` for dynamic rows and `Value` for repeated scalar values;
both validate the index against the captured snapshot. Explicitly distinguishing
them also handles a scalar list whose first value is an empty string. Translate a
schema library's primitive-array path to `Value` using that library's field type.

Appending a segment creates an independent path, so reuse a prefix safely.
The zero-value `schema.Path{}` addresses the whole form. Field and index segment
construction can represent invalid external paths; resolution supplies the
diagnostic without selecting another field.

The [address-book example](../../examples/field-array/app/app.vo) adapts positional
name errors to the current stable person row. Moving the row retains the error's
native identity, and the next validation maps positions using the new snapshot.

## Server and asynchronous results

Freeze the exact values being validated, before they can be changed:

```vo
resolver := schema.From(forms.Data{Values: submittedValues})
// Translate the validator's result for those same submitted values.
fieldErrors := resolver.Errors(issues)
```

`From` receives `forms.Data`, copies the text value map and every list, and freezes
the declared file field names. `Field("attachments")` can target a file selection;
`Value(index)` applies only to ordered text fields. File contents and native tokens
are unnecessary for path resolution. Later changes to the caller's map or
array order cannot move its positional errors to another row. Reuse this resolver
only for results belonging to that snapshot; create a fresh one for a new
validation. Array ownership is checked once per addressed group and cached in
that resolver. A resolver belongs to one validation operation; do not share it
between concurrent operations.

For a server response, translate issues to `forms.Errors` before serializing the
response or constructing `forms.Submission`. The wire data then uses stable native
field names. `Path` is a typed in-process value with private segments; this package
does not define an external JSON schema-library protocol.

For client async validation, capture `schema.From(data)` when `ValidateAsync`
receives that attempt's values, and use it in `DecodeValidation` for that attempt.
Keep request and decoder ownership with the Form: edits and array structure
changes cancel old validation, and obsolete task responses never reach the
decoder. For already-running submissions, the existing submitted-value matching
continues to suppress field errors for values edited after submission. Constructing
a resolver from the live form when an old response arrives can misattribute
positional issues; always retain the submitted snapshot or return stable names.

## Errors and diagnostics

`resolver.Errors(issues)` returns a fresh `forms.Errors` map. It ignores empty
messages, preserves issue order for each field, removes exact duplicate messages
for that field, and joins distinct messages with a newline. It retains unresolved
issues as form-level messages. Unknown fields, negative/out-of-range indices,
malformed array order, missing row fields and incomplete row paths cannot silently
select another row. `resolver.Resolve(path)` exposes the exact diagnostic for
application logging or a custom adapter. The normal Form normalization still
handles fields that stop participating after validation.

This package supplies issue adaptation. It includes no required/email/number rule
catalog, automatic type coercion, network validation service or schema engine.
Use ordinary typed Vo code or a separately selected library for those decisions.
The existing `Options.Validate` and `DecodeValidation` callbacks remain usable
directly when a validator already returns `forms.Errors`.
