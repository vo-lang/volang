# Experimental forms

`forms.Use(scope, key, Options)` owns a form's values, saved baseline, touched
fields, errors and pending work. It builds on the same scoped state/effect/task
contracts as ordinary components. Import `github.com/vo-lang/ui/next/forms`.

```vo
form := forms.Use(scope, "profile", forms.Options{
    Initial: forms.Values{"name": []string{"Ada"}, "updates": []string{"false"}},
    Validate: func(data forms.Data) forms.Errors {
        values := data.Values
        if strings.TrimSpace(values.Get("name")) == "" {
            return forms.Errors{"name": "Enter your name."}
        }
        return nil
    },
    FocusInvalid: func(name string) { nameRef.Focus() },
})
name := form.Input("name", ui.Element("input").Ref(nameRef))
field := kit.FormField(kit.FieldProps{
    ID: "profile-name", Label: "Name", Error: form.Error("name"),
}, name)
view := form.View(func(data forms.Data) ui.Request {
    values := data.Values
    return web.After(400, "Saved " + values.Get("name"))
}, func(result ui.Result) forms.Outcome {
    return forms.Outcome{Message: result.Value}
}, field, ui.Element("button", ui.Text("Save")).Attr("type", "submit"))
```

`web.After` simulates a cancellable operation in this example. An application's
request and result decoder define its actual service and response format.

`Data` is the canonical snapshot passed to `Validate`, `ValidateAsync`, `Submit`
and `View`: `data.Values` holds text fields and `data.Files` holds typed native
selections. `form.Data()` returns a detached snapshot of both.

`Values` is an alias of the standard `net/url.Values`: each field has an ordered
list of strings. `values.Get(name)` and `form.Value(name)` read the first value;
`values[name]` and `form.All(name)` read all values. `Change` replaces a field with
one string, and `ChangeAll` replaces its entire list. No values (nil or an empty
list) and a single empty string have distinct meanings. List order participates
in dirty comparison, submission snapshots and response-error matching.

`Checkbox` binds one `"true"` or `"false"` string. `Input` accepts at most one value;
it rejects a multiple-value field to avoid discarding additional values. `Initial`
is deeply copied once; returned `Values()`/`All()` and values passed to validators
or request builders have independent lists. Mutating a returned snapshot never
publishes an edit. Call `Change`, `ChangeAll` or use a binding to publish edits.
`MultipleSelect(name, control)` binds a native select to the complete ordered list
and enables its `multiple` attribute. It preserves a changed second selection even
when the first value stays unchanged, including input made before SSR activation.
The binders own the input/change and blur handlers; caller attributes remain
available. `kit.FormField` adds labels and errors while retaining caller classes
and descriptions. `Class` and `DescribedBy` compose those token attributes.

For repeated choices, connect the native checkbox group through the same form:

```vo
// Declare "interests": []string{"writing"} in Initial.
choices := kit.CheckboxGroup(kit.CheckboxGroupProps{
    ID: "interests", Label: "What makes you curious?", Name: "interests",
    Options: []kit.ChoiceOption{
        {Value: "writing", Label: "Writing"}, {Value: "design", Label: "Design"},
    },
    Value: func() []string { return form.All("interests") },
    OnChange: func(values []string) { form.ChangeAll("interests", values) },
    OnBlur: func() { form.Blur("interests") },
    Error: form.Error("interests"),
})
```

This experimental API previously used `map[string]string`. Migrate initial
strings to one-element lists and snapshot reads to `Get`; scalar `Value`,
`Change`, `Input` and `Checkbox` calls retain their behavior. The stable legacy
UI package is unaffected.

Validation is synchronous first. `Blur` marks the field touched and validates;
edits recompute synchronous errors. Untouched field errors become visible on a
submit attempt. Optional `ValidateAsync` and `DecodeValidation` define a scoped
request and decode its successful response into field errors. Transport errors
appear at the form level. Keep validators and request/response transformations
pure. `FocusInvalid` runs after a failed submit validation, with the first invalid
field in declaration order, or an empty string for a form-level error.

The optional [schema adapter](schema/README.md) translates structured validation
issues into these same errors. `schema.Adapt` binds positional array paths to
stable row identities from each validation snapshot. `schema.From(data)` also
supports server or asynchronous results for a retained snapshot. Rules and value
decoding stay with the application or selected schema library.

`Submit` validates a frozen snapshot, then sends it. It returns whether a new
attempt was accepted; that does not imply validation or saving succeeded. A
submit replaces an outstanding blur validation. Duplicate submit attempts during
validation/submission are ignored. Editing while validation runs cancels that
attempt. During an already-started submission, edits remain available: success
advances the saved baseline to the submitted values, so newer edits stay dirty.
Field errors from a response apply only when that field still has its submitted
value. They remain visible when another field changes or blurs. Editing the
affected field clears its returned error; a new submit attempt validates and
retries the current values. Unknown response field names become form-level errors.

`Pending`, `Validating`, `Submitting`, `Dirty`, `Touched`, `Error` and `Message`
drive the view. `Error("")` reads the form-level error. `Cancel` retires pending
work; it cannot reverse a server write that already occurred. `Reset` cancels
work, restores the original values and clears interaction/validation state.
`Revert` cancels work and restores the latest successful submission baseline.
Use it for a Discard changes action that follows successful saves.
The bound form uses native submit/reset events and disables native constraint
validation so the declared validators own error presentation. Submission reads
the application model. Native `FormData` still follows successful-control rules,
including omission of disabled or unchecked controls; the model does not infer
field participation from DOM visibility, disabled attributes or native names.
Use `Options.Fields` to explicitly exclude a field from model submissions.

For a native server form that returned rejected input and errors, pass
`Options.Submission: &forms.Submission{Values: submitted, Errors: errors}`.
Keep `Initial` as the persisted values. The form copies the submission once when
its state is created, overrides declared fields, and displays returned errors in
the first HTML render. Untouched values and the saved baseline stay intact. Early
browser edits follow normal input handling during activation. Submitted values
must name declared fields; unknown or inactive error names become form errors.
The application should consume this startup seed after the first commit so later
route remounts use current data. The [Fieldnotes template](../templates/fieldnotes/README.md)
demonstrates a standard POST/422/303 flow and scoped JSON writes sharing validation.

Declare potential conditional fields in `Initial`. `Options.Fields` selects the
current participants; nil selects all declared fields. Hidden fields keep their
values and do not enter validation/submission snapshots. Changing participation
cancels pending work after commit. Removing the form declaration, even inside a
surviving component scope, cancels its tasks. Reinstating it retains the values;
obsolete declaration handles cannot start new work. Use a new component key to
create fresh initial values. The hook reserves the effect key `"forms:" + key` in
its declaring scope. Public mutations belong in handlers or post-commit work.

## Dynamic and nested arrays

Declare each top-level array in `Options.Arrays` and build its initial rows with
`WithArray`. Each row is a relative `Values` map with at least one field. The helper
returns a deep copy and refuses to replace an existing field group. Nest another
`WithArray` inside a row to initialize nested arrays; the top-level declaration
owns their complete lifetime too.

```vo
initial := forms.WithArray(nil, "people", []forms.Values{
    forms.WithArray(forms.Values{"name": []string{"Ada"}}, "phones", nil),
})
form := forms.Use(scope, "contacts", forms.Options{
    Initial: initial, Arrays: []string{"people"},
})
people := form.Array("people")
for _, row := range people.Items() {
    name := form.Input(row.Field("name"), ui.Element("input"))
    // Render the row container with .Key(row.Key).
    phones := form.Array(row.Field("phones"))
    // Add phone fields and phones.OrderInputs() inside the same native form.
}
// Event handlers:
added := people.Append(forms.WithArray(forms.Values{"name": []string{"New"}}, "phones", nil))
people.Move(added.Key, 0)
people.Remove(added.Key)
```

`Items()` returns detached row identities; `row.Field(name)` builds the stable
field path. `Insert(index, values)` and `Append(values)` return the new row.
`Move(key, index)` uses the final zero-based index. `Remove(key)` returns false
when that row is already absent. Initial rows use deterministic `initial-N` keys;
inserted rows use a monotonic `row-N` identity, retained through reset, revert and
reload. The form supports at most 2,147,483,646 generated row identities in one
lineage. Array operations reject invalid indices, malformed row data and stale
field membership before publishing a change.

Moving a row retains its names, touched state and field errors. Use `row.Key` for
the row component/container so its local state and DOM identity also survive.
Removing a row removes its complete nested subtree and associated field metadata.
Bindings ignore input/change/blur already queued for a removed field before the
DOM commit; explicit application mutations of a missing field still have diagnostics.
Changing array structure cancels pending validation and submission immediately;
a submission started after that edit in the same event turn is accepted normally.
Cancellation cannot undo work already performed by a server. Ordinary scalar edits
during a submission continue to preserve that submission's frozen snapshot.

`Reset` restores the original row shape and values. `Revert` restores the shape and
values of the latest successful submission. Both keep the identity counter.
Nil `Options.Fields` includes current dynamic fields; explicitly selecting a
declared array name includes all its current descendants. Excluding that name
keeps the group's values while leaving it out of validation and submission. Array
groups participate as complete snapshots; selecting only a descendant without its
owning array is rejected. The model can retain conditionally hidden controls inside
a selected group; they continue to participate until the group is excluded.
Array declarations are fixed for the lifetime of a form scope; use a new scope/key to
change them. Form operations read the latest snapshot even when several array
edits occur before the next render.

Render `OrderInputs()` once for each participating array inside its native form.
These hidden controls carry the same ordered values as the model. The leading
empty value keeps an empty array distinct from an omitted array in standard
`FormData`; subsequent values are row IDs. A name field can therefore be
`people[row-1].name`, while a nested order field is `people[row-1].phones`.
`ArrayRows(values, name)` validates this representation and returns the same row
paths to a server/validator. It returns an error for duplicate/invalid IDs,
orphan fields, or rows without fields. Keep array ordering changes in the
`FieldArray` API. The values remain ordinary `net/url.Values`; this package does
not implicitly interpret them as JavaScript property paths or application structs.

A restored `Submission` can include a complete replacement for a declared array
group. Include its order field and every submitted row's fields. Omitted array
groups keep their initial values; the leading empty order value explicitly
restores an empty group. Returned row errors use stable `row.Field(...)` names.

Reload envelope v2 preserves compatible array groups, current values, successful
baselines and the identity counter. Legacy v1 scalar snapshots remain readable.
Changed array declarations or row-field shapes inferred from the new initial
values reset the affected array to its new defaults while retaining compatible
scalar fields. Empty initial arrays carry no row-field schema to compare; when
changing such a row schema, change the form/component key explicitly. Reload
always recreates task ownership and never replays a submission.

The ordinary [address-book example](../examples/field-array/app/app.vo) demonstrates
nested phone lists, row-local state, focused keyboard movement and save/reset/revert.
Its save request uses a cancellable timer; connect an application service for
actual persistence.

The native contract probe covers snapshots, dirty/touched, async ordering, duplicate
submits, edits during saving, response errors, conditional fields and cancellation.
Studio and the independent workbench consume the API through actual browser VM
and Wasm VM images, including server HTML activation. Ordered repeated values
and checkbox groups/native multiple selects use the same lifecycle. Dynamic arrays
now share that snapshot and cancellation path. Structured schema issues use the
optional adapter. File values remain a separate extension; this package has not
completed all form product certification requirements.

With `develop.Run`, reload retains field values and saved baselines while recreating
validation and task ownership. It preserves edits made during an outstanding save
without replaying that save. New source controls field participation and reset
defaults. See [development reload](../develop/README.md).

## Native files

Declare file field names in `Options.Files`, initially empty. Keep those names
separate from `Initial`. `form.Selection(name)` returns a typed `files.Selection`;
`ChangeFiles` publishes one into the existing form state. `FileInput` connects an
authored native input or complete labelled field to that state:

```vo
// Options{Initial: forms.Values{"title": []string{"Draft"}},
//         Files: []string{"attachments"}}
field := kit.FormField(kit.FieldProps{
    ID: "attachments", Label: "Attachments", Error: form.Error("attachments"),
}, ui.Element("input").Attr("type", "file").Attr("name", "attachments").Attr("multiple", "true"))
picker := form.FileInput("attachments", field, files.Options{
    ID: "attachments", MaxFiles: 3, MaxFileBytes: 2 * 1024 * 1024,
})
// In a submit request builder receiving forms.Data:
request := upload.Multipart("/api/upload", data, web.HTTPOptions{})
```

Imports are `next/files` and `next/web/upload` under `github.com/vo-lang/ui`.
Apply `kit.FormField` before `FileInput` so its label/ID stay on the native input.
The authored `name` controls native FormData; use the declared name consistently.
File fields participate through `Options.Fields` just like scalar fields. File
declarations are fixed for a scope and currently belong outside dynamic array
groups; they are not restored from a server submission or serialized on reload.

Rejected native selections publish their bounded error through the same form
validation path and mark the field touched. A file edit cancels pending validation.
Response errors apply only to the unchanged submitted file selection. Successful
submission clears its unchanged submitted files; a newer selection stays dirty.
`Reset` and `Revert` clear all file selections while retaining native revision
ordering, so an already newer native picker event wins over an earlier clear.
File contents never become a saved baseline that could reopen a native picker.
Reload restores text/array state and resets files; selecting files again is explicit.

Use [file reads](../files/README.md) for bounded previews and
[multipart requests](../web/upload/README.md) for native upload bodies.

### Experimental callback migration

The previous `func(forms.Values)` validation/submission callbacks now receive
`forms.Data`. Read text from `data.Values`; read selections from `data.Files`.
`Initial`, `Submission.Values`, `WithArray`, `ArrayRows`, `Form.Values` and scalar
binders retain their text representation. `schema.Adapt` also receives `Data`,
and `schema.From(forms.Data{Values: values})` adapts a text-only server snapshot.
The stable legacy UI API remains unchanged.

## Independent field updates

Use `form.Field(name, render)` to give a field its own component subscription.
`Value`, `All`, `Error`, `Touched` and native bindings subscribe to the field they
read; reading a second field adds that dependency. The form declaration observes
field structure. `Dirty`, pending status and messages subscribe to their selected
result, while `Data` and `Values` intentionally observe the whole snapshot.

```vo
field := form.Field("name", func(form *forms.Form) ui.View {
    return kit.FormField(kit.FieldProps{
        ID: "profile-name", Label: "Name", Error: form.Error("name"),
    }, form.Input("name", ui.Element("input")))
})
```

`Options.FieldValidators` maps text field names to pure `func([]string) string`
checks. Each validator receives a detached list and returns an empty string on
success. Without whole-form or asynchronous validation, editing a field runs only
its independent validator. Submission and explicit validation check all active
fields. Use `Validate` for cross-field rules; that path still validates the full
snapshot and notifies any sibling whose visible error changes. Async validation,
server errors, participation, reset and cancellation preserve their existing rules.
