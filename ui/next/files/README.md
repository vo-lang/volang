# Native file selections

Import `github.com/vo-lang/ui/next/files`. `Enhance` connects an authored native
`input[type=file]` (or its complete labelled field) to a typed immutable
`Selection`. Store it in component state or use the [form binding](../forms/README.md).
There is one state owner; the browser retains the native `File` objects.

```vo
selection := ui.State(scope, "files", func() any { return files.Selection{} })
field := ui.Element("input").Attr("type", "file").Attr("id", "attachment").Attr("name", "attachment")
picker := files.Enhance(field, selection.Get().(files.Selection), func(next files.Selection) {
    selection.Update(func(any) any { return next })
}, files.Options{ID: "attachment"})
```

Use the Store's `Update` method to publish a new snapshot. Native type, name, accept, multiple,
disabled, required and labels remain ordinary HTML. Never bind a string `value`
to the input. Apply `kit.FormField` before enhancement, and replace the enhancement
key together with its native control when changing its ID or control identity.

`Selection.Files()` returns detached metadata; `Len`, `Error`, `Clear` and
`SameFiles` inspect or clear it. Files expose name, byte size, media type and
last-modified milliseconds. `File.Token()` is an opaque root-local adapter handle,
with no filesystem path or persistence contract. Metadata is descriptive; the
host checks actual file sizes for reads and uploads.

Zero limits use at most 32 files and 128 MiB per file; `Options.MaxFiles` and
`MaxFileBytes` can lower them. A single native input without `multiple` permits
one file. A root retains at most 256 references and 512 MiB of referenced file
sizes. This bounds retained native references, without copying their contents.
Oversized/rejected selections retain the picker and publish an error with no
accepted files. Acknowledging the error preserves it; `Clear` explicitly clears
both native selection and error. Unexpected adapter/configuration failures reach
`OnError`, or the component error boundary when no callback is supplied.

`ReadText(file)` is a scoped request for a whole UTF-8 file within 1 MiB. It preserves
the BOM and reports invalid UTF-8. `ReadBytes(file, offset, length)` reads up to
256 KiB; `DecodeBytes(result)` decodes its bounded binary result. EOF yields fewer
bytes. Normal UI task cancellation interrupts the native reader. Use
[multipart](../web/upload/README.md) to upload larger files directly.

Files selected in server HTML before activation are adopted from the same native
input. Native reset and programmatic FileList changes are captured before a
managed submit. A later native selection wins over a clear from an older revision.
Superseded references retire after the model acknowledges the new selection;
unmount retires the input's references. Reads/uploads started in that same commit
capture their native Blob before retirement and can finish under their task owner.
Later requests using retired or other-root references fail locally.

Closing a root cancels its tasks and releases all file references. A replacement
input cannot reuse another input's selection: choose the files again. Reload
does not serialize selections; new roots start with empty file fields. Native
FormData continues to include actual files under normal successful-control rules.
The initial implementation targets the Web host; desktop file adapters remain
part of the later platform work.

The [delivery example](../examples/files/app/app.vo) exercises labelled selections,
previews, validation and actual HTTP uploads. Browser contracts cover Chromium,
Firefox and WebKit, including early selection, cancellation and disposal.
