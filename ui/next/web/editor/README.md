# Progressive source editor

`Enhance` wraps a native textarea with an optional CodeMirror editor. The textarea
keeps the application's value, form binding, element reference, input events and
server-rendered fallback. Loading the library does not interrupt a focused native
edit or composition. A failed enhancement leaves the textarea usable.

Use `ui.ElementRef.SelectTextIfUnchanged(source, ui.TextSelection{Start: start,
End: end})` to select a source range and focus its control after the next commit.
Offsets are zero-based UTF-16 code units; the end is exclusive. `Direction` is
`forward` by default and also accepts `backward`. Ranges cannot split a Unicode
character. Vo strings use UTF-8 byte indexing, so compiler byte ranges require
conversion before calling this API.

The supplied source must equal the native control's current value. A stale source,
unacknowledged edit, active composition, disabled control or missing binding leaves
selection and focus intact. A true return value confirms queuing, as with `Focus`;
the condition is evaluated when the request is applied. Native controls normalize
line endings, so use the source obtained from the control's input contract.

The operation supports HTML textarea and text/search/url/tel/password inputs.
Other elements and malformed ranges are rejected during batch preflight before
DOM changes. The optional editor projects the same selection and scrolls its
selected range into view; it owns and releases that projection with its widget.
Ref requests belong in event handlers or post-commit effects.

Studio's console and UI Playground use this contract for compiler errors and
warnings. Their diagnostic lists belong to the submitted source; editing the draft
replaces the old actions with a request to run again. Dependency-file diagnostics
remain visible without pretending that they belong to `main.vo`.

## Optional semantic service

The browser host can pass an `EditorLanguageServiceFactory` as the second argument
to `createCodeEditorProvider(loadLibrary, languageService)`. A factory receives the
native textarea and its widget lifetime signal. It may return `complete` and
`definition` functions; ordinary editor projects need no compiler service.
The types are declared in the host's `ui_next/editor-service` module. The enhanced
surface adds the available semantic shortcuts to its accessibility attributes;
the native textarea retains only its authored shortcuts. Read-only editors omit
completion, and plain-language or disabled editors omit semantic shortcuts.

Requests contain the exact source, an absolute UTF-16 cursor position and an
AbortSignal. Completion responses echo that source and supply a replacement range
plus labelled items. Definition responses echo the request source and identify a
target file, target source and range. `local: true` explicitly identifies the same
document and requires identical source; file basenames never determine identity.

Ctrl + Space explicitly requests suggestions. F12 selects a local definition or
opens a read-only panel for an external definition. Escape and **Back to source**
close the panel. Requests cancel on source/selection changes, blur, reconfiguration
and disposal. Late, mismatched or malformed results are ignored; query failures
leave the editor usable and announce a retryable failure. Native editing,
composition, maximum length, form state and undo still use the textarea contract.

Studio supplies this service through a lazy compiler Worker. The public optional
editor does not include that application service or download a compiler itself.
