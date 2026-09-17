# Native HTML text, data and media

HTML `title` and `style` use the ordinary element and text API:

```vo
ui.Element("title", ui.Text("A title with < & >"))
ui.Element("style", ui.Text(".notice { color: var(--accent); }"))
```

They accept no children or one direct `Text` child. Build the complete string in
the calling component; nested components, fragments and multiple text children
have a diagnostic. An empty `Text("")` keeps its own identity. A later text update
retains both the element and its text node; removing that child releases it.
Normal keyed movement, ownership and root disposal apply.

Server HTML puts the text identity on `data-vo-text` on the parent. Comments
cannot carry that identity inside these elements: the HTML parser treats them as
content. The host adopts the real text node, including reconstructing an empty
node, and checks it against the same generated mutation frame. These are the
existing `#text` creation and update operations and add no binary operation.

Title text is HTML-escaped. CSS text is emitted literally so selectors, strings
and ampersands retain their meaning. Style text requires LF line endings and
cannot contain a closing HTML style tag, matched without ASCII case sensitivity.
Use CSS escapes when such literal content is needed in CSS strings. NUL is rejected
in both elements. The guest and host validate these constraints before commit;
the host also rejects malformed or duplicate text identities. Title and style
cannot be Portal destinations. SVG text keeps its existing namespace behavior.

Ordinary escaped HTML text and attributes preserve carriage returns with character
references. This also applies to title/textarea values and makes their parsed text
match client creation. These choices follow the [HTML text syntax and parser
rules](https://html.spec.whatwg.org/multipage/syntax.html#restrictions-on-the-contents-of-raw-text-and-escapable-raw-text-elements).

`title` stays at its declared position. Use the page document configuration for
initial document metadata and `navigation.SetTitle` for route titles. Neither
recipe hoists, deduplicates or loads resources. Ordinary stylesheets and the
build's script entry remain the application defaults.

## Document data

```vo
ui.DataBlock("application/ld+json", map[string]string{
    "@context": "https://schema.org",
    "@type": "WebPage",
    "name": "A small beginning",
})
```

`DataBlock` supports `application/json` and `application/ld+json`. It snapshots
the value through `encoding/json.Marshal`, including its HTML escapes, normalizes
CRLF/lone-CR formatting whitespace to LF, then
returns an ordinary script View with one Text child. Nil encodes as JSON null.
Unsupported media types and encoding errors have render diagnostics. Subsequent
changes to a source map or slice do not alter an already constructed View.

The data is readable in server HTML before activation. Hydration adopts its real
text node through `data-vo-text`; state changes, keyed moves and disposal retain
the ordinary ownership rules. Nothing is hoisted, fetched or evaluated. Both
supported media types may be switched on a retained node. The existing pages
template demonstrates WebPage JSON-LD and a saved notebook JSON snapshot; its
public tests exercise early data, identity and updates, and source reload keeps
saved values alongside an unsaved input draft.

Generic `Element("script", Text(...))` uses the same constraints: HTML namespace,
one direct Text child, one of these two explicit types, no src, valid JSON,
LF newlines and escaped `<` characters. Prefer DataBlock for automatic encoding.
The host initializes the inert type before insertion and validates all type/src
mutations before committing, including intermediate changes in a batch. These
are standard [HTML script data blocks](https://html.spec.whatwg.org/multipage/scripting.html#the-script-element).

The internal wire advances to v23 to bind producer and host semantics. It adds
no mutation operation, event queue, task or browser dependency.

## Parser contexts and embedded documents

HTML element names use lowercase ASCII; SVG names preserve case. The guest and
host now validate the same namespace naming rules before committing. This also
prevents uppercase HTML spellings from changing interpretation during parsing.

An iframe owns its native nested document. Supply `src` or `srcdoc` and a useful
title; managed children and Portal destinations inside the iframe are rejected.
The framework escapes srcdoc as an ordinary attribute, and the browser parses
that separate document. Use a managed widget when integration needs an explicit
bridge across document or subtree ownership.

`template`, `noscript`, `xmp`, `noembed`, `noframes` and `plaintext` currently
report a dedicated-content-binding diagnostic. The previous generic path could
emit their HTML and fail during adoption. Template fragments and noscript's
mode-dependent content need defined lifecycle and serialization contracts;
the obsolete text elements have no ordinary child-container semantics. Managed
executable scripts also need an explicit resource lifecycle. These specialized
bindings remain outside the currently supported native element set.

## Media properties

For native `audio` and `video`, `Attr("muted", "true")` sets the reflected
`defaultMuted` declaration and live `muted` property. `"false"` or removing a
previous declaration clears both. Changing a declaration updates live mute;
unrelated renders preserve native choices. Adopting an unchanged server default
also preserves a native mute choice made before the framework starts. This uses
the separate [live and default media properties](https://html.spec.whatwg.org/multipage/media.html#dom-media-defaultmuted).

Native playback, time controls and media events continue to belong to the browser.
Keyed movement preserves playback; removal pauses owned media. A retained-HTML
close keeps native playback available. This adds no audio engine or playback task.

## Checkbox properties

`ui.Element("input").Attr("type", "checkbox").Indeterminate(true)` controls the
native checkbox's mixed presentation. It is independent of checkedness: use the
existing checked binding/default for the submitted value and native validity.
`Attr("indeterminate", "true")` remains an ordinary ineffective HTML attribute;
use the typed property method. Omitting the method releases control and preserves
the current native property. A binding on any other input type is rejected.

Native click and Space clear mixed presentation while toggling checkedness.
`Event.Checked` reports that new checked value. A controlled property settles
after the input is acknowledged; pending native edits survive older commits.
For a selection group, derive mixed presentation from child selections, and set
the children together in the master's change callback. A constant true reader
intentionally restores mixed presentation after acknowledgement. Controlled form
reset uses the same owner values; reset application state in an on-reset handler
when desired. Removal releases the existing node/listener ownership.

Server HTML carries the intended property in `data-vo-indeterminate`; the native
mixed property activates when the host commits the first frame. HTML alone cannot
set it. Provide visible selection summaries for pre-activation/no-script content,
as the example does. Changed checkedness before adoption is replayed through the
existing input route and survives the initial commit. This captures final native
values; the earlier event history is unavailable.

The wire advances to v20 with an appended `property` operation. It currently
admits only native checkbox `indeterminate` with true/false and explicit release;
unknown property names/values and incompatible final input types fail preflight.
There is no arbitrary browser object setter. The public contract follows the
[HTML checkbox property](https://html.spec.whatwg.org/multipage/input.html#dom-input-indeterminate)
and the [mixed checkbox pattern](https://www.w3.org/WAI/ARIA/apg/patterns/checkbox/).
