# Semantic documents

`github.com/vo-lang/ui/next/document` renders versioned content as ordinary Vo
views. Applications can load one chapter with `Resource` or `data.Use`, decode the
result, and insert the view into a styled article:

```vo
decoded, err := document.Decode(result.Value)
if err != nil {
    return ui.Element("p", ui.Text("This chapter could not be opened."))
}
return ui.Element("article", document.View(decoded)).Class("prose")
```

The optional package has no browser service or Markdown parser. `View` takes a
snapshot of the document, including maps and child lists. Native HTML rendering,
Wasm VM use the same semantic tree. Links use the ordinary
navigation adapter when installed; native href behavior remains available before
startup and without JavaScript.

The version 1 envelope has `version`, `headingID`, and `nodes`. Nodes have `tag`,
optional `text`, `attrs`, and `children`. Text belongs to `#text` nodes. Supported
content includes paragraphs, headings, emphasis, links, images, blockquotes,
ordered/unordered lists, code blocks, rules, tables, and labelled disabled task
checkboxes. Tables explicitly contain `thead`/`tbody`, rows and cells so browser
HTML parsing and client rendering create the same structure. A removed leading
document heading is represented by `headingID`; an application can use that ID on
its own title. No root heading is inserted automatically.

`Decode` checks JSON and calls `Validate`. `Validate` also accepts manually built
`Document` values. Unsupported versions, tags, attributes and structural nesting
return errors. Input and content are bounded to 2 MiB, 8,192 nodes and 64 levels;
attributes are bounded to eight per node and 4,096 bytes per value, IDs to 1,024
bytes. IDs must be unique within the document, including `headingID`. Cyclic
manually built child slices terminate at the depth limit. `View` validates manual
input again and panics for invalid data; decode failures should use a local
application error view before calling it.

Styling remains with the application. `pre` can expose `data-language`; table
cells can use `doc-align-start`, `doc-align-center`, or `doc-align-end`. The
[Studio stylesheet](../../../apps/studio/next/studio.css) demonstrates readable
prose, code, lists, responsive tables and constrained images. Syntax highlighting,
interactive Markdown components and full-text indexing are separate extensions.

## Maintained Studio content

`vo-dev generate studio-docs --write` parses maintained Markdown with locked
CommonMark tooling at build time. It emits one metadata catalog and one semantic
JSON asset per chapter under `apps/studio/next/documentation`. The corresponding
JSON index is shared with the local server, and provenance records all source and
output digests. Both the public generator check and Studio builds reject stale
outputs. `vo-dev lint artifacts` additionally checks Git tracking of these files.

Known relative Markdown links resolve to Studio chapter URLs and namespaced
heading anchors. Other relative repository links resolve to their source on
GitHub. Raw HTML is displayed as literal code/text. The existing released UI
chapters remain with that product; experimental UI guides belong to the rewrite.

The native contract suite covers valid content, snapshot isolation, malformed
trees and budgets. Studio's native document fixture decodes and renders every
generated chapter; browser contracts exercise per-chapter loading, cache reuse,
local retry, SSR, input/DOM adoption and long-document mobile layout.

## Reusable content

`Prepare(Document)` and `Parse(encoded)` return a validated `*Prepared` snapshot.
`Parse` decodes and validates once. Its `View()` reuses the immutable descriptors
and skips unchanged component updates; `HeadingID()` exposes the document heading
identity. Keep the prepared value in state or `DerivedMemo`, replacing it when
source data changes. Mutating the original nodes cannot change the prepared view,
and the same prepared value can mount in independent roots. The ordinary `View`
function remains available for callers that intentionally rebuild a snapshot.
