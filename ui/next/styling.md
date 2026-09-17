# Styling components

The experimental Web framework uses ordinary CSS for layout and presentation.
`Class`, `Attr("style", ...)`, pseudo-classes, logical properties, media queries,
container queries and custom properties keep their browser meanings. Stylesheets
are application build assets; component rendering does not inject styles or
maintain a second style registry.

## A component boundary

Attach `StyleScope` to the component's rendered element:

```vo
return ui.Element("article",
    ui.Element("h2", ui.Text(title)).Class("title"),
    content,
).StyleScope("acme-card").Class("card")
```

Use the same package-prefixed name in the stylesheet:

```css
@scope ([data-vui-scope="acme-card"]) to ([data-vui-scope]) {
  :scope { display: grid; gap: 16px; container: card / inline-size; }
  .title { color: var(--card-accent, #376748); font-size: 24px; }
  @container card (max-width: 280px) { .title { font-size: 20px; } }
}
```

`StyleScope` emits one `data-vui-scope` attribute and adds no DOM wrapper. Names
contain 1–96 lowercase ASCII letters, digits or hyphens, beginning with a letter.
They identify the authored style, so every instance shares the same name. Choose
package-prefixed names across libraries; runtime component IDs and list keys are
unrelated. The method accepts element views, including SVG; components and
fragments need to mark their actual rendered roots. Multiple sibling roots can
each carry the same name. No stylesheet is loaded automatically by this method.

The lower bound in this CSS pattern excludes nested marked roots and their
descendants. Unmarked content supplied by the caller belongs to the surrounding
style scope; give it a marker when it needs a separate boundary. Omitting the
`to` clause intentionally lets selectors reach nested components. Markers can
also be authored with `Attr`, which keeps the normal attribute overwrite rules.

`@scope` limits selector matching. Inherited fonts, colors and custom properties
continue across the boundary; this is how nested components share a theme.
Global application CSS also remains effective. The pattern does not provide
Shadow DOM isolation, rename animation names or rewrite selectors. Namespace
keyframes and other global CSS names in library stylesheets. These details follow
the browser's [scope semantics](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference/At-rules/@scope).

## Defaults and overrides

The optional `kit/theme.css` puts all its defaults in the named `vui` cascade
layer. Normal unlayered application CSS overrides those defaults without
increasing selector specificity. Applications that use layers should declare
their order before loading their stylesheets:

```css
@layer vui, components, application;

@layer components {
  @scope ([data-vui-scope="acme-card"]) to ([data-vui-scope]) {
    .action { border-radius: 6px; }
  }
}
@layer application {
  .action { border-radius: 99px; }
}
```

For normal declarations, layer precedence is evaluated before specificity and
scope proximity. Loading more rules into an already declared layer does not
move that layer. Inline declarations and `!important` follow the standard
cascade; important declarations reverse layer order. See the CSS working draft's
[cascade order](https://drafts.csswg.org/css-cascade-6/#cascade-sort-order).
Prefer tokens and ordinary rules for application customization. UIKit's reduced
motion rules intentionally retain `!important` to disable component animation.

Set `.vui` on a theme root and `data-theme="dark"` for its dark defaults. Descendants
inherit the tokens; a nested `.vui` starts another set of defaults. Component
tokens such as `--card-accent` can be set on an individual instance. Native
dialogs and popovers retain their DOM ancestry when displayed in the top layer.
An extension that physically moves nodes into another document/container needs
to arrange its own stylesheet and inherited theme context.

## Browser baseline and evidence

This optional boundary pattern requires native `@scope` support. It has no
legacy-browser selector rewriting fallback. The standalone starter demonstrates
the pattern; applications targeting older browsers can use namespaced ordinary
classes and a CSS build pipeline appropriate to their browser baseline.

The `/?example=styling&backend=vm` example has two instances, a nested
guest, a shared theme and keyed reordering. Its permanent browser contract covers
scope limits including the nested root, CSS layers and late stylesheet loading,
theme inheritance, hover/focus, container/media queries, RTL and 390px layout.
Adding `&ssr` exercises CSS before guest startup, server-node adoption and an
input edited before activation. The automated matrix covers Chromium, Firefox
and WebKit with Wasm VM. Desktop rendering has no equivalent claim.

The light/dark example checks selected accent headings, guest text and button
labels against their actual computed backgrounds at a minimum 4.5 ratio, using
[WCAG contrast calculations](https://www.w3.org/WAI/WCAG22/Understanding/contrast-minimum.html).
This targeted regression does not constitute an accessibility audit of the page
or framework. Keyboard focus uses the same navigation chord as an ordinary
native button in each browser's current keyboard preference.
