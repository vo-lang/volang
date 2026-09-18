# Volang Studio

Studio showcases Volang UI through Gallery, editable Playground examples and
language/framework documentation. Application state and components are written
in Vo; browser adapters own DOM, storage and worker boundaries.

## Develop and build

From the repository root, follow the [runtime prerequisites](../../ui/next/README.md#reproduce), then run:

```sh
node eng/ui-next/cli.mjs dev
node eng/ui-next/cli.mjs build --studio --static
```

The static site is `target/ui-next/studio-static`. Deploy the complete directory
at the origin root, including its asset inventory and custom 404 page. The root
opens `/studio/gallery`. [Distribution details](next/README.md) describe static,
server and desktop hosts.

Gallery demonstrates components; Playground runs isolated console and UI
examples with Stop, diagnostics and persistent drafts. Documentation is generated
from the maintained Markdown catalog. Git, accounts and project management are
outside Studio's scope.

## Validation

`node eng/ui-next/ci.mjs` checks the framework and Studio in Chromium, Firefox
and WebKit. Native desktop checks validate VM, JIT and Native AOT separately.
See [CI delivery](../../docs/ci.md) for exact prerequisites and artifact evidence.
