# Volang Studio

Studio is a small place to explore Volang and its UI framework. Current Web
development uses [next/](next/README.md), with application state, components and
views written in Vo. It remains a preview while the Web rewrite completes its
device, performance and migration checks.

- **Gallery** demonstrates the framework's controls, forms, themes, navigation
  and collection behavior through working examples.
- **Playground** edits and runs console or UI examples, with independent workers,
  Stop, local drafts and an optional source editor.
- **Docs** serves maintained language guides and the new UI introductions, with
  readable initial HTML and chapter links.

The new application excludes Git, accounts and remote workspace management.
Its recovery page exports projects saved by the previous browser Studio without
changing their files. Old URLs and the previous site's cache have an explicit
[upgrade path](../../ui/next/migration.md#studio-旧站升级入口).

## Develop

Follow the [framework runtime prerequisites](../../ui/next/README.md#reproduce)
from the repository root, then start the application:

```sh
node eng/ui-next/cli.mjs dev
```

The command prints the Gallery address. Compatible Vo changes preserve state;
styles update in place, and compilation errors leave the last working page
available. Web Studio executes with Wasm VM. The
[performance report](../../ui/next/performance.md) preserves the measurements
and the retired Core Wasm AOT comparison.

## Build and check

```sh
node eng/ui-next/cli.mjs build --studio --static
```

The output in `target/ui-next/studio-static` is a complete static site, including
rendered pages, application bytecode, the Wasm VM, optional tools and an artifact inventory.
Deploy at an origin root with directory-index and custom 404 support. The
matching request-time distribution is `target/ui-next/studio-distribution`;
its `server/entry.mjs` runs with Node 24 and a matching native Vo compiler.
[Deployment details](next/README.md#independent-production-distribution) cover
compression, caching, source-free operation and upgrade behavior.

`node eng/ui-next/cli.mjs check` runs the framework and application development
checks. The CI core entry is `node eng/ui-next/ci.mjs`, with prerequisites owned
by [the UI rewrite CI lane](../../docs/ci.md). Browser coverage includes
Chromium, Firefox and WebKit, the Web VM, static and native rendering,
editing, worker cleanup, failure recovery and previous-site upgrades.

The [desktop preview](../../ui/next/desktop.md) packages Studio with a native
VM, JIT or Native AOT UI and optional Wasm Playground workers. The
[rewrite plan](../../docs/ui-platform-rewrite-plan-20260913.md) tracks the
remaining Web and desktop acceptance. The new UI CI task owns the
default Pages candidate and its final-directory checks. The local workflow
change takes effect after an authorized successful main CI and deployment.
Native hosts and positional UI commands retain their transition contracts;
their [implementation and operating guide](legacy.md) is preserved separately.
