# Embed a Volang UI application in an existing Web host

The `vo-web/ui/next` subpath exposes the preview host entry and its
TypeScript contracts. Normal Volang applications continue to use `vo ui create`,
`dev` and `build`; this entry is for a host that already owns its page and loading.

```ts
import { mountUi, type UiApplication } from 'vo-web/ui/next';

const root = document.getElementById('volang-root')!;
const application: UiApplication = mountUi(root, {
  backend: 'vm',
  artifact: '/assets/application.vob',
  loadVm: () => import('vo-web/wasm'),
  hydrate: false,
});
application.done.catch(error => console.error(error));
await application.ready;
// When the host removes its page or view:
application.close();
```

The host serves a matching compiled Vo application and required runtime assets.
Use `hydrate: true` only with the matching server-rendered Vo markup. Serve HTML
as UTF-8. Register the error handler before waiting for readiness: readiness can
resolve false when loading fails or the root closes. `close()` is idempotent and
also cancels a pending artifact load. The owning host decides when to remove its
page or mount a replacement.

Calling `mountUi` immediately binds events declared by matching server HTML.
While the artifact and runtime load, its bounded input queue preserves their
order, including input followed by a button action or form submission. Events
before the host script runs keep native browser behavior; activation recovers
edited native controls but cannot recover earlier button actions. Providers and
application effects start after the first successful commit.

A failed load detaches listeners and preserves server markup and current control
values so the host can retry. Actions from the failed attempt are discarded.
Explicit `close()` clears the owned root, including during startup. Mounting a
second application into that same root fails without disturbing the first;
closing an old application cannot remove its replacement.

For VM execution, pass a `loadVm` callback returning the matching Wasm module;
the mount initializes it. The package's compatibility runtime is available as
`() => import('vo-web/wasm')`; a matching execution-only build can also be used.
Generated Wasm declarations require TypeScript's `ESNext.Disposable` library
when compiling against an older JavaScript target.

Providers for tasks, subscriptions and optional DOM widgets remain scoped
to the mounted root. The entry exposes their types and `createLazyWidget`; callers
explicitly supply heavyweight library loaders. Basic entry imports omit chart and
editor libraries and the compatibility UI kernel.

A browser module host can preserve the package's directory layout and use an
import map. A bundler must carry the referenced Wasm assets as well as JavaScript;
a successful JavaScript build alone does not establish runtime asset delivery.
Compatibility `vo-web/ui` entrypoints retain their existing API. This new subpath
has preview stability and does not grant product certification to the rewrite.

For maintainers, run `npm run build:release` before `npm pack`. The `prepack`
hook checks required outputs and makes wasm-pack's generated directories visible
to npm, including their nested snippets and licenses. Packaging requires that
hook; do not disable lifecycle scripts when creating the archive. Installing the
finished archive requires no lifecycle scripts or additional dependencies.

Widget providers can declare `commitTargets(): readonly Node[] | undefined` on
an instance with `afterCommit`. Changes to those controls or their ancestors
notify the instance after the DOM commit. Structural mutations and new widget
values always notify it, and dependencies are refreshed after the callback.
Return an empty array for a widget that only depends on its own value. Omit the
method, or return `undefined`, when synchronization depends on arbitrary page
layout or document changes; such instances keep receiving every commit. A target
does not implicitly include mutations to its descendants or siblings. Include
all native controls whose attributes affect an association, or retain root-wide
notifications for dependencies such as external label text and associations.
The built-in file control observes its field's inputs. An active editor uses
targeted notifications when it has an explicit accessible label, and retains
root-wide notifications when it derives its name from native labels.

The internal wire is version 25. `DomRenderer`'s event sink accepts an optional
second boolean indicating permission to coalesce latest pointer positions. The
canonical host passes it to `InputQueue.push(event, latest)`. Custom sinks may
retain every sample. Latest delivery merges only consecutive compatible
pointermove events; other events remain FIFO barriers. Input turns are bounded to
128 events and the existing frame byte limit, with host-task yielding between
backlog batches. Size and viewport observers are owned and shared by each root.
