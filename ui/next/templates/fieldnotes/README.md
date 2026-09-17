# Fieldnotes

A small reading library and preferences application written in Vo. Its fictional
note collection demonstrates URL filters, nested routes, pagination, request
caching, server HTML and form submission. It uses the experimental UI packages
vendored into this project, with separate browser and native server entries.

Use the matching UI toolchain, with its `bin` directory on PATH. Set
`VO_UI_TOOLCHAIN` to the package root when using another matching `vo` executable.

```sh
vo ui dev --project .
vo ui check --project .
vo ui test --project .
vo ui build --project .
vo ui preview --project .
```

Open the reported local URL. The server redirects to the library, and deep
library/detail/preferences links also return their own HTML before WebAssembly
starts. Search, topic, sort and page live in the URL. Browser history preserves
the route, filters and scroll. Search updates preserve input focus and selection.

The browser application lives in `app/`. Shared payloads, query identities and
validation live alongside it in `app/model.vo`. The native `server/` entry owns
the note dataset, search/sort/pagination, JSON responses and preferences cookie.
Client code receives the relevant query results with each server page; it does
not need to ship the catalog or refetch fresh initial data.
Focusing or pointing at a note link starts a bounded detail prefetch. Opening the
note shares that pending request or its fresh cached result. Background reads
use the same client, timeouts and cache budget as visible queries.

Preferences are saved in a small cookie for this browser and deployment prefix.
They demonstrate text/select/checkbox/multiple-choice fields and conditional
validation. The digest choices are sample preferences for an application to
extend with its own delivery service. The form works through a standard POST
without JavaScript, then uses a scoped JSON request after startup. Server errors
and rejected input appear together in HTML and survive client activation; the
saved baseline stays available through Discard changes. Cancellation retires
the pending UI request; a server write that has already finished remains saved.

The shared bootstrap explicitly imports the managed navigation adapter from
`@volang/ui-next`. It creates services for each actual mounted container,
including after development reload. A smaller app can omit this optional import.
The application itself has no JavaScript business logic.

Production output is `target/ui-next/dist/`. Copy the entire directory and run
its `server/entry.mjs` with Node and the matching `vo` executable available.
`BASE_PATH`, `HOST`, `PORT` and `VO_EXECUTABLE` configure the emitted entry.
Keep the running directory immutable; restart against a new complete build to
adopt changes. Public assets, private prepared server bytecode and their report
are delivered together. This remains an experimental starter.
