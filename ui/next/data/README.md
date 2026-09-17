# Shared request data

Optional pure-Vo request sharing and bounded inactive-result caching. Import
`github.com/vo-lang/ui/next/data`. The client belongs to one component scope;
pass it to that scope's descendants directly or through context. Independent
roots and server requests must use independent clients.

```vo
client := data.NewClient(scope, "search-client", data.Options{
    FreshMilliseconds: 30000,
    RetainedEntries: 32,
    RetainedBytes: 4 * 1024 * 1024,
})
result := data.Use(scope, "results", client,
    web.FetchText("/api/search?q=" + url.QueryEscape(query.Get())).WithTimeout(5000))
```

Declare the client on every owner render while consumers use it. Options
initialize that client once; changing its owner key creates a new lifetime.
`Scope.Contains` enforces ownership without exposing internal root identities.
Keep client and observer keys distinct within the scope. The package also reserves
`data-guard:<client-key>`, `data-client:<client-key>` and `data-query:<observer-key>`
for its state and effect declarations.

`Use` returns a value snapshot with `Value`, `Error`, `HasValue`, `Fetching` and
`Stale`. Results are immutable strings; consumers decode their own typed values.
The exact request service, payload and timeout form the cache identity. Include
all result-affecting parameters in the request payload, and invalidate or replace
the client when external context changes. This API shares idempotent reads;
submissions belong in tasks or the forms package.

Multiple observers of the same request share one pending task. A parameter change
immediately reads the new request's snapshot, including on the first render.
Existing successful data stays available during refresh and after a failed refresh.
`HasValue` distinguishes an empty successful response from an absent result.
The first render reports `Fetching: true` for an absent entry, but starts no work
until commit. SSR produces that loading view when no initial result was supplied.

`client.Invalidate(request)` marks that exact request stale and refreshes it when
observed. `InvalidateAll()` does this for every cached request. Each explicit
invalidation replaces pending work; observers still share the replacement.
Use these methods in handlers or post-commit work. Render-time calls are rejected
before cancellation or cache changes, so an error boundary cannot disrupt its
parent's requests through an invalid child render.

`FreshMilliseconds` controls successful-data freshness. Zero means immediately
stale. Freshness is checked on reads and subscriptions; a new subscription to a
stale entry starts a refresh. There is no expiration timer, background polling,
window-focus refresh or automatic retry. Use explicit invalidation when needed.
`Options.Now` optionally supplies a millisecond clock for deterministic tests;
the default uses the current time. A clock moving backwards makes the value stale.

Set `Immutable: true` for versioned content whose identity changes whenever its
bytes change, such as a document URL containing its content digest. Successful
values then stay fresh regardless of age or clock changes; `FreshMilliseconds`
is ignored. Explicit invalidation still refreshes active consumers, and normal
retention limits, errors and cancellation still apply. This also lets static
pages transfer older build-time snapshots without an immediate duplicate read.

Removing one observer leaves other observers' task running. Removing the last
observer cancels pending work and retains completed data or an error. Cancelled
and disposed task generations reject late results. Disposing the owner cancels
all pending work and releases the cache. Provider cancellation remains subject to
the underlying `ui.Request` service's AbortSignal contract.

Inactive entries use a small least-recently-used queue, bounded by both
`RetainedEntries` and `RetainedBytes`. Zero selects 32 entries and 4 MiB respectively;
negative limits are rejected. The byte count includes request service/payload and
result/error strings. It excludes object overhead and active queries, so it is an
inactive retention budget, not a whole-application heap limit. Oversized inactive
results are evicted immediately. A cancelled request with no result/error is removed.

## Intentional prefetch

Call `client.Prefetch(request)` from a handler or post-commit effect to warm a
likely next query, such as a detail link receiving pointer or keyboard focus.
It uses the same request identity, task and cache entry as `data.Use`. A consumer
can adopt an in-flight request across a route change or read its completed result.
Fresh results and repeated prefetch calls reuse their entry. Configure a positive
freshness interval when completed prefetches should avoid immediate revalidation.

`Options.PrefetchLimit` limits pending background pins to four by default, with
explicit values from 1 to 32. `Prefetch` returns true for cached, already pinned
or newly admitted requests, and false when the background budget is full. There
is no waiting queue or automatic retry. An existing observed request can acquire
a pin without starting another request; it then survives removal of its ordinary
observers. Normal visible queries continue to use their existing lifecycle and
are outside this background admission limit.

Completion or failure releases the background slot. Unobserved results return
to the existing count/byte LRU; oversized results can be evicted immediately.
Fresh prefetch hits advance inactive recency. `Invalidate` and `InvalidateAll`
replace live prefetched work using the same cancellation and generation rules.
`CancelPrefetch(request)` releases that request's shared background pin; ordinary
observers keep their pending work alive. Removing the client owner cancels all
work. Repeated calls share one pin, so cancellation applies to the client/request
identity rather than an individual caller. Use a request timeout to bound slow
background work. No result callback or extra view subscription is required.

The [Fieldnotes template](../templates/fieldnotes/README.md) prefetches detail data
on link intent. Its tests hold a real response across navigation and verify one
request serves both the link intent and the detail page. The native
[prefetch contracts](../tests/runtime/data_prefetch.vo) cover admission, ownership,
error/cancel recovery, revalidation, render guards and normal cache eviction.

See [the shared search example](../examples/workbench/search.vo) for two consumers,
cache reuse across unmounts, refresh, timeout and local error handling. Native
contracts live in [runtime/data.vo](../tests/runtime/data.vo); real HTTP contracts
live in [data-contracts.mjs](../../../eng/ui-next/data-contracts.mjs).

## Server initial values

Prepare successful reads in request-scoped server application code, preserving
their original completion time. Pass the same snapshot to the server render and
the browser's first render:

```vo
initial := []data.InitialValue{{
    Request: web.FetchText("/api/items").WithTimeout(5000),
    Value: responseBody,
    UpdatedAtMilliseconds: completedAt,
}}
encoded, err := data.EncodeInitial(initial)
// Handle err, then transfer encoded with the rendered HTML.
// In the browser, DecodeInitial(encoded) supplies Options.Initial.
client := data.NewClient(scope, "items-client", data.Options{
    Initial: initial,
    FreshMilliseconds: 30000,
})
```

`Initial` is copied once when the client is created. Later option changes do not
overwrite live cache results. Empty successful strings remain successful values.
The complete request identity must match the consumer, including its timeout.
Both server and first browser render expose the supplied value with `HasValue`
and without `Fetching` or `Stale`, so elapsed time cannot change the hydration
markup. After commit, subscriptions check the original timestamp: fresh values
avoid another request; stale values share one refresh while retaining their data.
A server/client clock disagreement can cause early revalidation.

`EncodeInitial` / `DecodeInitial` use version 1 JSON and reject invalid requests,
duplicate identities, invalid timestamps, more than 256 entries or more than
4 MiB of encoded data. Initial values must also fit the client's configured
retention budget; oversized initial caches fail before rendering. Only successful
string values and their request identities/timestamps travel. Tasks, callbacks,
errors and cache owners remain local to each request or browser root.

The HTML adapter must embed this as data and escape `<` in any JSON script
content. The [workbench fixture server](../../../eng/ui-next/server.mjs) demonstrates
this handoff using its in-memory data source. Native contracts additionally cover
malformed snapshots, retention limits, isolated first renders, empty results and
same-commit subscriber synchronization in
[data_initial.vo](../tests/runtime/data_initial.vo). All three browser engines
exercise fresh/stale initial data on Wasm VM.

Automatic server prefetch, route snapshot merging, pagination helpers, optimistic
writes, persistence and background refresh policies remain future capabilities.
