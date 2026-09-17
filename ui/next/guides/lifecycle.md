# A place for every effect.

Render describes the interface. External work belongs to handlers, committed
effects and owned requests. Results return through the root's serialized UI
writer, so background work does not mutate mounted state directly.

## Run work after commit

Declare `ui.Effect` during render. Its setup runs after that view is committed;
return a cleanup function to release the owned resource.

```vo
ui.Effect(scope, "ready", func() func() {
    inputRef.Focus()
    return nil
})
```

Keep the effect key stable. An unchanged effect does not restart simply because
its component renders again. Explicit dependencies select when it must be
replaced. Removing the declaration or disposing the component runs its cleanup.
Use a stable element ref for focus and measurement; request those operations
through the framework's post-commit element operations.

## Let data follow its input

A `Resource` describes a request from the current render. It starts after commit,
cancels replaced work and ignores results from older requests.

```vo
load := ui.Resource(scope, "search",
    web.FetchText("/api/search?q=" + url.QueryEscape(query.Get())).WithTimeout(5000))
result := load.Get()
```

Use `Pending`, `Value` and `Error` to render progress, success or a local error.
A timeout reports a normal error and aborts the host operation. `load.Reload()`
retries the current request from a handler. Keep errors next to the action or
content they affect and provide a retry when it can help.

Use the optional `data` package when multiple components should share a request,
a cache entry or invalidation. Each client belongs to its root or request scope;
server requests must not share mutable client state accidentally.

## Start an explicit action

Use `ui.Start` in a handler or committed effect for one-off work. Keep the returned
task when the interface offers cancellation.

```vo
task := ui.Start(scope, web.After(1000, "ready").WithTimeout(2000), func(result ui.Result) {
    if result.Error != "" {
        status.Set(result.Error)
        return
    }
    status.Set(result.Value)
})
// In a later cancellation handler:
task.Cancel()
```

The callback runs through the UI writer while its scope is live. Cancellation
and scope disposal prevent later results from reaching it. Cancellation is
idempotent. A handle used by a later handler must survive component renders;
keep it in component-owned storage instead of recreating a local variable on
every render.

## Own subscriptions

`ui.Watch(scope, key, request, receive)` declares successive host values. Its
callback refreshes on render. Replacing the request, removing the declaration or
disposing the scope cancels the source. `ui.Subscribe` provides the imperative
variant for handlers and committed effects.

```vo
ui.Watch(scope, "page", ui.Request{Service: "web.page-active"}, func(result ui.Result) {
    active.Set(result.Error == "" && result.Value == "true")
})
```

A subscription stays active across normal values. A terminal error retires it.
Host providers release their listeners when the abort signal fires. Applications
use typed request helpers where available and keep provider details behind the
managed host boundary.

## Inspect owned work

The development panel lists active and recent terminal tasks. Each record shows
its component, service, waiting/completed/failed/cancelled status, accepted replies,
timeout and elapsed observation time. A terminal subscription error counts as a
reply. Repeated cancellation and stale results do not create extra records.

Time starts when the guest queues the request and includes guest/host waiting.
It does not establish when a provider began executing. If the inspector attaches
later, it shows a partial record measured since attachment and leaves earlier
service and start information unknown. Request bodies and result values stay
outside the record; bounded errors remain visible for diagnosis.

Component disposal cancels its tasks before effect cleanup. Treat cleanup as
resource release, and avoid trying to publish a new component state from an
already disposed scope.

Return to [first steps](first-steps.md) to run and test a complete application.
