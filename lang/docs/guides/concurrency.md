# Goroutines, channels, and islands

Volang separates lightweight cooperative concurrency inside an island from
parallel or isolated execution across islands. That distinction keeps local
coordination cheap and cross-thread or cross-instance authority explicit.

## Goroutines

`go` schedules a function in the current island:

```vo
go refreshCache()
```

Goroutines share the island heap and scheduler. They yield at defined runtime
boundaries such as channel operations, explicit scheduler calls, waits, and
host effects. Code must not assume a particular interleaving.

Keep goroutine ownership visible. A component, request, session, or service
should own a cancellation scope and wait for or deliberately detach the work
it starts.

## Channels

Channels are typed, island-local coordination objects:

```vo
jobs := make(chan string, 8)
results := make(chan int, 8)

go func() {
    for job := range jobs {
        results <- len(job)
    }
}()
```

An unbuffered send rendezvous with a receiver. A buffered channel allows up to
its capacity before a sender blocks. Closing announces that no more values will
be sent; receivers can use the two-result form to distinguish a zero value from
closure.

The goroutine that owns the sending lifecycle should normally own `close`.
Closing twice or sending after close is a program error.

## Select

`select` waits on multiple channel operations. Include cancellation or timeout
when a wait belongs to a bounded operation:

```vo
select {
case value := <-results:
    println(value)
case <-ctx.Done():
    return ctx.Err()
}
```

Ready-case selection follows the scheduler contract. Business correctness
must not depend on one ready case always winning.

## Cancellation

The `context` standard package carries cancellation, deadlines, and request
values across an owned call tree. Pass the context explicitly as the first
parameter and check it at blocking or expensive boundaries.

For interactive work, use a generation or request ID in addition to
cancellation. Cancellation limits waste; the generation check prevents a late
completion from overwriting newer state.

```vo
generation := beginRequest()
go func() {
    result, err := load(ctx)
    if currentGeneration() != generation {
        return
    }
    publish(result, err)
}()
```

## Islands and ports

An island owns a VM instance, heap, goroutine scheduler, GC state, and host
capability boundary. Native hosts may run islands on different threads; browser
hosts may run them in different Wasm instances or workers.

Heap references and channel values never cross directly. Ports carry encoded,
validated messages between islands. This makes ownership, serialization,
backpressure, failure, and host routing part of the contract.

Use an island when work benefits from parallel execution, fault isolation,
separate resource limits, or a different host authority. Keep frequent fine-
grained state changes inside one island.

## UI applications

The UI Island owns component state and commits. Background goroutines perform
I/O, compilation, search, preview builds, and file watching, then publish a
small result after cancellation and generation checks. Renderer and platform
callbacks enqueue typed events; they do not mutate component state directly.

## Failure and shutdown

Design shutdown from the owner outward:

1. stop accepting new work;
2. cancel owned contexts;
3. close sender-owned channels or ports;
4. drain or join required workers;
5. release host capabilities and external resources.

The runtime reports deadlock only under the normative scheduler conditions.
Applications should still provide operation deadlines and diagnostics so a
blocked external host call is distinguishable from an internal channel cycle.
