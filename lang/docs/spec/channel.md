# Channel Specification

## Overview

Vo provides **channels** (`chan T`) as the unified communication primitive for concurrent programming. Channels enable type-safe message passing between goroutines, both within the same island (zero-copy) and across island boundaries (serialized deep-copy).

There is no separate "port" type. A channel that is transferred to another island automatically becomes a cross-island channel. The runtime handles serialization transparently.

## Type Syntax

```go
chan T          // bidirectional channel of type T
chan<- T        // send-only channel of type T
<-chan T        // receive-only channel of type T
```

Channels are reference types. The zero value of a channel is `nil`.

### Direction

Channel direction restricts the operations available on a channel endpoint:

| Type | Send | Receive | Close |
|------|------|---------|-------|
| `chan T` | ✅ | ✅ | ✅ |
| `chan<- T` | ✅ | ❌ | ✅ |
| `<-chan T` | ❌ | ✅ | ❌ |

A bidirectional `chan T` is implicitly convertible to either `chan<- T` or `<-chan T`. The reverse is not allowed.

Direction constraints apply regardless of whether the channel is local or cross-island.

## Creation

```go
ch := make(chan int)       // unbuffered channel
ch := make(chan int, 10)   // buffered channel with capacity 10
```

**Constraints**:
- Capacity must be non-negative. Negative capacity is a runtime error.
- Element type `T` has no constraints at creation time. Any type is valid, including `func`, `chan`, `any`, etc.

## Operations

### Send

```go
ch <- value
```

- Blocks if the buffer is full and no receiver is waiting.
- Panics if `ch` is nil.
- Panics if `ch` is closed.
- Panics if `ch` is a receive-only channel (compile error).

### Receive

```go
v := <-ch            // receive, block until value available
v, ok := <-ch        // receive with closed check
```

- Blocks if the buffer is empty and no sender is waiting.
- If channel is closed and buffer is empty: returns zero value of `T`, `ok` is `false`.
- Panics if `ch` is nil.
- Panics if `ch` is a send-only channel (compile error).

### Close

```go
close(ch)
```

- Marks the channel as closed. No more values can be sent.
- All blocked receivers are woken and receive zero values.
- All blocked senders are woken and panic.
- Panics if `ch` is nil.
- Panics if `ch` is already closed.
- Panics if `ch` is a receive-only channel (compile error).

### Length and Capacity

```go
len(ch)    // number of elements currently buffered
cap(ch)    // buffer capacity
```

Both return `0` if `ch` is nil.

## Select

The `select` statement waits on multiple channel operations simultaneously.

```go
select {
case v := <-ch1:
    // received v from ch1
case ch2 <- value:
    // sent value to ch2
default:
    // no channel ready
}
```

### Semantics

1. All channel operands are evaluated.
2. If one or more cases are ready, one is chosen **uniformly at random** (Go semantics).
3. If no case is ready and `default` is present, `default` executes.
4. If no case is ready and no `default`, the goroutine blocks until any case becomes ready.

### Nil Channels in Select

A `nil` channel in a select case is never ready. This is useful for dynamically enabling/disabling cases:

```go
var ch1 chan int = nil  // disabled
ch2 := make(chan int)

select {
case v := <-ch1:   // never selected (ch1 is nil)
case v := <-ch2:   // can be selected
}
```

## Range over Channel

```go
for v := range ch {
    // receive values until ch is closed
}
```

Equivalent to:

```go
for {
    v, ok := <-ch
    if !ok {
        break
    }
    // use v
}
```

## Islands and Cross-Island Channels

### Island Model

An island is an independent VM instance with its own GC heap, goroutine scheduler, and stack space. Islands run on separate OS threads and do not share memory.

```go
isl := make(island)
go(isl) func() {
    // runs on island isl
}()
```

### Channel Transfer

When a channel is captured by a `go(island)` closure, the runtime automatically establishes cross-island communication:

```go
ch := make(chan int, 1)

go(isl) func() {
    ch <- 42        // cross-island send (serialized)
}()

v := <-ch           // cross-island receive (deserialized)
```

The user does not need to take any special action. The channel automatically transitions from local to cross-island when transferred.

### What Happens at Transfer

1. The **home island** (where the channel was created) retains the channel's queue state (buffer, waiters).
2. The receiving island gets a **remote proxy** — a lightweight handle that forwards operations to the home island via message passing.
3. Send and receive operations on the remote proxy are serialized into messages, sent to the home island, executed there, and results are sent back.

This is fully transparent to user code. A `chan int` behaves identically whether local or remote — it just has different performance characteristics.

### Performance Characteristics

| Scenario | Send Cost | Receive Cost |
|----------|-----------|--------------|
| Same island, buffered | Zero-copy slot move | Zero-copy slot move |
| Same island, direct handoff | Zero-copy + wake receiver | Zero-copy + wake sender |
| Cross-island | Serialize + message + deserialize | Message + serialize + deserialize |

Cross-island operations involve a round-trip message to the home island. Latency depends on transport implementation.

### Element Type Constraints

At channel creation time, element type `T` is unconstrained. However, when a channel is captured by `go(island)`, the compiler checks that `T` is **sendable** — i.e., its values can be serialized for cross-island transfer.

**Sendable types**:

| Type | Sendable | Reason |
|------|----------|--------|
| `bool`, `int`, `float`, `rune`, `string` | ✅ | Scalar / immutable |
| `[]T` where T is sendable | ✅ | Deep-copied |
| `[N]T` where T is sendable | ✅ | Deep-copied |
| `*T` where T is sendable | ✅ | Pointed object deep-copied |
| `struct` where all fields are sendable | ✅ | Deep-copied |
| `map[K]V` where K, V are sendable | ✅ | Deep-copied |
| `any` / `interface{}` | ⚠️ | Allowed at compile time; runtime check on actual value |
| `func` / closures | ❌ | May capture island-local state |
| `chan T` | ✅ | Transferred via remote proxy (not serialized as a value) |
| `island` | ❌ | Represents a VM instance |

**Example — compile error**:

```go
ch := make(chan func())   // OK: creation is fine

go(isl) func() {
    ch <- myFunc          // compile error: chan func() captured by go(island),
}()                       // but func is not sendable
```

**Example — valid cross-island**:

```go
ch := make(chan int, 1)

go(isl) func() {
    ch <- 42              // OK: int is sendable
}()
```

**Example — channel of channels**:

```go
ch := make(chan chan int)

inner := make(chan int)
go(isl) func() {
    ch <- inner           // OK: chan int is sendable (transferred as remote proxy)
}()
```

### Multiple Islands Sharing a Channel

A channel can be shared across multiple islands. Each island that receives the channel gets its own remote proxy, all pointing to the same home island queue.

```go
ch := make(chan int, 10)

for i := 0; i < 4; i++ {
    isl := make(island)
    go(isl) func() {
        for j := 0; j < 100; j++ {
            ch <- j       // all four islands send to same channel
        }
    }()
}

for i := 0; i < 400; i++ {
    v := <-ch             // home island receives all values
}
```

## Nil Channel Behavior

| Operation | Nil Channel |
|-----------|-------------|
| `ch <- v` | Blocks forever (panics in current implementation) |
| `<-ch` | Blocks forever (panics in current implementation) |
| `close(ch)` | Panics |
| `len(ch)` | Returns 0 |
| `cap(ch)` | Returns 0 |

## Comparison

Channels support equality comparison:

```go
ch1 == ch2    // true if same channel instance
ch1 == nil    // true if ch1 is nil
```

Channels are not ordered. `<`, `>`, `<=`, `>=` are compile errors.

## Design Summary

| Aspect | Design |
|--------|--------|
| Type | `chan T`, `chan<- T`, `<-chan T` |
| Creation | `make(chan T)`, `make(chan T, cap)` |
| Operations | Send (`<-`), receive (`<-`), `close`, `len`, `cap` |
| Select | Full support, random case selection |
| Range | `for v := range ch` until closed |
| Cross-island | Transparent — channel auto-proxies when transferred via `go(island)` |
| Element constraint | Unconstrained locally; sendable required when captured by `go(island)` |
| Serialization | Transparent — local is zero-copy, remote is auto-serialized |
| Direction | `chan<- T` (send-only), `<-chan T` (recv-only), `chan T` (bidirectional) |
| Zero value | `nil` |
| Comparison | `==` and `!=` only |
