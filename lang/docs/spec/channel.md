# Channel and Port Specification

## Overview

Vo has **two distinct communication primitives**:

- `chan T` for **island-local synchronization**
- `port T` for **cross-island message delivery**

This split is intentional.

- A **channel** is a local synchronization object with local waiters and local queue state.
- A **port** is a mailbox-like object whose **receive side stays on one home island**, while
  **send capability may be transferred to other islands**.

Transparent cross-island channel proxying is **not** part of the language design.

`select` is therefore a **local wait primitive**:

- it may wait on local channels
- it may wait on local receive ports
- it does not perform distributed arbitration across remote synchronization objects

---

## Channels

### Type Syntax

```go
chan T          // bidirectional local channel
chan<- T        // send-only local channel
<-chan T        // receive-only local channel
```

Channels are reference types. The zero value of a channel is `nil`.

### Direction

| Type | Send | Receive | Close | Cross-Island Transfer |
|------|------|---------|-------|-----------------------|
| `chan T` | ✅ | ✅ | ✅ | ❌ |
| `chan<- T` | ✅ | ❌ | ✅ | ❌ |
| `<-chan T` | ❌ | ✅ | ❌ | ❌ |

A bidirectional `chan T` is implicitly convertible to either `chan<- T` or `<-chan T`.
The reverse is not allowed.

### Locality

Channels are **always island-local**.

- A channel may be shared between goroutines running on the same island.
- A channel may **not** be transferred to another island.
- Capturing a channel in `go(island)` is a compile error.

This rule is semantic, not an optimization hint.

### Creation

```go
ch := make(chan int)       // unbuffered channel
ch := make(chan int, 10)   // buffered channel
```

**Constraints**:

- Capacity must be non-negative. Negative capacity is a runtime error.
- Element type `T` is unconstrained at channel creation time.

### Operations

#### Send

```go
ch <- value
```

- Blocks if the buffer is full and no receiver is waiting.
- Blocks forever if `ch` is `nil`.
- Panics if `ch` is closed.
- Is a compile error on a receive-only channel.

#### Receive

```go
v := <-ch
v, ok := <-ch
```

- Blocks if the buffer is empty and no sender is waiting.
- If the channel is closed and empty, returns the zero value of `T` and `ok == false`.
- Blocks forever if `ch` is `nil`.
- Is a compile error on a send-only channel.

#### Close

```go
close(ch)
```

- Marks the channel as closed.
- Blocked receivers wake and continue draining buffered values.
- Once the buffer is empty, receives return the zero value of `T` and `ok == false`.
- Blocked senders wake and panic.
- Panics if `ch` is `nil`.
- Panics if `ch` is already closed.
- Is a compile error on a receive-only channel.

#### Length and Capacity

```go
len(ch)
cap(ch)
```

Both return `0` if `ch` is `nil`.

### Select on Channels

Channels support full local `select` semantics:

```go
select {
case v := <-ch1:
    // received from ch1
case ch2 <- value:
    // sent to ch2
default:
    // no case ready
}
```

Semantics:

1. All operands are evaluated exactly once.
2. If one or more cases are ready, one ready case is chosen uniformly at random.
3. If no case is ready and `default` is present, `default` runs.
4. If no case is ready and `default` is absent, the goroutine blocks.

### Range over Channel

```go
for v := range ch {
    // use v
}
```

This continues until `ch` is closed and drained.

---

## Ports

### Purpose

A port is the cross-island messaging primitive.

- The **receive side** of a port stays on one island.
- The **send side** may be copied and transferred to other islands.
- Waiting happens only on the receive side, which is why receive ports can participate in local
  `select`.

Conceptually, a port is a **typed mailbox**, not a transparent remote channel.

### Type Syntax

```go
port T          // local owner endpoint
port<- T        // send-only capability
<-port T        // receive-only local view
```

Ports are reference types. The zero value of a port is `nil`.

### Direction and Transfer

| Type | Send | Receive | Close | Cross-Island Transfer |
|------|------|---------|-------|-----------------------|
| `port T` | ✅ | ✅ | ✅ | ❌ |
| `port<- T` | ✅ | ❌ | ❌ | ✅ if `T` is sendable |
| `<-port T` | ❌ | ✅ | ❌ | ❌ |

A `port T` is implicitly convertible to `port<- T` and `<-port T`.
The reverse is not allowed.

Only the send capability may cross an island boundary.

### Creation

```go
p := make(port int, 16)
```

Ports are **always buffered mailboxes**.

**Constraints**:

- Port capacity must be positive.
- The element type `T` is unconstrained at creation time.
- If a send capability is transferred across islands, values of `T` must be sendable.

### Operations

#### Send

```go
p <- value
```

Valid on `port T` and `port<- T`.

- Blocks if the mailbox is full.
- If the sender is on another island, the value is serialized at the island boundary and delivered
  to the home island mailbox.
- Blocks forever if `p` is `nil`.
- Panics if `p` is closed.
- Is a compile error on `<-port T`.

#### Receive

```go
v := <-p
v, ok := <-p
```

Valid on `port T` and `<-port T`.

- Blocks if the mailbox is empty.
- If the port is closed and empty, returns the zero value of `T` and `ok == false`.
- Blocks forever if `p` is `nil`.
- Is a compile error on `port<- T`.

#### Close

```go
close(p)
```

Only the local owner endpoint `port T` may be closed.

- Closing a port prevents future sends.
- Buffered values remain receivable.
- Once the mailbox is drained, receives return the zero value of `T` and `ok == false`.
- Blocked senders wake and panic.
- Panics if `p` is `nil`.
- Panics if `p` is already closed.
- Is a compile error on `port<- T` and `<-port T`.

#### Length and Capacity

```go
len(p)
cap(p)
```

These operations are defined on local receive-capable endpoints (`port T`, `<-port T`).
They return `0` for `nil`.

### Select on Ports

Ports participate in `select` **only through receive cases**:

```go
select {
case msg := <-inbox:
    // received from local inbox
case v := <-ch:
    // received from local channel
default:
    // no case ready
}
```

Rules:

- Receive on `port T` or `<-port T` is allowed in `select`.
- Send on `port T` or `port<- T` is **not** allowed in `select`.
- `select` remains local: it waits on local wait queues only.

This keeps port waiting semantics simple and avoids distributed select protocols.

### Range over Port

```go
for msg := range inbox {
    // receive until closed and drained
}
```

`range` is valid on receive-capable local ports (`port T`, `<-port T`).

### Delivery Failure

Transport failure is **not** equivalent to a clean `close`.

- A normal `close` means the local owner closed the mailbox.
- Delivery failure means the runtime could not route a message or could not reach the home island.

The exact error surface is implementation-defined, but delivery failure must remain observably
distinct from `close`.

---

## Islands and Transfer Rules

### Island Model

An island is an independent VM instance with its own heap, scheduler, and stacks.
Islands do not share memory.

```go
isl := make(island)
go(isl) func() {
    // runs on island isl
}()
```

### What May Cross an Island Boundary

- `chan T` may **not** cross an island boundary.
- `port T` may **not** cross an island boundary.
- `<-port T` may **not** cross an island boundary.
- `port<- T` **may** cross an island boundary if `T` is sendable.

### Example — compile error: channel capture

```go
ch := make(chan int, 1)

go(isl) func() {
    ch <- 42
}()
```

This is a compile error because `chan int` is island-local.

### Example — compile error: receive port capture

```go
reply := make(port int, 1)

go(isl) func() {
    v := <-reply
    _ = v
}()
```

This is a compile error because the receive side of a port is local to its home island.

### Example — valid cross-island send

```go
reply := make(port int, 1)

go(isl) func(out port<- int) {
    out <- 42
}(reply)

v := <-reply
_ = v
```

### Example — request/reply shape

```go
type Request struct {
    value int
    reply port<- int
}

// Assume `requests` is a `port<- Request` capability published by a worker island.
reply := make(port int, 1)

requests <- Request{value: 41, reply: reply}

v := <-reply
_ = v // 42
```

---

## Sendable Types for Cross-Island Transfer

Sendability matters only for values that may cross island boundaries, such as:

- values captured by `go(island)`
- values sent through `port<- T`

### Sendable Values

| Type | Sendable | Reason |
|------|----------|--------|
| `bool`, `int`, `float`, `rune`, `string` | ✅ | Scalar / immutable |
| `[]T` where `T` is sendable | ✅ | Deep-copied |
| `[N]T` where `T` is sendable | ✅ | Deep-copied |
| `*T` where `T` is sendable | ✅ | Pointed object deep-copied |
| `struct` where all fields are sendable | ✅ | Deep-copied |
| `map[K]V` where `K` and `V` are sendable | ✅ | Deep-copied |
| `any` / `interface{}` | ⚠️ | Runtime check on the dynamic value |
| `func` / closures | ❌ | May capture island-local state |
| `chan T` | ❌ | Channels are local synchronization objects |
| `port T` | ❌ | Owns a local receive side |
| `<-port T` | ❌ | Receive capability is local |
| `port<- T` | ✅ | Send capability may be transferred |
| `island` | ❌ | Represents a VM instance |

### Example — compile error: non-sendable element type

```go
out := make(port func(), 1)

go(isl) func(dst port<- func()) {
    dst <- myFunc
}(out)
```

This is a compile error because `func()` is not sendable.

---

## Nil Endpoint Behavior

| Operation | Nil `chan` | Nil `port` |
|-----------|------------|------------|
| `x <- v` | Blocks forever | Blocks forever |
| `<-x` | Blocks forever | Blocks forever |
| `close(x)` | Panics | Panics |
| `len(x)` | Returns `0` | Returns `0` |
| `cap(x)` | Returns `0` | Returns `0` |
| `select` case on `x` | Never ready | Never ready |

---

## Comparison

Channels and ports support equality comparison:

```go
ch1 == ch2
p1 == p2
ch1 == nil
p1 == nil
```

Equality is by endpoint identity.

Channels and ports are not ordered. `<`, `>`, `<=`, and `>=` are compile errors.

---

## Design Summary

| Aspect | Channel | Port |
|--------|---------|------|
| Role | Local synchronization | Cross-island message delivery |
| Types | `chan T`, `chan<- T`, `<-chan T` | `port T`, `port<- T`, `<-port T` |
| Island boundary | Never crosses | Only `port<- T` crosses |
| Buffering | Unbuffered or buffered | Always buffered |
| Waiters live on | Local channel | Local receive side |
| Select | Full local send/recv support | Receive-only in local `select` |
| Close | Owner closes | Local owner closes |
| Transfer rule | Not sendable | Only send capability is sendable |
| Failure model | Local close / panic semantics | Close remains distinct from transport failure |
