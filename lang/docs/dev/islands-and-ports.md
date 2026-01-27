---
Title: Islands & Ports (Cross-VM Concurrency) RFC
Status: Draft
---

# 1. Summary
This RFC defines a cross-VM concurrency model for Vo based on **multiple VM instances**, where:

- Each **`island`** is a first-class **VM instance** that runs on a dedicated OS thread.
- Each island schedules multiple fibers (Vo goroutines).
- `go` is extended to optionally target a specific island: `go @(i) f()`.
- Cross-island communication uses `port T`, a channel-like primitive with **copy/pack** semantics.

Primary goals:

- Preserve Go-style ergonomics (`go`, `<-`, `make`, `close`).
- Make *local vs cross-island* relationships obvious.
- Guarantee **no shared heap objects across islands**.
- Fail fast: invalid cross-island usage is a **compile-time error**.

# 2. Core Concepts

## 2.1 Island

`island` is a language primitive type representing a VM instance.

Properties:

- One island corresponds to:
  - One VM instance
  - One dedicated OS thread
  - One GC/heap
  - One scheduler + fiber set
- Islands do not share heap objects.

Creation:

```vo
var i island
i = make(island)
```

Notes:

- `island` is a builtin primitive type like `chan`, `map`, and `[]T`.
- Builtin primitive types do not have methods.

## 2.2 Local vs Cross-Island

- **Local**: goroutines and `chan T` within a single island.
- **Cross-island**: goroutines scheduled on different islands.

Rules:

- `chan T` is **local-only**.
- `port T` is used for **cross-island** communication.

# 3. Language Surface

## 3.1 Targetable `go`

Existing form:

```vo
go f()
```

New form:

```vo
go @(i) f()
```

Semantics:

- `go f()` schedules `f()` on the **current island**.
- `go @(i) f()` schedules `f()` on the **target island `i`**.

Constraints:

- The expression inside `go @( ... )` must be of type `island`.

Note: The `@` prefix avoids ambiguity with `go (expr)()` (parenthesized call expression).

## 3.2 `port T` (Cross-Island Channel)

`port T` is a new language primitive type that mirrors `chan T` syntax:

```vo
p := make(port int, 8)

p <- 1
x := <-p
close(p)
```

Differences from `chan T`:

- `chan T`: local-only, may carry local heap references.
- `port T`: cross-island, values are copied/packed on send and unpacked on receive.

# 4. Sendability (Compile-Time)

Cross-island boundaries require **sendable** values.

Sendability is checked at compile time for:

- `make(port T, ...)`
- captured variables used by `go @(i) ...`

## 4.1 Capture Rule (Option 2)

`go @(i) func() { ... }` may capture outer variables **only if all captured values are sendable**.

If any capture is not sendable, it is a compile-time error.

## 4.2 v1 Sendable Types

Sendable (deep-copied on send):

- Scalars: `bool`, integers, floats, `rune`
- `string` (copied)
- `[]T` where `T` is sendable (deep-copied payload)
- `[N]T` where `T` is sendable (deep-copied elements)
- `struct` where all fields are sendable (deep-copied recursively)
- `*T` where `T` is sendable (deep-copied pointed object; receiver gets a new pointer)
- `map[K]V` where `K` and `V` are sendable (deep-copied entire map)

Runtime-checked (compile-time allows, runtime verifies):

- `any` / `interface{}` (actual value checked at runtime; panics if contains non-sendable)

Not sendable (compile-time error):

- `chan T` (bound to island scheduler)
- `port T` (bound to island scheduler)
- `island` (represents VM instance)
- function values / closures (may capture island-local state)

# 5. Operational Semantics

## 5.1 `port` send / recv

- `p <- v`:
  - `v` is packed into an island-independent message representation (deep copy).
  - the message is enqueued for a receiver.
- `<-p`:
  - if no message is available, the current fiber blocks and is resumed when a message arrives.
  - the message is unpacked into objects allocated in the receiver island heap.

## 5.2 Scheduling

- Each island has its own scheduler.
- `go @(i) f()` posts a “start goroutine” request to island `i`.

# 6. Examples

## 6.1 Cross-island worker

```vo
i := make(island)
p := make(port int, 8)

go @(i) func() {
    p <- 123
}()

x := <-p
assert(x == 123, "port x")
```

## 6.2 Capture must be sendable

```vo
i := make(island)
msg := "hello"

go @(i) func() {
    println(msg)
}()
```

```vo
i := make(island)
m := map[string]int{}

go @(i) func() {
    _ = m
}()
// compile-time error: capture not sendable
```

# 7. Non-Goals (v1)

- Sharing heap objects across islands.
- Implicit cross-island transfer for `any`.

# 8. Open Questions

- `select` support on `port T`.
- Island lifecycle surface (creation limits, shutdown semantics, waiting).
