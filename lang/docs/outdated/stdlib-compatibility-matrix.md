---
Title: Vo Stdlib Compatibility Matrix
Status: Active
Last-Updated: 2026-02-17
---

# 1. Purpose

This document is the product contract for Vo stdlib scope and compatibility.

It answers:
- Which packages are in scope
- Which compatibility level each package targets
- Which features are intentionally out of scope
- Which packages are blocked or pending design work

`lang/docs/dev-notes/stdlib-design.md` remains the implementation-tradeoff document
(Vo vs native split), while this file is the compatibility/source-of-truth matrix.

# 2. Compatibility Levels

- **Exact**: Public API and core behavior align with common Go expectations.
- **Subset-Compatible**: High-frequency subset is compatible; low-frequency APIs are omitted.
- **Vo-Adapted**: Similar intent, but API/semantics are adapted to Vo constraints.
- **Deferred**: Explicitly planned but not yet exposed.
- **Out-of-Scope**: Not planned for near-term stdlib scope.

# 3. Scope Tiers

## P0 (Must-Have)

`errors, strings, bytes, strconv, unicode/utf8, math, time, io, os, path/filepath, encoding/json, net, net/http, fmt, math/rand, os/exec`

## P1 (Should-Have)

`regexp, sort, slices, maps, encoding/base64, encoding/hex, encoding/toml, io/fs, log, flag, bufio`

## P2 (Not near-term)

`reflect, runtime full parity, plugin, syscall/raw sockets, unsafe-heavy APIs`

# 4. Package Matrix

| Package | Tier | Current Level | Notes |
|---|---:|---|---|
| errors | P0 | Exact | Core error chaining and `errors.Is` pattern are available. |
| strings | P0 | Subset-Compatible | Core operations and reader support implemented; track edge parity gaps separately. |
| bytes | P0 | Subset-Compatible | Core operations implemented, includes buffer/reader support. |
| strconv | P0 | Subset-Compatible | Core parse/format paths present. |
| unicode/utf8 | P0 | Exact | Vo implementation covers mainstream API surface. |
| math | P0 | Subset-Compatible | Broad function coverage with native acceleration where needed. |
| time | P0 | Subset-Compatible | Extended in Phase 1 for arithmetic/comparison/timeout usage. |
| io | P0 | Subset-Compatible | Core interfaces/utilities available. |
| os | P0 | Subset-Compatible | Core file/env/process APIs present. |
| path/filepath | P0 | Subset-Compatible | Mainstream path operations available. |
| encoding/json | P0 | Subset-Compatible | Vo parser/encoder + native helpers implemented. |
| net | P0 | Subset-Compatible | TCP/UDP/Unix + DNS; deadline parity improved in Phase 1. |
| net/http | P0 | Subset-Compatible | Pure-Vo HTTP path + native HTTPS bridge in Phase 1. |
| fmt | P0 | Subset-Compatible | Print/Sprint family and writer APIs (`F*`) available in Phase 1. |
| math/rand | P0 | Vo-Adapted | Core random APIs available; Go `Source/Rand` full surface pending. |
| os/exec | P0 | Subset-Compatible | Core command run/output APIs; fd redirection enabled in Phase 1. |
| regexp | P1 | Subset-Compatible | Native regex-backed implementation for core workflows. |
| sort | P1 | Subset-Compatible | Common typed sort/search paths available. |
| slices | P1 | Vo-Adapted | Typed variants due no-generics language constraint. |
| maps | P1 | Vo-Adapted | Typed variants due no-generics language constraint. |
| encoding/base64 | P1 | Subset-Compatible | Main encode/decode APIs present. |
| encoding/hex | P1 | Subset-Compatible | Main encode/decode APIs present. |
| encoding/toml | P1 | Subset-Compatible | Marshal/unmarshal + dynamic decode present. |
| bufio | P1 | Deferred | Planned; not active in stdlib package list yet. |
| context | P0 | Deferred | Planned with island-aware cancellation semantics. |
| sync | P0 | Deferred | Planned as island-local coordination API. |
| reflect | P2 | Out-of-Scope | Near-term focus is high-frequency runtime-safe APIs. |

# 5. Island Concurrency Boundary (Contract)

- `chan` and future `sync` are **island-local** concurrency tools.
- Cross-island concurrency uses `port` and explicit message passing.
- Stdlib APIs must not silently assume shared-heap cross-island synchronization.

# 6. Update Policy

Any stdlib API addition/change should update this matrix in the same PR:
1. Add/adjust package level.
2. Document compatibility impact.
3. Mark platform caveats when behavior differs by target.
