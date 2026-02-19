---
Title: Vo Stdlib Development Plan
Status: Active
Last-Updated: 2026-02-18
---

# Vo Standard Library — Comprehensive Development Plan

## 1. Overall Goals

### 1.1 Mission Statement

The Vo standard library aims to provide a **production-ready, Go-compatible core library** that covers the most commonly used Go packages, with APIs that are familiar to Go programmers while adapted to Vo's unique constraints (no generics, island concurrency, `?` error handling, `~>` dynamic access).

### 1.2 Detailed Goals

1. **Go API Coverage**: Cover the high-frequency subset of Go's top ~25 most-used standard library packages. Omit rarely-used, overly complex, or platform-inapplicable APIs.

2. **API Consistency**: Public function signatures, type names, and semantic behavior should match Go unless Vo language constraints require adaptation (e.g., typed variants instead of generics, `?` instead of `if err != nil`).

3. **Three-Tier Scope**:
   - **P0 (Must-Have)**: `errors`, `fmt`, `strings`, `bytes`, `strconv`, `unicode/utf8`, `math`, `time`, `io`, `os`, `path/filepath`, `encoding/json`, `net`, `net/http`, `math/rand`, `os/exec`, `context`, `sync`
   - **P1 (Should-Have)**: `regexp`, `sort`, `slices`, `maps`, `encoding/base64`, `encoding/hex`, `encoding/toml`, `bufio`, `log`, `flag`, `io/fs`
   - **P2 (Deferred)**: `reflect`, full `runtime` parity, `plugin`, `syscall`/raw sockets, `unsafe`-heavy APIs

4. **Implementation Quality**:
   - Every exposed function must have at least one test case exercising its primary path and one for its error path.
   - Vo-implemented functions preferred where FFI overhead would dominate (< 100 cycles of work). Native (Rust extern) used where work > 500 cycles, system calls are required, or large data tables are needed.
   - All packages must be usable from both VM and JIT execution modes.

5. **Island Concurrency Contract**:
   - `chan` and future `sync` are **island-local**.
   - Cross-island uses `port` + explicit message passing.
   - No stdlib API may silently assume shared-heap cross-island synchronization.

6. **Platform Support — Capability Layering**:

   Packages are classified by **capability tier**, not a binary core/std split.
   Silent degradation is **forbidden** — if a feature is unavailable on a platform, the call must fail-fast with a clear error or panic, never silently change semantics.

   | Tier | Contract | Environment | Examples |
   |------|----------|-------------|----------|
   | **core-baseline** | Must work in VM, JIT, no_std, and WASM | No OS, no allocator-heavy deps | `errors`, `strings`, `bytes`, `strconv`, `math`, `sort`, `slices`, `maps`, `encoding/hex`, `encoding/base64`, `dyn`, `unicode/utf8` |
   | **core-extended** | Pure Vo, but uses allocator-heavy or complex algorithms; works in VM/JIT/WASM with std allocator | WASM+std, no OS syscalls | `encoding/json`, `encoding/toml`, `fmt`, `regexp` (baseline subset), `log` (baseline subset), `flag` (baseline subset) |
   | **std** | Requires OS support (filesystem, network, process, time) | Full OS | `io`, `os`, `os/exec`, `time`, `net`, `net/http`, `net/url`, `bufio`, `path/filepath`, `math/rand`, `io/fs` |
   | **runtime** | Requires deep VM/scheduler integration | VM internals | `sync`, `context` |

   **Packages with baseline/extended split** (see Section 3 for milestone details):
   - **`regexp`**: core-extended provides a pure-Vo Thompson NFA subset; std-extended adds Rust `regex` crate. Patterns beyond baseline return `ErrUnsupportedSyntax`.
   - **`log`**: core-extended provides `Logger`/`Print*` writing to a runtime-injected sink; std-extended adds file output, stderr, caller/fileline.
   - **`flag`**: core-extended provides `FlagSet.Parse(args []string)` (pure Vo); std-extended adds package-level `Parse()` via `os.Args`.

---

## 2. Current State vs Goals — Gap Analysis

### 2.1 Package-Level Status Summary

| Package | Tier | Goal Level | Current Level | Status | Gap Summary |
|---|---|---|---|---|---|
| `errors` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `New`, `Wrap`, `Is`, `As`, `Unwrap`, `Join` all present |
| `strings` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Replacer`/`NewReplacer`, `IndexFunc`, `LastIndexFunc`, `Title`, `ToValidUTF8`, `Builder`, `Reader`, `Cut`/`CutPrefix`/`CutSuffix` all present |
| `bytes` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `IndexFunc`, `LastIndexFunc`, `Title`, `Buffer`, `Reader` all present; minor edge methods may be missing |
| `strconv` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `AppendFloat`, `QuoteRune`, `QuoteRuneToASCII`, `QuoteRuneToGraphic`, `QuoteToASCII`, `QuoteToGraphic`, `CanBackquote`, `AppendQuote*` variants all present |
| `unicode` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `IsSpace`, `IsPrint`, `IsControl`, `IsPunct`, `IsGraphic`, `IsMark`, `IsNumber`, `IsSymbol`, `IsTitle`, `SimpleFold`, `ToLower`/`ToUpper`/`ToTitle` all present; `In` deferred (requires category tables) |
| `unicode/utf8` | P0 | Exact | Exact | ✅ Complete | Bug fixed: byte-index sign-extension for bytes ≥ 0x80 |
| `math` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | Core surface covered; missing `Remainder`, `Gamma`, `Lgamma`, `Erf`, `Erfc`, `J0/J1/Jn/Y0/Y1/Yn` (low frequency) |
| `math/bits` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | Bug fixed: `Reverse8`/`Reverse16` now use arithmetic instead of buggy lookup table; full surface covered |
| `math/rand` | P0 | Vo-Adapted | Vo-Adapted | ✅ Adequate | `Int63`, `Int31`, `Int31n`, `Int64n`, `ExpFloat64`, `NormFloat64`, `New`/`NewSource`, `Rand` struct all present |
| `time` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Format`, `Parse`, `Date`, `Year/Month/Day/Hour/Minute/Second`, `Truncate`, `Round`, layout constants (`RFC3339`, `DateTime`, etc.), `Weekday`, `Month`, `MarshalJSON`/`UnmarshalJSON`, `MarshalText`/`UnmarshalText`, `Timer`/`Ticker`/`After`/`Tick`/`AfterFunc`, `Location`/`FixedZone`/`LoadLocation`/`In`/`UTC`/`Local` all present |
| `io` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | Core interfaces, `ReadAll`, `Copy`, `CopyN`, `ReadFull`, `LimitReader`, `SectionReader`, `Discard`, `WriteString`, `Pipe`/`PipeReader`/`PipeWriter` all present |
| `os` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `UserHomeDir`, `UserCacheDir`, `UserConfigDir`, `TempDir`, `CreateTemp`, `Symlink`, `Readlink`, `Link` all present |
| `path/filepath` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Clean`, `Join`, `Split`, `Base`, `Dir`, `Ext`, `Abs`, `Rel`, `Walk`, `WalkDir`, `Glob`, `Match`, `EvalSymlinks` all present |
| `encoding/json` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Marshal`, `Unmarshal`, `MarshalIndent`, `Decode` (dynamic), `Encoder`/`Decoder` with streaming, `Valid`, `Compact`, `Indent`, `HTMLEscape`, `Marshaler`/`Unmarshaler` interfaces all present |
| `encoding/toml` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Marshal`, `Unmarshal`, `Decode`/`DecodeString`, `Valid` — Vo-specific addition |
| `net` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Dial`, `DialTimeout`, `Dialer`, `Listen`, `LookupHost`, `LookupIP`, `LookupAddr`, TCP/UDP/Unix types all present; `LookupCNAME/MX/TXT/SRV` deferred |
| `net/http` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Redirect`, `Error`, `NotFound`, `FormValue`, `ParseForm`, `Cookie`, `ServeMux`, `Server`, `FileServer`, `StripPrefix`, `HandlerFunc`, `MaxBytesReader`, `MaxBytesHandler`, `DetectContentType`, `NotFoundHandler`, `RedirectHandler`, `PostForm`, `NewRequestWithContext`, `Flusher`/`Hijacker`/`CloseNotifier` interfaces, `Request.Context`/`WithContext`/`Clone` all present; chunked transfer, keep-alive, TLS server deferred |
| `fmt` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | Print family + Scan family all present; `Stringer`, `GoStringer`, `Formatter`, `State`, `Scanner`, `ScanState` interfaces; Scan functions return `[]any` (Vo-adapted, no pointer args) |
| `os/exec` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Command`, `Cmd.Run/Start/Wait/Output/CombinedOutput`, `LookPath` present |
| `regexp` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | String variants + byte variants (`Find`, `FindIndex`, `FindAll`, `FindSubmatch`, `ReplaceAll`, `ReplaceAllStringFunc`), `String`, `NumSubexp`, `SubexpNames` all present |
| `sort` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Interface`, `Sort`, `Stable`, `IsSorted`, `ReverseInterface`, `Slice`, `SliceStable`, `SliceIsSorted`, typed variants, `Search*` all present |
| `slices` | P1 | Vo-Adapted | Vo-Adapted | ✅ Adequate | `IndexFunc`, `ContainsFunc`, `SortFunc`, `SortStableFunc`, `IsSortedFunc`, `BinarySearchFunc`, `MaxFunc`, `MinFunc`, `Insert`, `Delete`, `DeleteFunc`, `Replace`, `Concat`, `Grow`, `Clip`, typed variants all present |
| `maps` | P1 | Vo-Adapted | Vo-Adapted | ✅ Adequate | `DeleteFunc`, typed variants present |
| `encoding/base64` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | All standard/URL/raw encode/decode present |
| `encoding/hex` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | Core encode/decode present |
| `encoding/csv` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Reader`, `Writer`, `ReadAll`, `WriteAll`, quoted fields, `FieldsPerRecord` validation, `ErrBadPattern`/`ErrFieldCount` |
| `encoding/binary` | P1 | Vo-Adapted | Vo-Adapted | ✅ Adequate | `BigEndian`, `LittleEndian`, `NativeEndian` byte order types, typed `ReadUint8`/`ReadUint16`/`ReadUint32`/`ReadUint64` and `WriteUint*` variants, varint encode/decode (`PutUvarint`, `Uvarint`, `PutVarint`, `Varint`, `AppendUvarint`, `AppendVarint`) |
| `cmp` | P1 | Vo-Adapted | Vo-Adapted | ✅ Adequate | `CompareInt`, `CompareInt64`, `CompareFloat64`, `CompareString`, `OrInt`, `OrString`, `Or`, `MinInt`, `MaxInt`, `MinFloat64`, `MaxFloat64`, `MinString`, `MaxString`, typed `IsZero*` |
| `path` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Clean`, `Base`, `Dir`, `Ext`, `IsAbs`, `Join`, `Split`, `Match` — forward-slash only (use `path/filepath` for OS paths) |
| `dyn` | — | Vo-Adapted | Vo-Adapted | ✅ Complete | Vo-specific package |
| `bufio` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Reader`, `Writer`, `Scanner`, split functions all present |
| `context` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Background`, `TODO`, `WithCancel`, `WithTimeout`, `WithDeadline`, `WithValue`, `WithCancelCause`, `WithoutCancel`, `Cause` all present |
| `sync` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Mutex`, `RWMutex`, `WaitGroup`, `Once`, `Map`, `Cond` (via channels), `Locker` interface, `Pool` all present |
| `log` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `Logger`, `Print*`, `Fatal*`, `Panic*`, flags, `New`, `SetOutput` all present |
| `flag` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `FlagSet`, `Flag`, typed flag types, `Parse`, package-level functions all present |
| `io/fs` | P1 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `FS`, `File`, `DirEntry`, `FileInfo`, `FileMode`, `MapFS`, `WalkDir` all present |
| `net/url` | P0 | Subset-Compatible | Subset-Compatible | ✅ Adequate | `URL`, `Parse`, `String`, `Query`, `PathEscape`, `PathUnescape`, `QueryEscape`, `QueryUnescape`, `ResolveReference`, `JoinPath`, `(*URL).JoinPath` all present |
| `reflect` | P2 | Out-of-Scope | Not Started | 🚫 Out of Scope | — |

### 2.2 Detailed Gap Analysis by Package

#### 2.2.1 `time` — Complete

**Status**: Fully complete. `Format`, `Parse`, `Date`, field accessors (`Year`, `Month`, `Day`, `Hour`, `Minute`, `Second`, `Nanosecond`, `Weekday`, `YearDay`), layout constants (`RFC3339`, `RFC1123`, `DateTime`, `DateOnly`, `TimeOnly`, etc.), `Truncate`, `Round`, `AppendFormat`, `ParseDuration`, `String()`, `Timer`, `Ticker`, `After`, `Tick`, `AfterFunc` (pure Vo goroutines + channels), `Location`, `FixedZone`, `LoadLocation`, `In`, `UTC`, `Local` (timezone via native Rust backend).

**Remaining**: None.

#### 2.2.2 `net/http` — Mostly Complete

**Status**: Basic HTTP/1.1 client (`Get`, `Post`, `Head`, `Client.Do`), full server (`ListenAndServe`, `Server`, `ServeMux`, `HandleFunc`), `Header`, `Request`, `Response`, `ResponseWriter`, `Cookie`/`SetCookie`, `Redirect`, `Error`, `NotFound`, `FormValue`, `ParseForm`, `net/url` package with `URL` struct, HTTPS client support, `FileServer`, `StripPrefix`, `HandlerFunc` adapter.

**Remaining (Deferred)**:
- `http.TimeoutHandler`
- `Transfer-Encoding: chunked` support
- Connection keep-alive (currently forces `Connection: close`)
- TLS configuration on server side
- HTTP/2 (out of scope)

#### 2.2.3 `context` — Complete

**Status**: `Background`, `TODO`, `WithCancel`, `WithTimeout`, `WithDeadline`, `WithValue` all implemented. Island-local design as specified.

#### 2.2.4 `sync` — Complete

**Status**: `Mutex`, `RWMutex`, `WaitGroup`, `Once`, `Map`, `Cond`, `Pool`, `Locker` all implemented as island-local primitives.

#### 2.2.5 `bufio` — Complete

**Status**: `Reader` (with `Read`, `ReadByte`, `UnreadByte`, `ReadRune`, `UnreadRune`, `ReadLine`, `ReadString`, `ReadBytes`, `Peek`, `Buffered`), `Writer` (with `Flush`, `Buffered`, `Available`), `Scanner` with `ScanLines`, `ScanWords`, `ScanBytes`, `ScanRunes` split functions all implemented.

#### 2.2.6 `log` — Complete

**Status**: `Logger`, `Print*`, `Fatal*`, `Panic*`, all flag constants (`Ldate`, `Ltime`, `Lmicroseconds`, `Llongfile`, `Lshortfile`, `LUTC`, `Lmsgprefix`, `LstdFlags`), `New`, `SetOutput`, `SetPrefix`, `SetFlags` all implemented.

#### 2.2.7 `flag` — Complete

**Status**: `FlagSet`, `Flag`, typed holder types (`BoolValue`, `IntValue`, etc.), `Bool`/`Int`/`String`/`Float64`/`Duration` with `Var` variants, `Parse`, `Args`, `NArg`, `NFlag`, `Arg`, `PrintDefaults`, `Usage` all implemented.

#### 2.2.8 `io/fs` — Complete

**Status**: `FS`, `File`, `DirEntry`, `FileInfo`, `ReadDirFS`, `ReadFileFS`, `StatFS`, `SubFS` interfaces; `FileMode` with full bitmask constants; `MapFS` in-memory implementation; `ReadDir`, `ReadFile`, `Stat`, `Sub`, `WalkDir` utility functions; sentinel errors (`ErrInvalid`, `ErrPermission`, `ErrExist`, `ErrNotExist`, `ErrClosed`) all implemented.

### 2.3 Remaining Cross-Cutting Gaps

1. **`fmt.Scan` family**: ✅ Done. `Sscan`, `Sscanln`, `Sscanf`, `Fscan`, `Fscanf`, `Fscanln`, `Scan`, `Scanf`, `Scanln` all implemented. Vo adaptation: returns `[]any` with typed values (since Vo has no pointer-to-primitive types). Native Rust parsing for `nativeSscan`, `nativeSscanf`, `nativeReadLine`.

2. **`sync.Pool`**: Object pooling implemented; `Get`/`Put` without GC-managed freeing (island-local manual pool).

3. **`net` DNS extras**: `LookupCNAME`, `LookupMX`, `LookupTXT`, `LookupSRV` not yet implemented (low priority).

4. **`net/http` server-side gaps**: Chunked transfer, keep-alive, TLS server deferred.

5. **`unicode.In`**: Category range table lookups deferred (requires embedded Unicode tables).

6. **`time.Timer`/`time.Ticker`/`time.After`/`time.Tick`/`time.AfterFunc`**: ✅ Done. Pure Vo implementation using goroutines + channels with generation counter for Stop/Reset correctness.

7. **`os.DirFS`**: ✅ Done. `DirFS(dir string) fs.FS` implemented in pure Vo with `dirFS`, `dirFile`, `dirFileInfo` wrapper types. Bridges `os.FileInfo.ModTime() int64` to `fs.FileInfo.ModTime() time.Time` via `time.Unix()`.

8. **Test Coverage** (current state as of 2026-02-19):
   - Total: 1972 tests passing (991 VM + 981 JIT), 0 failures
   - New test files added this phase: `math/bits`, `unicode/utf8`, `io`, `sort` (Interface), `strings` (IndexFunc/Title), `bytes` (IndexFunc/Title), `regexp` (byte variants), `encoding/csv`, `path`, `binary`, `cmp_test`, `context_extended`, `time_marshal`, `fmt_scan`, `os_dirfs`

### 2.4 WASM / no_std Support — Actual State

WASM support is implemented in `lang/crates/vo-web/runtime-wasm/` (`vo-web-runtime-wasm` crate). The native backend (`vo-stdlib`) gates all OS-dependent modules behind `#[cfg(feature = "std")]` and provides a no-op `register_externs` for the `time` module, deferring to the wasm crate.

**Legend**: ✅ Full  ⚠️ Partial/Degraded  ❌ Not Available

| Package | WASM Status | Notes |
|---------|-------------|-------|
| `errors`, `strings`, `bytes`, `strconv`, `math`, `math/bits`, `sort`, `slices`, `maps`, `unicode`, `unicode/utf8`, `encoding/hex`, `encoding/base64`, `dyn`, `cmp`, `path` | ✅ Full | Always registered; no `cfg(feature="std")` gate |
| `fmt`, `encoding/json`, `encoding/toml`, `io` | ✅ Full | Always registered; pure Rust/Vo, no OS deps |
| `math/rand` | ⚠️ Partial | xoroshiro128++ works, but `auto_seed()` uses a **counter only** (no time/pid/thread) — output is predictable across runs; `Read` not available in wasm |
| `regexp` | ⚠️ Partial | Backed by **JS `RegExp`** (not Rust `regex` crate); most patterns work, but Go-specific syntax (e.g., `(?P<name>...)` named groups, POSIX classes) may differ |
| `time` | ⚠️ Partial | `Now()` works (JS `Date`), mono clock works (`Performance.now`); **`Sleep` panics** ("not supported on wasm"); **timezone externs not registered** — `LoadLocation`, `In`, `Zone`, `localOffsetAt`, etc. will panic with unregistered extern error |
| `os` | ⚠️ Partial | Full file I/O via **in-memory VirtualFS** (JS-side state); `Getenv` returns `""`, `os.Args` = `["wasm"]`, `Exit` no-op, `Pipe` unimplemented, `symlink`/`link` return error |
| `path/filepath` | ⚠️ Partial | Only `EvalSymlinks` has a wasm impl (VFS-backed); all other functions are pure Vo and work fine |
| `bufio`, `log`, `flag`, `io/fs` | ✅ Full | Pure Vo; no wasm-specific issues |
| `net`, `net/url`, `net/http` | ❌ Not Available | No wasm extern implementation; calling any native net function will fail with unregistered extern |
| `os/exec` | ❌ Not Available | No wasm implementation; process spawning not possible in browser |
| `sync`, `context` | ❓ Scheduler-dependent | Island-local goroutines/channels; behavior depends on whether the WASM scheduler supports concurrent fibers |

**Key gaps to address if full WASM support is needed:**
- Timezone: register `localOffsetAt`, `ianaOffsetAt` etc. via JS `Intl.DateTimeFormat` in `vo-web-runtime-wasm/src/time.rs`
- `rand` seeding: use `crypto.getRandomValues` for true entropy
- `time.Sleep`: use a blocking-compatible yield mechanism (JS `setTimeout` callback)

### 2.5 WASM / no_std — Ultimate Target State

The target is **three distinct capability tiers** that compose cleanly, with no silent degradation at any tier boundary.

#### Tier 1: `no_std + alloc` (embedded / bare-metal / custom allocator)

Everything in this tier runs without an OS, without `thread_local!`, and without `std::`. Platform provides nothing except an allocator.

| Package | Status | Notes |
|---------|--------|-------|
| `errors`, `strings`, `bytes`, `strconv`, `math`, `math/bits`, `unicode`, `unicode/utf8`, `sort`, `slices`, `maps`, `encoding/*`, `fmt`, `io`, `bufio`, `log`, `flag`, `dyn`, `cmp`, `path`, `io/fs` | ✅ Works now (minor auditing needed) | Must replace `thread_local!` in `rand.rs` |
| `regexp` | ✅ After W4 | Rust `regex` crate is `no_std`-compatible with `alloc` |
| `math/rand` | ✅ After W5 | Replace `thread_local!` + `AtomicU64`; platform injects entropy via a registered callback |
| `time` | ❌ Excluded | Requires OS clock; no meaningful implementation without OS |
| `os`, `net`, `net/http`, `os/exec` | ❌ Excluded | OS/network required by definition |
| `sync`, `context` | ❓ Future | Requires scheduler; channel semantics depend on coroutine support |

#### Tier 2: WASM / browser

Builds on Tier 1. Browser JS APIs replace OS-level services.

| Package | Target State | Implementation Path |
|---------|-------------|---------------------|
| `time.Now()` | ✅ Done | `js_sys::Date::now()` |
| `time` timezone | ✅ After W1.1 | `Intl.DateTimeFormat().resolvedOptions().timeZone` + `Intl.DateTimeFormat` offset calculation |
| `time.Sleep` | ✅ After W2.1 | Cooperative fiber suspend: scheduler registers a JS `setTimeout` callback that resumes the fiber |
| `time.Timer`/`Ticker` | ✅ After W2.2 | Same as Sleep; goroutine-based implementation works once Sleep works |
| `math/rand` | ✅ After W1.2 | `crypto.getRandomValues` for seeding + refill; true entropy |
| `regexp` | ✅ After W4 | Switch to Rust `regex` crate (compiles to wasm32); drop JS `RegExp` shim; identical behavior to native |
| `os` (VirtualFS) | ✅ Done | In-memory VirtualFS; covers playground use case |
| `os` (real FS) | 🔮 Optional | Browser File System Access API (`showOpenFilePicker`) — gated behind feature flag |
| `net/http` (client) | ✅ After W3 | `fetch` API bridge: `Client.Do` → JS `fetch()`; supports GET/POST/headers/body/response streaming |
| `net/http` (server) | ❌ Not possible | Browsers cannot bind server sockets; out of scope |
| `net` (raw TCP/UDP) | ❌ Not possible | Browser has no raw socket API; out of scope |
| `os/exec` | ❌ Not possible | No process spawning in browser; out of scope |
| `sync`, `context` | ✅ After W2.2 | Verify WASM scheduler handles single-threaded coroutine multiplexing |

#### Tier 3: Native (current full state)

All packages including server-side `net/http`, `os/exec`, filesystem, real timezone database.

#### Package classification summary

```
Tier 1 (no_std+alloc): errors strings bytes strconv math math/bits unicode unicode/utf8
                        sort slices maps encoding/* fmt io bufio log flag dyn cmp path
                        io/fs regexp* math/rand*        (* after W4/W5)

Tier 2 (wasm):          Tier 1 + time os(VFS) math/rand(crypto) regexp(Rust) sync context
                        net/http(client fetch)           (* after W1-W4)

Tier 3 (native):        Everything
```

---

## 3. Development Plan

### Phase 1: Core Completeness (High Priority)

**Goal**: Make all P0 packages reach their target compatibility level. Focus on the most impactful gaps.

#### Phase 1.1: `time` Package Enhancement

**Priority**: Critical — blocks `log`, many user applications
**Effort**: Large

| Task | Description | Impl |
|------|-------------|------|
| 1.1.1 | Add `Month`, `Weekday` types with `String()` methods | Vo |
| 1.1.2 | Implement `Time.Date()`, `Time.Clock()`, accessor methods (`Year`, `Month`, `Day`, `Hour`, `Minute`, `Second`, `Nanosecond`, `YearDay`, `Weekday`) | Vo (Gregorian calendar algorithm) |
| 1.1.3 | Add `Date()` constructor function | Vo |
| 1.1.4 | Add layout constants (`RFC3339`, `DateTime`, `DateOnly`, `TimeOnly`, etc.) | Vo |
| 1.1.5 | Implement `Time.Format(layout) string` | Vo (reference-time parsing) |
| 1.1.6 | Implement `time.Parse(layout, value) (Time, error)` | Vo |
| 1.1.7 | Add `Time.String()` default formatting | Vo |
| 1.1.8 | Add `Time.Truncate(d)`, `Time.Round(d)` | Vo |
| 1.1.9 | Add `Time.AppendFormat(b, layout)` | Vo |
| 1.1.10 | Add `ParseDuration(s string) (Duration, error)` | Vo |
| 1.1.11 | Add `Duration.String() string` | Vo |
| 1.1.12 | Write comprehensive tests | Vo |

**Deferred**: Timezone/`Location` support, `Timer`/`Ticker` (requires goroutine + channel integration).

#### Phase 1.2a: `net/url` Package (New — prerequisite for `net/http`)

**Priority**: Critical — blocks `net/http` `Request.URL` upgrade
**Effort**: Medium

| Task | Description | Impl |
|------|-------------|------|
| 1.2a.1 | Implement `url.URL` struct: `Scheme`, `User`, `Host`, `Path`, `RawPath`, `RawQuery`, `Fragment` fields | Vo |
| 1.2a.2 | Implement `url.Parse(rawURL string) (*URL, error)` | Vo |
| 1.2a.3 | Implement `URL.String() string` (round-trip) | Vo |
| 1.2a.4 | Implement `URL.Query() url.Values` (parse query string) | Vo |
| 1.2a.5 | Implement `url.Values` type (`Get`, `Set`, `Add`, `Del`, `Encode`, `Has`) | Vo |
| 1.2a.6 | Implement `url.PathEscape`, `url.PathUnescape`, `url.QueryEscape`, `url.QueryUnescape` | Vo |
| 1.2a.7 | Implement `URL.ResolveReference(ref *URL) *URL` | Vo |
| 1.2a.8 | Implement `URL.Hostname()`, `URL.Port()`, `URL.RequestURI()` | Vo |
| 1.2a.9 | Write comprehensive URL parsing/encoding tests | Vo |

#### Phase 1.2b: `net/http` Enhancement

**Priority**: Critical — HTTP is the #1 use case for network applications
**Effort**: Large
**Depends on**: Phase 1.2a (`net/url`)

| Task | Description | Impl |
|------|-------------|------|
| **URL integration** | | |
| 1.2b.1 | Update `Request.URL` from `string` to `*url.URL` | Vo |
| 1.2b.2 | Update client `Do`/`Get`/`Post` to use `url.URL` for host/path extraction | Vo |
| 1.2b.3 | Update server `parseRequest` to populate `Request.URL` as `*url.URL` | Vo |
| **Server-side request body** | | |
| 1.2b.4 | Implement server-side `Request.Body` reading based on `Content-Length` header | Vo |
| 1.2b.5 | Implement `Request.ParseForm()` — parse URL query + `application/x-www-form-urlencoded` body | Vo |
| 1.2b.6 | Implement `Request.FormValue(key string) string` | Vo |
| **Chunked transfer encoding** | | |
| 1.2b.7 | Implement chunked response body **decoding** in `readResponse` (client reads chunked server responses) | Vo |
| 1.2b.8 | Implement chunked response body **encoding** in `ResponseWriter.Write` (server sends chunked responses when `Content-Length` unknown) | Vo |
| 1.2b.9 | Implement chunked request body decoding on server side (`Transfer-Encoding: chunked` in incoming requests) | Vo |
| **Keep-alive lifecycle** | | |
| 1.2b.10 | Remove forced `Connection: close` on server responses; default to keep-alive for HTTP/1.1 | Vo |
| 1.2b.11 | Implement server-side connection reuse loop (read next request on same conn after response) | Vo |
| 1.2b.12 | Implement client-side connection pooling in `Client` (reuse connections to same host) | Vo |
| 1.2b.13 | Add `Server.ReadTimeout`, `Server.WriteTimeout`, `Server.IdleTimeout` enforcement | Vo |
| **Routing and helpers** | | |
| 1.2b.14 | Improve `ServeMux` pattern matching (method-based patterns: `GET /path`) | Vo |
| 1.2b.15 | Add `http.Redirect(w, r, url, code)` | Vo |
| 1.2b.16 | Add `http.FileServer` and `http.StripPrefix` | Vo + native |
| 1.2b.17 | Add `Cookie` / `SetCookie` support | Vo |
| **Testing** | | |
| 1.2b.18 | Write tests: URL integration (client + server round-trip with `*url.URL`) | Vo |
| 1.2b.19 | Write tests: server body reading + form parsing | Vo |
| 1.2b.20 | Write tests: chunked encoding/decoding (client ↔ server) | Vo |
| 1.2b.21 | Write tests: keep-alive connection reuse + timeout | Vo |

#### Phase 1.3: `context` Package (New)

**Priority**: High — enables idiomatic timeout/cancellation patterns
**Effort**: Medium

| Task | Description | Impl |
|------|-------------|------|
| 1.3.1 | Design island-local `Context` interface | Design |
| 1.3.2 | Implement `Background()`, `TODO()` | Vo |
| 1.3.3 | Implement `WithCancel` | Vo + native (atomic) |
| 1.3.4 | Implement `WithTimeout`, `WithDeadline` | Vo + native (timer) |
| 1.3.5 | Implement `WithValue` | Vo |
| 1.3.6 | Integrate with `net/http` server handlers | Vo |
| 1.3.7 | Write tests | Vo |

#### Phase 1.4: `sync` Package (New)

**Priority**: High — enables goroutine coordination within islands
**Effort**: Medium

| Task | Description | Impl |
|------|-------------|------|
| 1.4.1 | Implement `Mutex`, `RWMutex` (island-local) | Vo + native (atomic ops) |
| 1.4.2 | Implement `WaitGroup` | Vo + native (atomic ops) |
| 1.4.3 | Implement `Once` | Vo + native (atomic ops) |
| 1.4.4 | Implement `Map` (concurrent map) | Vo |
| 1.4.5 | Implement `Pool` | Vo |
| 1.4.6 | Write tests | Vo |

#### Phase 1.5: Existing P0 Package Polish

| Task | Package | Description |
|------|---------|-------------|
| 1.5.1 | `fmt` | Add `Sscanf`, `Fscanf`, `Scan`, `Scanln`, `Scanf` (scan family) — native impl |
| 1.5.2 | `strconv` | Add `AppendFloat`, `QuoteRune`, `QuoteToASCII`, `CanBackquote` |
| 1.5.3 | `unicode` | Add `IsSpace`, `IsPrint`, `IsControl`, `IsPunct`, `IsGraphic`, `SimpleFold` — native impl |
| 1.5.4 | `os` | Add `UserHomeDir`, `UserCacheDir`, `UserConfigDir`, `Symlink`, `Readlink` — native impl |
| 1.5.5 | `net` | Add `Dialer` type, `ListenTCP`/`ListenUDP` direct functions |
| 1.5.6 | `math/rand` | Add `ExpFloat64`, `NormFloat64`; consider `New`/`NewSource` |

### Phase 2: P1 Package Completion

**Goal**: Complete all P1 packages to their target level.

#### Phase 2.1: `bufio` Package (New)

**Priority**: Important — enables efficient line-by-line I/O
**Effort**: Medium

| Task | Description | Impl |
|------|-------------|------|
| 2.1.1 | Implement `Reader` with `Read`, `ReadByte`, `ReadRune`, `ReadLine`, `ReadString`, `ReadBytes`, `Peek`, `UnreadByte`, `UnreadRune` | Vo |
| 2.1.2 | Implement `Writer` with `Write`, `WriteByte`, `WriteRune`, `WriteString`, `Flush` | Vo |
| 2.1.3 | Implement `Scanner` with `Scan`, `Text`, `Bytes`, `Err`, `Split` | Vo |
| 2.1.4 | Add `ScanLines`, `ScanWords`, `ScanRunes`, `ScanBytes` split functions | Vo |
| 2.1.5 | Add `ReadWriter` | Vo |
| 2.1.6 | Write comprehensive tests | Vo |

#### Phase 2.2: `log` Package (New — baseline/extended)

**Priority**: Important — basic logging is essential
**Effort**: Small (depends on `time.Format` for date/time flags)

**Baseline (core-extended — no OS deps)**:

| Task | Description | Impl |
|------|-------------|------|
| 2.2.1 | Implement `Logger` struct with output sink, prefix, flags | Vo |
| 2.2.2 | Implement `Print/Println/Printf`, `Fatal*`, `Panic*` | Vo |
| 2.2.3 | Default output goes to **runtime-injected sink** (host provides writer; in WASM this is the host console) | Vo + native |
| 2.2.4 | Add flag constants: `Ldate`, `Ltime`, `Lmicroseconds`, `Lmsgprefix`, `LstdFlags` | Vo |
| 2.2.5 | Implement `New(sink, prefix, flag) *Logger` | Vo |
| 2.2.6 | Write baseline tests (VM + JIT) | Vo |

**Extended (std — requires OS)**:

| Task | Description | Impl |
|------|-------------|------|
| 2.2.7 | Implement `SetOutput(w io.Writer)` to redirect to file/stderr | Vo |
| 2.2.8 | Add `Llongfile`, `Lshortfile`, `LUTC` flag support (requires `runtime.Caller` equivalent or native helper) | Vo + native |
| 2.2.9 | Standard logger defaults to `os.Stderr` | Vo |
| 2.2.10 | Write extended tests | Vo |

#### Phase 2.3: `flag` Package (New — baseline/extended)

**Priority**: Important — CLI argument parsing
**Effort**: Small

**Baseline (core-extended — no OS deps)**:

| Task | Description | Impl |
|------|-------------|------|
| 2.3.1 | Implement `FlagSet` struct with flag registration | Vo |
| 2.3.2 | Implement `FlagSet.Parse(args []string) error` — pure Vo, takes explicit args | Vo |
| 2.3.3 | Add type-specific registration: `FlagSet.Bool`, `FlagSet.Int`, `FlagSet.String`, `FlagSet.Float64`, `FlagSet.Duration` | Vo |
| 2.3.4 | Implement `FlagSet.Args()`, `FlagSet.NArg()`, `FlagSet.NFlag()`, `FlagSet.Arg(i)` | Vo |
| 2.3.5 | Implement `FlagSet.PrintDefaults()` | Vo |
| 2.3.6 | Write baseline tests (VM + JIT) | Vo |

**Extended (std — requires OS)**:

| Task | Description | Impl |
|------|-------------|------|
| 2.3.7 | Implement package-level `Parse()` — wraps default `FlagSet` with `os.Args[1:]` | Vo |
| 2.3.8 | Implement package-level `Bool`, `Int`, `String`, etc. (delegate to default FlagSet) | Vo |
| 2.3.9 | Implement `Usage` variable and default usage output | Vo |
| 2.3.10 | Write extended tests | Vo |

#### Phase 2.4: `io/fs` Package (New)

**Priority**: Medium
**Effort**: Small

| Task | Description | Impl |
|------|-------------|------|
| 2.4.1 | Define `FS`, `File`, `DirEntry`, `FileInfo`, `FileMode` interfaces | Vo |
| 2.4.2 | Implement `ReadDir`, `ReadFile`, `Stat`, `Sub` utility functions | Vo |
| 2.4.3 | Bridge to `os` for default filesystem | Vo |
| 2.4.4 | Write tests | Vo |

#### Phase 2.5: Existing P1 Package Polish — ✅ COMPLETE

| Task | Package | Description | Status |
|------|---------|-------------|--------|
| 2.5.1b | `regexp` (extended) | Byte variants (`Find`, `FindIndex`, `FindAll`, `FindSubmatch`, `ReplaceAll`), `ReplaceAllStringFunc`, `SubexpNames`, `NumSubexp`, `String` | ✅ Done |
| 2.5.2 | `sort` | Added `Interface`, `Sort`, `Stable`, `IsSorted`, `ReverseInterface` | ✅ Done |
| 2.5.3 | `strings` | Added `Replacer`/`NewReplacer`, `IndexFunc`, `LastIndexFunc`, `Title`, `ToValidUTF8` | ✅ Done |
| 2.5.4 | `bytes` | Added `IndexFunc`, `LastIndexFunc`, `Title` | ✅ Done |
| 2.5.5 | `strconv` | Added `QuoteRuneToASCII`, `QuoteRuneToGraphic`, `QuoteToGraphic`, `AppendQuote*` variants | ✅ Done |
| 2.5.6 | `unicode` | Added `IsTitle` | ✅ Done |
| 2.5.7 | `net` | Added `Dialer` type with `Dial`/`DialContext` methods | ✅ Done |
| 2.5.8 | `math/bits` | Fixed `Reverse8`/`Reverse16` sign-extension bug | ✅ Done |
| 2.5.9 | `unicode/utf8` | Fixed byte-index sign-extension bug for bytes ≥ 0x80 | ✅ Done |

### Phase 3: Quality & Testing — ✅ COMPLETE (2026-02)

**Goal**: Ensure robustness and correctness across all packages with verifiable, executable criteria.

#### 3.1 Test Files Created

| Task | Description | Status |
|------|-------------|--------|
| 3.1.1 | `math/bits` dedicated test file | ✅ Done |
| 3.1.2 | `unicode/utf8` dedicated test file | ✅ Done |
| 3.1.3 | `io` dedicated test file | ✅ Done |
| 3.1.4 | Added `sort.Interface` tests to sort.vo | ✅ Done |
| 3.1.5 | Added `strings.Replacer`/`IndexFunc`/`Title` tests | ✅ Done |
| 3.1.6 | Added `regexp` byte variant + `ReplaceAllStringFunc` tests | ✅ Done |

#### 3.2 Mode Matrix Verification

All tests verified in both VM and JIT modes via `./d.py test both --release`.

**Final result: 1972 tests passing (991 VM + 981 JIT), 0 failures.**

#### 3.3 Bug Fixes Applied

| Bug | Package | Fix |
|-----|---------|-----|
| Signed byte sign-extension in rune lookup tables | `unicode/utf8`, `math/bits` | Changed byte indexing from `int8` arithmetic to unsigned masking |
| `Reverse8`/`Reverse16` wrong results for bytes ≥ 0x80 | `math/bits` | Replaced buggy lookup-table approach with pure arithmetic |

### Phase 4: Extended Features (Future)

| Task | Description | Depends On |
|------|-------------|------------|
| 4.1 | ~~Timezone/`Location` support in `time`~~ | ✅ Done — `FixedZone`, `LoadLocation`, `In`, `UTC`, `Local`; native Rust backend |
| 4.2 | ~~`time.Timer`, `time.Ticker`~~ | ✅ Done — pure Vo goroutine implementation |
| 4.3 | ~~`time.After`, `time.Tick`, `time.AfterFunc`~~ | ✅ Done — pure Vo goroutine implementation |
| 4.4 | TLS server support | Native bridge |
| 4.5 | `encoding/xml` | If demand warrants |
| 4.6 | `crypto/sha256`, `crypto/md5`, `crypto/hmac` | Native impl |

---

### Phase W: WASM / no_std Full Support

**Goal**: Reach full WASM + `no_std` support without architectural drift. Unlike earlier phases, Phase W is **not fully reorderable**: async substrate work must land before Sleep/fetch.

**Architecture decisions (locked for this plan):**
1. **Do not enable `std` just for WASM**. Build a platform-agnostic suspend/resume path that works in `no_std + alloc`.
2. **For HTTP, move directly to the final ABI** (`nativeHttpDo`) across both native and WASM implementations; do not keep a compatibility layer.
3. **For regexp, move directly to the final unified backend** in `vo-stdlib`; remove WASM-specific regexp module and registrations in the same phase.

#### Phase W0: Async Suspension Substrate for WASM/no_std (~4-6 days)

This is the prerequisite for W2 (`time.Sleep`) and W3 (`fetch`).

| Task | File | Description |
|------|------|-------------|
| W0.1 | `vo-runtime/src/ffi/mod.rs` + VM extern dispatch | Introduce a platform-agnostic extern wait/suspend result available in both `std` and `no_std` builds (instead of relying on `std`-only I/O wait variants). |
| W0.2 | `vo-vm/src/scheduler.rs` + VM run loop | Add generic "blocked-by-runtime-event" scheduling path. Keep existing std I/O polling behavior, but route WASM wakeups through the same fiber state machine. |
| W0.3 | `vo-web/src/lib.rs` + `vo-web/runtime-wasm` (new wake module) | Implement re-entrant dispatch entry for browser callbacks (`setTimeout`, `Promise.then`). Callback must wake specific fiber and continue scheduling safely. |
| W0.4 | Tests (`vo-web` wasm tests) | Add suspend/resume conformance test using a minimal test extern: verify fiber blocks, callback wakes, and execution resumes at correct PC. |

**Exit criteria:** no `std` feature dependency is introduced in WASM path; suspend/resume works for both timer and Promise-style callbacks.

#### Phase W1: Fix Critical WASM Runtime Gaps (~3-5 days)

These are user-visible failures in playground flows (timezone APIs and entropy quality).

| Task | File | Description |
|------|------|-------------|
| W1.1 | `vo-web/runtime-wasm/src/time.rs` | Implement `localOffsetAt` / `localAbbrevAt` via JS Intl APIs for a given Unix timestamp. |
| W1.2 | `vo-web/runtime-wasm/src/time.rs` | Implement `ianaOffsetAt` / `ianaAbbrevAt` / `loadLocation` using `Intl.DateTimeFormat`; validate timezone names and return consistent errors. |
| W1.3 | `vo-stdlib/src/rand.rs` | WASM entropy path: use `crypto.getRandomValues` for `auto_seed()` and byte-fill operations used by `Read`. |
| W1.4 | `vo-web/runtime-wasm/src/time.rs` + extern registration tests | Add regression check that exported symbol names exactly match Vo extern declarations (e.g., `time_blocking_sleepNano` vs `time_sleepNano`). |

**Exit criteria:** timezone APIs no longer panic on WASM; entropy source is cryptographically strong in browser; symbol mapping test prevents silent mismatch regressions.

#### Phase W2: `time.Sleep` / Timer / Ticker on WASM (~3-5 days)

`time.Sleep` currently panics; `Timer`/`Ticker`/`After` rely on it.

| Task | File | Description |
|------|------|-------------|
| W2.1 | `vo-web/runtime-wasm/src/time.rs` | Replace panic implementation with suspend-via-W0 path. Convert nanoseconds to milliseconds carefully (floor/ceil policy documented and tested). |
| W2.2 | `vo-web/runtime-wasm/src/time.rs` + registration | Register the **correct extern name** used by Vo stdlib (`time_blocking_sleepNano`). |
| W2.3 | `lang/test_data` + wasm integration tests | Add tests for `time.Sleep`, `time.After`, `NewTimer`, `NewTicker`, reset/stop behavior, and no-busy-loop guarantees. |
| W2.4 | Scheduler re-entry tests | Verify callback-driven re-entry remains correct with nested goroutines and multiple concurrent sleepers. |

**Exit criteria:** no panic path for `Sleep` on WASM; timer family behaves consistently with native semantics for supported cases.

#### Phase W3: `net/http` Client via Browser `fetch` (~5-8 days)

Enables outbound HTTP requests in playground with a single final ABI shared by native and WASM.

| Task | File | Description |
|------|------|-------------|
| W3.1 | `lang/stdlib/net/http/http.vo` | Replace extern declaration and call site usage from `nativeHttpsRequest` to `nativeHttpDo` (single final ABI). |
| W3.2 | `vo-stdlib/src/net/http/mod.rs` | Rename and re-register native bridge to `net_http_nativeHttpDo`; remove `nativeHttpsRequest` registration and implementation naming. |
| W3.3 | `vo-web/runtime-wasm/src/net_http.rs` (new) + wasm registration | Implement `nativeHttpDo` via `fetch`: request mapping, response mapping, deterministic error mapping, timeout integration. |
| W3.4 | W0 suspend/resume integration | Promise completion must wake the blocked fiber and continue VM scheduling; include cancellation and timeout wiring where supported. |
| W3.5 | `vo-web/runtime-wasm/src/lib.rs` + `vo-web/src/lib.rs` | Register WASM net/http externs in runtime initialization and ensure no stale symbol names remain. |
| W3.6 | `lang/stdlib/net/http/http.vo` docs/comments + playground docs | Document CORS behavior and browser limitations (forbidden headers, credential mode, opaque responses). |
| W3.7 | Tests + symbol audit | Add tests that assert only `nativeHttpDo` is referenced/registered; fail fast if `nativeHttpsRequest` reappears. |

**Exit criteria:** standard `http.Client.Do` path works in playground for CORS-allowed endpoints; error mapping and timeout behavior are deterministic; old `nativeHttpsRequest` symbol is fully removed.

#### Phase W4: Regexp Engine Unification (~2-3 days)

Goal: remove JS-vs-Rust behavior drift with a direct final-state cut (single backend).

| Task | File | Description |
|------|------|-------------|
| W4.1 | `vo-stdlib/Cargo.toml` | Ensure `regex` is configured for `no_std + alloc` compatible build on wasm/native targets used by Vo. |
| W4.2 | `vo-stdlib/src/regexp.rs` | Remove std-only registration/implementation gates; make Rust regex backend the canonical engine. |
| W4.3 | `vo-stdlib/src/lib.rs` | Always register `regexp` externs from stdlib (not only under `std` feature). |
| W4.4 | `vo-web/runtime-wasm/src/regexp.rs` + `vo-web/runtime-wasm/src/lib.rs` + `vo-web/src/lib.rs` | Remove WASM-specific regexp implementation and all related module references/registrations. |
| W4.5 | Tests + symbol audit | Add tests to assert regexp externs are provided only by `vo-stdlib`; fail fast if WASM JS-regexp path is reintroduced. |

**Exit criteria:** regexp conformance tests pass on both native and WASM with matching outputs for supported syntax; `vo-web/runtime-wasm` no longer registers regexp externs.

#### Phase W5: `no_std` Hardening and CI Enforcement (~4-7 days)

Finalize Tier-1 portability by removing remaining `std` coupling in core runtime/stdlib paths.

| Task | File | Description |
|------|------|-------------|
| W5.1 | `vo-stdlib/src/rand.rs` | Replace `thread_local` RNG dependency in no_std path with global lock-free/lock-based no_std-safe RNG state; keep behavior deterministic under tests. |
| W5.2 | `vo-stdlib/src/{math,bits,bytes,strings,strconv,unicode,fmt,json,toml_pkg,io}.rs` | Complete `std` → `core/alloc` audit and migration. |
| W5.3 | `vo-stdlib/src/source.rs` + `vo-stdlib/Cargo.toml` | Split or gate source-embedding path so no_std builds do not pull `std`-only filesystem/embed dependencies. |
| W5.4 | `vo-stdlib/src/lib.rs` + crate attrs | Make `no_std` default with explicit `alloc` usage; keep `std` feature only for OS/network/process modules. |
| W5.5 | `vo-runtime` core path | Audit and remove remaining `std::` usage in VM-critical code paths needed by no_std targets. |
| W5.6 | CI | Add mandatory build jobs for wasm and embedded no_std targets; fail PR on regression. |

**Exit criteria:** `vo-stdlib` and `vo-runtime` compile in no_std targets used by project policy, and CI enforces it.

#### Phase W — Effort Summary

| Phase | Effort | Unlocks |
|-------|--------|---------|
| W0 (async substrate) | ~4-6 days | Shared suspend/resume foundation for timer + fetch in WASM/no_std |
| W1 (timezone + rand entropy) | ~3-5 days | Fixes runtime gaps and entropy quality in playground |
| W2 (Sleep + Timer WASM) | ~3-5 days | `time.Sleep`, `time.After`, `time.Timer`, `time.Ticker` in WASM |
| W3 (net/http fetch) | ~5-8 days | HTTP client from playground; real API demos |
| W4 (regexp unification) | ~2-3 days | Consistent regexp behavior native vs WASM |
| W5 (no_std hardening) | ~4-7 days | Embedded/bare-metal readiness with CI enforcement |
| **Total** | **~21-34 days** | |

**Recommended order**: **W0 → W1 → W4 → W2 → W3 → W5**.

Rationale:
- W0 is the mandatory runtime foundation for W2/W3.
- W1 removes immediate user-facing failures.
- W4 is low-risk cleanup and reduces cross-platform divergence early.
- W5 should land after behavior features are stable, then lock with CI.

---

## 4. Dependency Graph

```
Phase 1.1 (time) ───────────────────────────────────┐
Phase 1.2a (net/url) ──┐                            │
                      ▼                            │
Phase 1.2b (net/http) ─────────────────────────┤
Phase 1.3 (context) ───────┐                        │
Phase 1.4 (sync) ──────────┤                        │
Phase 1.5 (P0 polish) ────┤                        │
                            ▼                        ▼
                      Phase 2.2 (log)          Phase 2.1 (bufio)
                      [baseline: independent]  [independent]
                      [extended: needs time.Format + OS]
                            │
                            ▼
                      Phase 2.3 (flag)
                      [baseline: independent]
                      [extended: needs os.Args]
                            │
                            ▼
                      Phase 2.4 (io/fs)
                            │
                            ▼
                      Phase 2.5 (P1 polish)
                      [includes regexp baseline/extended]
                            │
                            ▼
                      Phase 3 (quality + mode matrix)
                            │
                            ▼
                      Phase 4 (extended features)
```

**Critical Paths**:
- `net/url` → `net/http` URL integration (blocks all HTTP improvements)
- `time.Format` → `log` extended → many user applications

---

## 5. Estimated Effort

| Phase | Effort | Description |
|-------|--------|-------------|
| 1.1 | ~3-5 days | `time` enhancement (Format/Parse is the bulk) |
| 1.2a | ~2-3 days | `net/url` package (URL parser, Values, escaping) |
| 1.2b | ~4-6 days | `net/http` enhancement (URL integration, server body, chunked, keep-alive) |
| 1.3 | ~2-3 days | `context` package |
| 1.4 | ~2-3 days | `sync` package |
| 1.5 | ~2-3 days | P0 polish (spread across packages) |
| 2.1 | ~2-3 days | `bufio` package |
| 2.2 | ~1-2 days | `log` package (baseline + extended) |
| 2.3 | ~1-2 days | `flag` package (baseline + extended) |
| 2.4 | ~1 day | `io/fs` package |
| 2.5 | ~3-5 days | P1 polish (includes `regexp` baseline Thompson NFA) |
| 3 | ~3-5 days | Testing, mode matrix verification, error audit |
| **Total** | **~26-42 days** | |

---

## 6. stdlib.toml Current State

The actual `stdlib.toml` uses a two-tier structure (`core` / `std`). The `core_extended` and `runtime` tiers described in Section 1.2 are a logical classification only — they are not yet reflected as separate TOML sections.

```toml
[meta]
version = "0.1.0"

# Core packages (no OS dependency, no_std compatible)
# Note: also includes logically "core-extended" packages (fmt, json, regexp, etc.)
[packages.core]
fmt = { path = "fmt" }
errors = { path = "errors" }
strings = { path = "strings" }
strconv = { path = "strconv" }
bytes = { path = "bytes" }
unicode = { path = "unicode" }
"unicode/utf8" = { path = "unicode/utf8" }
math = { path = "math" }
"math/bits" = { path = "math/bits" }
sort = { path = "sort" }
slices = { path = "slices" }
maps = { path = "maps" }
regexp = { path = "regexp" }
dyn = { path = "dyn" }
"encoding/hex" = { path = "encoding/hex" }
"encoding/base64" = { path = "encoding/base64" }
"encoding/json" = { path = "encoding/json" }
"encoding/toml" = { path = "encoding/toml" }
"encoding/csv" = { path = "encoding/csv" }
"encoding/binary" = { path = "encoding/binary" }
cmp = { path = "cmp" }
path = { path = "path" }

# Standard packages (requires OS support)
# Note: context and sync are island-local but listed here, not in a separate runtime section
[packages.std]
io = { path = "io" }
os = { path = "os" }
time = { path = "time" }
net = { path = "net" }
"net/url" = { path = "net/url" }
"net/http" = { path = "net/http" }
"math/rand" = { path = "math/rand" }
"path/filepath" = { path = "path/filepath" }
"os/exec" = { path = "os/exec" }
context = { path = "context" }
sync = { path = "sync" }
bufio = { path = "bufio" }
log = { path = "log" }
flag = { path = "flag" }
"io/fs" = { path = "io/fs" }

# [packages.runtime]  # future: separate tier for deep VM/scheduler packages
# reflect = { path = "reflect" }  # P2
```

> **Delta from original tier plan**: `core_extended` packages (`fmt`, `encoding/json`, `encoding/toml`, `regexp`, `log`, `flag`) are merged into `core`. `context` and `sync` are in `std` rather than a separate `runtime` section. The tier split can be applied in a future refactor if WASM/no_std builds require it.

---

## 7. Phase Gate Requirements

Every phase (1.1, 1.2a, 1.2b, 1.3, ...) has a **completion gate**. A phase is not considered done until **all** of the following are satisfied in the same commit/PR:

### 7.1 Mandatory Gate Checklist

| # | Requirement | Details |
|---|---|---|
| G1 | **All phase tasks completed** | Every task in the phase table is implemented and working |
| G2 | **Tests pass in both VM and JIT** | `./d.py test both` passes with no regressions |
| G3 | **stdlib.toml updated** | New packages added to correct tier (`core`, `core_extended`, `std`, `runtime`) |
| G4 | **Compatibility matrix updated** | `stdlib-compatibility-matrix.md` reflects new/changed package status, contract level, and platform support |
| G5 | **This plan updated** | Phase marked as done with actual effort recorded |
| G6 | **Test coverage meets standard** | Every new exported function has at least happy-path + error-path test (per Section 3.1) |
| G7 | **Capability mode verified** | For core-baseline/core-extended packages: WASM build does not regress. For baseline/extended split packages: core-only mode returns clear errors for extended-only features |

### 7.2 Update Triggers

This plan should also be updated when:
1. New requirements emerge from user feedback.
2. Language features change that affect stdlib design (e.g., generics, new concurrency primitives).
3. A phase takes significantly longer or shorter than estimated — adjust remaining estimates.
