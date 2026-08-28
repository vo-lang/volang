# Standard library

The standard library is source-distributed with the toolchain. Packages compile
with the application, while operations that require the operating system,
network, clock, process, or browser are provided through governed runtime and
host contracts.

## Core packages

- `errors`, `fmt`, `strconv`, and `strings` cover errors, formatting,
  conversions, and text.
- `bytes`, `unicode`, and `unicode/utf8` cover byte and Unicode processing.
- `cmp`, `maps`, `slices`, and `sort` provide common collection operations.
- `math`, `math/bits`, and `math/rand` provide numerical helpers.
- `regexp` provides regular-expression compilation and matching.

## Encoding

- `encoding/json` encodes and decodes typed values and open dynamic values.
- `encoding/toml` handles TOML configuration data.
- `encoding/base64`, `encoding/hex`, and `encoding/binary` cover binary
  interchange.
- `encoding/csv` reads and writes row-oriented text data.

Prefer typed structs for stable schemas. Use `any` and the `dyn` package when a
schema is intentionally open.

## I/O, files, and processes

- `io`, `io/fs`, `bufio`, and `bytes` define stream, filesystem, buffering, and
  in-memory byte contracts.
- `os`, `path`, and `path/filepath` expose host filesystem and path behavior.
- `os/exec` starts governed child processes on hosts that grant the capability.
- `flag` parses command-line flags.
- `log` supplies structured application logging primitives.

Host-dependent functions can report unsupported capability on sandboxed
targets. Libraries should accept interfaces such as readers, writers, and
filesystems so callers can provide deterministic test implementations.

## Network and URLs

- `net` defines addresses and connection-level contracts.
- `net/http` supplies HTTP request, response, client, and server behavior where
  the host supports it.
- `net/url` parses and constructs URLs.

Browser networking remains subject to origin and host policy. Desktop and
embedded hosts should grant the narrowest authority needed by the application.

## Time, context, and synchronization

- `time` provides instants, durations, timers, and formatting.
- `context` carries cancellation, deadlines, and request-scoped values.
- `sync` provides synchronization primitives for supported shared-runtime
  scenarios.
- Channels remain the preferred coordination primitive between goroutines in
  one island. Ports carry messages across islands.

## Runtime packages

- `runtime` exposes scheduler and runtime information intended for application
  use.
- `runtime/mem` exposes bounded memory observations and controls where the
  backend contract permits them.
- `toolchain` exposes versioned toolchain facts to governed generators and
  build integrations.
- `dyn` defines the dynamic-access protocol and stable error categories.

## Portability

Package presence alone does not grant a platform effect. The same API can be
implemented by a native provider, a Web host import, a deterministic test
provider, or an unsupported-capability response. Check errors and declare UI
platform requirements in the application manifest.

The maintained package source lives under `lang/stdlib`. Its tests in
`tests/lang` are the executable compatibility contract across VM, JIT, GC
stress, Native AOT, and Core Wasm AOT lanes.
