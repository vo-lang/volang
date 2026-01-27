# Vo Networking Design (Native + WASM)

*(Design document — English)*

## 1. Goals

- Provide a **Go-compatible, practical subset** of `net` and `net/http` that covers the vast majority of real-world application code.
- Keep **core abstractions identical to Go** where they are ecosystem boundaries:
  - `net.Conn`, `net.Listener`, `net.PacketConn`, `net.Addr`
  - `net/http` request/response/client/server model (with platform-appropriate constraints)
- Provide a **first-class WASM story** for networking:
  - `net` on WASM supports **pure IP/CIDR utilities**, **DoH-based DNS**, and **WebSocket-based connections**.
  - `net/http` on WASM supports **HTTP client via `fetch`**, no server.

## 2. Non-goals

- Implementing “everything in Go stdlib net” (we intentionally keep the API surface small).
- Exposing low-level socket tuning APIs (`syscall.RawConn`, `Dialer.Control`, FD conversions).
- Implementing raw IP sockets (`ip4:icmp`, etc.).
- Providing an HTTP server implementation on browser WASM.

---

## 3. Feature Matrix (Final)

### 3.1 `net` (Native)

**Supported**
- **Core interfaces**
  - `Addr`
  - `Conn` (includes deadlines)
  - `Listener`
  - `PacketConn`
- **Entry points**
  - `Dial`, `DialTimeout`
  - `Listen`, `ListenPacket`
- **TCP/UDP**
  - `TCPConn`, `TCPAddr`, `ResolveTCPAddr`, `ListenTCP`, `DialTCP`
  - `UDPConn`, `UDPAddr`, `ResolveUDPAddr`, `ListenUDP`, `DialUDP`
- **Unix domain sockets**
  - `unix` stream sockets (`Dial("unix", path)`, `Listen("unix", path)`)
  - `UnixConn`, `UnixAddr` (minimal set; enough for IPC and daemon control sockets)
- **IP & CIDR utilities**
  - `IP`, `IPMask`, `IPNet`
  - `ParseIP`, `ParseCIDR`
  - `SplitHostPort`, `JoinHostPort`
- **DNS lookup (explicit)**
  - `LookupHost`, `LookupIP`, `LookupAddr` (small set)

**Not supported**
- `FileConn/FileListener/FilePacketConn`
- `syscall.RawConn`, `Dialer.Control/ControlContext`
- Interface enumeration (`Interfaces`, `InterfaceAddrs`, `HardwareAddr`, `ParseMAC`, etc.)
- Raw IP networking (`ip`, `ip4`, `ip6`, `IPConn`)

---

### 3.2 `net` (Browser WASM, `vo-web/runtime-wasm`)

**Supported**
- **Pure utilities**
  - `ParseIP`, `ParseCIDR`, `IP`, `IPNet`, `IPMask`
  - `SplitHostPort`, `JoinHostPort`
- **DNS**
  - `LookupHost`, `LookupIP` via **DoH (DNS over HTTPS)** with a configurable endpoint
- **Connections**
  - `Dial("ws", url)` / `Dial("wss", url)` returning a `Conn` implemented over WebSocket
  - Deadlines for the WebSocket-Conn are supported in a best-effort manner (timer-driven); exact semantics are defined below.

**Not supported**
- `tcp/udp/unix` sockets on browser WASM
- `Listen`, `ListenPacket` (no server sockets in browser)
- `PacketConn` implementation (no UDP)

---

## 4. API Surface (Recommended Minimal Set)

### 4.1 `net` public API subset

#### Types (core)
- `type Addr interface { Network() string; String() string }`
- `type Conn interface { Read([]byte)(int,error); Write([]byte)(int,error); Close()error; LocalAddr()Addr; RemoteAddr()Addr; SetDeadline(time.Time)error; SetReadDeadline(time.Time)error; SetWriteDeadline(time.Time)error }`
- `type Listener interface { Accept()(Conn,error); Close()error; Addr()Addr }`
- `type PacketConn interface { ReadFrom([]byte)(int,Addr,error); WriteTo([]byte,Addr)(int,error); Close()error; LocalAddr()Addr; SetDeadline/SetReadDeadline/SetWriteDeadline }`

#### Functions (core)
- `Dial(network, address string) (Conn, error)`
- `DialTimeout(network, address string, timeout time.Duration) (Conn, error)`
- `Listen(network, address string) (Listener, error)`
- `ListenPacket(network, address string) (PacketConn, error)`
- `SplitHostPort(hostport string) (host, port string, err error)`
- `JoinHostPort(host, port string) string`

#### IP/CIDR
- `type IP []byte`
- `type IPMask []byte`
- `type IPNet struct { IP IP; Mask IPMask }`
- `ParseIP(string) IP`
- `ParseCIDR(string) (IP, *IPNet, error)`

#### DNS (small set)
- `LookupHost(host string) ([]string, error)`
- `LookupIP(host string) ([]IP, error)`
- `LookupAddr(addr string) ([]string, error)`

#### Unix (native only)
- `UnixAddr`, `UnixConn`
- `Dial("unix", path)` / `Listen("unix", path)`

#### WASM-only network strings
- `Dial("ws", url)` / `Dial("wss", url)`

---

## 5. `net/http` Support

### 5.1 `net/http` (Native)

**Supported**
- HTTP client:
  - `Client`, `Transport` (simplified but compatible), `NewRequest`, `Do`, `Get`, `Post`
- Core types:
  - `Request`, `Response`, `Header`, `Cookie` (minimal)
- HTTP server:
  - `Handler`, `HandlerFunc`, `ServeMux` (minimal)
  - `Server` with `ListenAndServe` and `Serve(Listener)`
  - `ResponseWriter`
- TLS integration:
  - HTTPS client + HTTPS server via `crypto/tls` integration

**Not supported**
- HTTP/2, h2c
- Advanced proxy features beyond basic
- WebSocket in `net/http` (WebSocket is considered a separate concern; WASM uses it via `net.Dial("ws")`)

### 5.2 `net/http` (Browser WASM)

**Supported**
- HTTP **client only**, implemented via `fetch`
  - `NewRequest`, `Client.Do`, `Get`, `Post`
  - `Request`, `Response`, `Header`
- The response body supports reading as bytes (buffered) and possibly streaming if the runtime supports it (explicitly scoped in implementation details).

**Not supported**
- HTTP server
- TLS knobs (browser controls TLS)
- Low-level transport hooks

---

## 6. WASM Implementation Architecture (vo-web)

### 6.1 Runtime structure

- Rust: `lang/crates/vo-web/runtime-wasm/src/net.rs` (new)
- JS: `lang/crates/vo-web/js/net.ts` (new)
- JS init: `lang/crates/vo-web/js/index.ts` calls `registerNetBindings()` during `init()`
- VM extern registration: `lang/crates/vo-web/src/lib.rs` adds
  - `vo_web_runtime_wasm::net::register_externs(&mut vm.state.extern_registry, &module.externs);`

### 6.2 JS binding model

We follow the existing `vfs.ts` pattern:
- JS exposes functions on `window`:
  - `window._netLookupHost(...)`
  - `window._netLookupIP(...)`
  - `window._netWsConnect(...)`
  - `window._netWsRead(...)`
  - `window._netWsWrite(...)`
  - `window._netWsClose(...)`
  - `window._netHttpFetch(...)` (for `net/http` client)
- Rust imports them via `wasm_bindgen extern "C"` and wraps into Vo extern functions.

### 6.3 DoH DNS

- Default DoH endpoint: configurable (exact endpoint decision is part of implementation config).
- DNS API required:
  - `LookupHost(host) -> []string`
  - `LookupIP(host) -> []IP`
- Implementation approach:
  - `window._netLookupHost(host)` returns `[addrs, err]`
  - `window._netLookupIP(host)` returns `[ipBytesList, err]` or string list convertible to IP

### 6.4 WebSocket Conn

- `Dial("ws"/"wss", url)` returns a `Conn` implemented by:
  - a JS-side WebSocket object stored in a connection table (integer handle)
  - Rust-side `Conn` methods call JS glue via handle
- `Read/Write` semantics:
  - WebSocket is message-based; we define a stream-ish mapping:
    - JS buffers incoming messages into a byte queue
    - `Read(p)` drains from the queue
    - `Write(p)` sends bytes as a single binary WebSocket message
- Deadlines:
  - Implemented with timers:
    - `Read` fails if no data arrives by deadline
    - `Write` fails if send cannot complete by deadline (best-effort; in browsers `send()` is usually sync but backpressure exists indirectly)

---

## 7. Error Model

- Keep Go-like behavior at the API boundary:
  - Parsing errors (host/port, CIDR) return `error`
  - Network operation errors return `error`
  - Deadline exceed returns an error compatible with `os.ErrDeadlineExceeded` semantics (native) and a consistent message/code on WASM
- WASM unsupported operations:
  - Must return a clear error (not silently succeed)
  - Examples: `Listen` on WASM, `Dial("tcp")` on WASM

---

## 8. Testing Strategy

### Native
- Unit tests for:
  - `SplitHostPort/JoinHostPort`
  - `ParseIP/ParseCIDR`
  - DNS lookups (can be integration-style; deterministic tests may require controlled resolver)
- Integration tests:
  - TCP echo server/client
  - UDP send/recv
  - Unix socket echo

### WASM
- Unit tests for pure parsing utilities (same test vectors as native)
- JS integration tests for:
  - DoH lookup (can be mocked in JS binding layer)
  - WebSocket conn read/write behavior
  - `net/http` client via `fetch` (mock fetch)

---

## 9. Implementation Choices (Native)

### 9.1 I/O Model

- **Blocking `std::net`** for TCP/UDP/Unix sockets
- **`socket2`** for setting deadlines via `SO_RCVTIMEO`/`SO_SNDTIMEO`
- Consistent with existing `os.rs` blocking I/O pattern

### 9.2 TLS

- **`rustls`** for TLS (pure Rust, cross-platform, modern TLS 1.2/1.3)
- **`webpki-roots`** for Mozilla CA certificate bundle

### 9.3 DNS

- **System resolver** via `libc::getaddrinfo`
- Respects system `/etc/resolv.conf` configuration
- No additional DNS library dependency

### 9.4 HTTP

- **Client**: `ureq` (blocking, no async, pure Rust, includes rustls)
- **Server**: Custom implementation on top of `net.Listener`
  - HTTP/1.1 only (no HTTP/2)
  - Simple request parsing + routing + response writing

### 9.5 Handle Management

Follow `os.rs` pattern with `lazy_static` handle tables:
```rust
lazy_static! {
    static ref CONN_HANDLES: Mutex<HashMap<i32, TcpStream>> = ...;
    static ref LISTENER_HANDLES: Mutex<HashMap<i32, TcpListener>> = ...;
}
```

### 9.6 Dependencies

```toml
# vo-stdlib/Cargo.toml additions
socket2 = { version = "0.5", optional = true }
ureq = { version = "2", optional = true, features = ["tls"] }
rustls = { version = "0.23", optional = true }
webpki-roots = { version = "0.26", optional = true }
```

---

## 10. Deliverables / File Layout (Planned)

### Vo source (stdlib)

```
lang/stdlib/
├── net/
│   └── net.vo          # interfaces + pure utilities
└── http/
    └── http.vo         # client/server types
```

### Rust implementation (vo-stdlib)

```
vo-stdlib/src/
├── net/
│   ├── mod.rs          # re-exports + register_externs
│   ├── addr.rs         # IP, IPMask, IPNet, parse utilities
│   ├── tcp.rs          # TcpConn, TcpListener
│   ├── udp.rs          # UdpConn
│   ├── unix.rs         # UnixConn, UnixListener
│   └── dns.rs          # LookupHost, LookupIP, LookupAddr
├── http/
│   ├── mod.rs
│   ├── client.rs       # Client, Transport, Get, Post (via ureq)
│   ├── server.rs       # Server, ServeMux, Handler
│   ├── request.rs      # Request, Header
│   └── response.rs     # Response, ResponseWriter
└── ...existing files
```

### WASM implementation (vo-web)

- `vo-web/runtime-wasm/src/net.rs` (register externs + wasm implementations)
- `vo-web/js/net.ts` (registerNetBindings + DoH + WebSocket + fetch glue)
- `vo-web/js/index.ts` updated to call `registerNetBindings()`

---

## 11. Implementation Phases

1. **Phase 1**: `net` pure utilities (`ParseIP`, `ParseCIDR`, `SplitHostPort`, `JoinHostPort`)
2. **Phase 2**: TCP (`Dial`, `Listen`, `Read`, `Write`, deadlines)
3. **Phase 3**: UDP (`DialUDP`, `ListenPacket`)
4. **Phase 4**: Unix sockets
5. **Phase 5**: DNS (`LookupHost`, `LookupIP`, `LookupAddr`)
6. **Phase 6**: `net/http` client
7. **Phase 7**: `net/http` server
