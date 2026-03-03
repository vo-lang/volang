# Studio Virtual Shell Design

**Date**: 2026-03-03  
**Status**: Approved — Implementation Phase 1

---

## 1. Motivation

Studio runs in two environments:
- **Local (Tauri)**: Full OS access — real filesystem, subprocesses, native git, native zip
- **WASM (Browser)**: Sandboxed — virtual filesystem (VFS), fetch-based network, WASM modules only

The existing `Bridge` interface covers filesystem and basic Vo execution but has no model for:
- Vo toolchain commands (run/check/build/test/bench/clean/get/dump/compile)
- Third-party tool invocation (git, zip)
- Arbitrary process execution
- Streaming output from long-running operations
- Capability declaration and graceful degradation

The virtual shell provides a **single typed command API** that works uniformly on both backends, with each backend free to implement each operation differently.

---

## 2. First-Principles Analysis

A shell has three essential properties:

1. **Typed intent** — what to do, expressed as structured data (not strings)
2. **Session context** — cwd, env vars; the execution context for each command
3. **Output semantics** — synchronous result for fast ops; streaming events for long-running ops

Key constraints for Studio:

- **WASM has no OS processes**, but can spawn virtual processes (registered WASM executables) and can make network requests via `fetch()`
- **The protocol is the only shared seam** — frontend knows nothing about Tauri IPC or WASM module internals
- **No bash parsing** — ops are typed discriminated unions, never shell strings
- **No PTY emulation** — streaming is line-oriented events, not a terminal
- **No silent degradation** — unsupported ops return `ERR_NOT_SUPPORTED`; unsupported tools return `ERR_TOOL_MISSING`

### Key Design Decisions

| Decision | Choice | Rationale |
|---|---|---|
| Protocol format | Typed discriminated union | No parsing, full type safety, IDE-friendly |
| WASM shell router | Pure TypeScript | git/zip/vo are JS assets; double FFI through Rust is wasteful |
| Session state | Client-side only | Stateless backend = simpler, testable, no session leaks |
| Streaming | Optional extension | Most ops are one-shot; long ops declare streaming intent |
| Integration | Additive to Bridge | No breaking changes to existing `fs*` and `compileRun` callers |
| `vo.get` in WASM | `fetch()` + fflate + VFS | Browser can download; no reason to hard-fail |
| `proc.spawn` in WASM | Virtual process registry | Known WASM tools can be registered; unknown → `ERR_TOOL_MISSING` |

### Capability Model

All capabilities are declared as **supported on both backends**. The distinction is only in the handler implementation:

| Capability | Local (Tauri) | WASM |
|---|---|---|
| `fs` | `std::fs` + workspace sandbox | VirtualFS (`vfs.ts`) |
| `vo.run` / `vo.check` / `vo.build` | `vox` crate direct | WASM VM module |
| `vo.test` / `vo.bench` / `vo.clean` | `vox` crate / subprocess | Phase 2 |
| `vo.get` | HTTP + unzip to disk | `fetch()` + fflate + VFS |
| `vo.dump` / `vo.compile` / `vo.init` / `vo.version` | `vox` crate | WASM VM module |
| `git` | System `git` binary | Phase 2 (isomorphic-git) |
| `zip` | `zip` crate | Phase 2 (fflate) |
| `proc.spawn` | `std::process::Command` | Virtual process registry |

---

## 3. Architecture

```
┌──────────────────────────────────────────────────────┐
│              Svelte Frontend (actions.ts)             │
│  Uses ShellClient. No knowledge of transport details. │
└──────────────────────┬───────────────────────────────┘
                       │  ShellRequest / ShellResponse / ShellEvent
                       │        (protocol.ts — types only)
              ┌────────▼────────────┐
              │     ShellClient     │  session mgmt, capability guard, exec
              │   + ShellTransport  │  (swappable: Tauri | WASM)
              └────────┬────────────┘
           ┌───────────┴────────────────┐
           │                            │
  ┌────────▼──────────┐     ┌───────────▼──────────────────┐
  │  TauriTransport   │     │       WasmTransport           │
  │  invoke(cmd)      │     │  direct TS function call       │
  └────────┬──────────┘     └───────────┬──────────────────┘
           │                            │
  ┌────────▼──────────┐     ┌───────────▼──────────────────┐
  │  Rust ShellRouter │     │     TS WasmShellRouter         │
  │  ├ FsHandler      │     │  ├ FsHandler (VirtualFS)       │
  │  ├ VoHandler      │     │  ├ VoHandler (wasm module)     │
  │  ├ GitHandler     │     │  ├ GitHandler (isogit, Ph2)    │
  │  ├ ZipHandler     │     │  ├ ZipHandler (fflate, Ph2)    │
  │  └ ProcHandler    │     │  └ ProcHandler (virt. registry)│
  └───────────────────┘     └──────────────────────────────┘
```

---

## 4. Protocol Types (protocol.ts)

### 4.1 Operations

```typescript
type VoTestTarget = "both" | "vm" | "jit" | "gc" | "nostd" | "wasm";
type VoRunMode    = "vm" | "jit";

type FsOp =
  | { kind: "fs.list";   path: string }
  | { kind: "fs.stat";   path: string }
  | { kind: "fs.read";   path: string }
  | { kind: "fs.write";  path: string; content: string }
  | { kind: "fs.mkdir";  path: string; recursive?: boolean }
  | { kind: "fs.remove"; path: string; recursive?: boolean }
  | { kind: "fs.rename"; oldPath: string; newPath: string }
  | { kind: "fs.copy";   src: string; dst: string };

type VoOp =
  | { kind: "vo.run";     path: string; mode?: VoRunMode; args?: string[] }
  | { kind: "vo.check";   path: string }
  | { kind: "vo.build";   path: string; output?: string }
  | { kind: "vo.test";    path?: string; target?: VoTestTarget; release?: boolean; verbose?: boolean; direct?: boolean }
  | { kind: "vo.bench";   suite?: "all" | "vo" | "score" }
  | { kind: "vo.clean";   what?: "all" | "vo" | "rust" }
  | { kind: "vo.get";     module: string }
  | { kind: "vo.dump";    path: string }
  | { kind: "vo.compile"; path: string; output?: string }
  | { kind: "vo.init" }
  | { kind: "vo.version" };

type GitOp =
  | { kind: "git.init" }
  | { kind: "git.status" }
  | { kind: "git.add";      paths: string[] }
  | { kind: "git.commit";   message: string; amend?: boolean }
  | { kind: "git.push";     remote?: string; branch?: string; force?: boolean }
  | { kind: "git.pull";     remote?: string; branch?: string }
  | { kind: "git.clone";    url: string; destPath: string }
  | { kind: "git.log";      limit?: number }
  | { kind: "git.diff";     staged?: boolean }
  | { kind: "git.checkout"; branch: string; create?: boolean }
  | { kind: "git.branch";   create?: string; delete?: string };

type ZipOp =
  | { kind: "zip.pack";   inputs: string[]; output: string }
  | { kind: "zip.unpack"; archive: string; outputDir: string }
  | { kind: "zip.list";   archive: string };

type ProcOp =
  | { kind: "proc.spawn"; program: string; args?: string[]; env?: Record<string, string> };

type ShellOp = FsOp | VoOp | GitOp | ZipOp | ProcOp;
```

### 4.2 Request / Response / Event

```typescript
interface ShellRequest {
  id:    string;                      // UUID per call, for correlation
  cwd:   string;                      // caller-maintained; no server-side session
  env?:  Record<string, string>;
  op:    ShellOp;
}

type ShellErrorCode =
  | "ERR_NOT_SUPPORTED"    // op not implemented on this backend
  | "ERR_NOT_FOUND"
  | "ERR_ACCESS_DENIED"
  | "ERR_ALREADY_EXISTS"
  | "ERR_VO_COMPILE"
  | "ERR_VO_RUNTIME"
  | "ERR_TOOL_MISSING"     // binary or virtual program not found
  | "ERR_INTERNAL";

type ShellResponse =
  | { id: string; kind: "ok";     data: unknown }
  | { id: string; kind: "stream"; jobId: string }   // subscribe ShellEvents by jobId
  | { id: string; kind: "error";  code: ShellErrorCode; message: string };

type ShellEvent =
  | { jobId: string; kind: "stdout";   line: string }
  | { jobId: string; kind: "stderr";   line: string }
  | { jobId: string; kind: "progress"; percent: number }
  | { jobId: string; kind: "done";     exitCode: number }
  | { jobId: string; kind: "fail";     code: ShellErrorCode; message: string };

type Capability =
  | "fs"
  | "vo.run" | "vo.check" | "vo.build" | "vo.test" | "vo.bench"
  | "vo.clean" | "vo.get" | "vo.dump" | "vo.compile" | "vo.init" | "vo.version"
  | "git" | "zip" | "proc.spawn";
```

### 4.3 Fs Result Types

```typescript
interface FsStatResult {
  name: string;
  path: string;
  isDir: boolean;
  size?: number;
  modifiedMs?: number;
}

type FsListResult = FsStatResult[];
```

---

## 5. Frontend Layer

### 5.1 ShellTransport Interface

```typescript
interface TransportInfo {
  workspaceRoot: string;
  capabilities:  Set<Capability>;
}

interface ShellTransport {
  initialize(): Promise<TransportInfo>;
  send(req: ShellRequest): Promise<ShellResponse>;
  onEvent(handler: (event: ShellEvent) => void): () => void;  // returns unsubscribe
}
```

### 5.2 TauriTransport

- `initialize()` → `invoke("cmd_shell_init")` → `{ workspaceRoot, capabilities }`
- `send(req)` → `invoke("cmd_shell_exec", { req })`
- `onEvent()` → `listen("shell-event", ...)` (Tauri event channel)

### 5.3 WasmTransport

- `initialize()` → call `WasmShellRouter.init()` → returns capabilities
- `send(req)` → `WasmShellRouter.handle(req)` (direct TS call, wrapped in Promise)
- `onEvent()` → internal event emitter (microtask-based for streaming simulation)

### 5.4 ShellClient

```typescript
class ShellClient {
  private transport: ShellTransport;
  private _cwd: string;
  private _env: Record<string, string>;
  private _capabilities: Set<Capability>;
  readonly workspaceRoot: string;

  // Execute op, throws ShellError for ERR_* responses
  async exec(op: ShellOp): Promise<unknown>;

  // Execute op that returns a stream; returns async iterable of ShellEvents
  async* stream(op: ShellOp): AsyncIterable<ShellEvent>;

  // Session control
  get cwd(): string;
  cd(path: string): void;
  setEnv(key: string, value: string): void;

  // Capability query
  supports(cap: Capability): boolean;
}
```

---

## 6. Backend: Rust (Tauri)

### Entry Point (lib.rs addition)

```rust
#[tauri::command]
fn cmd_shell_init(state: tauri::State<'_, AppState>) -> Result<ShellInitResponse, String>;

#[tauri::command]
fn cmd_shell_exec(req: ShellRequest, state: tauri::State<'_, AppState>,
                  app: tauri::AppHandle) -> Result<ShellResponse, String>;
```

### Router (shell/router.rs)

```rust
pub struct ShellRouter {
    workspace_root: PathBuf,
    fs:   FsHandler,
    vo:   VoHandler,
    git:  GitHandler,
    zip:  ZipHandler,
    proc: ProcHandler,
}

impl ShellRouter {
    pub fn handle(&self, req: ShellRequest, events: EventSender) -> ShellResponse;
}
```

Dispatch is a `match req.op.kind { "fs.*" => self.fs.handle(...), ... }` pattern using serde-tagged enums.

### Handler Contracts

Each handler implements:
```rust
trait Handler {
    fn capabilities() -> &'static [&'static str];
    fn handle(&self, op: ShellOp, cwd: &Path, env: &HashMap<String, String>,
              events: &EventSender) -> ShellResponse;
}
```

- **FsHandler**: wraps existing `resolve_path` + `std::fs` ops
- **VoHandler**: calls `vo_runtime` / `vox` crate; streams output via `EventSender`
- **GitHandler**: `std::process::Command::new("git")` with captured stdout/stderr
- **ZipHandler**: `zip` crate
- **ProcHandler**: `std::process::Command::new(program)` with sandboxed env

---

## 7. Backend: TypeScript WASM

### WasmShellRouter (shell/wasm/router.ts)

```typescript
class WasmShellRouter {
  private fs:   WasmFsHandler;
  private vo:   WasmVoHandler;
  private git:  WasmGitHandler;
  private zip:  WasmZipHandler;
  private proc: WasmProcHandler;

  capabilities(): Set<Capability>;
  async handle(req: ShellRequest): Promise<ShellResponse>;
}
```

### Handler Details

- **WasmFsHandler**: delegates to `vfs` from `vfs.ts` (already imported in bridge)
- **WasmVoHandler**: calls `wasmMod.compileRunEntry()`, `checkEntry()`, `buildEntry()`, etc.
- **WasmGitHandler**: Phase 2 — isomorphic-git with VFS adapter
- **WasmZipHandler**: Phase 2 — fflate operating on VFS buffers
- **WasmProcHandler**: maintains `Map<string, VirtualExecutable>`; extensible registry

```typescript
interface VirtualExecutable {
  run(args: string[], cwd: string, env: Record<string, string>,
      emit: (ev: ShellEvent) => void): Promise<number>; // exit code
}
```

---

## 8. Integration with Existing Bridge

No breaking changes. `Bridge` gets a new field:

```typescript
export interface Bridge {
  // ... existing fields unchanged ...
  shell: ShellClient;
}
```

`actions.ts` continues to use `bridge().fsListDir(...)` for existing code.  
New code uses `bridge().shell.exec({ kind: "fs.list", path: "..." })`.  
Migration is incremental — existing `fs*` methods can be re-implemented on top of `shell.exec` over time.

---

## 9. File Layout

```
studio/src/lib/shell/
├── protocol.ts          types: ShellOp, ShellRequest/Response/Event, Capability, error codes, result types
├── client.ts            ShellClient class: exec, stream, cd, setEnv, supports
├── transport.ts         ShellTransport interface, TauriTransport, WasmTransport
├── wasm/
│   ├── router.ts        WasmShellRouter
│   ├── fs_handler.ts    wraps VirtualFS
│   ├── vo_handler.ts    wraps wasm module exports
│   ├── git_handler.ts   Phase 2: isomorphic-git
│   ├── zip_handler.ts   Phase 2: fflate
│   └── proc_handler.ts  virtual process registry
└── index.ts             public exports

studio/src-tauri/src/shell/
├── mod.rs               cmd_shell_init, cmd_shell_exec, serde types
├── router.rs            ShellRouter, dispatch logic
├── fs_handler.rs        std::fs + resolve_path
├── vo_handler.rs        vo_runtime / vox crate
├── git_handler.rs       system git subprocess
├── zip_handler.rs       zip crate
└── proc_handler.rs      std::process::Command
```

---

## 10. Implementation Phases

### Phase 1 (current)
- `protocol.ts`, `client.ts`, `transport.ts`, `index.ts`
- Rust: `shell/mod.rs`, `router.rs`, `fs_handler.rs`, `vo_handler.rs` skeleton
- TS WASM: `router.ts`, `fs_handler.ts`, `vo_handler.ts` skeleton
- Wire into `Bridge`

### Phase 2
- Rust: `git_handler.rs`, `zip_handler.rs`, `proc_handler.rs`
- TS WASM: `git_handler.ts` (isogit), `zip_handler.ts` (fflate)
- Full streaming output for `vo.test`, `git.clone`, etc.

### Phase 3
- `proc.spawn` virtual registry with first registered tools
- `vo.get` WASM via fetch
- Terminal UI component using `ShellClient.stream()`
