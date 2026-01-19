---
Title: Vo Playground (Web-first + WASM + Tauri) RFC
Status: Draft
Owner: Vibe Studio
---

# 1. Summary
This RFC defines a cross-platform Vo Playground that runs in the browser and on desktop with a single Web UI, using a WASM-compiled Vo runtime and a Tauri shell for desktop distribution.

The goal is a focused Playground (not an IDE) that provides:
- A code editor
- One-click run
- Deterministic output capture
- Examples and shareable snippets

# 2. Goals
- Web-first experience with identical behavior on Desktop.
- Single UI implementation shared across Web + Desktop.
- Local execution in the browser via WASM (no server required).
- Deterministic output capture (stdout/stderr) and runtime status.
- Simple embedding and hosting (static site).

# 3. Non-Goals
- Full IDE features (project management, LSP, tasks, debugging).
- Plugin/extension system.
- Multi-file workspace management (beyond minimal example bundles).

# 4. High-Level Architecture
```
+-------------------+        +--------------------+
| Playground UI     |        | Desktop Shell      |
| (Web UI)          |        | (Tauri)            |
|  - Editor (Monaco)|        |  - WebView         |
|  - Run/Output     |<------>|  - OS Integration  |
+---------+---------+        +---------+----------+
          |                            |
          v                            v
+-------------------+        +--------------------+
| Vo WASM Runtime   |  (same binary used in both) |
|  - compile        |                              |
|  - run            |                              |
|  - stdio capture  |                              |
+-------------------+                              |
```

# 5. Module Boundaries
## 5.1 Playground UI (Web)
- Monaco editor integration
- Examples menu + templates
- Run/Stop buttons and output panel
- Output formatting and status view

## 5.2 Vo WASM Runtime
- Public API for compile/run
- Runtime limits (time + memory)
- Output capture buffer
- Error reporting with source positions

## 5.3 Host Bridge
- Web: JS/WASM bindings (TypeScript)
- Desktop: Tauri wrapper + WebView asset hosting
- Shared API shape for Web + Desktop

## 5.4 Example Pack
- Curated code snippets
- Each example is a single file, with metadata (title, description)

# 6. Runtime Flow
1. User edits code in Monaco.
2. UI calls `compile(source)`.
3. If compile succeeds, UI calls `run(program, options)`.
4. Output and status are streamed or polled.
5. UI renders stdout/stderr and runtime status.

# 7. API Sketch (WASM)
```ts
// TypeScript view of WASM exports
export interface VoWasmRuntime {
  compile(source: string): CompileResult;
  run(program: ProgramHandle, options?: RunOptions): RunResult;
  reset(): void;
}

export interface CompileResult {
  ok: boolean;
  program?: ProgramHandle;
  error?: CompileError;
}

export interface CompileError {
  message: string;
  line: number;
  column: number;
}

export interface RunOptions {
  stdin?: string;
  timeLimitMs?: number;
  memoryLimitMb?: number;
}

export interface RunResult {
  status: "ok" | "error" | "timeout" | "oom";
  stdout: string;
  stderr: string;
}
```

# 8. Consistency Constraints (Web + Desktop)
- Identical runtime binary in both targets.
- Output format and error formatting are consistent.
- Same default limits and runtime options.
- Deterministic example outputs.

# 9. UI Layout Sketch
```
+------------------------------------------------------+
| Vo Playground                      [Run] [Reset]     |
+-------------------------+----------------------------+
| Editor (Monaco)         | Output / Console           |
|                         |                            |
|                         |                            |
|                         |                            |
+-------------------------+----------------------------+
| Examples / Docs / Help                               |
+------------------------------------------------------+
```

# 10. Security & Resource Limits
- Run within WASM sandbox (browser/tauri webview).
- Default time limit for execution.
- Default memory limit for runtime heap.
- Explicit termination on limit violations.

# 11. Milestones
## Phase 0: Spike (1–2 days)
- Compile Vo to WASM and execute a trivial program in browser.
- Capture stdout and verify error reporting.

## Phase 1: WASM Runtime API (3–5 days)
- Stable compile/run/reset API surface.
- Error model finalized.
- Basic limits (time/memory).

## Phase 2: Web UI (1–2 weeks)
- Monaco editor integration.
- Run button + output panel.
- Examples list.

## Phase 3: Desktop Packaging (3–5 days)
- Tauri shell with local assets.
- Test parity between Web and Desktop.

## Phase 4: Polish (1–2 weeks)
- Output formatting, status indicators.
- Error annotations in editor.
- Shareable URL encoding (optional).

# 12. Open Questions
- Do we need multi-file support or only single file?
- Do we allow import of standard library subsets?
- How strict should time/memory limits be by default?

# 13. Acceptance Criteria
- Web and Desktop produce identical output for examples.
- Compile + run flow is stable for basic programs.
- Runtime errors include line/column info.
- Output capture is deterministic and complete.
