# Shell API LLM Enhancements

**Date**: 2026-03-04  
**Status**: Implementation

---

## Motivation

The current `ShellClient` API is sufficient for the interactive terminal but has
gaps that make programmatic use by an LLM agent inefficient or impossible:

1. `exec()` returns `unknown` — no per-op type inference
2. No batch file read — LLM reading 10 files = 10 shell-handler compilations in WASM
3. `grep` lives only in `terminal_cmd.ts` TS helpers — not a real ShellOp
4. `vo.check` returns `{ok, errors: string[]}` — no file/line/col info, LLM can't
   pinpoint errors to fix them
5. `vo.run` has no `stdin` — can't feed inputs to programs

---

## Changes

### 1. New Ops

#### `fs.readMany`
```
{ kind: 'fs.readMany'; paths: string[] }
→ FsReadManyResult: Array<{path, content} | {path, error}>
```
Reads multiple files atomically. Each file either succeeds with content or fails
with an error string. Never throws — errors are per-file inline.

#### `fs.grep`
```
{ kind: 'fs.grep'; path: string; pattern: string;
  recursive?: boolean; caseInsensitive?: boolean; fixedString?: boolean }
→ FsGrepResult: FsGrepMatch[]
```
Server-side grep, executed by the Vo shell handler. `fixedString: true` treats
`pattern` as a literal (like `grep -F`). Default is regexp.

### 2. Structured `vo.check` Output

Old: `{ ok: boolean; errors: string[] }`  
New: `{ ok: boolean; diags: VoCheckDiag[] }`

```typescript
interface VoCheckDiag {
  file:     string;   // workspace-relative path
  line:     number;   // 1-indexed
  col:      number;   // 1-indexed
  message:  string;
  severity: 'error' | 'warning';
}
```

Tauri path: `vox.CompileFile/CompileDir` returns a structured error string
(`file:line:col: message`); parsed into `VoCheckDiag[]` in `vo_ops.vo`.

WASM path: `WasmVoHandler.check()` upgraded to return `VoCheckResult`. Error
message from `compileRunEntry` is parsed with a best-effort regex.

### 3. `vo.run` Stdin

```typescript
{ kind: 'vo.run'; path: string; mode?: VoRunMode; args?: string[]; stdin?: string }
```

Tauri: piped into `exec.Command.Stdin`. WASM: ignored (no WASM binding for stdin;
program proceeds with empty stdin).

### 4. Per-Op Typed `exec()` Overloads

`ShellClient.exec()` gets typed overloads so callers get correct return types
without casting:

```typescript
shell.exec({ kind: 'fs.list', path: '/' })     // → Promise<FsListResult>
shell.exec({ kind: 'fs.stat', path: '...' })   // → Promise<FsStatResult>
shell.exec({ kind: 'fs.read', path: '...' })   // → Promise<string>
shell.exec({ kind: 'fs.readMany', paths: [] }) // → Promise<FsReadManyResult>
shell.exec({ kind: 'fs.grep', ... })           // → Promise<FsGrepResult>
shell.exec({ kind: 'vo.check', path: '...' }) // → Promise<VoCheckResult>
shell.exec({ kind: 'vo.run', path: '...' })   // → Promise<VoRunResult>
// etc.
```

---

## Files Modified

### TypeScript
| File | Change |
|------|--------|
| `src/lib/shell/protocol.ts` | New ops, result interfaces, `OpResultMap` |
| `src/lib/shell/client.ts` | Typed `exec()` overloads |
| `src/lib/shell/wasm/vo_handler.ts` | `check()` → `VoCheckResult` |
| `src/lib/terminal_cmd.ts` | `grep` uses `fs.grep` ShellOp; `renderResult` handles new types |

### Vo (shell handler)
| File | Change |
|------|--------|
| `studio/vo/shell/fs.vo` | `handleFsReadMany`, `handleFsGrep` |
| `studio/vo/shell/vo_ops.vo` | Structured `vo.check` diags; `vo.run` stdin |
| `studio/vo/shell/main.vo` | Dispatch for `fs.readMany`, `fs.grep` |

---

## Result Type Reference

```typescript
// fs
type FsListResult     = FsStatResult[];
interface FsStatResult  { name: string; path: string; isDir: boolean; size?: number; modifiedMs?: number }
type FsReadResult     = string;
type FsReadManyResult = Array<{ path: string; content: string } | { path: string; error: string }>;
interface FsGrepMatch   { path: string; line: number; text: string }
type FsGrepResult     = FsGrepMatch[];

// vo
interface VoCheckDiag  { file: string; line: number; col: number; message: string; severity: 'error'|'warning' }
interface VoCheckResult { ok: boolean; diags: VoCheckDiag[] }
interface VoRunResult   { stdout: string }
interface VoBuildResult { output: string }
interface VoDumpResult  { bytecode: string }
interface VoInitResult  { path: string }

// git
interface GitLogEntry    { id: string; summary: string; authorName: string; timeUnix: number }
interface GitStatusEntry { path: string; status: string }
interface GitBranchEntry { name: string; isHead: boolean }

// zip
interface ZipListEntry { name: string; size: number; compressedSize: number; isDir: boolean }

// http
interface HttpResult { statusCode: number; status: string; headers: string[]; body: string }
```

---

## Capability Notes (WASM vs Tauri)

| Op | WASM | Tauri |
|----|------|-------|
| `fs.readMany` | ✓ (VFS) | ✓ |
| `fs.grep` | ✓ (VFS + regexp) | ✓ |
| `vo.check` | ✓ (WASM compiler) | ✓ |
| `vo.run` stdin | ✗ (no WASM binding) | ✓ |
| `vo.run` general | ✓ | ✓ |
