# Studio GitHub Repo Open / Run / Share Design

**Date**: 2026-04-09  
**Status**: Proposed  
**Scope**: `studio` frontend/backend, `studio/wasm`, `studio/src-tauri`, and the shared Studio launch/session model  
**Related documents**:
- `lang/docs/dev-notes/2026-03-14-studio-rewrite-design.md`
- `lang/docs/dev-notes/2026-03-25-studio-project-location-design.md`
- `lang/docs/dev-notes/2026-04-04-studio-gui-unified-architecture-plan.md`

---

## 1. Problem Statement

Studio should support this product flow as a first-class capability:

1. a user opens a GitHub repository URL such as `https://github.com/vo-lang/MarbleRush`
2. Studio fetches the repository, opens it as a normal session, and can run it immediately
3. Studio exposes a **Share** action that generates a URL
4. another user can open that URL in Studio and get the same runnable project

This must be a real architecture, not a thin special case layered on top of the current `openUrlSession(url)` import path.

The design must work in both:

- **Web Studio**
- **Native Studio**

It must also fit the existing Studio architecture:

- backend/service layering
- typed session lifecycle
- `app.prepare -> compileRun / gui.run` execution model
- runner vs dev mode launch behavior

---

## 2. What Exists Today

Studio already has pieces of this story, but they do not form a coherent product feature.

### 2.1 Existing launch/bootstrap/session path

Current code already has:

- `BootstrapContext` with `initialPath`, `initialUrl`, `initialRunTarget`, `launchUrl`
- web/native startup logic that reads query params, CLI flags, and env vars into those fields
- frontend `ProjectService.openWorkspace()`, `openRunSession(path)`, `openUrl(url)`
- backend `openWorkspaceSession`, `openRunSession`, `openUrlSession`
- `App.svelte` startup branching that opens the initial session and, in runner mode, auto-runs GUI targets

This means the new typed launch model should evolve the existing bootstrap/open pipeline, not replace it wholesale in one step.

### 2.2 Existing generic URL import

Current `openUrlSession(url)` behavior is:

- fetch raw URL bytes
- if it looks like `tar.gz`, extract files
- otherwise treat it as a single text file
- place extracted files into a session root
- heuristically pick `main.vo` or the first `.vo` file

Both web and native already implement this same shape, which is good. However both implementations are still text-biased:

- web tar import only keeps UTF-8 files
- native tar import skips non-UTF-8 archive entries
- native single-file import rejects non-UTF-8 content

This path is reusable as the future `archive_url` source path, but it must be upgraded into a byte-preserving remote import pipeline with typed source metadata.

### 2.3 Existing GitHub project catalog and remote sync

Studio already has substantial authenticated GitHub integration, but it is catalog-centric rather than launch-centric.

Current code already has:

- `GitHubRemoteClient` for GitHub API calls
- `ManagedProject.remote` with `gist` and `repo` remotes
- `ProjectCatalogService.ensureProjectReady()`, `pullProject()`, and `pushProject()`
- Home/catalog UI flows for remote-only projects
- project action menu support for **Open on GitHub**

This is valuable existing code, but it is not yet the public repo open/share feature.

The current catalog path has four hard limits for this feature:

1. **It assumes authenticated catalog context**
   - the current repo flow is designed around logged-in project sync, not arbitrary public repo bootstrap

2. **It is text-only**
   - `pullRepoFiles()` returns `Record<string, string>` and skips large/binary-oriented file paths

3. **It materializes managed-project files, not session provenance**
   - the flow produces local files for a catalog project, not a typed `SessionSource`

4. **It does not produce a canonical share URL**
   - there is still no commit-pinned, session-level share contract

### 2.4 Existing share-adjacent protocol pieces

There is no Share action yet, but there are important pieces already in place:

- query-based launch is already supported on web and native
- runner mode is already driven by launch parameters
- Studio already exposes **Open on GitHub** for managed repo projects

This means the correct share protocol should extend the existing query-based launch system rather than inventing a second routing system.

### 2.5 Important observation

The underlying VFS/runtime stack is already capable of storing byte-oriented files.
The limitation is not the storage layer.
The limitation is the current URL import abstraction and the lack of a typed source model.

That means the correct solution is to introduce a real **source model** and refactor the existing URL import / GitHub catalog paths around it, not to add another special-case importer.

---

## 3. Goals

### 3.1 Product goals

- open a public GitHub repo directly from its GitHub URL
- run it immediately in Studio
- generate a share URL from Studio
- let another user open the share URL in web or native Studio
- make the share URL deterministic by default

### 3.2 Architectural goals

- one authoritative launch/source model for web and native
- no special UI-only GitHub path
- no overloading authenticated GitHub project-sync state as the public open/share protocol
- preserve binary assets during remote import
- keep sharing based on typed source metadata, not ad hoc path guessing
- make session provenance explicit and queryable

### 3.3 UX goals

- opening a raw GitHub repo URL should feel simple
- sharing should produce a clean canonical Studio URL
- share links should open into either dev mode or runner mode intentionally
- shared runs should default to immutable commit-pinned behavior

---

## 4. Non-Goals

This design does **not** require these as prerequisites:

- private GitHub repo sharing
- generic Git provider support beyond GitHub
- gist-based sharing
- overlay/COW filesystem optimization for imported repos
- a new repo-side manifest format
- replacing the existing authenticated GitHub project catalog

Those may be layered on later, but they are not required to land the first correct version of this feature.

---

## 5. Design Principles

1. **Source identity must be typed**
   - `workspace`, `path`, `github_repo`, and `archive_url` are different sources and must not be represented as one raw string

2. **Open and share use the same contract**
   - the same typed launch spec that opens a project must be serializable into a share URL

3. **Share links default to immutable commits**
   - repo URLs may be opened from floating refs, but generated share URLs should pin to a resolved commit by default

4. **Imported remote sources are immutable; sessions are writable**
   - Studio should not treat fetched remote source cache as the editable session root

5. **Public GitHub repo open/share is separate from authenticated GitHub sync**
   - do not overload `ManagedProject.remote` catalog semantics as the authoritative open/share model

6. **Binary-preserving import is mandatory**
   - imported repos must retain exact file bytes, not just UTF-8 text

7. **Runner behavior must be explicit**
   - a shared URL must be able to say “open in dev mode” or “open and run in runner mode”

---

## 6. Target Architecture Overview

The feature is built around a new typed contract:

- **LaunchSpec** — what Studio should open/run
- **ProjectSourceSpec** — where the project comes from
- **ResolvedSource** — immutable fetched source identity with resolved commit metadata
- **SessionSource** — source metadata carried by the active session
- **ShareSpec** — canonical shareable subset of session source + launch intent

High-level flow:

```text
Raw input
  ├─ GitHub repo URL
  ├─ canonical Studio share URL
  ├─ local path
  └─ workspace open
        ↓
LaunchSpec
        ↓
ProjectService / Backend openSession(spec)
        ↓
SourceResolver
  ├─ resolve remote metadata
  ├─ fetch archive bytes if needed
  ├─ extract exact files into immutable source cache
  └─ materialize writable session root
        ↓
SessionInfo { source, share, root, entryPath, ... }
        ↓
Normal Studio compile / prepare / run pipeline
```

This architecture makes GitHub open/share a natural extension of Studio’s session model rather than a side-channel import.

---

## 7. New Authoritative Data Model

### 7.1 `LaunchSpec`

```ts
interface LaunchSpec {
  version: 1;
  mode: 'dev' | 'runner';
  source: ProjectSourceSpec;
  run?: {
    entry?: string | null;
    appKind?: 'auto' | 'code' | 'gui';
    autoRun?: boolean;
  };
}
```

Notes:

- `mode` chooses full IDE vs runner surface
- `run.entry` is relative to the source root or selected subdir root
- `autoRun` means Studio should start execution automatically after opening
- `appKind` is a hint for how the opened app should be launched; `auto` remains valid

### 7.2 `ProjectSourceSpec`

```ts
type ProjectSourceSpec =
  | { kind: 'workspace' }
  | { kind: 'path'; path: string }
  | {
      kind: 'github_repo';
      owner: string;
      repo: string;
      ref?: string | null;
      commit?: string | null;
      subdir?: string | null;
    }
  | {
      kind: 'archive_url';
      url: string;
    };
```

Rules:

- `github_repo` is now first-class
- `commit` wins over `ref` when both exist
- `subdir` means the effective project root is inside the repo
- `archive_url` remains supported, but it is no longer the same thing as a GitHub repo source

### 7.3 `ResolvedSource`

```ts
interface ResolvedSource {
  spec: ProjectSourceSpec;
  resolvedKind: 'workspace' | 'path' | 'github_repo' | 'archive_url';
  resolvedCommit?: string | null;
  requestedRef?: string | null;
  defaultBranch?: string | null;
  htmlUrl?: string | null;
  archiveUrl?: string | null;
  sourceCacheRoot?: string | null;
}
```

This is backend-owned metadata produced by source resolution.

### 7.4 `SessionInfo`

`SessionInfo` should gain authoritative source/share information:

```ts
interface SessionInfo {
  root: string;
  origin: SessionOrigin;
  projectMode: 'single-file' | 'module';
  entryPath: string | null;
  singleFileRun: boolean;
  source: SessionSource;
  share: ShareInfo | null;
}
```

Where:

```ts
type SessionSource =
  | { kind: 'workspace' }
  | { kind: 'path'; path: string }
  | {
      kind: 'github_repo';
      owner: string;
      repo: string;
      requestedRef: string | null;
      resolvedCommit: string | null;
      subdir: string | null;
      htmlUrl: string;
      sourceCacheRoot: string;
    }
  | { kind: 'archive_url'; url: string };

interface ShareInfo {
  spec: LaunchSpec;
  canonicalUrl: string;
  shareable: boolean;
  reason?: string;
}
```

Key point:

- the active session itself knows whether it is shareable and what exact URL should be copied

---

## 8. Canonical Share URL Protocol

### 8.1 Why a canonical Studio URL is required

A raw GitHub URL is a fine input.
It is **not** a complete share protocol.

A real share URL must capture:

- Studio mode (`dev` vs `runner`)
- source kind
- owner/repo identity
- immutable commit or explicit ref
- subdir if the project root is not the repo root
- entry file if not inferred from root
- app kind when needed

### 8.2 Canonical format

The canonical share URL should stay query-based because:

- native Studio already parses launch query parameters from `launchUrl`
- web Studio already bootstraps from `window.location.search`
- query parsing is simpler than hash routing for launch state

Recommended format:

```text
https://studio.vo-lang.org/?mode=runner&source=github&repo=vo-lang/MarbleRush&commit=<sha>&entry=main.vo&app=gui
```

Additional optional parameters:

```text
subdir=demo
ref=main
```

Rules:

- `repo` is encoded as `owner/name`
- generated share URLs should prefer `commit=`
- `ref=` is allowed for explicit non-immutable open behavior but should not be the default for sharing
- `source=github` is the public stable discriminant

### 8.3 Parsing rules

Studio bootstrap should parse into a typed `LaunchSpec`, not into separate loose fields.

New bootstrap shape:

```ts
interface BootstrapContext {
  workspaceRoot: string;
  launchUrl: string | null;
  initialLaunch: LaunchSpec | null;
  platform: 'native' | 'wasm';
}
```

The old fields:

- `initialPath`
- `initialUrl`
- `initialRunTarget`

should become transitional compatibility fields and then be removed after migration.

Current compatibility inputs should continue to work during the migration and be lowered into `LaunchSpec`:

- web query params: `project`, `url`, `run`, `mode`
- native query params: `path`, `project`, `url`, `run`, `mode`
- native CLI/env inputs: `--path`, `--url`, `--run`, `--mode`, `STUDIO_PATH`, `STUDIO_URL`, `STUDIO_RUN`, `STUDIO_MODE`

---

## 9. GitHub Repo Source Resolution

### 9.1 Input normalization

Studio should support these inputs:

1. raw GitHub repo URL
   - `https://github.com/vo-lang/MarbleRush`
2. canonical Studio share URL
   - `...?source=github&repo=vo-lang/MarbleRush&commit=<sha>...`
3. existing explicit archive URL
   - still supported through `archive_url`

For the GitHub feature itself, the authoritative source representation is always `ProjectSourceSpec { kind: 'github_repo', ... }`.

### 9.2 Resolution steps

For `github_repo`:

1. validate `owner` and `repo`
2. if `commit` is present:
   - use it directly as the immutable source key
3. else if `ref` is present:
   - resolve it to a commit SHA
4. else:
   - fetch repo metadata to discover default branch
   - resolve default branch head to a commit SHA
5. build canonical archive download URL
6. download archive bytes
7. extract exact file bytes into immutable source cache
8. materialize a writable session root from that cache
9. discover project root / entry using explicit inputs first, heuristics second

### 9.3 Why resolved commit is required

If a user opens `https://github.com/vo-lang/MarbleRush`, that means “current default branch head”.
That is acceptable for a convenience open action.

It is **not** acceptable as the default share representation because it is not deterministic.

Therefore:

- open may start from floating repo identity
- share must default to immutable `commit`

### 9.4 Binary-preserving extraction

The imported repo must preserve exact bytes for:

- images
- audio
- wasm artifacts
- fonts
- model files
- lockfiles and metadata

Therefore the current text-only tar import path must be replaced with a byte-preserving archive extraction pipeline.

Required behavior:

- reject unsafe paths (`..`, absolute paths, drive prefixes)
- keep all file bytes as-is
- preserve directory structure
- ignore symlinks for the initial version
- ignore executable mode bits for the initial version unless a later requirement proves necessary

---

## 10. Source Cache vs Session Root

### 10.1 Problem

A fetched GitHub repo should not be edited in-place as the authoritative fetched source.
Otherwise:

- reopening the same shared project can see stale local mutations
- a commit-keyed cache becomes impure
- the share URL no longer corresponds to what the session contains

### 10.2 Correct model

Split remote sessions into two layers:

1. **Immutable source cache**
   - keyed by source identity and resolved commit
   - exact extracted repo bytes

2. **Writable session root**
   - materialized from source cache for the active session
   - safe for normal editing and file operations

### 10.3 Cache layout

Recommended layout:

Native:

```text
<workspaceRoot>/.studio-sources/github/<owner>/<repo>/<commit>/
<workspaceRoot>/.studio-sessions/github/<owner>/<repo>/<commit>/<session-id>/
```

Web VFS:

```text
/__studio/sources/github/<owner>/<repo>/<commit>/
/__studio/sessions/github/<owner>/<repo>/<commit>/<session-id>/
```

### 10.4 Session metadata sidecar

Each materialized session root should contain or be associated with a metadata record such as:

```json
{
  "version": 1,
  "source": {
    "kind": "github_repo",
    "owner": "vo-lang",
    "repo": "MarbleRush",
    "requestedRef": "main",
    "resolvedCommit": "abc123...",
    "subdir": null
  }
}
```

This does not need to be user-facing, but it makes provenance explicit for future reopen/share/publish flows.

---

## 11. Entry and Project Root Resolution

### 11.1 Resolution order

Project root and run entry should be determined in this order:

1. explicit `subdir`
2. explicit `run.entry`
3. repo root `vo.mod` + `main.vo`
4. repo root `vo.mod` + `app.vo`
5. repo root single `main.vo`
6. exactly one discoverable project under repo root
7. otherwise: open in dev mode without auto-run and prompt selection later

### 11.2 Runner-mode rule

If `mode=runner` and Studio cannot determine a unique runnable entry, it should fail clearly instead of guessing.

That means:

- canonical share URLs generated by Studio should always include enough information to avoid ambiguity
- raw repo URLs may still use heuristics for convenience open

### 11.3 App kind rule

`appKind` should be carried in the share spec when known.
This avoids depending on local per-user GUI classification state for a remote shared project.

Recommended values:

- `gui`
- `code`
- `auto`

Generated share URLs should use `gui` or `code` when the current session already knows the answer.

---

## 12. Frontend Architecture Changes

### 12.1 `ProjectService`

Current `ProjectService` methods should be unified behind one typed open path:

```ts
openLaunch(spec: LaunchSpec): Promise<SessionInfo>
```

Compatibility wrappers may remain temporarily:

- `openWorkspace()`
- `openRunSession(path)`
- `openUrl(url)`

But they should translate into `LaunchSpec` internally.

### 12.2 `App.svelte` bootstrap

Startup logic should no longer branch on loose fields like:

- `initialPath`
- `initialUrl`
- `initialRunTarget`

Instead:

```ts
const launch = bootstrap.initialLaunch;
if (launch) {
  openedSession = await registry.project.openLaunch(launch);
} else {
  openedSession = await registry.project.openWorkspace();
}
```

### 12.3 Share action surface

Studio should expose a share action in the dev surface whenever `session.share?.shareable === true`.

Recommended UI behavior:

- **Share** button in the dev toolbar/header
- action copies `session.share.canonicalUrl`
- secondary action: **Open on GitHub** when source kind is `github_repo`

If the current session is not shareable:

- either hide the button
- or show a disabled share action with a clear reason

### 12.4 Session state

The frontend session store should carry source/share data, not only root paths.
This allows:

- rendering source badges
- deciding whether Share is available
- building future “Publish & Share” flows cleanly

---

## 13. Backend Changes

### 13.1 Backend interface

Add a typed open contract:

```ts
openSession(spec: LaunchSpec): Promise<SessionInfo>
```

The old methods can remain temporarily as adapters.

### 13.2 Native backend

Native should evolve the code that already exists today:

- extend Rust bootstrap parsing to populate `initialLaunch` while keeping current fields during migration
- refactor the current `cmd_open_url_session` / `import_url_project` path into a source-aware resolver used by both `archive_url` and `github_repo`
- keep existing session root / entry detection heuristics where they are still correct
- add byte-preserving archive fetch + extract
- add source cache + session materialization
- return typed `SessionInfo.source/share` payload

### 13.3 Web backend

Web should evolve the same way in the browser VFS:

- extend `getBootstrapContext()` to produce `initialLaunch` while keeping current compatibility fields during migration
- refactor the current `openUrlSession()` / `importProjectFromUrl()` path into the authoritative `archive_url` implementation
- add GitHub source resolution on top of that remote import layer
- replace text-only tar extraction with byte-preserving extraction into VFS
- materialize writable sessions from immutable source cache
- return typed `SessionInfo.source/share`

### 13.4 Important boundary decision

Do **not** make frontend services directly know GitHub download/archive logic.
That belongs inside backend-specific source-resolution logic.

Frontend should depend on typed session data, not on provider-specific fetch flows.

### 13.5 Reuse boundary for existing GitHub helpers

Existing GitHub code should be reused selectively:

- `GitHubRemoteClient` and existing repo metadata patterns are reusable for GitHub identity / metadata resolution where the ownership matches
- current catalog models such as `GitHubRemoteRef { kind: 'repo' }` are useful bridging concepts
- current `pullRepoFiles()` is **not** the authoritative repo-open path for this feature because it is auth-bound and text-only
- `ProjectCatalogService` should stay responsible for managed-project sync UX, not for bootstrap/session source resolution

---

## 14. Relationship to Existing GitHub Project Catalog

Studio already has authenticated GitHub project catalog/sync functionality.
That system should remain separate.

### 14.1 What the catalog is for

- user-owned remote projects
- push/pull/diff
- project manifest tracking
- GitHub account connection

### 14.2 What the new source/share system is for

- opening public GitHub repos directly
- deterministic share URLs
- source provenance and session identity
- public runnable links between users

### 14.3 Integration points

The two systems can integrate at the edges:

- if the current managed project has a GitHub repo remote and is in a shareable synced state, Share can derive a `github_repo` share spec from that remote
- existing catalog flows such as `ensureProjectReady()` / `pullProject()` can remain for managed-project UX without becoming the authoritative public bootstrap path
- if the project is local-only or dirty, Share should not silently invent a remote source

This keeps the public open/share model clean while still reusing existing GitHub data when appropriate.

---

## 15. Share Semantics

### 15.1 Default share behavior

Share should generate a URL pinned to the currently resolved commit.

Example:

```text
https://studio.vo-lang.org/?mode=runner&source=github&repo=vo-lang/MarbleRush&commit=8e9d...&entry=main.vo&app=gui
```

### 15.2 Why not share branch HEAD by default

Branch HEAD moves.
A shared runner link should not silently start a different program tomorrow.

Therefore commit pinning is the only correct default.

### 15.3 Optional future extension

A future secondary share action may support:

- **Share latest on branch**

But that should be clearly labeled as floating and non-deterministic.

---

## 16. Failure Model

Failures should be explicit and typed.

Examples:

- repository not found
- repository is private or inaccessible
- ref not found
- archive download failed
- archive extraction rejected unsafe paths
- no runnable project entry found
- runner mode requested but entry resolution is ambiguous

Studio should fail with these precise messages instead of falling back to a generic URL import interpretation.

---

## 17. Implementation Plan

Each phase lists the exact files that must change, what changes, and known risks.
File paths are relative to `studio/`.

### Phase 1 — Typed launch/source model and `openSession(spec)` plumbing

Goal: introduce `LaunchSpec`, `ProjectSourceSpec` as new types, add a single typed `openSession(spec)` path through the entire stack, and keep all existing open methods working as compatibility wrappers.

**Do not extend `SessionInfo` with `source`/`share` yet.** That happens in Phase 3/4 when there is real source metadata to populate. Phase 1 only adds the new launch→open pipeline.

Files to change:

1. **`src/lib/types.ts`**
   - add `LaunchSpec`, `ProjectSourceSpec` type definitions
   - add `initialLaunch: LaunchSpec | null` to `BootstrapContext`
   - keep `initialPath`, `initialUrl`, `initialRunTarget` during migration

2. **`src/lib/backend/backend.ts`**
   - add `openSession(spec: LaunchSpec): Promise<SessionInfo>` to the `Backend` interface
   - keep `openWorkspaceSession`, `openRunSession`, `openUrlSession` as-is

3. **`src/lib/backend/web_backend.ts`**
   - extend `getBootstrapContext()` to parse query params into `initialLaunch` (currently parses `mode`, `project`, `url`)
   - implement `openSession(spec)` that dispatches to existing `openWorkspaceSession`/`openRunSession`/`openUrlSession` based on `spec.source.kind`
   - keep all existing open methods unchanged

4. **`src/lib/backend/native_backend.ts`**
   - add `openSession(spec)` that invokes a new Tauri command `cmd_open_session`
   - keep existing `openWorkspaceSession`/`openRunSession`/`openUrlSession` calls

5. **`src-tauri/src/state.rs`**
   - add `initial_launch` field to `BootstrapContext` struct (serialized as `initialLaunch`)
   - extend `parse_launch_config()` to populate `initial_launch` from the existing `launch_url` query params and CLI args (new params: `source`, `repo`, `commit`, `ref`, `subdir`, `entry`, `app`)
   - keep `initial_path`, `initial_url`, `initial_run_target` during migration

6. **`src-tauri/src/commands/session.rs`**
   - add `cmd_open_session(spec: LaunchSpec)` Tauri command that dispatches to existing `cmd_open_workspace_session`/`cmd_open_run_session`/`cmd_open_url_session` logic
   - keep all existing commands unchanged

7. **`src/lib/services/project_service.ts`**
   - add `openLaunch(spec: LaunchSpec): Promise<SessionInfo>` that calls `backend.openSession(spec)` then `bindSession()`
   - keep `openWorkspace()`, `openRunSession()`, `openUrl()` as compatibility wrappers

8. **`src/App.svelte`**
   - add a new branch: if `bootstrap.initialLaunch` is present, call `registry.project.openLaunch(bootstrap.initialLaunch)`
   - keep existing `initialPath`/`initialUrl`/`initialRunTarget` branches as fallback

Known risks:

- **Rust↔TS serialization**: `LaunchSpec` must be serializable as JSON across the Tauri boundary. Use `serde` on the Rust side with `#[serde(tag = "kind")]` for the `ProjectSourceSpec` discriminated union. Test round-trip serialization.
- **No breakage contract**: Phase 1 must pass all existing Studio startup paths unchanged. The new `openSession` path is additive. Verify by testing: workspace open, `--path`, `--url`, `--run`, `?mode=runner&url=...`.
- **`initialLaunch` is `null` for all removed inputs initially**: the old fields still drive the old branches. `initialLaunch` only becomes non-null when the new query params (`source=`, `repo=`, etc.) are present.

### Phase 2 — Byte-preserving remote import

Goal: upgrade the existing URL import pipeline to preserve binary file bytes, while keeping the same external behavior for text-only archives.

Files to change:

1. **`src/lib/backend/web_backend.ts`**
   - add `setFileBytes(path: string, bytes: Uint8Array)` alongside existing `setFile()`. It writes to `vfsFiles` and metadata maps but does **not** write to the text `files` map. This is correct because binary files should not be readable via `readFile()` (which returns `string`).
   - change `parseTarTextFiles()` → `parseTarFiles()`: return `Array<{ path: string; bytes: Uint8Array }>` instead of text-only. Keep existing `sanitizeTarPath()` for path safety.
   - change `importProjectFromUrl()` to use `setFileBytes()` for tar entries and keep `setFile()` for the single-file text fallback.
   - verify: `listDirEntries()` already iterates `vfsFiles` keys, so binary files will appear in directory listings.

2. **`src-tauri/src/commands/session.rs`**
   - change `extract_tar_gz_project()`: replace `String::from_utf8(buf)` + skip on failure with `fs::write(&target, &buf)` (write raw bytes). This is a one-line change per entry.
   - change `write_text_import()`: keep as-is for single-file imports (text is still correct there).

3. **Both backends: source cache layout** (preparation for Phase 3)
   - web: add `/__studio/sources/` and `/__studio/sessions/` VFS directory conventions
   - native: add `.studio-sources/` and `.studio-sessions/` under workspace root
   - for now, `archive_url` imports still go to the existing `url/` session root. Source cache is exercised in Phase 3.

Known risks:

- **Web VFS dual-map consistency**: `files` (text) and `vfsFiles` (bytes) are currently kept in sync by `setFile()`. After this change, binary-only files exist in `vfsFiles` but not `files`. Code that iterates `files` (currently only `listDirEntries` uses `vfsFiles`, not `files`) must be audited. Grepping for `files.` usage in `web_backend.ts` is required.
- **Existing archive URL behavior must not regress**: test that `?url=https://...tar.gz` still works for text-only archives.

### Phase 3 — Public GitHub repo source resolver and `SessionInfo.source`

Goal: add `github_repo` source kind, resolve GitHub repo URLs to commit-pinned archives, fetch and extract them, and start carrying `SessionSource` on `SessionInfo`.

**This is the phase where `SessionInfo` gains `source`.** Not Phase 1, not Phase 4.

Files to change:

1. **`src/lib/types.ts`**
   - add `SessionSource` type (discriminated union: `workspace | path | github_repo | archive_url`)
   - extend `SessionInfo` with `source: SessionSource | null` (nullable for old API preservation during migration)

2. **`src-tauri/src/state.rs`**
   - add `SessionSource` enum (serde-tagged) and `source: Option<SessionSource>` to `SessionInfo` struct
   - update `session_info()` helper to accept optional source

3. **`src-tauri/src/commands/session.rs`**
   - add GitHub URL detection: if the URL matches `https://github.com/{owner}/{repo}[/tree/{ref}]`, parse into `ProjectSourceSpec { kind: 'github_repo' }`
   - add GitHub source resolution:
     - fetch `https://api.github.com/repos/{owner}/{repo}` (no auth for public repos) to get `default_branch`
     - resolve ref to commit SHA via `https://api.github.com/repos/{owner}/{repo}/commits/{ref}`
     - download archive: `https://github.com/{owner}/{repo}/archive/{commit}.tar.gz` (no auth, no API rate limit)
   - extract archive using the byte-preserving pipeline from Phase 2
   - write into source cache: `.studio-sources/github/{owner}/{repo}/{commit}/`
   - materialize session root: `.studio-sessions/github/{owner}/{repo}/{commit}/{session-id}/` (copy from source cache)
   - return `SessionInfo` with `source: { kind: 'github_repo', owner, repo, resolvedCommit, ... }`
   - integrate into `cmd_open_session(spec)` for `github_repo` kind

4. **`src/lib/backend/web_backend.ts`**
   - same GitHub URL detection and resolution logic, but using `fetch()` instead of `ureq`
   - same source cache / session materialization in VFS
   - same `SessionSource` population

5. **`src/lib/backend/native_backend.ts`** — no change beyond what Phase 1 already added

6. **`src/stores/session.ts`** — `SessionState` gains optional `source` field for UI consumption

7. **`src/App.svelte`** — no change needed; `SessionInfo.source` is carried through existing `bindSession` flow

Technical path for GitHub archive download (why not `pullRepoFiles()`):

- `pullRepoFiles()` requires auth token, returns text-only `Record<string, string>`, and fetches blobs one by one (N+1 API calls)
- GitHub public archive URL `https://github.com/{owner}/{repo}/archive/{ref}.tar.gz` is a single HTTP request, no auth, includes all files as bytes
- this is the correct path for public repo open

Known risks:

- **GitHub API rate limiting**: unauthenticated API calls are limited to 60/hour. The metadata resolution (2 calls: repo info + commit resolve) is low-volume. The archive download itself is not an API call. If rate limiting becomes a problem, Studio can optionally use a GitHub token from the catalog if one is connected.
- **Large repos**: GitHub archive downloads can be large. Studio should enforce a reasonable size limit (e.g. 50MB) and fail with a clear message.
- **CORS on web**: `https://github.com/{owner}/{repo}/archive/{ref}.tar.gz` may not have CORS headers. Web Studio may need a proxy or use the API endpoint `https://api.github.com/repos/{owner}/{repo}/tarball/{ref}` which does support CORS. This must be tested early.
- **`SessionInfo.source` is nullable**: all existing consumers see `null` for non-GitHub sessions. This is safe because they only read `root`, `origin`, `projectMode`, `entryPath`, `singleFileRun` today.

### Phase 4 — Share URL generation and Share UI

Goal: generate canonical share URLs from `SessionInfo.source` and expose a Share action in the UI.

**Prerequisite**: Phase 3 must be complete so `SessionInfo.source` is populated for GitHub sessions.

Files to change:

1. **`src/lib/types.ts`**
   - add `ShareInfo { canonicalUrl: string; shareable: boolean; reason?: string }`
   - extend `SessionInfo` with `share: ShareInfo | null`

2. **`src-tauri/src/state.rs`**
   - add `ShareInfo` struct and `share: Option<ShareInfo>` to `SessionInfo`
   - `session_info()` computes `ShareInfo` when source is `github_repo` with a resolved commit

3. **`src/lib/backend/web_backend.ts`**
   - `buildSessionInfo()` computes `ShareInfo` when source is `github_repo`
   - canonical URL format: `https://studio.vo-lang.org/?mode={mode}&source=github&repo={owner}/{repo}&commit={sha}&entry={entry}&app={appKind}`

4. **`src/stores/session.ts`**
   - `SessionState` gains `share: ShareInfo | null`
   - `sessionOpen()` passes it through

5. **`src/App.svelte`** and/or **`src/components/DevWorkbench.svelte`**
   - pass `share` state down to toolbar
   - add Share button (visible when `session.share?.shareable === true`)
   - action: copy `session.share.canonicalUrl` to clipboard

6. **Bootstrap parsing** (both backends)
   - parse `source=github&repo=...&commit=...&entry=...&app=...&mode=...` into `LaunchSpec` with `github_repo` source
   - this closes the round-trip: Share URL → open in Studio → same session

Known risks:

- **Canonical URL domain**: `https://studio.vo-lang.org/` must actually serve web Studio. If not deployed yet, the share URL can use relative format (`?source=github&...`) for local/dev testing.
- **`appKind` inference**: the current session must know whether it is `gui` or `code` before generating the share URL. This is already tracked by `sessionProjectHasGui` in `App.svelte` and `ManagedProjectConfig.hasGui` in the catalog. Phase 4 should read this from session state, not require the user to specify it.

### Phase 5 — Catalog integration and publish/share polish

Goal: bridge the existing managed-project catalog with the new share system.

Files to change:

1. **`src/lib/services/project_catalog_service.ts`**
   - when a managed project has `remote.kind === 'repo'` and is in sync, derive `ShareInfo` from its `owner/repo` + last synced commit hash
   - expose a method like `getShareInfoForProject(project): ShareInfo | null`

2. **`src/components/home/ProjectActionsMenu.svelte`**
   - add **Share** action alongside existing **Open on GitHub** when `ShareInfo` is available
   - keep **Open on GitHub** as a separate action

3. **`src/components/Home.svelte`**
   - wire share action through to catalog service

Known risks:

- **Sync state accuracy**: `ManagedProject.currentRemoteHash` may be stale. Share should only work when the project is confirmed in-sync. The existing `syncState()` function can gate this.
- **This phase is optional for MVP**: the core GitHub open/run/share flow works after Phase 4. Phase 5 is about making it nicer for users who also use the managed project catalog.

---

## 18. Why This Is the Right Shape

This design fixes the real boundary problems instead of adding another one-off import path.

It gives Studio:

- a typed source model
- a typed launch model
- a deterministic sharing model
- a clean separation between remote source provenance and editable session roots
- a real GitHub repo concept instead of an opaque URL string

Most importantly, it aligns with Studio’s existing architecture rather than fighting it.

The feature becomes:

- understandable
- testable
- extensible
- reproducible

That is the correct foundation for direct GitHub open/run/share.
