# Studio Project Location & Open Project Design

**Status**: In Progress  
**Date**: 2026-03-25

## Problem

Native Studio users cannot choose where to create projects (always `~/.studio/workspace`), and cannot open existing projects from the Home page without CLI flags. Web users are fine since everything lives in a browser VFS sandbox.

## Design Principles

1. **Workspace root (`~/.studio/workspace`) keeps its current role** — Studio's internal data space and default project location. No renaming, no semantic change.
2. **TERM stays scoped to session root** — this is correct IDE behavior, not a limitation.
3. **Same UI flow, different platform capabilities** — web and native share components; native conditionally shows pickers and extra buttons.
4. **Recent projects list, not multi-root scanning** — simple, predictable, low overhead.

## What We Are NOT Doing

- No `studioDataRoot` / `libraryRoots` / `accessScope` concepts
- No project identity model overhaul (`projectConfigKey` is already path-based)
- No TERM boundary changes
- No domain model refactor as a prerequisite

## Architecture Changes

### 1. Backend Dialog Capability

```typescript
// backend.ts — new methods
pickDirectory(defaultPath?: string): Promise<string | null>;
pickFile(defaultPath?: string, filters?: { name: string; extensions: string[] }[]): Promise<string | null>;
```

- **Native**: Tauri `dialog` API (`FileDialogBuilder`)
- **Web**: returns `null` (UI hides picker based on `platform`)

Tauri commands:

```rust
// commands/dialog.rs
cmd_pick_directory(default_path: Option<String>) -> Result<Option<String>, String>
cmd_pick_file(default_path: Option<String>) -> Result<Option<String>, String>
```

### 2. Recent Projects

New module `project_catalog/recent.ts`:

```typescript
interface RecentProject {
  name: string;
  type: ManagedProjectType;
  localPath: string;
  entryPath: string | null;
  openedAt: number; // ms timestamp
}
```

- `addRecent(project)` — called on open/create
- `loadRecent()` — on startup
- `removeRecent(localPath)` — manual removal
- LRU cap: 30 entries
- Storage: localStorage `vo_studio_recent_projects_v1`

Catalog merge order:

```
final = merge(
  discoverLocalProjects(workspaceRoot),
  recentProjects.filter(not already in workspace discover),
  remoteProjects,
)
```

### 3. Create Project with Location

```typescript
// project_catalog_service.ts — updated signatures
async createSingleProject(name: string, location?: string): Promise<ManagedProject>
async createModuleProject(name: string, location?: string): Promise<ManagedProject>
```

When `location` is provided and outside workspace root, use a new bypass backend method:

```typescript
// backend.ts — new method
createProjectFiles(files: { path: string; content: string }[]): Promise<void>;
```

Native: writes via `std::fs` directly (absolute paths, no `resolve_path`).  
Web: writes to VFS as normal.

### 4. CreateProjectModal Location Field

```
┌───────────────────────────────────────┐
│  Create New                           │
│                                       │
│  [Single File]  [Module Project]      │
│                                       │
│  Name: [my_app                     ]  │
│                                       │
│  Location: [~/code          ] [Browse]│  ← native only
│  Location: Browser Workspace          │  ← web (read-only)
│                                       │
│  Creates ~/code/my_app.vo             │  ← live preview
│                                       │
│  [Cancel]                   [Create]  │
└───────────────────────────────────────┘
```

- Native: editable path + Browse button, default = last used location (localStorage), fallback = workspace root
- Web: read-only label "Browser Workspace", no Browse

### 5. Home "Open Project" Button

Visible only when `platform === 'native'`.

Flow:

```
Click "Open Project"
  → pickDirectory() (or pickFile for .vo)
  → user cancels → noop
  → openRunSession(selectedPath)
  → addRecent(project)
  → bindDevSession(session)
  → switch to develop mode
```

### 6. Catalog Discover Decoupled from Session

Problem: `cmd_discover_projects` uses `resolve_path(session_root, root)`. When session root is an external project, workspace root cannot be resolved.

Solution: new Tauri command that bypasses session root:

```rust
cmd_discover_workspace_projects(state) -> Vec<DiscoveredProject>
// Scans state.workspace_root() directly, no resolve_path
```

Frontend: `ProjectCatalogService` uses this dedicated command for workspace discovery, independent of current session state.

## Implementation Steps

1. **Backend dialog** — Tauri dialog commands + Backend interface + NativeBackend/WebBackend impl
2. **Recent projects** — `recent.ts` module + catalog merge logic
3. **CreateProjectModal** — location field with platform-conditional rendering
4. **External path creation** — `createProjectFiles` backend method + catalog service changes
5. **Open Project** — Home button + flow wiring
6. **Catalog discover decoupling** — `cmd_discover_workspace_projects` + catalog service update

## Platform Behavior Summary

| Feature | Web | Native |
|---|---|---|
| Create location | Fixed (VFS) | User-chosen via OS dialog |
| Open existing | N/A | OS directory/file dialog |
| Recent projects | From VFS opens | From any local path |
| TERM scope | VFS root | Session root |
| Project discover | VFS workspace | Workspace root + recent list |
