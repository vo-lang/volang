import type { Readable } from 'svelte/store';

export type ManagedProjectType = 'single' | 'module';
export type ProjectSyncState = 'local-only' | 'remote-only' | 'in-sync' | 'local-ahead' | 'remote-ahead' | 'diverged' | 'unknown';
export type ProjectStatusTone = 'local' | 'cloud' | 'synced' | 'pending' | 'danger' | 'checking';

export interface ProjectStatusPresentation {
  label: string;
  glyph: string;
  tone: ProjectStatusTone;
}

export interface GitHubUser {
  login: string;
  avatarUrl: string;
  htmlUrl: string;
  name: string | null;
}

export interface GitHubRemoteRef {
  kind: 'gist' | 'repo';
  gistId?: string;
  owner?: string;
  repo?: string;
}

export interface ManagedProjectConfig {
  hasGui: boolean;
}

export interface ManagedProject {
  name: string;
  type: ManagedProjectType;
  localPath: string | null;
  entryPath: string | null;
  remote: GitHubRemoteRef | null;
  pushedAt: string | null;
  remoteUpdatedAt: string | null;
  syncedHash: string | null;
  currentLocalHash: string | null;
  currentRemoteHash: string | null;
  hasGui: boolean;
}

export interface ManifestProject {
  name: string;
  type: ManagedProjectType;
  remote: GitHubRemoteRef;
  pushedAt: string;
  contentHash?: string;
  entryPath?: string;
  hasGui?: boolean;
}

export interface ProjectManifest {
  v: number;
  projects: ManifestProject[];
}

export interface RemoteMetadata {
  updatedAt: string;
}

export interface ProjectDiffEntry {
  path: string;
  localContent: string | null;
  remoteContent: string | null;
  status: 'added' | 'removed' | 'modified' | 'unchanged';
}

export interface ProjectDiffResult {
  project: ManagedProject;
  syncState: ProjectSyncState;
  entries: ProjectDiffEntry[];
}

export interface GitHubAccountState {
  token: string | null;
  user: GitHubUser | null;
  connecting: boolean;
  error: string;
}

export interface ProjectCatalogState {
  root: string;
  projects: ManagedProject[];
  loading: boolean;
  remoteLoading: boolean;
  refreshing: boolean;
  error: string;
  busyKeys: string[];
  checkingSyncKeys: string[];
  manifestGistId: string | null;
}

export interface ProjectCatalogSnapshot {
  github: GitHubAccountState;
  catalog: ProjectCatalogState;
}

export interface ManagedProjectServices {
  github: Readable<GitHubAccountState>;
  catalog: Readable<ProjectCatalogState>;
}

export function projectKey(project: Pick<ManagedProject, 'name' | 'type'>): string {
  return `${project.type}:${project.name}`;
}

export function syncStateFromHashes(
  project: Pick<ManagedProject, 'localPath' | 'remote' | 'syncedHash'>,
  currentLocalHash: string | null,
  currentRemoteHash: string | null,
): ProjectSyncState {
  if (!project.remote && project.localPath) return 'local-only';
  if (project.remote && !project.localPath) return 'remote-only';
  if (!project.remote && !project.localPath) return 'local-only';
  if (currentLocalHash == null || currentRemoteHash == null) return 'unknown';
  if (currentLocalHash === currentRemoteHash) return 'in-sync';
  if (project.syncedHash != null) {
    const localChanged = currentLocalHash !== project.syncedHash;
    const remoteChanged = currentRemoteHash !== project.syncedHash;
    if (localChanged && remoteChanged) return 'diverged';
    if (localChanged) return 'local-ahead';
    return 'remote-ahead';
  }
  return 'diverged';
}

export function syncState(project: ManagedProject): ProjectSyncState {
  return syncStateFromHashes(project, project.currentLocalHash, project.currentRemoteHash);
}

export function syncStateLabel(state: ProjectSyncState): string {
  switch (state) {
    case 'local-only':
      return 'Local only';
    case 'remote-only':
      return 'Cloud only';
    case 'in-sync':
      return 'Synced';
    case 'local-ahead':
      return 'Upload pending';
    case 'remote-ahead':
      return 'Download available';
    case 'diverged':
      return 'Sync conflict';
    case 'unknown':
      return 'Sync status pending';
  }
}

export function projectStatusPresentation(
  project: ManagedProject,
  options: { state?: ProjectSyncState; checkingSync?: boolean } = {},
): ProjectStatusPresentation {
  const state = options.state ?? syncState(project);
  if (!project.remote && project.localPath) {
    return { label: 'Local only', glyph: '', tone: 'local' };
  }
  if (project.remote && !project.localPath) {
    return { label: 'Cloud only', glyph: '', tone: 'cloud' };
  }
  if (!project.remote && !project.localPath) {
    return { label: 'Local only', glyph: '', tone: 'local' };
  }
  if (options.checkingSync) {
    return { label: 'Checking sync…', glyph: '', tone: 'checking' };
  }
  switch (state) {
    case 'in-sync':
      return { label: 'Synced', glyph: '✓', tone: 'synced' };
    case 'local-ahead':
      return { label: 'Upload pending', glyph: '↑', tone: 'pending' };
    case 'remote-ahead':
      return { label: 'Download available', glyph: '↓', tone: 'pending' };
    case 'diverged':
      return { label: 'Sync conflict', glyph: '⇅', tone: 'danger' };
    case 'local-only':
      return { label: 'Local only', glyph: '', tone: 'local' };
    case 'remote-only':
      return { label: 'Cloud only', glyph: '', tone: 'cloud' };
    case 'unknown':
      return { label: 'Sync status pending', glyph: '', tone: 'checking' };
  }
}

export function hashContent(content: string): string {
  let hash = 0x811c9dc5;
  for (let i = 0; i < content.length; i += 1) {
    hash ^= content.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193);
  }
  return (hash >>> 0).toString(16).padStart(8, '0');
}

export function hashFiles(files: Record<string, string>): string {
  const keys = Object.keys(files).sort();
  let combined = '';
  for (const key of keys) {
    combined += key;
    combined += '\u0000';
    combined += files[key];
    combined += '\u0000';
  }
  return hashContent(combined);
}
