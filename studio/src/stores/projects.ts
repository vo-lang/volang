import { writable, get } from 'svelte/store';
import { github, loadManifest, saveManifest, fetchGistMetadata, fetchRepoMetadata } from './github';
import type { Manifest, ManifestProject, RemoteSource, RemoteMetadata } from './github';
import { bridge } from '../lib/bridge';
import type { FsEntry } from '../lib/bridge';

// ── Types ─────────────────────────────────────────────────────────────────────

export type SyncState = 'local-only' | 'remote-only' | 'in-sync' | 'local-ahead' | 'remote-ahead' | 'diverged';

export interface ProjectEntry {
  name: string;                    // display name (filename w/o .vo, or dir name)
  type: 'single' | 'multi';
  localPath: string | null;        // VFS path; null = remote-only
  remote: RemoteSource | null;     // null = local-only
  pushedAt: string | null;         // ISO timestamp of last push/pull sync
  remoteUpdatedAt: string | null;  // ISO timestamp from GitHub API
  syncedHash: string | null;       // content hash when local === remote (at last push/pull)
  currentLocalHash: string | null; // live hash of current local content
  currentRemoteHash: string | null; // live hash of current remote content
}

export function syncState(p: ProjectEntry): SyncState {
  if (!p.remote && p.localPath) return 'local-only';
  if (p.remote && !p.localPath) return 'remote-only';
  if (!p.remote && !p.localPath) return 'local-only';

  // Both current hashes available — primary comparison path
  if (p.currentLocalHash != null && p.currentRemoteHash != null) {
    // Identical content → always in-sync regardless of baseline
    if (p.currentLocalHash === p.currentRemoteHash) return 'in-sync';
    // Content differs — use syncedHash baseline to determine direction
    if (p.syncedHash != null) {
      const localChanged = p.currentLocalHash !== p.syncedHash;
      const remoteChanged = p.currentRemoteHash !== p.syncedHash;
      if (localChanged && remoteChanged) return 'diverged';
      if (localChanged) return 'local-ahead';
      return 'remote-ahead';
    }
    // No baseline (legacy manifest) — content differs but can't tell direction
    return 'diverged';
  }

  // Remote hash unavailable (network failure) — fall back to local vs baseline
  if (p.currentLocalHash != null && p.syncedHash != null) {
    return p.currentLocalHash !== p.syncedHash ? 'local-ahead' : 'in-sync';
  }

  return 'in-sync';
}

export function syncStateLabel(state: SyncState): string {
  switch (state) {
    case 'local-only': return 'Local only';
    case 'remote-only': return 'Remote only';
    case 'in-sync': return 'In sync';
    case 'local-ahead': return 'Local ahead';
    case 'remote-ahead': return 'Remote ahead';
    case 'diverged': return 'Diverged';
  }
}

// Simple hash for content comparison (FNV-1a 32-bit)
export function hashContent(content: string): string {
  let hash = 0x811c9dc5;
  for (let i = 0; i < content.length; i++) {
    hash ^= content.charCodeAt(i);
    hash = Math.imul(hash, 0x01000193);
  }
  return (hash >>> 0).toString(16).padStart(8, '0');
}

// Hash multiple files (sorted by path for determinism)
export function hashFiles(files: Record<string, string>): string {
  const keys = Object.keys(files).sort();
  let combined = '';
  for (const k of keys) {
    combined += k + '\0' + files[k] + '\0';
  }
  return hashContent(combined);
}

export interface ProjectsState {
  manifestGistId: string | null;
  manifestLoaded: boolean;
  projects: ProjectEntry[];
  isLoading: boolean;
  error: string;
}

export const projects = writable<ProjectsState>({
  manifestGistId: null,
  manifestLoaded: false,
  projects: [],
  isLoading: false,
  error: '',
});

// ── Local discovery ───────────────────────────────────────────────────────────

const SKIP_DIR_NAMES = new Set(['.vo-cache', '.git', 'node_modules']);

async function collectLocalFiles(dirPath: string): Promise<Record<string, string>> {
  const b = bridge();
  const result: Record<string, string> = {};
  async function walk(dir: string, prefix: string) {
    let entries: FsEntry[];
    try { entries = await b.fsListDir(dir); } catch { return; }
    for (const e of entries) {
      if (e.name.startsWith('.') || SKIP_DIR_NAMES.has(e.name)) continue;
      const relPath = prefix ? prefix + '/' + e.name : e.name;
      if (e.isDir) {
        await walk(e.path, relPath);
      } else {
        try { result[relPath] = await b.fsReadFile(e.path); } catch { /* skip */ }
      }
    }
  }
  await walk(dirPath, '');
  return result;
}

export async function discoverLocalProjects(root: string): Promise<ProjectEntry[]> {
  const b = bridge();
  let entries: FsEntry[];
  try {
    entries = await b.fsListDir(root);
  } catch {
    return [];
  }

  const result: ProjectEntry[] = [];
  for (const e of entries) {
    if (e.isDir) {
      // Check if directory contains vo.mod → multi-file project
      let children: FsEntry[];
      try {
        children = await b.fsListDir(e.path);
      } catch {
        continue;
      }
      if (children.some(c => !c.isDir && c.name === 'vo.mod')) {
        const files = await collectLocalFiles(e.path);
        result.push({
          name: e.name,
          type: 'multi',
          localPath: e.path,
          remote: null,
          pushedAt: null,
          remoteUpdatedAt: null,
          syncedHash: null,
          currentLocalHash: hashFiles(files),
          currentRemoteHash: null,
        });
      }
    } else if (e.name.endsWith('.vo')) {
      // Single-file project
      const name = e.name.replace(/\.vo$/, '');
      let content = '';
      try { content = await b.fsReadFile(e.path); } catch { /* empty */ }
      result.push({
        name,
        type: 'single',
        localPath: e.path,
        remote: null,
        pushedAt: null,
        remoteUpdatedAt: null,
        syncedHash: null,
        currentLocalHash: hashContent(content),
        currentRemoteHash: null,
      });
    }
  }
  return result;
}

// ── Merge logic ───────────────────────────────────────────────────────────────

export function mergeProjects(
  local: ProjectEntry[],
  manifest: ManifestProject[],
  remoteMetaMap: Map<string, RemoteMetadata>,
): ProjectEntry[] {
  const result: ProjectEntry[] = [];
  const matched = new Set<string>();

  for (const lp of local) {
    const mp = manifest.find(m => m.name === lp.name && m.type === lp.type);
    if (mp) {
      matched.add(mp.name + ':' + mp.type);
      const key = mp.name + ':' + mp.type;
      const remoteMeta = remoteMetaMap.get(key);
      const remoteHash = remoteMeta ? hashRemoteFiles(remoteMeta.files) : null;
      result.push({
        name: lp.name,
        type: lp.type,
        localPath: lp.localPath,
        remote: mp.remote,
        pushedAt: mp.pushedAt,
        remoteUpdatedAt: remoteMeta?.updatedAt ?? null,
        syncedHash: mp.contentHash ?? null,
        currentLocalHash: lp.currentLocalHash,
        currentRemoteHash: remoteHash,
      });
    } else {
      result.push(lp);
    }
  }

  // Remote-only projects (in manifest but not local)
  for (const mp of manifest) {
    const key = mp.name + ':' + mp.type;
    if (!matched.has(key)) {
      const remoteMeta = remoteMetaMap.get(key);
      const remoteHash = remoteMeta ? hashRemoteFiles(remoteMeta.files) : null;
      result.push({
        name: mp.name,
        type: mp.type,
        localPath: null,
        remote: mp.remote,
        pushedAt: mp.pushedAt,
        remoteUpdatedAt: remoteMeta?.updatedAt ?? null,
        syncedHash: mp.contentHash ?? null,
        currentLocalHash: null,
        currentRemoteHash: remoteHash,
      });
    }
  }

  return result;
}

function hashRemoteFiles(files: Record<string, string>): string {
  // For single-file gists, hash the single file content directly
  const keys = Object.keys(files);
  if (keys.length === 1) return hashContent(files[keys[0]]);
  return hashFiles(files);
}

// ── Load all projects ─────────────────────────────────────────────────────────

export async function loadProjects(root: string): Promise<void> {
  projects.update(s => ({ ...s, isLoading: true, error: '' }));

  try {
    const localProjects = await discoverLocalProjects(root);

    const { token } = get(github);
    if (token) {
      const { gistId, manifest } = await loadManifest(token);

      // Fetch remote metadata for all manifest projects (in parallel)
      const remoteMetaMap = new Map<string, RemoteMetadata>();
      const fetches = manifest.projects.map(async (mp) => {
        const key = mp.name + ':' + mp.type;
        try {
          let meta: RemoteMetadata;
          if (mp.remote.kind === 'gist' && mp.remote.gistId) {
            meta = await fetchGistMetadata(token, mp.remote.gistId);
          } else if (mp.remote.kind === 'repo' && mp.remote.owner && mp.remote.repo) {
            meta = await fetchRepoMetadata(token, mp.remote.owner, mp.remote.repo);
          } else {
            return;
          }
          remoteMetaMap.set(key, meta);
        } catch {
          // Network error for this project — skip remote metadata
        }
      });
      await Promise.all(fetches);

      const merged = mergeProjects(localProjects, manifest.projects, remoteMetaMap);
      projects.update(s => ({
        ...s,
        manifestGistId: gistId,
        manifestLoaded: true,
        projects: merged,
        isLoading: false,
      }));
    } else {
      // No GitHub connection — local-only projects
      projects.update(s => ({
        ...s,
        manifestGistId: null,
        manifestLoaded: false,
        projects: localProjects,
        isLoading: false,
      }));
    }
  } catch (e: any) {
    projects.update(s => ({ ...s, isLoading: false, error: String(e.message ?? e) }));
  }
}

// ── Manifest sync helpers ─────────────────────────────────────────────────────

export function buildManifest(entries: ProjectEntry[]): Manifest {
  const mp: ManifestProject[] = [];
  for (const p of entries) {
    if (p.remote) {
      mp.push({
        name: p.name,
        type: p.type,
        remote: p.remote,
        pushedAt: p.pushedAt ?? new Date().toISOString(),
        contentHash: p.currentLocalHash ?? p.syncedHash ?? undefined,
      });
    }
  }
  return { v: 1, projects: mp };
}

// ── Helpers for fetching remote content (used by diff modal) ─────────────────

export async function fetchRemoteContent(p: ProjectEntry): Promise<Record<string, string>> {
  const { token } = get(github);
  if (!token || !p.remote) return {};
  if (p.remote.kind === 'gist' && p.remote.gistId) {
    const meta = await fetchGistMetadata(token, p.remote.gistId);
    return meta.files;
  } else if (p.remote.kind === 'repo' && p.remote.owner && p.remote.repo) {
    const meta = await fetchRepoMetadata(token, p.remote.owner, p.remote.repo);
    return meta.files;
  }
  return {};
}

export async function fetchLocalContent(p: ProjectEntry): Promise<Record<string, string>> {
  if (!p.localPath) return {};
  const b = bridge();
  if (p.type === 'single') {
    const content = await b.fsReadFile(p.localPath);
    return { [p.name + '.vo']: content };
  } else {
    return collectLocalFiles(p.localPath);
  }
}

export async function persistManifest(): Promise<void> {
  const { token } = get(github);
  const s = get(projects);
  if (!token || !s.manifestGistId) return;
  const manifest = buildManifest(s.projects);
  await saveManifest(token, s.manifestGistId, manifest);
}
