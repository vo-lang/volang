import { writable, get } from 'svelte/store';
import { github, loadManifest, saveManifest } from './github';
import type { Manifest, ManifestProject, RemoteSource } from './github';
import { bridge } from '../lib/bridge';
import type { FsEntry } from '../lib/bridge';

// ── Types ─────────────────────────────────────────────────────────────────────

export type SyncState = 'local-only' | 'remote-only' | 'synced';

export interface ProjectEntry {
  name: string;                    // display name (filename w/o .vo, or dir name)
  type: 'single' | 'multi';
  localPath: string | null;        // VFS path; null = remote-only
  remote: RemoteSource | null;     // null = local-only
  pushedAt: string | null;         // ISO timestamp of last push
}

export function syncState(p: ProjectEntry): SyncState {
  if (p.localPath && p.remote) return 'synced';
  if (p.localPath) return 'local-only';
  return 'remote-only';
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
        result.push({
          name: e.name,
          type: 'multi',
          localPath: e.path,
          remote: null,
          pushedAt: null,
        });
      }
    } else if (e.name.endsWith('.vo')) {
      // Single-file project
      const name = e.name.replace(/\.vo$/, '');
      result.push({
        name,
        type: 'single',
        localPath: e.path,
        remote: null,
        pushedAt: null,
      });
    }
  }
  return result;
}

// ── Merge logic ───────────────────────────────────────────────────────────────

export function mergeProjects(
  local: ProjectEntry[],
  manifest: ManifestProject[],
): ProjectEntry[] {
  const result: ProjectEntry[] = [];
  const matched = new Set<string>();

  for (const lp of local) {
    const mp = manifest.find(m => m.name === lp.name && m.type === lp.type);
    if (mp) {
      matched.add(mp.name + ':' + mp.type);
      result.push({
        name: lp.name,
        type: lp.type,
        localPath: lp.localPath,
        remote: mp.remote,
        pushedAt: mp.pushedAt,
      });
    } else {
      result.push(lp);
    }
  }

  // Remote-only projects (in manifest but not local)
  for (const mp of manifest) {
    const key = mp.name + ':' + mp.type;
    if (!matched.has(key)) {
      result.push({
        name: mp.name,
        type: mp.type,
        localPath: null,
        remote: mp.remote,
        pushedAt: mp.pushedAt,
      });
    }
  }

  return result;
}

// ── Load all projects ─────────────────────────────────────────────────────────

export async function loadProjects(root: string): Promise<void> {
  projects.update(s => ({ ...s, isLoading: true, error: '' }));

  try {
    const localProjects = await discoverLocalProjects(root);

    const { token } = get(github);
    if (token) {
      const { gistId, manifest } = await loadManifest(token);
      const merged = mergeProjects(localProjects, manifest.projects);
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
      });
    }
  }
  return { v: 1, projects: mp };
}

export async function persistManifest(): Promise<void> {
  const { token } = get(github);
  const s = get(projects);
  if (!token || !s.manifestGistId) return;
  const manifest = buildManifest(s.projects);
  await saveManifest(token, s.manifestGistId, manifest);
}
