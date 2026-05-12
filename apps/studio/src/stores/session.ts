import { writable } from 'svelte/store';
import type { ProjectMode, SessionSource, ShareInfo, StudioMode } from '../lib/types';

export interface SessionState {
  root: string;
  projectName: string;
  mode: StudioMode;
  entryPath: string | null;
  projectMode: ProjectMode | null;
  source: SessionSource | null;
  share: ShareInfo | null;
}

export const session = writable<SessionState>({
  root: '',
  projectName: '',
  mode: 'dev',
  entryPath: null,
  projectMode: null,
  source: null,
  share: null,
});

export function sessionOpen(
  root: string,
  mode: StudioMode,
  entryPath: string | null,
  projectMode: ProjectMode,
  source: SessionSource | null,
  share: ShareInfo | null,
): void {
  const projectName = displayName(root, entryPath, projectMode);
  session.set({
    root,
    projectName,
    mode,
    entryPath,
    projectMode,
    source,
    share,
  });
}

function displayName(root: string, entryPath: string | null, projectMode: ProjectMode): string {
  if (projectMode === 'single-file' && entryPath) {
    return basename(entryPath);
  }
  return basename(root);
}

function basename(path: string): string {
  const parts = path.split('/');
  return parts[parts.length - 1] || parts[parts.length - 2] || path;
}
