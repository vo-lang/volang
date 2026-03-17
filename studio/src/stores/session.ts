import { writable } from 'svelte/store';
import type { ProjectMode, StudioMode } from '../lib/types';

export interface SessionState {
  root: string;
  projectName: string;
  mode: StudioMode;
  entryPath: string | null;
  projectMode: ProjectMode | null;
}

export const session = writable<SessionState>({
  root: '',
  projectName: '',
  mode: 'dev',
  entryPath: null,
  projectMode: null,
});

export function sessionOpen(root: string, mode: StudioMode, entryPath: string | null, projectMode: ProjectMode): void {
  const projectName = displayName(root, entryPath, projectMode);
  session.set({
    root,
    projectName,
    mode,
    entryPath,
    projectMode,
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
