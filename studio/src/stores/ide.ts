import { writable } from 'svelte/store';
import type { FsEntry } from '../lib/bridge';

export type ProjectMode = 'single' | 'multi';

export interface EditTarget {
  mode: ProjectMode;
  workspaceRoot: string;
}

export function editTargetLabel(t: EditTarget): string {
  const name = t.workspaceRoot.split('/').pop() || 'workspace';
  return `${t.mode === 'multi' ? 'Project' : 'File'} · ${name}`;
}

export interface IdeState {
  workspaceRoot: string;
  projectMode: ProjectMode;
  editTarget: EditTarget | null;
  dirCache: Record<string, FsEntry[]>;
  expandedDirs: string[];
  activeFilePath: string;
  code: string;
  dirty: boolean;
  output: string;
  isRunning: boolean;
  isGuiApp: boolean;
  guestRender: Uint8Array | null;
  compileError: string;
}

export const ide = writable<IdeState>({
  workspaceRoot: '',
  projectMode: 'single',
  editTarget: null,
  dirCache: {},
  expandedDirs: [],
  activeFilePath: '',
  code: '',
  dirty: false,
  output: '',
  isRunning: false,
  isGuiApp: false,
  guestRender: null,
  compileError: '',
});
