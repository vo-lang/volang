import { writable } from 'svelte/store';

export interface ContextMenuTarget {
  x: number;
  y: number;
  path: string;
  name: string;
  isDir: boolean;
}

export type InlineInputMode = 'create-file' | 'create-dir' | 'rename';

export interface InlineInput {
  mode: InlineInputMode;
  dirPath: string;      // parent dir (for create) or parent of target (for rename)
  targetPath?: string;  // path being renamed (rename mode only)
  initialValue: string;
}

export type AppMode = 'manage' | 'develop' | 'run';

export interface ExplorerState {
  appMode: AppMode;
  // The VFS root for local file browsing — set once at initWorkspace, never mutated
  localRoot: string;
  // FileExplorer navigation
  explorerCwd: string;
  explorerHistoryBack: string[];
  explorerHistoryFwd: string[];
  explorerSelected: string | null;
  // Shared across panels
  contextMenu: ContextMenuTarget | null;
  inlineInput: InlineInput | null;
}

export const explorer = writable<ExplorerState>({
  appMode: 'develop',
  localRoot: '',
  explorerCwd: '',
  explorerHistoryBack: [],
  explorerHistoryFwd: [],
  explorerSelected: null,
  contextMenu: null,
  inlineInput: null,
});
