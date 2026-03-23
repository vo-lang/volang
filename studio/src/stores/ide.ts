import { writable } from 'svelte/store';

// AppMode is UI-level navigation state — owned by App shell
export type AppMode = 'manage' | 'develop' | 'docs' | 'terminal' | 'runner';

// RunStatus is also kept here for backward compat with Console.svelte; runtime store owns canonical status
export type RunStatus = 'idle' | 'preparing' | 'compiling' | 'running' | 'done' | 'error';

// UI-level state: navigation mode + output overlay toggle
export interface IdeState {
  appMode: AppMode;
  outputExpanded: boolean;
  previewCollapsed: boolean;
}

export const ide = writable<IdeState>({
  appMode: 'develop',
  outputExpanded: false,
  previewCollapsed: false,
});

// Re-export domain stores + helpers for convenience
export type { ConsoleLineKind, ConsoleLine } from './console';
export { consolePush, consoleClear } from './console';
export type { EditorState } from './editor';
export type { SessionState } from './session';
export type { StudioMode } from '../lib/types';
