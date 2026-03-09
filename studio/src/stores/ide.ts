import { writable, derived, get } from 'svelte/store';
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

export type ConsoleLineKind = 'stdout' | 'stderr' | 'system' | 'success';

export interface ConsoleLine {
  text: string;
  kind: ConsoleLineKind;
  ts: number; // Date.now()
}

export type RunStatus = 'idle' | 'compiling' | 'running' | 'done' | 'error';

export interface IdeState {
  workspaceRoot: string;
  projectMode: ProjectMode;
  editTarget: EditTarget | null;
  dirCache: Record<string, FsEntry[]>;
  expandedDirs: Set<string>;
  activeFilePath: string;
  runEntryPath: string;
  code: string;
  dirty: boolean;
  // Console
  consoleLines: ConsoleLine[];
  runStatus: RunStatus;
  runDurationMs: number | null;
  // Execution
  isRunning: boolean;
  isGuiApp: boolean;
  guestRender: Uint8Array | null;
  // Runtime display
  outputExpanded: boolean;
  // Console UI preferences
  consoleShowTimestamps: boolean;
  consoleWordWrap: boolean;
}

export const ide = writable<IdeState>({
  workspaceRoot: '',
  projectMode: 'single',
  editTarget: null,
  dirCache: {},
  expandedDirs: new Set<string>(),
  activeFilePath: '',
  runEntryPath: '',
  code: '',
  dirty: false,
  consoleLines: [],
  runStatus: 'idle',
  runDurationMs: null,
  isRunning: false,
  isGuiApp: false,
  guestRender: null,
  outputExpanded: false,
  consoleShowTimestamps: true,
  consoleWordWrap: true,
});

// Helper to append console lines from the store
export function consolePush(kind: ConsoleLineKind, text: string) {
  const ts = Date.now();
  ide.update(s => ({
    ...s,
    consoleLines: [...s.consoleLines, { text, kind, ts }],
  }));
}

export function consolePushLines(kind: ConsoleLineKind, text: string) {
  if (!text) return;
  const ts = Date.now();
  const lines = text.split('\n');
  // Remove trailing empty line from split
  if (lines.length > 0 && lines[lines.length - 1] === '') lines.pop();
  const newLines: ConsoleLine[] = lines.map(line => ({ text: line, kind, ts }));
  ide.update(s => ({
    ...s,
    consoleLines: [...s.consoleLines, ...newLines],
  }));
}

export function consoleClear() {
  ide.update(s => ({ ...s, consoleLines: [] }));
}
