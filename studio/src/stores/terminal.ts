import { writable, get } from 'svelte/store';

export type TermLineKind =
  | 'input'       // $ command line the user typed
  | 'output'      // normal stdout
  | 'error'       // stderr / error message
  | 'stream-out'  // live stdout from streaming op
  | 'stream-err'  // live stderr from streaming op
  | 'system';     // status messages

export interface TermLine {
  id:   number;
  kind: TermLineKind;
  text: string;
  ts:   number;
}

export interface TerminalState {
  lines:   TermLine[];
  cwd:     string;
  busy:    boolean;
  history: string[];    // command history (oldest first)
}

function makeState(): TerminalState {
  return {
    lines:   [],
    cwd:     '/',
    busy:    false,
    history: [],
  };
}

export const terminal = writable<TerminalState>(makeState());

// ── Helpers ──────────────────────────────────────────────────────────────────

let _lineId = 0;

export function termPush(kind: TermLineKind, text: string): void {
  const ts = Date.now();
  const lines = text.split('\n');
  if (lines.length > 0 && lines[lines.length - 1] === '') lines.pop();
  terminal.update(s => ({
    ...s,
    lines: [
      ...s.lines,
      ...lines.map(l => ({ id: ++_lineId, kind, text: l, ts })),
    ],
  }));
}

export function termClear(): void {
  terminal.update(s => ({ ...s, lines: [] }));
}

export function termSetCwd(cwd: string): void {
  terminal.update(s => ({ ...s, cwd }));
}

export function termSetBusy(busy: boolean): void {
  terminal.update(s => ({ ...s, busy }));
}

export function termPushHistory(cmd: string): void {
  if (!cmd.trim()) return;
  terminal.update(s => {
    const h = s.history.filter(c => c !== cmd);
    return { ...s, history: [...h, cmd] };
  });
}

export function termInit(workspaceRoot: string): void {
  terminal.update(s => ({
    ...makeState(),
    cwd: workspaceRoot,
  }));
}
