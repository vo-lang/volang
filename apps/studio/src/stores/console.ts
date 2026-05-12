import { writable } from 'svelte/store';

export type ConsoleLineKind = 'stdout' | 'stderr' | 'system' | 'success';

export interface ConsoleLine {
  text: string;
  kind: ConsoleLineKind;
  ts: number;
}

export interface ConsoleState {
  lines: ConsoleLine[];
  showTimestamps: boolean;
  wordWrap: boolean;
}

export const console_ = writable<ConsoleState>({
  lines: [],
  showTimestamps: true,
  wordWrap: true,
});

export function consolePush(kind: ConsoleLineKind, text: string): void {
  if (!text) return;
  const ts = Date.now();
  const lines = text.split('\n');
  if (lines.length > 0 && lines[lines.length - 1] === '') lines.pop();
  console_.update((s) => ({
    ...s,
    lines: [...s.lines, ...lines.map((l) => ({ text: l, kind, ts }))],
  }));
}

export function consoleClear(): void {
  console_.update((s) => ({ ...s, lines: [] }));
}

export function consoleToggleTimestamps(): void {
  console_.update((s) => ({ ...s, showTimestamps: !s.showTimestamps }));
}

export function consoleToggleWordWrap(): void {
  console_.update((s) => ({ ...s, wordWrap: !s.wordWrap }));
}
