import { consolePush } from '../../stores/console';

export type UiConsoleLine = {
  kind: 'stdout' | 'stderr' | 'system' | 'success';
  text: string;
};

export function formatDurationMs(durationMs: number): string {
  return durationMs < 1000 ? `${Math.round(durationMs)}ms` : `${(durationMs / 1000).toFixed(durationMs < 10_000 ? 2 : 1)}s`;
}

export function pushUiConsole(line: UiConsoleLine | null): void {
  if (!line || !line.text) {
    return;
  }
  consolePush(line.kind, line.text);
}

export function formatCommonGuiLogLine(
  source: string,
  message: string,
  displayPath: (path: string) => string,
): UiConsoleLine | null {
  const trimmed = message.trim();
  if (!trimmed) {
    return null;
  }
  if (source === 'guest') {
    return { kind: 'stdout', text: trimmed };
  }
  const cacheHit = trimmed.match(/^compile cache hit (.+)$/);
  if (cacheHit) {
    return { kind: 'success', text: `Using cached GUI build for ${displayPath(cacheHit[1])}` };
  }
  const cacheStore = trimmed.match(/^compile cache store (.+)$/);
  if (cacheStore) {
    return { kind: 'system', text: `Updated GUI build cache for ${displayPath(cacheStore[1])}` };
  }
  const compile = trimmed.match(/^runGui compile (.+) (\d+)ms$/);
  if (compile) {
    return { kind: 'system', text: `Compiled GUI ${displayPath(compile[1])} in ${formatDurationMs(Number(compile[2]))}` };
  }
  const loadVm = trimmed.match(/^runGui load vm (.+) (\d+)ms$/);
  if (loadVm) {
    return { kind: 'system', text: `Loaded GUI runtime for ${displayPath(loadVm[1])} in ${formatDurationMs(Number(loadVm[2]))}` };
  }
  const startApp = trimmed.match(/^runGui start app (.+) (\d+)ms$/);
  if (startApp) {
    return { kind: 'system', text: `Started GUI app ${displayPath(startApp[1])} in ${formatDurationMs(Number(startApp[2]))}` };
  }
  return null;
}
