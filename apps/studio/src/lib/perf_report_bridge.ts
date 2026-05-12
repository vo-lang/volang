type StudioPerfHostLogRecord = {
  code?: unknown;
  text?: unknown;
};

const VOPLAY_PERF_REPORT_CODE = 'voplay_perf_report';
const VOPLAY_PERF_REPORT_ROUTE = '/__voplay_perf_report';
const VOPLAY_PERF_REPORT_QUEUE_LIMIT = 32;
const VOPLAY_PERF_REPORT_DEDUPE_MS = 1000;

const pendingVoplayPerfReports: string[] = [];
let voplayPerfReportFlushTimer: ReturnType<typeof window.setTimeout> | null = null;
let lastVoplayPerfReportText = '';
let lastVoplayPerfReportMs = 0;

export function handleVoplayPerfHostLog(record: StudioPerfHostLogRecord): boolean {
  if (record.code !== VOPLAY_PERF_REPORT_CODE) {
    return false;
  }
  const text = typeof record.text === 'string' ? record.text.trim() : '';
  if (!text) {
    return true;
  }
  if (isDuplicateVoplayPerfPayload(text)) {
    return true;
  }
  queueVoplayPerfPayload(text);
  return true;
}

export function shouldEmitVoplayPerfConsoleDiagnostics(): boolean {
  if (typeof window === 'undefined') {
    return false;
  }
  try {
    const params = new URLSearchParams(window.location.search);
    const perfMode = (params.get('voplayPerf') ?? params.get('perf') ?? '').toLowerCase();
    return params.has('voplayPerfConsole')
      || params.has('voplayPerfDiag')
      || perfMode === 'console'
      || perfMode === 'diag'
      || perfMode === 'debug'
      || perfMode === 'verbose'
      || window.localStorage.getItem('voplay.perfConsole') === '1';
  } catch {
    return false;
  }
}

function parseVoplayPerfPayload(text: string): unknown {
  try {
    return JSON.parse(text);
  } catch {
    return {
      schemaVersion: 1,
      source: 'voplay-guest',
      kind: 'raw',
      message: text,
    };
  }
}

function isDuplicateVoplayPerfPayload(text: string): boolean {
  const nowMs = Date.now();
  if (
    text === lastVoplayPerfReportText
    && nowMs - lastVoplayPerfReportMs <= VOPLAY_PERF_REPORT_DEDUPE_MS
  ) {
    lastVoplayPerfReportMs = nowMs;
    return true;
  }
  lastVoplayPerfReportText = text;
  lastVoplayPerfReportMs = nowMs;
  return false;
}

function queueVoplayPerfPayload(text: string): void {
  pendingVoplayPerfReports.push(text);
  if (pendingVoplayPerfReports.length > VOPLAY_PERF_REPORT_QUEUE_LIMIT) {
    pendingVoplayPerfReports.splice(
      0,
      pendingVoplayPerfReports.length - VOPLAY_PERF_REPORT_QUEUE_LIMIT,
    );
  }
  if (voplayPerfReportFlushTimer !== null) {
    return;
  }
  try {
    voplayPerfReportFlushTimer = window.setTimeout(flushVoplayPerfPayloads, 0);
  } catch {
    flushVoplayPerfPayloads();
  }
}

function flushVoplayPerfPayloads(): void {
  voplayPerfReportFlushTimer = null;
  const pending = pendingVoplayPerfReports.splice(0);
  for (const text of pending) {
    postVoplayPerfPayload(parseVoplayPerfPayload(text));
  }
}

function postVoplayPerfPayload(payload: unknown): void {
  if (typeof window === 'undefined' || !isLocalHost(window.location.hostname)) {
    return;
  }
  try {
    void fetch(VOPLAY_PERF_REPORT_ROUTE, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(payload),
      keepalive: true,
    }).catch(() => {});
  } catch {
    // Perf reporting must not affect the running game.
  }
}

function isLocalHost(hostname: string): boolean {
  return hostname === 'localhost'
    || hostname === '127.0.0.1'
    || hostname === '::1'
    || hostname === '[::1]';
}
