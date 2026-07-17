type StudioPerfHostLogRecord = {
  code?: unknown;
  text?: unknown;
};

const VOPLAY_PERF_REPORT_CODE = 'voplay_perf_report';
const VOPLAY_PERF_REPORT_ROUTE = '/__voplay_perf_report';
const VOPLAY_PERF_REPORT_QUEUE_LIMIT = 32;
const VOPLAY_PERF_REPORT_DEDUPE_MS = 1000;

type PendingVoplayPerfReport = {
  text: string;
  studioSessionId: number | null;
  studioPerfEpoch: number;
};

const pendingVoplayPerfReports: PendingVoplayPerfReport[] = [];
let voplayPerfReportFlushTimer: ReturnType<typeof window.setTimeout> | null = null;
let lastVoplayPerfReportText = '';
let lastVoplayPerfReportSessionId: number | null = null;
let lastVoplayPerfReportEpoch = 0;
let lastVoplayPerfReportMs = 0;
let voplayPerfEvidenceEpoch = 0;
let activeVoplayPerfSessionId: number | null = null;

export function setActiveVoplayPerfSessionId(studioSessionId: number | null): void {
  if (studioSessionId !== null
    && (!Number.isSafeInteger(studioSessionId) || studioSessionId < 1)) {
    throw new Error('active Voplay perf session ID must be a positive safe integer or null');
  }
  activeVoplayPerfSessionId = studioSessionId;
}

export function rotateVoplayPerfEvidenceEpoch(): number {
  if (!Number.isSafeInteger(voplayPerfEvidenceEpoch + 1)) {
    throw new Error('Voplay perf evidence epoch overflow');
  }
  voplayPerfEvidenceEpoch += 1;
  return voplayPerfEvidenceEpoch;
}

export function handleVoplayPerfHostLog(
  record: StudioPerfHostLogRecord,
  studioSessionId: number | null = activeVoplayPerfSessionId,
): boolean {
  if (record.code !== VOPLAY_PERF_REPORT_CODE) {
    return false;
  }
  const text = typeof record.text === 'string' ? record.text.trim() : '';
  if (!text) {
    return true;
  }
  const boundedSessionId = Number.isSafeInteger(studioSessionId) && Number(studioSessionId) > 0
    ? Number(studioSessionId)
    : null;
  const studioPerfEpoch = voplayPerfEvidenceEpoch;
  if (isDuplicateVoplayPerfPayload(text, boundedSessionId, studioPerfEpoch)) {
    return true;
  }
  queueVoplayPerfPayload(text, boundedSessionId, studioPerfEpoch);
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

function parseVoplayPerfPayload({ text, studioSessionId, studioPerfEpoch }: PendingVoplayPerfReport): unknown {
  try {
    const payload: unknown = JSON.parse(text);
    if (payload != null && typeof payload === 'object' && !Array.isArray(payload)) {
      return { ...payload, studioSessionId, studioPerfEpoch };
    }
    return {
      schemaVersion: 1,
      source: 'voplay-guest',
      kind: 'raw',
      value: payload,
      studioSessionId,
      studioPerfEpoch,
    };
  } catch {
    return {
      schemaVersion: 1,
      source: 'voplay-guest',
      kind: 'raw',
      message: text,
      studioSessionId,
      studioPerfEpoch,
    };
  }
}

function isDuplicateVoplayPerfPayload(
  text: string,
  studioSessionId: number | null,
  studioPerfEpoch: number,
): boolean {
  const nowMs = Date.now();
  if (
    text === lastVoplayPerfReportText
    && studioSessionId === lastVoplayPerfReportSessionId
    && studioPerfEpoch === lastVoplayPerfReportEpoch
    && nowMs - lastVoplayPerfReportMs <= VOPLAY_PERF_REPORT_DEDUPE_MS
  ) {
    lastVoplayPerfReportMs = nowMs;
    return true;
  }
  lastVoplayPerfReportText = text;
  lastVoplayPerfReportSessionId = studioSessionId;
  lastVoplayPerfReportEpoch = studioPerfEpoch;
  lastVoplayPerfReportMs = nowMs;
  return false;
}

function queueVoplayPerfPayload(
  text: string,
  studioSessionId: number | null,
  studioPerfEpoch: number,
): void {
  pendingVoplayPerfReports.push({ text, studioSessionId, studioPerfEpoch });
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
  if (pending.length === 0) {
    return;
  }
  postVoplayPerfPayload(pending.map(parseVoplayPerfPayload));
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
      cache: 'no-store',
    }).then((response) => response.arrayBuffer()).catch(() => {});
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
