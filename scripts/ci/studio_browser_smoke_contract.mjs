const SHA256_PATTERN = /^[0-9a-f]{64}$/;

export const STUDIO_BROWSER_SMOKE_SCHEMA_VERSION = 1;
export const STUDIO_BROWSER_SMOKE_KIND = 'studio.browser-smoke';

function finiteNonNegative(value) {
  return Number.isFinite(value) && value >= 0;
}

function safePositiveInteger(value) {
  return Number.isSafeInteger(value) && value > 0;
}

function canonicalTimestamp(value) {
  if (typeof value !== 'string') {
    return null;
  }
  const parsed = Date.parse(value);
  return Number.isFinite(parsed) && new Date(parsed).toISOString() === value ? parsed : null;
}

function canonicalEvidencePath(value) {
  return typeof value === 'string'
    && value.length > 0
    && !value.startsWith('/')
    && !value.includes('\\')
    && value.split('/').every((segment) => segment.length > 0 && segment !== '.' && segment !== '..');
}

export function isBlockKartPerfFrameRecord(record, notBefore = null, studioSessionId = null) {
  const payload = record?.payload;
  const receivedAtMs = canonicalTimestamp(record?.receivedAt);
  const hasNotBefore = notBefore != null;
  const notBeforeMs = hasNotBefore ? canonicalTimestamp(notBefore) : null;
  return payload?.source === 'blockkart'
    && payload?.kind === 'perf-summary'
    && safePositiveInteger(payload.frame)
    && safePositiveInteger(payload.studioSessionId)
    && (studioSessionId == null || payload.studioSessionId === studioSessionId)
    && Number.isSafeInteger(payload.studioPerfEpoch)
    && payload.studioPerfEpoch >= 0
    && safePositiveInteger(payload?.window?.frames)
    && finiteNonNegative(payload?.current?.frameMs)
    && finiteNonNegative(payload?.renderer?.submitFrameMs)
    && safePositiveInteger(payload?.screen?.pixelWidth)
    && safePositiveInteger(payload?.screen?.pixelHeight)
    && receivedAtMs !== null
    && (!hasNotBefore || (notBeforeMs !== null && receivedAtMs >= notBeforeMs));
}

export function selectLatestBlockKartPerfFrame(
  records,
  notBefore = null,
  studioSessionId = null,
  studioPerfEpoch = null,
  minimumFrameExclusive = null,
) {
  const valid = (Array.isArray(records) ? records : [])
    .filter((record) => isBlockKartPerfFrameRecord(record, notBefore, studioSessionId))
    .filter((record) => studioPerfEpoch == null
      || record.payload.studioPerfEpoch === studioPerfEpoch)
    .filter((record) => minimumFrameExclusive == null
      || record.payload.frame > minimumFrameExclusive);
  return valid.length > 0 ? valid[valid.length - 1] : null;
}

export function webGpuRequirementFailure(required, probe) {
  if (!required || probe?.adapter === true) {
    return null;
  }
  return `a WebGPU adapter is required: ${probe?.reason ?? 'adapter probe did not succeed'}`;
}

function validateScreenshot(label, artifact, errors) {
  if (!artifact || typeof artifact !== 'object') {
    errors.push(`${label} screenshot evidence is missing`);
    return;
  }
  if (!canonicalEvidencePath(artifact.path)) {
    errors.push(`${label} screenshot path is not a canonical repository-relative path`);
  }
  if (!Number.isSafeInteger(artifact.bytes) || artifact.bytes <= 8) {
    errors.push(`${label} screenshot byte length is invalid`);
  }
  if (!SHA256_PATTERN.test(artifact.sha256 ?? '')) {
    errors.push(`${label} screenshot sha256 is invalid`);
  }
  if (artifact.mediaType !== 'image/png') {
    errors.push(`${label} screenshot media type must be image/png`);
  }
}

export function validateStudioBrowserSmokeEvidence(evidence) {
  const errors = [];
  if (evidence?.schemaVersion !== STUDIO_BROWSER_SMOKE_SCHEMA_VERSION) {
    errors.push(`schemaVersion must be ${STUDIO_BROWSER_SMOKE_SCHEMA_VERSION}`);
  }
  if (evidence?.kind !== STUDIO_BROWSER_SMOKE_KIND) {
    errors.push(`kind must be ${STUDIO_BROWSER_SMOKE_KIND}`);
  }
  if (evidence?.status !== 'ok') {
    errors.push('formal evidence status must be ok');
  }
  const startedAtMs = canonicalTimestamp(evidence?.startedAt);
  const quickplayStartedAtMs = canonicalTimestamp(evidence?.quickplayStartedAt);
  const visualCaptureCompletedAtMs = canonicalTimestamp(evidence?.visualCaptureCompletedAt);
  const finishedAtMs = canonicalTimestamp(evidence?.finishedAt);
  if (startedAtMs === null
    || quickplayStartedAtMs === null
    || finishedAtMs === null
    || startedAtMs > quickplayStartedAtMs
    || quickplayStartedAtMs > finishedAtMs) {
    errors.push('evidence timestamps must be canonical and monotonic');
  }
  if (visualCaptureCompletedAtMs === null
    || quickplayStartedAtMs === null
    || finishedAtMs === null
    || visualCaptureCompletedAtMs < quickplayStartedAtMs
    || visualCaptureCompletedAtMs > finishedAtMs) {
    errors.push('visual capture completion timestamp must be canonical and inside the quickplay runtime window');
  }
  if (evidence?.webGpu?.required !== true || evidence?.webGpu?.supported !== true) {
    errors.push('formal success must record the required supported WebGPU gate');
  }
  const webGpuFailure = webGpuRequirementFailure(evidence?.webGpu?.required === true, evidence?.webGpu);
  if (webGpuFailure) {
    errors.push(webGpuFailure);
  }
  if (evidence?.webGpu?.adapter !== true) {
    errors.push('formal success requires a WebGPU adapter');
  }
  if (evidence?.webGpu?.probeExecutedInPage !== true) {
    errors.push('WebGPU probe must execute in the page context');
  }
  if (evidence?.runtime?.ready !== true
    || evidence?.runtime?.status !== 'ready'
    || evidence?.runtime?.kind !== 'gui'
    || evidence?.runtime?.isRunning !== true
    || evidence?.runtime?.lastErrorPresent !== true
    || !Object.prototype.hasOwnProperty.call(evidence?.runtime ?? {}, 'lastError')
    || evidence?.runtime?.lastError !== null) {
    errors.push('GUI runtime readiness evidence is incomplete');
  }
  if (!safePositiveInteger(evidence?.runtime?.moduleBytes)
    || !safePositiveInteger(evidence?.runtime?.renderBytes)) {
    errors.push('GUI runtime byte evidence is incomplete');
  }
  if (evidence?.renderer?.active !== true
    || !Number.isSafeInteger(evidence?.renderer?.count)
    || evidence.renderer.count <= 0) {
    errors.push('renderer bridge readiness evidence is incomplete');
  }
  if (evidence?.runtime?.sessionId == null
    || evidence?.renderer?.sessionId !== evidence.runtime.sessionId) {
    errors.push('renderer bridge session does not match the active GUI runtime session');
  }
  if (!Array.isArray(evidence?.renderer?.capabilities)
    || evidence.renderer.capabilities.length !== evidence?.renderer?.count) {
    errors.push('renderer capability evidence does not match the renderer count');
  }
  if (evidence?.firstFrame?.ok !== true
    || evidence?.firstFrame?.ready !== true
    || evidence?.firstFrame?.skipped === true) {
    errors.push('first-frame evidence must be successful and unskipped');
  }
  if (evidence?.firstFrame?.source !== 'voplay-perf-endpoint') {
    errors.push('first-frame evidence must come from the Voplay perf endpoint');
  }
  if (!isBlockKartPerfFrameRecord(
    evidence?.firstFrame?.record,
    evidence?.quickplayStartedAt,
    evidence?.runtime?.sessionId,
  )) {
    errors.push('first-frame perf record is missing, stale, or malformed');
  }
  const initialReceivedAtMs = canonicalTimestamp(evidence?.firstFrame?.record?.receivedAt);
  if (initialReceivedAtMs === null
    || quickplayStartedAtMs === null
    || finishedAtMs === null
    || initialReceivedAtMs < quickplayStartedAtMs
    || initialReceivedAtMs > finishedAtMs) {
    errors.push('first-frame perf record timestamp is outside the quickplay runtime window');
  }
  if (evidence?.firstFrame?.frame !== evidence?.firstFrame?.record?.payload?.frame
    || !Number.isSafeInteger(evidence?.firstFrame?.reportCount)
    || evidence.firstFrame.reportCount <= 0) {
    errors.push('first-frame perf endpoint facts are inconsistent');
  }
  if (evidence?.finalFrame?.ok !== true
    || evidence?.finalFrame?.ready !== true
    || evidence?.finalFrame?.skipped === true
    || evidence?.finalFrame?.source !== 'voplay-perf-endpoint') {
    errors.push('post-capture frame evidence must be successful, unskipped, and endpoint-backed');
  }
  if (!isBlockKartPerfFrameRecord(
    evidence?.finalFrame?.record,
    evidence?.finalFrame?.notBefore,
    evidence?.runtime?.sessionId,
  )) {
    errors.push('post-capture perf record is missing, stale, malformed, or from another session');
  }
  const initialEpoch = evidence?.firstFrame?.record?.payload?.studioPerfEpoch;
  const finalEpoch = evidence?.finalFrame?.record?.payload?.studioPerfEpoch;
  const finalNotBeforeMs = canonicalTimestamp(evidence?.finalFrame?.notBefore);
  const finalReceivedAtMs = canonicalTimestamp(evidence?.finalFrame?.record?.receivedAt);
  if (finalReceivedAtMs === null
    || finalNotBeforeMs === null
    || finishedAtMs === null
    || finalReceivedAtMs < finalNotBeforeMs
    || finalReceivedAtMs > finishedAtMs) {
    errors.push('post-capture perf record timestamp is outside the final probe runtime window');
  }
  if (visualCaptureCompletedAtMs === null
    || finalNotBeforeMs === null
    || finalNotBeforeMs < visualCaptureCompletedAtMs) {
    errors.push('post-capture perf probe began before visual capture completed');
  }
  if (!safePositiveInteger(evidence?.finalFrame?.expectedPerfEpoch)
    || finalEpoch !== evidence.finalFrame.expectedPerfEpoch
    || !Number.isSafeInteger(initialEpoch)
    || finalEpoch <= initialEpoch) {
    errors.push('post-capture perf epoch did not advance causally');
  }
  if (evidence?.finalFrame?.minimumFrameExclusive !== evidence?.firstFrame?.frame
    || evidence?.finalFrame?.frame !== evidence?.finalFrame?.record?.payload?.frame
    || !safePositiveInteger(evidence?.finalFrame?.frame)
    || evidence.finalFrame.frame <= evidence?.firstFrame?.frame
    || !safePositiveInteger(evidence?.finalFrame?.reportCount)) {
    errors.push('post-capture renderer frame did not advance from the first-frame proof');
  }
  if (initialReceivedAtMs === null
    || finalNotBeforeMs === null
    || finalNotBeforeMs < initialReceivedAtMs) {
    errors.push('post-capture perf proof precedes the first-frame proof');
  }
  if (typeof evidence?.quickplay?.href !== 'string'
    || evidence.quickplay.href.length === 0
    || typeof evidence?.quickplay?.entryPath !== 'string'
    || evidence.quickplay.entryPath.length === 0
    || !safePositiveInteger(evidence?.quickplay?.canvasWidth)
    || !safePositiveInteger(evidence?.quickplay?.canvasHeight)) {
    errors.push('quickplay surface identity and dimensions are incomplete');
  }
  if (evidence?.bytecodeContract?.ok !== true
    || evidence?.bytecodeContract?.skipped === true
    || !safePositiveInteger(evidence?.bytecodeContract?.closureCount)
    || evidence?.bytecodeContract?.wrongCount !== 0) {
    errors.push('quickplay bytecode contract did not produce an applicable valid closure proof');
  }
  if (!Array.isArray(evidence?.pageFailures) || evidence.pageFailures.length !== 0) {
    errors.push('page failure evidence must be an empty array');
  }
  validateScreenshot('viewport', evidence?.screenshots?.viewport, errors);
  validateScreenshot('canvas', evidence?.screenshots?.canvas, errors);
  return errors;
}
