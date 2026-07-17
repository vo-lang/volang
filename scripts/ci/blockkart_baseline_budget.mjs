export const BLOCKKART_BASELINE_BUDGET_DEFAULTS = Object.freeze({
  minimumTimeoutMs: 900_000,
  firstFrameTimeoutMs: 90_000,
  captureMs: 6_000,
  restartCount: 0,
  restartWaitTimeoutMs: 30_000,
  renderStressScenario: false,
  startRaceScenario: false,
  storageReloadScenario: false,
  resizeScenario: false,
  resizeStepCount: 2,
  resizeStepWaitMs: 900,
  viewportWidth: 1280,
  viewportHeight: 720,
  longRunTelemetryThresholdMs: 60_000,
  longRunTelemetryMaxAgeMs: 45_000,
  longRunTelemetrySpanGraceMs: 60_000,
  startupWarnMs: 20_000,
  maxSlowFrames: 2,
  restartCooldownMs: 150,
  screenshotRetryDelayMs: 500,
  screenshotBringToFrontMs: 5_000,
  cdpScreenshotAttempts: 3,
  cdpScreenshotTimeoutMs: 15_000,
  canvasDataUrlAttempts: 3,
  canvasDataUrlTimeoutMs: 60_000,
  cleanupAndReportingMs: 240_000,
  observerGraceMs: 30_000,
});

export const BLOCKKART_BASELINE_MAX_TIMER_MS = 2_147_000_000;

function safeInteger(name, value, { allowZero = false } = {}) {
  if (!Number.isSafeInteger(value) || (allowZero ? value < 0 : value < 1)) {
    throw new Error(`${name} must be a ${allowZero ? 'non-negative' : 'positive'} safe integer`);
  }
  return value;
}

function checkedSum(parts) {
  let total = 0;
  for (const part of parts) {
    total += part;
    if (!Number.isSafeInteger(total) || total > BLOCKKART_BASELINE_MAX_TIMER_MS) {
      throw new Error(`BlockKart baseline timeout budget exceeds the Node timer limit (${BLOCKKART_BASELINE_MAX_TIMER_MS}ms)`);
    }
  }
  return total;
}

function boolean(name, value) {
  if (typeof value !== 'boolean') {
    throw new Error(`${name} must be a boolean`);
  }
  return value;
}

export function blockKartBaselineTimeoutBreakdown(options = {}) {
  const budget = { ...BLOCKKART_BASELINE_BUDGET_DEFAULTS, ...options };
  const minimumTimeoutMs = safeInteger('minimumTimeoutMs', budget.minimumTimeoutMs);
  if (minimumTimeoutMs > BLOCKKART_BASELINE_MAX_TIMER_MS) {
    throw new Error(`minimumTimeoutMs exceeds the Node timer limit (${BLOCKKART_BASELINE_MAX_TIMER_MS}ms)`);
  }
  const firstFrameTimeoutMs = safeInteger('firstFrameTimeoutMs', budget.firstFrameTimeoutMs);
  const captureMs = safeInteger('captureMs', budget.captureMs, { allowZero: true });
  const restartCount = safeInteger('restartCount', budget.restartCount, { allowZero: true });
  const restartWaitTimeoutMs = safeInteger('restartWaitTimeoutMs', budget.restartWaitTimeoutMs);
  const renderStressScenario = boolean('renderStressScenario', budget.renderStressScenario);
  const startRaceScenario = boolean('startRaceScenario', budget.startRaceScenario);
  const storageReloadScenario = boolean('storageReloadScenario', budget.storageReloadScenario);
  const resizeScenario = boolean('resizeScenario', budget.resizeScenario);
  const resizeStepCount = safeInteger('resizeStepCount', budget.resizeStepCount);
  const resizeStepWaitMs = safeInteger('resizeStepWaitMs', budget.resizeStepWaitMs);
  const restartCooldownMs = safeInteger('restartCooldownMs', budget.restartCooldownMs);
  const screenshotRetryDelayMs = safeInteger('screenshotRetryDelayMs', budget.screenshotRetryDelayMs);
  const screenshotBringToFrontMs = safeInteger('screenshotBringToFrontMs', budget.screenshotBringToFrontMs);
  const cdpScreenshotAttempts = safeInteger('cdpScreenshotAttempts', budget.cdpScreenshotAttempts);
  const cdpScreenshotTimeoutMs = safeInteger('cdpScreenshotTimeoutMs', budget.cdpScreenshotTimeoutMs);
  const canvasDataUrlAttempts = safeInteger('canvasDataUrlAttempts', budget.canvasDataUrlAttempts);
  const canvasDataUrlTimeoutMs = safeInteger('canvasDataUrlTimeoutMs', budget.canvasDataUrlTimeoutMs);
  const cleanupAndReportingMs = safeInteger('cleanupAndReportingMs', budget.cleanupAndReportingMs);
  const parts = {
    // Hook readiness, first frame, initial perf readiness, and post-capture perf readiness.
    readinessWaitsMs: firstFrameTimeoutMs * 4,
    captureMs,
    restartWaitsMs: restartCount * restartWaitTimeoutMs,
    restartCooldownsMs: restartCount * restartCooldownMs,
    scenarioWaitsMs: (
      Number(renderStressScenario)
      + Number(startRaceScenario)
      + Number(storageReloadScenario) * 2
    ) * restartWaitTimeoutMs,
    resizeWaitsMs: resizeScenario ? resizeStepCount * resizeStepWaitMs : 0,
    cdpScreenshotsMs: cdpScreenshotAttempts * cdpScreenshotTimeoutMs * 2,
    canvasDataUrlsMs: canvasDataUrlAttempts * canvasDataUrlTimeoutMs,
    screenshotControlMs: screenshotBringToFrontMs
      + Math.max(0, cdpScreenshotAttempts - 1) * screenshotRetryDelayMs
      + Math.max(0, canvasDataUrlAttempts - 1) * screenshotRetryDelayMs,
    cleanupAndReportingMs,
  };
  const calculatedTimeoutMs = checkedSum(Object.values(parts));
  return Object.freeze({
    ...parts,
    minimumTimeoutMs,
    calculatedTimeoutMs,
    timeoutMs: Math.max(minimumTimeoutMs, calculatedTimeoutMs),
  });
}

export function blockKartBaselineTimeoutBudget(options = {}) {
  return blockKartBaselineTimeoutBreakdown(options).timeoutMs;
}

export function configuredBlockKartBaselineTimeout(value, options = {}) {
  const requiredTimeoutMs = blockKartBaselineTimeoutBudget(options);
  if (value == null || String(value).trim() === '') {
    return requiredTimeoutMs;
  }
  const raw = String(value).trim();
  if (!/^[1-9][0-9]*$/.test(raw)) {
    throw new Error('BLOCKKART_BASELINE_TIMEOUT_MS must be a canonical positive integer');
  }
  const configuredTimeoutMs = Number(raw);
  if (!Number.isSafeInteger(configuredTimeoutMs)
    || configuredTimeoutMs > BLOCKKART_BASELINE_MAX_TIMER_MS) {
    throw new Error(`BLOCKKART_BASELINE_TIMEOUT_MS exceeds the Node timer limit (${BLOCKKART_BASELINE_MAX_TIMER_MS}ms)`);
  }
  if (configuredTimeoutMs < requiredTimeoutMs) {
    throw new Error(`BLOCKKART_BASELINE_TIMEOUT_MS ${configuredTimeoutMs}ms is below the required ${requiredTimeoutMs}ms budget`);
  }
  return configuredTimeoutMs;
}

export function blockKartBaselineObserverTimeoutBudget(options = {}) {
  const observerGraceMs = safeInteger(
    'observerGraceMs',
    options.observerGraceMs ?? BLOCKKART_BASELINE_BUDGET_DEFAULTS.observerGraceMs,
  );
  const childTimeoutMs = blockKartBaselineTimeoutBudget(options);
  return checkedSum([childTimeoutMs, observerGraceMs]);
}
