#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));
const outDir = path.resolve(argValue('--out-dir') || process.env.VOPLAY_RENDER_STRESS_OUT_DIR || path.join(root, 'target/voplay-render-stress'));
const restartCount = nonNegativeInt(argValue('--restart-count') || process.env.VOPLAY_RENDER_STRESS_RESTART_COUNT, 50);
const captureMs = positiveInt(argValue('--capture-ms') || process.env.VOPLAY_RENDER_STRESS_CAPTURE_MS, 36000);
const pulseMode = normalizePulseMode(argValue('--pulse-mode') || process.env.VOPLAY_RENDER_STRESS_PULSE_MODE || 'hybrid');
const budgetAllScenes = boolOption('--budget-all', process.env.VOPLAY_RENDER_STRESS_BUDGET_ALL, false);
const coverageCaptureDefaultMs = budgetAllScenes ? captureMs : Math.min(captureMs, 14000);
const coverageCaptureMs = positiveInt(argValue('--coverage-capture-ms') || process.env.VOPLAY_RENDER_STRESS_COVERAGE_CAPTURE_MS, coverageCaptureDefaultMs);
const soakOnly = budgetAllScenes && restartCount === 0 && captureMs >= 600000 && coverageCaptureMs >= 600000;
const budgetPath = path.resolve(argValue('--budget') || process.env.VOPLAY_RENDER_STRESS_BUDGET || path.join(root, 'eng/perf-budgets/blockkart-voplay.medium.json'));
if (!existsSync(budgetPath)) fail(`budget file missing: ${budgetPath}`);
const perfBudget = readJson(budgetPath);
const targetFps = positiveInt(perfBudget?.target?.fps, 60);
const targetFrameMs = 1000 / targetFps;
const renderBudget = perfBudget?.render ?? {};

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : process.argv[index + 1] ?? '';
}

function nonNegativeInt(value, fallback) {
  const parsed = Number.parseInt(String(value ?? ''), 10);
  return Number.isFinite(parsed) && parsed >= 0 ? parsed : fallback;
}

function positiveInt(value, fallback) {
  const parsed = Number.parseInt(String(value ?? ''), 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function boolOption(name, envValue, fallback) {
  const index = process.argv.indexOf(name);
  if (index !== -1) {
    const raw = process.argv[index + 1];
    if (raw && !raw.startsWith('--')) {
      return parseBool(raw, true);
    }
    return true;
  }
  if (envValue != null && String(envValue) !== '') {
    return parseBool(envValue, fallback);
  }
  return fallback;
}

function parseBool(value, fallback) {
  const raw = String(value ?? '').trim().toLowerCase();
  if (raw === '1' || raw === 'true' || raw === 'yes' || raw === 'on') return true;
  if (raw === '0' || raw === 'false' || raw === 'no' || raw === 'off') return false;
  return fallback;
}

function normalizePulseMode(value) {
  const mode = String(value ?? '').trim().toLowerCase();
  if (mode === '' || mode === 'default') {
    return '';
  }
  if (mode === 'raf' || mode === 'timer' || mode === 'hybrid') {
    return mode;
  }
  fail(`unsupported --pulse-mode "${value}"; expected raf, timer, hybrid, or default`);
}

function readJson(file) {
  return JSON.parse(readFileSync(file, 'utf8'));
}

function fail(message) {
  console.error(`voplay render stress: ${message}`);
  process.exit(1);
}

function runBaseline(name, args, options = {}) {
  const sceneOut = path.join(outDir, name);
  rmSync(sceneOut, { recursive: true, force: true });
  mkdirSync(sceneOut, { recursive: true });
  const command = [
    'scripts/ci/blockkart_baseline.mjs',
    '--out-dir',
    sceneOut,
    '--capture-ms',
    String(options.captureMs ?? captureMs),
    ...(pulseMode ? ['--pulse-mode', pulseMode] : []),
    ...args,
  ];
  const result = spawnSync(process.execPath, command, {
    cwd: root,
    env: { ...process.env },
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  const logPath = path.join(sceneOut, 'baseline-run.log');
  writeFileSync(logPath, `${result.stdout ?? ''}${result.stderr ?? ''}`);
  const jsonPath = path.join(sceneOut, 'blockkart-baseline.json');
  const markdownPath = path.join(sceneOut, 'blockkart-baseline.md');
  if (result.status !== 0 && !(options.allowBaselineFailureWithJson && existsSync(jsonPath))) {
    return {
      name,
      status: 'fail',
      command: [process.execPath, ...command],
      artifacts: { directory: sceneOut, log: logPath, json: existsSync(jsonPath) ? jsonPath : null, markdown: existsSync(markdownPath) ? markdownPath : null },
      issues: [{ code: 'baseline.command_failed', severity: 0, detail: tail(`${result.stdout ?? ''}${result.stderr ?? ''}`) }],
    };
  }
  if (!existsSync(jsonPath)) {
    return {
      name,
      status: 'fail',
      command: [process.execPath, ...command],
      artifacts: { directory: sceneOut, log: logPath, json: null, markdown: existsSync(markdownPath) ? markdownPath : null },
      issues: [{ code: 'baseline.missing_json', severity: 0, detail: `${jsonPath} was not written` }],
    };
  }
  return summarizeBaselineScene(name, readJson(jsonPath), { sceneOut, logPath, jsonPath, markdownPath, command: [process.execPath, ...command] }, options);
}

function summarizeBaselineScene(name, baseline, meta, options = {}) {
  const strictBudget = options.strictBudget !== false;
  const requirePerfSummary = options.requirePerfSummary !== false;
  const sceneBudget = sceneRenderBudget(name);
  const perfSummary = latestPerfSummary(baseline);
  const renderer = perfSummary?.renderer ?? latestRendererFromReports(baseline.perfReports);
  const workload = perfSummary?.workload ?? {};
  const latestSceneReport = baseline.diagnostics?.latestSceneReport ?? null;
  const stressSceneReport = baseline.renderStress?.latestSceneReport ?? null;
  const primitiveUploadBytes = Math.max(
    numberOr(latestSceneReport?.primitiveUploadBytes, 0),
    numberOr(stressSceneReport?.primitiveUploadBytes, 0),
  );
  const bufferCreates = numberOr(workload.bufferCreates, 0);
  const bindGroupCreates = numberOr(workload.bindGroupCreates, 0);
  const textureUploads = numberOr(workload.textureUploads, 0);
  const residentChunkRebuilds = numberOr(workload.residentChunkRebuilds, 0);
  const resourceChurnEvents = bufferCreates + bindGroupCreates + textureUploads + residentChunkRebuilds + (primitiveUploadBytes > 0 ? 1 : 0);
  const pulse = latestPerfReport(baseline.perfReports, 'pulse')?.metrics?.wait ?? {};
  const webgpu = latestPerfReport(baseline.perfReports, 'webgpu')?.metrics ?? {};
  const frameP90Ms = numberOr(perfSummary?.window?.frameP90Ms, pulse.p90);
  const frameP99Ms = numberOr(perfSummary?.window?.frameP99Ms, pulse.p99);
  const windowSlowFrames = numberOr(perfSummary?.window?.slowFrames, null);
  const activeWorkP90Ms = numberOr(perfSummary?.window?.activeWorkP90Ms, perfSummary?.gate?.activeWorkP90Ms);
  const activeWorkP99Ms = numberOr(perfSummary?.window?.activeWorkP99Ms, perfSummary?.gate?.activeWorkP99Ms);
  const renderWorkP90Ms = numberOr(perfSummary?.window?.renderWorkP90Ms, perfSummary?.current?.renderWorkMs);
  const renderSubmitP90Ms = numberOr(perfSummary?.window?.renderSubmitP90Ms, webgpu.submitCpu?.p90);
  const gpuWorkDoneP90Ms = numberOr(perfSummary?.window?.gpuWorkDoneP90Ms, webgpu.workDoneRaw?.p90);
  const passTimings = rendererPassTimings(renderer);
  const slowestPass = Object.entries(passTimings)
    .sort((a, b) => Number(b[1] ?? 0) - Number(a[1] ?? 0))[0] ?? ['', 0];
  const issues = [];
  for (const issue of baseline.issues ?? []) {
    if (shouldIgnoreBaselineIssue(issue, options)) {
      continue;
    }
    if (issue.severity === 'P0' || issue.severity === 'P1') {
      issues.push({ code: `baseline.${issue.severity}`, severity: issue.severity === 'P0' ? 0 : 1, detail: `${issue.owner}: ${issue.title}: ${issue.detail ?? ''}` });
    }
  }
  if (!baseline.visual?.canvas?.nonEmpty) {
    issues.push({ code: 'canvas.blank', severity: 0, detail: JSON.stringify(baseline.visual?.canvas ?? {}) });
  }
  if (baseline.lifecycle?.state !== 'Running' || baseline.lifecycle?.reachedRunning !== true) {
    issues.push({ code: 'lifecycle.not_running', severity: 0, detail: JSON.stringify(baseline.lifecycle ?? {}) });
  }
  const warnings = baseline.warnings?.length ?? 0;
  const errors = baseline.errors?.length ?? 0;
  const resourceFailures = baseline.resourceFailures?.length ?? 0;
  if (warnings > 0) issues.push({ code: 'console.warning', severity: 1, detail: `${warnings} warnings` });
  if (errors > 0) issues.push({ code: 'console.error', severity: 0, detail: `${errors} errors` });
  if (resourceFailures > 0) issues.push({ code: 'resource.failure', severity: 0, detail: `${resourceFailures} resource failures` });
  if (!Number.isFinite(frameP90Ms) || !Number.isFinite(frameP99Ms)) {
    issues.push({ code: 'render.missing_real_samples', severity: 1, detail: 'baseline did not expose frame p90/p99 samples' });
  }
  if (requirePerfSummary && !perfSummary) {
    issues.push({ code: 'render.missing_perf_summary', severity: 1, detail: 'BlockKart did not emit a perf-summary report with renderer pass diagnostics' });
  }
  const budgetP90Ms = numberOr(sceneBudget.p90Ms, renderBudget.p90Ms, targetFrameMs);
  const budgetP99Ms = numberOr(sceneBudget.p99Ms, renderBudget.p99Ms, targetFrameMs);
  const budgetSlowFrames = numberOr(sceneBudget.maxSlowFrames, renderBudget.maxSlowFrames, null);
  const budgetResourceChurn = numberOr(sceneBudget.maxResourceChurnEvents, renderBudget.maxResourceChurnEvents, null);
  const budgetUploadBytes = numberOr(sceneBudget.maxUploadBytes, renderBudget.maxUploadBytes, null);
  const enforceRenderFrameBudget = strictBudget && (perfSummary || requirePerfSummary);
  const p90OverBudget = enforceRenderFrameBudget && Number.isFinite(frameP90Ms) && Number.isFinite(budgetP90Ms) && frameP90Ms > budgetP90Ms;
  const p99OverBudget = enforceRenderFrameBudget && Number.isFinite(frameP99Ms) && Number.isFinite(budgetP99Ms) && frameP99Ms > budgetP99Ms;
  const slowFramesOverBudget = Number.isFinite(windowSlowFrames) && Number.isFinite(budgetSlowFrames) && windowSlowFrames > budgetSlowFrames;
  const resourceChurnOverBudget = Number.isFinite(resourceChurnEvents) && Number.isFinite(budgetResourceChurn) && resourceChurnEvents > budgetResourceChurn;
  const uploadOverBudget = Number.isFinite(primitiveUploadBytes) && Number.isFinite(budgetUploadBytes) && primitiveUploadBytes > budgetUploadBytes;
  const hostPacingOnly = isHostPacingOnly(perfSummary, {
    frameP90Ms,
    frameP99Ms,
    activeWorkP90Ms,
    activeWorkP99Ms,
    renderWorkP90Ms,
    renderSubmitP90Ms,
    gpuWorkDoneP90Ms,
    passTimings,
    budgetP90Ms,
    budgetP99Ms,
  });
  if (strictBudget && perfSummary && perfSummary.gatePassed === false) {
    issues.push({ code: 'render.perf_gate_failed', severity: 1, detail: `status=${perfSummary.status ?? 'unknown'} failure=${perfSummary.failure ?? 'unknown'} hostPacingOnly=${hostPacingOnly}` });
  }
  if (requirePerfSummary && ((workload.frameGraphPasses ?? 0) <= 0 || (workload.frameGraphResources ?? 0) <= 0)) {
    issues.push({ code: 'render.missing_frame_graph_diagnostics', severity: 1, detail: JSON.stringify(workload) });
  }
  if (p90OverBudget) {
    issues.push({ code: 'render.p90_over_budget', severity: 1, detail: `${frameP90Ms.toFixed(2)}ms > ${budgetP90Ms.toFixed(2)}ms` });
  }
  if (p99OverBudget) {
    issues.push({ code: 'render.p99_over_budget', severity: 1, detail: `${frameP99Ms.toFixed(2)}ms > ${budgetP99Ms.toFixed(2)}ms hostPacingOnly=${hostPacingOnly}` });
  }
  if (strictBudget && slowFramesOverBudget) {
    issues.push({ code: 'render.slow_frames_over_budget', severity: 1, detail: `${windowSlowFrames} > ${budgetSlowFrames} hostPacingOnly=${hostPacingOnly}` });
  }
  if ((workload.frameGraphTargets ?? 0) > 0 && workload.frameGraphReadyTargets !== workload.frameGraphTargets) {
    issues.push({ code: 'render.target_not_ready', severity: 0, detail: `${workload.frameGraphReadyTargets}/${workload.frameGraphTargets} targets ready` });
  }
  if (strictBudget && resourceChurnOverBudget) {
    issues.push({ code: 'render.resource_churn_over_budget', severity: 1, detail: `${resourceChurnEvents} > ${budgetResourceChurn}` });
  }
  if (strictBudget && uploadOverBudget) {
    issues.push({ code: 'render.upload_over_budget', severity: 1, detail: `${primitiveUploadBytes} > ${budgetUploadBytes}` });
  }
  const waterSurfaceCount = Math.max(
    numberOr(latestSceneReport?.waterSurfaces, 0),
    numberOr(latestSceneReport?.visibleWaterSurfaces, 0),
    numberOr(stressSceneReport?.waterSurfaces, 0),
    numberOr(stressSceneReport?.visibleWaterSurfaces, 0),
  );
  if (/water/i.test(name) && waterSurfaceCount > 0 && numberOr(workload.waterDraws, 0) <= 0) {
    issues.push({ code: 'render.water_pass_not_submitted', severity: 0, detail: `waterSurfaces=${waterSurfaceCount} waterDraws=${numberOr(workload.waterDraws, 0)}` });
  }
  return {
    name,
    status: issues.some((issue) => issue.severity <= 1) ? 'fail' : 'pass',
    command: meta.command,
    budgetApplied: strictBudget,
    budget: sceneBudget,
    artifacts: {
      directory: meta.sceneOut,
      log: meta.logPath,
      json: meta.jsonPath,
      markdown: meta.markdownPath,
      viewportScreenshot: baseline.artifacts?.viewportScreenshot ?? null,
      canvasScreenshot: baseline.artifacts?.canvasScreenshot ?? null,
    },
    viewport: baseline.viewport,
    canvas: {
      width: baseline.finalState?.canvasWidth ?? 0,
      height: baseline.finalState?.canvasHeight ?? 0,
      nonEmpty: baseline.visual?.canvas?.nonEmpty === true,
      uniqueSampledColors: baseline.visual?.canvas?.uniqueSampledColors ?? 0,
      lumaStdDev: baseline.visual?.canvas?.lumaStdDev ?? 0,
    },
    lifecycle: baseline.lifecycle,
    timings: {
      frameId: perfSummary?.frame ?? null,
      frameP90Ms,
      frameP99Ms,
      windowSlowFrames,
      activeWorkP90Ms,
      activeWorkP99Ms,
      renderWorkP90Ms,
      renderSubmitP90Ms,
      gpuWorkDoneP90Ms,
      slowFrames: baseline.voplaySlowFrames?.length ?? 0,
      passTimings,
      slowestPass: { name: slowestPass[0], ms: Number(slowestPass[1] ?? 0) },
      classification: perfSummary?.classification ?? null,
    },
    frameGraph: {
      passes: workload.frameGraphPasses ?? 0,
      resources: workload.frameGraphResources ?? 0,
      targets: workload.frameGraphTargets ?? 0,
      readyTargets: workload.frameGraphReadyTargets ?? 0,
    },
    workload: {
      drawCalls: workload.drawCalls ?? 0,
      modelDraws: workload.modelDraws ?? 0,
      primitiveDraws: workload.primitiveDraws ?? 0,
      primitiveChunks: workload.primitiveChunks ?? 0,
      instances: workload.instances ?? 0,
      triangles: workload.triangles ?? 0,
      postEffects: workload.postEffects ?? 0,
      bufferCreates,
      bindGroupCreates,
      textureUploads,
      residentChunkRebuilds,
      resourceChurnEvents,
      primitiveUploadBytes,
      waterDraws: workload.waterDraws ?? 0,
      diagnosticFlags: workload.diagnosticFlags ?? 0,
    },
    attribution: {
      slowFrames: baseline.performanceAttribution?.slowFrames ?? null,
      render: baseline.performanceAttribution?.render ?? null,
      sceneComplexity: baseline.performanceAttribution?.sceneComplexity ?? null,
      optimizationTarget: baseline.performanceAttribution?.optimizationTarget ?? null,
    },
    diagnostics: {
      perfGate: perfSummary ? {
        passed: perfSummary.gatePassed === true,
        status: perfSummary.status ?? '',
        failure: perfSummary.failure ?? '',
        hostPacingOnly,
      } : null,
      latestSceneReport,
      latestVehicleReport: baseline.diagnostics?.latestVehicleReport ?? null,
      renderStress: baseline.renderStress ?? null,
      resize: baseline.resize ?? null,
      perfReportCount: baseline.perfReports?.length ?? 0,
      warnings,
      errors,
      resourceFailures,
      restart: baseline.restart,
    },
    issues,
  };
}

function latestPerfSummary(baseline) {
  const reports = (baseline.perfReports ?? []).filter((report) => report?.kind === 'perf-summary');
  return reports.at(-1) ?? null;
}

function shouldIgnoreBaselineIssue(issue, options) {
  if (!options.ignoreBaselineSlowFrameIssue) {
    return false;
  }
  return issue?.severity === 'P1'
    && String(issue?.owner ?? '').toLowerCase() === 'voplay'
    && /slow frames/i.test(String(issue?.title ?? ''));
}

function latestPerfReport(reports, kind) {
  return (reports ?? []).filter((report) => report?.kind === kind).at(-1) ?? null;
}

function latestRendererFromReports(reports) {
  const renderer = latestPerfSummary({ perfReports: reports })?.renderer;
  return renderer ?? {};
}

function rendererPassTimings(renderer) {
  return {
    depth: numberOr(renderer?.depthPassMs, 0),
    shadow: numberOr(renderer?.shadowPassMs, 0),
    main: numberOr(renderer?.mainPassMs, 0),
    post: numberOr(renderer?.postPassMs, 0),
    overlay: numberOr(renderer?.overlayPassMs, 0),
    backendSubmit: numberOr(renderer?.queueSubmitCpuMs, 0) + numberOr(renderer?.presentCpuMs, 0),
  };
}

function isHostPacingOnly(perfSummary, metrics) {
  if (!perfSummary) {
    return false;
  }
  const status = String(perfSummary.status ?? '');
  if (status !== 'pacing-limited' && status !== 'cadence-jitter') {
    return false;
  }
  const failure = String(perfSummary.failure ?? '');
  if (failure !== 'slow_frames' && failure !== 'frame_max' && failure !== 'frame_p99' && !(failure === '' && status === 'cadence-jitter')) {
    return false;
  }
  const budgetP90Ms = Number(metrics.budgetP90Ms);
  if (!Number.isFinite(budgetP90Ms) || budgetP90Ms <= 0) {
    return false;
  }
  const frameP90Ms = Number(metrics.frameP90Ms);
  const activeWorkP90Ms = Number(metrics.activeWorkP90Ms);
  const activeWorkP99Ms = Number(metrics.activeWorkP99Ms);
  const renderWorkP90Ms = Number(metrics.renderWorkP90Ms);
  const renderSubmitP90Ms = Number(metrics.renderSubmitP90Ms);
  const gpuWorkDoneP90Ms = Number(metrics.gpuWorkDoneP90Ms);
  const slowestPassMs = Math.max(...Object.values(metrics.passTimings ?? {}).map((value) => Number(value)).filter(Number.isFinite), 0);
  const renderLaneBudget = Math.max(3, budgetP90Ms * 0.35);
  return Number.isFinite(frameP90Ms)
    && frameP90Ms <= budgetP90Ms
    && (!Number.isFinite(activeWorkP90Ms) || activeWorkP90Ms <= budgetP90Ms)
    && (!Number.isFinite(activeWorkP99Ms) || activeWorkP99Ms <= budgetP90Ms)
    && (!Number.isFinite(renderWorkP90Ms) || renderWorkP90Ms <= renderLaneBudget)
    && (!Number.isFinite(renderSubmitP90Ms) || renderSubmitP90Ms <= renderLaneBudget)
    && (!Number.isFinite(gpuWorkDoneP90Ms) || gpuWorkDoneP90Ms <= renderLaneBudget)
    && slowestPassMs <= renderLaneBudget
    && hasPaceDominatedSpike(perfSummary.classification, budgetP90Ms, renderLaneBudget);
}

function hasPaceDominatedSpike(classification, budgetP90Ms, renderLaneBudget) {
  return paceDominatedFrame(classification?.latestSpike, budgetP90Ms, renderLaneBudget)
    || paceDominatedFrame(classification?.windowWorst, budgetP90Ms, renderLaneBudget);
}

function paceDominatedFrame(frame, budgetP90Ms, renderLaneBudget) {
  const frameMs = Number(frame?.frameMs);
  const paceWaitMs = Number(frame?.paceWaitMs);
  if (!Number.isFinite(frameMs) || !Number.isFinite(paceWaitMs) || frameMs <= budgetP90Ms) {
    return false;
  }
  const renderWorkMs = Number(frame?.renderWorkMs);
  const submitCpuMs = Number(frame?.submitCpuMs ?? frame?.rendererQueueSubmitCpuMs);
  const gpuWorkDoneMs = Number(frame?.gpuWorkDoneMs);
  return paceWaitMs >= Math.max(frameMs * 0.7, budgetP90Ms * 0.5)
    && (!Number.isFinite(renderWorkMs) || renderWorkMs <= renderLaneBudget)
    && (!Number.isFinite(submitCpuMs) || submitCpuMs <= renderLaneBudget)
    && (!Number.isFinite(gpuWorkDoneMs) || gpuWorkDoneMs <= renderLaneBudget);
}

function numberOr(...values) {
  for (const value of values) {
    const number = Number(value);
    if (Number.isFinite(number)) return number;
  }
  return null;
}

function sceneRenderBudget(name) {
  return {
    ...(renderBudget ?? {}),
    ...((renderBudget.scenes ?? {})[name] ?? {}),
  };
}

function maxSceneDiagnostic(scene, key) {
  return Math.max(
    numberOr(scene.diagnostics?.latestSceneReport?.[key], 0),
    numberOr(scene.diagnostics?.renderStress?.latestSceneReport?.[key], 0),
  );
}

function tail(text) {
  const value = String(text ?? '');
  return value.length > 4000 ? value.slice(value.length - 4000) : value;
}

function markdownReport(report) {
  const lines = ['# voplay render stress', ''];
  lines.push(`- Status: ${report.status}`);
  lines.push(`- Target: ${report.target.width}x${report.target.height} @ ${report.target.fps} FPS`);
  lines.push(`- Budget: ${path.relative(root, report.budget.path)} p90=${formatMs(report.budget.render.p90Ms)} p99=${formatMs(report.budget.render.p99Ms)} slowFrames=${report.budget.render.maxSlowFrames}`);
  lines.push(`- Budget all scenes: ${report.budgetAllScenes === true}`);
  lines.push(`- Scene count: ${report.summary.sceneCount}`);
  lines.push(`- P0/P1: ${report.summary.p0}/${report.summary.p1}`);
  lines.push(`- Frame p90/p99: ${formatMs(report.summary.frameP90Ms)} / ${formatMs(report.summary.frameP99Ms)}`);
  lines.push(`- Coverage: ${Object.entries(report.coverage ?? {}).map(([name, ok]) => `${name}=${ok === true}`).join(' ')}`);
  lines.push('');
  for (const scene of report.scenes) {
    const artifacts = scene.artifacts ?? {};
    const canvas = scene.canvas ?? {};
    const lifecycle = scene.lifecycle ?? {};
    const timings = scene.timings ?? {};
    const frameGraph = scene.frameGraph ?? {};
    const workload = scene.workload ?? {};
    const diagnostics = scene.diagnostics ?? {};
    const slowestPass = timings.slowestPass ?? {};
    lines.push(`## ${scene.name}`);
    lines.push(`- Status: ${scene.status} budgetApplied=${scene.budgetApplied === true}`);
    lines.push(`- Baseline JSON: ${path.relative(root, artifacts.json ?? artifacts.directory ?? outDir)}`);
    lines.push(`- Baseline MD: ${path.relative(root, artifacts.markdown ?? artifacts.directory ?? outDir)}`);
    lines.push(`- Canvas screenshot: ${artifacts.canvasScreenshot ? path.relative(root, artifacts.canvasScreenshot) : 'missing'}`);
    lines.push(`- Canvas: ${canvas.width ?? 0}x${canvas.height ?? 0} nonEmpty=${canvas.nonEmpty === true} colors=${canvas.uniqueSampledColors ?? 0}`);
    lines.push(`- Lifecycle: ${lifecycle.state ?? 'missing'} reachedRunning=${lifecycle.reachedRunning === true}`);
    lines.push(`- Timings: frame=${timings.frameId ?? 'n/a'} p90/p99=${formatMs(timings.frameP90Ms)}/${formatMs(timings.frameP99Ms)} activeP90/P99=${formatMs(timings.activeWorkP90Ms)}/${formatMs(timings.activeWorkP99Ms)} renderWorkP90=${formatMs(timings.renderWorkP90Ms)} windowSlow=${timings.windowSlowFrames ?? 'n/a'} submitP90=${formatMs(timings.renderSubmitP90Ms)} gpuP90=${formatMs(timings.gpuWorkDoneP90Ms)}`);
    if (timings.classification) {
      const classification = timings.classification;
      const worst = classification.windowWorst ?? {};
      lines.push(`- Classification: current=${classification.primaryCause ?? 'n/a'} worst=${classification.windowWorstCause ?? worst.cause ?? 'n/a'} worstFrame=${classification.windowWorstFrame ?? worst.frame ?? 'n/a'} worstMs=${formatMs(classification.windowWorstMs ?? worst.frameMs)} evidence=${worst.evidence ?? 'n/a'}`);
    }
    lines.push(`- Passes: slowest=${slowestPass.name || 'unknown'} ${formatMs(slowestPass.ms)} graph=${frameGraph.passes ?? 0} passes ${frameGraph.resources ?? 0} resources targets=${frameGraph.readyTargets ?? 0}/${frameGraph.targets ?? 0}`);
    lines.push(`- Workload: draws=${workload.drawCalls ?? 0} models=${workload.modelDraws ?? 0} primitives=${workload.primitiveDraws ?? 0} chunks=${workload.primitiveChunks ?? 0} instances=${workload.instances ?? 0} churn=${workload.resourceChurnEvents ?? 0} upload=${workload.primitiveUploadBytes ?? 0}`);
    if (scene.attribution?.slowFrames) {
      const slow = scene.attribution.slowFrames;
      lines.push(`- Slow attribution: count=${slow.count ?? 0} steady=${slow.steadyState ?? 0} restart=${slow.restartRebuild ?? 0} reasons=${JSON.stringify(slow.byReason ?? {})}`);
    }
    if (scene.attribution?.optimizationTarget) {
      const target = scene.attribution.optimizationTarget;
      lines.push(`- Optimization target: ${target.owner ?? 'n/a'} ${target.subsystem ?? 'n/a'} ${target.name ?? 'n/a'} ${formatMs(target.durationMs)}`);
    }
    lines.push(`- Diagnostics: warnings=${diagnostics.warnings ?? 0} errors=${diagnostics.errors ?? 0} resourceFailures=${diagnostics.resourceFailures ?? 0} perfReports=${diagnostics.perfReportCount ?? 0} gate=${diagnostics.perfGate?.passed === true ? 'pass' : diagnostics.perfGate?.failure ?? 'missing'}`);
    for (const issue of scene.issues ?? []) {
      lines.push(`- Issue ${issue.code}: ${issue.detail}`);
    }
    lines.push('');
  }
  for (const issue of report.coverageIssues ?? []) {
    lines.push(`- Coverage issue ${issue.detail}`);
  }
  for (const issue of report.summaryIssues ?? []) {
    lines.push(`- Summary issue ${issue.code}: ${issue.detail}`);
  }
  return `${lines.join('\n')}\n`;
}

function formatMs(value) {
  const number = Number(value);
  return Number.isFinite(number) ? `${number.toFixed(2)}ms` : 'n/a';
}

mkdirSync(outDir, { recursive: true });
const baselinePerfArgs = ['--perf-mode', 'stats', '--perf-console', '0', '--perf-gpu-probe', '0', '--perf-diag', 'pulseHybrid'];
const coverageArgs = ['--no-fail-on-issues', ...baselinePerfArgs];
const coverageBudgetOptions = {
  strictBudget: budgetAllScenes,
  ignoreBaselineSlowFrameIssue: !budgetAllScenes,
};
const scenes = [
  runBaseline(soakOnly ? 'blockkart-quickplay-baseline-soak-10m' : 'blockkart-quickplay-baseline', baselinePerfArgs, { strictBudget: true, captureMs }),
];
if (!soakOnly) {
  scenes.push(
    runBaseline('blockkart-primitive-10k', [...coverageArgs, '--stress-profile', 'primitive10k'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }),
    runBaseline('blockkart-water', [...coverageArgs, '--stress-profile', 'water'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }),
    runBaseline('blockkart-resource-churn-soak', [...coverageArgs, '--stress-profile', 'resource-churn'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }),
    runBaseline('blockkart-chunked-world-drive', [...coverageArgs, '--stress-profile', 'chunked-world-drive'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }),
    runBaseline('blockkart-shadow-post-matrix', [...coverageArgs, '--stress-profile', 'shadow-post-matrix'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }),
    runBaseline('blockkart-resize-recreate-targets', [...coverageArgs, '--resize-cycle'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }),
  );
}
if (!soakOnly && restartCount > 0) {
  scenes.push(runBaseline(`blockkart-restart-${restartCount}`, ['--no-fail-on-issues', ...baselinePerfArgs, '--restart-count', String(restartCount)], { ...coverageBudgetOptions, requirePerfSummary: false, allowBaselineFailureWithJson: true }));
}

const p0 = scenes.flatMap((scene) => scene.issues.filter((issue) => issue.severity === 0)).length;
let p1 = scenes.flatMap((scene) => scene.issues.filter((issue) => issue.severity === 1)).length;
const frameP90Values = scenes.map((scene) => scene.timings?.frameP90Ms).filter(Number.isFinite);
const frameP99Values = scenes.map((scene) => scene.timings?.frameP99Ms).filter(Number.isFinite);
const summaryFrameP90Ms = frameP90Values.length ? Math.max(...frameP90Values) : null;
const summaryFrameP99Ms = frameP99Values.length ? Math.max(...frameP99Values) : null;
const summarySlowFrames = scenes.reduce((sum, scene) => sum + (scene.timings?.slowFrames ?? 0), 0);
const coverage = {
  blockKartBaseline: scenes.some((scene) => scene.name === 'blockkart-quickplay-baseline' && scene.status === 'pass'),
  primitive10k: scenes.some((scene) => (scene.workload?.instances ?? 0) >= 10000 || (scene.workload?.primitiveDraws ?? 0) >= 10000 || maxSceneDiagnostic(scene, 'primitiveInstances') >= 10000),
  chunkedWorld: scenes.some((scene) => (scene.workload?.primitiveChunks ?? 0) > 0),
  water: scenes.some((scene) => maxSceneDiagnostic(scene, 'waterSurfaces') > 0 || maxSceneDiagnostic(scene, 'visibleWaterSurfaces') > 0 || (scene.workload?.waterDraws ?? 0) > 0),
  shadowPostMatrix: scenes.some((scene) => Number(scene.timings?.passTimings?.shadow ?? 0) > 0 && Number(scene.workload?.postEffects ?? 0) > 0),
  restart50: restartCount >= 50 && scenes.some((scene) => scene.diagnostics?.restart?.requested === restartCount && scene.diagnostics?.restart?.completed === restartCount),
  resizeRecreateTargets: scenes.some((scene) => scene.diagnostics?.resize?.completed === true || scene.diagnostics?.latestSceneReport?.resizeRecreateTargets === true),
  resourceChurnSoak: scenes.some((scene) => scene.name === 'blockkart-resource-churn-soak' && scene.diagnostics?.renderStress?.completed === true && Number(scene.workload?.resourceChurnEvents ?? 0) > 0),
  realCanvasCapture: scenes.every((scene) => scene.canvas?.nonEmpty === true),
  realPerfSamples: scenes.every((scene) => Number.isFinite(scene.timings?.frameP90Ms) && Number.isFinite(scene.timings?.frameP99Ms)),
};
const coverageIssues = soakOnly ? [] : Object.entries(coverage)
  .filter(([, covered]) => covered !== true)
  .map(([name]) => ({ code: 'coverage.missing', severity: 1, detail: name }));
p1 += coverageIssues.length;
const summaryIssues = [];
const summaryBudgetP90Ms = numberOr(renderBudget.p90Ms, targetFrameMs);
const summaryBudgetP99Ms = numberOr(renderBudget.p99Ms, targetFrameMs);
const summaryBudgetSlowFrames = numberOr(renderBudget.maxSlowFrames, null);
if (Number.isFinite(summaryFrameP90Ms) && Number.isFinite(summaryBudgetP90Ms) && summaryFrameP90Ms > summaryBudgetP90Ms) {
  summaryIssues.push({ code: 'summary.p90_over_budget', severity: 1, detail: `${summaryFrameP90Ms.toFixed(2)}ms > ${summaryBudgetP90Ms.toFixed(2)}ms` });
}
if (Number.isFinite(summaryFrameP99Ms) && Number.isFinite(summaryBudgetP99Ms) && summaryFrameP99Ms > summaryBudgetP99Ms) {
  summaryIssues.push({ code: 'summary.p99_over_budget', severity: 1, detail: `${summaryFrameP99Ms.toFixed(2)}ms > ${summaryBudgetP99Ms.toFixed(2)}ms` });
}
if (Number.isFinite(summarySlowFrames) && Number.isFinite(summaryBudgetSlowFrames) && summarySlowFrames > summaryBudgetSlowFrames) {
  summaryIssues.push({ code: 'summary.slow_frames_over_budget', severity: 1, detail: `${summarySlowFrames} > ${summaryBudgetSlowFrames}` });
}
p1 += summaryIssues.filter((issue) => issue.severity === 1).length;
const report = {
  schemaVersion: 1,
  kind: 'voplay.renderStressReport',
  mode: soakOnly ? 'soak-10m' : 'stress',
  generatedAt: new Date().toISOString(),
  target: { width: perfBudget?.target?.width ?? 1280, height: perfBudget?.target?.height ?? 720, fps: targetFps, frameBudgetMs: Number(targetFrameMs.toFixed(4)) },
  budget: { path: budgetPath, render: renderBudget },
  budgetAllScenes,
  scenes,
  summary: {
    sceneCount: scenes.length,
    passCount: scenes.filter((scene) => scene.status === 'pass').length,
    p0,
    p1,
    frameP90Ms: summaryFrameP90Ms,
    frameP99Ms: summaryFrameP99Ms,
    slowFrames: summarySlowFrames,
  },
  coverage,
  coverageIssues,
  summaryIssues,
  status: p0 === 0 && p1 === 0 ? 'pass' : 'fail',
};
writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify(report, null, 2)}\n`);
writeFileSync(path.join(outDir, 'report.md'), markdownReport(report));

if (report.status !== 'pass') {
  fail(`stress thresholds failed: p0=${p0} p1=${p1}; see ${path.join(outDir, 'report.json')}`);
}
console.log(`voplay render stress: ok ${path.join(outDir, 'report.json')}`);
