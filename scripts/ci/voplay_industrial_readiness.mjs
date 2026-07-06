#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');

const args = process.argv.slice(2);
let outDir = path.join(root, 'target', 'voplay-industrial-readiness');
let allowNotReady = false;
for (let i = 0; i < args.length; i++) {
  const arg = args[i];
  if (arg === '--out-dir') {
    outDir = path.resolve(args[++i]);
  } else if (arg === '--allow-not-ready') {
    allowNotReady = true;
  } else {
    console.error(`voplay industrial readiness: unknown argument ${arg}`);
    process.exit(2);
  }
}

const checks = [];
const gateReports = [];
const evidenceTable = [];
const sourceFactRequirements = [];

function addCheck(phase, code, status, detail, evidence = {}) {
  checks.push({
    phase,
    code,
    status: status ? 'pass' : 'fail',
    severity: status ? null : 'P0',
    detail,
    evidence,
  });
}

function addRequiredSourceFact(code, status, detail, evidence = {}) {
  const fact = {
    code,
    required: true,
    status: Boolean(status),
    detail,
    evidence,
  };
  sourceFactRequirements.push(fact);
  addCheck('source-audit', `source_fact.${code}`, fact.status, detail, evidence);
}

function addEvidenceRow(row) {
  evidenceTable.push({
    capability: row.capability,
    expectedOwner: row.expectedOwner,
    actualOwner: row.actualOwner,
    mutationPath: row.mutationPath,
    runtimePath: row.runtimePath,
    bypassFound: row.bypassFound,
    evidence: row.evidence,
    gateCoverage: row.gateCoverage,
    status: row.status ? 'pass' : 'fail',
    nextFix: row.nextFix,
  });
}

function readText(file) {
  if (!existsSync(file)) {
    return null;
  }
  return readFileSync(file, 'utf8');
}

function readProjectFile(projectRoot, relativePath, phase, code) {
  const file = path.join(projectRoot, relativePath);
  const source = readText(file);
  addCheck(phase, code, source !== null, `${relativePath} exists`, { path: file });
  return source || '';
}

function listVoFiles(projectRoot) {
  const files = [];
  const visit = (dir) => {
    for (const entry of readdirSync(dir)) {
      const file = path.join(dir, entry);
      const stat = statSync(file);
      if (stat.isDirectory()) {
        if (entry === '.git' || entry === 'target' || entry === 'node_modules') {
          continue;
        }
        visit(file);
        continue;
      }
      if (file.endsWith('.vo')) {
        files.push({
          file,
          rel: path.relative(projectRoot, file),
          source: readFileSync(file, 'utf8'),
        });
      }
    }
  };
  visit(projectRoot);
  return files;
}

function readJson(file) {
  const source = readText(file);
  if (source === null) {
    return { exists: false, value: null, error: null };
  }
  try {
    return { exists: true, value: JSON.parse(source), error: null };
  } catch (error) {
    return { exists: true, value: null, error: String(error?.message || error) };
  }
}

function clipOutput(text, limit = 6000) {
  const source = String(text || '').trim();
  return source.length > limit ? `${source.slice(0, limit)}...` : source;
}

function runGateScript(relativePath) {
  const script = path.join(root, relativePath);
  const result = spawnSync(process.execPath, [script], {
    cwd: root,
    env: process.env,
    encoding: 'utf8',
    maxBuffer: 20 * 1024 * 1024,
  });
  return {
    status: result.status,
    signal: result.signal,
    stdout: clipOutput(result.stdout),
    stderr: clipOutput(result.stderr),
    error: result.error ? String(result.error.message || result.error) : null,
  };
}

function lineCount(source) {
  if (source.length === 0) {
    return 0;
  }
  return source.split(/\r?\n/).length;
}

function lineOf(source, token) {
  const lines = source.split(/\r?\n/);
  for (let i = 0; i < lines.length; i++) {
    if (lines[i].includes(token)) {
      return i + 1;
    }
  }
  return null;
}

function lineEvidence(file, source, token) {
  const line = lineOf(source, token);
  return {
    path: file,
    token,
    line,
    ref: line === null ? file : `${file}:${line}`,
  };
}

function countOccurrences(source, token) {
  if (!token) {
    return 0;
  }
  return source.split(token).length - 1;
}

function hasAny(source, tokens) {
  return tokens.some((token) => source.includes(token));
}

function evidenceForFirstToken(file, source, tokens) {
  for (const token of tokens) {
    if (source.includes(token)) {
      return lineEvidence(file, source, token);
    }
  }
  return lineEvidence(file, source, tokens[0] || '');
}

function evidenceForFirstRegex(entries, regex, fallbackToken) {
  for (const entry of entries) {
    const lines = entry.source.split(/\r?\n/);
    for (let i = 0; i < lines.length; i++) {
      const match = lines[i].match(regex);
      if (match) {
        return {
          path: entry.file,
          token: match[0],
          line: i + 1,
          ref: `${entry.file}:${i + 1}`,
        };
      }
    }
  }
  const first = entries[0];
  return {
    path: first?.file || blockKartRoot,
    token: fallbackToken,
    line: null,
    ref: first?.file || blockKartRoot,
  };
}

function sourceRegexHits(entries, regex) {
  const hits = [];
  for (const entry of entries) {
    const lines = entry.source.split(/\r?\n/);
    for (let i = 0; i < lines.length; i++) {
      if (regex.test(lines[i])) {
        hits.push({ path: entry.file || entry.path || entry.rel, line: i + 1, text: lines[i].trim() });
      }
    }
  }
  return hits;
}

function bodyContains(source, signature, tokens) {
  const body = bodyOfFunction(source, signature);
  return tokens.some((token) => body.includes(token));
}

function constructsRuntimeStage(source, token) {
  return source.split(/\r?\n/).some((line) => {
    if (line.includes(`struct ${token}`)) {
      return false;
    }
    return line.includes(`${token} {`) || line.includes(`${token}::default(`);
  });
}

function bodyOfFunction(source, signature) {
  const start = source.indexOf(signature);
  if (start < 0) {
    return '';
  }
  const brace = source.indexOf('{', start);
  if (brace < 0) {
    return '';
  }
  let depth = 0;
  for (let i = brace; i < source.length; i++) {
    if (source[i] === '{') {
      depth++;
    } else if (source[i] === '}') {
      depth--;
      if (depth === 0) {
        return source.slice(brace + 1, i);
      }
    }
  }
  return '';
}

function listFiles(projectRoot, extension) {
  const files = [];
  const visit = (dir) => {
    if (!existsSync(dir)) {
      return;
    }
    for (const entry of readdirSync(dir)) {
      const file = path.join(dir, entry);
      const stat = statSync(file);
      if (stat.isDirectory()) {
        if (entry === '.git' || entry === 'target' || entry === 'node_modules') {
          continue;
        }
        visit(file);
      } else if (file.endsWith(extension)) {
        files.push(file);
      }
    }
  };
  visit(projectRoot);
  return files;
}

function resolveGitDir(projectRoot) {
  const gitPath = path.join(projectRoot, '.git');
  if (!existsSync(gitPath)) {
    return null;
  }
  const stat = statSync(gitPath);
  if (stat.isDirectory()) {
    return gitPath;
  }
  const content = readText(gitPath) || '';
  const match = content.match(/^gitdir:\s*(.+)\s*$/m);
  if (!match) {
    return null;
  }
  return path.resolve(projectRoot, match[1]);
}

function gitCommit(projectRoot) {
  const gitDir = resolveGitDir(projectRoot);
  if (!gitDir) {
    return null;
  }
  const head = readText(path.join(gitDir, 'HEAD'))?.trim();
  if (!head) {
    return null;
  }
  if (!head.startsWith('ref: ')) {
    return head;
  }
  const ref = head.slice('ref: '.length).trim();
  const looseRef = readText(path.join(gitDir, ref))?.trim();
  if (looseRef) {
    return looseRef;
  }
  const packedRefs = readText(path.join(gitDir, 'packed-refs')) || '';
  for (const line of packedRefs.split(/\r?\n/)) {
    if (line.startsWith('#') || line.startsWith('^')) {
      continue;
    }
    const [sha, name] = line.trim().split(/\s+/);
    if (name === ref) {
      return sha;
    }
  }
  return null;
}

function freshEvidenceIssues(report, expectedGate) {
  const evidence = report?.freshEvidence;
  const issues = [];
  if (!evidence || typeof evidence !== 'object') {
    return ['missing freshEvidence'];
  }
  if (expectedGate && evidence.gate !== expectedGate) {
    issues.push(`gate ${evidence.gate ?? '(missing)'} did not match ${expectedGate}`);
  }
  for (const [field, prefix] of [
    ['taskRunId', expectedGate ? `${expectedGate}:` : ''],
    ['sourceDigest', 'sha256:'],
    ['gateDigest', 'sha256:'],
    ['artifactDigest', 'sha256:'],
  ]) {
    if (typeof evidence[field] !== 'string' || evidence[field].length === 0) {
      issues.push(`${field} missing`);
    } else if (prefix && !evidence[field].startsWith(prefix)) {
      issues.push(`${field} has unexpected prefix`);
    }
  }
  if (typeof evidence.generatedAt !== 'string' || Number.isNaN(Date.parse(evidence.generatedAt))) {
    issues.push('generatedAt missing or invalid');
  }
  if (!evidence.testedCommits || typeof evidence.testedCommits !== 'object' || Object.keys(evidence.testedCommits).length === 0) {
    issues.push('testedCommits missing');
  }
  if (!evidence.dirtyFlags || typeof evidence.dirtyFlags !== 'object' || Object.keys(evidence.dirtyFlags).length === 0) {
    issues.push('dirtyFlags missing');
  }
  return issues;
}

function checkReport(file, phase, code, validate, expectedFreshGate = null) {
  const result = readJson(file);
  const report = {
    path: file,
    exists: result.exists,
    parseError: result.error,
    status: result.value?.status ?? null,
    kind: result.value?.kind ?? null,
    freshEvidence: result.value?.freshEvidence ?? null,
  };
  gateReports.push(report);
  if (!result.exists) {
    addCheck(phase, code, false, `${path.relative(root, file)} was not produced`, report);
    return null;
  }
  if (result.error) {
    addCheck(phase, code, false, `${path.relative(root, file)} is not valid JSON`, report);
    return null;
  }
  const verdict = validate(result.value);
  const freshnessIssues = expectedFreshGate ? freshEvidenceIssues(result.value, expectedFreshGate) : [];
  addCheck(phase, code, verdict.ok && freshnessIssues.length === 0, verdict.detail, { ...report, ...verdict.evidence, freshnessIssues });
  return result.value;
}

function sceneNames(report) {
  return new Set((report?.scenes || []).map((scene) => scene.name));
}

function allScenesPass(report) {
  return (report?.scenes || []).every((scene) => scene.status === 'pass');
}

function scenarioNames(report) {
  return new Set((report?.scenarios || []).map((scenario) => scenario.name));
}

function allIndustrialPhysicsSamplesClean(report) {
  return (report?.scenarios || []).every((scenario) => (
    scenario.status === 'pass'
    && Number(scenario.fallbackContactEvents || 0) === 0
    && Number(scenario.invalidSamples || 0) === 0
    && !scenario.issues
  ));
}

function scanScriptIndustrialClaims() {
  const offenders = [];
  for (const file of listFiles(path.join(root, 'scripts', 'ci'), '.mjs')) {
    if (path.basename(file) === 'voplay_industrial_readiness.mjs') {
      continue;
    }
    const source = readText(file) || '';
    if (/industrialReady\s*:\s*true|industrial ready/i.test(source)) {
      offenders.push(path.relative(root, file));
    }
  }
  return offenders;
}

const renderer = readProjectFile(voplayRoot, 'rust/src/renderer.rs', 'phase-0', 'source.renderer_exists');
const rendererFrameSubmit = readProjectFile(voplayRoot, 'rust/src/renderer/frame_submit.rs', 'phase-0', 'source.renderer_frame_submit_exists');
const rendererFrameDecode = readProjectFile(voplayRoot, 'rust/src/renderer/frame_decode.rs', 'phase-0', 'source.renderer_frame_decode_exists');
const rendererFrameOrchestrator = readProjectFile(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs', 'phase-0', 'source.renderer_frame_orchestrator_exists');
const rendererFrameWorkloadPlan = readProjectFile(voplayRoot, 'rust/src/renderer/frame_workload_plan.rs', 'phase-0', 'source.renderer_frame_workload_plan_exists');
const rendererFramePassSequence = readProjectFile(voplayRoot, 'rust/src/renderer/frame_pass_sequence.rs', 'phase-0', 'source.renderer_frame_pass_sequence_exists');
const rendererPassDispatch = readProjectFile(voplayRoot, 'rust/src/renderer/pass_dispatch.rs', 'phase-0', 'source.renderer_pass_dispatch_exists');
const rendererFramePerfFinalize = readProjectFile(voplayRoot, 'rust/src/renderer/frame_perf_finalize.rs', 'phase-0', 'source.renderer_frame_perf_finalize_exists');
const rendererFrameOrchestratorRuntime = readText(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator_runtime.rs')) || '';
const frameGraph = readProjectFile(voplayRoot, 'rust/src/renderer_frame.rs', 'phase-0', 'source.framegraph_exists');
const renderWorld = readProjectFile(voplayRoot, 'rust/src/render_world.rs', 'phase-0', 'source.render_world_exists');
const renderWorldAuditSource = [
  renderWorld,
  readText(path.join(voplayRoot, 'rust/src/render_world/store.rs')) || '',
  readText(path.join(voplayRoot, 'rust/src/render_world/tests.rs')) || '',
].join('\n');
const primitivePipeline = readProjectFile(voplayRoot, 'rust/src/primitive_pipeline.rs', 'phase-0', 'source.primitive_pipeline_exists');
const pipeline3d = readProjectFile(voplayRoot, 'rust/src/pipeline3d.rs', 'phase-0', 'source.pipeline3d_exists');
const pipelineCache = readProjectFile(voplayRoot, 'rust/src/pipeline3d/pipeline_cache.rs', 'phase-0', 'source.pipeline_cache_exists');
const pipeline3dOwnerSource = listFiles(path.join(voplayRoot, 'rust/src/pipeline3d'), '.rs')
  .map((file) => readText(file) || '')
  .join('\n');
const vehicle = readProjectFile(voplayRoot, 'scene3d/vehicle.vo', 'phase-0', 'source.vehicle_exists');
const dynamics = readProjectFile(voplayRoot, 'scene3d/kart_dynamics.vo', 'phase-0', 'source.dynamics_exists');
const physics = readProjectFile(voplayRoot, 'scene3d/physics.vo', 'phase-0', 'source.physics_exists');
const contactEvent = readProjectFile(voplayRoot, 'scene3d/contact_event.vo', 'phase-0', 'source.contact_event_exists');
const replay = readProjectFile(voplayRoot, 'scene3d/replay.vo', 'phase-0', 'source.replay_exists');
const vehiclePhysicsSession = readProjectFile(voplayRoot, 'scene3d/vehicle_physics_session.vo', 'phase-0', 'source.vehicle_physics_session_exists');
const physicsStressSource = readProjectFile(voplayRoot, 'examples/physics_stress/main.vo', 'phase-0', 'source.physics_stress_exists');
const sceneSource = readProjectFile(voplayRoot, 'scene3d/scene.vo', 'phase-0', 'source.scene_exists');
const vehicleTelemetry = readProjectFile(voplayRoot, 'scene3d/vehicle_telemetry.vo', 'phase-0', 'source.vehicle_telemetry_exists');
const blockKartWorld = readProjectFile(blockKartRoot, 'world.vo', 'phase-0', 'source.blockkart_world_exists');
const blockKartPrimitiveWorld = readProjectFile(blockKartRoot, 'primitive_world.vo', 'phase-0', 'source.blockkart_primitive_world_exists');
const blockKartTrackRuntime = readProjectFile(blockKartRoot, 'track_runtime.vo', 'phase-0', 'source.blockkart_track_runtime_exists');
const blockKartBudget = readProjectFile(blockKartRoot, 'performance_budget.vo', 'phase-0', 'source.blockkart_budget_exists');
const blockKartVoFiles = listVoFiles(blockKartRoot);
const rendererRuntime = [renderer, rendererFrameSubmit, rendererFrameOrchestrator, rendererFrameOrchestratorRuntime].join('\n');
const rendererModuleRuntime = listFiles(path.join(voplayRoot, 'rust', 'src', 'renderer'), '.rs')
  .map((file) => readText(file) || '')
  .join('\n');
const rendererAuditSource = [renderer, rendererFrameSubmit, rendererFrameOrchestrator, rendererModuleRuntime, frameGraph].join('\n');

const submitFrameBody = bodyOfFunction(renderer, 'pub fn submit_frame');
const setPoseBody = bodyOfFunction(vehicle, 'func (v *Vehicle) SetPose');
const applyPoseResetBody = bodyOfFunction(vehicle, 'func (v *Vehicle) applyPoseResetToBackend');
const renderBatchPlannerBuildBody = bodyOfFunction(renderWorldAuditSource, 'pub fn build');
const renderBatchPlannerSelectLodBody = bodyOfFunction(renderWorldAuditSource, 'fn select_lod');
const rendererLines = lineCount(renderer);
const frameSubmitLines = lineCount(rendererFrameSubmit);
const frameOrchestratorLines = lineCount(rendererFrameOrchestrator);
const submitFrameLines = lineCount(submitFrameBody);
const pipeline3dLines = lineCount(pipeline3d);
const fileBudgetEntries = {
  'rust/src/renderer/frame_orchestrator.rs': {
    lines: lineCount(rendererFrameOrchestrator),
    budget: 300,
  },
  'rust/src/renderer/frame_decode.rs': {
    lines: lineCount(rendererFrameDecode),
    budget: 700,
  },
  'rust/src/renderer_frame.rs': {
    lines: lineCount(frameGraph),
    budget: 900,
  },
  'rust/src/render_world.rs': {
    lines: lineCount(renderWorld),
    budget: 700,
  },
  'rust/src/primitive_pipeline.rs': {
    lines: lineCount(primitivePipeline),
    budget: 700,
  },
  'rust/src/pipeline3d/pipeline_cache.rs': {
    lines: lineCount(pipelineCache),
    budget: 500,
  },
};
const fileBudgets = Object.fromEntries(
  Object.entries(fileBudgetEntries).map(([file, entry]) => [
    file,
    {
      ...entry,
      status: entry.lines <= entry.budget ? 'pass' : 'fail',
      overBy: Math.max(0, entry.lines - entry.budget),
    },
  ]),
);
const fileBudgetFailures = Object.entries(fileBudgets)
  .filter(([, entry]) => entry.status !== 'pass')
  .map(([file, entry]) => ({ file, ...entry }));
function ownerModuleFacts(relativePath, productionSource) {
  const source = readText(path.join(voplayRoot, relativePath)) || '';
  const base = path.basename(relativePath, '.rs');
  const owner = base
    .split('_')
    .map((part) => `${part.slice(0, 1).toUpperCase()}${part.slice(1)}`)
    .join('');
  const hasProductionMethod = /\bpub(?:\(crate\)|\(super\))?\s+fn\s+(prepare|upload|draw|bind|submit|load|compile|create|resolve|cache)\w*\b/.test(source)
    || /\bfn\s+(prepare|upload|draw|bind|submit|load|compile|create|resolve|cache)\w*\b/.test(source);
  const calledByProduction = productionSource.includes(`${owner}::`)
    || productionSource.includes(`${owner} {`)
    || productionSource.includes(`${base}::`);
  return {
    path: relativePath,
    owner,
    lines: lineCount(source),
    hasProductionMethod,
    calledByProduction,
    status: hasProductionMethod && calledByProduction ? 'pass' : 'fail',
  };
}
const pipelineOwnerFiles = [
  'rust/src/pipeline3d/shader_library.rs',
  'rust/src/pipeline3d/material_binder.rs',
  'rust/src/pipeline3d/mesh_submitter.rs',
  'rust/src/pipeline3d/skinned_submitter.rs',
  'rust/src/pipeline3d/terrain_submitter.rs',
  'rust/src/pipeline3d/primitive_submitter.rs',
  'rust/src/pipeline3d/water_submitter.rs',
  'rust/src/pipeline3d/decal_submitter.rs',
];
const pipelineOwnerFacts = pipelineOwnerFiles.map((file) => ownerModuleFacts(file, pipeline3d + '\n' + pipelineCache + '\n' + pipeline3dOwnerSource + '\n' + rendererModuleRuntime));
const emptyOwnerModules = pipelineOwnerFacts.filter((entry) => entry.status !== 'pass');
const renderPassBodyTokens = ['begin_render_pass'];
const directDrawTokens = ['.draw_models(', 'draw_main_and_water('];
const runtimeHasRenderPassBody = renderPassBodyTokens.some((token) => rendererRuntime.includes(token));
const runtimeHasDirectDraw = directDrawTokens.some((token) => rendererRuntime.includes(token));
const runtimeHasQueueSubmit = rendererRuntime.includes('queue.submit');
const runtimeUsesExecuteRenderNodeMacro = rendererRuntime.includes('execute_render_node!');
const rendererStillOwnsTargets =
  renderer.includes('targets: RendererTargetRegistry')
  || /self\.targets\.\w+\s*=/.test(rendererRuntime)
  || rendererRuntime.includes('create_depth_view(')
  || rendererRuntime.includes('create_post_color_view(')
  || rendererRuntime.includes('create_receiver_mask_view(');
const stageTokens = [
  'RenderFrameDecode',
  'RenderSceneSnapshot',
  'FrameGraphBuild',
  'FrameGraphExecute',
  'PerfPacketEncode',
  'RenderFramePipeline',
];
const constructedPipelineStages = Object.fromEntries(
  stageTokens.map((token) => [token, constructsRuntimeStage(rendererAuditSource, token)]),
);
const allPipelineStagesConstructed = Object.values(constructedPipelineStages).every(Boolean);
const renderFramePipelineRuntimeCall = rendererFramePerfFinalize.includes('RenderFramePipeline::from_frame_metrics')
  && rendererFrameOrchestrator.includes('self.finalize_frame_perf(FramePerfFinalizeContext')
  && renderer.includes('last_frame_pipeline');
const renderPipelineStagesPass = allPipelineStagesConstructed && renderFramePipelineRuntimeCall;
const batchPlanHasZeroBounds = /bounds:\s*RenderChunkBounds\s*\{[\s\S]*?center:\s*Vec3::ZERO[\s\S]*?radius:\s*0\.0/.test(renderBatchPlannerBuildBody);
const batchPlanUsesSeedWorkloadLod = renderBatchPlannerSelectLodBody.includes('seed')
  || renderBatchPlannerSelectLodBody.includes('workload')
  || /select_lod\([^)]*seed[^)]*workload/.test(renderWorld);
const frustumCullingCounterMutated = /frustum_culled_chunks\s*(?:\+=|=)/.test(renderBatchPlannerBuildBody);
const distanceCullingCounterMutated = /distance_culled_chunks\s*(?:\+=|=)/.test(renderBatchPlannerBuildBody);
const batchPlanContractPass = !batchPlanHasZeroBounds
  && !batchPlanUsesSeedWorkloadLod
  && frustumCullingCounterMutated
  && distanceCullingCounterMutated;
const physicsTrackPositionSurfaceInference = [
  vehicle,
  contactEvent,
  replay,
  readText(path.join(voplayRoot, 'scene3d/vehicle_telemetry.vo')) || '',
].some((source) => source.includes('SurfaceMaterialAtTrackPosition'));
const setPoseDirectPhysicsMutation = [
  'Body.SetPosition(',
  'Body.SetRotation(',
  'Body.SetVelocity(',
  'Body.SetAngularVelocity(',
  'Body.Physics.velocity',
  'Body.Physics.angularVelocity',
].some((token) => setPoseBody.includes(token));
const directBodyMutationTokens = [
  'Body.SetPosition(',
  'Body.SetRotation(',
  'Body.SetVelocity(',
  'Body.SetAngularVelocity(',
  'Body.Physics.velocity',
  'Body.Physics.angularVelocity',
];
const poseResetHelperDirectPhysicsMutation = hasAny(applyPoseResetBody, directBodyMutationTokens);
const replayRecordsBackendApplyHash = replay.includes('BackendApplyHash') || replay.includes('BackendApplyCommandHash');
const physicsBackendContractPass = !physicsTrackPositionSurfaceInference
  && !setPoseDirectPhysicsMutation
  && !poseResetHelperDirectPhysicsMutation
  && replayRecordsBackendApplyHash;
const blockKartPrimitiveAuthoringPresent = /type BlockKartPrimitiveScene|primitive3d\.NewLayer|primitive3d\.NewBuilder|primitive3d\.LayerDesc|primitive3d\.ChunkingDesc|SpawnPreparedMapPrimitiveLayers|AddStatic|AddDetail|spawnPrimitiveRoadBoxPhysics|BlockKartVisualContent|spawnPrimitiveTrackPhysics|spawnRoadColliderStrip/.test(blockKartPrimitiveWorld);
const blockKartHudAssemblesLowLevelFacts = /PrimitiveStats\(|WheelState\(|VehicleGrounded|WheelMaxSlip|PrimitiveVisibleChunks/.test(blockKartWorld);
const blockKartVisualReadsMutableVehicleState = /w\.vehicle\.(SteerAngle|WheelSpin)/.test(blockKartPrimitiveWorld);
const blockKartDirectVehiclePoseCalls = /w\.vehicle\.SetPose\(/.test(blockKartWorld);
const blockKartDirectPlayerPhysicsMutation = /w\.player\.SetPosition\(|w\.player\.SetRotation\(|w\.player\.SetVelocity\(|w\.player\.SetAngularVelocity\(/.test(blockKartWorld);
const directEntityMutationRegex = /\b[A-Za-z_][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)*\.Set(Position|Rotation|Velocity|AngularVelocity)\(/;
const blockKartDirectEntityPhysicsMutation = blockKartVoFiles.some((entry) => directEntityMutationRegex.test(entry.source));
const resourceRegistryOwnsAllTargetKinds = [
  'depth_view',
  'msaa_color_view',
  'post_color_view',
  'receiver_mask_view',
  'surface_props_view',
  'shadow',
  'water',
  'overlay',
  'capture',
  'readback',
].every((token) => frameGraph.includes(token));
const executeNodeTakesAdHocClosure = /fn execute_node[\s\S]*?execute:\s*F[\s\S]*?F:\s*FnOnce/.test(frameGraph);
const executeNodeOwnsPassDispatch = frameGraph.includes('fn execute_node') && !executeNodeTakesAdHocClosure;
const batchPlanSceneWired = rendererFrameWorkloadPlan.includes('RenderBatchPlanner::build')
  && rendererFrameWorkloadPlan.includes('planned_model_draws')
  && rendererFrameWorkloadPlan.includes('planned_primitive_draws')
  && rendererFrameWorkloadPlan.includes('planned_primitive_chunks')
  && rendererFrameWorkloadPlan.includes('planned_water_draws')
  && rendererFrameWorkloadPlan.includes('planned_water_chunks')
  && rendererFrameWorkloadPlan.includes('terrain_batch_inputs')
  && rendererFrameWorkloadPlan.includes('decal_batch_inputs')
  && rendererFrameWorkloadPlan.includes('planned_projected_decals')
  && rendererFrameOrchestrator.includes('projected_decals: &planned_projected_decals')
  && rendererFramePerfFinalize.includes('perf.visible_objects = context.render_batch_plan.visible_objects');
const terrainBatchEnumMentions = countOccurrences(renderWorldAuditSource, 'RenderBatchKind::Terrain');
const decalBatchEnumMentions = countOccurrences(renderWorldAuditSource, 'RenderBatchKind::Decal');
const terrainBatchConstructed = /kind:\s*RenderBatchKind::Terrain/.test(renderWorldAuditSource);
const decalBatchConstructed = /kind:\s*RenderBatchKind::Decal/.test(renderWorldAuditSource);
const terrainBatchTested = /terrain/i.test(renderWorldAuditSource) && /#\[test\][\s\S]*terrain/i.test(renderWorldAuditSource);
const decalBatchTested = /decal/i.test(renderWorldAuditSource) && /#\[test\][\s\S]*decal/i.test(renderWorldAuditSource);
const terrainDecalRealEntries = terrainBatchConstructed && decalBatchConstructed && terrainBatchTested && decalBatchTested;
const batchPlanningIndustrialPass = batchPlanContractPass && batchPlanSceneWired && terrainDecalRealEntries;
const blockKartProductBoundaryPass = !blockKartPrimitiveAuthoringPresent
  && !blockKartHudAssemblesLowLevelFacts
  && !blockKartVisualReadsMutableVehicleState
  && !blockKartDirectVehiclePoseCalls
  && !blockKartDirectEntityPhysicsMutation;
const renderPassModuleFiles = [
  'rust/src/renderer/depth_pass.rs',
  'rust/src/renderer/shadow_pass.rs',
  'rust/src/renderer/main_opaque_pass.rs',
  'rust/src/renderer/main_transparent_pass.rs',
  'rust/src/renderer/water_pass.rs',
  'rust/src/renderer/post_pass.rs',
  'rust/src/renderer/overlay_pass.rs',
  'rust/src/renderer/backend_submit_pass.rs',
];
const renderPassModuleEntries = renderPassModuleFiles.map((relativePath) => ({
  path: relativePath,
  source: readText(path.join(voplayRoot, relativePath)) || '',
}));
const frameGraphManualSequenceHits = [
  ...(rendererFramePassSequence.includes('FrameGraphPlanNodes') ? [{
    path: 'rust/src/renderer/frame_pass_sequence.rs',
    token: 'FrameGraphPlanNodes',
    line: lineOf(rendererFramePassSequence, 'FrameGraphPlanNodes'),
  }] : []),
  ...sourceRegexHits([{ path: 'rust/src/renderer/frame_pass_sequence.rs', source: rendererFramePassSequence }], /\.execute_node\(&context\.nodes\./),
  ...(rendererFrameWorkloadPlan.includes('FrameGraphPlanNodes') ? [{
    path: 'rust/src/renderer/frame_workload_plan.rs',
    token: 'FrameGraphPlanNodes',
    line: lineOf(rendererFrameWorkloadPlan, 'FrameGraphPlanNodes'),
  }] : []),
  ...(readText(path.join(voplayRoot, 'rust/src/renderer/frame_graph_plan.rs'))?.includes('struct FrameGraphPlanNodes') ? [{
    path: 'rust/src/renderer/frame_graph_plan.rs',
    token: 'struct FrameGraphPlanNodes',
    line: lineOf(readText(path.join(voplayRoot, 'rust/src/renderer/frame_graph_plan.rs')) || '', 'struct FrameGraphPlanNodes'),
  }] : []),
];
const frameGraphHasExecuteAll = /\bfn\s+execute_all\b/.test(frameGraph);
const rendererRuntimeUsesExecuteAll = /\bexecute_all\s*\(/.test(rendererRuntime) || /\bexecute_all\s*\(/.test(rendererFramePassSequence);
const dispatcherRendererHits = sourceRegexHits(
  [
    { path: 'rust/src/renderer/pass_dispatch.rs', source: rendererPassDispatch },
    ...renderPassModuleEntries,
  ],
  /renderer:\s*&'?[_A-Za-z0-9]*\s*mut\s+Renderer\b/,
);
const renderHotPathPanicHits = sourceRegexHits(
  [
    { path: 'rust/src/renderer_frame.rs', source: frameGraph },
    { path: 'rust/src/render_world.rs', source: renderWorld },
    { path: 'rust/src/primitive_pipeline.rs', source: primitivePipeline },
    ...renderPassModuleEntries,
  ],
  /\b(panic!|expect\s*\(|unwrap\s*\(|todo!|unimplemented!)/,
);
const invalidBatchIndexSilentSkipHits = sourceRegexHits(
  [{ path: 'rust/src/render_world.rs', source: renderWorld }],
  /\.filter_map\(\|[^|]*(?:index|draw_index|chunk_index)[^|]*\|[\s\S]*?\.get\(/,
);
const sessionStepBody = bodyOfFunction(vehiclePhysicsSession, 'func (s *VehiclePhysicsSession) Step');
const sessionIsWrapperOnly = /FixedPhysicsStep\s*\(|StepAndSyncPhysics\s*\(/.test(sessionStepBody)
  || !/s\.(?:StepIndex|Telemetry|Replay|Backend|LastPacket|InvalidSample|Controller|Dynamics)/.test(sessionStepBody);
const physicsStressBypassesSession = /UpdateIntent\s*\(/.test(physicsStressSource)
  || /StepAndSyncPhysics\s*\(/.test(physicsStressSource);
const backendPacketSchemaTokens = [
  'PhysicsBackendPacketSchemaVersion',
  'PhysicsBackendPacketKind',
  'PhysicsBackendPacketLength',
  'PhysicsBackendPacketHash',
  'PhysicsBackendCapability',
];
const backendPacketContractSource = [sceneSource, physics, dynamics, replay].join('\n');
const backendPacketMissingTokens = backendPacketSchemaTokens.filter((token) => !backendPacketContractSource.includes(token));
const invalidTelemetryStressBypass = !physicsStressSource.includes('InvalidSampleCount')
  || !physicsStressSource.includes('ValidationIssues')
  || /sampleFinite\s*\(/.test(physicsStressSource);
const executableReplayTokens = [
  'PhysicsReplayVerifier',
  'StepHash',
  'BackendPacketHash',
  'ReplayMismatch',
  'ValidatePhysicsReplayTrace',
  'PhysicsPoseHash',
  'PhysicsTelemetryHash',
  'Mismatches',
];
const executableReplayMissingTokens = executableReplayTokens.filter((token) => !replay.includes(token) && !physicsStressSource.includes(token));
const replayUsesRecordedTrace = /runScenarioWithReplay\s*\([^)]*true/.test(physicsStressSource)
  && /for[^\n]*sample[^\n]*range\s+trace\.Samples/.test(physicsStressSource)
  && /sample\.BackendApplyHash/.test(physicsStressSource)
  && /sample\.PoseHash/.test(physicsStressSource)
  && /sample\.TelemetryHash/.test(physicsStressSource);
const sameRuntimeReplayDriftOnly = /replayDrift|driftMeters|ReplayDrift/.test(physicsStressSource)
  || !replayUsesRecordedTrace;
const blockKartGenericAuthoringHits = sourceRegexHits(
  blockKartVoFiles.map((entry) => ({ path: entry.rel, source: entry.source })),
  /ProductPrimitiveAuthoring|PrimitiveBatchAuthoring|PrimitiveInstanceAuthoring|SurfaceMaterialAuthoring|ColliderAuthoring|ProductPrimitive(Place|Dynamic|SetPose)|ProductPrimitives\.(Place|Dynamic|BuildLayer|Material|Shape|Layer)|primitive3d\.(NewLayer|NewBuilder|LayerDesc|ChunkingDesc|MaterialDesc|ShapeDesc|MaterialPreset)|PrepareMapWithAssets|SpawnPreparedMap|ProductSpawnTrackColliderStrip|PackWriter|vopack\./,
);

addCheck('phase-0', 'report.phase_claims_guarded', scanScriptIndustrialClaims().length === 0, 'phase scripts do not claim industrial readiness', { offenders: scanScriptIndustrialClaims() });
addCheck('phase-1', 'renderer.submit_frame_size', submitFrameLines <= 220, 'submit_frame is only an orchestration body', { lines: submitFrameLines, budget: 220 });
addCheck('phase-1', 'renderer.file_size', rendererLines <= 1600, 'renderer.rs is reduced to orchestration and backend glue', { lines: rendererLines, budget: 1600 });
addCheck('phase-1', 'renderer.frame_submit_size', frameSubmitLines <= 700, 'frame_submit.rs is reduced to frame orchestration and pass registration glue', { lines: frameSubmitLines, budget: 700 });
for (const [file, budget] of Object.entries(fileBudgets)) {
  addCheck('source-audit', `file_budget.${file}`, budget.status === 'pass', `${file} stays within ownership file budget`, budget);
}
addCheck('source-audit', 'code_ownership.owner_modules_non_empty', emptyOwnerModules.length === 0, 'pipeline3d owner modules have production responsibility and production call paths', { emptyOwnerModules, pipelineOwnerFacts });
addCheck('phase-1', 'renderer.frame_orchestrator_no_pass_body', !rendererFrameOrchestrator.includes('begin_render_pass') && !rendererFrameOrchestrator.includes('queue.submit') && !rendererFrameOrchestrator.includes('.draw_models(') && !rendererFrameOrchestrator.includes('draw_main_and_water('), 'frame_orchestrator.rs contains orchestration only and no pass bodies', { lines: frameOrchestratorLines, forbidden: ['begin_render_pass', 'queue.submit', 'draw_models', 'draw_main_and_water'] });
addCheck('phase-1', 'renderer.no_begin_render_pass_in_submit_frame', !submitFrameBody.includes('begin_render_pass'), 'submit_frame contains no render pass bodies', { tokens: ['begin_render_pass'] });
addCheck('phase-1', 'renderer.no_begin_render_pass_in_runtime_glue', !runtimeHasRenderPassBody, 'renderer runtime glue contains no render pass bodies', { tokens: renderPassBodyTokens });
addCheck('phase-1', 'renderer.no_direct_draw_in_submit_frame', !/\.draw_models\(|draw_main_and_water\(|\.draw\(/.test(submitFrameBody), 'submit_frame contains no direct draw calls', { tokens: ['draw_models', 'draw_main_and_water'] });
addCheck('phase-1', 'renderer.no_direct_draw_in_runtime_glue', !runtimeHasDirectDraw, 'renderer runtime glue contains no direct draw calls', { tokens: directDrawTokens });
addCheck('phase-1', 'renderer.no_queue_submit_body', !submitFrameBody.includes('queue.submit'), 'backend submission is outside submit_frame pass body', { tokens: ['queue.submit'] });
addCheck('phase-1', 'renderer.no_queue_submit_in_runtime_glue', !runtimeHasQueueSubmit, 'renderer runtime glue delegates backend submission to a pass executor module', { tokens: ['queue.submit'] });
addCheck('phase-1', 'renderer.no_legacy_execute_pass', !rendererRuntime.includes('.execute_pass('), 'renderer runtime does not use legacy execute_pass', {});
addCheck('phase-1', 'renderer.framegraph_node_runtime', rendererFrameOrchestrator.includes('execute_frame_pass_sequence') && (rendererFramePassSequence.includes('execute_node(') || rendererFramePassSequence.includes('execute_all(')), 'renderer routes runtime pass work through FrameGraph node execution', {});
addCheck('phase-1', 'framegraph.pass_node_contract', ['RenderPassNode', 'RenderPassWorkload', 'execute_node', 'transient_writes'].every((token) => frameGraph.includes(token)), 'FrameGraph exposes pass node execution, workload, and transient write contracts', {});
addCheck('phase-1', 'framegraph.pass_modules_exist', renderPassModuleFiles.every((file) => existsSync(path.join(voplayRoot, file))), 'render pass implementations live in dedicated pass executor modules', { expected: renderPassModuleFiles });
addCheck('phase-1', 'framegraph.resource_registry_exists', frameGraph.includes('struct RenderResourceRegistry'), 'RenderResourceRegistry exists', {});
addCheck('phase-1', 'renderer.targets_owned_by_registry', !rendererStillOwnsTargets, 'renderer target lifecycle is owned by RenderResourceRegistry instead of ad hoc RendererTargetRegistry fields', {});

addCheck('phase-2', 'batch_plan.contract_exists', ['struct RenderBatchPlan', 'struct RenderBatchPlanner', 'struct RenderWorldChunk'].every((token) => renderWorld.includes(token)), 'RenderBatchPlan and RenderWorldChunk contracts exist', {});
addCheck('phase-2', 'batch_plan.drives_submission', !runtimeHasDirectDraw && rendererFrameWorkloadPlan.includes('RenderBatchPlanner::build') && rendererFrameWorkloadPlan.includes('planned_model_draws') && rendererFrameWorkloadPlan.includes('planned_primitive_draws') && rendererFrameWorkloadPlan.includes('planned_water_draws') && rendererFrameWorkloadPlan.includes('planned_projected_decals') && rendererFrameWorkloadPlan.includes('terrain_batch_inputs') && rendererFrameWorkloadPlan.includes('decal_batch_inputs'), 'RenderBatchPlan owns real draw submission routing', {});
addCheck('phase-2', 'pipeline3d.file_size', pipeline3dLines <= 900, 'pipeline3d.rs is split into small stable responsibilities', { lines: pipeline3dLines, budget: 900 });
const splitPipelineFiles = [
  'rust/src/pipeline3d/shader_library.rs',
  'rust/src/pipeline3d/pipeline_cache.rs',
  'rust/src/pipeline3d/material_binder.rs',
  'rust/src/pipeline3d/mesh_submitter.rs',
  'rust/src/pipeline3d/terrain_submitter.rs',
  'rust/src/pipeline3d/skinned_submitter.rs',
];
addCheck('phase-2', 'pipeline3d.split_modules_exist', splitPipelineFiles.every((file) => existsSync(path.join(voplayRoot, file))), 'pipeline3d responsibilities are split into dedicated modules', { expected: splitPipelineFiles });

const backendAdapterBody = bodyOfFunction(vehicle, 'func (v *Vehicle) applyForceCommandToBackend');
addCheck('phase-3', 'physics.intent_chain', vehicle.includes('v.Dynamics.Step(') && vehicle.includes('BuildPhysicsBackendApplyCommand') && vehicle.includes('physBackend.ApplyVehicleForces'), 'VehicleIntent reaches KartDynamics, PhysicsBackendApplyCommand, and PhysicsBackend.ApplyVehicleForces', {});
addCheck('phase-3', 'physics.backend_command_contract', ['Wheels []PhysicsBackendWheelCommand', 'BodyForce voplay.Vec3', 'DragForce float64', 'Downforce float64', 'WaterLift float64', 'AirControl float64', 'WallGrip float64', 'RailGrip float64', 'DebugHash int'].every((token) => dynamics.includes(token)), 'PhysicsBackendApplyCommand covers wheel, body, drag, downforce, water, air, wall, rail, and debug hash', {});
addCheck('phase-3', 'physics.backend_apply_contract', vehicle.includes('physBackend.ApplyVehicleForces') && !vehicle.includes('physBackend.SetRaycastVehicleWheelControl'), 'Vehicle applies wheel/backend forces only through PhysicsBackend.ApplyVehicleForces', {});
addCheck('phase-3', 'physics.reset_command_contract', ['type VehiclePoseResetCommand struct', 'type VehicleMotionResetCommand struct', 'type VehicleRecoveryCommand struct', 'type VehicleSleepCommand struct'].every((token) => vehicle.includes(token)), 'vehicle reset, motion, recovery, and sleep commands exist as backend contract types', {});
addCheck('phase-3', 'physics.no_body_position_surface_fallback', !vehicle.includes('SurfaceMaterialAtTrackPosition(v.Track, v.Body.Position())'), 'Vehicle surface selection does not infer material from body position when backend wheel contacts are absent', {});
addCheck('phase-3', 'physics.contact_industrial_contract', contactEvent.includes('IndustrialReady() bool') && contactEvent.includes('NormalImpulse') && contactEvent.includes('TangentImpulse'), 'ContactEvent exposes industrial impulse contract', {});
addCheck('phase-3', 'physics.replay_trace_contract', replay.includes('type PhysicsReplayTrace struct') && replay.includes('PoseHash') && replay.includes('TelemetryHash'), 'PhysicsReplayTrace records pose and telemetry hashes', {});

addCheck('phase-4', 'blockkart.primitive_authoring_migrated', !blockKartPrimitiveWorld.includes('type PrimitiveWorld') && !blockKartPrimitiveWorld.includes('BuildPrimitiveWorld'), 'BlockKart primitive authoring has moved to voplay authoring APIs', { path: path.join(blockKartRoot, 'primitive_world.vo') });
addCheck('phase-4', 'blockkart.perf_json_encoder_migrated', !blockKartBudget.includes('blockKartPerfReportJSON') && !blockKartBudget.includes('jsonString('), 'BlockKart performance reporting uses voplay structured telemetry encoder', { path: path.join(blockKartRoot, 'performance_budget.vo') });
addCheck('phase-4', 'blockkart.no_low_level_workarounds', !/SetRaycastVehicleWheelControl|ContactEventSourceBackendPairFallback|hostPacingOnly|resourcePacingWaiver|SurfaceAt\(w\.player\.Position\(\)\)/.test(blockKartWorld + blockKartBudget), 'BlockKart source contains no low-level render or physics workaround tokens', {});

addRequiredSourceFact(
  'render_pipeline_stages_constructed',
  allPipelineStagesConstructed && renderFramePipelineRuntimeCall,
  'all render frame pipeline stage structs are constructed and stored by runtime source',
  {
    constructedPipelineStages,
    renderFramePipelineRuntimeCall,
    runtimeCall: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_perf_finalize.rs'), rendererFramePerfFinalize, 'RenderFramePipeline::from_frame_metrics'),
    orchestratorCall: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'self.finalize_frame_perf(FramePerfFinalizeContext'),
    state: lineEvidence(path.join(voplayRoot, 'rust/src/renderer.rs'), renderer, 'last_frame_pipeline'),
    stageDefinitions: stageTokens.map((token) => lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, `struct ${token}`)),
  },
);
addRequiredSourceFact(
  'frame_orchestrator_stage_only',
  !rendererFrameOrchestrator.includes('let mut reader = StreamReader')
    && !rendererFrameOrchestrator.includes('PostUniform::from_settings')
    && !rendererFrameOrchestrator.includes('encode_renderer_perf_packet'),
  'frame_orchestrator.rs does not own decode, post setup, or perf packet encode details',
  {
    decodeStage: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_decode.rs'), rendererFrameDecode, 'pub(super) fn decode_frame_commands'),
    streamReaderOwner: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_decode.rs'), rendererFrameDecode, 'StreamReader::new'),
    streamReader: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'let mut reader = StreamReader'),
    postSetup: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'PostUniform::from_settings'),
    perfEncode: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'encode_renderer_perf_packet'),
  },
);
addRequiredSourceFact(
  'framegraph_dispatch_owns_pass_execution',
  executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro && !rendererRuntime.includes('.execute_pass('),
  'FrameGraphExecutor.execute_node owns pass dispatch without ad hoc runtime closures or legacy execute_pass',
  {
    executeNode: lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'fn execute_node'),
    macro: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'macro_rules! execute_render_node'),
    executeNodeTakesAdHocClosure,
    runtimeUsesExecuteRenderNodeMacro,
  },
);
addRequiredSourceFact(
  'framegraph_execute_all_owns_pass_order',
  frameGraphHasExecuteAll && rendererRuntimeUsesExecuteAll && frameGraphManualSequenceHits.length === 0,
  'FrameGraphExecutor.execute_all owns the compiled pass order without a hand-written FrameGraphPlanNodes sequence',
  {
    frameGraphHasExecuteAll,
    rendererRuntimeUsesExecuteAll,
    frameGraphManualSequenceHits,
  },
);
addRequiredSourceFact(
  'renderer_dispatcher_no_global_renderer',
  dispatcherRendererHits.length === 0,
  'render pass dispatch contexts carry explicit pass resources without capturing &mut Renderer',
  { dispatcherRendererHits },
);
addRequiredSourceFact(
  'render_hot_path_no_panic_or_silent_skip',
  renderHotPathPanicHits.length === 0 && invalidBatchIndexSilentSkipHits.length === 0,
  'render frame hot paths report structured errors and invalid batch references without panic/unwrap/expect or silent filter_map drops',
  { renderHotPathPanicHits, invalidBatchIndexSilentSkipHits },
);
addRequiredSourceFact(
  'resource_registry_owns_all_targets',
  resourceRegistryOwnsAllTargetKinds && !rendererStillOwnsTargets,
  'RenderResourceRegistry owns surface/depth/main/post/shadow/water/overlay/capture/readback lifecycle',
  {
    registry: lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'struct RenderResourceRegistry'),
    rendererStillOwnsTargets,
    resourceRegistryOwnsAllTargetKinds,
  },
);
addRequiredSourceFact(
  'batch_plan_real_bounds',
  !batchPlanHasZeroBounds,
  'RenderBatchPlan bounds come from real model, primitive, water, terrain, and decal bounds',
  {
    zeroBounds: lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'center: Vec3::ZERO'),
  },
);
addRequiredSourceFact(
  'batch_plan_real_lod_inputs',
  !batchPlanUsesSeedWorkloadLod,
  'RenderBatchPlanner LOD selection uses distance, projected size, metadata, and quality profile inputs',
  {
    selectLod: lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'fn select_lod'),
    selectLodBody: renderBatchPlannerSelectLodBody.trim(),
  },
);
addRequiredSourceFact(
  'batch_plan_real_culling_counters',
  frustumCullingCounterMutated && distanceCullingCounterMutated,
  'frustum and distance culling counters are incremented from real batch-planning decisions',
  {
    frustumCullingCounterMutated,
    distanceCullingCounterMutated,
    counters: [
      lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'frustum_culled_chunks'),
      lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'distance_culled_chunks'),
    ],
  },
);
addRequiredSourceFact(
  'batch_plan_scene_wired',
  batchPlanSceneWired,
  'RenderBatchPlan controls the renderer scene submission path and telemetry workload source',
  {
    frameOrchestratorSubmission: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'planned_model_draws'),
    frameOrchestratorWorkload: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'self.prepare_frame_workload_plan'),
    workloadPlanner: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_workload_plan.rs'), rendererFrameWorkloadPlan, 'RenderBatchPlanner::build'),
    workloadTerrain: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_workload_plan.rs'), rendererFrameWorkloadPlan, 'terrain_batch_inputs'),
    workloadDecal: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_workload_plan.rs'), rendererFrameWorkloadPlan, 'decal_batch_inputs'),
    frameOrchestratorDecal: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'planned_projected_decals'),
    telemetry: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_perf_finalize.rs'), rendererFramePerfFinalize, 'perf.visible_objects = context.render_batch_plan.visible_objects'),
  },
);
addRequiredSourceFact(
  'batch_plan_terrain_decal_real_entries',
  terrainDecalRealEntries,
  'terrain and decal batch kinds have real construction paths and unit coverage, not enum-only accounting',
  {
    terrainBatchEnumMentions,
    decalBatchEnumMentions,
    terrainBatchConstructed,
    decalBatchConstructed,
    terrainBatchTested,
    decalBatchTested,
    terrain: lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'RenderBatchKind::Terrain'),
    decal: lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'RenderBatchKind::Decal'),
  },
);
addRequiredSourceFact(
  'physics_surface_source_no_track_position_inference',
  !physicsTrackPositionSurfaceInference,
  'industrial vehicle, contact, and telemetry surface selection does not infer material from track position',
  {
    vehicle: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, 'SurfaceMaterialAtTrackPosition'),
    contactEvent: lineEvidence(path.join(voplayRoot, 'scene3d/contact_event.vo'), contactEvent, 'SurfaceMaterialAtTrackPosition'),
    telemetry: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle_telemetry.vo'), readText(path.join(voplayRoot, 'scene3d/vehicle_telemetry.vo')) || '', 'SurfaceMaterialAtTrackPosition'),
  },
);
addRequiredSourceFact(
  'physics_set_pose_backend_only',
  !setPoseDirectPhysicsMutation && !poseResetHelperDirectPhysicsMutation,
  'Vehicle.SetPose/reset/respawn/recovery/sleep route pose and velocity mutation through backend helper contracts',
  {
    setPose: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, 'func (v *Vehicle) SetPose'),
    applyPoseResetToBackend: evidenceForFirstToken(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, directBodyMutationTokens),
    setPoseDirectPhysicsMutation,
    poseResetHelperDirectPhysicsMutation,
    directMutationTokens: directBodyMutationTokens,
  },
);
addRequiredSourceFact(
  'physics_pose_reset_helper_backend_only',
  !poseResetHelperDirectPhysicsMutation,
  'applyPoseResetToBackend does not directly mutate body pose, body velocity, or raw physics velocity fields outside a backend adapter allowlist',
  {
    applyPoseResetToBackend: evidenceForFirstToken(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, directBodyMutationTokens),
    directMutationTokens: directBodyMutationTokens,
  },
);
addRequiredSourceFact(
  'physics_replay_records_backend_apply_hash',
  replayRecordsBackendApplyHash,
  'PhysicsReplayTrace records backend apply hash in addition to fixed dt, intent, surface/contact, pose, and telemetry hashes',
  {
    replayTrace: lineEvidence(path.join(voplayRoot, 'scene3d/replay.vo'), replay, 'type PhysicsReplayTrace struct'),
  },
);
addRequiredSourceFact(
  'physics_session_is_runtime_authority',
  !sessionIsWrapperOnly && !physicsStressBypassesSession,
  'VehiclePhysicsSession is the fixed-step runtime authority and stress scenarios do not bypass it with direct scene/controller stepping',
  {
    sessionIsWrapperOnly,
    physicsStressBypassesSession,
    sessionStep: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle_physics_session.vo'), vehiclePhysicsSession, 'func (s *VehiclePhysicsSession) Step'),
    stressDirectSceneStep: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'StepAndSyncPhysics'),
    stressUpdateIntent: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'UpdateIntent'),
  },
);
addRequiredSourceFact(
  'physics_backend_packet_schema_contract',
  backendPacketMissingTokens.length === 0,
  'backend packets expose schema version, packet kind, byte length, packet hash, and capability contract identifiers',
  { backendPacketMissingTokens, expectedTokens: backendPacketSchemaTokens },
);
addRequiredSourceFact(
  'physics_invalid_telemetry_is_not_sanitized_away',
  !invalidTelemetryStressBypass,
  'industrial physics stress reports invalid raw samples and validation issues without accepting cleaned samples as pass evidence',
  {
    invalidTelemetryStressBypass,
    invalidSample: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'InvalidSampleCount'),
    validationIssues: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'ValidationIssues'),
    sampleFinite: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'sampleFinite'),
  },
);
addRequiredSourceFact(
  'physics_replay_is_executable_step_contract',
  executableReplayMissingTokens.length === 0 && !sameRuntimeReplayDriftOnly,
  'physics replay validation executes a recorded step contract with per-step hashes and mismatch evidence',
  { executableReplayMissingTokens, replayUsesRecordedTrace, sameRuntimeReplayDriftOnly },
);
addRequiredSourceFact(
  'blockkart_product_boundary',
  blockKartGenericAuthoringHits.length === 0
    && !blockKartPrimitiveAuthoringPresent
    && !blockKartHudAssemblesLowLevelFacts
    && !blockKartVisualReadsMutableVehicleState
    && !blockKartDirectVehiclePoseCalls
    && !blockKartDirectEntityPhysicsMutation,
  'BlockKart owns product content/rules/HUD consumption only and does not author generic engine primitives, physics poses, or low-level facts',
  {
    primitiveAuthoring: evidenceForFirstToken(path.join(blockKartRoot, 'primitive_world.vo'), blockKartPrimitiveWorld, ['BlockKartVisualContent', 'primitive3d.NewBuilder', 'primitive3d.LayerDesc', 'primitive3d.ChunkingDesc', 'spawnPrimitiveTrackPhysics', 'spawnRoadColliderStrip']),
    genericAuthoringHits: blockKartGenericAuthoringHits,
    hudFacts: lineEvidence(path.join(blockKartRoot, 'world.vo'), blockKartWorld, 'PrimitiveStats()'),
    visualMutableState: lineEvidence(path.join(blockKartRoot, 'primitive_world.vo'), blockKartPrimitiveWorld, 'w.vehicle.SteerAngle'),
    directSetPose: lineEvidence(path.join(blockKartRoot, 'world.vo'), blockKartWorld, 'w.vehicle.SetPose'),
    directPlayerMutation: evidenceForFirstToken(path.join(blockKartRoot, 'world.vo'), blockKartWorld, ['w.player.SetPosition(', 'w.player.SetVelocity(', 'w.player.SetAngularVelocity(']),
    directEntityMutation: evidenceForFirstRegex(blockKartVoFiles, directEntityMutationRegex, '.SetPosition('),
  },
);
addRequiredSourceFact(
  'blockkart_no_direct_player_physics_mutation',
  !blockKartDirectPlayerPhysicsMutation,
  'BlockKart product world does not directly mutate kart/entity pose, velocity, or angular velocity',
  {
    directPlayerMutation: evidenceForFirstToken(path.join(blockKartRoot, 'world.vo'), blockKartWorld, ['w.player.SetPosition(', 'w.player.SetVelocity(', 'w.player.SetAngularVelocity(']),
  },
);
addRequiredSourceFact(
  'blockkart_no_direct_entity_physics_mutation',
  !blockKartDirectEntityPhysicsMutation,
  'BlockKart product source does not directly mutate entity pose, velocity, or angular velocity',
  {
    directEntityMutation: evidenceForFirstRegex(blockKartVoFiles, directEntityMutationRegex, '.SetPosition('),
  },
);

addEvidenceRow({
  capability: 'Render frame pipeline',
  expectedOwner: 'voplay renderer stage APIs',
  actualOwner: renderPipelineStagesPass
    ? 'FrameDecode owns command stream decode; Renderer stores RenderFramePipeline stage metrics; PostPassSetup and Renderer own post/perf packet stages'
    : 'FrameSubmitOrchestrator::run delegates to Renderer::run_frame_orchestrator',
  mutationPath: renderPipelineStagesPass
    ? 'decode_frame_commands returns RenderFrameDecode; RenderFramePipeline::from_frame_metrics, PostPassSetup::upload_uniforms, and update_last_perf_packet construct stage facts from live frame counters'
    : 'command stream mutates draw list and retained render world inside run_frame_orchestrator',
  runtimePath: renderPipelineStagesPass
    ? 'Renderer.submit_frame -> run_frame_orchestrator -> finalize_frame_perf -> RenderFramePipeline::from_frame_metrics -> last_frame_pipeline'
    : 'Renderer.submit_frame -> submit_frame_inner -> FrameSubmitOrchestrator::run -> run_frame_orchestrator',
  bypassFound: !renderPipelineStagesPass || rendererFrameOrchestrator.includes('let mut reader = StreamReader'),
  evidence: renderPipelineStagesPass ? [
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_decode.rs'), rendererFrameDecode, 'pub(super) fn decode_frame_commands'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_decode.rs'), rendererFrameDecode, 'StreamReader::new'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_decode.rs'), rendererFrameDecode, 'RenderFrameDecode {'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'RenderFramePipeline {'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_perf_finalize.rs'), rendererFramePerfFinalize, 'RenderFramePipeline::from_frame_metrics'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/post_pass.rs'), readText(path.join(voplayRoot, 'rust/src/renderer/post_pass.rs')) || '', 'pub(super) fn upload_uniforms'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer.rs'), renderer, 'update_last_perf_packet'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer.rs'), renderer, 'last_frame_pipeline'),
  ] : [
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_submit.rs'), rendererFrameSubmit, 'FrameSubmitOrchestrator::run'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'let mut reader = StreamReader'),
  ],
  gateCoverage: 'source_fact.render_pipeline_stages_constructed, source_fact.frame_orchestrator_stage_only',
  status: renderPipelineStagesPass && !rendererFrameOrchestrator.includes('let mut reader = StreamReader'),
  nextFix: renderPipelineStagesPass
    ? ''
    : 'split decode_frame/build_scene_snapshot/build_batch_plan/build_frame_graph/execute_frame_graph/submit_backend/encode_telemetry and construct RenderFramePipeline',
});
addEvidenceRow({
  capability: 'FrameGraph pass dispatch',
  expectedOwner: 'FrameGraphExecutor with registered pass nodes',
  actualOwner: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro
    ? 'FrameGraphExecutor.execute_node dispatches RenderPassKind through FramePassDispatcher'
    : 'frame_orchestrator.rs macro supplies closures to execute_node',
  mutationPath: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro
    ? 'FramePassDispatcher builds pass contexts; FrameGraphExecutor records node diagnostics from dispatcher workload'
    : 'runtime closures build pass contexts and mutate perf/pass side effects',
  runtimePath: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro
    ? 'run_frame_orchestrator -> execute_frame_pass_sequence -> FrameGraphExecutor.execute_node -> FramePassDispatcher::execute'
    : 'execute_render_node! -> FrameGraphExecutor.execute_node',
  bypassFound: runtimeUsesExecuteRenderNodeMacro || executeNodeTakesAdHocClosure,
  evidence: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro ? [
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'trait RenderPassNodeDispatcher'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'pub(crate) fn execute_node'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/pass_dispatch.rs'), readText(path.join(voplayRoot, 'rust/src/renderer/pass_dispatch.rs')) || '', 'impl RenderPassNodeDispatcher for FramePassDispatcher'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_pass_sequence.rs'), rendererFramePassSequence, '.execute_node(&context.nodes.main_opaque'),
  ] : [
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'macro_rules! execute_render_node'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'F: FnOnce'),
  ],
  gateCoverage: 'source_fact.framegraph_dispatch_owns_pass_execution',
  status: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro,
  nextFix: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro
    ? ''
    : 'register pass executors by RenderPassKind and make execute_node dispatch through node metadata',
});
addEvidenceRow({
  capability: 'Batch planning and visibility',
  expectedOwner: 'RenderBatchPlanner',
  actualOwner: batchPlanningIndustrialPass
    ? 'RenderBatchPlanner owns visible model, primitive, terrain, water, and decal submission facts'
    : batchPlanContractPass
    ? 'RenderBatchPlanner owns model bounds, primitive chunk facts, distance LOD, and culling counters'
    : 'PrimitiveRenderWorld performs real culling before RenderBatchPlanner rebuilds placeholder plan facts',
  mutationPath: batchPlanningIndustrialPass
    ? 'RenderBatchPlanner drives visible/resident/dirty/upload/submission facts for every batch kind'
    : batchPlanContractPass
    ? 'PrimitiveRenderWorld exposes PrimitiveChunkBatchInfo; RenderBatchPlanner::build routes planned batches after camera and distance decisions'
    : 'RenderBatchPlanner::build pushes chunks with zero bounds and seed/workload LOD',
  runtimePath: batchPlanningIndustrialPass
    ? 'Renderer.submit_frame -> RenderBatchPlanner::build -> FrameGraph pass inputs -> BackendSubmitExecutor'
    : batchPlanContractPass
    ? 'run_frame_orchestrator -> collect_scene_primitive_draws_with_chunk_info(None) -> RenderBatchPlanner::build(camera, quality) -> planned_* batches'
    : 'run_frame_orchestrator -> RenderBatchPlanner::build -> planned_* batches',
  bypassFound: !batchPlanningIndustrialPass,
  evidence: batchPlanContractPass ? [
    lineEvidence(path.join(voplayRoot, 'rust/src/primitive_scene.rs'), readText(path.join(voplayRoot, 'rust/src/primitive_scene.rs')) || '', 'PrimitiveChunkBatchInfo'),
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'bounds_from_model_matrix'),
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'plan.frustum_culled_chunks += 1'),
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'plan.distance_culled_chunks += 1'),
  ] : [
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'center: Vec3::ZERO'),
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'fn select_lod'),
  ],
  gateCoverage: 'source_fact.batch_plan_real_bounds, source_fact.batch_plan_real_lod_inputs, source_fact.batch_plan_real_culling_counters, source_fact.batch_plan_scene_wired, source_fact.batch_plan_terrain_decal_real_entries',
  status: batchPlanningIndustrialPass,
  nextFix: batchPlanningIndustrialPass
    ? ''
    : batchPlanContractPass
    ? 'extend the same planner fact path to active terrain and decal batch kinds'
    : 'carry model/chunk bounds and camera/quality inputs into RenderBatchPlanner and count real culls',
});
addEvidenceRow({
  capability: 'Physics backend contract',
  expectedOwner: 'voplay scene3d backend helpers',
  actualOwner: physicsBackendContractPass
    ? 'Vehicle pose reset/apply helpers carry backend surface and apply-hash facts into telemetry/replay'
    : 'Vehicle methods and BlockKart call sites still perform direct pose/surface paths',
  mutationPath: physicsBackendContractPass
    ? 'Vehicle.SetPose -> applyPoseResetToBackend; ApplyForceCommand records LastBackendApplyCommand; telemetry/replay carry BackendApplyHash'
    : 'Vehicle.SetPose writes Entity pose and Physics velocity fields directly',
  runtimePath: physicsBackendContractPass
    ? 'vehicle sync -> carried wheel surface material -> CurrentSurfaceMaterial/ContactEvent/VehicleTelemetry; RecordVehicle -> BackendApplyHash'
    : 'BlockKart spawn/reset -> Vehicle.SetPose; vehicle/contact/telemetry -> SurfaceMaterialAtTrackPosition',
  bypassFound: !physicsBackendContractPass,
  evidence: [
    lineEvidence(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, 'func (v *Vehicle) SetPose'),
    lineEvidence(path.join(voplayRoot, 'scene3d/contact_event.vo'), contactEvent, 'SurfaceMaterialAtTrackPosition'),
    lineEvidence(path.join(voplayRoot, 'scene3d/replay.vo'), replay, 'BackendApplyHash'),
  ],
  gateCoverage: 'source_fact.physics_surface_source_no_track_position_inference, source_fact.physics_set_pose_backend_only, source_fact.physics_replay_records_backend_apply_hash',
  status: physicsBackendContractPass,
  nextFix: physicsBackendContractPass
    ? ''
    : 'introduce backend pose/reset/recovery helpers and make track-position surface inference compat-only',
});
addEvidenceRow({
  capability: 'BlockKart product boundary',
  expectedOwner: 'voplay product authoring APIs and structured telemetry',
  actualOwner: blockKartProductBoundaryPass
    ? 'scene3d owns BlockKart primitive catalog/map/collider/pack adapters and primitive mutation; BlockKart keeps content placement, rules, tuning, HUD consumption, and diagnostics consumption'
    : 'BlockKart primitive_world.vo and world.vo still own low-level engine facts',
  mutationPath: blockKartProductBoundaryPass
    ? 'BlockKart calls scene3d.NewBlockKartPrimitiveContent, SpawnBlockKartMap*, SpawnBlockKartTrackColliderStrip, and SetBlockKartPrimitivePose wrappers'
    : 'BlockKart builds primitive layers/materials/colliders or mutates player physics directly',
  runtimePath: blockKartProductBoundaryPass
    ? 'World.buildPrimitiveLevel -> ensurePrimitiveScene -> scene3d.BlockKartPrimitiveContent; attachPrimitiveTrackColliderEntities -> scene3d.SpawnBlockKartTrackColliderStrip'
    : 'World.buildPrimitiveLevel/spawnPrimitiveTrackPhysics/hudState/currentPrimitiveKartVisualState',
  bypassFound: !blockKartProductBoundaryPass,
  evidence: [
    lineEvidence(path.join(voplayRoot, 'scene3d/blockkart_primitives.vo'), readText(path.join(voplayRoot, 'scene3d/blockkart_primitives.vo')) || '', 'type BlockKartPrimitiveContent struct'),
    lineEvidence(path.join(voplayRoot, 'scene3d/blockkart_map.vo'), readText(path.join(voplayRoot, 'scene3d/blockkart_map.vo')) || '', 'func SpawnBlockKartTrackColliderStrip'),
    lineEvidence(path.join(voplayRoot, 'scene3d/blockkart_pack.vo'), readText(path.join(voplayRoot, 'scene3d/blockkart_pack.vo')) || '', 'func WriteBlockKartRuntimePack'),
    lineEvidence(path.join(blockKartRoot, 'primitive_world.vo'), blockKartPrimitiveWorld, 'scene3d.NewBlockKartPrimitiveContent'),
    lineEvidence(path.join(blockKartRoot, 'track_runtime.vo'), blockKartTrackRuntime, 'scene3d.SpawnBlockKartTrackColliderStrip'),
  ],
  gateCoverage: 'source_fact.blockkart_product_boundary',
  status: blockKartProductBoundaryPass,
  nextFix: blockKartProductBoundaryPass
    ? ''
    : 'move primitive/chunk/collider/surface authoring to voplay and make BlockKart consume read-only telemetry',
});

const unresolvedEvidenceNextFixes = evidenceTable
  .filter((row) => row.status === 'pass' && String(row.nextFix || '').trim().length > 0)
  .map((row) => ({
    capability: row.capability,
    nextFix: row.nextFix,
  }));
addRequiredSourceFact(
  'evidence_has_no_unresolved_next_fix',
  unresolvedEvidenceNextFixes.length === 0,
  'passed evidence rows do not carry unresolved Next Fix work',
  { unresolvedEvidenceNextFixes },
);

const provenancePath = path.join(root, 'apps/studio/public/quickplay/blockkart/provenance.json');
const provenance = readJson(provenancePath);
const expectedBlockKartCommit = provenance.value?.project?.commit ?? null;
const expectedVoplayCommit = provenance.value?.dependencies?.find((dep) => dep.module === 'github.com/vo-lang/voplay')?.commit ?? null;
const dirtyProvenanceEntries = [
  ...(provenance.value?.project?.dirty === true ? [{
    kind: 'project',
    owner: provenance.value?.project?.module || 'github.com/vo-lang/blockkart',
    commit: provenance.value?.project?.commit || null,
  }] : []),
  ...((provenance.value?.dependencies ?? [])
    .filter((dep) => dep?.dirty === true)
    .map((dep) => ({
      kind: 'dependency',
      owner: dep.module || '(unknown)',
      source: dep.source || null,
      version: dep.version || null,
      commit: dep.commit || null,
    }))),
];

function baselineSourceMismatches(report) {
  const mismatches = [];
  if (!expectedBlockKartCommit || report.project?.commit !== expectedBlockKartCommit || report.project?.provenanceCommit !== expectedBlockKartCommit) {
    mismatches.push({
      module: 'github.com/vo-lang/blockkart',
      expected: expectedBlockKartCommit,
      commit: report.project?.commit ?? null,
      provenanceCommit: report.project?.provenanceCommit ?? null,
    });
  }
  const voplay = (report.dependencies ?? []).find((dep) => dep.module === 'github.com/vo-lang/voplay') ?? null;
  if (!expectedVoplayCommit || voplay?.commit !== expectedVoplayCommit || voplay?.provenanceCommit !== expectedVoplayCommit) {
    mismatches.push({
      module: 'github.com/vo-lang/voplay',
      expected: expectedVoplayCommit,
      commit: voplay?.commit ?? null,
      provenanceCommit: voplay?.provenanceCommit ?? null,
    });
  }
  return mismatches;
}

function sceneSourceMismatches(report) {
  const mismatches = [];
  for (const scene of report.scenes ?? []) {
    const sceneReport = {
      project: scene.source?.project ?? null,
      dependencies: scene.source?.dependencies ?? [],
    };
    for (const mismatch of baselineSourceMismatches(sceneReport)) {
      mismatches.push({ scene: scene.name, ...mismatch });
    }
  }
  return mismatches;
}

function passStatusReport(report, detail) {
  return {
    ok: report.status === 'pass' || report.status === 'ok',
    detail,
    evidence: {
      status: report.status ?? null,
      gate: report.gate ?? null,
      summary: report.summary ?? null,
      issueCount: report.issueCount ?? null,
    },
  };
}

checkReport(
  path.join(root, 'target/voplay-framegraph-unit/report.json'),
  'phase-2',
  'gate.voplay_framegraph_unit',
  (report) => passStatusReport(report, 'voplay framegraph unit report passes'),
  'voplay-framegraph-unit',
);

checkReport(
  path.join(root, 'target/voplay-render-core-unit/report.json'),
  'phase-2',
  'gate.voplay_render_core_unit',
  (report) => passStatusReport(report, 'voplay render core unit report passes'),
  'voplay-render-core-unit',
);

checkReport(
  path.join(root, 'target/voplay-render-structure-lint/report.json'),
  'phase-2',
  'gate.voplay_render_structure_lint',
  (report) => passStatusReport(report, 'voplay render structure lint report passes'),
  'voplay-render-structure-lint',
);

checkReport(
  path.join(root, 'target/voplay-batch-planner-unit/report.json'),
  'phase-3',
  'gate.voplay_batch_planner_unit',
  (report) => passStatusReport(report, 'voplay batch planner unit report passes'),
  'voplay-batch-planner-unit',
);

checkReport(
  path.join(root, 'target/voplay-physics-backend-contract/report.json'),
  'phase-4',
  'gate.voplay_physics_backend_contract',
  (report) => passStatusReport(report, 'voplay physics backend contract report passes'),
  'voplay-physics-backend-contract',
);

checkReport(
  path.join(root, 'target/blockkart-product-boundary-strict/report.json'),
  'phase-6',
  'gate.blockkart_product_boundary_strict',
  (report) => passStatusReport(report, 'BlockKart product boundary strict report passes'),
  'blockkart-product-boundary-strict',
);

const renderStress = checkReport(path.join(root, 'target/voplay-render-stress-budgeted/report.json'), 'phase-5', 'gate.render_stress_budgeted', (report) => {
  const required = [
    'blockkart-quickplay-baseline',
    'blockkart-primitive-10k',
    'blockkart-water',
    'blockkart-resource-churn-soak',
    'blockkart-resize-recreate-targets',
    'blockkart-restart-50',
    'blockkart-chunked-world-drive',
    'blockkart-shadow-post-matrix',
  ];
  const names = sceneNames(report);
  const missing = required.filter((name) => !names.has(name));
  const sourceMismatches = sceneSourceMismatches(report);
  const ok = report.status === 'pass' && allScenesPass(report) && missing.length === 0 && sourceMismatches.length === 0;
  return {
    ok,
    detail: 'render stress budgeted report passes all required industrial scenes',
    evidence: {
      status: report.status,
      missingScenes: missing,
      sourceMismatches,
      sceneCount: report.scenes?.length || 0,
      summary: report.summary || null,
    },
  };
}, 'voplay-render-stress-budgeted');

checkReport(path.join(root, 'target/voplay-render-soak-10m/report.json'), 'phase-5', 'gate.render_soak_10m', (report) => {
  const sourceMismatches = sceneSourceMismatches(report);
  return {
    ok: report.status === 'pass' && allScenesPass(report) && sourceMismatches.length === 0,
    detail: 'ten minute render soak report exists and passes',
    evidence: {
      status: report.status,
      sourceMismatches,
      sceneCount: report.scenes?.length || 0,
      summary: report.summary || null,
    },
  };
}, 'voplay-render-soak-10m');

checkReport(path.join(root, 'target/voplay-physics-industrial-stress/report.json'), 'phase-5', 'gate.physics_industrial_stress', (report) => {
  const required = ['skidpad', 'slalom', 'drift-turbo', 'boost-pad', 'offroad-transition', 'surface-transition', 'jump-landing', 'wall-impact', 'rail-ride', 'wall-ride', 'water-skim', 'recovery', 'multi-vehicle-scripted-soak'];
  const names = scenarioNames(report);
  const missing = required.filter((name) => !names.has(name));
  const replayOk = report.replay?.status === 'pass'
    && Number(report.replay?.samples ?? 0) > 0
    && Number(report.replay?.mismatches ?? Infinity) === 0
    && Number.isFinite(Number(report.replay?.stepHash))
    && Number.isFinite(Number(report.replay?.backendPacketHash))
    && (!Number.isFinite(Number(report.replay?.driftMeters)) || Number(report.replay?.driftMeters) <= 0.01);
  const ok = report.status === 'pass' && allIndustrialPhysicsSamplesClean(report) && missing.length === 0 && replayOk;
  return {
    ok,
    detail: 'industrial physics stress passes scenario, fallback, invalid sample, and executable replay gates',
    evidence: {
      status: report.status,
      missingScenarios: missing,
      scenarioCount: report.scenarios?.length || 0,
      replay: report.replay || null,
      summary: report.summary || null,
    },
  };
}, 'voplay-physics-industrial-stress');

checkReport(path.join(root, 'target/quickplay-source-audit/quickplay-source-audit.json'), 'phase-6', 'gate.quickplay_source_audit', (report) => ({
  ok: report.status === 'ok' && Array.isArray(report.issues) && report.issues.length === 0,
  detail: 'quickplay source audit passes',
  evidence: {
    status: report.status,
    issueCount: report.issues?.length ?? null,
  },
}), 'quickplay-source-audit');

checkReport(
  path.join(root, 'target/quickplay-regenerate-check/report.json'),
  'phase-6',
  'gate.quickplay_regenerate_check',
  (report) => passStatusReport(report, 'quickplay regenerate check report passes'),
  'quickplay-regenerate-check',
);

checkReport(
  path.join(root, 'target/quickplay-validate/report.json'),
  'phase-6',
  'gate.quickplay_validate',
  (report) => passStatusReport(report, 'quickplay validate report passes'),
  'quickplay-validate',
);

checkReport(path.join(root, 'target/blockkart-baseline/blockkart-baseline.json'), 'phase-6', 'gate.blockkart_baseline_report', (report) => {
  const sourceMismatches = baselineSourceMismatches(report);
  return {
    ok: (report.status === 'pass' || report.status === 'ok' || report.lifecycle?.reachedRunning === true) && sourceMismatches.length === 0,
    detail: 'BlockKart baseline report reaches running state',
    evidence: {
      status: report.status || null,
      lifecycle: report.lifecycle || null,
      sourceMismatches,
    },
  };
}, 'blockkart-baseline');

checkReport(path.join(root, 'target/blockkart-baseline-restart-50/blockkart-baseline.json'), 'phase-6', 'gate.blockkart_restart_50_report', (report) => {
  const sourceMismatches = baselineSourceMismatches(report);
  return {
    ok: (report.status === 'pass' || report.status === 'ok' || report.lifecycle?.reachedRunning === true) && Number(report.restart?.failures || 0) === 0 && sourceMismatches.length === 0,
    detail: 'BlockKart restart-50 report passes without restart failures',
    evidence: {
      status: report.status || null,
      restart: report.restart || null,
      lifecycle: report.lifecycle || null,
      sourceMismatches,
    },
  };
}, 'blockkart-baseline-restart-50');

const voplayArtifact = provenance.value?.dependencies?.find((dep) => dep.module === 'github.com/vo-lang/voplay') || null;
addCheck('phase-6', 'artifact.provenance_exists', provenance.exists && !provenance.error, 'quickplay provenance JSON exists and parses', { path: provenancePath, parseError: provenance.error });
addCheck('phase-6', 'artifact.voplay_commit_recorded', Boolean(voplayArtifact?.commit), 'quickplay provenance records the voplay artifact source commit', { version: voplayArtifact?.version || null, cacheDir: voplayArtifact?.cacheDir || null, commit: voplayArtifact?.commit || null });
addCheck('phase-6', 'artifact.blockkart_commit_recorded', Boolean(provenance.value?.project?.commit), 'quickplay provenance records the BlockKart source commit', { commit: provenance.value?.project?.commit || null });
addCheck('phase-6', 'artifact.provenance_clean', provenance.exists && !provenance.error && dirtyProvenanceEntries.length === 0, 'strict industrial readiness requires quickplay provenance dirty flags to be false', { dirtyProvenanceEntries });

const defaultBackendApplyBody = bodyOfFunction(physics, 'func (defaultPhysicsBackend) ApplyVehicleForces');
const unconsumedBackendFields = ['BodyForce', 'DragForce', 'Downforce', 'WaterLift', 'AirControl', 'WallGrip', 'RailGrip']
  .filter((field) => !defaultBackendApplyBody.includes(`command.${field}`));
addCheck('source-audit', 'physics.backend_apply_fields_consumed', unconsumedBackendFields.length === 0, 'default backend consumes every PhysicsBackendApplyCommand field or the field is removed from the contract', { unconsumedBackendFields });
const applyForceCommandBody = bodyOfFunction(vehicle, 'func (v *Vehicle) ApplyForceCommand');
addCheck('source-audit', 'physics.no_post_backend_force_patch', !(applyForceCommandBody.includes('applyForceCommandToBackend') && applyForceCommandBody.includes('v.Body.ApplyForce')), 'ApplyForceCommand does not apply core body forces after backend apply', {
  applyForceCommand: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, 'func (v *Vehicle) ApplyForceCommand'),
  postBackendForce: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, 'v.Body.ApplyForce'),
});
const emptyBackendHooks = ['ApplyPoseReset', 'ApplyMotionReset', 'ApplySleepState', 'ApplyRecovery']
  .filter((hook) => new RegExp(`func \\(defaultPhysicsBackend\\) ${hook}[^{}]*\\{\\}`).test(physics));
addCheck('source-audit', 'physics.backend_reset_hooks_non_empty', emptyBackendHooks.length === 0, 'default backend reset, motion, sleep, and recovery hooks have real behavior', { emptyBackendHooks });
const blockKartLowLevelHits = [];
for (const entry of blockKartVoFiles) {
  for (const token of ['ApplyVehicleConstraint(', 'ApplyEntityPhysicsConstraint(', '.Diagnostics()', '.Telemetry()', 'PrimitiveStats(', 'primitive3d.NewLayer', 'primitive3d.NewBuilder', 'primitive3d.LayerDesc', 'primitive3d.ChunkingDesc', 'primitive3d.MaterialDesc']) {
    if (entry.source.includes(token)) {
      blockKartLowLevelHits.push({ path: entry.file, token, line: lineOf(entry.source, token) });
    }
  }
}
addCheck('source-audit', 'blockkart.no_low_level_render_physics_workarounds', blockKartLowLevelHits.length === 0, 'BlockKart does not read low-level render/physics diagnostics or apply physics/render workarounds directly', { blockKartLowLevelHits });

const blockKartStrictGate = runGateScript('scripts/ci/blockkart_product_boundary_strict.mjs');
addCheck(
  'source-audit',
  'gate.blockkart_product_boundary_strict',
  blockKartStrictGate.status === 0,
  'blockkart-product-boundary-strict must pass before industrial readiness can pass',
  blockKartStrictGate,
);

const requiredFalseFacts = sourceFactRequirements
  .filter((fact) => fact.required && fact.status !== true)
  .map((fact) => fact.code);
addCheck(
  'source-audit',
  'source_facts.required_all_pass',
  requiredFalseFacts.length === 0,
  'all industrial-required source facts pass',
  { requiredFalseFacts },
);

const sourceFacts = {
  rendererLines,
  frameSubmitLines,
  frameOrchestratorLines,
  submitFrameLines,
  pipeline3dLines,
  fileBudgets,
  fileBudgetFailures,
  emptyOwnerModules,
  runtimeUsesExecutePass: rendererRuntime.includes('.execute_pass('),
  runtimeUsesExecuteNode: rendererRuntime.includes('execute_node(') || runtimeUsesExecuteRenderNodeMacro,
  runtimeUsesExecuteRenderNodeMacro,
  frameGraphHasExecuteAll,
  rendererRuntimeUsesExecuteAll,
  frameGraphManualSequenceHits,
  dispatcherRendererHits,
  renderHotPathPanicHits,
  invalidBatchIndexSilentSkipHits,
  submitFrameHasBeginRenderPass: submitFrameBody.includes('begin_render_pass'),
  submitFrameHasDirectDraw: /\.draw_models\(|draw_main_and_water\(|\.draw\(/.test(submitFrameBody),
  submitFrameHasQueueSubmit: submitFrameBody.includes('queue.submit'),
  runtimeHasRenderPassBody,
  runtimeHasDirectDraw,
  runtimeHasQueueSubmit,
  rendererStillOwnsTargets,
  constructedPipelineStages,
  allPipelineStagesConstructed,
  frameOrchestratorStageOnly: !rendererFrameOrchestrator.includes('let mut reader = StreamReader')
    && !rendererFrameOrchestrator.includes('PostUniform::from_settings')
    && !rendererFrameOrchestrator.includes('encode_renderer_perf_packet'),
  executeNodeTakesAdHocClosure,
  executeNodeOwnsPassDispatch,
  resourceRegistryOwnsAllTargetKinds,
  batchPlanSceneWired,
  batchPlanHasZeroBounds,
  batchPlanUsesSeedWorkloadLod,
  terrainBatchEnumMentions,
  decalBatchEnumMentions,
  terrainBatchConstructed,
  decalBatchConstructed,
  terrainBatchTested,
  decalBatchTested,
  terrainDecalRealEntries,
  frustumCullingCounterMutated,
  distanceCullingCounterMutated,
  physicsTrackPositionSurfaceInference,
  setPoseDirectPhysicsMutation,
  poseResetHelperDirectPhysicsMutation,
  replayRecordsBackendApplyHash,
  sessionIsWrapperOnly,
  physicsStressBypassesSession,
  backendPacketMissingTokens,
  invalidTelemetryStressBypass,
  executableReplayMissingTokens,
  sameRuntimeReplayDriftOnly,
  blockKartPrimitiveAuthoringPresent,
  blockKartGenericAuthoringHits,
  blockKartHudAssemblesLowLevelFacts,
  blockKartVisualReadsMutableVehicleState,
  blockKartDirectVehiclePoseCalls,
  blockKartDirectPlayerPhysicsMutation,
  blockKartDirectEntityPhysicsMutation,
  budgetedRenderStressStatus: renderStress?.status || null,
  soakReportExists: existsSync(path.join(root, 'target/voplay-render-soak-10m/report.json')),
  requiredFalseFacts,
};
const failures = checks.filter((check) => check.status !== 'pass');
const sourceAuditFailures = failures.filter((failure) => (
  failure.phase === 'source-audit'
  || failure.code.startsWith('source_fact.')
  || failure.code.startsWith('source_audit.')
));
const firstPrinciplesVerdict = {
  status: sourceAuditFailures.length === 0 ? 'pass' : 'fail',
  industrialReadyEligible: failures.length === 0 && sourceAuditFailures.length === 0,
  questions: [
    {
      question: 'data_owner',
      answer: 'voplay owns renderer/physics durable state; BlockKart owns product content, race rules, tuning, HUD consumption, and diagnostics consumption.',
      pass: !blockKartPrimitiveAuthoringPresent && !blockKartDirectEntityPhysicsMutation,
    },
    {
      question: 'mutation_authority',
      answer: 'renderer mutation must flow through stage pipeline, FrameGraph, ResourceRegistry, and BatchPlanner; vehicle mutation must flow through backend contract helpers and adapters.',
      pass: !runtimeHasDirectDraw && !runtimeHasQueueSubmit && !poseResetHelperDirectPhysicsMutation,
    },
    {
      question: 'runtime_path',
      answer: 'expected render path is Renderer.submit_frame -> stage pipeline -> FrameGraphExecutor.execute_node -> BackendSubmitExecutor; expected physics path is VehicleIntent -> KartDynamics.Step -> PhysicsBackendApplyCommand -> backend apply -> telemetry.',
      pass: renderPipelineStagesPass && executeNodeOwnsPassDispatch && physicsBackendContractPass,
    },
    {
      question: 'bypass_paths',
      answer: 'current audit treats direct body mutation, BlockKart player Set* calls, generic primitive authoring, enum-only terrain/decal batches, and unresolved evidence nextFix rows as bypasses.',
      pass: sourceAuditFailures.length === 0,
    },
    {
      question: 'performance_evidence',
      answer: 'budgeted render stress and soak reports must be fresh and pass hard p90/p99/slow-frame/resource budgets without host pacing waiver.',
      pass: renderStress?.status === 'pass' && existsSync(path.join(root, 'target/voplay-render-soak-10m/report.json')),
    },
    {
      question: 'blockkart_boundary',
      answer: 'BlockKart must consume voplay product APIs and telemetry without owning generic authoring or low-level physics/render workarounds.',
      pass: !blockKartPrimitiveAuthoringPresent && !blockKartHudAssemblesLowLevelFacts && !blockKartDirectEntityPhysicsMutation,
    },
    {
      question: 'gate_leak_check',
      answer: 'required source facts, sourceAuditFailures, and unresolved evidence nextFix checks must agree before final readiness can pass.',
      pass: requiredFalseFacts.length === 0 && sourceAuditFailures.length === 0,
    },
  ],
};
const industrialReady = failures.length === 0;
const dirtyProvenance = dirtyProvenanceEntries.length > 0;
const generatedAt = new Date().toISOString();
const freshEvidence = sourceBoundEvidence({
  gate: 'voplay-industrial-readiness',
  generatedAt,
  root,
  repos: [
    { name: 'volang', root },
    { name: 'voplay', root: voplayRoot },
    { name: 'BlockKart', root: blockKartRoot },
  ],
  gateFiles: [
    'scripts/ci/voplay_industrial_readiness.mjs',
    'scripts/ci/repo_roots.mjs',
    'scripts/ci/source_bound_evidence.mjs',
    'scripts/ci/voplay_render_architecture_lint.mjs',
    'scripts/ci/blockkart_product_boundary_strict.mjs',
    'scripts/ci/quickplay_source_audit.mjs',
    'scripts/ci/voplay_render_stress.mjs',
    'scripts/ci/voplay_physics_stress.mjs',
    'scripts/ci/blockkart_baseline.mjs',
    'eng/tasks.toml',
    'eng/ci.toml',
    'eng/project.toml',
  ],
  artifacts: [
    'apps/studio/public/quickplay/blockkart/project.json',
    'apps/studio/public/quickplay/blockkart/deps.json',
    'apps/studio/public/quickplay/blockkart/provenance.json',
    'target/voplay-render-stress-budgeted/report.json',
    'target/voplay-render-soak-10m/report.json',
    'target/voplay-physics-industrial-stress/report.json',
    'target/blockkart-baseline/blockkart-baseline.json',
    'target/blockkart-baseline-restart-50/blockkart-baseline.json',
  ],
});
const readiness = {
  schemaVersion: 1,
  kind: 'voplay.industrialReadinessReport',
  generatedAt,
  freshEvidence,
  industrialReady,
  completionPolicy: 'phase gates may pass; only this Final Gate may report industrialReady true',
  strictMode: !allowNotReady,
  roots: {
    volang: root,
    voplay: voplayRoot,
    blockKart: blockKartRoot,
  },
  dirtyProvenance,
  dirtyProvenanceEntries,
  commits: {
    volang: gitCommit(root),
    voplaySource: gitCommit(voplayRoot),
    blockKartSource: gitCommit(blockKartRoot),
    blockKartArtifact: provenance.value?.project?.commit || null,
    voplayArtifact: voplayArtifact?.commit || null,
    voplayArtifactVersion: voplayArtifact?.version || null,
  },
  sourceFacts,
  sourceFactRequirements,
  sourceAuditFailures,
  firstPrinciplesVerdict,
  evidenceTable,
  gateReports,
  checks,
  failures,
};

mkdirSync(outDir, { recursive: true });
const jsonPath = path.join(outDir, 'report.json');
const mdPath = path.join(outDir, 'report.md');
writeFileSync(jsonPath, `${JSON.stringify(readiness, null, 2)}\n`);
writeFileSync(mdPath, [
  '# voplay Industrial Readiness',
  '',
  `industrialReady: ${industrialReady ? 'true' : 'false'}`,
  `generatedAt: ${readiness.generatedAt}`,
  '',
  '## Commits',
  '',
  `- volang: ${readiness.commits.volang || 'unknown'}`,
  `- voplaySource: ${readiness.commits.voplaySource || 'unknown'}`,
  `- voplayArtifact: ${readiness.commits.voplayArtifact || readiness.commits.voplayArtifactVersion || 'unknown'}`,
  `- blockKartSource: ${readiness.commits.blockKartSource || 'unknown'}`,
  `- blockKartArtifact: ${readiness.commits.blockKartArtifact || 'unknown'}`,
  '',
  '## Source Facts',
  '',
  `- rendererLines: ${rendererLines}`,
  `- frameSubmitLines: ${frameSubmitLines}`,
  `- frameOrchestratorLines: ${frameOrchestratorLines}`,
  `- submitFrameLines: ${submitFrameLines}`,
  `- pipeline3dLines: ${pipeline3dLines}`,
  `- submitFrameHasBeginRenderPass: ${readiness.sourceFacts.submitFrameHasBeginRenderPass}`,
  `- submitFrameHasDirectDraw: ${readiness.sourceFacts.submitFrameHasDirectDraw}`,
  `- submitFrameHasQueueSubmit: ${readiness.sourceFacts.submitFrameHasQueueSubmit}`,
  `- runtimeHasRenderPassBody: ${readiness.sourceFacts.runtimeHasRenderPassBody}`,
  `- runtimeHasDirectDraw: ${readiness.sourceFacts.runtimeHasDirectDraw}`,
  `- runtimeHasQueueSubmit: ${readiness.sourceFacts.runtimeHasQueueSubmit}`,
  `- rendererStillOwnsTargets: ${readiness.sourceFacts.rendererStillOwnsTargets}`,
  `- allPipelineStagesConstructed: ${readiness.sourceFacts.allPipelineStagesConstructed}`,
  `- runtimeUsesExecuteRenderNodeMacro: ${readiness.sourceFacts.runtimeUsesExecuteRenderNodeMacro}`,
  `- batchPlanSceneWired: ${readiness.sourceFacts.batchPlanSceneWired}`,
  `- batchPlanHasZeroBounds: ${readiness.sourceFacts.batchPlanHasZeroBounds}`,
  `- batchPlanUsesSeedWorkloadLod: ${readiness.sourceFacts.batchPlanUsesSeedWorkloadLod}`,
  `- terrainDecalRealEntries: ${readiness.sourceFacts.terrainDecalRealEntries}`,
  `- physicsTrackPositionSurfaceInference: ${readiness.sourceFacts.physicsTrackPositionSurfaceInference}`,
  `- setPoseDirectPhysicsMutation: ${readiness.sourceFacts.setPoseDirectPhysicsMutation}`,
  `- poseResetHelperDirectPhysicsMutation: ${readiness.sourceFacts.poseResetHelperDirectPhysicsMutation}`,
  `- blockKartPrimitiveAuthoringPresent: ${readiness.sourceFacts.blockKartPrimitiveAuthoringPresent}`,
  `- blockKartDirectPlayerPhysicsMutation: ${readiness.sourceFacts.blockKartDirectPlayerPhysicsMutation}`,
  `- blockKartDirectEntityPhysicsMutation: ${readiness.sourceFacts.blockKartDirectEntityPhysicsMutation}`,
  `- soakReportExists: ${readiness.sourceFacts.soakReportExists}`,
  '',
  '## First Principles Verdict',
  '',
  `- status: ${firstPrinciplesVerdict.status}`,
  `- industrialReadyEligible: ${firstPrinciplesVerdict.industrialReadyEligible}`,
  ...firstPrinciplesVerdict.questions.map((item) => `- ${item.pass ? 'pass' : 'fail'} ${item.question}: ${item.answer}`),
  '',
  '## Required Source Facts',
  '',
  ...(sourceFactRequirements.length === 0
    ? ['- none']
    : sourceFactRequirements.map((fact) => `- ${fact.status ? 'pass' : 'fail'} ${fact.code}: ${fact.detail}`)),
  '',
  '## Source Audit Failures',
  '',
  ...(sourceAuditFailures.length === 0
    ? ['- none']
    : sourceAuditFailures.map((failure) => `- [${failure.phase}] ${failure.code}: ${failure.detail}`)),
  '',
  '## Evidence Table',
  '',
  '| Capability | Expected Owner | Actual Owner | Mutation Path | Runtime Path | Bypass Found | Evidence | Gate Coverage | Status | Next Fix |',
  '| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |',
  ...(evidenceTable.length === 0
    ? ['| none | | | | | | | | | |']
    : evidenceTable.map((row) => `| ${mdCell(row.capability)} | ${mdCell(row.expectedOwner)} | ${mdCell(row.actualOwner)} | ${mdCell(row.mutationPath)} | ${mdCell(row.runtimePath)} | ${mdCell(String(row.bypassFound))} | ${mdCell(formatEvidence(row.evidence))} | ${mdCell(row.gateCoverage)} | ${mdCell(row.status)} | ${mdCell(row.nextFix)} |`)),
  '',
  '## Failures',
  '',
  ...(failures.length === 0
    ? ['- none']
    : failures.map((failure) => `- [${failure.phase}] ${failure.code}: ${failure.detail}`)),
  '',
].join('\n'));

console.log(`voplay industrial readiness: industrialReady=${industrialReady} failures=${failures.length}`);
console.log(`voplay industrial readiness: wrote ${path.relative(root, jsonPath)} and ${path.relative(root, mdPath)}`);

if (!industrialReady && !allowNotReady) {
  process.exit(1);
}

function mdCell(value) {
  return String(value ?? '')
    .replaceAll('|', '\\|')
    .replace(/\r?\n/g, '<br>');
}

function formatEvidence(evidence) {
  if (!Array.isArray(evidence)) {
    return JSON.stringify(evidence ?? {});
  }
  return evidence
    .map((item) => item?.ref || item?.path || JSON.stringify(item))
    .join('<br>');
}
