#!/usr/bin/env node
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));
const localVoplayRoot = path.resolve(root, '..', 'voplay');
const localBlockKartRoot = path.resolve(root, '..', 'BlockKart');
const voplayRoot = path.resolve(process.env.VOPLAY_ROOT || (existsSync(path.join(localVoplayRoot, 'vo.mod')) ? localVoplayRoot : path.join(root, 'ci_modules/voplay')));
const blockKartRoot = path.resolve(process.env.BLOCKKART_ROOT || (existsSync(path.join(localBlockKartRoot, 'vo.mod')) ? localBlockKartRoot : path.join(root, 'ci_modules/BlockKart')));

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

function checkReport(file, phase, code, validate) {
  const result = readJson(file);
  const report = {
    path: file,
    exists: result.exists,
    parseError: result.error,
    status: result.value?.status ?? null,
    kind: result.value?.kind ?? null,
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
  addCheck(phase, code, verdict.ok, verdict.detail, { ...report, ...verdict.evidence });
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
const frameGraph = readProjectFile(voplayRoot, 'rust/src/renderer_frame.rs', 'phase-0', 'source.framegraph_exists');
const renderWorld = readProjectFile(voplayRoot, 'rust/src/render_world.rs', 'phase-0', 'source.render_world_exists');
const pipeline3d = readProjectFile(voplayRoot, 'rust/src/pipeline3d.rs', 'phase-0', 'source.pipeline3d_exists');
const vehicle = readProjectFile(voplayRoot, 'scene3d/vehicle.vo', 'phase-0', 'source.vehicle_exists');
const dynamics = readProjectFile(voplayRoot, 'scene3d/kart_dynamics.vo', 'phase-0', 'source.dynamics_exists');
const contactEvent = readProjectFile(voplayRoot, 'scene3d/contact_event.vo', 'phase-0', 'source.contact_event_exists');
const replay = readProjectFile(voplayRoot, 'scene3d/replay.vo', 'phase-0', 'source.replay_exists');
const blockKartWorld = readProjectFile(blockKartRoot, 'world.vo', 'phase-0', 'source.blockkart_world_exists');
const blockKartPrimitiveWorld = readProjectFile(blockKartRoot, 'primitive_world.vo', 'phase-0', 'source.blockkart_primitive_world_exists');
const blockKartBudget = readProjectFile(blockKartRoot, 'performance_budget.vo', 'phase-0', 'source.blockkart_budget_exists');
const rendererRuntime = [renderer, rendererFrameSubmit, rendererFrameOrchestrator].join('\n');
const rendererModuleRuntime = listFiles(path.join(voplayRoot, 'rust', 'src', 'renderer'), '.rs')
  .map((file) => readText(file) || '')
  .join('\n');
const rendererAuditSource = [renderer, rendererFrameSubmit, rendererFrameOrchestrator, rendererModuleRuntime, frameGraph].join('\n');

const submitFrameBody = bodyOfFunction(renderer, 'pub fn submit_frame');
const setPoseBody = bodyOfFunction(vehicle, 'func (v *Vehicle) SetPose');
const renderBatchPlannerBuildBody = bodyOfFunction(renderWorld, 'pub fn build');
const renderBatchPlannerSelectLodBody = bodyOfFunction(renderWorld, 'fn select_lod');
const rendererLines = lineCount(renderer);
const frameSubmitLines = lineCount(rendererFrameSubmit);
const frameOrchestratorLines = lineCount(rendererFrameOrchestrator);
const submitFrameLines = lineCount(submitFrameBody);
const pipeline3dLines = lineCount(pipeline3d);
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
const renderFramePipelineRuntimeCall = rendererFrameOrchestrator.includes('RenderFramePipeline::from_frame_metrics')
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
const replayRecordsBackendApplyHash = replay.includes('BackendApplyHash') || replay.includes('BackendApplyCommandHash');
const physicsBackendContractPass = !physicsTrackPositionSurfaceInference
  && !setPoseDirectPhysicsMutation
  && replayRecordsBackendApplyHash;
const blockKartPrimitiveAuthoringPresent = /type BlockKartPrimitiveScene|primitive3d\.NewLayer|SpawnPreparedMapPrimitiveLayers|AddStatic|AddDetail|spawnPrimitiveRoadBoxPhysics/.test(blockKartPrimitiveWorld);
const blockKartHudAssemblesLowLevelFacts = /PrimitiveStats\(|WheelState\(|VehicleGrounded|WheelMaxSlip|PrimitiveVisibleChunks/.test(blockKartWorld);
const blockKartVisualReadsMutableVehicleState = /w\.vehicle\.(SteerAngle|WheelSpin)/.test(blockKartPrimitiveWorld);
const blockKartDirectVehiclePoseCalls = /w\.vehicle\.SetPose\(/.test(blockKartWorld);
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

addCheck('phase-0', 'report.phase_claims_guarded', scanScriptIndustrialClaims().length === 0, 'phase scripts do not claim industrial readiness', { offenders: scanScriptIndustrialClaims() });
addCheck('phase-1', 'renderer.submit_frame_size', submitFrameLines <= 220, 'submit_frame is only an orchestration body', { lines: submitFrameLines, budget: 220 });
addCheck('phase-1', 'renderer.file_size', rendererLines <= 1600, 'renderer.rs is reduced to orchestration and backend glue', { lines: rendererLines, budget: 1600 });
addCheck('phase-1', 'renderer.frame_submit_size', frameSubmitLines <= 700, 'frame_submit.rs is reduced to frame orchestration and pass registration glue', { lines: frameSubmitLines, budget: 700 });
addCheck('phase-1', 'renderer.frame_orchestrator_no_pass_body', !rendererFrameOrchestrator.includes('begin_render_pass') && !rendererFrameOrchestrator.includes('queue.submit') && !rendererFrameOrchestrator.includes('.draw_models(') && !rendererFrameOrchestrator.includes('draw_main_and_water('), 'frame_orchestrator.rs contains orchestration only and no pass bodies', { lines: frameOrchestratorLines, forbidden: ['begin_render_pass', 'queue.submit', 'draw_models', 'draw_main_and_water'] });
addCheck('phase-1', 'renderer.no_begin_render_pass_in_submit_frame', !submitFrameBody.includes('begin_render_pass'), 'submit_frame contains no render pass bodies', { tokens: ['begin_render_pass'] });
addCheck('phase-1', 'renderer.no_begin_render_pass_in_runtime_glue', !runtimeHasRenderPassBody, 'renderer runtime glue contains no render pass bodies', { tokens: renderPassBodyTokens });
addCheck('phase-1', 'renderer.no_direct_draw_in_submit_frame', !/\.draw_models\(|draw_main_and_water\(|\.draw\(/.test(submitFrameBody), 'submit_frame contains no direct draw calls', { tokens: ['draw_models', 'draw_main_and_water'] });
addCheck('phase-1', 'renderer.no_direct_draw_in_runtime_glue', !runtimeHasDirectDraw, 'renderer runtime glue contains no direct draw calls', { tokens: directDrawTokens });
addCheck('phase-1', 'renderer.no_queue_submit_body', !submitFrameBody.includes('queue.submit'), 'backend submission is outside submit_frame pass body', { tokens: ['queue.submit'] });
addCheck('phase-1', 'renderer.no_queue_submit_in_runtime_glue', !runtimeHasQueueSubmit, 'renderer runtime glue delegates backend submission to a pass executor module', { tokens: ['queue.submit'] });
addCheck('phase-1', 'renderer.no_legacy_execute_pass', !rendererRuntime.includes('.execute_pass('), 'renderer runtime does not use legacy execute_pass', {});
addCheck('phase-1', 'renderer.framegraph_node_runtime', rendererRuntime.includes('execute_node(') || rendererRuntime.includes('execute_render_node!'), 'renderer routes runtime pass work through execute_node', {});
addCheck('phase-1', 'framegraph.pass_node_contract', ['RenderPassNode', 'RenderPassWorkload', 'execute_node', 'transient_writes'].every((token) => frameGraph.includes(token)), 'FrameGraph exposes pass node execution, workload, and transient write contracts', {});
addCheck('phase-1', 'framegraph.pass_modules_exist', renderPassModuleFiles.every((file) => existsSync(path.join(voplayRoot, file))), 'render pass implementations live in dedicated pass executor modules', { expected: renderPassModuleFiles });
addCheck('phase-1', 'framegraph.resource_registry_exists', frameGraph.includes('struct RenderResourceRegistry'), 'RenderResourceRegistry exists', {});
addCheck('phase-1', 'renderer.targets_owned_by_registry', !rendererStillOwnsTargets, 'renderer target lifecycle is owned by RenderResourceRegistry instead of ad hoc RendererTargetRegistry fields', {});

addCheck('phase-2', 'batch_plan.contract_exists', ['struct RenderBatchPlan', 'struct RenderBatchPlanner', 'struct RenderWorldChunk'].every((token) => renderWorld.includes(token)), 'RenderBatchPlan and RenderWorldChunk contracts exist', {});
addCheck('phase-2', 'batch_plan.drives_submission', !runtimeHasDirectDraw && rendererRuntime.includes('RenderBatchPlanner::build') && rendererRuntime.includes('planned_model_draws') && rendererRuntime.includes('planned_primitive_draws') && rendererRuntime.includes('planned_water_draws'), 'RenderBatchPlan owns real draw submission routing', {});
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
addCheck('phase-3', 'physics.intent_chain', vehicle.includes('v.Dynamics.Step(') && vehicle.includes('BuildPhysicsBackendApplyCommand'), 'VehicleIntent reaches KartDynamics and PhysicsBackendApplyCommand', {});
addCheck('phase-3', 'physics.backend_command_contract', ['Wheels []PhysicsBackendWheelCommand', 'BodyForce voplay.Vec3', 'DragForce float64', 'Downforce float64', 'WaterLift float64', 'AirControl float64', 'WallGrip float64', 'RailGrip float64', 'DebugHash int'].every((token) => dynamics.includes(token)), 'PhysicsBackendApplyCommand covers wheel, body, drag, downforce, water, air, wall, rail, and debug hash', {});
addCheck('phase-3', 'physics.backend_adapter_only_wheel_control', backendAdapterBody.includes('SetRaycastVehicleWheelControl') && !setPoseBody.includes('SetRaycastVehicleWheelControl'), 'raycast wheel control is only written by backend adapter paths', {});
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
    runtimeCall: lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'RenderFramePipeline::from_frame_metrics'),
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
  !setPoseDirectPhysicsMutation,
  'Vehicle.SetPose/reset/respawn/recovery/sleep route pose and velocity mutation through backend helper contracts',
  {
    setPose: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, 'func (v *Vehicle) SetPose'),
    directMutationTokens: ['Body.SetPosition(', 'Body.SetVelocity(', 'Body.Physics.velocity'],
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
  'blockkart_product_boundary',
  !blockKartPrimitiveAuthoringPresent
    && !blockKartHudAssemblesLowLevelFacts
    && !blockKartVisualReadsMutableVehicleState
    && !blockKartDirectVehiclePoseCalls,
  'BlockKart owns product content/rules/HUD consumption only and does not author generic engine primitives, physics poses, or low-level facts',
  {
    primitiveAuthoring: lineEvidence(path.join(blockKartRoot, 'primitive_world.vo'), blockKartPrimitiveWorld, 'type BlockKartPrimitiveScene'),
    hudFacts: lineEvidence(path.join(blockKartRoot, 'world.vo'), blockKartWorld, 'PrimitiveStats()'),
    visualMutableState: lineEvidence(path.join(blockKartRoot, 'primitive_world.vo'), blockKartPrimitiveWorld, 'w.vehicle.SteerAngle'),
    directSetPose: lineEvidence(path.join(blockKartRoot, 'world.vo'), blockKartWorld, 'w.vehicle.SetPose'),
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
    ? 'Renderer.submit_frame -> run_frame_orchestrator -> decode_frame_commands -> RenderFramePipeline::from_frame_metrics -> last_frame_pipeline'
    : 'Renderer.submit_frame -> submit_frame_inner -> FrameSubmitOrchestrator::run -> run_frame_orchestrator',
  bypassFound: !renderPipelineStagesPass || rendererFrameOrchestrator.includes('let mut reader = StreamReader'),
  evidence: renderPipelineStagesPass ? [
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_decode.rs'), rendererFrameDecode, 'pub(super) fn decode_frame_commands'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_decode.rs'), rendererFrameDecode, 'StreamReader::new'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_decode.rs'), rendererFrameDecode, 'RenderFrameDecode {'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'RenderFramePipeline {'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'RenderFramePipeline::from_frame_metrics'),
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
    ? 'finish BlockKart product-boundary migration so scoped product code consumes voplay engine APIs read-only'
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
    ? 'run_frame_orchestrator -> frame_graph.node -> FrameGraphExecutor.execute_node -> FramePassDispatcher::execute'
    : 'execute_render_node! -> FrameGraphExecutor.execute_node',
  bypassFound: runtimeUsesExecuteRenderNodeMacro || executeNodeTakesAdHocClosure,
  evidence: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro ? [
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'trait RenderPassNodeDispatcher'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'pub(crate) fn execute_node'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/pass_dispatch.rs'), readText(path.join(voplayRoot, 'rust/src/renderer/pass_dispatch.rs')) || '', 'impl RenderPassNodeDispatcher for FramePassDispatcher'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, '.execute_node(&main_node'),
  ] : [
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs'), rendererFrameOrchestrator, 'macro_rules! execute_render_node'),
    lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame.rs'), frameGraph, 'F: FnOnce'),
  ],
  gateCoverage: 'source_fact.framegraph_dispatch_owns_pass_execution',
  status: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro,
  nextFix: executeNodeOwnsPassDispatch && !runtimeUsesExecuteRenderNodeMacro
    ? 'finish BlockKart product-boundary migration'
    : 'register pass executors by RenderPassKind and make execute_node dispatch through node metadata',
});
addEvidenceRow({
  capability: 'Batch planning and visibility',
  expectedOwner: 'RenderBatchPlanner',
  actualOwner: batchPlanContractPass
    ? 'RenderBatchPlanner owns model bounds, primitive chunk facts, distance LOD, and culling counters'
    : 'PrimitiveRenderWorld performs real culling before RenderBatchPlanner rebuilds placeholder plan facts',
  mutationPath: batchPlanContractPass
    ? 'PrimitiveRenderWorld exposes PrimitiveChunkBatchInfo; RenderBatchPlanner::build routes planned batches after camera and distance decisions'
    : 'RenderBatchPlanner::build pushes chunks with zero bounds and seed/workload LOD',
  runtimePath: batchPlanContractPass
    ? 'run_frame_orchestrator -> collect_scene_primitive_draws_with_chunk_info(None) -> RenderBatchPlanner::build(camera, quality) -> planned_* batches'
    : 'run_frame_orchestrator -> RenderBatchPlanner::build -> planned_* batches',
  bypassFound: !batchPlanContractPass,
  evidence: batchPlanContractPass ? [
    lineEvidence(path.join(voplayRoot, 'rust/src/primitive_scene.rs'), readText(path.join(voplayRoot, 'rust/src/primitive_scene.rs')) || '', 'PrimitiveChunkBatchInfo'),
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'bounds_from_model_matrix'),
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'plan.frustum_culled_chunks += 1'),
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'plan.distance_culled_chunks += 1'),
  ] : [
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'center: Vec3::ZERO'),
    lineEvidence(path.join(voplayRoot, 'rust/src/render_world.rs'), renderWorld, 'fn select_lod'),
  ],
  gateCoverage: 'source_fact.batch_plan_real_bounds, source_fact.batch_plan_real_lod_inputs, source_fact.batch_plan_real_culling_counters',
  status: batchPlanContractPass,
  nextFix: batchPlanContractPass
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
    ? 'extend native RaycastVehicleState with backend contact material IDs to remove the remaining sync-time Track.SurfaceAt bridge'
    : 'introduce backend pose/reset/recovery helpers and make track-position surface inference compat-only',
});
addEvidenceRow({
  capability: 'BlockKart product boundary',
  expectedOwner: 'voplay generic authoring and structured telemetry',
  actualOwner: 'BlockKart primitive_world.vo and world.vo',
  mutationPath: 'BlockKart builds primitive layers/materials/colliders and assembles low-level HUD fields',
  runtimePath: 'World.buildPrimitiveLevel/spawnPrimitiveTrackPhysics/hudState/currentPrimitiveKartVisualState',
  bypassFound: blockKartPrimitiveAuthoringPresent || blockKartHudAssemblesLowLevelFacts || blockKartVisualReadsMutableVehicleState,
  evidence: [
    lineEvidence(path.join(blockKartRoot, 'primitive_world.vo'), blockKartPrimitiveWorld, 'type BlockKartPrimitiveScene'),
    lineEvidence(path.join(blockKartRoot, 'world.vo'), blockKartWorld, 'PrimitiveStats()'),
  ],
  gateCoverage: 'source_fact.blockkart_product_boundary',
  status: !blockKartPrimitiveAuthoringPresent && !blockKartHudAssemblesLowLevelFacts && !blockKartVisualReadsMutableVehicleState,
  nextFix: 'move primitive/chunk/collider/surface authoring to voplay and make BlockKart consume read-only telemetry',
});

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
  const ok = report.status === 'pass' && allScenesPass(report) && missing.length === 0;
  return {
    ok,
    detail: 'render stress budgeted report passes all required industrial scenes',
    evidence: {
      status: report.status,
      missingScenes: missing,
      sceneCount: report.scenes?.length || 0,
      summary: report.summary || null,
    },
  };
});

checkReport(path.join(root, 'target/voplay-render-soak-10m/report.json'), 'phase-5', 'gate.render_soak_10m', (report) => ({
  ok: report.status === 'pass' && allScenesPass(report),
  detail: 'ten minute render soak report exists and passes',
  evidence: {
    status: report.status,
    sceneCount: report.scenes?.length || 0,
    summary: report.summary || null,
  },
}));

checkReport(path.join(root, 'target/voplay-physics-industrial-stress/report.json'), 'phase-5', 'gate.physics_industrial_stress', (report) => {
  const required = ['skidpad', 'slalom', 'drift-turbo', 'boost-pad', 'offroad-transition', 'jump-landing', 'wall-impact', 'rail-ride', 'wall-ride', 'water-skim', 'multi-vehicle-scripted-soak'];
  const names = scenarioNames(report);
  const missing = required.filter((name) => !names.has(name));
  const replayOk = report.replay?.status === 'pass' && Number(report.replay?.driftMeters || 0) <= 0.01;
  const ok = report.status === 'pass' && allIndustrialPhysicsSamplesClean(report) && missing.length === 0 && replayOk;
  return {
    ok,
    detail: 'industrial physics stress passes scenario, fallback, invalid sample, and replay drift gates',
    evidence: {
      status: report.status,
      missingScenarios: missing,
      scenarioCount: report.scenarios?.length || 0,
      replay: report.replay || null,
      summary: report.summary || null,
    },
  };
});

checkReport(path.join(root, 'target/quickplay-source-audit/quickplay-source-audit.json'), 'phase-5', 'gate.quickplay_source_audit', (report) => ({
  ok: report.status === 'ok' && Array.isArray(report.issues) && report.issues.length === 0,
  detail: 'quickplay source audit passes',
  evidence: {
    status: report.status,
    issueCount: report.issues?.length ?? null,
  },
}));

checkReport(path.join(root, 'target/blockkart-baseline/blockkart-baseline.json'), 'phase-5', 'gate.blockkart_baseline_report', (report) => ({
  ok: report.status === 'pass' || report.status === 'ok' || report.lifecycle?.reachedRunning === true,
  detail: 'BlockKart baseline report reaches running state',
  evidence: {
    status: report.status || null,
    lifecycle: report.lifecycle || null,
  },
}));

checkReport(path.join(root, 'target/blockkart-baseline-restart-50/blockkart-baseline.json'), 'phase-5', 'gate.blockkart_restart_50_report', (report) => ({
  ok: (report.status === 'pass' || report.status === 'ok' || report.lifecycle?.reachedRunning === true) && Number(report.restart?.failures || 0) === 0,
  detail: 'BlockKart restart-50 report passes without restart failures',
  evidence: {
    status: report.status || null,
    restart: report.restart || null,
    lifecycle: report.lifecycle || null,
  },
}));

const provenancePath = path.join(root, 'apps/studio/public/quickplay/blockkart/provenance.json');
const provenance = readJson(provenancePath);
const voplayArtifact = provenance.value?.dependencies?.find((dep) => dep.module === 'github.com/vo-lang/voplay') || null;
addCheck('phase-5', 'artifact.provenance_exists', provenance.exists && !provenance.error, 'quickplay provenance JSON exists and parses', { path: provenancePath, parseError: provenance.error });
addCheck('phase-5', 'artifact.voplay_commit_recorded', Boolean(voplayArtifact?.commit), 'quickplay provenance records the voplay artifact source commit', { version: voplayArtifact?.version || null, cacheDir: voplayArtifact?.cacheDir || null, commit: voplayArtifact?.commit || null });
addCheck('phase-5', 'artifact.blockkart_commit_recorded', Boolean(provenance.value?.project?.commit), 'quickplay provenance records the BlockKart source commit', { commit: provenance.value?.project?.commit || null });

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
  runtimeUsesExecutePass: rendererRuntime.includes('.execute_pass('),
  runtimeUsesExecuteNode: rendererRuntime.includes('execute_node(') || runtimeUsesExecuteRenderNodeMacro,
  runtimeUsesExecuteRenderNodeMacro,
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
  batchPlanSceneWired: renderer.includes('RenderBatchPlanner::build') && renderer.includes('render_batch_plan.visible_objects'),
  batchPlanHasZeroBounds,
  batchPlanUsesSeedWorkloadLod,
  frustumCullingCounterMutated,
  distanceCullingCounterMutated,
  physicsTrackPositionSurfaceInference,
  setPoseDirectPhysicsMutation,
  replayRecordsBackendApplyHash,
  blockKartPrimitiveAuthoringPresent,
  blockKartHudAssemblesLowLevelFacts,
  blockKartVisualReadsMutableVehicleState,
  blockKartDirectVehiclePoseCalls,
  budgetedRenderStressStatus: renderStress?.status || null,
  soakReportExists: existsSync(path.join(root, 'target/voplay-render-soak-10m/report.json')),
  requiredFalseFacts,
};
const failures = checks.filter((check) => check.status !== 'pass');
const industrialReady = failures.length === 0;
const readiness = {
  schemaVersion: 1,
  kind: 'voplay.industrialReadinessReport',
  generatedAt: new Date().toISOString(),
  industrialReady,
  completionPolicy: 'phase gates may pass; only this Final Gate may report industrialReady true',
  strictMode: !allowNotReady,
  roots: {
    volang: root,
    voplay: voplayRoot,
    blockKart: blockKartRoot,
  },
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
  `- batchPlanHasZeroBounds: ${readiness.sourceFacts.batchPlanHasZeroBounds}`,
  `- batchPlanUsesSeedWorkloadLod: ${readiness.sourceFacts.batchPlanUsesSeedWorkloadLod}`,
  `- physicsTrackPositionSurfaceInference: ${readiness.sourceFacts.physicsTrackPositionSurfaceInference}`,
  `- setPoseDirectPhysicsMutation: ${readiness.sourceFacts.setPoseDirectPhysicsMutation}`,
  `- blockKartPrimitiveAuthoringPresent: ${readiness.sourceFacts.blockKartPrimitiveAuthoringPresent}`,
  `- soakReportExists: ${readiness.sourceFacts.soakReportExists}`,
  '',
  '## Required Source Facts',
  '',
  ...(sourceFactRequirements.length === 0
    ? ['- none']
    : sourceFactRequirements.map((fact) => `- ${fact.status ? 'pass' : 'fail'} ${fact.code}: ${fact.detail}`)),
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
