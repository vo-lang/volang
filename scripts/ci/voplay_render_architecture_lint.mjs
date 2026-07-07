#!/usr/bin/env node
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const gate = argValue('--gate') || 'voplay-render-architecture-lint';
const outDir = path.resolve(argValue('--out-dir') || process.env.VOPLAY_RENDER_ARCHITECTURE_LINT_OUT_DIR || path.join(root, 'target', gate));

const issues = [];

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : process.argv[index + 1] ?? '';
}

function readProjectFile(projectRoot, relativePath) {
  const file = path.join(projectRoot, relativePath);
  if (!existsSync(file)) {
    fail(`${relativePath} is missing under ${projectRoot}`);
  }
  return readFileSync(file, 'utf8');
}

function readProjectFileIfExists(projectRoot, relativePath) {
  const file = path.join(projectRoot, relativePath);
  return existsSync(file) ? readFileSync(file, 'utf8') : '';
}

function fail(message) {
  console.error(`voplay architecture lint: ${message}`);
  process.exit(1);
}

function normalizeIssue(code, detail, metadata = {}) {
  const evidence = metadata.evidence ?? {};
  return {
    code,
    severity: metadata.severity ?? 'P1',
    owner: metadata.owner ?? 'voplay/render',
    file: metadata.file ?? evidence.file ?? evidence.path ?? null,
    line: metadata.line ?? evidence.line ?? null,
    reason: metadata.reason ?? detail,
    requiredFix: metadata.requiredFix ?? 'Move this behavior behind the owning subsystem contract and add source-bound behavior coverage.',
    detail,
    evidence,
  };
}

function check(condition, code, detail, metadata = {}) {
  if (!condition) {
    issues.push(normalizeIssue(code, detail, metadata));
  }
}

function addSourceIssue(code, severity, owner, hit, reason, requiredFix) {
  issues.push(normalizeIssue(code, reason, {
    severity,
    owner,
    file: hit.path ?? hit.file ?? null,
    line: hit.line ?? null,
    requiredFix,
    evidence: hit,
  }));
}

function writeReport(status) {
  const generatedAt = new Date().toISOString();
  mkdirSync(outDir, { recursive: true });
  writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify({
    schemaVersion: 1,
    kind: 'voplay.renderArchitectureLintReport',
    gate,
    status,
    generatedAt,
    freshEvidence: sourceBoundEvidence({
      gate,
      generatedAt,
      root,
      repos: [
        { name: 'volang', root },
        { name: 'voplay', root: voplayRoot },
        { name: 'BlockKart', root: blockKartRoot },
      ],
      gateFiles: [
        'scripts/ci/voplay_render_architecture_lint.mjs',
        'scripts/ci/repo_roots.mjs',
        'scripts/ci/source_bound_evidence.mjs',
        'eng/tasks.toml',
        'eng/ci.toml',
      ],
      artifacts: [],
    }),
    issueCount: issues.length,
    issues,
  }, null, 2)}\n`);
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
    if (source[i] === '{') depth++;
    if (source[i] === '}') {
      depth--;
      if (depth === 0) {
        return source.slice(brace + 1, i);
      }
    }
  }
  return '';
}

function constructsRuntimeStage(source, token) {
  return source.split(/\r?\n/).some((line) => {
    if (line.includes(`struct ${token}`)) {
      return false;
    }
    return line.includes(`${token} {`) || line.includes(`${token}::default(`);
  });
}

function listFiles(projectRoot, extension) {
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
      if (file.endsWith(extension)) {
        files.push(file);
      }
    }
  };
  visit(projectRoot);
  return files;
}

const renderer = readProjectFile(voplayRoot, 'rust/src/renderer.rs');
const rendererFrameSubmit = readProjectFile(voplayRoot, 'rust/src/renderer/frame_submit.rs');
const rendererFrameOrchestrator = readProjectFile(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs');
const rendererFrameDecode = readProjectFile(voplayRoot, 'rust/src/renderer/frame_decode.rs');
const rendererFrameDecodeRuntime = readProjectFileIfExists(voplayRoot, 'rust/src/renderer/frame_decode_runtime.rs');
const rendererFrameTransaction = readProjectFileIfExists(voplayRoot, 'rust/src/renderer/frame_transaction.rs');
const rendererFrameTransactionBuilder = readProjectFileIfExists(voplayRoot, 'rust/src/renderer/frame_transaction_builder.rs');
const mainTransparentPass = readProjectFile(voplayRoot, 'rust/src/renderer/main_transparent_pass.rs');
const rendererFrameOrchestratorRuntime = readProjectFileIfExists(voplayRoot, 'rust/src/renderer/frame_orchestrator_runtime.rs');
const rendererFrame2dUpload = readProjectFile(voplayRoot, 'rust/src/renderer/frame_2d_upload.rs');
const rendererFrameGraphPlan = readProjectFile(voplayRoot, 'rust/src/renderer/frame_graph_plan.rs');
const rendererFramePassSequence = readProjectFile(voplayRoot, 'rust/src/renderer/frame_pass_sequence.rs');
const rendererPassDispatch = readProjectFileIfExists(voplayRoot, 'rust/src/renderer/pass_dispatch.rs');
const rendererFramePerfFinalize = readProjectFile(voplayRoot, 'rust/src/renderer/frame_perf_finalize.rs');
const rendererFrameSurface = readProjectFile(voplayRoot, 'rust/src/renderer/frame_surface.rs');
const rendererFrameWorkloadPlan = readProjectFile(voplayRoot, 'rust/src/renderer/frame_workload_plan.rs');
const frameGraph = readProjectFile(voplayRoot, 'rust/src/renderer_frame.rs');
const frameGraphResourceRegistry = readProjectFileIfExists(voplayRoot, 'rust/src/renderer_frame/resource_registry.rs');
const renderWorld = readProjectFile(voplayRoot, 'rust/src/render_world.rs');
const renderWorldStore = readProjectFileIfExists(voplayRoot, 'rust/src/render_world/store.rs');
const renderWorldTests = readProjectFileIfExists(voplayRoot, 'rust/src/render_world/tests.rs');
const pipeline3d = readProjectFile(voplayRoot, 'rust/src/pipeline3d.rs');
const primitivePipeline = readProjectFile(voplayRoot, 'rust/src/primitive_pipeline.rs');
const primitivePipelineRuntime = readProjectFileIfExists(voplayRoot, 'rust/src/primitive_pipeline/runtime.rs');
const primitivePipelineTests = readProjectFileIfExists(voplayRoot, 'rust/src/primitive_pipeline/tests.rs');
const pipelineCache = readProjectFile(voplayRoot, 'rust/src/pipeline3d/pipeline_cache.rs');
const pipeline3dOwnerSource = listFiles(path.join(voplayRoot, 'rust/src/pipeline3d'), '.rs')
  .map((file) => readFileSync(file, 'utf8'))
  .join('\n');
const rendererPerf = readProjectFile(voplayRoot, 'rust/src/renderer_perf.rs');
const perfDiagnostics = readProjectFile(voplayRoot, 'perf_diagnostics.vo');
const vehicle = readProjectFile(voplayRoot, 'scene3d/vehicle.vo');
const contactEvent = readProjectFile(voplayRoot, 'scene3d/contact_event.vo');
const kartDynamics = readProjectFile(voplayRoot, 'scene3d/kart_dynamics.vo');
const vehicleTelemetry = readProjectFile(voplayRoot, 'scene3d/vehicle_telemetry.vo');
const vehicleIdleSleep = readProjectFile(voplayRoot, 'scene3d/vehicle_idle_sleep.vo');
const physicsStress = readProjectFile(voplayRoot, 'examples/physics_stress/main.vo');
const replay = readProjectFile(voplayRoot, 'scene3d/replay.vo');
const blockKartBudget = readProjectFile(blockKartRoot, 'performance_budget.vo');
const blockKartTheme = readProjectFile(blockKartRoot, 'theme.vo');
const blockKartWorld = readProjectFile(blockKartRoot, 'world.vo');
const blockKartPrimitiveWorld = readProjectFile(blockKartRoot, 'primitive_world.vo');
const blockKartProductFoundation = readProjectFile(blockKartRoot, 'product_foundation.vo');
const blockKartVoFiles = listFiles(blockKartRoot, '.vo')
  .map((file) => ({
    file,
    rel: path.relative(blockKartRoot, file),
    source: readFileSync(file, 'utf8'),
  }));

const rendererRuntime = [
  renderer,
  rendererFrameSubmit,
  rendererFrameOrchestrator,
  rendererFrame2dUpload,
  rendererFrameGraphPlan,
  rendererFramePassSequence,
  rendererPassDispatch,
  rendererFramePerfFinalize,
  rendererFrameSurface,
  rendererFrameWorkloadPlan,
].join('\n');
const frameGraphAuditSource = [frameGraph, frameGraphResourceRegistry].join('\n');
const rendererAuditSource = [rendererRuntime, frameGraphAuditSource].join('\n');
const renderWorldAuditSource = [renderWorld, renderWorldStore, renderWorldTests].join('\n');
const primitivePipelineAuditSource = [primitivePipeline, primitivePipelineRuntime, primitivePipelineTests].join('\n');
const rendererDecodeAuditSource = [
  rendererFrameDecode,
  rendererFrameDecodeRuntime,
  rendererFrameTransactionBuilder,
].join('\n');
const scene3dAuditSource = listFiles(path.join(voplayRoot, 'scene3d'), '.vo')
  .map((file) => readFileSync(file, 'utf8'))
  .join('\n');
const renderBatchPlannerBuildBody = bodyOfFunction(renderWorldAuditSource, 'pub fn build');
const renderBatchPlannerSelectLodBody = bodyOfFunction(renderWorldAuditSource, 'fn select_lod');
const runtimePart = (source) => source.split('#[cfg(test)]')[0] || source;
function sourceHits(source, relativePath, pattern) {
  const hits = [];
  const lines = source.split(/\r?\n/);
  for (let i = 0; i < lines.length; i++) {
    if (pattern.test(lines[i])) {
      hits.push({ path: relativePath, line: i + 1, text: lines[i].trim() });
    }
  }
  return hits;
}

function sourceHitsInFiles(files, pattern, allow = () => false) {
  const hits = [];
  for (const file of files) {
    const relativePath = path.relative(voplayRoot, file).split(path.sep).join('/');
    if (allow(relativePath)) {
      continue;
    }
    const source = runtimePart(readFileSync(file, 'utf8'));
    const lines = source.split(/\r?\n/);
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (pattern.test(line)) {
        hits.push({ path: relativePath, line: i + 1, text: line.trim() });
      }
    }
  }
  return hits;
}

function meaningfulOwnerModule(source) {
  const body = bodyOfFunction(source, 'impl ');
  const functionCount = (body.match(/\b(?:pub\(crate\)\s+)?fn\s+\w+/g) || []).length;
  const realSideEffect = /\b(begin_render_pass|draw|draw_indexed|set_pipeline|set_bind_group|set_vertex_buffer|set_index_buffer|write_buffer|queue\.write|upload_|cache|report|stats|telemetry|bind_group|prepare_buffer|create_buffer|submit)\b|\.store\(/.test(body);
  const facadeReturn = functionCount <= 1 && (
    /return\s+[^;]+;\s*\}\s*$/.test(body.trim())
    || /^\s*pub\(crate\)\s+fn\s+\w+[\s\S]*\{\s*(?:[A-Za-z0-9_:().&*\s]+|decals\.len\(\))\s*\}\s*$/.test(body.trim())
  );
  return realSideEffect && !facadeReturn;
}
const renderFileBudgets = {
  'rust/src/renderer/frame_orchestrator.rs': { lines: rendererFrameOrchestrator.split(/\r?\n/).length, budget: 300 },
  'rust/src/renderer/frame_2d_upload.rs': { lines: rendererFrame2dUpload.split(/\r?\n/).length, budget: 160 },
  'rust/src/renderer/frame_decode.rs': { lines: rendererFrameDecode.split(/\r?\n/).length, budget: 700 },
  'rust/src/renderer/frame_decode_runtime.rs': { lines: rendererFrameDecodeRuntime.split(/\r?\n/).length, budget: 700 },
  'rust/src/renderer/frame_graph_plan.rs': { lines: rendererFrameGraphPlan.split(/\r?\n/).length, budget: 300 },
  'rust/src/renderer/frame_pass_sequence.rs': { lines: rendererFramePassSequence.split(/\r?\n/).length, budget: 300 },
  'rust/src/renderer/frame_perf_finalize.rs': { lines: rendererFramePerfFinalize.split(/\r?\n/).length, budget: 300 },
  'rust/src/renderer/frame_surface.rs': { lines: rendererFrameSurface.split(/\r?\n/).length, budget: 80 },
  'rust/src/renderer/frame_transaction.rs': { lines: rendererFrameTransaction.split(/\r?\n/).length, budget: 700 },
  'rust/src/renderer/frame_transaction_builder.rs': { lines: rendererFrameTransactionBuilder.split(/\r?\n/).length, budget: 900 },
  'rust/src/renderer/frame_workload_plan.rs': { lines: rendererFrameWorkloadPlan.split(/\r?\n/).length, budget: 300 },
  'rust/src/renderer_frame.rs': { lines: frameGraph.split(/\r?\n/).length, budget: 900 },
  'rust/src/renderer_frame/resource_registry.rs': { lines: frameGraphResourceRegistry.split(/\r?\n/).length, budget: 360 },
  'rust/src/render_world.rs': { lines: renderWorld.split(/\r?\n/).length, budget: 700 },
  'rust/src/primitive_pipeline.rs': { lines: primitivePipeline.split(/\r?\n/).length, budget: 700 },
  'rust/src/pipeline3d/pipeline_cache.rs': { lines: pipelineCache.split(/\r?\n/).length, budget: 500 },
};
check(rendererFrameOrchestratorRuntime.trim().length === 0, 'render.orchestrator_runtime_sidecar', 'frame_orchestrator_runtime.rs must not exist; frame orchestration belongs to frame_orchestrator.rs and named owner modules');
const ownerModuleFiles = [
  'rust/src/pipeline3d/shader_library.rs',
  'rust/src/pipeline3d/material_binder.rs',
  'rust/src/pipeline3d/mesh_submitter.rs',
  'rust/src/pipeline3d/skinned_submitter.rs',
  'rust/src/pipeline3d/terrain_submitter.rs',
  'rust/src/pipeline3d/primitive_submitter.rs',
  'rust/src/pipeline3d/water_submitter.rs',
  'rust/src/pipeline3d/decal_submitter.rs',
];
const ownerModuleFacts = ownerModuleFiles.map((relativePath) => {
  const source = readProjectFile(voplayRoot, relativePath);
  const base = path.basename(relativePath, '.rs');
  const owner = base
    .split('_')
    .map((part) => `${part.slice(0, 1).toUpperCase()}${part.slice(1)}`)
    .join('');
  const hasProductionMethod = /\bpub(?:\(crate\)|\(super\))?\s+fn\s+(prepare|upload|draw|bind|submit|load|compile|create|resolve|cache)\w*\b/.test(source)
    || /\bfn\s+(prepare|upload|draw|bind|submit|load|compile|create|resolve|cache)\w*\b/.test(source);
  const calledByProduction = [pipeline3d, pipelineCache, pipeline3dOwnerSource, rendererRuntime]
    .some((productionSource) => productionSource.includes(`${owner}::`) || productionSource.includes(`${owner} {`) || productionSource.includes(`${base}::`));
  return { path: relativePath, owner, hasProductionMethod, calledByProduction };
});
const receiverMaskDeclaredConditionally = /if\s+post_depth_active\s*\{[\s\S]*declare_target\(\s*RES_RECEIVER_MASK/.test(rendererRuntime);
const surfacePropsDeclaredConditionally = /if\s+post_depth_active\s*\{[\s\S]*declare_target\(\s*RES_SURFACE_PROPS/.test(rendererRuntime);
const mainOpaqueWritesAuxResources = /RenderPassKind::MainOpaque[\s\S]*RES_RECEIVER_MASK[\s\S]*RES_SURFACE_PROPS/.test(rendererRuntime);
const postReadsAuxResources = /RenderPassKind::Post[\s\S]*RES_RECEIVER_MASK[\s\S]*RES_SURFACE_PROPS/.test(rendererRuntime);
const transparentSortReady = mainTransparentPass.includes('RenderDrawItem')
  && /depth/i.test(mainTransparentPass)
  && /sort_by|sort_unstable_by/.test(mainTransparentPass)
  && /stable/i.test(mainTransparentPass);
const residentVisibilityRemovesHidden = !/if\s+!update\.visible\s*\{\s*return;\s*\}/.test(bodyOfFunction(primitivePipelineAuditSource, 'pub fn upsert_instance'));
const flushResidentRebuildBody = bodyOfFunction(primitivePipelineAuditSource, 'pub fn flush_resident_rebuild_queue');
const dirtyRangeDrivesUpload = /range\.dirty_start[\s\S]*range\.dirty_count[\s\S]*upload_resident_dirty_range/s.test(flushResidentRebuildBody)
  && /fn\s+upload_resident_dirty_range[\s\S]*queue\.write_buffer\([\s\S]*byte_offset[\s\S]*bytemuck::bytes_of/s.test(primitivePipelineAuditSource)
  && /range\.requires_full_rebuild[\s\S]*rebuild_resident_chunk_full/s.test(flushResidentRebuildBody);
const framePassManualSequenceHits = [
  ...sourceHits(rendererFramePassSequence, 'rust/src/renderer/frame_pass_sequence.rs', /execute_node\(&context\.nodes|FrameGraphPlanNodes/),
  ...sourceHits(rendererFrameGraphPlan, 'rust/src/renderer/frame_graph_plan.rs', /struct FrameGraphPlanNodes|nodes:\s*FrameGraphPlanNodes/),
];
const dispatcherRendererHits = [
  ...sourceHits(rendererPassDispatch, 'rust/src/renderer/pass_dispatch.rs', /renderer:\s*&'[^,]*\s+mut\s+Renderer/),
  ...sourceHitsInFiles(listFiles(path.join(voplayRoot, 'rust/src/renderer'), '.rs'), /renderer:\s*&'[^,]*\s+mut\s+Renderer/),
];
const renderHotPathFiles = [
  path.join(voplayRoot, 'rust/src/renderer.rs'),
  path.join(voplayRoot, 'rust/src/renderer_runtime.rs'),
  path.join(voplayRoot, 'rust/src/pipeline3d.rs'),
  path.join(voplayRoot, 'rust/src/primitive_pipeline/runtime.rs'),
  ...listFiles(path.join(voplayRoot, 'rust/src/renderer'), '.rs'),
  path.join(voplayRoot, 'rust/src/renderer_frame.rs'),
  path.join(voplayRoot, 'rust/src/renderer_frame/resource_registry.rs'),
  path.join(voplayRoot, 'rust/src/render_world.rs'),
  path.join(voplayRoot, 'rust/src/primitive_pipeline.rs'),
  ...listFiles(path.join(voplayRoot, 'rust/src/pipeline3d'), '.rs'),
];
const hotPathPanicPattern = /\b(panic!|expect\s*\(|unwrap\s*\(|assert!\s*\(|assert_eq!\s*\(|assert_ne!\s*\(|todo!|unimplemented!)/;
const hotPathPanicHits = sourceHitsInFiles(renderHotPathFiles, hotPathPanicPattern);
const legacyExecutePassHits = sourceHits(frameGraph, 'rust/src/renderer_frame.rs', /\b(execute_pass|FnOnce)\b/);
const decodeOwnerMutationHits = sourceHits(
  rendererDecodeAuditSource,
  'rust/src/renderer/frame_decode*.rs',
  /\bself\.(draw_list|render_world|primitive_pipeline|primitive_shapes|primitive_materials)\b/,
);
const trackSurfaceAtHits = sourceHits(scene3dAuditSource, 'scene3d/**/*.vo', /\bTrack\.SurfaceAt\(/);
const directConstraintHits = sourceHits(scene3dAuditSource, 'scene3d/**/*.vo', /\b(ApplyVehicleConstraint|VehicleConstraintCommand)\b/);
const facadeOwnerModuleFacts = [
  'rust/src/pipeline3d/primitive_submitter.rs',
  'rust/src/pipeline3d/water_submitter.rs',
  'rust/src/pipeline3d/decal_submitter.rs',
].map((relativePath) => {
  const source = readProjectFile(voplayRoot, relativePath);
  return {
    path: relativePath,
    meaningful: meaningfulOwnerModule(source),
    lineCount: source.split(/\r?\n/).length,
  };
});
const invalidBatchIndexSkipHits = sourceHits(renderWorldAuditSource, 'rust/src/render_world.rs', /\.filter_map\(\|index\|.*\.get\(\*index\)|\.filter_map\(\|\(draw_index, draw\)\|/);
const frameGraphDependencyOrderingReady = frameGraphAuditSource.includes('dependency_ordered_nodes')
  && frameGraphAuditSource.includes('producer_by_resource')
  && frameGraphAuditSource.includes('visit_node');
const resourceRegistryProvenanceReady = frameGraphAuditSource.includes('struct RenderResourceRegistry')
  && frameGraphAuditSource.includes('backing_owner')
  && frameGraphAuditSource.includes('ready_cause')
  && frameGraphAuditSource.includes('validate_backing_generation')
  && frameGraphAuditSource.includes('actual_texture_view')
  && frameGraphAuditSource.includes('mark_ready_with_cause');
const dirtyRebuildPolicyReady = primitivePipelineAuditSource.includes('ResidentRebuildPolicy')
  && primitivePipelineAuditSource.includes('full_rebuild_count')
  && primitivePipelineAuditSource.includes('dirty_upload_bytes')
  && primitivePipelineAuditSource.includes('rebuild_reason');
const dirtyRangeFullRebuildHits = [
  ...sourceHits(primitivePipelineRuntime, 'rust/src/primitive_pipeline/runtime.rs', /rebuild_resident_chunk\(device,\s*queue,\s*range\.chunk_ref,\s*models\)/),
];
const dirtyRangePartialUploadVerified = /partial.*upload|upload.*partial/i.test(primitivePipelineAuditSource)
  && /fn\s+upload_resident_dirty_range[\s\S]*queue\.write_buffer\([\s\S]*byte_offset[\s\S]*bytemuck::bytes_of/s.test(primitivePipelineAuditSource)
  && /full_rebuild_count\s*(?:[:=]|,)\s*0|fullRebuildCount\s*=\s*0/.test(primitivePipelineAuditSource);
const facadeSubmitterReturnHits = [
  ...sourceHits(readProjectFile(voplayRoot, 'rust/src/pipeline3d/primitive_submitter.rs'), 'rust/src/pipeline3d/primitive_submitter.rs', /->\s*crate::primitive_pipeline::PrimitiveRenderFilter|^\s*filter\s*$/),
  ...sourceHits(readProjectFile(voplayRoot, 'rust/src/pipeline3d/water_submitter.rs'), 'rust/src/pipeline3d/water_submitter.rs', /->\s*crate::primitive_pipeline::PrimitiveRenderFilter|^\s*crate::primitive_pipeline::PrimitiveRenderFilter::Water\s*$/),
  ...sourceHits(readProjectFile(voplayRoot, 'rust/src/pipeline3d/decal_submitter.rs'), 'rust/src/pipeline3d/decal_submitter.rs', /->\s*usize|decals\.len\(\)/),
];
const structuredSkipCountersReady = renderWorldAuditSource.includes('invalid_batch_indices')
  && renderWorldAuditSource.includes('missing_chunk_info')
  && renderWorldAuditSource.includes('skip_reasons');

for (const pass of ['DepthPrepass', 'Shadow', 'MainOpaque', 'MainTransparent', 'Water', 'Post', 'Overlay', 'BackendSubmit']) {
  check(rendererRuntime.includes(`RenderPassKind::${pass}`), 'renderer.pass_missing', `${pass} is missing from renderer frame graph planning`);
}
check(legacyExecutePassHits.length === 0, 'framegraph.legacy_execute_pass_api', `FrameGraph still exposes closure-based execute_pass/FnOnce: ${JSON.stringify(legacyExecutePassHits.slice(0, 8))}`);
check(decodeOwnerMutationHits.length === 0, 'frame_decode.direct_owner_mutation', `FrameDecode runtime still mutates render owners directly instead of producing a FrameTransaction: ${JSON.stringify(decodeOwnerMutationHits.slice(0, 12))}`);
check(rendererRuntime.includes('execute_node(') || rendererRuntime.includes('execute_all('), 'renderer.node_execution_missing', 'renderer runtime does not route passes through RenderPassNode execution');
check(frameGraphAuditSource.includes('fn execute_all'), 'framegraph.execute_all_missing', 'FrameGraphExecutor.execute_all is missing; graph traversal is not owned by FrameGraph');
check(rendererRuntime.includes('execute_all('), 'renderer.execute_all_not_wired', 'renderer runtime does not submit the compiled frame graph to execute_all');
check(frameGraphDependencyOrderingReady, 'framegraph.dependency_order_missing', 'FrameGraphExecutor does not own dependency-ordered node traversal');
check(framePassManualSequenceHits.length === 0, 'renderer.manual_pass_sequence', `renderer still drives pass order through hand-written node sequence: ${JSON.stringify(framePassManualSequenceHits.slice(0, 12))}`);
check(dispatcherRendererHits.length === 0, 'renderer.dispatcher_global_renderer', `frame pass dispatch/context still captures &mut Renderer instead of explicit resources: ${JSON.stringify(dispatcherRendererHits.slice(0, 12))}`);
check(!rendererRuntime.includes('execute_render_node!'), 'renderer.execute_render_node_macro', 'renderer runtime still routes passes through execute_render_node! macro closures');
check(!rendererRuntime.includes('.execute_pass('), 'renderer.legacy_execute_pass', 'renderer runtime still calls legacy execute_pass');
check(!rendererRuntime.includes('execute_recorded('), 'renderer.legacy_recorded_pass', 'renderer runtime still records pass execution outside execute_pass');
check(!rendererRuntime.includes('if frame_graph.has_pass'), 'renderer.manual_pass_branch', 'renderer runtime still branches directly on frame_graph.has_pass');
check(rendererRuntime.includes('RenderBatchPlanner::build') && rendererRuntime.includes('render_batch_plan.visible_objects'), 'renderer.batch_plan_not_wired', 'RenderBatchPlan is not wired into renderer telemetry');
check(pipeline3d.split(/\r?\n/).length <= 2000, 'pipeline3d.line_budget', 'pipeline3d.rs exceeded the current refactor guardrail of 2000 lines');
for (const [file, entry] of Object.entries(renderFileBudgets)) {
  check(entry.lines <= entry.budget, 'render.file_budget', `${file} exceeds budget ${entry.lines}/${entry.budget}`);
}
check(ownerModuleFacts.every((entry) => entry.hasProductionMethod && entry.calledByProduction), 'render.empty_owner_module', `owner module lacks production responsibility: ${JSON.stringify(ownerModuleFacts.filter((entry) => !entry.hasProductionMethod || !entry.calledByProduction))}`);
check(facadeOwnerModuleFacts.every((entry) => entry.meaningful), 'render.facade_owner_module', `submitter owner module only returns counts/filters or has no draw/upload/report side effect: ${JSON.stringify(facadeOwnerModuleFacts.filter((entry) => !entry.meaningful))}`);
check(facadeSubmitterReturnHits.length === 0, 'render.facade_submitter_return', `pipeline submitter owner returns only filters/counts instead of a submit/report contract: ${JSON.stringify(facadeSubmitterReturnHits.slice(0, 12))}`);
check(!(receiverMaskDeclaredConditionally && mainOpaqueWritesAuxResources), 'framegraph.conditional_receiver_mask_contract', 'MainOpaque writes RES_RECEIVER_MASK while the resource is only declared for post-depth frames');
check(!(surfacePropsDeclaredConditionally && mainOpaqueWritesAuxResources), 'framegraph.conditional_surface_props_contract', 'MainOpaque writes RES_SURFACE_PROPS while the resource is only declared for post-depth frames');
check(!(receiverMaskDeclaredConditionally && postReadsAuxResources), 'framegraph.conditional_receiver_mask_post_read', 'Post reads RES_RECEIVER_MASK while the resource is only declared for post-depth frames');
check(!(surfacePropsDeclaredConditionally && postReadsAuxResources), 'framegraph.conditional_surface_props_post_read', 'Post reads RES_SURFACE_PROPS while the resource is only declared for post-depth frames');
check(transparentSortReady, 'transparent.depth_stable_sort_missing', 'transparent pass does not build depth-aware stable RenderDrawItem ordering');
check(residentVisibilityRemovesHidden, 'primitive.resident_visible_false_stale_instance', 'upsert_instance visible=false returns without removing resident instance');
check(dirtyRangeDrivesUpload, 'primitive.dirty_range_not_used_for_upload', 'resident dirty range is recorded but discarded before upload/rebuild work');
check(dirtyRebuildPolicyReady, 'primitive.dirty_rebuild_policy_missing', 'dirty range full-rebuild fallback lacks an explicit resident rebuild policy, byte accounting, full rebuild count, and rebuild reason');
for (const hit of dirtyRangeFullRebuildHits) {
  addSourceIssue(
    'primitive.dirty_range_full_rebuild',
    'P0',
    'voplay/render',
    hit,
    'resident dirty range still triggers full resident chunk rebuild or offset-0 full buffer upload',
    'Implement real partial resident instance buffer upload for single-instance updates and keep full rebuilds only for capacity, layout, or resident generation changes.',
  );
}
check(dirtyRangePartialUploadVerified, 'primitive.dirty_range_partial_upload_unverified', 'single-instance resident update lacks source-bound proof for partial upload with fullRebuildCount=0', {
  severity: 'P0',
  owner: 'voplay/render',
  file: 'rust/src/primitive_pipeline/runtime.rs',
  line: dirtyRangeFullRebuildHits[0]?.line ?? null,
  requiredFix: 'Add behavior coverage proving one-instance dirty updates write the matching offset/length and do not rebuild the whole resident chunk.',
  evidence: { dirtyRangeFullRebuildHits, dirtyRangePartialUploadVerified },
});
check(invalidBatchIndexSkipHits.length === 0 || /invalid.*batch|batch.*invalid|skip_reason|skipped_indices|structured/i.test(renderWorldAuditSource), 'render_world.invalid_batch_index_silent_skip', `RenderBatchPlan silently drops invalid batch indices instead of emitting structured skip/error evidence: ${JSON.stringify(invalidBatchIndexSkipHits.slice(0, 12))}`);
check(structuredSkipCountersReady, 'render_world.structured_skip_counters_missing', 'RenderBatchPlan lacks structured counters for invalid batch indices, missing chunk info, and skip reasons');
for (const hit of hotPathPanicHits) {
  addSourceIssue(
    'render.hot_path_panic',
    'P0',
    'voplay/render',
    hit,
    'render runtime hot path contains panic-prone call',
    'Replace panic-prone calls in render runtime paths with Result-returning structured renderer errors or test-only scoped assertions.',
  );
}
for (const token of ['RenderFrameDecode', 'RenderSceneSnapshot', 'FrameGraphBuild', 'FrameGraphExecute', 'PerfPacketEncode', 'RenderFramePipeline']) {
  check(frameGraphAuditSource.includes(`struct ${token}`), 'framegraph.pipeline_contract_missing', `${token} is missing`);
  check(constructsRuntimeStage(rendererAuditSource, token), 'framegraph.pipeline_stage_unused', `${token} is declared but is not constructed by runtime code`);
}
check(frameGraphAuditSource.includes('struct RenderResourceRegistry'), 'framegraph.registry_missing', 'RenderResourceRegistry is missing');
check(frameGraphAuditSource.includes('enum RenderResourceLifetime'), 'framegraph.lifetime_missing', 'RenderResourceLifetime is missing');
check(resourceRegistryProvenanceReady, 'framegraph.registry_provenance_missing', 'RenderResourceRegistry target status lacks backing owner and ready-cause provenance');
check(frameGraphAuditSource.includes('fn execute_node'), 'framegraph.node_executor_missing', 'FrameGraphExecutor.execute_node is missing');
check(frameGraphAuditSource.includes('struct RenderPassWorkload'), 'framegraph.workload_missing', 'RenderPassWorkload is missing');
check(frameGraphAuditSource.includes('transient_writes'), 'framegraph.transient_writes_missing', 'RenderPassNode transient writes are missing');
check(frameGraphAuditSource.includes('missing_read_count'), 'framegraph.missing_reads_missing', 'FrameGraph missing-read diagnostics are missing');
for (const token of ['struct RenderWorldChunk', 'struct RenderBatchPlan', 'struct RenderBatchPlanner', 'fn build_batch_plan']) {
  check(renderWorldAuditSource.includes(token), 'render_world.batch_planner_missing', `${token} is missing`);
}
for (const token of ['RenderBatchKind::Mesh', 'RenderBatchKind::Primitive', 'RenderBatchKind::Water']) {
  check(renderWorldAuditSource.includes(token), 'render_world.unified_batch_kind_missing', `${token} is missing from unified batch planning`);
}
check(/kind:\s*RenderBatchKind::Terrain/.test(renderWorldAuditSource) && /#\[test\][\s\S]*terrain/i.test(renderWorldAuditSource), 'render_world.terrain_batch_unwired', 'RenderBatchKind::Terrain is not backed by a real construction path and unit coverage');
check(/kind:\s*RenderBatchKind::Decal/.test(renderWorldAuditSource) && /#\[test\][\s\S]*decal/i.test(renderWorldAuditSource), 'render_world.decal_batch_unwired', 'RenderBatchKind::Decal is not backed by a real construction path and unit coverage');
check(!/bounds:\s*RenderChunkBounds\s*\{[\s\S]*?center:\s*Vec3::ZERO[\s\S]*?radius:\s*0\.0/.test(renderBatchPlannerBuildBody), 'render_world.zero_bounds', 'RenderBatchPlanner still emits zero placeholder bounds');
check(!renderBatchPlannerSelectLodBody.includes('seed') && !renderBatchPlannerSelectLodBody.includes('workload'), 'render_world.seed_workload_lod', 'RenderBatchPlanner LOD still uses seed/workload heuristics');
check(/frustum_culled_chunks\s*(?:\+=|=)/.test(renderBatchPlannerBuildBody), 'render_world.frustum_counters_not_mutated', 'frustum_culled_chunks is not mutated from batch-planning decisions');
check(/distance_culled_chunks\s*(?:\+=|=)/.test(renderBatchPlannerBuildBody), 'render_world.distance_counters_not_mutated', 'distance_culled_chunks is not mutated from batch-planning decisions');
check(rendererPerf.includes('RENDERER_PERF_PAYLOAD_VERSION: u32 = 6'), 'renderer_perf.payload_version', 'renderer perf payload was not upgraded to v6');
check(perfDiagnostics.includes('version != 6') && perfDiagnostics.includes('FrameGraphMissingReads') && perfDiagnostics.includes('WaterDraws'), 'perf_decode.v6_missing', 'Vo perf decoder does not expose v6 water and FrameGraph diagnostics');
check(primitivePipelineAuditSource.includes('draw_main_and_water') && primitivePipelineAuditSource.includes('PrimitiveRenderFilter::Water'), 'primitive.water_filter', 'primitive pipeline does not split main and water draw filters');

const updateIntentBody = bodyOfFunction(vehicle, 'func (v *Vehicle) UpdateIntent');
check(updateIntentBody.includes('v.Dynamics.Step('), 'vehicle.intent_chain_missing', 'Vehicle.UpdateIntent does not call KartDynamics.Step');
check(!updateIntentBody.includes('BuildVehicleForceCommand'), 'vehicle.intent_bypasses_dynamics', 'Vehicle.UpdateIntent still builds force commands directly');
check(!updateIntentBody.includes('DefaultSurfaceMaterial()'), 'vehicle.intent_default_surface', 'Vehicle.UpdateIntent still uses default surface instead of sampled surface');
check(vehicle.includes('func (v *Vehicle) applyForceCommandToBackend'), 'vehicle.backend_adapter_missing', 'Vehicle backend adapter is missing');
const backendAdapterBody = bodyOfFunction(vehicle, 'func (v *Vehicle) applyForceCommandToBackend');
check(backendAdapterBody.includes('BuildPhysicsBackendApplyCommand'), 'vehicle.backend_contract_not_used', 'Vehicle backend adapter does not use BuildPhysicsBackendApplyCommand');
check(backendAdapterBody.includes('physBackend.ApplyVehicleForces') && !vehicle.includes('physBackend.SetRaycastVehicleWheelControl'), 'vehicle.backend_apply_contract_not_used', 'Vehicle backend adapter does not use PhysicsBackend.ApplyVehicleForces as the wheel/backend apply contract');
check(!backendAdapterBody.includes('vehicleDriveWheelCount'), 'vehicle.backend_adapter_owns_distribution', 'Vehicle backend adapter still owns drive wheel force distribution');
check(!vehicle.includes('v.applyControls(command.VehicleInput'), 'vehicle.command_folded_to_input', 'Vehicle.ApplyForceCommand still folds command back to VehicleInput');
check(!vehicle.includes('func (v *Vehicle) applyControls'), 'vehicle.legacy_apply_controls', 'Vehicle still exposes the legacy direct applyControls path');
check(!vehicle.includes('SurfaceMaterialAtTrackPosition(v.Track, v.Body.Position())'), 'vehicle.body_position_surface_fallback', 'Vehicle still infers surface material from body position when backend wheel contacts are absent');
check(!vehicle.includes('SurfaceMaterialAtTrackPosition'), 'vehicle.track_position_surface_inference', 'Vehicle still infers surface material from track position in industrial paths');
check(trackSurfaceAtHits.length === 0, 'vehicle.track_surface_at_control_chain', `scene3d industrial paths still call Track.SurfaceAt directly: ${JSON.stringify(trackSurfaceAtHits.slice(0, 8))}`);
check(directConstraintHits.length === 0, 'vehicle.direct_constraint_contract', `scene3d still exposes direct VehicleConstraintCommand/ApplyVehicleConstraint control paths: ${JSON.stringify(directConstraintHits.slice(0, 12))}`);
check(!contactEvent.includes('SurfaceMaterialAtTrackPosition'), 'contact.track_position_surface_inference', 'ContactEvent still infers surface material from track position');
check(!vehicleTelemetry.includes('SurfaceMaterialAtTrackPosition'), 'telemetry.track_position_surface_inference', 'VehicleTelemetry still infers wheel surface material from track position');
const setPoseBody = bodyOfFunction(vehicle, 'func (v *Vehicle) SetPose');
check(!/Body\.SetPosition\(|Body\.SetRotation\(|Body\.SetVelocity\(|Body\.SetAngularVelocity\(|Body\.Physics\.velocity|Body\.Physics\.angularVelocity/.test(setPoseBody), 'vehicle.set_pose_direct_physics_mutation', 'Vehicle.SetPose still mutates body pose or physics state directly');
const applyPoseResetBody = bodyOfFunction(vehicle, 'func (v *Vehicle) applyPoseResetToBackend');
check(!/Body\.SetPosition\(|Body\.SetRotation\(|Body\.SetVelocity\(|Body\.SetAngularVelocity\(|Body\.Physics\.velocity|Body\.Physics\.angularVelocity/.test(applyPoseResetBody), 'vehicle.pose_reset_helper_direct_physics_mutation', 'Vehicle.applyPoseResetToBackend still mutates body pose or physics state directly');
for (const token of ['type KartVehicleModel struct', 'type WheelContactSample struct', 'type VehicleForceCommand struct', 'type PhysicsBackendApplyCommand struct', 'BodyForce voplay.Vec3', 'DebugHash int', 'func BuildPhysicsBackendApplyCommand', 'func NormalizeVehicleForceCommand']) {
  check(kartDynamics.includes(token), 'vehicle.dynamics_contract_missing', `${token} is missing`);
}
check(contactEvent.includes('func (event ContactEvent) IndustrialReady() bool'), 'contact.industrial_ready_missing', 'ContactEvent industrial validity helper is missing');
check(contactEvent.includes('func (s *Scene) IndustrialContactEvents() []ContactEvent'), 'contact.industrial_events_missing', 'Scene industrial contact filter is missing');
check(vehicleIdleSleep.includes('func VehicleShouldSleepIdlePhysics'), 'vehicle.idle_sleep_helper_missing', 'voplay generic idle sleep helper is missing');
check(replay.includes('BackendApplyHash') || replay.includes('BackendApplyCommandHash'), 'replay.backend_apply_hash_missing', 'PhysicsReplayTrace does not record backend apply hash');

check(physicsStress.includes('ScenarioDefinition{Name: "multi-vehicle-scripted-soak", VehicleCount: 24}'), 'physics.soak_vehicle_count', 'physics stress does not require 24 vehicles');
check(physicsStress.includes('Code: "physics.contact_fallback", Severity: 0'), 'physics.fallback_not_p0', 'fallback contacts are not P0 in physics stress');

for (const token of ['BlockKartRenderBudget', 'PostProcessConfig', 'ShadowResolution', 'applyBlockKartRenderBudget', 'defaultBlockKartRenderBudget']) {
  for (const entry of blockKartVoFiles) {
    check(!entry.source.includes(token), 'blockkart.raw_render_budget', `BlockKart ${entry.rel} still contains ${token}`);
  }
}
check(blockKartBudget.includes('SetRenderingQuality'), 'blockkart.quality_profile_missing', 'BlockKart does not use voplay rendering quality profile');
check(!/type BlockKartPrimitiveScene|primitive3d\.NewLayer|primitive3d\.NewBuilder|primitive3d\.LayerDesc|primitive3d\.ChunkingDesc|SpawnPreparedMapPrimitiveLayers|spawnPrimitiveRoadBoxPhysics|BlockKartVisualContent|spawnPrimitiveTrackPhysics|spawnRoadColliderStrip/.test(blockKartPrimitiveWorld), 'blockkart.primitive_authoring_owner', 'BlockKart still owns generic primitive/chunk/collider/surface authoring');
check(!/PrimitiveStats\(|WheelState\(|VehicleGrounded|WheelMaxSlip|PrimitiveVisibleChunks/.test(blockKartWorld), 'blockkart.low_level_hud_facts', 'BlockKart HUD still assembles low-level engine facts directly');
check(!/w\.vehicle\.(SteerAngle|WheelSpin)/.test(blockKartPrimitiveWorld), 'blockkart.visual_mutable_vehicle_state', 'BlockKart kart visual state still reads mutable vehicle steering or wheel spin fields');
check(!/w\.vehicle\.SetPose\(/.test(blockKartWorld), 'blockkart.direct_vehicle_set_pose', 'BlockKart still calls Vehicle.SetPose directly');
check(!/w\.player\.SetPosition\(|w\.player\.SetRotation\(|w\.player\.SetVelocity\(|w\.player\.SetAngularVelocity\(/.test(blockKartWorld), 'blockkart.direct_player_physics_mutation', 'BlockKart world still directly mutates player pose, velocity, or angular velocity');
const directEntityMutation = /\b[A-Za-z_][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)*\.Set(Position|Rotation|Velocity|AngularVelocity)\(/;
for (const entry of blockKartVoFiles) {
  check(!directEntityMutation.test(entry.source), 'blockkart.direct_entity_physics_mutation', `BlockKart ${entry.rel} directly mutates entity pose, velocity, or angular velocity`);
}
check(!blockKartWorld.includes('SurfaceAt(w.player.Position())'), 'blockkart.surface_position_workaround', 'BlockKart world still infers surface from player position');
check(!blockKartProductFoundation.includes('SurfaceAt(w.player.Position())'), 'blockkart.surface_position_workaround', 'BlockKart product diagnostics still infer surface from player position');
check(!blockKartWorld.includes('kartPhysicsIdle'), 'blockkart.local_physics_sleep_thresholds', 'BlockKart still owns local physics sleep thresholds');
check(blockKartWorld.includes('VehicleShouldSleepIdlePhysics'), 'blockkart.idle_sleep_helper_missing', 'BlockKart does not delegate idle sleep policy to voplay');
check(blockKartVoFiles.some((entry) => entry.source.includes('VehicleAudioInputFromTelemetry')), 'blockkart.audio_telemetry_missing', 'BlockKart vehicle audio does not consume voplay vehicle telemetry input');
for (const token of ['hostPacingOnly', 'resource pacing waiver', 'resourcePacingWaiver']) {
  for (const entry of blockKartVoFiles) {
    check(!entry.source.includes(token), 'blockkart.render_waiver', `BlockKart ${entry.rel} still contains ${token}`);
  }
}

if (issues.length > 0) {
  writeReport('failed');
  for (const issue of issues) {
    console.error(`voplay architecture lint: ${issue.code}: ${issue.detail}`);
  }
  process.exit(1);
}

writeReport('ok');
console.log(`voplay architecture lint: ok voplay=${voplayRoot} blockkart=${blockKartRoot}`);
