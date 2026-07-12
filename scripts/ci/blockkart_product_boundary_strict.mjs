#!/usr/bin/env node
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';
import { analyzeBlockKartBoundary } from './blockkart_boundary_facts.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const gate = 'blockkart-product-boundary-strict';
const outDir = path.join(root, 'target', gate);
const issues = [];

function fail(message) {
  console.error(`blockkart product boundary strict: ${message}`);
  process.exit(1);
}

function check(condition, code, detail, evidence = {}) {
  if (!condition) {
    issues.push({ code, detail, evidence });
  }
}

function writeReport(status) {
  const generatedAt = new Date().toISOString();
  mkdirSync(outDir, { recursive: true });
  writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify({
    schemaVersion: 1,
    kind: 'blockkart.productBoundaryStrictReport',
    gate,
    status,
    generatedAt,
    freshEvidence: sourceBoundEvidence({
      gate,
      generatedAt,
      root,
      repos: [
        { name: 'volang', root },
        { name: 'BlockKart', root: blockKartRoot },
        { name: 'voplay', root: voplayRoot },
      ],
      gateFiles: [
        'scripts/ci/blockkart_product_boundary_strict.mjs',
        'scripts/ci/blockkart_boundary_facts.mjs',
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

function readProjectFile(projectRoot, relativePath) {
  const file = path.join(projectRoot, relativePath);
  if (!existsSync(file)) {
    fail(`${relativePath} is missing under ${projectRoot}`);
  }
  return readFileSync(file, 'utf8');
}

function listVoFiles(projectRoot) {
  const files = [];
  const visit = (dir) => {
    for (const entry of readdirSync(dir)) {
      const file = path.join(dir, entry);
      const stat = statSync(file);
      if (stat.isDirectory()) {
        if (entry === '.git' || entry === 'target' || entry === 'node_modules') continue;
        visit(file);
      } else if (file.endsWith('.vo')) {
        files.push({
          file,
          rel: path.relative(projectRoot, file).split(path.sep).join('/'),
          source: readFileSync(file, 'utf8'),
        });
      }
    }
  };
  visit(projectRoot);
  return files;
}

function lineOf(source, pattern) {
  const lines = source.split(/\r?\n/);
  for (let i = 0; i < lines.length; i++) {
    if (typeof pattern === 'string' ? lines[i].includes(pattern) : pattern.test(lines[i])) {
      return i + 1;
    }
  }
  return null;
}

function firstLineMatching(source, pattern) {
  const lines = source.split(/\r?\n/);
  for (let i = 0; i < lines.length; i++) {
    if (pattern.test(lines[i])) {
      return i + 1;
    }
  }
  return null;
}

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function scan(pattern, allow = () => false) {
  const hits = [];
  for (const entry of blockKartVoFiles) {
    if (allow(entry)) continue;
    const lines = entry.source.split(/\r?\n/);
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      const matched = typeof pattern === 'string' ? line.includes(pattern) : pattern.test(line);
      if (matched) {
        hits.push({ path: entry.rel, line: i + 1, text: line.trim() });
      }
    }
  }
  return hits;
}

const blockKartVoFiles = listVoFiles(blockKartRoot);
const voplayVoFiles = listVoFiles(voplayRoot);
const boundaryFacts = analyzeBlockKartBoundary(blockKartVoFiles);
const world = readProjectFile(blockKartRoot, 'world.vo');
const primitiveWorld = readProjectFile(blockKartRoot, 'primitive_world.vo');
const primitiveContent = readProjectFile(blockKartRoot, 'primitive_content.vo');
const diagnosticsJson = readProjectFile(blockKartRoot, 'diagnostics_json.vo');
const vehicleTelemetry = readProjectFile(voplayRoot, 'scene3d/vehicle_telemetry.vo');
const sceneDiagnostics = readProjectFile(voplayRoot, 'scene3d/diagnostics.vo');
const primitiveAuthoring = readProjectFile(voplayRoot, 'scene3d/primitive_authoring.vo');

const directVehicleWriteHits = scan(/\bw\.vehicle\.(Speed|Yaw|Grounded)\s*=/);
const directVehicleFactHits = scan(/\bw\.vehicle\.(Speed|Yaw|Grounded)\b/);
const directPhysicsStepHits = scan(/\b(ProductStepAndSyncPhysics|[A-Za-z_][A-Za-z0-9_]*\s*\.\s*StepAndSyncPhysics)\s*\(/);
const handwrittenJsonHits = scan(/"\{"\s*\+|jsonString\(|jsonBool\(|jsonFloat\(/, (entry) => entry.rel === 'diagnostics_json.vo');
const manualMarkerJsonHits = scan(/\bblockKartEmitJSON\s*\(|\bprintln\s*\(\s*blockKart[A-Za-z0-9_]*Marker\s*\+|blockKart[A-Za-z0-9_]*ReportMarker/, (entry) => entry.rel === 'diagnostics_json.vo');
const lowLevelHudHits = scan(/\bPrimitiveStats\(|\bWheelState\(|VehicleGrounded|WheelMaxSlip|primitiveUploadBytes|primitiveDrawCalls|diag\.Primitive\.DrawCalls|content\.UploadBytes|primitiveStats\.DrawCalls|primitiveStats\.UploadBytes/);
const engineProductFacadeHits = voplayVoFiles.flatMap((entry) => {
  const hits = [];
  const pattern = /\b(?:ProductVehicleTelemetry|ProductSceneDiagnostics|ProductPrimitiveAuthoring|NewProductPrimitiveRuntime|NewProductPrimitiveAuthoring|ProductPrimitive(?:Place|Dynamic|SetPose)|ProductSpawnTrackColliderStrip)\b/g;
  for (const match of entry.source.matchAll(pattern)) {
    hits.push({ path: entry.rel, line: lineOf(entry.source, match[0]), symbol: match[0] });
  }
  return hits;
});
const engineBlockKartContentHits = voplayVoFiles.flatMap((entry) => {
  const hits = [];
  const pattern = /\bBlockKart(?:Primitive|Map|Pack)[A-Za-z0-9_]*\b/g;
  for (const match of entry.source.matchAll(pattern)) {
    hits.push({ path: entry.rel, line: lineOf(entry.source, match[0]), symbol: match[0] });
  }
  return hits;
});
const directIntentBypassHits = scan(/UpdateIntentFromSyncedState\s*\(|\b[A-Za-z_][A-Za-z0-9_]*\.UpdateIntent\s*\(/);
const directConstraintHits = scan(/\b(ApplyEntityPhysicsTarget|ApplyEntityPhysicsConstraint|ApplyVehicleConstraint|VehicleConstraintCommand)\b/);
const rawRenderKnobHits = scan(/\b[A-Za-z_][A-Za-z0-9_]*\.scene\.RenderDebugMode\b|\.RenderDebugMode\s*=(?!=)|\b(SetRenderDebug3D|PostProcessConfig|ShadowResolution)\b/);
const worldBody = bodyOfType(world, 'World');
const runtimeContextBody = bodyOfType(world, 'BlockKartRuntimeContext');
const worldFields = worldBody
  .split(/\r?\n/)
  .map((line, index) => ({ line: index + 1, text: line.trim() }))
  .filter((entry) => /^[A-Za-z_][A-Za-z0-9_]*\s+/.test(entry.text));
const worldAllowedStateGroups = new Set([
  'core BlockKartRuntimeCore',
  'input BlockKartRuntimeInputState',
  'race BlockKartRuntimeRaceState',
  'kart BlockKartRuntimeKartState',
  'hud BlockKartRuntimeHudState',
]);
const worldForbiddenFieldHits = worldFields.filter((entry) => !worldAllowedStateGroups.has(entry.text));
const runtimeContextFields = runtimeContextBody
  .split(/\r?\n/)
  .map((line, index) => ({ line: index + 1, text: line.trim() }))
  .filter((entry) => /^[A-Za-z_][A-Za-z0-9_]*\s+/.test(entry.text));
const runtimeContextAllowedGroups = new Set([
  'core BlockKartRuntimeCore',
  'input BlockKartRuntimeInputState',
  'race BlockKartRuntimeRaceState',
  'kart BlockKartRuntimeKartState',
  'hud BlockKartRuntimeHudState',
]);
const runtimeContextForbiddenFieldHits = runtimeContextFields.filter((entry) => (
  !runtimeContextAllowedGroups.has(entry.text)
  && /\b(scene|camera|player|vehicle|kartController|racingInput|touch|vehicleAudio|assets|primitive|track|checkpoint|raceState|courseTime|finishTime|collected|lap|kart|boost|drift|collectibles|checkpoints|boostPads|obstacles|debugHud|perf|physics)/i.test(entry.text)
));
const worldReceiverAllowlist = new Set([
  'Update',
  'Draw',
  'PrepareRender',
  'FixedUpdate',
  'CycleTerrainCameraView',
  'ActivateRenderStressProfile',
  'SetRacingDebugOverlayVisible',
  'SetRenderDebugMode',
  'SetHeroCameraVisible',
  'Close',
]);
const worldReceiverHits = blockKartVoFiles.flatMap((entry) => (
  [...entry.source.matchAll(/^func \(w \*World\) ([A-Za-z_][A-Za-z0-9_]*)\b/gm)]
    .map((match) => ({ path: entry.rel, name: match[1], line: lineOf(entry.source, `func (w *World) ${match[1]}`) }))
))
  .filter((entry) => !worldReceiverAllowlist.has(entry.name));
const ownerWorldContextHits = blockKartVoFiles.flatMap((entry) => (
  [...entry.source.matchAll(/^func \([^)]*\*(RaceSession|KartRig|TrackRuntime|HUDPresenter|PerfReporter|AssetRuntimeCache)\) ([A-Za-z_][A-Za-z0-9_]*)\([^)]*\*World/gm)]
    .map((match) => ({ path: entry.rel, owner: match[1], name: match[2], line: firstLineMatching(entry.source, new RegExp(`^func \\([^)]*\\*${escapeRegExp(match[1])}\\) ${escapeRegExp(match[2])}\\(`)) }))
));
const ownerRuntimeContextHits = blockKartVoFiles.flatMap((entry) => (
  [...entry.source.matchAll(/^func \([^)]*\*(RaceSession|KartRig|TrackRuntime|HUDPresenter|PerfReporter|AssetRuntimeCache)\) ([A-Za-z_][A-Za-z0-9_]*)\([^)]*\*BlockKartRuntimeContext/gm)]
    .map((match) => ({ path: entry.rel, owner: match[1], name: match[2], line: firstLineMatching(entry.source, new RegExp(`^func \\([^)]*\\*${escapeRegExp(match[1])}\\) ${escapeRegExp(match[2])}\\(`)) }))
));
const runtimeOwners = readProjectFile(blockKartRoot, 'runtime_owners.vo');
const runtimeOwnerNames = ['RaceSession', 'KartRig', 'TrackRuntime', 'HUDPresenter', 'PerfReporter', 'AssetRuntimeCache'];
const runtimeOwnerFacts = runtimeOwnerNames.map((owner) => {
  const methodPattern = new RegExp(`^func \\([^)]*\\*?${owner}\\) ([A-Za-z_][A-Za-z0-9_]*)\\b`, 'gm');
  const methods = [...runtimeOwners.matchAll(methodPattern)].map((match) => match[1]);
  const productionCallPattern = new RegExp(`\\.(${methods.map(escapeRegExp).join('|')})\\(`);
  const calledByProduct = methods.length > 0 && blockKartVoFiles
    .filter((entry) => entry.rel !== 'runtime_owners.vo')
    .some((entry) => productionCallPattern.test(entry.source));
  return { owner, methods, calledByProduct };
});

check(directVehicleWriteHits.length === 0, 'blockkart.direct_vehicle_state_write', 'BlockKart product runtime writes voplay vehicle physics state directly', { directVehicleWriteHits });
check(directVehicleFactHits.length === 0, 'blockkart.direct_vehicle_low_level_fact', 'BlockKart product runtime reads Speed/Yaw/Grounded instead of structured telemetry', { directVehicleFactHits });
check(directPhysicsStepHits.length === 0, 'blockkart.direct_scene_step', 'BlockKart product runtime calls Scene.StepAndSyncPhysics directly', { directPhysicsStepHits });
check(handwrittenJsonHits.length === 0, 'blockkart.handwritten_json_report', 'BlockKart diagnostics still hand-write JSON strings instead of a structured encoder/schema', { handwrittenJsonHits });
check(manualMarkerJsonHits.length === 0, 'blockkart.manual_marker_json_report', 'BlockKart business/runtime files emit marker-prefixed JSON instead of using a schema-owned report encoder', { manualMarkerJsonHits });
check(lowLevelHudHits.length === 0, 'blockkart.low_level_hud_fact', 'BlockKart HUD/diagnostics assemble low-level render, primitive, GPU, or vehicle facts', { lowLevelHudHits });
check(engineProductFacadeHits.length === 0, 'voplay.product_facade', 'voplay still exposes product-named facade APIs instead of engine owners', { engineProductFacadeHits });
check(engineBlockKartContentHits.length === 0, 'voplay.blockkart_content', 'voplay still owns BlockKart-specific content', { engineBlockKartContentHits });
check(directIntentBypassHits.length === 0, 'blockkart.direct_vehicle_intent_bypass', 'BlockKart product runtime updates controller/vehicle intent outside VehiclePhysicsSession', { directIntentBypassHits });
check(directConstraintHits.length === 0, 'blockkart.direct_physics_constraint', 'BlockKart product runtime applies low-level physics constraints directly', { directConstraintHits });
check(rawRenderKnobHits.length === 0, 'blockkart.raw_render_knob', 'BlockKart product runtime writes or exposes low-level render knobs directly', { rawRenderKnobHits });
check(worldFields.length === worldAllowedStateGroups.size && worldForbiddenFieldHits.length === 0 && worldFields.every((entry) => worldAllowedStateGroups.has(entry.text)), 'blockkart.world_mega_owner', 'World must stay a lifecycle composition root containing only explicit core/input/race/kart/HUD owner state groups', { fieldCount: worldFields.length, worldForbiddenFieldHits, worldFields });
check(runtimeContextBody.trim() === '' && runtimeContextFields.length === 0 && runtimeContextForbiddenFieldHits.length === 0, 'blockkart.runtime_context_mega_owner', 'BlockKartRuntimeContext broad wrapper must remain removed', { fieldCount: runtimeContextFields.length, runtimeContextForbiddenFieldHits, runtimeContextFields });
check(worldReceiverHits.length === 0, 'blockkart.world_receiver_too_wide', 'World receiver methods must stay at lifecycle/composition boundary', { worldReceiverHits });
check(ownerWorldContextHits.length === 0, 'blockkart.owner_methods_take_world', 'Runtime owner methods must not use *World as the mutation context for cross-domain state', { ownerWorldContextHits: ownerWorldContextHits.slice(0, 80) });
check(ownerRuntimeContextHits.length === 0, 'blockkart.owner_methods_take_runtime_context', 'Runtime owner methods must not use *BlockKartRuntimeContext as the mutation context for cross-domain state', { ownerRuntimeContextHits: ownerRuntimeContextHits.slice(0, 80) });
check(boundaryFacts.wideOwnerParameters.length === 0, 'blockkart.owner_ports_semantically_narrow', 'Runtime owner ports must remain narrow after alias and wrapper expansion', { wideOwnerParameters: boundaryFacts.wideOwnerParameters.slice(0, 120), stateGroupsByType: boundaryFacts.stateGroupsByType });
check(runtimeOwnerFacts.every((entry) => entry.methods.length > 0 && entry.calledByProduct), 'blockkart.empty_runtime_owner', 'runtime_owners.vo owners must own production methods and be called by runtime', { runtimeOwnerFacts });

check(vehicleTelemetry.includes('func (v *Vehicle) Telemetry() VehicleTelemetry'), 'voplay.vehicle_telemetry_owner_missing', 'Vehicle must expose structured telemetry through its engine owner', {
  vehicleTelemetry: lineOf(vehicleTelemetry, 'func (v *Vehicle) Telemetry() VehicleTelemetry'),
});
check(sceneDiagnostics.includes('func (s *Scene) Diagnostics() SceneDiagnostics'), 'voplay.scene_diagnostics_owner_missing', 'Scene must expose structured diagnostics through its engine owner', {
  sceneDiagnostics: lineOf(sceneDiagnostics, 'func (s *Scene) Diagnostics() SceneDiagnostics'),
});
check(primitiveAuthoring.includes('type PrimitiveWorldAuthoring struct') && primitiveContent.includes('scene3d.PrimitiveWorldAuthoring'), 'blockkart.primitive_authoring_boundary_missing', 'BlockKart content must consume the generic scene3d primitive authoring owner', {
  engineOwner: lineOf(primitiveAuthoring, 'type PrimitiveWorldAuthoring struct'),
  productConsumer: lineOf(primitiveContent, 'scene3d.PrimitiveWorldAuthoring'),
});
check(diagnosticsJson.includes('jsonString') && handwrittenJsonHits.length === 0, 'blockkart.json_encoder_schema_missing', 'diagnostics_json.vo should be the only JSON escaping/encoding owner', {});
check(/\\\\n|newline/i.test(diagnosticsJson) && /\\\\u00|control/i.test(diagnosticsJson), 'blockkart.json_encoder_control_escape_missing', 'diagnostics_json.vo must escape newlines and control characters through the structured encoder', {});
check(!/type BlockKartPrimitiveScene|BuildPrimitiveWorld/.test(primitiveWorld), 'blockkart.primitive_world_owner_too_wide', 'primitive_world.vo still owns broad primitive scene construction', {});

if (issues.length > 0) {
  writeReport('failed');
  for (const issue of issues) {
    console.error(`blockkart product boundary strict: ${issue.code}: ${issue.detail}`);
    if (issue.evidence && Object.keys(issue.evidence).length > 0) {
      console.error(`  evidence: ${JSON.stringify(issue.evidence).slice(0, 1200)}`);
    }
  }
  process.exit(1);
}

writeReport('ok');
console.log(`blockkart product boundary strict: ok voplay=${voplayRoot} blockkart=${blockKartRoot}`);

function bodyOfType(source, typeName) {
  const start = source.indexOf(`type ${typeName} struct`);
  if (start < 0) return '';
  const brace = source.indexOf('{', start);
  if (brace < 0) return '';
  let depth = 0;
  for (let i = brace; i < source.length; i++) {
    if (source[i] === '{') depth++;
    if (source[i] === '}') {
      depth--;
      if (depth === 0) return source.slice(brace + 1, i);
    }
  }
  return '';
}
