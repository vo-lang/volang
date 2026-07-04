#!/usr/bin/env node
import { existsSync, readFileSync, readdirSync, statSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));
const localVoplayRoot = path.resolve(root, '..', 'voplay');
const localBlockKartRoot = path.resolve(root, '..', 'BlockKart');
const voplayRoot = path.resolve(process.env.VOPLAY_ROOT || (existsSync(path.join(localVoplayRoot, 'vo.mod')) ? localVoplayRoot : path.join(root, 'ci_modules/voplay')));
const blockKartRoot = path.resolve(process.env.BLOCKKART_ROOT || (existsSync(path.join(localBlockKartRoot, 'vo.mod')) ? localBlockKartRoot : path.join(root, 'ci_modules/BlockKart')));

const issues = [];

function readProjectFile(projectRoot, relativePath) {
  const file = path.join(projectRoot, relativePath);
  if (!existsSync(file)) {
    fail(`${relativePath} is missing under ${projectRoot}`);
  }
  return readFileSync(file, 'utf8');
}

function fail(message) {
  console.error(`voplay architecture lint: ${message}`);
  process.exit(1);
}

function check(condition, code, detail) {
  if (!condition) {
    issues.push({ code, detail });
  }
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
const frameGraph = readProjectFile(voplayRoot, 'rust/src/renderer_frame.rs');
const renderWorld = readProjectFile(voplayRoot, 'rust/src/render_world.rs');
const pipeline3d = readProjectFile(voplayRoot, 'rust/src/pipeline3d.rs');
const primitivePipeline = readProjectFile(voplayRoot, 'rust/src/primitive_pipeline.rs');
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

const rendererRuntime = [renderer, rendererFrameSubmit, rendererFrameOrchestrator].join('\n');
const rendererAuditSource = [rendererRuntime, frameGraph].join('\n');
const renderBatchPlannerBuildBody = bodyOfFunction(renderWorld, 'pub fn build');
const renderBatchPlannerSelectLodBody = bodyOfFunction(renderWorld, 'fn select_lod');

for (const pass of ['DepthPrepass', 'Shadow', 'MainOpaque', 'MainTransparent', 'Water', 'Post', 'Overlay', 'BackendSubmit']) {
  check(rendererRuntime.includes(`RenderPassKind::${pass}`), 'renderer.pass_missing', `${pass} is missing from renderer frame graph planning`);
}
check(rendererRuntime.includes('execute_node('), 'renderer.node_execution_missing', 'renderer runtime does not route passes through RenderPassNode execution');
check(!rendererRuntime.includes('execute_render_node!'), 'renderer.execute_render_node_macro', 'renderer runtime still routes passes through execute_render_node! macro closures');
check(!rendererRuntime.includes('.execute_pass('), 'renderer.legacy_execute_pass', 'renderer runtime still calls legacy execute_pass');
check(!rendererRuntime.includes('execute_recorded('), 'renderer.legacy_recorded_pass', 'renderer runtime still records pass execution outside execute_pass');
check(!rendererRuntime.includes('if frame_graph.has_pass'), 'renderer.manual_pass_branch', 'renderer runtime still branches directly on frame_graph.has_pass');
check(rendererRuntime.includes('RenderBatchPlanner::build') && rendererRuntime.includes('render_batch_plan.visible_objects'), 'renderer.batch_plan_not_wired', 'RenderBatchPlan is not wired into renderer telemetry');
check(pipeline3d.split(/\r?\n/).length <= 2000, 'pipeline3d.line_budget', 'pipeline3d.rs exceeded the current refactor guardrail of 2000 lines');
for (const token of ['RenderFrameDecode', 'RenderSceneSnapshot', 'FrameGraphBuild', 'FrameGraphExecute', 'PerfPacketEncode', 'RenderFramePipeline']) {
  check(frameGraph.includes(`struct ${token}`), 'framegraph.pipeline_contract_missing', `${token} is missing`);
  check(constructsRuntimeStage(rendererAuditSource, token), 'framegraph.pipeline_stage_unused', `${token} is declared but is not constructed by runtime code`);
}
check(frameGraph.includes('struct RenderResourceRegistry'), 'framegraph.registry_missing', 'RenderResourceRegistry is missing');
check(frameGraph.includes('enum RenderResourceLifetime'), 'framegraph.lifetime_missing', 'RenderResourceLifetime is missing');
check(frameGraph.includes('fn execute_node'), 'framegraph.node_executor_missing', 'FrameGraphExecutor.execute_node is missing');
check(frameGraph.includes('struct RenderPassWorkload'), 'framegraph.workload_missing', 'RenderPassWorkload is missing');
check(frameGraph.includes('transient_writes'), 'framegraph.transient_writes_missing', 'RenderPassNode transient writes are missing');
check(frameGraph.includes('missing_read_count'), 'framegraph.missing_reads_missing', 'FrameGraph missing-read diagnostics are missing');
for (const token of ['struct RenderWorldChunk', 'struct RenderBatchPlan', 'struct RenderBatchPlanner', 'fn build_batch_plan']) {
  check(renderWorld.includes(token), 'render_world.batch_planner_missing', `${token} is missing`);
}
for (const token of ['RenderBatchKind::Mesh', 'RenderBatchKind::Primitive', 'RenderBatchKind::Water']) {
  check(renderWorld.includes(token), 'render_world.unified_batch_kind_missing', `${token} is missing from unified batch planning`);
}
check(!/bounds:\s*RenderChunkBounds\s*\{[\s\S]*?center:\s*Vec3::ZERO[\s\S]*?radius:\s*0\.0/.test(renderBatchPlannerBuildBody), 'render_world.zero_bounds', 'RenderBatchPlanner still emits zero placeholder bounds');
check(!renderBatchPlannerSelectLodBody.includes('seed') && !renderBatchPlannerSelectLodBody.includes('workload'), 'render_world.seed_workload_lod', 'RenderBatchPlanner LOD still uses seed/workload heuristics');
check(/frustum_culled_chunks\s*(?:\+=|=)/.test(renderBatchPlannerBuildBody), 'render_world.frustum_counters_not_mutated', 'frustum_culled_chunks is not mutated from batch-planning decisions');
check(/distance_culled_chunks\s*(?:\+=|=)/.test(renderBatchPlannerBuildBody), 'render_world.distance_counters_not_mutated', 'distance_culled_chunks is not mutated from batch-planning decisions');
check(rendererPerf.includes('RENDERER_PERF_PAYLOAD_VERSION: u32 = 6'), 'renderer_perf.payload_version', 'renderer perf payload was not upgraded to v6');
check(perfDiagnostics.includes('version != 6') && perfDiagnostics.includes('FrameGraphMissingReads') && perfDiagnostics.includes('WaterDraws'), 'perf_decode.v6_missing', 'Vo perf decoder does not expose v6 water and FrameGraph diagnostics');
check(primitivePipeline.includes('draw_main_and_water') && primitivePipeline.includes('PrimitiveRenderFilter::Water'), 'primitive.water_filter', 'primitive pipeline does not split main and water draw filters');

const updateIntentBody = bodyOfFunction(vehicle, 'func (v *Vehicle) UpdateIntent');
check(updateIntentBody.includes('v.Dynamics.Step('), 'vehicle.intent_chain_missing', 'Vehicle.UpdateIntent does not call KartDynamics.Step');
check(!updateIntentBody.includes('BuildVehicleForceCommand'), 'vehicle.intent_bypasses_dynamics', 'Vehicle.UpdateIntent still builds force commands directly');
check(!updateIntentBody.includes('DefaultSurfaceMaterial()'), 'vehicle.intent_default_surface', 'Vehicle.UpdateIntent still uses default surface instead of sampled surface');
check(vehicle.includes('func (v *Vehicle) applyForceCommandToBackend'), 'vehicle.backend_adapter_missing', 'Vehicle backend adapter is missing');
const backendAdapterBody = bodyOfFunction(vehicle, 'func (v *Vehicle) applyForceCommandToBackend');
check(backendAdapterBody.includes('BuildPhysicsBackendApplyCommand'), 'vehicle.backend_contract_not_used', 'Vehicle backend adapter does not use BuildPhysicsBackendApplyCommand');
check(!backendAdapterBody.includes('vehicleDriveWheelCount'), 'vehicle.backend_adapter_owns_distribution', 'Vehicle backend adapter still owns drive wheel force distribution');
check(!vehicle.includes('v.applyControls(command.VehicleInput'), 'vehicle.command_folded_to_input', 'Vehicle.ApplyForceCommand still folds command back to VehicleInput');
check(!vehicle.includes('func (v *Vehicle) applyControls'), 'vehicle.legacy_apply_controls', 'Vehicle still exposes the legacy direct applyControls path');
check(!vehicle.includes('SurfaceMaterialAtTrackPosition(v.Track, v.Body.Position())'), 'vehicle.body_position_surface_fallback', 'Vehicle still infers surface material from body position when backend wheel contacts are absent');
check(!vehicle.includes('SurfaceMaterialAtTrackPosition'), 'vehicle.track_position_surface_inference', 'Vehicle still infers surface material from track position in industrial paths');
check(!contactEvent.includes('SurfaceMaterialAtTrackPosition'), 'contact.track_position_surface_inference', 'ContactEvent still infers surface material from track position');
check(!vehicleTelemetry.includes('SurfaceMaterialAtTrackPosition'), 'telemetry.track_position_surface_inference', 'VehicleTelemetry still infers wheel surface material from track position');
const setPoseBody = bodyOfFunction(vehicle, 'func (v *Vehicle) SetPose');
check(!/Body\.SetPosition\(|Body\.SetRotation\(|Body\.SetVelocity\(|Body\.SetAngularVelocity\(|Body\.Physics\.velocity|Body\.Physics\.angularVelocity/.test(setPoseBody), 'vehicle.set_pose_direct_physics_mutation', 'Vehicle.SetPose still mutates body pose or physics state directly');
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
check(!/type BlockKartPrimitiveScene|primitive3d\.NewLayer|SpawnPreparedMapPrimitiveLayers|spawnPrimitiveRoadBoxPhysics/.test(blockKartPrimitiveWorld), 'blockkart.primitive_authoring_owner', 'BlockKart still owns generic primitive/chunk/collider/surface authoring');
check(!/PrimitiveStats\(|WheelState\(|VehicleGrounded|WheelMaxSlip|PrimitiveVisibleChunks/.test(blockKartWorld), 'blockkart.low_level_hud_facts', 'BlockKart HUD still assembles low-level engine facts directly');
check(!/w\.vehicle\.(SteerAngle|WheelSpin)/.test(blockKartPrimitiveWorld), 'blockkart.visual_mutable_vehicle_state', 'BlockKart kart visual state still reads mutable vehicle steering or wheel spin fields');
check(!/w\.vehicle\.SetPose\(/.test(blockKartWorld), 'blockkart.direct_vehicle_set_pose', 'BlockKart still calls Vehicle.SetPose directly');
check(!blockKartWorld.includes('SurfaceAt(w.player.Position())'), 'blockkart.surface_position_workaround', 'BlockKart world still infers surface from player position');
check(!blockKartProductFoundation.includes('SurfaceAt(w.player.Position())'), 'blockkart.surface_position_workaround', 'BlockKart product diagnostics still infer surface from player position');
check(!blockKartWorld.includes('kartPhysicsIdle'), 'blockkart.local_physics_sleep_thresholds', 'BlockKart still owns local physics sleep thresholds');
check(blockKartWorld.includes('VehicleShouldSleepIdlePhysics'), 'blockkart.idle_sleep_helper_missing', 'BlockKart does not delegate idle sleep policy to voplay');
check(blockKartWorld.includes('VehicleAudioInputFromVehicle'), 'blockkart.audio_telemetry_missing', 'BlockKart vehicle audio does not consume voplay vehicle telemetry input');
for (const token of ['hostPacingOnly', 'resource pacing waiver', 'resourcePacingWaiver']) {
  for (const entry of blockKartVoFiles) {
    check(!entry.source.includes(token), 'blockkart.render_waiver', `BlockKart ${entry.rel} still contains ${token}`);
  }
}

if (issues.length > 0) {
  for (const issue of issues) {
    console.error(`voplay architecture lint: ${issue.code}: ${issue.detail}`);
  }
  process.exit(1);
}

console.log(`voplay architecture lint: ok voplay=${voplayRoot} blockkart=${blockKartRoot}`);
