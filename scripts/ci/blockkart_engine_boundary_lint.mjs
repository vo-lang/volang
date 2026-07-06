#!/usr/bin/env node
import { existsSync, readFileSync, readdirSync, statSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const issues = [];

function fail(message) {
  console.error(`blockkart engine boundary lint: ${message}`);
  process.exit(1);
}

function readProjectFile(projectRoot, relativePath) {
  const file = path.join(projectRoot, relativePath);
  if (!existsSync(file)) {
    fail(`${relativePath} is missing under ${projectRoot}`);
  }
  return readFileSync(file, 'utf8');
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

const vehicle = readProjectFile(voplayRoot, 'scene3d/vehicle.vo');
const dynamics = readProjectFile(voplayRoot, 'scene3d/kart_dynamics.vo');
const contactEvent = readProjectFile(voplayRoot, 'scene3d/contact_event.vo');
const vehicleTelemetry = readProjectFile(voplayRoot, 'scene3d/vehicle_telemetry.vo');
const replay = readProjectFile(voplayRoot, 'scene3d/replay.vo');
const idleSleep = readProjectFile(voplayRoot, 'scene3d/vehicle_idle_sleep.vo');
const blockKartWorld = readProjectFile(blockKartRoot, 'world.vo');
const blockKartProductFoundation = readProjectFile(blockKartRoot, 'product_foundation.vo');
const blockKartBudget = readProjectFile(blockKartRoot, 'performance_budget.vo');
const blockKartPrimitiveWorld = readProjectFile(blockKartRoot, 'primitive_world.vo');
const blockKartVoFiles = listVoFiles(blockKartRoot);

check(vehicle.includes('func (v *Vehicle) applyForceCommandToBackend'), 'voplay.backend_adapter_missing', 'voplay vehicle backend adapter is missing');
const backendAdapterBody = bodyOfFunction(vehicle, 'func (v *Vehicle) applyForceCommandToBackend');
check(backendAdapterBody.includes('BuildPhysicsBackendApplyCommand'), 'voplay.backend_apply_builder_not_used', 'voplay vehicle backend adapter does not build PhysicsBackendApplyCommand');
check(backendAdapterBody.includes('physBackend.ApplyVehicleForces') && !vehicle.includes('physBackend.SetRaycastVehicleWheelControl'), 'voplay.backend_apply_contract_not_used', 'voplay vehicle backend adapter does not route wheel controls through PhysicsBackend.ApplyVehicleForces');
check(!vehicle.includes('func (v *Vehicle) applyControls'), 'voplay.legacy_apply_controls', 'voplay still exposes direct applyControls');
check(!vehicle.includes('SurfaceMaterialAtTrackPosition'), 'voplay.vehicle_track_position_surface_inference', 'Vehicle still infers surface material from track position');
check(!contactEvent.includes('SurfaceMaterialAtTrackPosition'), 'voplay.contact_track_position_surface_inference', 'ContactEvent still infers surface material from track position');
check(!vehicleTelemetry.includes('SurfaceMaterialAtTrackPosition'), 'voplay.telemetry_track_position_surface_inference', 'VehicleTelemetry still infers surface material from track position');
const setPoseBody = bodyOfFunction(vehicle, 'func (v *Vehicle) SetPose');
check(!/Body\.SetPosition\(|Body\.SetRotation\(|Body\.SetVelocity\(|Body\.SetAngularVelocity\(|Body\.Physics\.velocity|Body\.Physics\.angularVelocity/.test(setPoseBody), 'voplay.set_pose_direct_physics_mutation', 'Vehicle.SetPose still mutates body pose or physics state directly');
const applyPoseResetBody = bodyOfFunction(vehicle, 'func (v *Vehicle) applyPoseResetToBackend');
check(!/Body\.SetPosition\(|Body\.SetRotation\(|Body\.SetVelocity\(|Body\.SetAngularVelocity\(|Body\.Physics\.velocity|Body\.Physics\.angularVelocity/.test(applyPoseResetBody), 'voplay.pose_reset_helper_direct_physics_mutation', 'Vehicle.applyPoseResetToBackend still mutates body pose or physics state directly');
check(dynamics.includes('type KartVehicleModel struct'), 'voplay.kart_model_missing', 'KartVehicleModel is missing');
check(dynamics.includes('type VehicleForceCommand struct'), 'voplay.force_command_missing', 'VehicleForceCommand is missing');
check(dynamics.includes('type PhysicsBackendApplyCommand struct'), 'voplay.backend_apply_command_missing', 'PhysicsBackendApplyCommand is missing');
check(dynamics.includes('func BuildPhysicsBackendApplyCommand'), 'voplay.backend_apply_builder_missing', 'BuildPhysicsBackendApplyCommand is missing');
check(dynamics.includes('BodyForce voplay.Vec3'), 'voplay.backend_body_force_missing', 'PhysicsBackendApplyCommand does not expose body force');
check(dynamics.includes('DebugHash int'), 'voplay.backend_debug_hash_missing', 'PhysicsBackendApplyCommand does not expose debug hash');
check(dynamics.includes('type WheelContactSample struct'), 'voplay.wheel_contact_missing', 'WheelContactSample is missing');
check(contactEvent.includes('IndustrialReady'), 'voplay.contact_industrial_missing', 'industrial contact validity helper is missing');
check(replay.includes('BackendApplyHash') || replay.includes('BackendApplyCommandHash'), 'voplay.replay_backend_apply_hash_missing', 'PhysicsReplayTrace does not record backend apply hash');
check(idleSleep.includes('VehicleShouldSleepIdlePhysics'), 'voplay.idle_sleep_helper_missing', 'vehicle idle sleep helper is missing');

check(blockKartBudget.includes('SetRenderingQuality'), 'blockkart.quality_profile_missing', 'BlockKart budget does not map to voplay quality profile');
check(!/type BlockKartPrimitiveScene|primitive3d\.NewLayer|primitive3d\.NewBuilder|primitive3d\.LayerDesc|primitive3d\.ChunkingDesc|SpawnPreparedMapPrimitiveLayers|spawnPrimitiveRoadBoxPhysics|BlockKartVisualContent|spawnPrimitiveTrackPhysics|spawnRoadColliderStrip/.test(blockKartPrimitiveWorld), 'blockkart.primitive_authoring_owner', 'BlockKart still owns generic primitive/chunk/collider/surface authoring');
check(!/PrimitiveStats\(|WheelState\(|VehicleGrounded|WheelMaxSlip|PrimitiveVisibleChunks/.test(blockKartWorld), 'blockkart.low_level_hud_facts', 'BlockKart HUD still assembles low-level engine facts directly');
check(!/w\.vehicle\.(SteerAngle|WheelSpin)/.test(blockKartPrimitiveWorld), 'blockkart.visual_mutable_vehicle_state', 'BlockKart kart visual state still reads mutable vehicle steering or wheel spin fields');
check(!/w\.vehicle\.SetPose\(/.test(blockKartWorld), 'blockkart.direct_vehicle_set_pose', 'BlockKart still calls Vehicle.SetPose directly');
check(!/w\.player\.SetPosition\(|w\.player\.SetRotation\(|w\.player\.SetVelocity\(|w\.player\.SetAngularVelocity\(/.test(blockKartWorld), 'blockkart.direct_player_physics_mutation', 'BlockKart world still directly mutates player pose, velocity, or angular velocity');
const directEntityMutation = /\b[A-Za-z_][A-Za-z0-9_]*(?:\.[A-Za-z_][A-Za-z0-9_]*)*\.Set(Position|Rotation|Velocity|AngularVelocity)\(/;
for (const entry of blockKartVoFiles) {
  check(!directEntityMutation.test(entry.source), 'blockkart.direct_entity_physics_mutation', `${entry.rel} directly mutates entity pose, velocity, or angular velocity`);
}
check(blockKartWorld.includes('VehicleShouldSleepIdlePhysics'), 'blockkart.idle_sleep_policy_owned', 'BlockKart does not delegate idle sleep policy to voplay');
check(blockKartVoFiles.some((entry) => entry.source.includes('VehicleAudioInputFromTelemetry')), 'blockkart.audio_telemetry_missing', 'BlockKart vehicle audio does not start from voplay telemetry');
check(!blockKartWorld.includes('SurfaceAt(w.player.Position())'), 'blockkart.surface_position_workaround', 'BlockKart world still infers surface by player position');
check(!blockKartProductFoundation.includes('SurfaceAt(w.player.Position())'), 'blockkart.surface_position_workaround', 'BlockKart diagnostics still infer surface by player position');
check(!blockKartWorld.includes('kartPhysicsIdle'), 'blockkart.local_physics_sleep_thresholds', 'BlockKart still owns local physics sleep thresholds');

for (const token of ['BlockKartRenderBudget', 'PostProcessConfig', 'ShadowResolution', 'applyBlockKartRenderBudget', 'defaultBlockKartRenderBudget', 'hostPacingOnly', 'resourcePacingWaiver', 'SetRaycastVehicleWheelControl', 'ContactEventSourceBackendPairFallback', 'FallbackContact']) {
  for (const entry of blockKartVoFiles) {
    check(!entry.source.includes(token), 'blockkart.engine_workaround', `${entry.rel} still contains ${token}`);
  }
}

if (issues.length > 0) {
  for (const issue of issues) {
    console.error(`blockkart engine boundary lint: ${issue.code}: ${issue.detail}`);
  }
  process.exit(1);
}

console.log(`blockkart engine boundary lint: ok voplay=${voplayRoot} blockkart=${blockKartRoot}`);
