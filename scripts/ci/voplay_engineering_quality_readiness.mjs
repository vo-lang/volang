#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { acceptedVolangPlanSnapshotCommits } from './active_plan_snapshot.mjs';
import { requireRepoRoot } from './repo_roots.mjs';
import {
  sourceBoundEvidence,
  verifySourceBoundEvidence,
} from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const currentCiRunId = process.env.VO_DEV_CI_RUN_ID ?? null;
const args = process.argv.slice(2);
let outDir = path.join(root, 'target', 'voplay-engineering-quality-readiness');
for (let i = 0; i < args.length; i++) {
  const arg = args[i];
  if (arg === '--out-dir') {
    outDir = path.resolve(args[++i]);
  } else {
    console.error(`voplay engineering quality readiness: unknown argument ${arg}`);
    process.exit(2);
  }
}

function readText(file) {
  return readFileSync(file, 'utf8');
}

function projectText(projectRoot, relativePath) {
  const file = path.join(projectRoot, relativePath);
  if (!existsSync(file)) {
    return '';
  }
  return readText(file);
}

function projectTexts(projectRoot, relativePaths) {
  return relativePaths.map((relativePath) => projectText(projectRoot, relativePath)).join('\n');
}

function lineCount(source) {
  return source.length === 0 ? 0 : source.split(/\r?\n/).length;
}

function productionSource(source) {
  return source.split('#[cfg(test)]')[0] || source;
}

function sha256Field(value) {
  return typeof value === 'string' && /^sha256:[0-9a-f]{64}$/.test(value);
}

function listFiles(projectRoot, extension) {
  const files = [];
  const visit = (dir) => {
    for (const entry of readdirSync(dir)) {
      const file = path.join(dir, entry);
      const stat = statSync(file);
      if (stat.isDirectory()) {
        if (entry === '.git' || entry === 'target' || entry === 'node_modules') continue;
        visit(file);
      } else if (file.endsWith(extension)) {
        files.push({
          rel: path.relative(projectRoot, file).split(path.sep).join('/'),
          source: readText(file),
        });
      }
    }
  };
  visit(projectRoot);
  return files;
}

function jsonFile(relativePath) {
  return JSON.parse(readText(path.join(root, relativePath)));
}

function jsonMaybe(relativePath) {
  const file = path.join(root, relativePath);
  if (!existsSync(file)) {
    return null;
  }
  try {
    return JSON.parse(readText(file));
  } catch {
    return null;
  }
}

function bodyOfFunction(source, signature) {
  const start = source.indexOf(signature);
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

function blockAfterToken(source, token) {
  const start = source.indexOf(token);
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

function gitOutput(args, cwd) {
  try {
    return { ok: true, stdout: execFileSync('git', args, { cwd, encoding: 'utf8' }).trim() };
  } catch (error) {
    return {
      ok: false,
      error: String(error?.stderr || error?.message || error).trim(),
    };
  }
}

function repoEnvName(repo) {
  let out = '';
  for (const ch of repo) {
    out += /[A-Za-z0-9]/.test(ch) ? ch.toUpperCase() : '_';
  }
  return `${out}_ROOT`;
}

function parseExpectedCommits(projectSource) {
  const expected = new Map();
  let current = null;
  for (const rawLine of projectSource.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (line === '[[first_party]]' || line === '[[external_project]]') {
      current = {};
      continue;
    }
    if (!current) continue;
    const pair = line.match(/^([A-Za-z0-9_]+)\s*=\s*"([^"]*)"$/);
    if (!pair) continue;
    current[pair[1]] = pair[2];
    if (current.name && current.expected_commit) {
      expected.set(current.name, current.expected_commit);
    }
  }
  return expected;
}

function activePlanSnapshot(activePlanSource) {
  const snapshot = {};
  for (const rawLine of activePlanSource.split(/\r?\n/)) {
    const match = rawLine.match(/^(volang|voplay|BlockKart)\s+([0-9a-f]{40})$/);
    if (match) snapshot[match[1]] = match[2];
  }
  return snapshot;
}

function repoHead(projectRoot) {
  const head = gitOutput(['rev-parse', 'HEAD'], projectRoot);
  return head.ok ? head.stdout : null;
}

function tomlBlocks(source, header) {
  const marker = `[[${header}]]`;
  return source
    .split(marker)
    .slice(1)
    .map((block) => block.split(/\n\[\[/)[0]);
}

function tomlStringValue(block, key) {
  const match = block.match(new RegExp(`^${key}\\s*=\\s*"([^"]*)"`, 'm'));
  return match ? match[1] : '';
}

function tomlStringArrayValue(block, key) {
  const match = block.match(new RegExp(`^${key}\\s*=\\s*\\[([\\s\\S]*?)\\]`, 'm'));
  if (!match) return [];
  return [...match[1].matchAll(/"([^"]+)"/g)].map((entry) => entry[1]);
}

function taskNeeds(source, taskName) {
  const block = tomlBlocks(source, 'task').find((entry) => tomlStringValue(entry, 'name') === taskName);
  return block ? tomlStringArrayValue(block, 'needs') : [];
}

function currentNodeAuditWorkspaces(source) {
  return tomlBlocks(source, 'node_workspace')
    .map((block) => ({
      name: tomlStringValue(block, 'name'),
      audit: tomlStringValue(block, 'audit'),
      auditLevel: tomlStringValue(block, 'audit_level'),
      status: tomlStringValue(block, 'status'),
    }))
    .filter((workspace) => workspace.audit === 'current');
}

function crossRepoHeadMismatches(expectedCommits) {
  const mismatches = [];
  for (const [repo, expected] of expectedCommits.entries()) {
    const envName = repoEnvName(repo);
    const rootValue = process.env[envName];
    if (!rootValue || String(rootValue).trim() === '') {
      continue;
    }
    const repoRoot = path.resolve(rootValue);
    const head = gitOutput(['rev-parse', 'HEAD'], repoRoot);
    if (!head.ok || head.stdout !== expected) {
      mismatches.push({
        repo,
        root: repoRoot,
        expected,
        found: head.ok ? head.stdout : null,
        error: head.error ?? null,
      });
    }
  }
  return mismatches;
}

function repoState(repo, repoRoot, expectedCommit = '') {
  const head = gitOutput(['rev-parse', 'HEAD'], repoRoot);
  const status = gitOutput(['status', '--porcelain'], repoRoot);
  return {
    repo,
    root: repoRoot,
    expectedCommit,
    head: head.ok ? head.stdout : null,
    headError: head.error ?? null,
    dirty: status.ok ? status.stdout !== '' : null,
    status: status.ok ? status.stdout : '',
    statusError: status.error ?? null,
  };
}

function dirtyProvenanceEvidence(provenance) {
  const dirtyEntries = [];
  if (provenance?.project?.dirty === true) {
    dirtyEntries.push({
      owner: provenance.project.module || 'project',
      kind: 'project',
      commit: provenance.project.commit ?? null,
    });
  }
  for (const dependency of provenance?.dependencies ?? []) {
    if (dependency?.dirty === true) {
      dirtyEntries.push({
        owner: dependency.module || '(unknown)',
        kind: 'dependency',
        source: dependency.source ?? null,
        version: dependency.version ?? null,
        commit: dependency.commit ?? null,
      });
    }
  }
  return dirtyEntries;
}

function lineOf(source, token) {
  const lines = source.split(/\r?\n/);
  for (let i = 0; i < lines.length; i++) {
    if (lines[i].includes(token)) return i + 1;
  }
  return null;
}

function lineOfAfter(source, startToken, token) {
  const start = source.indexOf(startToken);
  if (start < 0) return lineOf(source, token);
  const line = lineOf(source.slice(start), token);
  if (line === null) return null;
  return source.slice(0, start).split(/\r?\n/).length + line - 1;
}

function packetDecodeBranchFailures(sceneSource, physicsWorldSource) {
  const specs = [
    {
      path: 'voplay/scene3d/physics_world.vo',
      sourceText: physicsWorldSource,
      signature: 'func (w *PhysicsWorldState) step',
      token: 'if headerError != ""',
      source: 'body',
      reason: 'invalid_header',
      reasonToken: 'Reason: headerError',
      expectedReturn: 'PhysicsBackendPacketError{',
    },
    {
      path: 'voplay/scene3d/physics_world.vo',
      sourceText: physicsWorldSource,
      signature: 'func (w *PhysicsWorldState) step',
      token: 'if r.Remaining() < 4',
      source: 'body',
      reason: 'missing_count',
      expectedReturn: 'PhysicsBackendPacketError{',
    },
    {
      path: 'voplay/scene3d/physics_world.vo',
      sourceText: physicsWorldSource,
      signature: 'func (w *PhysicsWorldState) step',
      token: 'if r.Remaining() != count * physics3DBodyStateBytes',
      source: 'body',
      reason: 'body_payload_length_mismatch',
      expectedReturn: 'PhysicsBackendPacketError{',
    },
    {
      path: 'voplay/scene3d/scene.vo',
      sourceText: sceneSource,
      signature: 'func (s *Scene) Contacts',
      token: 'if headerError != ""',
      source: 'contact',
      reason: 'invalid_header',
      reasonToken: 'Reason: headerError',
      expectedReturn: 'return []Contact{}',
    },
    {
      path: 'voplay/scene3d/scene.vo',
      sourceText: sceneSource,
      signature: 'func (s *Scene) Contacts',
      token: 'if r.Remaining() < 4',
      source: 'contact',
      reason: 'missing_count',
      expectedReturn: 'return []Contact{}',
    },
    {
      path: 'voplay/scene3d/scene.vo',
      sourceText: sceneSource,
      signature: 'func (s *Scene) Contacts',
      token: 'if r.Remaining() != count * physicsContactDetailBytes',
      source: 'contact',
      reason: 'contact_payload_length_mismatch',
      expectedReturn: 'return []Contact{}',
    },
  ];
  const failures = [];
  for (const spec of specs) {
    const body = bodyOfFunction(spec.sourceText, spec.signature);
    const branch = blockAfterToken(body, spec.token);
    const routesStructuredError = spec.source === 'body'
      ? branch.includes('PhysicsBackendPacketError{') && branch.includes(', true')
      : branch.includes('recordPhysicsBackendPacketError');
    const structured = routesStructuredError
      && branch.includes(`Source: "${spec.source}"`)
      && branch.includes(spec.reasonToken ?? `Reason: "${spec.reason}"`)
      && branch.includes('ExpectedBytes:')
      && branch.includes('ActualBytes:')
      && branch.includes(spec.expectedReturn);
    if (!structured) {
      failures.push({
        path: spec.path,
        line: lineOfAfter(spec.sourceText, spec.signature, spec.token) ?? lineOf(spec.sourceText, spec.signature),
        text: `${spec.signature} ${spec.token}`,
        reason: `malformed ${spec.source} packet branch must record PhysicsBackendPacketError reason ${spec.reason} before returning`,
      });
    }
  }
  const contactsBody = bodyOfFunction(sceneSource, 'func (s *Scene) Contacts');
  if (/^\s*return nil\s*$/m.test(contactsBody)) {
    failures.push({
      path: 'voplay/scene3d/scene.vo',
      line: lineOfAfter(sceneSource, 'func (s *Scene) Contacts', 'return nil'),
      text: 'return nil',
      reason: 'Contacts must return an empty slice with packet error telemetry on malformed packets',
    });
  }
  return failures;
}

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function emptyOwnerModules(projectRoot) {
  const modules = [];
  const pipelineDir = path.join(projectRoot, 'rust/src/pipeline3d');
  if (!existsSync(pipelineDir)) return modules;
  const rustFiles = listFiles(projectRoot, '.rs');
  const productionSource = rustFiles
    .map((file) => file.source)
    .join('\n');
  for (const entry of readdirSync(pipelineDir)) {
    if (!entry.endsWith('_submitter.rs')) continue;
    const rel = `rust/src/pipeline3d/${entry}`;
    const source = projectText(projectRoot, rel);
    const hasProductionMethod = /\bpub(?:\(crate\)|\(super\))?\s+fn\s+(prepare|upload|draw|bind|submit|load|compile|create|resolve|cache)\w*\b/.test(source)
      || /\bfn\s+(prepare|upload|draw|bind|submit|load|compile|create|resolve|cache)\w*\b/.test(source);
    const hasMeaningfulImpl = /impl\s+\w+[\s\S]*\{[\s\S]*\bfn\b/.test(source);
    const ownerName = entry
      .replace(/\.rs$/, '')
      .split('_')
      .map((part) => `${part.slice(0, 1).toUpperCase()}${part.slice(1)}`)
      .join('');
    const calledByProduction = productionSource.includes(`${ownerName}::`) || productionSource.includes(`${ownerName} {`);
    if (!hasProductionMethod || !hasMeaningfulImpl || !calledByProduction) {
      modules.push({
        path: rel,
        owner: ownerName,
        hasProductionMethod,
        hasMeaningfulImpl,
        calledByProduction,
        lineCount: lineCount(source),
      });
    }
  }
  return modules;
}

function findBlockKartLowLevelFacts(files) {
  const forbidden = [
    'ApplyVehicleConstraint(',
    'ApplyEntityPhysicsConstraint(',
    'PrimitiveStats(',
    'primitive3d.NewLayer',
    'primitive3d.NewBuilder',
    'primitive3d.LayerDesc',
    'primitive3d.ChunkingDesc',
    'primitive3d.MaterialDesc',
  ];
  const hits = [];
  for (const file of files) {
    for (const token of forbidden) {
      const line = lineOf(file.source, token);
      if (line !== null) {
        hits.push({ path: file.rel, token, line });
      }
    }
  }
  return hits;
}

function regexHits(entries, regex) {
  const hits = [];
  for (const entry of entries) {
    const lines = entry.source.split(/\r?\n/);
    for (let i = 0; i < lines.length; i++) {
      if (regex.test(lines[i])) {
        hits.push({ path: entry.rel || entry.path, line: i + 1, text: lines[i].trim() });
      }
    }
  }
  return hits;
}

function sourceAuditFailuresFromSource({
  rendererHotPathEntries,
  frameGraph,
  framePassSequence,
  rendererPassDispatch,
  renderPassSources,
  renderWorld,
  voplayPhysics,
  voplayPhysicsWorld,
  voplayVehicle,
  voplayDynamics,
  kartController,
  vehicleTelemetry,
  vehiclePhysicsSession,
  physicsStressSource,
  voplayToolSources,
  sceneSource,
  replaySource,
  blockKartFiles,
}) {
  const failures = [];
  const manualFrameGraphSequence =
    framePassSequence.includes('FrameGraphPlanNodes')
    || /\.execute_node\(&context\.nodes\./.test(framePassSequence)
    || frameGraph.includes('struct FrameGraphPlanNodes');
  const framePassSequenceUsesExecuteAll =
    /\.executor\(\)\s*\.\s*execute_all\(&mut dispatcher\)/s.test(framePassSequence);
  if (!/\bfn\s+execute_all\b/.test(frameGraph) || !framePassSequenceUsesExecuteAll || manualFrameGraphSequence) {
    failures.push({
      code: 'render.framegraph_manual_sequence',
      severity: 'P1',
      owner: 'voplay/render',
      message: 'FrameGraph does not own full pass execution order through execute_all',
      evidence: {
        hasExecuteAll: /\bfn\s+execute_all\b/.test(frameGraph),
        framePassSequenceUsesExecuteAll,
        manualFrameGraphSequence,
        frameGraphPlanNodesLine: lineOf(framePassSequence, 'FrameGraphPlanNodes') ?? lineOf(frameGraph, 'struct FrameGraphPlanNodes'),
      },
    });
  }
  if (/renderer:\s*&'?[_A-Za-z0-9]*\s*mut\s+Renderer\b/.test(`${rendererPassDispatch}\n${renderPassSources}`)) {
    failures.push({
      code: 'render.dispatcher_global_renderer',
      severity: 'P1',
      owner: 'voplay/render',
      message: 'Render pass dispatch contexts still capture &mut Renderer',
      evidence: { path: 'voplay/rust/src/renderer/pass_dispatch.rs', line: lineOf(rendererPassDispatch, 'renderer:') },
    });
  }
  const hotPathPanicHits = regexHits(rendererHotPathEntries, /\b(panic!|expect\s*\(|unwrap\s*\(|assert!\s*\(|assert_eq!\s*\(|assert_ne!\s*\(|todo!|unimplemented!)/);
  const dirtyRangeFullRebuildHits = regexHits(rendererHotPathEntries, /rebuild_resident_chunk\(device,\s*queue,\s*range\.chunk_ref,\s*models\)/);
  const silentBatchSkip = /\.filter_map\(\|[^|]*(?:index|draw_index|chunk_index)[^|]*\|[\s\S]*?\.get\(/.test(renderWorld);
  if (hotPathPanicHits.length > 0 || silentBatchSkip) {
    failures.push({
      code: 'render.hot_path_panic_or_silent_skip',
      severity: 'P1',
      owner: 'voplay/render',
      message: 'Render hot paths still contain panic-prone calls or silent invalid batch index drops',
      evidence: {
        hotPathPanicHits: hotPathPanicHits.slice(0, 40),
        silentSkipLine: lineOf(renderWorld, '.filter_map('),
      },
    });
  }
  if (dirtyRangeFullRebuildHits.length > 0) {
    failures.push({
      code: 'render.dirty_range_full_rebuild',
      severity: 'P0',
      owner: 'voplay/render',
      message: 'Dirty resident instance ranges still rebuild or rewrite full resident buffers instead of partial uploads',
      evidence: { hits: dirtyRangeFullRebuildHits },
    });
  }
  if (/struct RenderPassResources[\s\S]*\b(device|queue|pipeline3d|primitive_pipeline|model_manager|texture_manager|render_world):/.test(rendererPassDispatch)) {
    failures.push({
      code: 'render.pass_context_too_wide',
      severity: 'P1',
      owner: 'voplay/render',
      message: 'RenderPassResources still exposes broad renderer resource bundles to pass executors',
      evidence: { path: 'voplay/rust/src/renderer/pass_dispatch.rs', line: lineOf(rendererPassDispatch, 'struct RenderPassResources') },
    });
  }
  if (!(frameGraph.includes('backing_generation') && frameGraph.includes('actual_texture_view') && frameGraph.includes('actual_backing_identity') && frameGraph.includes('validate_backing('))) {
    failures.push({
      code: 'render.resource_registry_generation_unverified',
      severity: 'P1',
      owner: 'voplay/render',
      message: 'FrameGraph resource readiness lacks current backing target generation and actual view identity agreement',
      evidence: { path: 'voplay/rust/src/renderer_frame.rs', line: lineOf(frameGraph, 'struct RenderTargetStatus') },
    });
  }
  const backendBody = bodyOfFunction(voplayPhysics, 'func (defaultPhysicsBackend) ApplyVehicleForces');
  const backendFields = ['BodyForce', 'DragForce', 'Downforce', 'WaterLift', 'AirControl', 'WallGrip', 'RailGrip'];
  const unconsumedFields = backendFields.filter((field) => !backendBody.includes(`command.${field}`));
  if (unconsumedFields.length > 0) {
    failures.push({
      code: 'physics.backend_apply_fields_unconsumed',
      severity: 'P0',
      owner: 'voplay/scene3d',
      message: 'PhysicsBackendApplyCommand fields are not consumed by the default backend apply path',
      evidence: { fields: unconsumedFields, path: 'voplay/scene3d/physics.vo' },
    });
  }
  const applyForceBody = bodyOfFunction(voplayVehicle, 'func (v *Vehicle) ApplyForceCommand');
  if (applyForceBody.includes('applyForceCommandToBackend') && applyForceBody.includes('v.Body.ApplyForce')) {
    failures.push({
      code: 'physics.post_backend_body_force',
      severity: 'P0',
      owner: 'voplay/scene3d',
      message: 'ApplyForceCommand applies core body forces after backend apply',
      evidence: { path: 'voplay/scene3d/vehicle.vo', line: lineOf(voplayVehicle, 'v.Body.ApplyForce') },
    });
  }
  const emptyHooks = [
    'ApplyPoseReset',
    'ApplyMotionReset',
    'ApplySleepState',
    'ApplyRecovery',
  ].filter((hook) => new RegExp(`func \\(defaultPhysicsBackend\\) ${hook}[^{}]*\\{\\}`).test(voplayPhysics));
  if (emptyHooks.length > 0) {
    failures.push({
      code: 'physics.backend_reset_hooks_empty',
      severity: 'P0',
      owner: 'voplay/scene3d',
      message: 'Default physics backend reset/recovery/sleep hooks are empty',
      evidence: { hooks: emptyHooks, path: 'voplay/scene3d/physics.vo' },
    });
  }
  if (kartController.includes('Track.SurfaceAt') && kartController.includes('Body.Position()')) {
    failures.push({
      code: 'physics.surface_authority_body_position',
      severity: 'P1',
      owner: 'voplay/scene3d',
      message: 'Kart controller still derives control surface from body position',
      evidence: { path: 'voplay/scene3d/kart_controller.vo', line: lineOf(kartController, 'Track.SurfaceAt') },
    });
  }
  if (/^func \(v \*Vehicle\) ApplyForceCommand\(/m.test(voplayVehicle)) {
    failures.push({
      code: 'physics.public_force_command_bypass',
      severity: 'P0',
      owner: 'voplay/scene3d',
      message: 'Vehicle.ApplyForceCommand remains public and can bypass VehiclePhysicsSession intent authority',
      evidence: { path: 'voplay/scene3d/vehicle.vo', line: lineOf(voplayVehicle, 'func (v *Vehicle) ApplyForceCommand') },
    });
  }
  const rawPhysicsBypassHits = regexHits([{ path: 'voplay/scene3d/physics_commands.vo', source: voplayPhysics }], /^func \(e \*Entity\) (SetPosition|SetVelocity|SetAngularVelocity)\(/)
    .filter((hit) => !bodyOfFunction(voplayPhysics, hit.text.replace(/\s*\{\s*$/, '')).includes('ApplyPhysicsTarget(PhysicsBackendTargetCommand'));
  if (rawPhysicsBypassHits.length > 0 || /^func ProductStepAndSyncPhysics\(/m.test(sceneSource)) {
    failures.push({
      code: 'physics.raw_public_bypass',
      severity: 'P0',
      owner: 'voplay/scene3d',
      message: 'Raw entity physics mutation or ProductStepAndSyncPhysics can bypass backend contract authority',
      evidence: { rawPhysicsBypassHits, productStepLine: lineOf(sceneSource, 'func ProductStepAndSyncPhysics') },
    });
  }
  const malformedPacketBranchFailures = packetDecodeBranchFailures(sceneSource, voplayPhysicsWorld);
  if (malformedPacketBranchFailures.length > 0 || !physicsStressSource.includes('PacketErrors')) {
    failures.push({
      code: 'physics.malformed_packet_silent',
      severity: 'P0',
      owner: 'voplay/scene3d',
      message: 'Malformed body/contact backend packets can return silently and stress lacks packet error accounting',
      evidence: { malformedPacketBranchFailures: malformedPacketBranchFailures.slice(0, 40), packetErrorsLine: lineOf(physicsStressSource, 'PacketErrors') },
    });
  }
  const invalidSampleLine = lineOf(vehicleTelemetry, 'InvalidSampleCount: invalidSamples');
  const rawPositionLine = lineOf(vehicleTelemetry, 'rawPosition');
  const cleanPositionLine = lineOf(vehicleTelemetry, 'vehicleCleanVec3(rawPosition)');
  const telemetryCountsRawBeforeClean = rawPositionLine !== null
    && invalidSampleLine !== null
    && cleanPositionLine !== null
    && invalidSampleLine < cleanPositionLine;
  if (!telemetryCountsRawBeforeClean) {
    failures.push({
      code: 'physics.telemetry_invalid_sanitized_first',
      severity: 'P1',
      owner: 'voplay/scene3d',
      message: 'Vehicle telemetry sanitizes invalid raw samples before industrial health accounting',
      evidence: { path: 'voplay/scene3d/vehicle_telemetry.vo', rawPositionLine, invalidSampleLine, cleanPositionLine },
    });
  }
  const blockKartLowLevelFacts = findBlockKartLowLevelFacts(blockKartFiles);
  if (blockKartLowLevelFacts.length > 0) {
    failures.push({
      code: 'blockkart.low_level_engine_facts',
      severity: 'P0',
      owner: 'BlockKart',
      message: 'BlockKart still reads or mutates low-level physics/render facts',
      evidence: { hits: blockKartLowLevelFacts },
    });
  }
  if (!voplayDynamics.includes('type PhysicsBackendApplyCommand struct')) {
    failures.push({
      code: 'physics.backend_contract_missing',
      severity: 'P0',
      owner: 'voplay/scene3d',
      message: 'PhysicsBackendApplyCommand contract is missing',
      evidence: { path: 'voplay/scene3d/kart_dynamics.vo' },
    });
  }
  const sessionStepBody = bodyOfFunction(vehiclePhysicsSession, 'func (s *VehiclePhysicsSession) Step');
  if (/FixedPhysicsStep\s*\(|StepAndSyncPhysics\s*\(/.test(sessionStepBody) || !/s\.(?:StepIndex|Telemetry|Replay|Backend|LastPacket|InvalidSample|Controller|Dynamics)/.test(sessionStepBody)) {
    failures.push({
      code: 'physics.session_wrapper_only',
      severity: 'P1',
      owner: 'voplay/scene3d',
      message: 'VehiclePhysicsSession is still a wrapper around scene/controller stepping',
      evidence: { path: 'voplay/scene3d/vehicle_physics_session.vo', line: lineOf(vehiclePhysicsSession, 'func (s *VehiclePhysicsSession) Step') },
    });
  }
  if (/UpdateIntent\s*\(/.test(physicsStressSource) || /StepAndSyncPhysics\s*\(/.test(physicsStressSource)) {
    failures.push({
      code: 'physics.stress_bypasses_session',
      severity: 'P1',
      owner: 'voplay/scene3d',
      message: 'Physics stress bypasses VehiclePhysicsSession with direct controller or Scene fixed-step calls',
      evidence: {
        updateIntentLine: lineOf(physicsStressSource, 'UpdateIntent'),
        stepAndSyncLine: lineOf(physicsStressSource, 'StepAndSyncPhysics'),
      },
    });
  }
  if (/UpdateIntent\s*\(/.test(voplayToolSources) || /StepAndSyncPhysics\s*\(/.test(voplayToolSources)) {
    failures.push({
      code: 'physics.tools_bypass_session',
      severity: 'P1',
      owner: 'voplay/scene3d',
      message: 'Voplay tools still bypass VehiclePhysicsSession with direct vehicle/controller update or Scene fixed-step calls',
      evidence: {
        updateIntentLine: lineOf(voplayToolSources, 'UpdateIntent'),
        stepAndSyncLine: lineOf(voplayToolSources, 'StepAndSyncPhysics'),
      },
    });
  }
  const packetContractSource = `${sceneSource}\n${voplayPhysics}\n${voplayDynamics}\n${replaySource}`;
  const packetTokens = ['PhysicsBackendPacketSchemaVersion', 'PhysicsBackendPacketKind', 'PhysicsBackendPacketLength', 'PhysicsBackendPacketHash', 'PhysicsBackendCapability'];
  const missingPacketTokens = packetTokens.filter((token) => !packetContractSource.includes(token));
  const enforcedPacketTokens = ['WritePhysicsBackendPacketHeader', 'ReadPhysicsBackendPacketHeader', 'ValidatePhysicsBackendPacketHeader'];
  const missingEnforcedPacketTokens = enforcedPacketTokens.filter((token) => !packetContractSource.includes(token) && !sceneSource.includes(token));
  if (missingPacketTokens.length > 0 || missingEnforcedPacketTokens.length > 0 || /Remaining\(\)\s*>=\s*count\s*\*/.test(sceneSource)) {
    failures.push({
      code: 'physics.backend_packet_schema_missing',
      severity: 'P0',
      owner: 'voplay/scene3d',
      message: 'Physics backend packet contract lacks enforced schema header, version, length, hash, or capability validation',
      evidence: { missingPacketTokens, missingEnforcedPacketTokens, remainingCountLine: lineOf(sceneSource, 'Remaining() >= count') },
    });
  }
  if (!voplayVehicle.includes('RawInvalidSampleCount') || !vehicleTelemetry.includes('RawInvalidSampleCount')) {
    failures.push({
      code: 'physics.invalid_wheel_packet_sanitized',
      severity: 'P1',
      owner: 'voplay/scene3d',
      message: 'Raycast wheel packet invalid raw samples can be sanitized before telemetry accounts for them',
      evidence: { vehicleSyncLine: lineOf(voplayVehicle, 'func (v *Vehicle) syncState') },
    });
  }
  if (!physicsStressSource.includes('InvalidSampleCount') || !physicsStressSource.includes('ValidationIssues') || /sampleFinite\s*\(/.test(physicsStressSource)) {
    failures.push({
      code: 'physics.invalid_telemetry_can_be_sanitized',
      severity: 'P1',
      owner: 'voplay/scene3d',
      message: 'Industrial telemetry can still pass through cleaned finite samples without invalid raw-sample evidence',
      evidence: {
        invalidSampleLine: lineOf(physicsStressSource, 'InvalidSampleCount'),
        validationIssuesLine: lineOf(physicsStressSource, 'ValidationIssues'),
        sampleFiniteLine: lineOf(physicsStressSource, 'sampleFinite'),
      },
    });
  }
  const replayTokens = [
    'PhysicsReplayVerifier',
    'StepHash',
    'BackendPacketHash',
    'ReplayMismatch',
    'ValidatePhysicsReplayTrace',
    'PhysicsPoseHash',
    'PhysicsTelemetryHash',
    'Mismatches',
  ];
  const missingReplayTokens = replayTokens.filter((token) => !replaySource.includes(token) && !physicsStressSource.includes(token));
  const replayUsesRecordedTrace = /runScenarioWithReplay\s*\([^)]*true/.test(physicsStressSource)
    && physicsStressSource.includes('Replays []ReplayReport')
    && physicsStressSource.includes('for _, def := range defs')
    && physicsStressSource.includes('ValidatePhysicsReplaySample')
    && physicsStressSource.includes('ValidatePhysicsBackendCommandReplay')
    && physicsStressSource.includes('StepFleet');
  const freshProcessReplayReady = physicsStressSource.includes('RunPhysicsReplayVerifierProcess')
    || physicsStressSource.includes('execPhysicsReplayVerifier')
    || physicsStressSource.includes('--verify-replay-trace');
  if (missingReplayTokens.length > 0 || !replayUsesRecordedTrace || !freshProcessReplayReady || /replayDrift|driftMeters|ReplayDrift/.test(physicsStressSource)) {
    failures.push({
      code: 'physics.replay_not_executable_contract',
      severity: 'P1',
      owner: 'voplay/scene3d',
      message: 'Physics replay validation lacks a fresh-process executable per-step contract',
      evidence: {
        missingReplayTokens,
        replayUsesRecordedTrace,
        freshProcessReplayReady,
        driftLine: lineOf(physicsStressSource, 'driftMeters') ?? lineOf(physicsStressSource, 'replayDrift'),
      },
    });
  }
  const blockKartGenericAuthoring = [];
  const genericAuthoring = /ProductPrimitiveAuthoring|ProductPrimitive(Place|Dynamic|SetPose)|ProductPrimitives\.(Place|Dynamic|BuildLayer|Material|Shape|Layer)|primitive3d\.(NewLayer|NewBuilder|LayerDesc|ChunkingDesc|MaterialDesc)|ProductSpawnTrackColliderStrip|PackWriter|vopack\./;
  const blockKartNamedAuthoring = [];
  const blockKartNamedAuthoringRegex = /NewBlockKartPrimitiveContent|PrepareBlockKartMapAsset|SpawnBlockKartMap|SpawnBlockKartRoadsideBakedContent|SetBlockKartPrimitivePose|attachPrimitiveTrackColliderEntities|spawnBlockKartTrackCollider|primitiveTerrainSurfacePosition/;
  const blockKartDirectIntentBypass = [];
  const blockKartDirectIntentRegex = /UpdateIntentFromSyncedState\s*\(|\b[A-Za-z_][A-Za-z0-9_]*\.UpdateIntent\s*\(/;
  const blockKartRuntimeContextMegaOwner = [];
  const blockKartOwnerContextMethods = [];
  for (const file of blockKartFiles) {
    const line = file.source.split(/\r?\n/).findIndex((sourceLine) => genericAuthoring.test(sourceLine));
    if (line >= 0 && !file.rel.startsWith('runtimepack/')) blockKartGenericAuthoring.push({ path: file.rel, line: line + 1 });
    const namedLine = file.source.split(/\r?\n/).findIndex((sourceLine) => blockKartNamedAuthoringRegex.test(sourceLine));
    if (namedLine >= 0) blockKartNamedAuthoring.push({ path: file.rel, line: namedLine + 1 });
    const directIntentLine = file.source.split(/\r?\n/).findIndex((sourceLine) => blockKartDirectIntentRegex.test(sourceLine));
    if (directIntentLine >= 0) blockKartDirectIntentBypass.push({ path: file.rel, line: directIntentLine + 1 });
    if (file.rel === 'world.vo') {
      const contextBody = bodyOfType(file.source, 'BlockKartRuntimeContext');
      const contextFields = contextBody
        .split(/\r?\n/)
        .map((sourceLine, index) => ({ line: index + 1, text: sourceLine.trim() }))
        .filter((entry) => /^[A-Za-z_][A-Za-z0-9_]*\s+/.test(entry.text));
      const allowedContextGroups = new Set([
        'core BlockKartRuntimeCore',
        'input BlockKartRuntimeInputState',
        'race BlockKartRuntimeRaceState',
        'kart BlockKartRuntimeKartState',
        'hud BlockKartRuntimeHudState',
      ]);
      const forbiddenContextFields = contextFields.filter((entry) => (
        !allowedContextGroups.has(entry.text)
        && /\b(scene|camera|player|vehicle|kartController|racingInput|touch|vehicleAudio|assets|primitive|track|checkpoint|raceState|courseTime|finishTime|collected|lap|kart|boost|drift|collectibles|checkpoints|boostPads|obstacles|debugHud|perf|physics)/i.test(entry.text)
      ));
      if (contextBody.trim() !== '' || contextFields.length > 0 || forbiddenContextFields.length > 0 || !contextFields.every((entry) => allowedContextGroups.has(entry.text))) {
        blockKartRuntimeContextMegaOwner.push({ path: file.rel, line: lineOf(file.source, 'type BlockKartRuntimeContext struct'), fieldCount: contextFields.length, forbiddenFields: forbiddenContextFields.slice(0, 20) });
      }
    }
    for (const match of file.source.matchAll(/^func \([^)]*\*(RaceSession|KartRig|TrackRuntime|HUDPresenter|PerfReporter|AssetRuntimeCache)\) ([A-Za-z_][A-Za-z0-9_]*)\([^)]*\*BlockKartRuntimeContext/gm)) {
      blockKartOwnerContextMethods.push({ path: file.rel, owner: match[1], name: match[2], line: lineOf(file.source, `func (`) });
    }
  }
  if (blockKartGenericAuthoring.length > 0) {
    failures.push({
      code: 'blockkart.generic_authoring_boundary',
      severity: 'P0',
      owner: 'BlockKart',
      message: 'BlockKart still owns generic primitive, collider, map, or pack authoring',
      evidence: { hits: blockKartGenericAuthoring },
    });
  }
  if (blockKartNamedAuthoring.length > 0) {
    failures.push({
      code: 'blockkart.named_authoring_boundary',
      severity: 'P0',
      owner: 'BlockKart',
      message: 'BlockKart still owns BlockKart-named primitive, map, collider, or dynamic visual authoring wrappers',
      evidence: { hits: blockKartNamedAuthoring },
    });
  }
  if (blockKartDirectIntentBypass.length > 0) {
    failures.push({
      code: 'blockkart.direct_vehicle_intent_bypass',
      severity: 'P0',
      owner: 'BlockKart',
      message: 'BlockKart updates controller or vehicle intent outside VehiclePhysicsSession',
      evidence: { hits: blockKartDirectIntentBypass },
    });
  }
  if (blockKartRuntimeContextMegaOwner.length > 0 || blockKartOwnerContextMethods.length > 0) {
    failures.push({
      code: 'blockkart.runtime_context_mega_owner',
      severity: 'P0',
      owner: 'BlockKart',
      message: 'BlockKartRuntimeContext or owner methods still preserve a hidden mega-owner mutation context',
      evidence: { blockKartRuntimeContextMegaOwner, blockKartOwnerContextMethods: blockKartOwnerContextMethods.slice(0, 40) },
    });
  }
  return failures;
}

function issue(code, severity, owner, expected, gate, expiry, mitigation, status, evidence = {}) {
  const item = {
    code,
    severity,
    owner,
    evidence,
    expected,
    gate,
    expiry,
    mitigation,
    status: status ? 'closed' : 'open',
  };
  issues.push(item);
  if (!status && (severity === 'P0' || severity === 'P1')) {
    failures.push({
      code,
      severity,
      owner,
      expected,
      gate,
      evidence,
    });
  }
}

const issues = [];
const failures = [];

const ciSystem = readText(path.join(root, 'cmd/vo-dev/src/ci_system.rs'));
const taskGraph = readText(path.join(root, 'cmd/vo-dev/src/task_graph.rs'));
const taskRunner = readText(path.join(root, 'cmd/vo-dev/src/task_runner.rs'));
const projectToml = readText(path.join(root, 'eng/project.toml'));
const ciToml = readText(path.join(root, 'eng/ci.toml'));
const tasksToml = readText(path.join(root, 'eng/tasks.toml'));
const toolchainsToml = readText(path.join(root, 'eng/toolchains.toml'));
const artifactsToml = readText(path.join(root, 'eng/artifacts.toml'));
const quickplayProvenance = jsonFile('apps/studio/public/quickplay/blockkart/provenance.json');
const frameOrchestrator = projectText(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs');
const rendererRoot = projectText(voplayRoot, 'rust/src/renderer.rs');
const rendererRuntimeRoot = projectText(voplayRoot, 'rust/src/renderer_runtime.rs');
const pipeline3dRoot = projectText(voplayRoot, 'rust/src/pipeline3d.rs');
const frameOrchestratorAudit = projectTexts(voplayRoot, [
  'rust/src/renderer/frame_orchestrator.rs',
  'rust/src/renderer/frame_orchestrator_runtime.rs',
  'rust/src/renderer/frame_2d_upload.rs',
  'rust/src/renderer/frame_graph_plan.rs',
  'rust/src/renderer/frame_pass_sequence.rs',
  'rust/src/renderer/frame_perf_finalize.rs',
  'rust/src/renderer/frame_surface.rs',
  'rust/src/renderer/frame_workload_plan.rs',
]);
const framePassSequence = projectText(voplayRoot, 'rust/src/renderer/frame_pass_sequence.rs');
const rendererPassDispatch = projectText(voplayRoot, 'rust/src/renderer/pass_dispatch.rs');
const renderPassSources = projectTexts(voplayRoot, [
  'rust/src/renderer/depth_pass.rs',
  'rust/src/renderer/shadow_pass.rs',
  'rust/src/renderer/main_opaque_pass.rs',
  'rust/src/renderer/main_transparent_pass.rs',
  'rust/src/renderer/water_pass.rs',
  'rust/src/renderer/post_pass.rs',
  'rust/src/renderer/overlay_pass.rs',
  'rust/src/renderer/backend_submit_pass.rs',
]);
const transparentPass = projectText(voplayRoot, 'rust/src/renderer/main_transparent_pass.rs');
const frameGraph = projectText(voplayRoot, 'rust/src/renderer_frame.rs');
const frameGraphResourceRegistry = projectText(voplayRoot, 'rust/src/renderer_frame/resource_registry.rs');
const frameGraphBackingSource = projectTexts(voplayRoot, [
  'rust/src/renderer_frame/resource_backing.rs',
  'rust/src/renderer_frame/resource_registry/target_store.rs',
  'rust/src/renderer_frame/resource_registry/view_access.rs',
]);
const frameGraphAuditSource = `${frameGraph}\n${frameGraphResourceRegistry}\n${frameGraphBackingSource}`;
const renderWorld = projectTexts(voplayRoot, [
  'rust/src/render_world.rs',
  'rust/src/render_world/store.rs',
  'rust/src/render_world/tests.rs',
]);
const primitivePipeline = projectTexts(voplayRoot, [
  'rust/src/primitive_pipeline.rs',
  'rust/src/primitive_pipeline/runtime.rs',
  'rust/src/primitive_pipeline/tests.rs',
]);
const rendererHotPathEntries = [
  { path: 'voplay/rust/src/renderer.rs', source: productionSource(rendererRoot) },
  { path: 'voplay/rust/src/renderer_runtime.rs', source: productionSource(rendererRuntimeRoot) },
  { path: 'voplay/rust/src/pipeline3d.rs', source: productionSource(pipeline3dRoot) },
  { path: 'voplay/rust/src/primitive_pipeline.rs', source: productionSource(projectText(voplayRoot, 'rust/src/primitive_pipeline.rs')) },
  { path: 'voplay/rust/src/primitive_pipeline/runtime.rs', source: projectText(voplayRoot, 'rust/src/primitive_pipeline/runtime.rs') },
  { path: 'voplay/rust/src/renderer_frame.rs', source: productionSource(frameGraph) },
  { path: 'voplay/rust/src/renderer_frame/resource_registry.rs', source: productionSource(frameGraphResourceRegistry) },
  { path: 'voplay/rust/src/render_world.rs', source: productionSource(projectText(voplayRoot, 'rust/src/render_world.rs')) },
  { path: 'voplay/rust/src/renderer/pass_dispatch.rs', source: rendererPassDispatch },
  ...[
    'depth_pass.rs',
    'shadow_pass.rs',
    'main_opaque_pass.rs',
    'main_transparent_pass.rs',
    'water_pass.rs',
    'post_pass.rs',
    'overlay_pass.rs',
    'backend_submit_pass.rs',
  ].map((file) => ({ path: `voplay/rust/src/renderer/${file}`, source: productionSource(projectText(voplayRoot, `rust/src/renderer/${file}`)) })),
];
const pipelineCache = projectTexts(voplayRoot, [
  'rust/src/pipeline3d/pipeline_cache.rs',
  'rust/src/pipeline3d/pipeline_factory.rs',
  'rust/src/pipeline3d/mesh_submitter.rs',
]);
const primitivePipelineBudgetSource = projectText(voplayRoot, 'rust/src/primitive_pipeline.rs');
const pipelineCacheBudgetSource = projectText(voplayRoot, 'rust/src/pipeline3d/pipeline_cache.rs');
const voplayPhysicsBackend = projectText(voplayRoot, 'scene3d/physics_backend.vo');
const voplayPhysicsCommands = projectText(voplayRoot, 'scene3d/physics_commands.vo');
const voplayPhysicsTypes = projectText(voplayRoot, 'scene3d/physics_types.vo');
const voplayPhysicsWorld = projectText(voplayRoot, 'scene3d/physics_world.vo');
const voplayPhysics = [voplayPhysicsBackend, voplayPhysicsCommands, voplayPhysicsTypes, voplayPhysicsWorld].join('\n');
const voplayVehicle = projectText(voplayRoot, 'scene3d/vehicle.vo');
const voplayDynamics = projectText(voplayRoot, 'scene3d/kart_dynamics.vo');
const kartController = projectText(voplayRoot, 'scene3d/kart_controller.vo');
const vehicleTelemetry = projectText(voplayRoot, 'scene3d/vehicle_telemetry.vo');
const vehiclePhysicsSession = projectText(voplayRoot, 'scene3d/vehicle_physics_session.vo');
const physicsStressSource = projectText(voplayRoot, 'examples/physics_stress/main.vo');
const voplayToolSources = projectTexts(voplayRoot, [
  'tools/vehicle_telemetry_parity.vo',
  'tools/racing_reference_demos.vo',
]);
const sceneSource = projectText(voplayRoot, 'scene3d/scene.vo');
const replaySource = projectText(voplayRoot, 'scene3d/replay.vo');
const blockKartWorld = projectText(blockKartRoot, 'world.vo');
const blockKartPrimitiveWorld = projectText(blockKartRoot, 'primitive_world.vo');
const blockKartFiles = listFiles(blockKartRoot, '.vo');

const workflowSources = [
  '.github/workflows/module-system-enforcement.yml',
  '.github/workflows/production-readiness.yml',
  '.github/workflows/deploy-site.yml',
].map((file) => readText(path.join(root, file))).join('\n');

const hasMultiCheckout =
  ciSystem.includes('checkouts: Vec<CiCheckout>')
  && ciSystem.includes('checkout_blockkart')
  && workflowSources.includes('Checkout BlockKart')
  && workflowSources.includes('checkout_voplay')
  && projectToml.includes('name = "BlockKart"')
  && projectToml.includes('repository = "vo-lang/BlockKart"')
  && /name = "BlockKart"[\s\S]*?ci_checkout = true/.test(projectToml);

issue(
  'Q-P0-CI-MULTI-CHECKOUT',
  'P0',
  'eng',
  'PR matrix and site metadata support multi-repo checkout for vogui, voplay, vopack, and BlockKart.',
  'cargo run -q -p vo-dev -- ci metadata site; cargo run -q -p vo-dev -- ci final-matrix',
  '2026-07-05',
  'Centralize checkout list in vo-dev matrix/metadata and consume named checkout fields in workflows.',
  hasMultiCheckout,
  { matrixFields: ['checkouts', 'checkout_voplay', 'checkout_vopack', 'checkout_blockkart'] },
);

const rootInjection =
  taskRunner.includes('"VOLANG_ROOT"')
  && taskRunner.includes('task_root_env')
  && taskRunner.includes('repo_root_env_name')
  && taskRunner.includes('project_repo_path')
  && taskGraph.includes('module-cache:')
  && taskGraph.includes('external:')
  && ![
    'scripts/ci/voplay_sample_validate.mjs',
    'scripts/ci/voplay_scene3d_unit.mjs',
    'scripts/ci/voplay_physics_stress.mjs',
    'scripts/ci/quickplay_source_audit.mjs',
    'scripts/ci/quickplay_validate.mjs',
    'scripts/ci/voplay_render_architecture_lint.mjs',
    'scripts/ci/blockkart_engine_boundary_lint.mjs',
    'scripts/ci/voplay_rust_unit.mjs',
    'scripts/ci/voplay_industrial_readiness.mjs',
    'apps/studio/scripts/package_blockkart_quickplay.mjs',
  ].some((relative) => /\.\.\/(voplay|BlockKart|vogui|vopack)|ci_modules\/(voplay|BlockKart|vogui|vopack)/.test(readText(path.join(root, relative))));

issue(
  'Q-P0-ROOT-INJECTION',
  'P0',
  'eng',
  'vo-dev injects all first-party and external repo roots and scripts reject implicit sibling fallbacks.',
  './d.py ci task quickplay-source-audit',
  '2026-07-05',
  'Use task graph repo inference plus repo_roots.mjs validation.',
  rootInjection,
  { env: ['VOLANG_ROOT', 'VOPLAY_ROOT', 'VOGUI_ROOT', 'VOPACK_ROOT', 'BLOCKKART_ROOT'] },
);

const routingReady =
  ciToml.includes('voplay-industrial-readiness')
  && ciToml.includes('voplay-render-soak-10m')
  && ciToml.includes('voplay-physics-industrial-stress')
  && ciToml.includes('apps/studio/scripts/package_blockkart_quickplay.mjs')
  && ciToml.includes('eng/perf-budgets/**')
  && ciToml.includes('eng/project.toml');
issue(
  'Q-P1-CHANGED-FILES-ROUTING',
  'P1',
  'eng',
  'Changed scripts/configs route to site, app-site, and voplay industrial gates.',
  'cargo run -q -p vo-dev -- task plan pr --changed --explain',
  '2026-07-05',
  'Keep concrete non-internal task routes in eng/ci.toml for app-site and industrial surfaces.',
  routingReady,
  { checkedPrefixes: ['scripts/ci/**', 'eng/project.toml', 'eng/perf-budgets/**'] },
);

const auditReady =
  taskNeeds(tasksToml, 'voplay-engineering-quality-readiness').includes('node-audit-current')
  && currentNodeAuditWorkspaces(toolchainsToml).length > 0
  && currentNodeAuditWorkspaces(toolchainsToml).every((workspace) => workspace.auditLevel === 'high');
issue(
  'Q-P1-NPM-AUDIT-QUALITY',
  'P1',
  'eng',
  'Quality readiness runs node-audit-current and active npm workspaces fail on high/critical findings.',
  './d.py ci task node-audit-current',
  '2026-07-05',
  'Quality gate depends on node-audit-current; moderate exceptions must be declared in the quality report.',
  auditReady,
  {
    engineeringNeeds: taskNeeds(tasksToml, 'voplay-engineering-quality-readiness'),
    currentNodeAuditWorkspaces: currentNodeAuditWorkspaces(toolchainsToml),
  },
);

const frameSurface = projectText(voplayRoot, 'rust/src/renderer/frame_surface.rs');
const orchestratorDecodeIndex = frameOrchestrator.indexOf('decode_frame_commands');
const orchestratorWorkloadPlanIndex = frameOrchestrator.indexOf('prepare_frame_workload_plan');
const orchestratorAcquireSurfaceIndex = frameOrchestrator.indexOf('acquire_surface_texture');
const surfaceAcquireAfterDecode =
  orchestratorDecodeIndex >= 0
  && orchestratorWorkloadPlanIndex > orchestratorDecodeIndex
  && orchestratorAcquireSurfaceIndex > orchestratorWorkloadPlanIndex
  && /SurfaceError::(Lost|Outdated|Timeout|OutOfMemory)/.test(frameSurface);
issue(
  'Q-P1-RENDER-SURFACE-RECOVERY',
  'P1',
  'voplay/render',
  'Surface acquire happens after CPU decode/batch planning and has classified recovery for lost/outdated/timeout/OOM.',
  './d.py ci task voplay-render-core-unit',
  '2026-07-06',
  'Move acquire into a late helper and unit-test error classification.',
  surfaceAcquireAfterDecode,
  { orchestratorDecodeIndex, orchestratorWorkloadPlanIndex, orchestratorAcquireSurfaceIndex, surfaceErrorClassified: /SurfaceError::(Lost|Outdated|Timeout|OutOfMemory)/.test(frameSurface) },
);

const transparentReady =
  transparentPass.includes('Translucent')
  && transparentPass.includes('depth_write_enabled: false')
  && transparentPass.includes('sort')
  && !/fn execute\(\)\s*->\s*Result<f64,\s*String>\s*\{\s*Ok\(0\.0\)\s*\}/.test(transparentPass);
issue(
  'Q-P1-RENDER-TRANSPARENT-PASS',
  'P1',
  'voplay/render',
  'MainTransparent pass executes a real translucent path with depth writes disabled and stable ordering.',
  './d.py ci task voplay-render-core-unit',
  '2026-07-06',
  'Route translucent primitive/model draws through a dedicated pass and expose workload telemetry.',
  transparentReady,
  { file: 'voplay/rust/src/renderer/main_transparent_pass.rs' },
);

const frameGraphHardFail =
  frameGraph.includes('missing required read')
  && frameGraph.includes('missing required write')
  && frameGraph.includes('validate_ready_reads')
  && frameGraph.includes('validate_required_writes');
issue(
  'Q-P1-FRAMEGRAPH-HARD-FAIL',
  'P1',
  'voplay/render',
  'FrameGraph fails before execution when required reads or writes are missing.',
  './d.py ci task voplay-render-core-unit',
  '2026-07-06',
  'Add pre-dispatch validation and regression tests for missing reads/writes.',
  frameGraphHardFail,
  { missingReadDiagnostics: frameGraph.includes('missing_read_count') },
);

const batchPlannerBounds =
  renderWorld.includes('model_bounds')
  && renderWorld.includes('ModelManager')
  && !bodyOfFunction(renderWorld, 'pub fn build').includes('.find(|input| input.draw_index == index)');
issue(
  'Q-P1-BATCH-PLANNER-BOUNDS',
  'P1',
  'voplay/render',
  'BatchPlanner uses model/terrain/decal/chunk bounds without hot-path linear lookups.',
  './d.py ci task voplay-batch-planner-unit',
  '2026-07-06',
  'Pre-index terrain/decal inputs and use model manager AABB in planning.',
  batchPlannerBounds,
  { file: 'voplay/rust/src/render_world.rs' },
);

const renderStressReport = jsonMaybe('target/voplay-render-stress-budgeted/report.json');
const renderStressScenes = Array.isArray(renderStressReport?.scenes) ? renderStressReport.scenes : [];
const primitiveChurnSourceReady =
  primitivePipeline.includes('dirty_start')
  && primitivePipeline.includes('dirty_count')
  && primitivePipeline.includes('rebuild_queue')
  && primitivePipeline.includes('staging_instances')
  && primitivePipeline.includes('dirty_upload_bytes')
  && primitivePipeline.includes('full_rebuild_count')
  && primitivePipeline.includes('upload_resident_dirty_range')
  && primitivePipeline.includes('rebuild_resident_chunk_full')
  && primitivePipeline.includes('dirty-range-partial-upload');
const primitiveChurnReportReady =
  renderStressReport?.status === 'pass'
  && renderStressReport?.coverage?.resourceChurnSoak === true
  && renderStressScenes.some((scene) => (
    scene?.name === 'blockkart-resource-churn-soak'
    && Number(scene?.workload?.primitiveUploadBytes ?? 0) > 0
    && Number.isFinite(Number(scene?.workload?.residentChunkRebuilds ?? 0))
    && Number(scene?.workload?.resourceChurnEvents ?? 0) > 0
  ));
const primitiveChurnReady = primitiveChurnSourceReady && primitiveChurnReportReady;
issue(
  'Q-P1-PRIMITIVE-CHURN-BUDGET',
  'P1',
  'voplay/render',
  'Primitive resident updates expose dirty range, staging/ring buffer, rebuild queue, and churn telemetry.',
  './d.py ci task voplay-render-stress-budgeted',
  '2026-07-06',
  'Avoid whole chunk rebuild on churn paths and budget upload bytes/rebuild queue length.',
  primitiveChurnReady,
  {
    file: 'voplay/rust/src/primitive_pipeline.rs',
    sourceReady: primitiveChurnSourceReady,
    reportReady: primitiveChurnReportReady,
    resourceChurnSoak: renderStressScenes.find((scene) => scene?.name === 'blockkart-resource-churn-soak')?.workload ?? null,
  },
);

const renderBudgetReady =
  lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs')) <= 300
  && lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_decode.rs')) <= 700
  && lineCount(projectText(voplayRoot, 'rust/src/renderer_frame.rs')) <= 900
  && lineCount(projectText(voplayRoot, 'rust/src/render_world.rs')) <= 700
  && lineCount(primitivePipelineBudgetSource) <= 700
  && lineCount(pipelineCacheBudgetSource) <= 500;
issue(
  'Q-P2-RENDER-OWNERSHIP-BUDGET',
  'P2',
  'voplay/render',
  'Render ownership files stay inside file budget with owner tests.',
  './d.py ci task voplay-render-architecture-lint',
  '2026-07-12',
  'Keep file-budget lint active; split resident store, batching, upload, cache, and submitters in follow-up slices.',
  renderBudgetReady,
  {
    frameOrchestratorLines: lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_orchestrator.rs')),
    primitivePipelineLines: lineCount(primitivePipelineBudgetSource),
    renderWorldLines: lineCount(projectText(voplayRoot, 'rust/src/render_world.rs')),
    pipelineCacheLines: lineCount(pipelineCacheBudgetSource),
  },
);

const physicsSource = voplayPhysics;
const vehicleSource = projectText(voplayRoot, 'scene3d/vehicle.vo');
const roadEdgeAssistSource = projectText(voplayRoot, 'scene3d/vehicle_road_edge_assist.vo');
const physicsAuthorityReady =
  physicsSource.includes('type PhysicsBackendTargetCommand struct')
  && bodyOfFunction(physicsSource, 'func (e *Entity) SetPosition').includes('ApplyPhysicsTarget(PhysicsBackendTargetCommand')
  && bodyOfFunction(physicsSource, 'func (e *Entity) SetVelocity').includes('ApplyPhysicsTarget(PhysicsBackendTargetCommand')
  && bodyOfFunction(physicsSource, 'func (e *Entity) SetAngularVelocity').includes('ApplyPhysicsTarget(PhysicsBackendTargetCommand')
  && bodyOfFunction(physicsSource, 'func (e *Entity) ApplyPhysicsTarget').includes('applyEntityPhysicsTarget')
  && !/^func \(v \*Vehicle\) ApplyForceCommand\(/m.test(vehicleSource)
  && vehicleSource.includes('func (v *Vehicle) applyForceCommand')
  && vehicleSource.includes('func applyVehicleBackendTarget')
  && vehicleSource.includes('func (v *Vehicle) applyBackendTarget')
  && roadEdgeAssistSource.includes('applyVehicleBackendTarget(vehicle, PhysicsBackendTargetCommand')
  && projectText(voplayRoot, 'scene3d/kart_dynamics.vo').includes('type BoostState struct')
  && projectText(voplayRoot, 'scene3d/contact_event.vo').includes('Surface SurfaceMaterial')
  && projectText(voplayRoot, 'scene3d/vehicle_telemetry.vo').includes('type WheelTelemetry struct')
  && projectText(voplayRoot, 'scene3d/vehicle_telemetry.vo').includes('SurfaceID MaterialID')
  && projectText(voplayRoot, 'scene3d/vehicle_telemetry.vo').includes('BackendApplyHash')
  && projectText(voplayRoot, 'scene3d/replay.vo').includes('BackendApplyHash')
  && projectText(voplayRoot, 'scene3d/vehicle_physics_session.vo').includes('FixedPhysicsStep')
  && projectText(voplayRoot, 'tests/main.vo').includes('scene3d vehicle telemetry records backend apply hash');
issue(
  'Q-P1-PHYSICS-AUTHORITY',
  'P1',
  'voplay/scene3d',
  'Road-edge constraints, boost state, contact surface, and per-wheel telemetry have a single physics authority path.',
  './d.py ci task voplay-scene3d-contract',
  '2026-07-07',
  'Move edge assist and boost authority into fixed-step dynamics and expose per-wheel contact telemetry.',
  physicsAuthorityReady,
  {
    files: ['scene3d/physics_backend.vo', 'scene3d/physics_commands.vo', 'scene3d/physics_types.vo', 'scene3d/physics_world.vo', 'scene3d/vehicle.vo', 'scene3d/vehicle_road_edge_assist.vo', 'scene3d/vehicle_telemetry.vo', 'scene3d/replay.vo'],
    targetCommandLine: lineOf(physicsSource, 'type PhysicsBackendTargetCommand struct'),
    privateForceCommandLine: lineOf(vehicleSource, 'func (v *Vehicle) applyForceCommand'),
    roadEdgeTargetLine: lineOf(roadEdgeAssistSource, 'applyVehicleBackendTarget(vehicle, PhysicsBackendTargetCommand'),
  },
);

const blockKartProductReady =
  ['RaceSession', 'KartRig', 'TrackRuntime', 'HUDPresenter', 'PerfReporter', 'AssetRuntimeCache'].every((token) =>
    blockKartFiles.some((file) => file.source.includes(token)),
  )
  && lineCount(blockKartWorld) <= 850
  && lineCount(blockKartPrimitiveWorld) <= 850
  && blockKartFiles.some((file) => file.source.includes('StructuredFailureReport'));
issue(
  'Q-P1-BLOCKKART-PRODUCT-OWNERS',
  'P1',
  'BlockKart',
  'BlockKart World and primitive_world are split into product owners with structured failure reports.',
  './d.py ci task blockkart-product-boundary-strict',
  '2026-07-07',
  'Extract race/session/rig/track/HUD/perf/assets owners and keep product panic paths structured.',
  blockKartProductReady,
  { worldLines: lineCount(blockKartWorld), primitiveWorldLines: lineCount(blockKartPrimitiveWorld) },
);

const trackManifestReady =
  blockKartFiles.some((file) => file.source.includes('BlockKartTrackSpec'))
  || existsSync(path.join(blockKartRoot, 'track_manifest.json'))
  || existsSync(path.join(blockKartRoot, 'assets/maps/primitive_track/blockkart.track.json'));
issue(
  'Q-P1-BLOCKKART-TRACK-MANIFEST',
  'P1',
  'BlockKart',
  'Track, visual road, terrain, collider, spawn, gates, respawn, and minimap derive from one manifest.',
  './d.py ci task quickplay-source-audit',
  '2026-07-07',
  'Introduce BlockKartTrackSpec manifest and make terrain/collider/capture tools consume its hash.',
  trackManifestReady,
  { candidate: 'assets/maps/primitive_track/blockkart.track.json' },
);

const provenanceV2Ready =
  artifactsToml.includes('provenance = "apps/studio/public/quickplay/blockkart/provenance.json"')
  && quickplayProvenance.schemaVersion === 2
  && Array.isArray(quickplayProvenance.outputs)
  && quickplayProvenance.outputs.every((output) => output.digest)
  && quickplayProvenance.generator?.version
  && sha256Field(quickplayProvenance.toolchain?.voDevSourceDigest)
  && quickplayProvenance.task?.id
  && quickplayProvenance.sourceRoots;
issue(
  'Q-P1-ARTIFACT-PROVENANCE-V2',
  'P1',
  'studio/artifacts',
  'Generated artifacts use provenance schema v2 with output digest, dependency commit, dirty flag, generator/toolchain version, task id, and source roots.',
  './d.py ci task quickplay-validate',
  '2026-07-06',
  'Upgrade quickplay provenance writer, validator, and vo-dev artifact lint to schema v2.',
  provenanceV2Ready,
  { schemaVersion: quickplayProvenance.schemaVersion },
);

const vpakProducer = (quickplayProvenance.producers ?? [])
  .find((entry) => entry?.output === 'assets/blockkart.vpak');
const producerPaths = (entries) => new Set((entries ?? []).map((entry) => entry.path));
const terrainProducer = (vpakProducer?.upstream ?? [])
  .find((entry) => entry?.id === 'primitive-terrain-assets');
const paintProducer = (vpakProducer?.upstream ?? [])
  .find((entry) => entry?.id === 'painted-terrain-textures');
const vpakProducerInputPaths = producerPaths(vpakProducer?.inputs);
const vpakProducerOutputPaths = producerPaths(vpakProducer?.outputs);
const terrainProducerInputPaths = producerPaths(terrainProducer?.inputs);
const terrainProducerOutputPaths = producerPaths(terrainProducer?.outputs);
const paintProducerInputPaths = producerPaths(paintProducer?.inputs);
const paintProducerOutputPaths = producerPaths(paintProducer?.outputs);
const quickplayProducerProvenanceReady =
  vpakProducer?.owner === 'BlockKart'
  && vpakProducer?.kind === 'vpak'
  && Array.isArray(vpakProducer.command)
  && vpakProducer.command.includes('tools/pack_primitive_assets.vo')
  && vpakProducerInputPaths.has('tools/pack_primitive_assets.vo')
  && vpakProducerInputPaths.has('assets/maps/primitive_track/blockkart.map.json')
  && vpakProducerOutputPaths.has('assets/blockkart.vpak')
  && terrainProducerInputPaths.has('tools/generate_primitive_terrain.mjs')
  && terrainProducerInputPaths.has('tools/terrain_heightfield_spec.mjs')
  && terrainProducerInputPaths.has('terrain/recipes/primitive_concept_v1.json')
  && terrainProducerOutputPaths.has('assets/maps/primitive_track/lowpoly_terrain.glb')
  && terrainProducerOutputPaths.has('assets/maps/primitive_track/lowpoly_terrain_lod.glb')
  && terrainProducerOutputPaths.has('assets/maps/primitive_track/lowpoly_terrain_height_grid.bin')
  && terrainProducerOutputPaths.has('assets/maps/primitive_track/terrain_splat_large.png')
  && paintProducerInputPaths.has('tools/paint_terrain_textures.mjs')
  && paintProducerInputPaths.has('docs/images/terrain-upgrade-concept-v1.png')
  && paintProducerOutputPaths.has('assets/effects/grass_card_atlas.png');
issue(
  'Q-P0-QUICKPLAY-PRODUCER-PROVENANCE',
  'P0',
  'studio/artifacts',
  'assets/blockkart.vpak records first-party vpak, terrain, and painted-texture producer lineage with source/output digests.',
  './d.py ci task quickplay-source-audit',
  '2026-07-07',
  'Regenerate quickplay provenance with producer records for tools/pack_primitive_assets.vo, tools/generate_primitive_terrain.mjs, and tools/paint_terrain_textures.mjs.',
  quickplayProducerProvenanceReady,
  {
    path: 'apps/studio/public/quickplay/blockkart/provenance.json',
    line: lineOf(JSON.stringify(quickplayProvenance, null, 2), '"producers"') ?? 1,
    producer: vpakProducer ?? null,
  },
);

const devDocFiles = readdirSync(path.join(root, 'lang/docs/dev'))
  .filter((file) => file.endsWith('.md'))
  .map((file) => ({ file, source: readText(path.join(root, 'lang/docs/dev', file)) }));
const voplayPlanFiles = devDocFiles.filter((file) =>
  file.file.startsWith('voplay-') && file.file.endsWith('-plan.md')
);
const activeVoplayQuality = voplayPlanFiles.filter((file) => /^Status:\s*active\s*$/m.test(file.source));
const supersededByQualityPlan = (file) =>
  /^Status:\s*superseded\s*$/m.test(file.source)
  && /^Superseded-By:\s*lang\/docs\/dev\/voplay-code-engineering-quality-plan\.md\s*$/m.test(file.source)
  && /^Superseded-Date:\s*\d{4}-\d{2}-\d{2}\s*$/m.test(file.source);
const docsReady =
  activeVoplayQuality.length === 1
  && activeVoplayQuality[0]?.file === 'voplay-code-engineering-quality-plan.md'
  && voplayPlanFiles
    .filter((file) => file.file !== 'voplay-code-engineering-quality-plan.md')
    .every(supersededByQualityPlan);
const activeQualityPlanSource = activeVoplayQuality[0]?.source ?? '';
const docsExpectedCommits = parseExpectedCommits(projectToml);
const planSnapshot = activePlanSnapshot(activeQualityPlanSource);
const acceptedVolangPlanCommits = acceptedVolangPlanSnapshotCommits(root);
const activePlanSnapshotFresh =
  acceptedVolangPlanCommits.includes(planSnapshot.volang)
  && planSnapshot.voplay === docsExpectedCommits.get('voplay')
  && planSnapshot.voplay === repoHead(voplayRoot)
  && planSnapshot.BlockKart === docsExpectedCommits.get('BlockKart')
  && planSnapshot.BlockKart === repoHead(blockKartRoot);
const requiredFinalGateCommands = [
  './d.py ci task voplay-engineering-quality-readiness',
  './d.py ci task voplay-industrial-readiness',
  './d.py ci task voplay-render-core-unit',
  './d.py ci task voplay-render-architecture-lint',
  './d.py ci task voplay-batch-planner-unit',
  './d.py ci task voplay-render-stress-budgeted',
  './d.py ci task voplay-render-soak-10m',
  './d.py ci task voplay-scene3d-contract',
  './d.py ci task voplay-physics-industrial-stress',
  './d.py ci task blockkart-product-boundary-strict',
  './d.py ci task quickplay-source-audit',
  './d.py ci task quickplay-regenerate-check',
  './d.py ci task quickplay-validate',
  './d.py ci task blockkart-baseline',
  './d.py ci task blockkart-baseline-restart-50',
  './d.py ci task docs-lint',
  './d.py ci task eng-lint-tasks',
  'git diff --check',
  'git diff --cached --check',
];
const planFinalGateCommands = [...activeQualityPlanSource.matchAll(/^\s*(\.\/d\.py ci task [a-z0-9-]+|git diff --(?:cached --)?check)\s*$/gm)]
  .map((match) => match[1]);
const missingFinalGateCommands = requiredFinalGateCommands
  .filter((command) => !planFinalGateCommands.includes(command));
const missingFinalGateTaskDefinitions = requiredFinalGateCommands
  .filter((command) => command.startsWith('./d.py ci task '))
  .map((command) => command.replace('./d.py ci task ', ''))
  .filter((taskName) => !new RegExp(`^name = "${escapeRegExp(taskName)}"$`, 'm').test(tasksToml));
const requiredFinalGateConditions = [
  '`fileBudgets` 全 pass。',
  '`codeOwnership.status == pass`。',
  '`stringOnlyChecks == []`。',
  '`emptyOwnerModules == []`。',
  '`sourceAuditFailures == []`。',
  '`failures == []`。',
  '`dirtyProvenance == false`。',
  'render stress / soak report fresh。',
  'physics stress / replay report fresh。',
  'quickplay artifact 与源码 commit 一致。',
];
const missingFinalGateConditions = requiredFinalGateConditions
  .filter((condition) => !activeQualityPlanSource.includes(condition));
const docsFinalGateContract = {
  activePlan: activeVoplayQuality[0]?.file ?? null,
  commandCount: planFinalGateCommands.length,
  missingFinalGateCommands,
  missingFinalGateTaskDefinitions,
  missingFinalGateConditions,
  planSnapshot,
};
issue(
  'Q-P1-DOCS-PLAN-STATE',
  'P1',
  'docs',
  'Exactly one active voplay quality plan exists; old voplay plans are superseded with metadata.',
  './d.py ci task docs-lint',
  '2026-07-05',
  'Mark old active industrial plan superseded by the quality plan and lint active/proposed conflicts.',
  docsReady,
  { activePlans: activeVoplayQuality.map((file) => file.file) },
);
issue(
  'Q-P1-DOCS-ACTIVE-PLAN-SNAPSHOT-FRESH',
  'P1',
  'docs',
  'The active voplay quality plan checkout snapshot matches current volang/voplay/BlockKart source commits.',
  './d.py ci task docs-lint',
  '2026-07-07',
  'Update the active plan snapshot whenever any pinned source checkout changes.',
  activePlanSnapshotFresh,
  {
    path: 'lang/docs/dev/voplay-code-engineering-quality-plan.md',
    line: lineOf(activeQualityPlanSource, '当前 checkout snapshot') ?? 1,
    planSnapshot,
    expected: {
      volang: repoHead(root),
      acceptedVolangPlanCommits,
      voplay: docsExpectedCommits.get('voplay'),
      BlockKart: docsExpectedCommits.get('BlockKart'),
    },
  },
);

const unexplainedWarningsReady =
  !projectText(voplayRoot, 'rust/src/lib.rs').includes('#[allow(dead_code)]')
  && !tasksToml.includes('allow warning');
issue(
  'Q-P1-RUST-WARNING-BUDGET',
  'P1',
  'voplay/rust',
  'Scoped Rust quality suites have no unexplained dead-code or unused warnings.',
  './d.py ci task voplay-render-core-unit',
  '2026-07-06',
  'Remove unused paths or make warning allowances explicit with owner and expiry.',
  unexplainedWarningsReady,
  { crate: 'vo-voplay' },
);

const p2Issues = issues.filter((item) => item.severity === 'P2');
const p2MetadataReady = p2Issues.every((item) => item.owner && item.expiry && item.mitigation);
const expectedCommits = parseExpectedCommits(projectToml);
const currentRepoStates = [
  repoState('voplay', voplayRoot, expectedCommits.get('voplay') || ''),
  repoState('BlockKart', blockKartRoot, expectedCommits.get('BlockKart') || ''),
];
const crossRepoHeadMismatchesList = crossRepoHeadMismatches(expectedCommits);
const dirtyProvenanceEntries = dirtyProvenanceEvidence(quickplayProvenance);
const dirtyProvenance = dirtyProvenanceEntries.length > 0;
const emptyOwnerModulesList = emptyOwnerModules(voplayRoot);
const sourceAuditFailures = [
  ...emptyOwnerModulesList.map((entry) => ({
    code: 'render.empty_owner_module',
    severity: 'P0',
    owner: 'voplay/render',
    message: 'Render split owner module has no production responsibility or production call path',
    evidence: entry,
  })),
  ...sourceAuditFailuresFromSource({
    rendererHotPathEntries,
    frameGraph: frameGraphAuditSource,
    framePassSequence,
    rendererPassDispatch,
    renderPassSources,
    renderWorld,
    voplayPhysics,
    voplayPhysicsWorld,
    voplayVehicle,
    voplayDynamics,
    kartController,
    vehicleTelemetry,
    vehiclePhysicsSession,
    physicsStressSource,
    voplayToolSources,
    sceneSource,
    replaySource,
    blockKartFiles,
  }),
];
const physicsContractReport = jsonMaybe('target/voplay-scene3d-contract/report.json');
const blockKartBoundaryReport = jsonMaybe('target/blockkart-product-boundary-strict/report.json');
const renderStressStructuredReady = renderStressReport?.status === 'pass'
  && renderStressReport?.coverage?.primitive10k === true
  && renderStressReport?.coverage?.resourceChurnSoak === true
  && renderStressReport?.coverage?.realPerfSamples === true
  && renderStressReport?.summary?.p0 === 0
  && renderStressReport?.summary?.p1 === 0
  && renderStressScenes.every((scene) => (
    scene?.observability?.status === 'pass'
    && scene?.observability?.stage === 'complete'
    && Array.isArray(scene?.observability?.timeline)
    && scene.observability.timeline.length > 0
    && Number.isFinite(scene?.observability?.frameIndex)
    && typeof scene?.observability?.lastPass === 'string'
    && scene.observability.lastPass.length > 0
    && Number.isFinite(scene?.observability?.frameP90Ms)
    && Number.isFinite(scene?.observability?.frameP99Ms)
    && Number.isFinite(scene?.observability?.resourceChurn)
    && (scene?.observability?.telemetryStatus === 'running' || scene?.observability?.telemetryStatus === 'pass')
    && scene?.observability?.telemetryFailure == null
    && scene?.observability?.perfEndpointError == null
    && Number.isFinite(scene?.observability?.telemetryReportCount)
    && Number.isFinite(scene?.observability?.telemetryReportAgeMs)
    && Number.isFinite(scene?.observability?.telemetryObservedSpanMs)
    && Number.isFinite(scene?.observability?.telemetryFrameProgress)
    && scene?.observability?.lastTelemetryPacket
  ));
const physicsAuthorityStructuredReady = physicsContractReport?.status === 'pass'
  && !sourceAuditFailures.some((entry) => entry.code.startsWith('physics.'));
const blockKartProductStructuredReady = !sourceAuditFailures.some((entry) => entry.code.startsWith('blockkart.'))
  && blockKartBoundaryReport?.status === 'ok'
  && blockKartBoundaryReport?.issueCount === 0;
const renderFileBudgetEntries = {
  'rust/src/renderer/frame_orchestrator.rs': {
    lines: lineCount(frameOrchestrator),
    budget: 300,
  },
  'rust/src/renderer/frame_2d_upload.rs': {
    lines: lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_2d_upload.rs')),
    budget: 160,
  },
  'rust/src/renderer/frame_decode.rs': {
    lines: lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_decode.rs')),
    budget: 700,
  },
  'rust/src/renderer/frame_graph_plan.rs': {
    lines: lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_graph_plan.rs')),
    budget: 300,
  },
  'rust/src/renderer/frame_pass_sequence.rs': {
    lines: lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_pass_sequence.rs')),
    budget: 300,
  },
  'rust/src/renderer/frame_perf_finalize.rs': {
    lines: lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_perf_finalize.rs')),
    budget: 300,
  },
  'rust/src/renderer/frame_surface.rs': {
    lines: lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_surface.rs')),
    budget: 80,
  },
  'rust/src/renderer_frame.rs': {
    lines: lineCount(frameGraph),
    budget: 900,
  },
  'rust/src/renderer_frame/resource_registry.rs': {
    lines: lineCount(frameGraphResourceRegistry),
    budget: 360,
  },
  'rust/src/renderer/frame_workload_plan.rs': {
    lines: lineCount(projectText(voplayRoot, 'rust/src/renderer/frame_workload_plan.rs')),
    budget: 300,
  },
  'rust/src/render_world.rs': {
    lines: lineCount(projectText(voplayRoot, 'rust/src/render_world.rs')),
    budget: 700,
  },
  'rust/src/primitive_pipeline.rs': {
    lines: lineCount(primitivePipelineBudgetSource),
    budget: 700,
  },
  'rust/src/pipeline3d/pipeline_cache.rs': {
    lines: lineCount(pipelineCacheBudgetSource),
    budget: 500,
  },
};
const fileBudgets = Object.fromEntries(
  Object.entries(renderFileBudgetEntries).map(([file, entry]) => [
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
const codeOwnershipPass = renderBudgetReady && blockKartProductReady && emptyOwnerModulesList.length === 0;
const docsSourceFacts = {
  docsReady,
  activePlanSnapshotFresh,
  finalGateContractPass: missingFinalGateCommands.length === 0
    && missingFinalGateTaskDefinitions.length === 0
    && missingFinalGateConditions.length === 0,
  fileBudgetsPass: fileBudgetFailures.length === 0,
  codeOwnershipPass,
  emptyOwnerModulesEmpty: emptyOwnerModulesList.length === 0,
  sourceAuditFailuresEmpty: sourceAuditFailures.length === 0,
  dirtyProvenanceClean: !dirtyProvenance,
  crossRepoHeadsMatch: crossRepoHeadMismatchesList.length === 0,
};
const docsSourceFactsReady = Object.values(docsSourceFacts).every(Boolean);
const stringOnlyChecks = [
  !renderStressStructuredReady ? {
    code: 'Q-P1-PRIMITIVE-CHURN-BUDGET',
    owner: 'voplay/render',
    reason: 'uses substring evidence for dirty range, staging, rebuild queue, and resident churn',
    gate: './d.py ci task voplay-render-stress-budgeted',
  } : null,
  !physicsAuthorityStructuredReady ? {
    code: 'Q-P1-PHYSICS-AUTHORITY',
    owner: 'voplay/scene3d',
    reason: 'uses token presence instead of backend consumption behavior',
    gate: './d.py ci task voplay-scene3d-contract',
  } : null,
  !blockKartProductStructuredReady ? {
    code: 'Q-P1-BLOCKKART-PRODUCT-OWNERS',
    owner: 'BlockKart',
    reason: 'uses owner name tokens and line counts without production call ownership',
    gate: './d.py ci task blockkart-product-boundary-strict',
  } : null,
  !docsSourceFactsReady ? {
    code: 'Q-P1-DOCS-PLAN-STATE',
    owner: 'docs',
    reason: 'active plan final-gate contract must match defined tasks and current source-audit facts',
    gate: './d.py ci task docs-lint',
    evidence: {
      docsFinalGateContract,
      docsSourceFacts,
    },
  } : null,
].filter(Boolean);
const today = new Date().toISOString().slice(0, 10);
const openP0 = issues.filter((item) => item.severity === 'P0' && item.status !== 'closed');
const openP1 = issues.filter((item) => item.severity === 'P1' && item.status !== 'closed');
const openP2Expired = p2Issues.filter((item) => item.status !== 'closed' && item.expiry && item.expiry < today);
const hardFailures = [
  ...fileBudgetFailures.map((entry) => ({
    code: 'Q-P0-FILE-BUDGET',
    severity: 'P0',
    owner: 'voplay/render',
    expected: `${entry.file} must stay within its file budget (${entry.lines}/${entry.budget}, over by ${entry.overBy}).`,
    gate: './d.py ci task voplay-render-architecture-lint',
    evidence: entry,
  })),
  ...(!codeOwnershipPass ? [{
    code: 'Q-P0-CODE-OWNERSHIP',
    severity: 'P0',
    owner: 'voplay/render',
    expected: 'codeOwnership.status must be pass; owner modules must have real production responsibility and budget compliance.',
    gate: './d.py ci task voplay-render-architecture-lint',
    evidence: {
      renderBudgetReady,
      blockKartProductReady,
      emptyOwnerModules: emptyOwnerModulesList,
      p2Issues: p2Issues.map((item) => item.code),
    },
  }] : []),
  ...(dirtyProvenance ? [{
    code: 'Q-P0-DIRTY-PROVENANCE',
    severity: 'P0',
    owner: 'studio/artifacts',
    expected: 'Strict quality readiness requires quickplay project and dependency dirty flags to be false.',
    gate: './d.py ci task quickplay-validate',
    evidence: { dirtyProvenanceEntries },
  }] : []),
  ...crossRepoHeadMismatchesList.map((entry) => ({
    code: 'Q-P0-CROSS-REPO-HEAD',
    severity: 'P0',
    owner: 'eng',
    expected: 'Sibling checkout HEAD must match eng/project.toml expected_commit.',
    gate: 'cargo run -q -p vo-dev -- ci final-matrix',
    evidence: entry,
  })),
  ...sourceAuditFailures.map((entry) => ({
    code: entry.code,
    severity: entry.severity,
    owner: entry.owner,
    expected: entry.message,
    gate: 'first-principles source audit',
    evidence: entry.evidence,
  })),
  ...stringOnlyChecks.map((entry) => ({
    code: 'Q-P1-STRING-ONLY-CHECK',
    severity: 'P1',
    owner: entry.owner,
    expected: 'P0/P1 closure must be backed by structured report, behavior test, stress evidence, or source audit rather than string-only checks.',
    gate: entry.gate,
    evidence: entry,
  })),
  ...openP2Expired.map((entry) => ({
    code: 'Q-P2-EXPIRED',
    severity: 'P2',
    owner: entry.owner,
    expected: entry.expected,
    gate: entry.gate,
    evidence: { expiry: entry.expiry, mitigation: entry.mitigation },
  })),
];
for (const failure of hardFailures) {
  failures.push(failure);
}
const industrialReportPath = path.join(root, 'target/voplay-industrial-readiness/report.json');
const industrialReport = existsSync(industrialReportPath)
  ? JSON.parse(readText(industrialReportPath))
  : null;
const industrialFreshEvidenceIssues = verifySourceBoundEvidence({
  evidence: industrialReport?.freshEvidence,
  expectedGate: 'voplay-industrial-readiness',
  expectedCiRunId: currentCiRunId,
  root,
});
if (!currentCiRunId) {
  failures.push({
    code: 'Q-P0-CI-RUN-ID',
    severity: 'P0',
    owner: 'eng',
    expected: 'Engineering readiness must run through vo-dev with a shared CI run id.',
    gate: './d.py ci task voplay-engineering-quality-readiness',
    evidence: { currentCiRunId },
  });
}
if (industrialFreshEvidenceIssues.length > 0 || industrialReport?.freshEvidence?.verdict?.status !== 'pass') {
  failures.push({
    code: 'Q-P0-INDUSTRIAL-FRESH-EVIDENCE',
    severity: 'P0',
    owner: 'eng',
    expected: 'Consumed industrial readiness must have recomputed current-source evidence from the same CI run.',
    gate: './d.py ci task voplay-industrial-readiness',
    evidence: {
      freshnessIssues: industrialFreshEvidenceIssues,
      verdict: industrialReport?.freshEvidence?.verdict ?? null,
    },
  });
}
const industrialReadyDependency =
  industrialReport?.industrialReady === true
  && Array.isArray(industrialReport?.failures)
  && industrialReport.failures.length === 0
  && Array.isArray(industrialReport?.sourceAuditFailures)
  && industrialReport.sourceAuditFailures.length === 0
  && industrialFreshEvidenceIssues.length === 0
  && industrialReport?.freshEvidence?.verdict?.status === 'pass'
  && industrialReport?.reportValidity?.status === 'pass'
  && industrialReport?.architectureEleganceReady === true
  && industrialReport?.sourceFirstPrinciplesReview === 'pass'
  && industrialReport?.freshSourceBoundReports === true
  && industrialReport?.artifactSourceAgreement === true
  && !dirtyProvenance;
const generatedAt = new Date().toISOString();
const freshEvidence = sourceBoundEvidence({
  gate: 'voplay-engineering-quality-readiness',
  generatedAt,
  root,
  repos: [
    { name: 'volang', root },
    { name: 'voplay', root: voplayRoot },
    { name: 'BlockKart', root: blockKartRoot },
  ],
  gateFiles: [
    'scripts/ci/voplay_engineering_quality_readiness.mjs',
    'scripts/ci/active_plan_snapshot.mjs',
    'scripts/ci/repo_roots.mjs',
    'scripts/ci/source_bound_evidence.mjs',
    'scripts/ci/quickplay_source_audit.mjs',
    'scripts/ci/voplay_render_architecture_lint.mjs',
    'scripts/ci/blockkart_product_boundary_strict.mjs',
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
const selfFreshEvidenceIssues = verifySourceBoundEvidence({
  evidence: freshEvidence,
  expectedGate: 'voplay-engineering-quality-readiness',
  expectedCiRunId: currentCiRunId,
  root,
});
if (selfFreshEvidenceIssues.length > 0 || freshEvidence.verdict.status !== 'pass') {
  failures.push({
    code: 'Q-P0-SELF-FRESH-EVIDENCE',
    severity: 'P0',
    owner: 'eng',
    expected: 'Engineering readiness final verdict must include its own fresh current-source evidence.',
    gate: './d.py ci task voplay-engineering-quality-readiness',
    evidence: { freshnessIssues: selfFreshEvidenceIssues, verdict: freshEvidence.verdict },
  });
}
const architectureEleganceReady = sourceAuditFailures.length === 0
  && stringOnlyChecks.length === 0
  && fileBudgetFailures.length === 0
  && emptyOwnerModulesList.length === 0
  && codeOwnershipPass
  && renderStressStructuredReady
  && physicsAuthorityStructuredReady
  && blockKartProductStructuredReady;
const sourceFirstPrinciplesReview = industrialReport?.firstPrinciplesVerdict?.status === 'pass'
  && sourceAuditFailures.length === 0
  ? 'pass'
  : 'fail';
const freshSourceBoundReports = selfFreshEvidenceIssues.length === 0
  && freshEvidence.verdict.status === 'pass'
  && industrialFreshEvidenceIssues.length === 0
  && industrialReport?.freshSourceBoundReports === true;
const artifactSourceAgreement = !dirtyProvenance
  && crossRepoHeadMismatchesList.length === 0
  && industrialReport?.artifactSourceAgreement === true;
const qualityReady =
  failures.length === 0
  && sourceAuditFailures.length === 0
  && !dirtyProvenance
  && crossRepoHeadMismatchesList.length === 0
  && stringOnlyChecks.length === 0
  && fileBudgetFailures.length === 0
  && openP0.length === 0
  && openP1.length === 0
  && openP2Expired.length === 0
  && p2MetadataReady
  && codeOwnershipPass
  && architectureEleganceReady
  && sourceFirstPrinciplesReview === 'pass'
  && freshSourceBoundReports
  && artifactSourceAgreement
  && industrialReadyDependency
  && freshEvidence.verdict.status === 'pass';

const report = {
  schema: 'voplay.engineeringQualityReadiness.v1',
  generatedAt,
  freshEvidence,
  qualityReady,
  architectureEleganceReady,
  sourceFirstPrinciplesReview,
  freshSourceBoundReports,
  artifactSourceAgreement,
  reportValidity: {
    status: freshSourceBoundReports && industrialReadyDependency ? 'pass' : 'fail',
    ciRunId: currentCiRunId,
    selfFreshnessIssues: selfFreshEvidenceIssues,
    industrialFreshnessIssues: industrialFreshEvidenceIssues,
  },
  architectureEvidence: {
    status: architectureEleganceReady ? 'pass' : 'fail',
    fileBudgetsPass: fileBudgetFailures.length === 0,
    codeOwnershipPass,
    semanticSourceAuditPass: sourceAuditFailures.length === 0,
    stringOnlyChecks,
    renderStressStructuredReady,
    physicsAuthorityStructuredReady,
    blockKartProductStructuredReady,
  },
  sourceReviewEvidence: {
    status: sourceFirstPrinciplesReview,
    sourceAuditFailures,
    industrialFirstPrinciplesVerdict: industrialReport?.firstPrinciplesVerdict ?? null,
  },
  industrialReadyDependency,
  dirtyProvenance,
  crossRepoHeadMismatches: crossRepoHeadMismatchesList,
  stringOnlyChecks,
  emptyOwnerModules: emptyOwnerModulesList,
  openP0,
  openP1,
  openP2Expired,
  sourceAuditFailures,
  failures,
  repoState: currentRepoStates,
  warningBudget: {
    status: unexplainedWarningsReady ? 'pass' : 'fail',
    unexplainedWarningsAllowed: 0,
  },
  fileBudgets,
  dependencyAudit: {
    status: auditReady ? 'pass' : 'fail',
    gate: 'node-audit-current',
    highCriticalAllowed: 0,
    moderateIssues: [],
  },
  ciTopology: {
    status: hasMultiCheckout && rootInjection && routingReady ? 'pass' : 'fail',
    requiredRepos: ['volang', 'vogui', 'voplay', 'vopack', 'BlockKart'],
  },
  artifactProvenance: {
    status: provenanceV2Ready && !dirtyProvenance ? 'pass' : 'fail',
    schemaVersion: quickplayProvenance.schemaVersion,
    dirtyEntries: dirtyProvenanceEntries,
  },
  artifactEvidence: {
    quickplay: {
      provenance: 'apps/studio/public/quickplay/blockkart/provenance.json',
      dirtyProvenance,
      dirtyEntries: dirtyProvenanceEntries,
      outputs: quickplayProvenance.outputs ?? [],
    },
  },
  codeOwnership: {
    status: codeOwnershipPass ? 'pass' : 'fail',
    p2Issues: p2Issues.map((item) => item.code),
  },
  performanceDebt: {
    status: primitiveChurnReady && batchPlannerBounds ? 'pass' : 'fail',
  },
  docsPlanState: {
    status: docsReady && docsSourceFactsReady ? 'pass' : 'fail',
    activePlans: activeVoplayQuality.map((file) => file.file),
    sourceFacts: docsSourceFacts,
  },
  docsEvidence: {
    activePlans: activeVoplayQuality.map((file) => file.file),
    supersededPlans: voplayPlanFiles
      .filter((file) => file.file !== 'voplay-code-engineering-quality-plan.md')
      .map((file) => ({ file: file.file, superseded: supersededByQualityPlan(file) })),
    finalGateContract: docsFinalGateContract,
  },
  trackedIssues: issues,
};

function markdownReport(value) {
  const lines = [];
  lines.push('# voplay Engineering Quality Readiness');
  lines.push('');
  lines.push(`- qualityReady: ${value.qualityReady ? 'true' : 'false'}`);
  lines.push(`- failures: ${value.failures.length}`);
  lines.push(`- tracked issues: ${value.trackedIssues.length}`);
  lines.push('');
  for (const item of value.trackedIssues) {
    lines.push(`- ${item.status === 'closed' ? 'closed' : 'open'} ${item.severity} ${item.code}: ${item.expected}`);
  }
  lines.push('');
  return `${lines.join('\n')}\n`;
}

mkdirSync(outDir, { recursive: true });
writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify(report, null, 2)}\n`);
writeFileSync(path.join(outDir, 'report.md'), markdownReport(report));

console.log(`voplay engineering quality readiness: qualityReady=${qualityReady} failures=${failures.length}`);
console.log(`voplay engineering quality readiness: report ${path.relative(root, path.join(outDir, 'report.json'))}`);
if (!qualityReady) {
  for (const failure of failures) {
    console.error(`voplay engineering quality readiness: ${failure.severity} ${failure.code}: ${failure.expected}`);
  }
  process.exit(1);
}
