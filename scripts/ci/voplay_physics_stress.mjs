#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const outDir = path.resolve(argValue('--out-dir') || process.env.VOPLAY_PHYSICS_STRESS_OUT_DIR || path.join(root, 'target/voplay-physics-industrial-stress'));
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const voguiRoot = requireRepoRoot('VOGUI_ROOT', 'vogui');
const vopackRoot = requireRepoRoot('VOPACK_ROOT', 'vopack');
const scenarioSourceFile = path.join(voplayRoot, 'examples/physics_stress/main.vo');
const scenarioFile = path.relative(root, scenarioSourceFile);
const projectDir = path.join(outDir, 'project');
const projectEntry = path.join(projectDir, 'main.vo');
const marker = '__VOPLAY_PHYSICS_STRESS_JSON__';
const budgetPath = path.resolve(argValue('--budget') || process.env.VOPLAY_PHYSICS_STRESS_BUDGET || path.join(root, 'eng/perf-budgets/blockkart-voplay.medium.json'));
if (!existsSync(budgetPath)) fail(`budget file missing: ${budgetPath}`);
const perfBudget = JSON.parse(readFileSync(budgetPath, 'utf8'));
const physicsBudget = perfBudget?.physics ?? {};

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : process.argv[index + 1] ?? '';
}

function fail(message) {
  console.error(`voplay physics stress: ${message}`);
  process.exit(1);
}

function numberOr(...values) {
  for (const value of values) {
    const number = Number(value);
    if (Number.isFinite(number)) {
      return number;
    }
  }
  return Number.NaN;
}

function tail(text) {
  const value = String(text ?? '');
  return value.length > 6000 ? value.slice(value.length - 6000) : value;
}

function replayExecutionPlanFacts(source) {
  const coordinatorStart = source.indexOf('func replayContracts(');
  const contractHelperStart = source.indexOf('func replayTraceContractReport(');
  const freshVerifierStart = source.indexOf('func verifyReplaySuiteFile(');
  const nextFunctionStart = source.indexOf('func scenarioDefinitionByName(', freshVerifierStart);
  if (coordinatorStart < 0 || contractHelperStart <= coordinatorStart || freshVerifierStart < 0 || nextFunctionStart <= freshVerifierStart) {
    return {
      coordinatorUsesContractOnly: false,
      freshVerifierExecutesTrace: false,
      sourceShapeComplete: false,
    };
  }
  const coordinator = source.slice(coordinatorStart, contractHelperStart);
  const freshVerifier = source.slice(freshVerifierStart, nextFunctionStart);
  return {
    coordinatorUsesContractOnly: coordinator.includes('report := replayTraceContractReport(execution.Definition, execution.Trace)')
      && !coordinator.includes('report := verifyReplayTrace(execution.Definition, execution.Trace)'),
    freshVerifierExecutesTrace: freshVerifier.includes('report := verifyReplayTrace(def, entry.Trace)'),
    sourceShapeComplete: true,
  };
}

function requireReplayExecutionPlan() {
  const source = readFileSync(scenarioSourceFile, 'utf8');
  const facts = replayExecutionPlanFacts(source);
  const duplicateReplayFixture = source.replace(
    'report := replayTraceContractReport(execution.Definition, execution.Trace)',
    'report := verifyReplayTrace(execution.Definition, execution.Trace)',
  );
  const negativeDuplicateRejected = replayExecutionPlanFacts(duplicateReplayFixture).coordinatorUsesContractOnly === false;
  if (!facts.sourceShapeComplete || !facts.coordinatorUsesContractOnly || !facts.freshVerifierExecutesTrace || !negativeDuplicateRejected) {
    fail(`replay execution plan contract failed: ${JSON.stringify({ ...facts, negativeDuplicateRejected })}`);
  }
  return {
    schemaVersion: 1,
    kind: 'voplay.physicsReplayExecutionPlan',
    referenceSimulationPasses: 1,
    freshReplaySimulationPasses: 1,
    duplicateInProcessReplay: false,
    ...facts,
    negativeDuplicateRejected,
  };
}

function prepareScenarioProject() {
  const localModules = [
    { module: 'github.com/vo-lang/voplay', dir: voplayRoot },
    { module: 'github.com/vo-lang/vogui', dir: voguiRoot },
    { module: 'github.com/vo-lang/vopack', dir: vopackRoot },
  ];
  for (const local of localModules) {
    if (!existsSync(path.join(local.dir, 'vo.mod'))) {
      fail(`local workspace module ${local.module} is missing vo.mod at ${local.dir}`);
    }
  }
  if (!existsSync(scenarioSourceFile)) {
    fail(`scenario source is missing at ${scenarioSourceFile}`);
  }
  mkdirSync(projectDir, { recursive: true });
  writeFileSync(
    path.join(projectDir, 'vo.mod'),
    'module github.com/vo-lang/voplay-physics-stress\n\nvo ^0.1.0\n\nrequire github.com/vo-lang/voplay v0.1.31\n',
  );
  const workLines = ['version = 1', ''];
  for (const local of localModules) {
    workLines.push('[[use]]');
    workLines.push(`path = ${JSON.stringify(path.relative(projectDir, local.dir).replaceAll(path.sep, '/'))}`);
    workLines.push('');
  }
  writeFileSync(path.join(projectDir, 'vo.work'), workLines.join('\n'));
  writeFileSync(projectEntry, readFileSync(scenarioSourceFile, 'utf8'));
}

function runScenarioProgram() {
  prepareScenarioProject();
  const command = ['./d.py', 'run', path.relative(root, projectEntry), '--mode=jit', '--release'];
  const result = spawnSync(command[0], command.slice(1), {
    cwd: root,
    env: {
      ...process.env,
      VOPLAY_PHYSICS_STRESS_ROOT: root,
      VOPLAY_PHYSICS_STRESS_ENTRY: path.relative(root, projectEntry),
    },
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  writeFileSync(path.join(outDir, 'physics-stress-run.log'), `${result.stdout ?? ''}${result.stderr ?? ''}`);
  if (result.status !== 0) {
    fail(`scenario program failed with exit ${result.status}; see ${path.join(outDir, 'physics-stress-run.log')}\n${tail(result.stdout + result.stderr)}`);
  }
  const line = `${result.stdout ?? ''}\n${result.stderr ?? ''}`.split(/\r?\n/).find((entry) => entry.includes(marker));
  if (!line) {
    fail(`scenario program did not emit ${marker}; see ${path.join(outDir, 'physics-stress-run.log')}`);
  }
  const jsonText = line.slice(line.indexOf(marker) + marker.length);
  try {
    return JSON.parse(jsonText);
  } catch (error) {
    fail(`could not parse scenario JSON: ${error instanceof Error ? error.message : String(error)}`);
  }
}

function validateReport(report) {
  const issues = [];
  const maxVelocity = numberOr(physicsBudget.maxVelocity);
  const maxAngularVelocity = numberOr(physicsBudget.maxAngularVelocity);
  const maxReplayDriftMeters = numberOr(physicsBudget.maxReplayDriftMeters, 0.01);
  const maxLongStuckSeconds = numberOr(physicsBudget.maxLongStuckSeconds);
  if (
    report?.replayExecutionPlan?.referenceSimulationPasses !== 1
    || report?.replayExecutionPlan?.freshReplaySimulationPasses !== 1
    || report?.replayExecutionPlan?.duplicateInProcessReplay !== false
    || report?.replayExecutionPlan?.negativeDuplicateRejected !== true
  ) {
    issues.push({ code: 'physics.replay_execution_plan', severity: 0, detail: JSON.stringify(report?.replayExecutionPlan ?? {}) });
  }
  if (report?.kind !== 'voplay.physicsStressReport') {
    issues.push({ code: 'report.kind', severity: 0, detail: `unexpected kind ${report?.kind}` });
  }
  const required = [
    'skidpad',
    'slalom',
    'drift-turbo',
    'boost-pad',
    'offroad-transition',
    'jump-landing',
    'wall-impact',
    'rail-ride',
    'wall-ride',
    'water-skim',
    'surface-transition',
    'road-edge-assist',
    'sleep-wake',
    'pose-reset',
    'recovery',
    'multi-vehicle-scripted-soak',
  ];
  const byName = new Map((report.scenarios ?? []).map((scenario) => [scenario.name, scenario]));
  for (const name of required) {
    if (!byName.has(name)) {
      issues.push({ code: 'scenario.missing', severity: 0, detail: name });
    }
  }
  for (const scenario of report.scenarios ?? []) {
    for (const issue of scenario.issues ?? []) {
      issues.push({ ...issue, scenario: scenario.name });
    }
    if ((scenario.invalidSamples ?? 0) > 0) {
      issues.push({ code: 'physics.invalid_samples', severity: 0, detail: String(scenario.invalidSamples), scenario: scenario.name });
    }
    if (!Array.isArray(scenario.samples) || scenario.samples.length === 0) {
      issues.push({ code: 'telemetry.samples_missing', severity: 1, detail: 'scenario emitted no telemetry samples', scenario: scenario.name });
    }
    if (!Array.isArray(scenario.final) || scenario.final.length === 0) {
      issues.push({ code: 'telemetry.final_missing', severity: 1, detail: 'scenario emitted no final telemetry', scenario: scenario.name });
    }
    if ((scenario.fallbackContactEvents ?? 0) > 0) {
      issues.push({ code: 'physics.contact_fallback', severity: 0, detail: String(scenario.fallbackContactEvents), scenario: scenario.name });
    }
    if ((scenario.packetErrors ?? 0) > 0) {
      issues.push({ code: 'physics.packet_errors', severity: 0, detail: String(scenario.packetErrors), scenario: scenario.name });
    }
    if ((scenario.rejectedBackendCommands ?? 0) > 0) {
      issues.push({ code: 'physics.backend_command_rejected', severity: 0, detail: String(scenario.rejectedBackendCommands), scenario: scenario.name });
    }
    if ((scenario.droppedFixedSteps ?? 0) > 0) {
      issues.push({ code: 'physics.fixed_steps_dropped', severity: 0, detail: String(scenario.droppedFixedSteps), scenario: scenario.name });
    }
    if (Number.isFinite(maxVelocity) && Number(scenario.maxSpeed ?? 0) > maxVelocity) {
      issues.push({ code: 'physics.excessive_velocity_budget', severity: 0, detail: `${scenario.maxSpeed} > ${maxVelocity}`, scenario: scenario.name });
    }
    if (Number.isFinite(maxAngularVelocity) && Number(scenario.maxAngularVelocity ?? 0) > maxAngularVelocity) {
      issues.push({ code: 'physics.excessive_angular_velocity_budget', severity: 0, detail: `${scenario.maxAngularVelocity} > ${maxAngularVelocity}`, scenario: scenario.name });
    }
    if (Number.isFinite(maxLongStuckSeconds) && Number(scenario.maxStuckSeconds ?? 0) > maxLongStuckSeconds) {
      issues.push({ code: 'physics.long_stuck_budget', severity: 0, detail: `${scenario.maxStuckSeconds} > ${maxLongStuckSeconds}`, scenario: scenario.name });
    }
    if (scenario.name === 'multi-vehicle-scripted-soak' && Number(scenario.vehicleCount ?? 0) < 24) {
      issues.push({ code: 'physics.multi_vehicle_count', severity: 0, detail: `${scenario.vehicleCount ?? 0} < 24`, scenario: scenario.name });
    }
  }
  for (const [scenarioName, commandKind, minimum] of [
    ['road-edge-assist', 'target', 1],
    ['sleep-wake', 'sleep_state', 2],
    ['pose-reset', 'pose_reset', 2],
    ['recovery', 'recovery', 1],
  ]) {
    const count = Number(byName.get(scenarioName)?.backendCommandSamples?.[commandKind] ?? 0);
    if (count < minimum) {
      issues.push({ code: 'physics.backend_command_coverage', severity: 1, detail: `${commandKind}=${count} < ${minimum}`, scenario: scenarioName });
    }
  }
  const replayByScenario = new Map((report.replays ?? []).map((replay) => [replay.referenceScenario, replay]));
  if ((report.replays ?? []).length !== required.length) {
    issues.push({ code: 'physics.replay_scenario_count', severity: 1, detail: `${report.replays?.length ?? 0} != ${required.length}` });
  }
  for (const scenarioName of required) {
    const replay = replayByScenario.get(scenarioName);
    const scenario = byName.get(scenarioName);
    if (!replay) {
      issues.push({ code: 'physics.replay_scenario_missing', severity: 1, detail: scenarioName });
      continue;
    }
    for (const issue of replay.issues ?? []) {
      issues.push({ ...issue, scenario: scenarioName });
    }
    const expectedSamples = Number(scenario?.steps ?? 0) * Number(scenario?.vehicleCount ?? 0);
    const replayReady = replay.status === 'pass'
      && replay.name === 'PhysicsReplayVerifier'
      && replay.freshProcess === true
      && Number(replay.mismatches ?? Infinity) === 0
      && Number(replay.samples ?? 0) === expectedSamples
      && Number(replay.vehicleCount ?? 0) === Number(scenario?.vehicleCount ?? 0)
      && Number(replay.backendCommandCount ?? 0) > 0
      && Number.isFinite(Number(replay.stepHash))
      && Number.isFinite(Number(replay.backendPacketHash))
      && Number.isFinite(Number(replay.backendCommandHash))
      && Number(replay.backendCommandHash) > 0;
    if (!replayReady) {
      issues.push({ code: 'physics.replay_scenario_contract', severity: 1, detail: JSON.stringify({ expectedSamples, replay }), scenario: scenarioName });
    }
  }
  const fleetReplay = replayByScenario.get('multi-vehicle-scripted-soak');
  if (Number(fleetReplay?.vehicleCount ?? 0) !== 24 || Number(fleetReplay?.samples ?? 0) !== 240 * 24) {
    issues.push({ code: 'physics.replay_fleet_coverage', severity: 1, detail: JSON.stringify(fleetReplay ?? {}) });
  }
  for (const issue of report.replay?.issues ?? []) {
    issues.push({ ...issue, scenario: report.replay.name });
  }
  if (report.replay?.status !== 'pass') {
    issues.push({ code: 'physics.replay_failed', severity: 1, detail: JSON.stringify(report.replay ?? {}) });
  }
  const replayHasExecutableContract = Number.isFinite(Number(report.replay?.stepHash))
    && Number.isFinite(Number(report.replay?.backendPacketHash))
    && Number.isFinite(Number(report.replay?.backendCommandHash))
    && Number(report.replay?.backendCommandHash) > 0
    && Number(report.replay?.samples ?? 0) > 0
    && Number(report.replay?.mismatches ?? Infinity) === 0
    && report.replay?.freshProcess === true
    && String(report.replay?.name || '').includes('PhysicsReplayVerifier');
  if (!replayHasExecutableContract) {
    issues.push({ code: 'physics.replay_contract_missing', severity: 1, detail: JSON.stringify(report.replay ?? {}) });
  }
  if (Number(report.replay?.mismatches ?? 0) > 0) {
    issues.push({ code: 'physics.replay_mismatch', severity: 1, detail: `${report.replay?.mismatches} mismatches` });
  }
  if (Number.isFinite(Number(report.replay?.driftMeters)) && Number(report.replay?.driftMeters) > maxReplayDriftMeters) {
    issues.push({ code: 'physics.replay_drift', severity: 1, detail: `${report.replay?.driftMeters} > ${maxReplayDriftMeters}` });
  }
  const p0 = issues.filter((issue) => issue.severity === 0).length;
  const p1 = issues.filter((issue) => issue.severity === 1).length;
  return { issues, p0, p1 };
}

function markdownReport(report, gate) {
  const lines = ['# voplay physics stress', ''];
  lines.push(`- Status: ${gate.p0 === 0 && gate.p1 === 0 ? 'pass' : 'fail'}`);
  lines.push(`- Scenario count: ${report.scenarios?.length ?? 0}`);
  lines.push(`- Replay: ${report.replay?.name || '(missing)'} samples=${report.replay?.samples ?? '(missing)'} mismatches=${report.replay?.mismatches ?? '(missing)'} stepHash=${report.replay?.stepHash ?? '(missing)'} backendPacketHash=${report.replay?.backendPacketHash ?? '(missing)'} backendCommandHash=${report.replay?.backendCommandHash ?? '(missing)'}`);
  lines.push(`- Scenario replays: ${(report.replays ?? []).length}`);
  lines.push(`- Budget: ${path.relative(root, budgetPath)}`);
  lines.push(`- Velocity budget: ${formatNumber(physicsBudget.maxVelocity)} m/s`);
  lines.push(`- Angular velocity budget: ${formatNumber(physicsBudget.maxAngularVelocity)} rad/s`);
  lines.push(`- Long stuck budget: ${formatNumber(physicsBudget.maxLongStuckSeconds)}s`);
  lines.push(`- P0/P1: ${gate.p0}/${gate.p1}`);
  lines.push(`- Program: ${scenarioFile}`);
  lines.push(`- Project: ${path.relative(root, projectDir)}`);
  lines.push(`- Log: ${path.relative(root, path.join(outDir, 'physics-stress-run.log'))}`);
  lines.push('');
  for (const scenario of report.scenarios ?? []) {
    lines.push(`## ${scenario.name}`);
    lines.push(`- Status: ${scenario.status}`);
    lines.push(`- Vehicles/steps: ${scenario.vehicleCount}/${scenario.steps}`);
    lines.push(`- Max speed: ${formatNumber(scenario.maxSpeed)} m/s`);
    lines.push(`- Max angular velocity: ${formatNumber(scenario.maxAngularVelocity)} rad/s`);
    lines.push(`- Max landing impulse proxy: ${formatNumber(scenario.maxLandingImpulse)}`);
    lines.push(`- Contacts: wheels=${scenario.maxContactCount} bodyEvents=${scenario.maxContactEvents} fallback=${scenario.fallbackContactEvents ?? 0} normalImpulse=${formatNumber(scenario.maxNormalImpulse)} tangentImpulse=${formatNumber(scenario.maxTangentImpulse)}`);
    lines.push(`- Backend safety: packetErrors=${scenario.packetErrors ?? 0} rejectedBackendCommands=${scenario.rejectedBackendCommands ?? 0} droppedFixedSteps=${scenario.droppedFixedSteps ?? 0}`);
    lines.push(`- Surfaces: ${JSON.stringify(scenario.surfaceSamples ?? {})}`);
    lines.push(`- Recovery requests: ${scenario.recoveryRequests}`);
    for (const issue of scenario.issues ?? []) {
      lines.push(`- Issue ${issue.code}: ${issue.detail}`);
    }
    lines.push('');
  }
  for (const replay of report.replays ?? []) {
    lines.push(`## Replay ${replay.referenceScenario}`);
    lines.push(`- Status/fresh process: ${replay.status}/${replay.freshProcess === true}`);
    lines.push(`- Vehicles/samples/commands: ${replay.vehicleCount}/${replay.samples}/${replay.backendCommandCount}`);
    lines.push(`- Hashes/mismatches: ${replay.stepHash}/${replay.backendPacketHash}/${replay.backendCommandHash}/${replay.mismatches}`);
    lines.push('');
  }
  if (gate.issues.length > 0) {
    lines.push('## Gate Issues');
    for (const issue of gate.issues) {
      lines.push(`- ${issue.scenario ? `${issue.scenario}: ` : ''}${issue.code} ${issue.detail ?? ''}`);
    }
    lines.push('');
  }
  return `${lines.join('\n')}\n`;
}

function formatNumber(value) {
  const number = Number(value);
  return Number.isFinite(number) ? number.toFixed(4) : 'n/a';
}

mkdirSync(outDir, { recursive: true });
const replayExecutionPlan = requireReplayExecutionPlan();
const report = runScenarioProgram();
report.replayExecutionPlan = replayExecutionPlan;
const gate = validateReport(report);
const generatedAt = new Date().toISOString();
report.summary = {
  ...(report.summary ?? {}),
  p0: gate.p0,
  p1: gate.p1,
  passCount: (report.scenarios ?? []).filter((scenario) => scenario.status === 'pass').length + (report.replay?.status === 'pass' ? 1 : 0),
};
report.gateIssues = gate.issues;
report.status = gate.p0 === 0 && gate.p1 === 0 ? 'pass' : 'fail';
report.generatedAt = generatedAt;
report.freshEvidence = sourceBoundEvidence({
  gate: 'voplay-physics-industrial-stress',
  generatedAt,
  root,
  repos: [
    { name: 'volang', root },
    { name: 'voplay', root: voplayRoot },
    { name: 'vogui', root: voguiRoot },
    { name: 'vopack', root: vopackRoot },
  ],
  gateFiles: [
    'scripts/ci/voplay_physics_stress.mjs',
    'scripts/ci/repo_roots.mjs',
    'scripts/ci/source_bound_evidence.mjs',
    scenarioSourceFile,
    budgetPath,
    'eng/tasks.toml',
    'eng/ci.toml',
  ],
  artifacts: [
    path.join(outDir, 'physics-stress-run.log'),
  ],
});
writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify(report, null, 2)}\n`);
writeFileSync(path.join(outDir, 'report.md'), markdownReport(report, gate));

if (report.status !== 'pass') {
  fail(`stress thresholds failed: p0=${gate.p0} p1=${gate.p1}; see ${path.join(outDir, 'report.json')}`);
}
console.log(`voplay physics stress: ok ${path.join(outDir, 'report.json')}`);
