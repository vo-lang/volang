#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const outDir = path.resolve(argValue('--out-dir') || process.env.VOPLAY_PHYSICS_STRESS_OUT_DIR || path.join(root, 'target/voplay-physics-stress'));
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
  const command = ['./d.py', 'run', path.relative(root, projectEntry)];
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
  for (const issue of report.replay?.issues ?? []) {
    issues.push({ ...issue, scenario: report.replay.name });
  }
  if (report.replay?.status !== 'pass') {
    issues.push({ code: 'physics.replay_failed', severity: 1, detail: JSON.stringify(report.replay ?? {}) });
  }
  const replayHasExecutableContract = Number.isFinite(Number(report.replay?.stepHash))
    && Number.isFinite(Number(report.replay?.backendPacketHash))
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
  lines.push(`- Replay: ${report.replay?.name || '(missing)'} samples=${report.replay?.samples ?? '(missing)'} mismatches=${report.replay?.mismatches ?? '(missing)'} stepHash=${report.replay?.stepHash ?? '(missing)'} backendPacketHash=${report.replay?.backendPacketHash ?? '(missing)'}`);
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
    lines.push(`- Surfaces: ${JSON.stringify(scenario.surfaceSamples ?? {})}`);
    lines.push(`- Recovery requests: ${scenario.recoveryRequests}`);
    for (const issue of scenario.issues ?? []) {
      lines.push(`- Issue ${issue.code}: ${issue.detail}`);
    }
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
const report = runScenarioProgram();
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
  gate: outDir.includes('industrial') ? 'voplay-physics-industrial-stress' : 'voplay-physics-stress',
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
