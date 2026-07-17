#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, readdirSync, statSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  acceptedVolangPlanSnapshotCommits,
  activePlanSnapshotContract,
  runActivePlanSnapshotContractSelftest,
} from './active_plan_snapshot.mjs';
import { BLOCKKART_BASELINE_BUDGET_DEFAULTS } from './blockkart_baseline_budget.mjs';
import { requireRepoRoot } from './repo_roots.mjs';
import { isBlockKartPerfFrameRecord } from './studio_browser_smoke_contract.mjs';
import {
  sourceBoundEvidence,
  verifySourceBoundEvidence,
} from './source_bound_evidence.mjs';

const QUICKPLAY_COMMON_GATE_FILES = Object.freeze([
  'scripts/ci/quickplay_vnext.mjs',
  'scripts/ci/quickplay_validate.mjs',
  'apps/studio/scripts/package_blockkart_quickplay.mjs',
  'apps/studio/src/lib/backend/web_backend.ts',
  'apps/studio/src/lib/quickplay.ts',
  'eng/artifacts.toml',
  'eng/tasks.toml',
  'eng/project.toml',
]);
const QUICKPLAY_SOURCE_AUDIT_GATE_FILES = Object.freeze([
  'scripts/ci/quickplay_source_audit.mjs',
  ...QUICKPLAY_COMMON_GATE_FILES,
]);
const QUICKPLAY_REGENERATE_GATE_FILES = Object.freeze([
  'scripts/ci/quickplay_regenerate_check.mjs',
  ...QUICKPLAY_COMMON_GATE_FILES,
]);

const root = path.resolve(fileURLToPath(new URL('../..', import.meta.url)));
const baselineContractSelftest = process.argv.slice(2).includes('--selftest-baseline-contract');
if (baselineContractSelftest) {
  runBlockKartBaselineReportContractSelftest();
  console.log('voplay industrial readiness: baseline report contract selftest passed');
  process.exit(0);
}
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const voguiRoot = requireRepoRoot('VOGUI_ROOT', 'vogui');
const vopackRoot = requireRepoRoot('VOPACK_ROOT', 'vopack');

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
const currentCiRunId = process.env.VO_DEV_CI_RUN_ID ?? null;

function fixedFreshEvidenceScope(gate) {
  const volang = { name: 'volang', root };
  const voplay = { name: 'voplay', root: voplayRoot };
  const blockKart = { name: 'BlockKart', root: blockKartRoot };
  const vogui = { name: 'vogui', root: voguiRoot };
  const vopack = { name: 'vopack', root: vopackRoot };
  const quickplayArtifacts = [
    'apps/studio/public/quickplay/blockkart/project.json',
    'apps/studio/public/quickplay/blockkart/deps.json',
    'apps/studio/public/quickplay/blockkart/provenance.json',
  ];
  const renderStressGateFiles = [
    'scripts/ci/voplay_render_stress.mjs',
    'scripts/ci/blockkart_baseline.mjs',
    'scripts/ci/blockkart_baseline_budget.mjs',
    'scripts/ci/studio_browser_smoke_contract.mjs',
    'scripts/ci/repo_roots.mjs',
    'scripts/ci/source_bound_evidence.mjs',
    'eng/perf-budgets/blockkart-voplay.medium.json',
    'eng/tasks.toml',
    'eng/ci.toml',
  ];
  const scopes = {
    'voplay-render-core-unit': {
      repos: [volang, voplay],
      gateFiles: ['scripts/ci/voplay_rust_unit.mjs', 'scripts/ci/repo_roots.mjs', 'scripts/ci/source_bound_evidence.mjs', 'eng/tasks.toml', 'eng/ci.toml'],
      artifacts: ['target/voplay-render-core-unit/rust-unit.log'],
    },
    'voplay-batch-planner-unit': {
      repos: [volang, voplay],
      gateFiles: ['scripts/ci/voplay_rust_unit.mjs', 'scripts/ci/repo_roots.mjs', 'scripts/ci/source_bound_evidence.mjs', 'eng/tasks.toml', 'eng/ci.toml'],
      artifacts: ['target/voplay-batch-planner-unit/rust-unit.log'],
    },
    'voplay-render-architecture-lint': {
      repos: [volang, voplay, blockKart],
      gateFiles: ['scripts/ci/voplay_render_architecture_lint.mjs', 'scripts/ci/repo_roots.mjs', 'scripts/ci/source_bound_evidence.mjs', 'eng/tasks.toml', 'eng/ci.toml'],
      artifacts: [],
    },
    'voplay-scene3d-contract': {
      repos: [volang, voplay, vogui, vopack],
      gateFiles: ['scripts/ci/voplay_scene3d_unit.mjs', 'scripts/ci/repo_roots.mjs', 'scripts/ci/source_bound_evidence.mjs', 'eng/tasks.toml', 'eng/ci.toml'],
      artifacts: ['target/voplay-scene3d-contract/scene3d-unit.log'],
    },
    'blockkart-product-boundary-strict': {
      repos: [volang, blockKart, voplay],
      gateFiles: ['scripts/ci/blockkart_product_boundary_strict.mjs', 'scripts/ci/blockkart_boundary_facts.mjs', 'scripts/ci/repo_roots.mjs', 'scripts/ci/source_bound_evidence.mjs', 'eng/tasks.toml', 'eng/ci.toml'],
      artifacts: [],
    },
    'voplay-render-stress-budgeted': {
      repos: [volang, voplay, blockKart],
      gateFiles: renderStressGateFiles,
      artifacts: [
        ...quickplayArtifacts,
        'target/voplay-render-stress-budgeted/blockkart-quickplay-baseline',
        'target/voplay-render-stress-budgeted/blockkart-primitive-10k',
        'target/voplay-render-stress-budgeted/blockkart-water',
        'target/voplay-render-stress-budgeted/blockkart-resource-churn-soak',
        'target/voplay-render-stress-budgeted/blockkart-chunked-world-drive',
        'target/voplay-render-stress-budgeted/blockkart-shadow-post-matrix',
        'target/voplay-render-stress-budgeted/blockkart-resize-recreate-targets',
        'target/voplay-render-stress-budgeted/blockkart-restart-50',
      ],
    },
    'voplay-render-soak-10m': {
      repos: [volang, voplay, blockKart],
      gateFiles: renderStressGateFiles,
      artifacts: [
        ...quickplayArtifacts,
        'target/voplay-render-soak-10m/blockkart-quickplay-baseline-soak-10m',
      ],
    },
    'voplay-physics-industrial-stress': {
      repos: [volang, voplay, vogui, vopack],
      gateFiles: ['scripts/ci/voplay_physics_stress.mjs', 'scripts/ci/repo_roots.mjs', 'scripts/ci/source_bound_evidence.mjs', 'eng/perf-budgets/blockkart-voplay.medium.json', 'eng/tasks.toml', 'eng/ci.toml'],
      artifacts: ['target/voplay-physics-industrial-stress/physics-stress-run.log'],
    },
    // Repository identities and ordering intentionally mirror each producer.
    // The regenerate producer has a different dependency order from audit/validate.
    'quickplay-source-audit': {
      repos: [
        volang,
        blockKart,
        { name: 'github.com/vo-lang/vogui', root: voguiRoot },
        { name: 'github.com/vo-lang/vopack', root: vopackRoot },
        { name: 'github.com/vo-lang/voplay', root: voplayRoot },
      ],
      gateFiles: QUICKPLAY_SOURCE_AUDIT_GATE_FILES,
      artifacts: ['apps/studio/public/quickplay/blockkart'],
    },
    'quickplay-regenerate-check': {
      repos: [
        volang,
        blockKart,
        { name: 'github.com/vo-lang/vogui', root: voguiRoot },
        { name: 'github.com/vo-lang/voplay', root: voplayRoot },
        { name: 'github.com/vo-lang/vopack', root: vopackRoot },
      ],
      gateFiles: QUICKPLAY_REGENERATE_GATE_FILES,
      artifacts: ['apps/studio/public/quickplay/blockkart'],
    },
    'quickplay-validate': {
      repos: [
        volang,
        blockKart,
        { name: 'github.com/vo-lang/vogui', root: voguiRoot },
        { name: 'github.com/vo-lang/vopack', root: vopackRoot },
        { name: 'github.com/vo-lang/voplay', root: voplayRoot },
      ],
      gateFiles: QUICKPLAY_COMMON_GATE_FILES,
      artifacts: ['apps/studio/public/quickplay/blockkart'],
    },
    'voplay-industrial-readiness': {
      repos: [volang, vogui, voplay, vopack, blockKart],
      gateFiles: [
        'scripts/ci/voplay_industrial_readiness.mjs',
        'scripts/ci/active_plan_snapshot.mjs',
        'scripts/ci/repo_roots.mjs',
        'scripts/ci/source_bound_evidence.mjs',
        'scripts/ci/voplay_render_architecture_lint.mjs',
        'scripts/ci/blockkart_product_boundary_strict.mjs',
        'scripts/ci/quickplay_source_audit.mjs',
        'scripts/ci/voplay_render_stress.mjs',
        'scripts/ci/voplay_physics_stress.mjs',
        'scripts/ci/blockkart_baseline.mjs',
        'scripts/ci/blockkart_baseline_budget.mjs',
        'scripts/ci/studio_browser_smoke_contract.mjs',
        'eng/tasks.toml',
        'eng/ci.toml',
        'eng/project.toml',
      ],
      artifacts: [
        ...quickplayArtifacts,
        'target/voplay-render-stress-budgeted/report.json',
        'target/voplay-render-soak-10m/report.json',
        'target/voplay-physics-industrial-stress/report.json',
        'target/blockkart-baseline/blockkart-baseline.json',
        'target/blockkart-baseline-restart-50/blockkart-baseline.json',
      ],
    },
  };
  if (gate === 'blockkart-baseline' || gate === 'blockkart-baseline-restart-50') {
    const artifactRoot = `target/${gate}`;
    const optionalScreenshots = [
      `${artifactRoot}/blockkart-baseline-viewport.png`,
      `${artifactRoot}/blockkart-baseline-canvas.png`,
    ].filter((file) => existsSync(path.join(root, file)));
    return {
      repos: [volang, voplay, blockKart],
      gateFiles: ['scripts/ci/blockkart_baseline.mjs', 'scripts/ci/blockkart_baseline_budget.mjs', 'scripts/ci/studio_browser_smoke_contract.mjs', 'scripts/ci/quickplay_validate.mjs', 'scripts/ci/repo_roots.mjs', 'scripts/ci/source_bound_evidence.mjs', 'apps/studio/package.json', 'apps/studio/vite.config.ts', 'eng/tasks.toml', 'eng/ci.toml'],
      artifacts: ['apps/studio/public/quickplay/blockkart', 'apps/studio/dist/index.html', ...optionalScreenshots],
    };
  }
  const scope = scopes[gate];
  if (!scope) throw new Error(`missing fixed freshEvidence scope for ${gate}`);
  return scope;
}

function addCheck(phase, code, status, detail, evidence = {}) {
  const evidenceItems = Array.isArray(evidence) ? evidence : Object.values(evidence ?? {});
  const firstEvidence = evidenceItems.find((item) => item && typeof item === 'object' && (item.path || item.file || item.ref || item.line));
  const owner =
    evidence.owner
    || firstEvidence?.owner
    || (code.includes('blockkart') ? 'BlockKart'
      : code.includes('physics') ? 'voplay/scene3d'
      : code.includes('artifact') || code.includes('quickplay') ? 'studio/artifacts'
      : code.includes('docs') || code.includes('plan') ? 'docs'
      : code.includes('render') || code.includes('framegraph') || code.includes('batch') || code.includes('primitive') ? 'voplay/render'
      : phase === 'source-audit' ? 'source-audit'
      : 'eng');
  const file = evidence.file || evidence.path || firstEvidence?.file || firstEvidence?.path || null;
  const line = evidence.line || firstEvidence?.line || null;
  const reason = evidence.reason || detail;
  const requiredFix = evidence.requiredFix || 'Fix the owning source path and add source-bound behavior evidence before this gate may pass.';
  checks.push({
    phase,
    code,
    status: status ? 'pass' : 'fail',
    severity: status ? null : 'P0',
    detail,
    owner,
    file,
    line,
    reason,
    requiredFix,
    evidence,
  });
}

function addRequiredSourceFact(code, status, detail, evidence = {}) {
  const fact = {
    code,
    required: true,
    status: Boolean(status),
    detail,
    owner: evidence.owner || (code.includes('blockkart') ? 'BlockKart' : code.includes('physics') ? 'voplay/scene3d' : 'voplay/render'),
    file: evidence.file || evidence.path || null,
    line: evidence.line || null,
    reason: evidence.reason || detail,
    requiredFix: evidence.requiredFix || 'Replace string-only readiness with source and behavior evidence for this required fact.',
    evidence,
  };
  sourceFactRequirements.push(fact);
  addCheck('source-audit', `source_fact.${code}`, fact.status, detail, fact);
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

function productionSource(source) {
  const value = source || '';
  return value.split('#[cfg(test)]')[0] || value;
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

function lineOfAfter(source, startToken, token) {
  const start = source.indexOf(startToken);
  if (start < 0) {
    return lineOf(source, token);
  }
  const line = lineOf(source.slice(start), token);
  if (line === null) {
    return null;
  }
  return source.slice(0, start).split(/\r?\n/).length + line - 1;
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

function blockAfterToken(source, token) {
  const start = source.indexOf(token);
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

function packetDecodeBranchFailures(sceneSource, physicsWorldSource) {
  const specs = [
    {
      path: 'scene3d/physics_world.vo',
      sourceText: physicsWorldSource,
      signature: 'func (w *PhysicsWorldState) step',
      token: 'if headerError != ""',
      source: 'body',
      reason: 'invalid_header',
      reasonToken: 'Reason: headerError',
      expectedReturn: 'PhysicsBackendPacketError{',
    },
    {
      path: 'scene3d/physics_world.vo',
      sourceText: physicsWorldSource,
      signature: 'func (w *PhysicsWorldState) step',
      token: 'if r.Remaining() < 4',
      source: 'body',
      reason: 'missing_count',
      expectedReturn: 'PhysicsBackendPacketError{',
    },
    {
      path: 'scene3d/physics_world.vo',
      sourceText: physicsWorldSource,
      signature: 'func (w *PhysicsWorldState) step',
      token: 'if r.Remaining() != count * physics3DBodyStateBytes',
      source: 'body',
      reason: 'body_payload_length_mismatch',
      expectedReturn: 'PhysicsBackendPacketError{',
    },
    {
      path: 'scene3d/scene.vo',
      sourceText: sceneSource,
      signature: 'func (s *Scene) Contacts',
      token: 'if headerError != ""',
      source: 'contact',
      reason: 'invalid_header',
      reasonToken: 'Reason: headerError',
      expectedReturn: 'return []Contact{}',
    },
    {
      path: 'scene3d/scene.vo',
      sourceText: sceneSource,
      signature: 'func (s *Scene) Contacts',
      token: 'if r.Remaining() < 4',
      source: 'contact',
      reason: 'missing_count',
      expectedReturn: 'return []Contact{}',
    },
    {
      path: 'scene3d/scene.vo',
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
      path: 'scene3d/scene.vo',
      line: lineOfAfter(sceneSource, 'func (s *Scene) Contacts', 'return nil'),
      text: 'return nil',
      reason: 'Contacts must return an empty slice with packet error telemetry on malformed packets',
    });
  }
  return failures;
}

function bodyOfType(source, typeName) {
  const start = source.indexOf(`type ${typeName} struct`);
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
  const scope = fixedFreshEvidenceScope(expectedGate);
  return verifySourceBoundEvidence({
    evidence: report?.freshEvidence,
    expectedGate,
    expectedCiRunId: currentCiRunId,
    root,
    expectedGateFiles: scope.gateFiles,
    expectedArtifacts: scope.artifacts,
    expectedRepos: scope.repos,
  });
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

function sceneObservabilityFailures(report) {
  return (report?.scenes || []).flatMap((scene) => {
    const heartbeat = scene?.observability;
    const complete = heartbeat?.status === 'pass'
      && heartbeat?.stage === 'complete'
      && Array.isArray(heartbeat?.timeline)
      && heartbeat.timeline.length > 0
      && Number.isFinite(heartbeat?.frameIndex)
      && typeof heartbeat?.lastPass === 'string'
      && heartbeat.lastPass.length > 0
      && Number.isFinite(heartbeat?.frameP90Ms)
      && Number.isFinite(heartbeat?.frameP99Ms)
      && Number.isFinite(heartbeat?.resourceChurn)
      && (heartbeat?.telemetryStatus === 'running' || heartbeat?.telemetryStatus === 'pass')
      && heartbeat?.telemetryFailure == null
      && heartbeat?.perfEndpointError == null
      && Number.isFinite(heartbeat?.telemetryReportCount)
      && Number.isFinite(heartbeat?.telemetryReportAgeMs)
      && Number.isFinite(heartbeat?.telemetryObservedSpanMs)
      && Number.isFinite(heartbeat?.telemetryFrameProgress)
      && heartbeat?.lastTelemetryPacket
      && typeof heartbeat.lastTelemetryPacket === 'object';
    return complete ? [] : [{ scene: scene?.name ?? null, heartbeat: heartbeat ?? null }];
  });
}

function sceneLongRunTelemetryFailures(report) {
  return (report?.scenes || []).flatMap((scene) => {
    const telemetry = scene?.diagnostics?.captureTelemetry;
    const requiredSpanMs = Math.max(0, Number(telemetry?.requestedMs ?? 0) - Number(telemetry?.spanGraceMs ?? 0));
    const complete = telemetry?.required === true
      && telemetry?.status === 'pass'
      && telemetry?.thresholdMs === BLOCKKART_BASELINE_BUDGET_DEFAULTS.longRunTelemetryThresholdMs
      && telemetry?.maxReportAgeMs === BLOCKKART_BASELINE_BUDGET_DEFAULTS.longRunTelemetryMaxAgeMs
      && telemetry?.spanGraceMs === BLOCKKART_BASELINE_BUDGET_DEFAULTS.longRunTelemetrySpanGraceMs
      && Number(telemetry?.actualMs ?? 0) >= requiredSpanMs
      && Number(telemetry?.observedSpanMs ?? 0) >= requiredSpanMs
      && Number(telemetry?.lastReportAgeMs ?? Infinity) <= Number(telemetry?.maxReportAgeMs ?? 0)
      && Number(telemetry?.frameProgress ?? 0) > 0
      && telemetry?.failure == null;
    return complete ? [] : [{ scene: scene?.name ?? null, telemetry: telemetry ?? null }];
  });
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
const frameGraphResourceRegistry = readProjectFile(voplayRoot, 'rust/src/renderer_frame/resource_registry.rs', 'phase-0', 'source.framegraph_resource_registry_exists');
const frameGraphBackingSource = [
  readText(path.join(voplayRoot, 'rust/src/renderer_frame/resource_backing.rs')) || '',
  ...listFiles(path.join(voplayRoot, 'rust/src/renderer_frame/resource_registry'), '.rs')
    .map((file) => readText(file) || ''),
].join('\n');
const frameGraphAuditSource = `${frameGraph}\n${frameGraphResourceRegistry}\n${frameGraphBackingSource}`;
const renderWorld = readProjectFile(voplayRoot, 'rust/src/render_world.rs', 'phase-0', 'source.render_world_exists');
const renderWorldAuditSource = [
  renderWorld,
  readText(path.join(voplayRoot, 'rust/src/render_world/store.rs')) || '',
  readText(path.join(voplayRoot, 'rust/src/render_world/tests.rs')) || '',
].join('\n');
const primitivePipeline = readProjectFile(voplayRoot, 'rust/src/primitive_pipeline.rs', 'phase-0', 'source.primitive_pipeline_exists');
const primitivePipelineRuntime = readProjectFile(voplayRoot, 'rust/src/primitive_pipeline/runtime.rs', 'phase-0', 'source.primitive_pipeline_runtime_exists');
const primitivePipelineTests = readText(path.join(voplayRoot, 'rust/src/primitive_pipeline/tests.rs')) || '';
const primitivePipelineAuditSource = `${primitivePipeline}\n${primitivePipelineRuntime}\n${primitivePipelineTests}`;
const pipeline3d = readProjectFile(voplayRoot, 'rust/src/pipeline3d.rs', 'phase-0', 'source.pipeline3d_exists');
const pipelineCache = readProjectFile(voplayRoot, 'rust/src/pipeline3d/pipeline_cache.rs', 'phase-0', 'source.pipeline_cache_exists');
const rendererRuntimeFile = readProjectFile(voplayRoot, 'rust/src/renderer_runtime.rs', 'phase-0', 'source.renderer_runtime_exists');
const pipeline3dOwnerSource = listFiles(path.join(voplayRoot, 'rust/src/pipeline3d'), '.rs')
  .map((file) => readText(file) || '')
  .join('\n');
const vehicle = readProjectFile(voplayRoot, 'scene3d/vehicle.vo', 'phase-0', 'source.vehicle_exists');
const dynamics = readProjectFile(voplayRoot, 'scene3d/kart_dynamics.vo', 'phase-0', 'source.dynamics_exists');
const physicsBackend = readProjectFile(voplayRoot, 'scene3d/physics_backend.vo', 'phase-0', 'source.physics_backend_exists');
const physicsCommands = readProjectFile(voplayRoot, 'scene3d/physics_commands.vo', 'phase-0', 'source.physics_commands_exists');
const physicsTypes = readProjectFile(voplayRoot, 'scene3d/physics_types.vo', 'phase-0', 'source.physics_types_exists');
const physicsWorld = readProjectFile(voplayRoot, 'scene3d/physics_world.vo', 'phase-0', 'source.physics_world_exists');
const physics = [physicsBackend, physicsCommands, physicsTypes, physicsWorld].join('\n');
const contactEvent = readProjectFile(voplayRoot, 'scene3d/contact_event.vo', 'phase-0', 'source.contact_event_exists');
const replay = readProjectFile(voplayRoot, 'scene3d/replay.vo', 'phase-0', 'source.replay_exists');
const vehiclePhysicsSession = readProjectFile(voplayRoot, 'scene3d/vehicle_physics_session.vo', 'phase-0', 'source.vehicle_physics_session_exists');
const physicsStressSource = readProjectFile(voplayRoot, 'examples/physics_stress/main.vo', 'phase-0', 'source.physics_stress_exists');
const voplayToolSources = [
  readText(path.join(voplayRoot, 'tools/vehicle_telemetry_parity.vo')) || '',
  readText(path.join(voplayRoot, 'tools/racing_reference_demos.vo')) || '',
].join('\n');
const sceneSource = readProjectFile(voplayRoot, 'scene3d/scene.vo', 'phase-0', 'source.scene_exists');
const vehicleTelemetry = readProjectFile(voplayRoot, 'scene3d/vehicle_telemetry.vo', 'phase-0', 'source.vehicle_telemetry_exists');
const blockKartWorld = readProjectFile(blockKartRoot, 'world.vo', 'phase-0', 'source.blockkart_world_exists');
const blockKartPrimitiveWorld = readProjectFile(blockKartRoot, 'primitive_world.vo', 'phase-0', 'source.blockkart_primitive_world_exists');
const blockKartTrackRuntime = readProjectFile(blockKartRoot, 'track_runtime.vo', 'phase-0', 'source.blockkart_track_runtime_exists');
const blockKartBudget = readProjectFile(blockKartRoot, 'performance_budget.vo', 'phase-0', 'source.blockkart_budget_exists');
const blockKartVoFiles = listVoFiles(blockKartRoot);
const projectToml = readText(path.join(root, 'eng/project.toml')) || '';
const activeQualityPlanSource = readText(path.join(root, 'lang/docs/dev/voplay-code-engineering-quality-plan.md')) || '';
const rendererRuntime = [renderer, rendererFrameSubmit, rendererFrameOrchestrator, rendererFrameOrchestratorRuntime].join('\n');
const rendererModuleRuntime = listFiles(path.join(voplayRoot, 'rust', 'src', 'renderer'), '.rs')
  .map((file) => productionSource(readText(file) || ''))
  .join('\n');
const rendererAuditSource = [productionSource(renderer), rendererFrameSubmit, rendererFrameOrchestrator, rendererModuleRuntime, frameGraphAuditSource].join('\n');

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
  'rust/src/renderer_frame/resource_registry.rs': {
    lines: lineCount(frameGraphResourceRegistry),
    budget: 360,
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
function ownerModuleFacts({ path: relativePath, owner, requiredMethods }, productionSource) {
  const source = readText(path.join(voplayRoot, relativePath)) || '';
  const missingMethods = requiredMethods.filter((method) => !source.includes(`fn ${method}`));
  const hasProductionMethod = missingMethods.length === 0;
  const calledByProduction = owner === 'Pipeline3D'
    ? requiredMethods.some((method) => productionSource.includes(`.${method}(`))
    : productionSource.includes(`${owner}::`);
  return {
    path: relativePath,
    owner,
    lines: lineCount(source),
    hasProductionMethod,
    calledByProduction,
    missingMethods,
    status: hasProductionMethod && calledByProduction ? 'pass' : 'fail',
  };
}
const pipelineOwnerFiles = [
  { path: 'rust/src/pipeline3d/mesh_submitter.rs', owner: 'MeshSubmitter', requiredMethods: ['submit'] },
  { path: 'rust/src/pipeline3d/decal_submitter.rs', owner: 'DecalSubmitter', requiredMethods: ['prepare_and_upload'] },
  { path: 'rust/src/pipeline3d/pipeline_factory.rs', owner: 'PipelineFactory', requiredMethods: ['create'] },
  { path: 'rust/src/pipeline3d/pipeline_cache.rs', owner: 'Pipeline3D', requiredMethods: ['ensure_model_capacity', 'ensure_main_texture_bind_group', 'ensure_terrain_texture_bind_group'] },
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
const blockKartPrimitiveAuthoringPresent = /primitive3d\.NewLayer|primitive3d\.NewBuilder|primitive3d\.LayerDesc|primitive3d\.ChunkingDesc|primitive3d\.MaterialDesc|SpawnPreparedMapPrimitiveLayers|spawnPrimitiveRoadBoxPhysics|spawnPrimitiveTrackPhysics|spawnRoadColliderStrip/.test(blockKartPrimitiveWorld);
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
].every((token) => frameGraphAuditSource.includes(token));
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
  && rendererFrameOrchestrator.includes('workload.planned_projected_decals')
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
  source: productionSource(readText(path.join(voplayRoot, relativePath)) || ''),
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
    { path: 'rust/src/renderer.rs', source: productionSource(renderer) },
    { path: 'rust/src/renderer_runtime.rs', source: productionSource(rendererRuntimeFile) },
    { path: 'rust/src/pipeline3d.rs', source: productionSource(pipeline3d) },
    { path: 'rust/src/renderer_frame.rs', source: productionSource(frameGraph) },
    { path: 'rust/src/renderer_frame/resource_registry.rs', source: productionSource(frameGraphResourceRegistry) },
    { path: 'rust/src/render_world.rs', source: productionSource(renderWorld) },
    { path: 'rust/src/primitive_pipeline.rs', source: productionSource(primitivePipeline) },
    { path: 'rust/src/primitive_pipeline/runtime.rs', source: primitivePipelineRuntime },
    ...renderPassModuleEntries,
  ],
  /\b(panic!|expect\s*\(|unwrap\s*\(|assert!\s*\(|assert_eq!\s*\(|assert_ne!\s*\(|todo!|unimplemented!)/,
);
const invalidBatchIndexSilentSkipHits = sourceRegexHits(
  [{ path: 'rust/src/render_world.rs', source: renderWorld }],
  /\.filter_map\(\|[^|]*(?:index|draw_index|chunk_index)[^|]*\|[\s\S]*?\.get\(/,
);
const dirtyRangeFullRebuildHits = [
  ...sourceRegexHits([{ path: 'rust/src/primitive_pipeline/runtime.rs', source: primitivePipelineRuntime }], /rebuild_resident_chunk\(device,\s*queue,\s*range\.chunk_ref,\s*models\)/),
];
const dirtyRangePartialUploadVerified = /partial.*upload|upload.*partial/i.test(primitivePipelineAuditSource)
  && /fn\s+upload_resident_dirty_range[\s\S]*queue\.write_buffer\([\s\S]*byte_offset[\s\S]*bytemuck::bytes_of/s.test(primitivePipelineRuntime)
  && /full_rebuild_count\s*(?:[:=]|,)\s*0|fullRebuildCount\s*=\s*0/.test(primitivePipelineAuditSource);
const broadPassCapabilityPattern = /\b(?:resources|pipelines|assets|renderer):\s*&'?[_A-Za-z0-9<> ,]*(?:FramePassResources|RenderPipelineScope|RenderAssetScope|Renderer)\b/;
const renderPassContextWideHits = sourceRegexHits(renderPassModuleEntries, broadPassCapabilityPattern);
const renderPassContextNegativeFixtureRejected = broadPassCapabilityPattern.test(
  "resources: &'a FramePassResources<'a>",
);
const resourceRegistryBackingAgreement = frameGraphAuditSource.includes('backing_generation')
  && frameGraphAuditSource.includes('actual_texture_view')
  && frameGraphAuditSource.includes('actual_backing_identity')
  && frameGraphAuditSource.includes('validate_backing(');
const sessionStepBody = bodyOfFunction(vehiclePhysicsSession, 'func (s *VehiclePhysicsSession) Step');
const sessionIsWrapperOnly = /FixedPhysicsStep\s*\(|StepAndSyncPhysics\s*\(/.test(sessionStepBody)
  || !/s\.(?:StepIndex|Telemetry|Replay|Backend|LastPacket|InvalidSample|Controller|Dynamics)/.test(sessionStepBody);
const physicsStressBypassesSession = /UpdateIntent\s*\(/.test(physicsStressSource)
  || /StepAndSyncPhysics\s*\(/.test(physicsStressSource);
const physicsToolsBypassSession = /UpdateIntent\s*\(/.test(voplayToolSources)
  || /StepAndSyncPhysics\s*\(/.test(voplayToolSources);
const backendPacketSchemaTokens = [
  'PhysicsBackendPacketSchemaVersion',
  'PhysicsBackendPacketKind',
  'PhysicsBackendPacketLength',
  'PhysicsBackendPacketHash',
  'PhysicsBackendCapability',
];
const backendPacketContractSource = [sceneSource, physics, dynamics, replay].join('\n');
const backendPacketMissingTokens = backendPacketSchemaTokens.filter((token) => !backendPacketContractSource.includes(token));
const backendPacketEnforcementTokens = [
  'WritePhysicsBackendPacketHeader',
  'ReadPhysicsBackendPacketHeader',
  'ValidatePhysicsBackendPacketHeader',
];
const backendPacketMissingEnforcementTokens = backendPacketEnforcementTokens.filter((token) => !backendPacketContractSource.includes(token));
const backendPacketStillCountDelimited = /Remaining\(\)\s*>=\s*count\s*\*/.test(sceneSource);
const invalidWheelPacketSanitized = !vehicle.includes('RawInvalidSampleCount')
  || !(readText(path.join(voplayRoot, 'scene3d/vehicle_telemetry.vo')) || '').includes('RawInvalidSampleCount');
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
  && physicsStressSource.includes('Replays []ReplayReport')
  && physicsStressSource.includes('for _, def := range defs')
  && physicsStressSource.includes('ValidatePhysicsReplaySample')
  && physicsStressSource.includes('ValidatePhysicsBackendCommandReplay')
  && physicsStressSource.includes('StepFleet');
const freshProcessReplayReady = physicsStressSource.includes('RunPhysicsReplayVerifierProcess')
  || physicsStressSource.includes('execPhysicsReplayVerifier')
  || physicsStressSource.includes('--verify-replay-trace');
const sameRuntimeReplayDriftOnly = /replayDrift|driftMeters|ReplayDrift/.test(physicsStressSource)
  || !replayUsesRecordedTrace
  || !freshProcessReplayReady;
const blockKartGenericAuthoringHits = sourceRegexHits(
  blockKartVoFiles.map((entry) => ({ path: entry.rel, source: entry.source })),
  /ProductPrimitiveAuthoring|ProductPrimitive(Place|Dynamic|SetPose)|ProductPrimitives\.(Place|Dynamic|BuildLayer|Material|Shape|Layer)|primitive3d\.(NewLayer|NewBuilder|LayerDesc|ChunkingDesc|MaterialDesc)|ProductSpawnTrackColliderStrip|PackWriter|vopack\./,
).filter((hit) => !hit.path.startsWith('runtimepack/'));
const blockKartNamedAuthoringHits = sourceRegexHits(
  blockKartVoFiles.map((entry) => ({ path: entry.rel, source: entry.source })),
  /NewBlockKartPrimitiveContent|PrepareBlockKartMapAsset|SpawnBlockKartMap|SpawnBlockKartRoadsideBakedContent|SetBlockKartPrimitivePose|attachPrimitiveTrackColliderEntities|spawnBlockKartTrackCollider|primitiveTerrainSurfacePosition/,
);
const blockKartDirectIntentBypassHits = sourceRegexHits(
  blockKartVoFiles.map((entry) => ({ path: entry.rel, source: entry.source })),
  /UpdateIntentFromSyncedState\s*\(|\b[A-Za-z_][A-Za-z0-9_]*\.UpdateIntent\s*\(/,
);
const blockKartRuntimeContextBody = bodyOfType(blockKartWorld, 'BlockKartRuntimeContext');
const blockKartWorldFields = bodyOfType(blockKartWorld, 'World')
  .split(/\r?\n/)
  .map((line) => line.trim())
  .filter((line) => /^[A-Za-z_][A-Za-z0-9_]*\s+/.test(line));
const blockKartWorldAllowedGroups = new Set([
  'core BlockKartRuntimeCore',
  'input BlockKartRuntimeInputState',
  'race BlockKartRuntimeRaceState',
  'kart BlockKartRuntimeKartState',
  'hud BlockKartRuntimeHudState',
]);
const blockKartRuntimeContextFields = blockKartRuntimeContextBody
  .split(/\r?\n/)
  .map((line, index) => ({ line: index + 1, text: line.trim() }))
  .filter((entry) => /^[A-Za-z_][A-Za-z0-9_]*\s+/.test(entry.text));
const blockKartRuntimeContextAllowedGroups = new Set([
  'core BlockKartRuntimeCore',
  'input BlockKartRuntimeInputState',
  'race BlockKartRuntimeRaceState',
  'kart BlockKartRuntimeKartState',
  'hud BlockKartRuntimeHudState',
]);
const blockKartRuntimeContextForbiddenFields = blockKartRuntimeContextFields.filter((entry) => (
  !blockKartRuntimeContextAllowedGroups.has(entry.text)
  && /\b(scene|camera|player|vehicle|kartController|racingInput|touch|vehicleAudio|assets|primitive|track|checkpoint|raceState|courseTime|finishTime|collected|lap|kart|boost|drift|collectibles|checkpoints|boostPads|obstacles|debugHud|perf|physics)/i.test(entry.text)
));
const blockKartOwnerRuntimeContextHits = sourceRegexHits(
  blockKartVoFiles.map((entry) => ({ path: entry.rel, source: entry.source })),
  /^func \([^)]*\*(RaceSession|KartRig|TrackRuntime|HUDPresenter|PerfReporter|AssetRuntimeCache)\) [A-Za-z_][A-Za-z0-9_]*\([^)]*\*BlockKartRuntimeContext/,
);
const malformedPacketBranchFailures = packetDecodeBranchFailures(sceneSource, physicsWorld);
const packetErrorContractReady = physics.includes('type PhysicsBackendPacketError')
  && sceneSource.includes('PhysicsBackendPacketError')
  && physicsStressSource.includes('PacketErrors');
const publicForceBypassHits = sourceRegexHits(
  [{ path: 'scene3d/vehicle.vo', source: vehicle }],
  /^func \(v \*Vehicle\) ApplyForceCommand\(/,
);
const rawPhysicsBypassHits = sourceRegexHits(
  [{ path: 'scene3d/physics_commands.vo', source: physicsCommands }],
  /^func \(e \*Entity\) (SetPosition|SetVelocity|SetAngularVelocity)\(/,
).filter((hit) => !bodyOfFunction(physicsCommands, hit.text.replace(/\s*\{\s*$/, '')).includes('ApplyPhysicsTarget(PhysicsBackendTargetCommand'));
const productPhysicsBypassHits = sourceRegexHits(
  [{ path: 'scene3d/scene.vo', source: sceneSource }],
  /^func ProductStepAndSyncPhysics\(/,
);
const constraintBypassHits = [
  ...sourceRegexHits([{ path: 'scene3d/physics_commands.vo', source: physicsCommands }], /\b(ApplyEntityPhysicsTarget|ApplyEntityPhysicsConstraint)\(/),
  ...sourceRegexHits([{ path: 'scene3d/vehicle_road_edge_assist.vo', source: readText(path.join(voplayRoot, 'scene3d/vehicle_road_edge_assist.vo')) || '' }], /\bvehicleBackendConstraintCommand\b/),
];
const physicsStressCommandFailureCountersReady = physicsStressSource.includes('PacketErrors')
  && physicsStressSource.includes('RejectedBackendCommands')
  && physicsStressSource.includes('DroppedFixedSteps')
  && physicsStressSource.includes('PhysicsRejectedCommandCount');
const acceptedVolangPlanCommits = acceptedVolangPlanSnapshotCommits(root);
runActivePlanSnapshotContractSelftest();
const planSnapshotContract = activePlanSnapshotContract({
  planText: activeQualityPlanSource,
  projectText: projectToml,
  rootPath: root,
  acceptedVolangCommits: acceptedVolangPlanCommits,
});
const planSnapshot = planSnapshotContract.snapshot;
const activePlanSnapshotFresh = planSnapshotContract.issues.length === 0;

addCheck('phase-0', 'evidence.ci_run_id_present', Boolean(currentCiRunId), 'industrial readiness must run through vo-dev with a shared CI run id', { currentCiRunId });
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
addCheck('phase-1', 'framegraph.resource_registry_exists', frameGraphAuditSource.includes('struct RenderResourceRegistry'), 'RenderResourceRegistry exists', {});
addCheck('phase-1', 'renderer.targets_owned_by_registry', !rendererStillOwnsTargets, 'renderer target lifecycle is owned by RenderResourceRegistry instead of ad hoc RendererTargetRegistry fields', {});

addCheck('phase-2', 'batch_plan.contract_exists', ['struct RenderBatchPlan', 'struct RenderBatchPlanner', 'struct RenderWorldChunk'].every((token) => renderWorld.includes(token)), 'RenderBatchPlan and RenderWorldChunk contracts exist', {});
addCheck('phase-2', 'batch_plan.drives_submission', !runtimeHasDirectDraw && rendererFrameWorkloadPlan.includes('RenderBatchPlanner::build') && rendererFrameWorkloadPlan.includes('planned_model_draws') && rendererFrameWorkloadPlan.includes('planned_primitive_draws') && rendererFrameWorkloadPlan.includes('planned_water_draws') && rendererFrameWorkloadPlan.includes('planned_projected_decals') && rendererFrameWorkloadPlan.includes('terrain_batch_inputs') && rendererFrameWorkloadPlan.includes('decal_batch_inputs'), 'RenderBatchPlan owns real draw submission routing', {});
addCheck('phase-2', 'pipeline3d.file_size', pipeline3dLines <= 900, 'pipeline3d.rs is split into small stable responsibilities', { lines: pipeline3dLines, budget: 900 });
const splitPipelineFiles = [
  'rust/src/pipeline3d/pipeline_cache.rs',
  'rust/src/pipeline3d/pipeline_factory.rs',
  'rust/src/pipeline3d/mesh_submitter.rs',
  'rust/src/pipeline3d/decal_submitter.rs',
];
addCheck('phase-2', 'pipeline3d.split_modules_exist', splitPipelineFiles.every((file) => existsSync(path.join(voplayRoot, file))), 'pipeline3d responsibilities are split into dedicated modules', { expected: splitPipelineFiles });

const backendAdapterBody = bodyOfFunction(vehicle, 'func (v *Vehicle) applyForceCommandToBackend');
addCheck('phase-3', 'physics.intent_chain', vehicle.includes('v.Dynamics.Step(') && dynamics.includes('BuildPhysicsBackendApplyCommand') && vehicle.includes('buildPhysicsBackendApplyCommandWithNormalizedConfig') && vehicle.includes('scene.physicsWorld.backend.ApplyVehicleForces'), 'VehicleIntent reaches KartDynamics, PhysicsBackendApplyCommand, and PhysicsBackend.ApplyVehicleForces', {});
addCheck('phase-3', 'physics.backend_command_contract', ['Wheels []PhysicsBackendWheelCommand', 'BodyForce voplay.Vec3', 'DragForce float64', 'Downforce float64', 'WaterLift float64', 'AirControl float64', 'WallGrip float64', 'RailGrip float64', 'DebugHash int'].every((token) => dynamics.includes(token)), 'PhysicsBackendApplyCommand covers wheel, body, drag, downforce, water, air, wall, rail, and debug hash', {});
addCheck('phase-3', 'physics.backend_apply_contract', vehicle.includes('scene.physicsWorld.backend.ApplyVehicleForces') && !vehicle.includes('scene.physicsWorld.backend.SetRaycastVehicleWheelControl'), 'Vehicle applies wheel/backend forces only through PhysicsBackend.ApplyVehicleForces', {});
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
  'active_plan_snapshot_fresh',
  activePlanSnapshotFresh,
  'active voplay quality plan snapshot matches current Volang and all pinned sibling source commits',
  {
    owner: 'docs',
    file: 'lang/docs/dev/voplay-code-engineering-quality-plan.md',
    line: lineOf(activeQualityPlanSource, '当前 checkout snapshot') ?? 1,
    reason: 'readiness must reject stale active plan source snapshots',
    requiredFix: 'Update the active plan snapshot from current source commits and rerun docs-lint/readiness.',
    planSnapshot,
    expected: planSnapshotContract.expected,
    heads: planSnapshotContract.heads,
    acceptedVolangPlanCommits,
    issues: planSnapshotContract.issues,
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
  {
    owner: 'voplay/render',
    file: renderHotPathPanicHits[0]?.path ?? invalidBatchIndexSilentSkipHits[0]?.path ?? null,
    line: renderHotPathPanicHits[0]?.line ?? invalidBatchIndexSilentSkipHits[0]?.line ?? null,
    reason: 'render frame hot paths still contain panic-prone calls or silent invalid batch drops',
    requiredFix: 'Replace panic-prone render runtime calls with structured Result errors and keep invalid index drops in structured skip telemetry.',
    renderHotPathPanicHits,
    invalidBatchIndexSilentSkipHits,
  },
);
addRequiredSourceFact(
  'dirty_range_partial_upload_verified',
  dirtyRangeFullRebuildHits.length === 0 && dirtyRangePartialUploadVerified,
  'single-instance dirty updates use true partial resident instance uploads and do not rebuild the whole resident chunk',
  {
    owner: 'voplay/render',
    file: dirtyRangeFullRebuildHits[0]?.path ?? 'rust/src/primitive_pipeline/runtime.rs',
    line: dirtyRangeFullRebuildHits[0]?.line ?? null,
    reason: 'resident dirty range still falls through to full chunk rebuild or lacks partial-upload behavior proof',
    requiredFix: 'Implement partial resident instance buffer uploads with offset/length tied to the dirty range, and add a unit/stress report proving fullRebuildCount=0 for one-instance updates.',
    dirtyRangeFullRebuildHits,
    dirtyRangePartialUploadVerified,
  },
);
addRequiredSourceFact(
  'render_pass_context_narrow',
  renderPassContextWideHits.length === 0 && renderPassContextNegativeFixtureRejected,
  'render pass contexts expose only explicit pass resources and cannot bypass FrameGraph through large resource bundles',
  {
    owner: 'voplay/render',
    file: renderPassContextWideHits[0]?.path ?? 'rust/src/renderer/pass_dispatch.rs',
    line: renderPassContextWideHits[0]?.line ?? null,
    reason: 'RenderPassResources still exposes broad device, queue, pipelines, managers, and render_world access',
    requiredFix: 'Split pass contexts into narrow read/write contracts per RenderPassKind and remove broad resource bundles from pass dispatch.',
    renderPassContextWideHits: renderPassContextWideHits.slice(0, 40),
    renderPassContextNegativeFixtureRejected,
  },
);
addRequiredSourceFact(
  'resource_registry_backing_generation_agrees',
  resourceRegistryBackingAgreement,
  'FrameGraph resource readiness agrees with actual backing target generation and view identity',
  {
    owner: 'voplay/render',
    file: 'rust/src/renderer_frame.rs',
    line: lineOf(frameGraph, 'struct RenderTargetStatus'),
    reason: 'RenderResourceRegistry records logical readiness without actual texture/view identity and backing-generation agreement',
    requiredFix: 'Store backing generation and actual target/view identity in the resource registry, then validate plan readiness against the live backing target before pass execution.',
    resourceRegistryBackingAgreement,
  },
);
addRequiredSourceFact(
  'resource_registry_owns_all_targets',
  resourceRegistryOwnsAllTargetKinds && !rendererStillOwnsTargets,
  'RenderResourceRegistry owns surface/depth/main/post/shadow/water/overlay/capture/readback lifecycle',
  {
    registry: lineEvidence(path.join(voplayRoot, 'rust/src/renderer_frame/resource_registry.rs'), frameGraphResourceRegistry, 'struct RenderResourceRegistry'),
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
  !sessionIsWrapperOnly && !physicsStressBypassesSession && !physicsToolsBypassSession,
  'VehiclePhysicsSession is the fixed-step runtime authority and stress/tools do not bypass it with direct scene/controller stepping',
  {
    sessionIsWrapperOnly,
    physicsStressBypassesSession,
    physicsToolsBypassSession,
    sessionStep: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle_physics_session.vo'), vehiclePhysicsSession, 'func (s *VehiclePhysicsSession) Step'),
    stressDirectSceneStep: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'StepAndSyncPhysics'),
    stressUpdateIntent: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'UpdateIntent'),
    toolDirectSceneStep: lineEvidence(path.join(voplayRoot, 'tools/vehicle_telemetry_parity.vo'), voplayToolSources, 'StepAndSyncPhysics'),
    toolUpdateIntent: lineEvidence(path.join(voplayRoot, 'tools/vehicle_telemetry_parity.vo'), voplayToolSources, 'UpdateIntent'),
  },
);
addRequiredSourceFact(
  'physics_backend_packet_schema_contract',
  backendPacketMissingTokens.length === 0
    && backendPacketMissingEnforcementTokens.length === 0
    && !backendPacketStillCountDelimited,
  'backend packets expose and enforce schema version, packet kind, byte length, packet hash, and capability contract identifiers',
  { backendPacketMissingTokens, backendPacketMissingEnforcementTokens, backendPacketStillCountDelimited, expectedTokens: backendPacketSchemaTokens },
);
addRequiredSourceFact(
  'physics_malformed_packets_are_structured_errors',
  malformedPacketBranchFailures.length === 0 && packetErrorContractReady,
  'malformed body/contact backend packets produce structured PhysicsBackendPacketError telemetry and stress evidence',
  {
    owner: 'voplay/scene3d',
    file: malformedPacketBranchFailures[0]?.path ?? 'scene3d/scene.vo',
    line: malformedPacketBranchFailures[0]?.line ?? null,
    reason: 'malformed backend body/contact packets can still return silently without structured error telemetry',
    requiredFix: 'Add PhysicsBackendPacketError with fileable reason codes, route body/contact decode failures into scene/session telemetry, and require packetErrors in physics stress.',
    malformedPacketBranchFailures: malformedPacketBranchFailures.slice(0, 40),
    packetErrorContractReady,
  },
);
addRequiredSourceFact(
  'physics_no_public_or_raw_bypass',
  publicForceBypassHits.length === 0 && rawPhysicsBypassHits.length === 0 && productPhysicsBypassHits.length === 0,
  'public vehicle force and raw entity physics mutation bypasses are internalized into the backend contract or blocked by industrial gate',
  {
    owner: 'voplay/scene3d',
    file: publicForceBypassHits[0]?.path ?? rawPhysicsBypassHits[0]?.path ?? productPhysicsBypassHits[0]?.path ?? 'scene3d/vehicle.vo',
    line: publicForceBypassHits[0]?.line ?? rawPhysicsBypassHits[0]?.line ?? productPhysicsBypassHits[0]?.line ?? null,
    reason: 'public/raw physics APIs can bypass VehiclePhysicsSession and backend contract authority',
    requiredFix: 'Move ApplyForceCommand and raw Entity Set* commands behind backend contract adapters or mark them debug/compat with industrial gate exclusions.',
    publicForceBypassHits,
    rawPhysicsBypassHits,
    productPhysicsBypassHits,
  },
);
addRequiredSourceFact(
  'physics_no_constraint_bypass',
  constraintBypassHits.length === 0,
  'constraint, road-edge assist, reset, recovery, and sleep mutations flow through backend contract commands and replay hashes',
  {
    owner: 'voplay/scene3d',
    file: constraintBypassHits[0]?.path ?? 'scene3d/physics_commands.vo',
    line: constraintBypassHits[0]?.line ?? null,
    reason: 'constraint helpers can still mutate entity position or velocity through raw physics commands',
    requiredFix: 'Promote road-edge assist and entity constraints into explicit backend contract commands included in telemetry and replay hashes.',
    constraintBypassHits: constraintBypassHits.slice(0, 40),
  },
);
addRequiredSourceFact(
  'physics_invalid_telemetry_is_not_sanitized_away',
  !invalidTelemetryStressBypass && !invalidWheelPacketSanitized,
  'industrial physics stress reports invalid raw samples and validation issues without accepting cleaned samples as pass evidence',
  {
    invalidTelemetryStressBypass,
    invalidWheelPacketSanitized,
    invalidSample: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'InvalidSampleCount'),
    validationIssues: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'ValidationIssues'),
    sampleFinite: lineEvidence(path.join(voplayRoot, 'examples/physics_stress/main.vo'), physicsStressSource, 'sampleFinite'),
    rawWheelInvalid: lineEvidence(path.join(voplayRoot, 'scene3d/vehicle.vo'), vehicle, 'RawInvalidSampleCount'),
  },
);
addRequiredSourceFact(
  'physics_replay_is_executable_step_contract',
  executableReplayMissingTokens.length === 0 && !sameRuntimeReplayDriftOnly,
  'physics replay validation executes a recorded step contract with per-step hashes and mismatch evidence',
  { executableReplayMissingTokens, replayUsesRecordedTrace, freshProcessReplayReady, sameRuntimeReplayDriftOnly },
);
addRequiredSourceFact(
  'physics_stress_reports_packet_and_command_failure_counters',
  physicsStressCommandFailureCountersReady,
  'industrial physics stress reports packet errors and rejected backend commands with zero-tolerance gates',
  {
    owner: 'voplay/scene3d',
    file: 'examples/physics_stress/main.vo',
    line: lineOf(physicsStressSource, 'type ScenarioReport struct'),
    reason: 'physics stress report lacks packet error or rejected backend command fields and gates',
    requiredFix: 'Emit packet errors and rejected backend commands from every scenario and fail industrial stress when either counter is nonzero.',
    physicsStressCommandFailureCountersReady,
  },
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
  'blockkart_no_named_authoring_wrappers',
  blockKartNamedAuthoringHits.length === 0,
  'BlockKart does not preserve generic primitive, map, collider, or dynamic visual authoring behind BlockKart-named runtime wrappers',
  {
    namedAuthoringHits: blockKartNamedAuthoringHits.slice(0, 40),
  },
);
addRequiredSourceFact(
  'blockkart_vehicle_session_is_runtime_authority',
  blockKartDirectIntentBypassHits.length === 0,
  'BlockKart default runtime sends vehicle intent through VehiclePhysicsSession and does not update controller/vehicle intent directly',
  {
    directIntentBypassHits: blockKartDirectIntentBypassHits.slice(0, 40),
    stepPhysics: lineEvidence(path.join(blockKartRoot, 'runtime_owners.vo'), readText(path.join(blockKartRoot, 'runtime_owners.vo')) || '', 'func (k *KartRig) StepPhysics'),
  },
);
addRequiredSourceFact(
  'blockkart_runtime_context_not_mega_owner',
  blockKartRuntimeContextBody.trim() === ''
    && blockKartRuntimeContextFields.length === 0
    && blockKartWorldFields.length === blockKartWorldAllowedGroups.size
    && blockKartWorldFields.every((entry) => blockKartWorldAllowedGroups.has(entry))
    && blockKartOwnerRuntimeContextHits.length === 0,
  'BlockKartRuntimeContext is removed, World contains only explicit owner state groups, and owner methods do not mutate through a broad runtime context',
  {
    fieldCount: blockKartRuntimeContextFields.length,
    forbiddenFields: blockKartRuntimeContextForbiddenFields.slice(0, 40),
    ownerRuntimeContextHits: blockKartOwnerRuntimeContextHits.slice(0, 40),
    worldFields: blockKartWorldFields,
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
const quickplayProject = readJson(path.join(
  root,
  'apps/studio/public/quickplay/blockkart/project.json',
));
const quickplayProjectOutput = (provenance.value?.outputs ?? [])
  .find((entry) => entry?.path === 'project.json') ?? null;
const quickplayVpakFile = (quickplayProject.value?.files ?? [])
  .find((entry) => entry?.path === 'assets/blockkart.vpak') ?? null;
const quickplayProducerProvenanceReady =
  /^sha256:[0-9a-f]{64}$/.test(quickplayProjectOutput?.digest ?? '')
  && Number.isSafeInteger(quickplayProjectOutput?.size)
  && /^sha256:[0-9a-f]{64}$/.test(quickplayVpakFile?.digest ?? '')
  && Number.isSafeInteger(quickplayVpakFile?.size)
  && quickplayVpakFile.size > 0;
addRequiredSourceFact(
  'quickplay_vpak_terrain_producer_provenance',
  quickplayProducerProvenanceReady,
  'quickplay project output and embedded assets/blockkart.vpak carry exact byte bindings',
  {
    owner: 'studio/artifacts',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    line: provenance.exists ? lineOf(readText(provenancePath) || '', '"outputs"') ?? 1 : 1,
    reason: 'readiness requires the generated project and runtime pack to be byte-bound',
    requiredFix: 'Regenerate the Quickplay vNext package and its output facts.',
    projectOutput: quickplayProjectOutput,
    vpak: quickplayVpakFile,
  },
);
const expectedBlockKartDigest = provenance.value?.sourceDigests?.['github.com/vo-lang/blockkart'] ?? null;
const expectedVoplayDigest = provenance.value?.sourceDigests?.['github.com/vo-lang/voplay'] ?? null;
const quickplaySourceDigestIssues = [];
if (!provenance.exists || provenance.error) {
  quickplaySourceDigestIssues.push({ field: 'provenance', issue: provenance.error ?? 'missing' });
}
if (!/^[0-9a-f]{40}$/.test(quickplayProject.value?.baseCommit ?? '')) {
  quickplaySourceDigestIssues.push({ field: 'project.baseCommit', issue: 'invalid Git base commit' });
}
const quickplaySourceDigests = provenance.value?.sourceDigests;
if (
  !quickplaySourceDigests
  || typeof quickplaySourceDigests !== 'object'
  || Array.isArray(quickplaySourceDigests)
  || Object.keys(quickplaySourceDigests).length === 0
) {
  quickplaySourceDigestIssues.push({ field: 'provenance.sourceDigests', issue: 'missing or empty' });
} else {
  for (const [module, digest] of Object.entries(quickplaySourceDigests)) {
    if (!module || !/^sha256:[0-9a-f]{64}$/.test(digest ?? '')) {
      quickplaySourceDigestIssues.push({ field: `provenance.sourceDigests.${module}`, issue: 'invalid sha256 binding' });
    }
  }
}
if (!quickplayProducerProvenanceReady) {
  quickplaySourceDigestIssues.push({ field: 'project.files.assets/blockkart.vpak', issue: 'missing exact byte binding' });
}

function baselineSourceMismatches(report) {
  const mismatches = [];
  if (!expectedBlockKartDigest || report.project?.sourceDigest !== expectedBlockKartDigest) {
    mismatches.push({
      module: 'github.com/vo-lang/blockkart',
      expected: expectedBlockKartDigest,
      sourceDigest: report.project?.sourceDigest ?? null,
    });
  }
  const voplay = (report.dependencies ?? []).find((dep) => dep.module === 'github.com/vo-lang/voplay') ?? null;
  if (!expectedVoplayDigest || voplay?.sourceDigest !== expectedVoplayDigest) {
    mismatches.push({
      module: 'github.com/vo-lang/voplay',
      expected: expectedVoplayDigest,
      sourceDigest: voplay?.sourceDigest ?? null,
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

function canonicalTimestampMs(value) {
  if (typeof value !== 'string') {
    return null;
  }
  const parsed = Date.parse(value);
  return Number.isFinite(parsed) && new Date(parsed).toISOString() === value ? parsed : null;
}

function rendererReadinessContract(
  readiness,
  {
    requireCausalEpoch = false,
    reportStartedAtMs = null,
    reportGeneratedAtMs = null,
  } = {},
) {
  const failures = [];
  const reject = (reason, actual) => failures.push({ reason, actual });
  const record = readiness?.record;
  const payload = record?.payload;
  const notBeforeMs = canonicalTimestampMs(readiness?.notBefore);
  const receivedAtMs = canonicalTimestampMs(record?.receivedAt);
  if (readiness == null || typeof readiness !== 'object' || Array.isArray(readiness)) {
    reject('invalid', readiness ?? null);
  }
  if (record == null || typeof record !== 'object' || Array.isArray(record)) {
    reject('record_invalid', record ?? null);
  }
  if (readiness?.ready !== true) {
    reject('not_ready', readiness?.ready ?? null);
  }
  if (readiness?.source !== 'voplay-perf-endpoint') {
    reject('source_invalid', readiness?.source ?? null);
  }
  if (!Number.isSafeInteger(readiness?.frame) || readiness.frame < 0) {
    reject('frame_invalid', readiness?.frame ?? null);
  }
  if (!Number.isSafeInteger(readiness?.reportCount) || readiness.reportCount < 1) {
    reject('report_count_invalid', readiness?.reportCount ?? null);
  }
  if (readiness?.failure !== null) {
    reject('failure_present', readiness?.failure ?? '(missing)');
  }
  if (notBeforeMs === null) {
    reject('not_before_invalid', readiness?.notBefore ?? null);
  }
  if (receivedAtMs === null) {
    reject('received_at_invalid', record?.receivedAt ?? null);
  } else if (notBeforeMs !== null && receivedAtMs < notBeforeMs) {
    reject('record_stale', {
      notBefore: readiness.notBefore,
      receivedAt: record.receivedAt,
    });
  }
  if (receivedAtMs !== null
    && Number.isFinite(reportStartedAtMs)
    && Number.isFinite(reportGeneratedAtMs)
    && (receivedAtMs < reportStartedAtMs || receivedAtMs > reportGeneratedAtMs)) {
    reject('record_outside_report_window', {
      reportStartedAt: new Date(reportStartedAtMs).toISOString(),
      reportGeneratedAt: new Date(reportGeneratedAtMs).toISOString(),
      receivedAt: record.receivedAt,
    });
  }
  const payloadValid = payload?.source === 'blockkart'
    && payload?.kind === 'perf-summary'
    && Number.isSafeInteger(payload?.frame)
    && payload.frame >= 0;
  if (!payloadValid) {
    reject('record_payload_invalid', {
      source: payload?.source ?? null,
      kind: payload?.kind ?? null,
      frame: payload?.frame ?? null,
    });
  } else if (Number.isSafeInteger(readiness?.frame) && payload.frame !== readiness.frame) {
    reject('frame_mismatch', {
      readinessFrame: readiness.frame,
      recordFrame: payload.frame,
    });
  }
  if (!Number.isSafeInteger(payload?.studioSessionId) || payload.studioSessionId < 1) {
    reject('record_session_invalid', payload?.studioSessionId ?? null);
  }
  if (!Number.isSafeInteger(payload?.studioPerfEpoch) || payload.studioPerfEpoch < 0) {
    reject('record_epoch_invalid', payload?.studioPerfEpoch ?? null);
  }
  if (!isBlockKartPerfFrameRecord(record, readiness?.notBefore)) {
    reject('record_metrics_invalid', {
      screen: payload?.screen ?? null,
      window: payload?.window ?? null,
      current: payload?.current ?? null,
      renderer: payload?.renderer ?? null,
    });
  }
  if (requireCausalEpoch
    && (!Number.isSafeInteger(readiness?.expectedPerfEpoch) || readiness.expectedPerfEpoch < 1)) {
    reject('expected_epoch_invalid', readiness?.expectedPerfEpoch ?? null);
  }
  if (requireCausalEpoch && payload?.studioPerfEpoch !== readiness?.expectedPerfEpoch) {
    reject('epoch_mismatch', {
      expected: readiness?.expectedPerfEpoch ?? null,
      actual: payload?.studioPerfEpoch ?? null,
    });
  }
  if (requireCausalEpoch
    && (!Number.isSafeInteger(readiness?.minimumFrameExclusive)
      || readiness.minimumFrameExclusive < 1)) {
    reject('minimum_frame_invalid', readiness?.minimumFrameExclusive ?? null);
  }
  if (requireCausalEpoch
    && Number.isSafeInteger(readiness?.frame)
    && Number.isSafeInteger(readiness?.minimumFrameExclusive)
    && readiness.frame <= readiness.minimumFrameExclusive) {
    reject('frame_not_advanced', {
      frame: readiness.frame,
      minimumFrameExclusive: readiness.minimumFrameExclusive,
    });
  }
  return {
    ok: failures.length === 0,
    failures,
    evidence: {
      ready: readiness?.ready ?? null,
      source: readiness?.source ?? null,
      frame: readiness?.frame ?? null,
      reportCount: readiness?.reportCount ?? null,
      failure: readiness?.failure ?? null,
      notBefore: readiness?.notBefore ?? null,
      receivedAt: record?.receivedAt ?? null,
      fresh: notBeforeMs !== null
        && receivedAtMs !== null
        && receivedAtMs >= notBeforeMs,
      insideReportWindow: receivedAtMs !== null
        && Number.isFinite(reportStartedAtMs)
        && Number.isFinite(reportGeneratedAtMs)
        && receivedAtMs >= reportStartedAtMs
        && receivedAtMs <= reportGeneratedAtMs,
      recordSource: payload?.source ?? null,
      recordKind: payload?.kind ?? null,
      recordFrame: payload?.frame ?? null,
      studioSessionId: payload?.studioSessionId ?? null,
      studioPerfEpoch: payload?.studioPerfEpoch ?? null,
      expectedPerfEpoch: readiness?.expectedPerfEpoch ?? null,
      minimumFrameExclusive: readiness?.minimumFrameExclusive ?? null,
    },
  };
}

export function validateBlockKartBaselineReport(report, { expectedRestartCount = null } = {}) {
  if (expectedRestartCount !== null && (!Number.isSafeInteger(expectedRestartCount) || expectedRestartCount < 0)) {
    throw new RangeError('expectedRestartCount must be a non-negative safe integer or null');
  }

  const acceptanceFailures = [];
  const reject = (code, actual) => acceptanceFailures.push({ code, actual });
  const isPositiveSafeInteger = (value) => Number.isSafeInteger(value) && value > 0;
  const reportStartedAtMs = canonicalTimestampMs(report?.startedAt);
  const reportGeneratedAtMs = canonicalTimestampMs(report?.generatedAt);
  const visualCaptureCompletedAtMs = canonicalTimestampMs(report?.visualCaptureCompletedAt);
  if (reportStartedAtMs === null) {
    reject('report.started_at_invalid', report?.startedAt ?? null);
  }
  if (reportGeneratedAtMs === null) {
    reject('report.generated_at_invalid', report?.generatedAt ?? null);
  }
  if (reportStartedAtMs !== null
    && reportGeneratedAtMs !== null
    && reportStartedAtMs > reportGeneratedAtMs) {
    reject('report.time_order_invalid', {
      startedAt: report.startedAt,
      generatedAt: report.generatedAt,
    });
  }
  if (visualCaptureCompletedAtMs === null) {
    reject('visual_capture.completed_at_invalid', report?.visualCaptureCompletedAt ?? null);
  } else if (reportStartedAtMs !== null
    && reportGeneratedAtMs !== null
    && (visualCaptureCompletedAtMs < reportStartedAtMs
      || visualCaptureCompletedAtMs > reportGeneratedAtMs)) {
    reject('visual_capture.completed_at_outside_report_window', {
      startedAt: report.startedAt,
      visualCaptureCompletedAt: report.visualCaptureCompletedAt,
      generatedAt: report.generatedAt,
    });
  }
  const policy = report?.policy;
  if (policy?.failOnIssues !== true) {
    reject('policy.fail_on_issues_disabled', policy?.failOnIssues ?? null);
  }
  if (policy?.requireWebGpuAdapter !== true) {
    reject('policy.webgpu_not_required', policy?.requireWebGpuAdapter ?? null);
  }
  if (policy?.visualCaptureEnabled !== true) {
    reject('policy.visual_capture_disabled', policy?.visualCaptureEnabled ?? null);
  }
  if (policy?.expectedLifecycleState !== 'Running') {
    reject('policy.lifecycle_not_running', policy?.expectedLifecycleState ?? null);
  }
  if (report?.status !== 'ok') {
    reject('status.not_ok', report?.status ?? null);
  }
  if (report?.firstFrame?.ok !== true) {
    reject('first_frame.not_ok', report?.firstFrame?.ok ?? null);
  }
  if (report?.firstFrame?.skipped !== false) {
    reject('first_frame.skipped', report?.firstFrame?.skipped ?? null);
  }
  if (report?.lifecycle?.state !== 'Running') {
    reject('lifecycle.not_running', report?.lifecycle?.state ?? null);
  }
  if (report?.lifecycle?.reachedRunning !== true) {
    reject('lifecycle.running_not_reached', report?.lifecycle?.reachedRunning ?? null);
  }
  if (report?.visual?.enabled !== true) {
    reject('visual.not_enabled', report?.visual?.enabled ?? null);
  }
  if (report?.visual?.viewport?.nonEmpty !== true) {
    reject('visual.viewport_empty', report?.visual?.viewport?.nonEmpty ?? null);
  }
  if (report?.visual?.canvas?.nonEmpty !== true) {
    reject('visual.canvas_empty', report?.visual?.canvas?.nonEmpty ?? null);
  }

  const webGpu = report?.webGpu;
  if (webGpu?.probeExecutedInPage !== true
    || webGpu?.supported !== true
    || webGpu?.adapter !== true) {
    reject('webgpu.adapter_unverified', webGpu?.adapter ?? null);
  }

  const rendererReadiness = report?.rendererReadiness;
  const rendererReadinessVerdict = rendererReadinessContract(rendererReadiness, {
    reportStartedAtMs,
    reportGeneratedAtMs,
  });
  for (const failure of rendererReadinessVerdict.failures) {
    reject(`renderer_readiness.${failure.reason}`, failure.actual);
  }
  const finalRendererReadiness = report?.finalRendererReadiness;
  const finalRendererReadinessVerdict = rendererReadinessContract(
    finalRendererReadiness,
    {
      requireCausalEpoch: true,
      reportStartedAtMs,
      reportGeneratedAtMs,
    },
  );
  for (const failure of finalRendererReadinessVerdict.failures) {
    reject(`final_renderer_readiness.${failure.reason}`, failure.actual);
  }
  const initialReadinessReceivedAtMs = canonicalTimestampMs(rendererReadiness?.record?.receivedAt);
  const finalReadinessNotBeforeMs = canonicalTimestampMs(finalRendererReadiness?.notBefore);
  if (finalRendererReadinessVerdict.evidence.expectedPerfEpoch
    <= rendererReadinessVerdict.evidence.studioPerfEpoch) {
    reject('final_renderer_readiness.epoch_not_advanced', {
      initial: rendererReadinessVerdict.evidence.studioPerfEpoch,
      final: finalRendererReadinessVerdict.evidence.expectedPerfEpoch,
    });
  }
  if (finalRendererReadiness?.minimumFrameExclusive !== rendererReadiness?.frame
    || finalRendererReadiness?.frame <= rendererReadiness?.frame) {
    reject('final_renderer_readiness.frame_not_advanced_from_initial', {
      initialFrame: rendererReadiness?.frame ?? null,
      minimumFrameExclusive: finalRendererReadiness?.minimumFrameExclusive ?? null,
      finalFrame: finalRendererReadiness?.frame ?? null,
    });
  }
  if (initialReadinessReceivedAtMs === null
    || finalReadinessNotBeforeMs === null
    || finalReadinessNotBeforeMs < initialReadinessReceivedAtMs) {
    reject('final_renderer_readiness.causal_time_order_invalid', {
      initialReceivedAt: rendererReadiness?.record?.receivedAt ?? null,
      finalNotBefore: finalRendererReadiness?.notBefore ?? null,
    });
  }
  if (visualCaptureCompletedAtMs === null
    || finalReadinessNotBeforeMs === null
    || finalReadinessNotBeforeMs < visualCaptureCompletedAtMs) {
    reject('final_renderer_readiness.before_visual_capture', {
      visualCaptureCompletedAt: report?.visualCaptureCompletedAt ?? null,
      finalNotBefore: finalRendererReadiness?.notBefore ?? null,
    });
  }

  const runtimeState = report?.runtimeState;
  const runtimeLastErrorOwnProperty = runtimeState != null
    && typeof runtimeState === 'object'
    && Object.hasOwn(runtimeState, 'lastError');
  if (runtimeState?.status !== 'ready') {
    reject('runtime_state.status_not_ready', runtimeState?.status ?? null);
  }
  if (runtimeState?.kind !== 'gui') {
    reject('runtime_state.kind_not_gui', runtimeState?.kind ?? null);
  }
  if (runtimeState?.isRunning !== true) {
    reject('runtime_state.not_running', runtimeState?.isRunning ?? null);
  }
  if (runtimeState?.lastErrorPresent !== true) {
    reject('runtime_state.last_error_presence_unverified', runtimeState?.lastErrorPresent ?? null);
  }
  if (!runtimeLastErrorOwnProperty || runtimeState.lastError !== null) {
    reject('runtime_state.last_error_not_null', runtimeLastErrorOwnProperty ? runtimeState.lastError : '(missing)');
  }
  if (!Number.isSafeInteger(runtimeState?.gui?.moduleBytesLength) || runtimeState.gui.moduleBytesLength < 1) {
    reject('runtime_state.module_bytes_invalid', runtimeState?.gui?.moduleBytesLength ?? null);
  }
  if (!Number.isSafeInteger(runtimeState?.gui?.renderBytesLength) || runtimeState.gui.renderBytesLength < 1) {
    reject('runtime_state.render_bytes_invalid', runtimeState?.gui?.renderBytesLength ?? null);
  }
  const runtimeSessionId = runtimeState?.gui?.sessionId;
  if (!isPositiveSafeInteger(runtimeSessionId)) {
    reject('runtime_state.session_id_invalid', runtimeSessionId ?? null);
  }
  if (rendererReadinessVerdict.evidence.studioSessionId !== runtimeSessionId) {
    reject('renderer_readiness.session_mismatch', {
      readinessSessionId: rendererReadinessVerdict.evidence.studioSessionId,
      runtimeSessionId: runtimeSessionId ?? null,
    });
  }
  if (finalRendererReadinessVerdict.evidence.studioSessionId !== runtimeSessionId) {
    reject('final_renderer_readiness.session_mismatch', {
      readinessSessionId: finalRendererReadinessVerdict.evidence.studioSessionId,
      runtimeSessionId: runtimeSessionId ?? null,
    });
  }

  const rendererState = report?.visual?.capture?.rendererState;
  const rendererEntries = rendererState?.renderers;
  if (rendererState?.active !== true) {
    reject('renderer_state.inactive', rendererState?.active ?? null);
  }
  if (!Array.isArray(rendererEntries) || rendererEntries.length < 1) {
    reject('renderer_state.renderers_empty', Array.isArray(rendererEntries) ? rendererEntries.length : null);
  }
  const renderersWithoutQuiesce = Array.isArray(rendererEntries)
    ? rendererEntries.flatMap((renderer, index) => renderer?.quiesceForCapture === true ? [] : [index])
    : [];
  if (renderersWithoutQuiesce.length !== 0) {
    reject('renderer_state.quiesce_unsupported', renderersWithoutQuiesce);
  }
  const rendererSessionId = rendererState?.sessionId;
  if (!isPositiveSafeInteger(rendererSessionId)) {
    reject('renderer_state.session_id_invalid', rendererSessionId ?? null);
  }

  const rendererQuiesce = report?.visual?.capture?.rendererQuiesce;
  const quiesceEntries = rendererQuiesce?.renderers;
  if (rendererQuiesce?.ok !== true) {
    reject('renderer_quiesce.not_ok', rendererQuiesce?.ok ?? null);
  }
  if (!isPositiveSafeInteger(rendererQuiesce?.stopped)) {
    reject('renderer_quiesce.stopped_invalid', rendererQuiesce?.stopped ?? null);
  }
  const quiesceSessionId = rendererQuiesce?.sessionId;
  if (!isPositiveSafeInteger(quiesceSessionId)) {
    reject('renderer_quiesce.session_id_invalid', quiesceSessionId ?? null);
  }
  if (!Array.isArray(quiesceEntries)) {
    reject('renderer_quiesce.renderers_invalid', quiesceEntries ?? null);
  } else if (!Array.isArray(rendererEntries) || quiesceEntries.length !== rendererEntries.length) {
    reject('renderer_quiesce.renderer_count_mismatch', {
      rendererCount: Array.isArray(rendererEntries) ? rendererEntries.length : null,
      quiesceEntryCount: quiesceEntries.length,
    });
  }
  const invalidQuiesceEntries = Array.isArray(quiesceEntries)
    ? quiesceEntries.flatMap((entry, index) => (
      entry?.quiesceForCapture === true && isPositiveSafeInteger(entry?.stopped)
        ? []
        : [{ index, quiesceForCapture: entry?.quiesceForCapture ?? null, stopped: entry?.stopped ?? null }]
    ))
    : [];
  if (invalidQuiesceEntries.length !== 0) {
    reject('renderer_quiesce.entry_invalid', invalidQuiesceEntries);
  }
  let quiesceStoppedSum = 0;
  let quiesceStoppedSumValid = Array.isArray(quiesceEntries) && invalidQuiesceEntries.length === 0;
  if (quiesceStoppedSumValid) {
    for (const entry of quiesceEntries) {
      const next = quiesceStoppedSum + entry.stopped;
      if (!Number.isSafeInteger(next)) {
        quiesceStoppedSumValid = false;
        break;
      }
      quiesceStoppedSum = next;
    }
  }
  if (!quiesceStoppedSumValid || rendererQuiesce?.stopped !== quiesceStoppedSum) {
    reject('renderer_quiesce.stopped_total_mismatch', {
      declared: rendererQuiesce?.stopped ?? null,
      entrySum: quiesceStoppedSumValid ? quiesceStoppedSum : null,
    });
  }
  if (!Array.isArray(rendererEntries)
    || !isPositiveSafeInteger(rendererQuiesce?.stopped)
    || rendererQuiesce.stopped < rendererEntries.length) {
    reject('renderer_quiesce.coverage_incomplete', {
      stopped: rendererQuiesce?.stopped ?? null,
      rendererCount: Array.isArray(rendererEntries) ? rendererEntries.length : null,
    });
  }
  const rendererSessionsAligned = isPositiveSafeInteger(runtimeSessionId)
    && isPositiveSafeInteger(rendererSessionId)
    && isPositiveSafeInteger(quiesceSessionId)
    && runtimeSessionId === rendererSessionId
    && runtimeSessionId === quiesceSessionId
    && rendererReadinessVerdict.evidence.studioSessionId === runtimeSessionId
    && finalRendererReadinessVerdict.evidence.studioSessionId === runtimeSessionId;
  if (!rendererSessionsAligned) {
    reject('renderer_sessions.mismatch', {
      runtimeSessionId: runtimeSessionId ?? null,
      rendererSessionId: rendererSessionId ?? null,
      quiesceSessionId: quiesceSessionId ?? null,
      rendererReadinessSessionId: rendererReadinessVerdict.evidence.studioSessionId,
      finalRendererReadinessSessionId: finalRendererReadinessVerdict.evidence.studioSessionId,
    });
  }

  const perfReports = report?.perfReports;
  const blockKartPerfSummaries = Array.isArray(perfReports)
    ? perfReports.filter((perfReport) => perfReport?.source === 'blockkart' && perfReport?.kind === 'perf-summary')
    : [];
  const finiteBlockKartPerfSummaries = blockKartPerfSummaries.filter((perfReport) => Number.isFinite(perfReport?.frame));
  const foreignBlockKartPerfSummaries = blockKartPerfSummaries.filter((perfReport) => (
    perfReport?.studioSessionId !== runtimeSessionId
    || perfReport?.studioPerfEpoch !== finalRendererReadinessVerdict.evidence.expectedPerfEpoch
  ));
  const finalFramePerfSummaries = blockKartPerfSummaries.filter((perfReport) => (
    perfReport?.frame === finalRendererReadinessVerdict.evidence.frame
  ));
  if (!Array.isArray(perfReports)) {
    reject('perf_reports.invalid', perfReports ?? null);
  } else if (blockKartPerfSummaries.length === 0) {
    reject('perf_reports.blockkart_summary_missing', 0);
  } else if (finiteBlockKartPerfSummaries.length === 0) {
    reject('perf_reports.blockkart_summary_frame_invalid', blockKartPerfSummaries.map((perfReport) => perfReport?.frame ?? null));
  }
  if (foreignBlockKartPerfSummaries.length !== 0) {
    reject('perf_reports.authority_mismatch', foreignBlockKartPerfSummaries.map((perfReport) => ({
      frame: perfReport?.frame ?? null,
      studioSessionId: perfReport?.studioSessionId ?? null,
      studioPerfEpoch: perfReport?.studioPerfEpoch ?? null,
    })));
  }
  if (finalFramePerfSummaries.length === 0) {
    reject('perf_reports.final_frame_missing', finalRendererReadinessVerdict.evidence.frame);
  }

  const resourceFailures = report?.resourceFailures;
  if (!Array.isArray(resourceFailures)) {
    reject('resource_failures.invalid', resourceFailures ?? null);
  } else if (resourceFailures.length !== 0) {
    reject('resource_failures.present', resourceFailures.length);
  }

  const errors = report?.errors;
  if (!Array.isArray(errors)) {
    reject('errors.invalid', errors ?? null);
  } else if (errors.length !== 0) {
    reject('errors.present', errors.length);
  }

  const issues = report?.issues;
  const p0p1Issues = Array.isArray(issues)
    ? issues.filter((issue) => issue?.severity === 'P0' || issue?.severity === 'P1')
    : [];
  if (!Array.isArray(issues)) {
    reject('issues.invalid', issues ?? null);
  } else if (p0p1Issues.length !== 0) {
    reject('issues.p0_p1_present', p0p1Issues.length);
  }

  let restartEvidence = null;
  if (expectedRestartCount !== null) {
    const restart = report?.restart;
    const iterations = restart?.iterations;
    const badIterations = Array.isArray(iterations)
      ? iterations.flatMap((iteration, position) => iteration?.ok === true ? [] : [{
        position: position + 1,
        index: iteration?.index ?? null,
        ok: iteration?.ok ?? null,
      }])
      : [];
    if (restart?.requested !== expectedRestartCount) {
      reject('restart.requested_mismatch', restart?.requested ?? null);
    }
    if (restart?.completed !== expectedRestartCount) {
      reject('restart.completed_mismatch', restart?.completed ?? null);
    }
    if (restart?.skipped !== false) {
      reject('restart.skipped', restart?.skipped ?? null);
    }
    if (restart?.failure != null) {
      reject('restart.failure_present', restart.failure);
    }
    if (!Array.isArray(iterations)) {
      reject('restart.iterations_invalid', iterations ?? null);
    } else {
      if (iterations.length !== expectedRestartCount) {
        reject('restart.iteration_count_mismatch', iterations.length);
      }
      if (badIterations.length !== 0) {
        reject('restart.iteration_failed', badIterations.length);
      }
    }
    restartEvidence = {
      requested: restart?.requested ?? null,
      completed: restart?.completed ?? null,
      skipped: restart?.skipped ?? null,
      failure: restart?.failure ?? null,
      iterationCount: Array.isArray(iterations) ? iterations.length : null,
      badIterations,
      finalRendererReadiness: finalRendererReadinessVerdict.evidence,
    };
  }

  return {
    ok: acceptanceFailures.length === 0,
    detail: expectedRestartCount === null
      ? 'BlockKart baseline report satisfies strict runtime, renderer, and visual acceptance'
      : `BlockKart restart-${expectedRestartCount} report satisfies strict runtime, renderer, visual, and iteration acceptance`,
    evidence: {
      timestamps: {
        startedAt: report?.startedAt ?? null,
        visualCaptureCompletedAt: report?.visualCaptureCompletedAt ?? null,
        generatedAt: report?.generatedAt ?? null,
        canonicalAndOrdered: reportStartedAtMs !== null
          && visualCaptureCompletedAtMs !== null
          && reportGeneratedAtMs !== null
          && reportStartedAtMs <= visualCaptureCompletedAtMs
          && visualCaptureCompletedAtMs <= reportGeneratedAtMs,
      },
      policy: {
        failOnIssues: policy?.failOnIssues ?? null,
        requireWebGpuAdapter: policy?.requireWebGpuAdapter ?? null,
        visualCaptureEnabled: policy?.visualCaptureEnabled ?? null,
        expectedLifecycleState: policy?.expectedLifecycleState ?? null,
      },
      status: report?.status ?? null,
      firstFrame: {
        ok: report?.firstFrame?.ok ?? null,
        skipped: report?.firstFrame?.skipped ?? null,
      },
      lifecycle: {
        state: report?.lifecycle?.state ?? null,
        reachedRunning: report?.lifecycle?.reachedRunning ?? null,
      },
      visual: {
        enabled: report?.visual?.enabled ?? null,
        viewportNonEmpty: report?.visual?.viewport?.nonEmpty ?? null,
        canvasNonEmpty: report?.visual?.canvas?.nonEmpty ?? null,
      },
      webGpu: {
        probeExecutedInPage: webGpu?.probeExecutedInPage ?? null,
        supported: webGpu?.supported ?? null,
        adapter: webGpu?.adapter ?? null,
      },
      rendererReadiness: rendererReadinessVerdict.evidence,
      finalRendererReadiness: finalRendererReadinessVerdict.evidence,
      runtimeState: {
        status: runtimeState?.status ?? null,
        kind: runtimeState?.kind ?? null,
        isRunning: runtimeState?.isRunning ?? null,
        lastErrorPresent: runtimeState?.lastErrorPresent ?? null,
        lastErrorOwnProperty: runtimeLastErrorOwnProperty,
        lastError: runtimeLastErrorOwnProperty ? runtimeState.lastError : null,
        moduleBytesLength: runtimeState?.gui?.moduleBytesLength ?? null,
        renderBytesLength: runtimeState?.gui?.renderBytesLength ?? null,
        sessionId: runtimeSessionId ?? null,
      },
      rendererState: {
        active: rendererState?.active ?? null,
        sessionId: rendererSessionId ?? null,
        rendererCount: Array.isArray(rendererEntries) ? rendererEntries.length : null,
        quiesceCapableCount: Array.isArray(rendererEntries)
          ? rendererEntries.length - renderersWithoutQuiesce.length
          : null,
      },
      rendererQuiesce: {
        ok: rendererQuiesce?.ok ?? null,
        sessionId: quiesceSessionId ?? null,
        stopped: rendererQuiesce?.stopped ?? null,
        rendererCount: Array.isArray(quiesceEntries) ? quiesceEntries.length : null,
        validEntryCount: Array.isArray(quiesceEntries)
          ? quiesceEntries.length - invalidQuiesceEntries.length
          : null,
        entryStoppedSum: quiesceStoppedSumValid ? quiesceStoppedSum : null,
      },
      rendererSessionsAligned,
      perfReports: {
        reportCount: Array.isArray(perfReports) ? perfReports.length : null,
        blockKartSummaryCount: blockKartPerfSummaries.length,
        finiteBlockKartSummaryCount: finiteBlockKartPerfSummaries.length,
        authorityMismatchCount: foreignBlockKartPerfSummaries.length,
        finalFrameSummaryCount: finalFramePerfSummaries.length,
        blockKartSummaryFrames: blockKartPerfSummaries.map((perfReport) => perfReport?.frame ?? null),
      },
      resourceFailureCount: Array.isArray(resourceFailures) ? resourceFailures.length : null,
      errorCount: Array.isArray(errors) ? errors.length : null,
      p0p1IssueCount: Array.isArray(issues) ? p0p1Issues.length : null,
      restart: restartEvidence,
      acceptanceFailures,
    },
  };
}

function runBlockKartBaselineReportContractSelftest() {
  const validBaseline = {
    startedAt: '2026-07-16T23:58:59.000Z',
    visualCaptureCompletedAt: '2026-07-16T23:59:29.999Z',
    generatedAt: '2026-07-17T00:00:02.000Z',
    policy: {
      failOnIssues: true,
      requireWebGpuAdapter: true,
      visualCaptureEnabled: true,
      expectedLifecycleState: 'Running',
    },
    status: 'ok',
    firstFrame: { ok: true, skipped: false },
    lifecycle: { state: 'Running', reachedRunning: true },
    visual: {
      enabled: true,
      viewport: { nonEmpty: true },
      canvas: { nonEmpty: true },
      capture: {
        rendererState: {
          active: true,
          sessionId: 7,
          renderers: [
            { id: 'renderer-1', quiesceForCapture: true },
            { id: 'renderer-2', quiesceForCapture: true },
          ],
        },
        rendererQuiesce: {
          ok: true,
          sessionId: 7,
          stopped: 2,
          renderers: [
            { quiesceForCapture: true, stopped: 1 },
            { quiesceForCapture: true, stopped: 1 },
          ],
        },
      },
    },
    webGpu: {
      probeExecutedInPage: true,
      supported: true,
      adapter: true,
    },
    rendererReadiness: {
      ready: true,
      source: 'voplay-perf-endpoint',
      frame: 42,
      reportCount: 1,
      notBefore: '2026-07-16T23:59:00.000Z',
      failure: null,
      record: {
        receivedAt: '2026-07-16T23:59:00.001Z',
        payload: {
          source: 'blockkart',
          kind: 'perf-summary',
          frame: 42,
          studioSessionId: 7,
          studioPerfEpoch: 0,
          screen: { pixelWidth: 1280, pixelHeight: 720 },
          window: { frames: 300 },
          current: { frameMs: 8.2 },
          renderer: { submitFrameMs: 1.1 },
        },
      },
    },
    finalRendererReadiness: {
      ready: true,
      source: 'voplay-perf-endpoint',
      frame: 43,
      reportCount: 1,
      notBefore: '2026-07-16T23:59:30.000Z',
      expectedPerfEpoch: 1,
      minimumFrameExclusive: 42,
      failure: null,
      record: {
        receivedAt: '2026-07-16T23:59:30.001Z',
        payload: {
          source: 'blockkart',
          kind: 'perf-summary',
          frame: 43,
          studioSessionId: 7,
          studioPerfEpoch: 1,
          screen: { pixelWidth: 1280, pixelHeight: 720 },
          window: { frames: 300 },
          current: { frameMs: 8.2 },
          renderer: { submitFrameMs: 1.1 },
        },
      },
    },
    runtimeState: {
      status: 'ready',
      kind: 'gui',
      isRunning: true,
      lastErrorPresent: true,
      lastError: null,
      gui: { moduleBytesLength: 1024, renderBytesLength: 256, sessionId: 7 },
    },
    perfReports: [{
      source: 'blockkart',
      kind: 'perf-summary',
      frame: 43,
      studioSessionId: 7,
      studioPerfEpoch: 1,
    }],
    resourceFailures: [],
    errors: [],
    issues: [],
  };
  const validRestart = {
    ...validBaseline,
    perfReports: [{
      source: 'blockkart',
      kind: 'perf-summary',
      frame: 84,
      studioSessionId: 7,
      studioPerfEpoch: 2,
    }],
    restart: {
      requested: 50,
      completed: 50,
      skipped: false,
      failure: null,
      iterations: Array.from({ length: 50 }, (_, index) => ({ index: index + 1, ok: true })),
    },
    finalRendererReadiness: {
      ready: true,
      source: 'voplay-perf-endpoint',
      frame: 84,
      reportCount: 2,
      notBefore: '2026-07-17T00:00:00.000Z',
      expectedPerfEpoch: 2,
      minimumFrameExclusive: 42,
      failure: null,
      record: {
        receivedAt: '2026-07-17T00:00:01.000Z',
        payload: {
          source: 'blockkart',
          kind: 'perf-summary',
          frame: 84,
          studioSessionId: 7,
          studioPerfEpoch: 2,
          screen: { pixelWidth: 1280, pixelHeight: 720 },
          window: { frames: 300 },
          current: { frameMs: 8.2 },
          renderer: { submitFrameMs: 1.1 },
        },
      },
    },
  };
  const clone = (value) => JSON.parse(JSON.stringify(value));
  const requireVerdict = (condition, message) => {
    if (!condition) throw new Error(`baseline report contract selftest: ${message}`);
  };
  const expectFailure = (fixture, options, code, mutate) => {
    const candidate = clone(fixture);
    mutate(candidate);
    const verdict = validateBlockKartBaselineReport(candidate, options);
    requireVerdict(!verdict.ok, `${code} mutation unexpectedly passed`);
    requireVerdict(
      verdict.evidence.acceptanceFailures.some((failure) => failure.code === code),
      `${code} mutation produced ${JSON.stringify(verdict.evidence.acceptanceFailures)}`,
    );
  };

  requireVerdict(validateBlockKartBaselineReport(validBaseline).ok, 'valid baseline was rejected');
  const closedTimeWindowBaseline = clone(validBaseline);
  closedTimeWindowBaseline.rendererReadiness.notBefore = closedTimeWindowBaseline.startedAt;
  closedTimeWindowBaseline.rendererReadiness.record.receivedAt = closedTimeWindowBaseline.startedAt;
  closedTimeWindowBaseline.generatedAt = closedTimeWindowBaseline.finalRendererReadiness.record.receivedAt;
  requireVerdict(
    validateBlockKartBaselineReport(closedTimeWindowBaseline).ok,
    'baseline records at the closed report-window boundaries were rejected',
  );
  requireVerdict(
    validateBlockKartBaselineReport({ ...clone(validBaseline), issues: [{ severity: 'P2' }] }).ok,
    'non-blocking P2 issue was rejected',
  );
  requireVerdict(
    validateBlockKartBaselineReport(validRestart, { expectedRestartCount: 50 }).ok,
    'valid restart-50 baseline was rejected',
  );

  const baselineMutations = [
    ['report.started_at_invalid', (report) => { report.startedAt = '2026-07-16T23:58:59+00:00'; }],
    ['report.generated_at_invalid', (report) => { report.generatedAt = 'invalid'; }],
    ['report.generated_at_invalid', (report) => { report.generatedAt = '2026-07-17T00:00:02+00:00'; }],
    ['report.time_order_invalid', (report) => { report.startedAt = '2026-07-17T00:00:03.000Z'; }],
    ['visual_capture.completed_at_invalid', (report) => { report.visualCaptureCompletedAt = 'invalid'; }],
    ['visual_capture.completed_at_invalid', (report) => { report.visualCaptureCompletedAt = '2026-07-16T23:59:29+00:00'; }],
    ['visual_capture.completed_at_outside_report_window', (report) => { report.visualCaptureCompletedAt = '2026-07-17T00:00:03.000Z'; }],
    ['policy.fail_on_issues_disabled', (report) => { report.policy.failOnIssues = false; }],
    ['policy.webgpu_not_required', (report) => { report.policy.requireWebGpuAdapter = false; }],
    ['policy.visual_capture_disabled', (report) => { report.policy.visualCaptureEnabled = false; }],
    ['policy.lifecycle_not_running', (report) => { report.policy.expectedLifecycleState = 'Ready'; }],
    ['status.not_ok', (report) => { report.status = 'pass'; }],
    ['first_frame.not_ok', (report) => { report.firstFrame.ok = false; }],
    ['first_frame.skipped', (report) => { report.firstFrame.skipped = true; }],
    ['lifecycle.not_running', (report) => { report.lifecycle.state = 'Failed'; }],
    ['lifecycle.running_not_reached', (report) => { report.lifecycle.reachedRunning = false; }],
    ['visual.not_enabled', (report) => { report.visual.enabled = false; }],
    ['visual.viewport_empty', (report) => { report.visual.viewport.nonEmpty = false; }],
    ['visual.canvas_empty', (report) => { report.visual.canvas.nonEmpty = false; }],
    ['webgpu.adapter_unverified', (report) => { report.webGpu.adapter = false; }],
    ['webgpu.adapter_unverified', (report) => { report.webGpu.supported = false; }],
    ['webgpu.adapter_unverified', (report) => { report.webGpu.probeExecutedInPage = false; }],
    ['renderer_readiness.invalid', (report) => { delete report.rendererReadiness; }],
    ['renderer_readiness.record_invalid', (report) => { delete report.rendererReadiness.record; }],
    ['renderer_readiness.not_ready', (report) => { report.rendererReadiness.ready = false; }],
    ['renderer_readiness.source_invalid', (report) => { report.rendererReadiness.source = 'page-global'; }],
    ['renderer_readiness.frame_invalid', (report) => { report.rendererReadiness.frame = Number.NaN; }],
    ['renderer_readiness.report_count_invalid', (report) => { report.rendererReadiness.reportCount = 0; }],
    ['renderer_readiness.failure_present', (report) => { report.rendererReadiness.failure = 'renderer unavailable'; }],
    ['renderer_readiness.failure_present', (report) => { delete report.rendererReadiness.failure; }],
    ['renderer_readiness.not_before_invalid', (report) => { report.rendererReadiness.notBefore = 'invalid'; }],
    ['renderer_readiness.received_at_invalid', (report) => { report.rendererReadiness.record.receivedAt = 'invalid'; }],
    ['renderer_readiness.received_at_invalid', (report) => { report.rendererReadiness.record.receivedAt = '2026-07-16T23:59:00+00:00'; }],
    ['renderer_readiness.record_stale', (report) => { report.rendererReadiness.record.receivedAt = '2026-07-16T23:58:59.999Z'; }],
    ['renderer_readiness.record_outside_report_window', (report) => { report.rendererReadiness.record.receivedAt = '2026-07-17T00:00:03.000Z'; }],
    ['renderer_readiness.record_payload_invalid', (report) => { report.rendererReadiness.record.payload.source = 'voplay'; }],
    ['renderer_readiness.record_payload_invalid', (report) => { report.rendererReadiness.record.payload.kind = 'perf-pulse'; }],
    ['renderer_readiness.record_payload_invalid', (report) => { report.rendererReadiness.record.payload.frame = Number.NaN; }],
    ['renderer_readiness.frame_mismatch', (report) => { report.rendererReadiness.record.payload.frame = 43; }],
    ['renderer_readiness.record_session_invalid', (report) => { delete report.rendererReadiness.record.payload.studioSessionId; }],
    ['renderer_readiness.session_mismatch', (report) => { report.rendererReadiness.record.payload.studioSessionId = 8; }],
    ['renderer_readiness.record_epoch_invalid', (report) => { delete report.rendererReadiness.record.payload.studioPerfEpoch; }],
    ['renderer_readiness.record_metrics_invalid', (report) => { delete report.rendererReadiness.record.payload.window; }],
    ['final_renderer_readiness.invalid', (report) => { delete report.finalRendererReadiness; }],
    ['final_renderer_readiness.expected_epoch_invalid', (report) => { delete report.finalRendererReadiness.expectedPerfEpoch; }],
    ['final_renderer_readiness.record_epoch_invalid', (report) => { delete report.finalRendererReadiness.record.payload.studioPerfEpoch; }],
    ['final_renderer_readiness.record_metrics_invalid', (report) => { delete report.finalRendererReadiness.record.payload.renderer; }],
    ['final_renderer_readiness.epoch_mismatch', (report) => { report.finalRendererReadiness.record.payload.studioPerfEpoch = 2; }],
    ['final_renderer_readiness.session_mismatch', (report) => { report.finalRendererReadiness.record.payload.studioSessionId = 8; }],
    ['final_renderer_readiness.minimum_frame_invalid', (report) => { delete report.finalRendererReadiness.minimumFrameExclusive; }],
    ['final_renderer_readiness.frame_not_advanced', (report) => {
      report.finalRendererReadiness.frame = 42;
      report.finalRendererReadiness.record.payload.frame = 42;
    }],
    ['final_renderer_readiness.epoch_not_advanced', (report) => {
      report.rendererReadiness.record.payload.studioPerfEpoch = 1;
      report.finalRendererReadiness.expectedPerfEpoch = 1;
      report.finalRendererReadiness.record.payload.studioPerfEpoch = 1;
    }],
    ['final_renderer_readiness.frame_not_advanced_from_initial', (report) => { report.finalRendererReadiness.minimumFrameExclusive = 41; }],
    ['final_renderer_readiness.causal_time_order_invalid', (report) => { report.finalRendererReadiness.notBefore = '2026-07-16T23:58:30.000Z'; }],
    ['final_renderer_readiness.record_outside_report_window', (report) => { report.finalRendererReadiness.record.receivedAt = '2026-07-17T00:00:03.000Z'; }],
    ['final_renderer_readiness.received_at_invalid', (report) => { report.finalRendererReadiness.record.receivedAt = '2026-07-16T23:59:30+00:00'; }],
    ['final_renderer_readiness.before_visual_capture', (report) => { report.finalRendererReadiness.notBefore = '2026-07-16T23:59:29.000Z'; }],
    ['runtime_state.status_not_ready', (report) => { report.runtimeState.status = 'loading'; }],
    ['runtime_state.kind_not_gui', (report) => { report.runtimeState.kind = 'console'; }],
    ['runtime_state.not_running', (report) => { report.runtimeState.isRunning = false; }],
    ['runtime_state.last_error_presence_unverified', (report) => { report.runtimeState.lastErrorPresent = false; }],
    ['runtime_state.last_error_presence_unverified', (report) => { delete report.runtimeState.lastErrorPresent; }],
    ['runtime_state.last_error_not_null', (report) => { report.runtimeState.lastError = 'runtime failed'; }],
    ['runtime_state.last_error_not_null', (report) => { delete report.runtimeState.lastError; }],
    ['runtime_state.module_bytes_invalid', (report) => { report.runtimeState.gui.moduleBytesLength = 0; }],
    ['runtime_state.render_bytes_invalid', (report) => { report.runtimeState.gui.renderBytesLength = 0; }],
    ['runtime_state.session_id_invalid', (report) => { report.runtimeState.gui.sessionId = 0; }],
    ['renderer_state.inactive', (report) => { report.visual.capture.rendererState.active = false; }],
    ['renderer_state.renderers_empty', (report) => { report.visual.capture.rendererState.renderers = []; }],
    ['renderer_state.quiesce_unsupported', (report) => { report.visual.capture.rendererState.renderers[0].quiesceForCapture = false; }],
    ['renderer_state.session_id_invalid', (report) => { report.visual.capture.rendererState.sessionId = 0; }],
    ['renderer_quiesce.not_ok', (report) => { report.visual.capture.rendererQuiesce.ok = false; }],
    ['renderer_quiesce.stopped_invalid', (report) => { report.visual.capture.rendererQuiesce.stopped = 0; }],
    ['renderer_quiesce.session_id_invalid', (report) => { report.visual.capture.rendererQuiesce.sessionId = 0; }],
    ['renderer_quiesce.renderers_invalid', (report) => { delete report.visual.capture.rendererQuiesce.renderers; }],
    ['renderer_quiesce.renderer_count_mismatch', (report) => { report.visual.capture.rendererQuiesce.renderers.pop(); }],
    ['renderer_quiesce.entry_invalid', (report) => { report.visual.capture.rendererQuiesce.renderers[0].quiesceForCapture = false; }],
    ['renderer_quiesce.entry_invalid', (report) => { report.visual.capture.rendererQuiesce.renderers[0].stopped = 0; }],
    ['renderer_quiesce.stopped_total_mismatch', (report) => { report.visual.capture.rendererQuiesce.stopped = 3; }],
    ['renderer_quiesce.coverage_incomplete', (report) => { report.visual.capture.rendererQuiesce.stopped = 1; }],
    ['renderer_sessions.mismatch', (report) => { report.visual.capture.rendererState.sessionId = 8; }],
    ['perf_reports.invalid', (report) => { delete report.perfReports; }],
    ['perf_reports.blockkart_summary_missing', (report) => { report.perfReports[0].source = 'voplay'; }],
    ['perf_reports.blockkart_summary_missing', (report) => { report.perfReports[0].kind = 'perf-pulse'; }],
    ['perf_reports.blockkart_summary_frame_invalid', (report) => { report.perfReports[0].frame = Number.POSITIVE_INFINITY; }],
    ['perf_reports.authority_mismatch', (report) => { report.perfReports[0].studioPerfEpoch = 0; }],
    ['perf_reports.final_frame_missing', (report) => { report.perfReports[0].frame = 44; }],
    ['resource_failures.invalid', (report) => { delete report.resourceFailures; }],
    ['resource_failures.present', (report) => { report.resourceFailures.push({ url: '/missing.bin' }); }],
    ['errors.invalid', (report) => { delete report.errors; }],
    ['errors.present', (report) => { report.errors.push({ message: 'boom' }); }],
    ['issues.invalid', (report) => { delete report.issues; }],
    ['issues.p0_p1_present', (report) => { report.issues.push({ severity: 'P0' }); }],
    ['issues.p0_p1_present', (report) => { report.issues.push({ severity: 'P1' }); }],
  ];
  for (const [code, mutate] of baselineMutations) {
    expectFailure(validBaseline, {}, code, mutate);
  }

  const restartMutations = [
    ['restart.requested_mismatch', (report) => { report.restart.requested = 49; }],
    ['restart.completed_mismatch', (report) => { report.restart.completed = 49; }],
    ['restart.skipped', (report) => { report.restart.skipped = true; }],
    ['restart.failure_present', (report) => { report.restart.failure = 'restart failed'; }],
    ['restart.iterations_invalid', (report) => { delete report.restart.iterations; }],
    ['restart.iteration_count_mismatch', (report) => { report.restart.iterations.pop(); }],
    ['restart.iteration_failed', (report) => { report.restart.iterations[17].ok = false; }],
    ['final_renderer_readiness.invalid', (report) => { delete report.finalRendererReadiness; }],
    ['final_renderer_readiness.record_invalid', (report) => { delete report.finalRendererReadiness.record; }],
    ['final_renderer_readiness.not_ready', (report) => { report.finalRendererReadiness.ready = false; }],
    ['final_renderer_readiness.source_invalid', (report) => { report.finalRendererReadiness.source = 'page-global'; }],
    ['final_renderer_readiness.frame_invalid', (report) => { report.finalRendererReadiness.frame = Number.NaN; }],
    ['final_renderer_readiness.report_count_invalid', (report) => { report.finalRendererReadiness.reportCount = 0; }],
    ['final_renderer_readiness.failure_present', (report) => { report.finalRendererReadiness.failure = 'renderer unavailable'; }],
    ['final_renderer_readiness.failure_present', (report) => { delete report.finalRendererReadiness.failure; }],
    ['final_renderer_readiness.not_before_invalid', (report) => { report.finalRendererReadiness.notBefore = 'invalid'; }],
    ['final_renderer_readiness.received_at_invalid', (report) => { report.finalRendererReadiness.record.receivedAt = 'invalid'; }],
    ['final_renderer_readiness.record_stale', (report) => { report.finalRendererReadiness.record.receivedAt = '2026-07-16T23:59:59.999Z'; }],
    ['final_renderer_readiness.record_payload_invalid', (report) => { report.finalRendererReadiness.record.payload.source = 'voplay'; }],
    ['final_renderer_readiness.record_payload_invalid', (report) => { report.finalRendererReadiness.record.payload.kind = 'perf-pulse'; }],
    ['final_renderer_readiness.record_payload_invalid', (report) => { report.finalRendererReadiness.record.payload.frame = Number.NaN; }],
    ['final_renderer_readiness.frame_mismatch', (report) => { report.finalRendererReadiness.record.payload.frame = 85; }],
    ['final_renderer_readiness.record_session_invalid', (report) => { delete report.finalRendererReadiness.record.payload.studioSessionId; }],
    ['final_renderer_readiness.session_mismatch', (report) => { report.finalRendererReadiness.record.payload.studioSessionId = 8; }],
    ['final_renderer_readiness.record_epoch_invalid', (report) => { delete report.finalRendererReadiness.record.payload.studioPerfEpoch; }],
    ['final_renderer_readiness.record_metrics_invalid', (report) => { delete report.finalRendererReadiness.record.payload.screen; }],
    ['final_renderer_readiness.expected_epoch_invalid', (report) => { delete report.finalRendererReadiness.expectedPerfEpoch; }],
    ['final_renderer_readiness.epoch_mismatch', (report) => { report.finalRendererReadiness.record.payload.studioPerfEpoch = 3; }],
    ['final_renderer_readiness.minimum_frame_invalid', (report) => { delete report.finalRendererReadiness.minimumFrameExclusive; }],
  ];
  for (const [code, mutate] of restartMutations) {
    expectFailure(validRestart, { expectedRestartCount: 50 }, code, mutate);
  }
}

runBlockKartBaselineReportContractSelftest();

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
  path.join(root, 'target/voplay-render-core-unit/report.json'),
  'phase-2',
  'gate.voplay_render_core_unit',
  (report) => passStatusReport(report, 'voplay render core unit report passes'),
  'voplay-render-core-unit',
);

checkReport(
  path.join(root, 'target/voplay-render-architecture-lint/report.json'),
  'phase-2',
  'gate.voplay_render_architecture_lint',
  (report) => passStatusReport(report, 'voplay render architecture lint report passes'),
  'voplay-render-architecture-lint',
);

checkReport(
  path.join(root, 'target/voplay-batch-planner-unit/report.json'),
  'phase-3',
  'gate.voplay_batch_planner_unit',
  (report) => passStatusReport(report, 'voplay batch planner unit report passes'),
  'voplay-batch-planner-unit',
);

checkReport(
  path.join(root, 'target/voplay-scene3d-contract/report.json'),
  'phase-4',
  'gate.voplay_scene3d_contract',
  (report) => passStatusReport(report, 'voplay Scene3D contract report passes'),
  'voplay-scene3d-contract',
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
  const observabilityFailures = sceneObservabilityFailures(report);
  const ok = report.status === 'pass' && allScenesPass(report) && missing.length === 0 && sourceMismatches.length === 0 && observabilityFailures.length === 0;
  return {
    ok,
    detail: 'render stress budgeted report passes all required industrial scenes',
    evidence: {
      status: report.status,
      missingScenes: missing,
      sourceMismatches,
      observabilityFailures,
      sceneCount: report.scenes?.length || 0,
      summary: report.summary || null,
    },
  };
}, 'voplay-render-stress-budgeted');

checkReport(path.join(root, 'target/voplay-render-soak-10m/report.json'), 'phase-5', 'gate.render_soak_10m', (report) => {
  const sourceMismatches = sceneSourceMismatches(report);
  const observabilityFailures = sceneObservabilityFailures(report);
  const longRunTelemetryFailures = sceneLongRunTelemetryFailures(report);
  return {
    ok: report.status === 'pass' && allScenesPass(report) && sourceMismatches.length === 0 && observabilityFailures.length === 0 && longRunTelemetryFailures.length === 0,
    detail: 'ten minute render soak report exists and passes',
    evidence: {
      status: report.status,
      sourceMismatches,
      observabilityFailures,
      longRunTelemetryFailures,
      sceneCount: report.scenes?.length || 0,
      summary: report.summary || null,
    },
  };
}, 'voplay-render-soak-10m');

checkReport(path.join(root, 'target/voplay-physics-industrial-stress/report.json'), 'phase-5', 'gate.physics_industrial_stress', (report) => {
  const required = ['skidpad', 'slalom', 'drift-turbo', 'boost-pad', 'offroad-transition', 'surface-transition', 'jump-landing', 'wall-impact', 'rail-ride', 'wall-ride', 'water-skim', 'road-edge-assist', 'sleep-wake', 'pose-reset', 'recovery', 'multi-vehicle-scripted-soak'];
  const names = scenarioNames(report);
  const missing = required.filter((name) => !names.has(name));
  const scenarioByName = new Map((report.scenarios ?? []).map((scenario) => [scenario.name, scenario]));
  const replayByScenario = new Map((report.replays ?? []).map((replay) => [replay.referenceScenario, replay]));
  const replayScenarioFailures = required.filter((name) => {
    const replay = replayByScenario.get(name);
    const scenario = scenarioByName.get(name);
    return !replay
      || replay.status !== 'pass'
      || replay.freshProcess !== true
      || Number(replay.mismatches ?? Infinity) !== 0
      || Number(replay.samples ?? 0) !== Number(scenario?.steps ?? 0) * Number(scenario?.vehicleCount ?? 0)
      || Number(replay.vehicleCount ?? 0) !== Number(scenario?.vehicleCount ?? 0)
      || Number(replay.backendCommandCount ?? 0) <= 0;
  });
  const fleetReplay = replayByScenario.get('multi-vehicle-scripted-soak');
  const replayOk = report.replay?.status === 'pass'
    && Number(report.replay?.samples ?? 0) > 0
    && Number(report.replay?.mismatches ?? Infinity) === 0
    && report.replay?.freshProcess === true
    && Number.isFinite(Number(report.replay?.stepHash))
    && Number.isFinite(Number(report.replay?.backendPacketHash))
    && Number.isFinite(Number(report.replay?.backendCommandHash))
    && Number(report.replay?.backendCommandHash) > 0
    && (report.replays ?? []).length === required.length
    && replayScenarioFailures.length === 0
    && Number(fleetReplay?.vehicleCount ?? 0) === 24
    && Number(fleetReplay?.samples ?? 0) === 240 * 24;
  const ok = report.status === 'pass' && allIndustrialPhysicsSamplesClean(report) && missing.length === 0 && replayOk;
  return {
    ok,
    detail: 'industrial physics stress passes scenario, fallback, invalid sample, and executable replay gates',
    evidence: {
      status: report.status,
      missingScenarios: missing,
      scenarioCount: report.scenarios?.length || 0,
      replayScenarioCount: report.replays?.length || 0,
      replayScenarioFailures,
      fleetReplay: fleetReplay || null,
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
  const verdict = validateBlockKartBaselineReport(report);
  return {
    ok: verdict.ok && sourceMismatches.length === 0,
    detail: verdict.detail,
    evidence: {
      ...verdict.evidence,
      sourceMismatches,
    },
  };
}, 'blockkart-baseline');

checkReport(path.join(root, 'target/blockkart-baseline-restart-50/blockkart-baseline.json'), 'phase-6', 'gate.blockkart_restart_50_report', (report) => {
  const sourceMismatches = baselineSourceMismatches(report);
  const verdict = validateBlockKartBaselineReport(report, { expectedRestartCount: 50 });
  return {
    ok: verdict.ok && sourceMismatches.length === 0,
    detail: verdict.detail,
    evidence: {
      ...verdict.evidence,
      sourceMismatches,
    },
  };
}, 'blockkart-baseline-restart-50');

const voplayArtifact = expectedVoplayDigest ? { sourceDigest: expectedVoplayDigest } : null;
addCheck('phase-6', 'artifact.provenance_exists', provenance.exists && !provenance.error, 'quickplay provenance JSON exists and parses', { path: provenancePath, parseError: provenance.error });
addCheck('phase-6', 'artifact.voplay_source_digest_recorded', Boolean(voplayArtifact?.sourceDigest), 'quickplay provenance records the voplay source digest', { sourceDigest: voplayArtifact?.sourceDigest || null });
addCheck('phase-6', 'artifact.blockkart_source_digest_recorded', Boolean(expectedBlockKartDigest), 'quickplay provenance records the BlockKart source digest', { sourceDigest: expectedBlockKartDigest });
addCheck('phase-6', 'artifact.vpak_byte_binding', quickplayProducerProvenanceReady, 'quickplay project output and embedded vpak carry exact byte bindings', {
  owner: 'studio/artifacts',
  file: 'apps/studio/public/quickplay/blockkart/provenance.json',
  line: provenance.exists ? lineOf(readText(provenancePath) || '', '"outputs"') ?? 1 : 1,
  reason: 'assets/blockkart.vpak and project.json must carry verified size/digest bindings',
  requiredFix: 'Regenerate the Quickplay vNext package and validate its output facts.',
  projectOutput: quickplayProjectOutput,
  vpak: quickplayVpakFile,
});
addCheck('phase-6', 'artifact.source_digest_bindings', quickplaySourceDigestIssues.length === 0, 'Quickplay provenance binds every consumed source closure with canonical digests', { quickplaySourceDigestIssues });

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
  for (const token of ['ApplyVehicleConstraint(', 'ApplyEntityPhysicsConstraint(', 'PrimitiveStats(', 'primitive3d.NewLayer', 'primitive3d.NewBuilder', 'primitive3d.LayerDesc', 'primitive3d.ChunkingDesc', 'primitive3d.MaterialDesc']) {
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
  dirtyRangeFullRebuildHits,
  dirtyRangePartialUploadVerified,
  renderPassContextWideHits,
  resourceRegistryBackingAgreement,
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
  malformedPacketBranchFailures,
  packetErrorContractReady,
  publicForceBypassHits,
  rawPhysicsBypassHits,
  productPhysicsBypassHits,
  constraintBypassHits,
  physicsStressCommandFailureCountersReady,
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
const generatedAt = new Date().toISOString();
const selfEvidenceScope = fixedFreshEvidenceScope('voplay-industrial-readiness');
const freshEvidence = sourceBoundEvidence({
  gate: 'voplay-industrial-readiness',
  generatedAt,
  root,
  repos: selfEvidenceScope.repos,
  gateFiles: selfEvidenceScope.gateFiles,
  artifacts: selfEvidenceScope.artifacts,
});
const selfFreshEvidenceIssues = verifySourceBoundEvidence({
  evidence: freshEvidence,
  expectedGate: 'voplay-industrial-readiness',
  expectedCiRunId: currentCiRunId,
  root,
  expectedGateFiles: selfEvidenceScope.gateFiles,
  expectedArtifacts: selfEvidenceScope.artifacts,
  expectedRepos: selfEvidenceScope.repos,
});
addCheck(
  'phase-0',
  'evidence.self_fresh',
  selfFreshEvidenceIssues.length === 0 && freshEvidence.verdict.status === 'pass',
  'industrial readiness final verdict is bound to its own fresh current-source evidence',
  { freshnessIssues: selfFreshEvidenceIssues, verdict: freshEvidence.verdict },
);

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
const staleSourceDigests = quickplaySourceDigestIssues.length > 0;
const freshSourceBoundReports = selfFreshEvidenceIssues.length === 0
  && freshEvidence.verdict.status === 'pass'
  && failures.every((failure) => !String(failure.code ?? '').includes('fresh'));
const artifactSourceAgreement = !staleSourceDigests && industrialReady;
const architectureEleganceReady = industrialReady
  && sourceAuditFailures.length === 0
  && firstPrinciplesVerdict.status === 'pass';
const readiness = {
  schemaVersion: 1,
  kind: 'voplay.industrialReadinessReport',
  generatedAt,
  freshEvidence,
  industrialReady,
  architectureEleganceReady,
  sourceFirstPrinciplesReview: firstPrinciplesVerdict.status,
  freshSourceBoundReports,
  artifactSourceAgreement,
  reportValidity: {
    status: industrialReady && freshSourceBoundReports ? 'pass' : 'fail',
    ciRunId: currentCiRunId,
    selfFreshnessIssues: selfFreshEvidenceIssues,
    failureCount: failures.length,
  },
  architectureEvidence: {
    status: architectureEleganceReady ? 'pass' : 'fail',
    requiredSourceFactsPass: requiredFalseFacts.length === 0,
    sourceAuditPass: sourceAuditFailures.length === 0,
    firstPrinciplesPass: firstPrinciplesVerdict.status === 'pass',
    renderStressPass: renderStress?.status === 'pass',
  },
  sourceReviewEvidence: firstPrinciplesVerdict,
  completionPolicy: 'phase gates may pass; only this Final Gate may report industrialReady true',
  strictMode: !allowNotReady,
  roots: {
    volang: root,
    voplay: voplayRoot,
    blockKart: blockKartRoot,
  },
  staleSourceDigests,
  sourceDigestIssues: quickplaySourceDigestIssues,
  revisions: {
    volang: gitCommit(root),
    voplaySource: gitCommit(voplayRoot),
    blockKartSource: gitCommit(blockKartRoot),
    blockKartBaseCommit: quickplayProject.value?.baseCommit ?? null,
  },
  sourceDigests: {
    blockKart: expectedBlockKartDigest,
    voplay: voplayArtifact?.sourceDigest || null,
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
