import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import path from 'node:path';

const VOLANG_EVIDENCE_PATHS = [
  ':(exclude)lang/docs/dev/vm-production-gate-evidence/**',
  ':(exclude)lang/docs/dev/vm-production-readiness.md',
];
export const PINNED_PLAN_REPOSITORIES = Object.freeze([
  'vogui',
  'voplay',
  'vopack',
  'BlockKart',
]);
export const PLAN_SNAPSHOT_REPOSITORIES = Object.freeze([
  'volang',
  ...PINNED_PLAN_REPOSITORIES,
]);
const planSnapshotPattern = new RegExp(
  `^(${PLAN_SNAPSHOT_REPOSITORIES.join('|')})\\s+([0-9a-f]{40})$`,
);

function gitLastSourceCommit(repoRoot, rev = 'HEAD') {
  if (!existsSync(repoRoot)) return null;
  const result = spawnSync('git', [
    'log',
    '-1',
    '--format=%H',
    rev,
    '--',
    '.',
    ...VOLANG_EVIDENCE_PATHS,
  ], {
    cwd: repoRoot,
    encoding: 'utf8',
  });
  if (result.status !== 0) return null;
  return result.stdout.trim() || null;
}

export function acceptedVolangPlanSnapshotCommits(repoRoot) {
  const current = gitLastSourceCommit(repoRoot);
  const previous = current ? gitLastSourceCommit(repoRoot, `${current}^`) : null;
  return [current, previous].filter(Boolean);
}

function lineOf(text, needle) {
  const lines = text.split(/\r?\n/);
  const index = lines.findIndex((line) => line.includes(needle));
  return index === -1 ? 1 : index + 1;
}

function gitHead(repoRoot) {
  if (!existsSync(repoRoot)) return null;
  const result = spawnSync('git', ['rev-parse', 'HEAD'], {
    cwd: repoRoot,
    encoding: 'utf8',
  });
  if (result.status !== 0) return null;
  return result.stdout.trim() || null;
}

export function parseProjectPins(projectText) {
  const pins = {};
  let section = null;
  const finishSection = () => {
    if (!section?.name) return;
    pins[section.name] = {
      expectedCommit: section.expected_commit ?? null,
      localHint: section.local_hint ?? null,
    };
  };
  for (const rawLine of projectText.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (/^\[\[.*\]\]$/.test(line)) {
      finishSection();
      section = line === '[[first_party]]' || line === '[[external_project]]' ? {} : null;
      continue;
    }
    if (!section) continue;
    const pair = line.match(/^([A-Za-z0-9_]+)\s*=\s*"([^"]*)"$/);
    if (!pair) continue;
    section[pair[1]] = pair[2];
  }
  finishSection();
  return pins;
}

export function parseActivePlanSnapshot(planText) {
  const snapshot = {};
  for (const rawLine of planText.split(/\r?\n/)) {
    const match = rawLine.match(planSnapshotPattern);
    if (match) snapshot[match[1]] = match[2];
  }
  return snapshot;
}

export function activePlanSnapshotContract({
  planText,
  projectText,
  rootPath,
  acceptedVolangCommits = acceptedVolangPlanSnapshotCommits(rootPath),
  gitHeadFn = gitHead,
}) {
  const snapshot = parseActivePlanSnapshot(planText);
  const pins = parseProjectPins(projectText);
  const issues = [];
  const expected = {
    volang: acceptedVolangCommits[0] ?? gitHeadFn(rootPath),
  };
  const accepted = {
    volang: acceptedVolangCommits,
  };
  const repoRoots = {
    volang: rootPath,
  };

  for (const name of PINNED_PLAN_REPOSITORIES) {
    const pin = pins[name];
    if (!/^[0-9a-f]{40}$/.test(pin?.expectedCommit ?? '')) {
      issues.push({
        file: 'eng/project.toml',
        line: lineOf(projectText, `name = "${name}"`),
        name,
        reason: `eng/project.toml is missing a canonical ${name} expected_commit`,
        expectedCommit: pin?.expectedCommit ?? null,
      });
    } else {
      expected[name] = pin.expectedCommit;
      accepted[name] = [pin.expectedCommit];
    }
    if (!pin?.localHint) {
      issues.push({
        file: 'eng/project.toml',
        line: lineOf(projectText, `name = "${name}"`),
        name,
        reason: `eng/project.toml is missing ${name} local_hint`,
      });
    } else {
      repoRoots[name] = path.resolve(rootPath, pin.localHint);
    }
  }

  const heads = {};
  for (const name of PLAN_SNAPSHOT_REPOSITORIES) {
    const found = snapshot[name] ?? null;
    if (!found) {
      issues.push({
        file: 'lang/docs/dev/voplay-code-engineering-quality-plan.md',
        line: lineOf(planText, '当前 checkout snapshot'),
        name,
        reason: `active plan snapshot is missing ${name}`,
      });
      continue;
    }
    const acceptedCommits = accepted[name] ?? [];
    if (acceptedCommits.length > 0 && !acceptedCommits.includes(found)) {
      issues.push({
        file: 'lang/docs/dev/voplay-code-engineering-quality-plan.md',
        line: lineOf(planText, name),
        name,
        reason: `active plan snapshot for ${name} is stale`,
        expected: expected[name] ?? null,
        acceptedCommits,
        found,
      });
    }
    const repoRoot = repoRoots[name] ?? null;
    const head = repoRoot ? gitHeadFn(repoRoot) : null;
    heads[name] = head;
    const acceptedHeads = name === 'volang' ? acceptedVolangCommits : [head];
    if (head && !acceptedHeads.includes(found)) {
      issues.push({
        file: 'lang/docs/dev/voplay-code-engineering-quality-plan.md',
        line: lineOf(planText, name),
        name,
        reason: `active plan snapshot for ${name} does not match checkout HEAD`,
        expected: head,
        acceptedCommits: acceptedHeads,
        found,
        repoRoot,
      });
    }
  }

  return {
    acceptedVolangCommits,
    expected,
    heads,
    issues,
    pins,
    snapshot,
  };
}

function expectSnapshotIssue(label, contract, pattern) {
  if (contract.issues.some((issue) => pattern.test(issue.reason))) return;
  throw new Error(`${label} unexpectedly passed: ${JSON.stringify(contract.issues)}`);
}

export function runActivePlanSnapshotContractSelftest() {
  const commits = {
    volang: '1'.repeat(40),
    vogui: '2'.repeat(40),
    voplay: '3'.repeat(40),
    vopack: '4'.repeat(40),
    BlockKart: '5'.repeat(40),
  };
  const rootPath = path.resolve('/fixture/volang');
  const projectText = PINNED_PLAN_REPOSITORIES.map((name) => `${name === 'BlockKart' ? '[[external_project]]' : '[[first_party]]'}
name = "${name}"
expected_commit = "${commits[name]}"
local_hint = "../${name}"
${name === 'vogui' ? '\n[[first_party.workspace]]\nname = "js"\nkind = "node"\npath = "js"\n' : ''}
`).join('\n');
  const planText = PLAN_SNAPSHOT_REPOSITORIES
    .map((name) => `${name} ${commits[name]}`)
    .join('\n');
  const gitHeadFn = (repoPath) => commits[path.basename(repoPath)] ?? null;
  const contract = (plan = planText, project = projectText, heads = gitHeadFn) => (
    activePlanSnapshotContract({
      planText: plan,
      projectText: project,
      rootPath,
      acceptedVolangCommits: [commits.volang],
      gitHeadFn: heads,
    })
  );

  const valid = contract();
  if (valid.issues.length > 0) {
    throw new Error(`valid five-repository snapshot failed: ${JSON.stringify(valid.issues)}`);
  }
  expectSnapshotIssue(
    'missing vogui snapshot regression',
    contract(planText.replace(`vogui ${commits.vogui}\n`, '')),
    /snapshot is missing vogui/,
  );
  expectSnapshotIssue(
    'stale vopack pin regression',
    contract(planText, projectText.replace(commits.vopack, '6'.repeat(40))),
    /snapshot for vopack is stale/,
  );
  expectSnapshotIssue(
    'vogui checkout drift regression',
    contract(planText, projectText, (repoPath) => (
      path.basename(repoPath) === 'vogui' ? '8'.repeat(40) : gitHeadFn(repoPath)
    )),
    /snapshot for vogui does not match checkout HEAD/,
  );
  expectSnapshotIssue(
    'vopack checkout drift regression',
    contract(planText, projectText, (repoPath) => (
      path.basename(repoPath) === 'vopack' ? '7'.repeat(40) : gitHeadFn(repoPath)
    )),
    /snapshot for vopack does not match checkout HEAD/,
  );
}
