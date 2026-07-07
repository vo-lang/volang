import { spawnSync } from 'node:child_process';
import { existsSync, readFileSync } from 'node:fs';
import path from 'node:path';

const root = path.resolve(new URL('../..', import.meta.url).pathname);
const activePlanPath = path.join(root, 'lang/docs/dev/voplay-code-engineering-quality-plan.md');
const projectPath = path.join(root, 'eng/project.toml');

function lineOf(text, needle) {
  const lines = text.split(/\r?\n/);
  const index = lines.findIndex((line) => line.includes(needle));
  return index === -1 ? 1 : index + 1;
}

function failActivePlanFreshness(message, details) {
  const issue = {
    owner: 'Docs',
    subsystem: 'Readiness',
    severity: 'P1',
    file: details.file,
    line: details.line ?? 1,
    reason: message,
    requiredFix: 'Update the active voplay quality plan snapshot from current volang/voplay/BlockKart source commits and rerun docs-lint.',
    details,
  };
  throw new Error(`active plan snapshot freshness failed: ${JSON.stringify(issue)}`);
}

function gitHead(repoRoot) {
  if (!existsSync(repoRoot)) return null;
  const result = spawnSync('git', ['rev-parse', 'HEAD'], {
    cwd: repoRoot,
    encoding: 'utf8',
  });
  if (result.status !== 0) return null;
  return result.stdout.trim();
}

function parseProjectPins(projectText) {
  const pins = {};
  let section = null;
  for (const rawLine of projectText.split(/\r?\n/)) {
    const line = rawLine.trim();
    if (line === '[[first_party]]' || line === '[[external_project]]') {
      section = {};
      continue;
    }
    if (!section) continue;
    const pair = line.match(/^([A-Za-z0-9_]+)\s*=\s*"([^"]*)"$/);
    if (!pair) continue;
    section[pair[1]] = pair[2];
    if (section.name && section.expected_commit) {
      pins[section.name] = {
        expectedCommit: section.expected_commit,
        localHint: section.local_hint ?? null,
      };
    }
  }
  return pins;
}

function parsePlanSnapshot(planText) {
  const snapshot = {};
  for (const rawLine of planText.split(/\r?\n/)) {
    const match = rawLine.match(/^(volang|voplay|BlockKart)\s+([0-9a-f]{40})$/);
    if (match) snapshot[match[1]] = match[2];
  }
  return snapshot;
}

function assertActivePlanSnapshotFreshness() {
  const planText = readFileSync(activePlanPath, 'utf8');
  const projectText = readFileSync(projectPath, 'utf8');
  const snapshot = parsePlanSnapshot(planText);
  const pins = parseProjectPins(projectText);
  const expected = {
    volang: {
      expectedCommit: gitHead(root),
      repoRoot: root,
    },
    voplay: {
      expectedCommit: pins.voplay?.expectedCommit ?? null,
      repoRoot: pins.voplay?.localHint ? path.resolve(root, pins.voplay.localHint) : null,
    },
    BlockKart: {
      expectedCommit: pins.BlockKart?.expectedCommit ?? null,
      repoRoot: pins.BlockKart?.localHint ? path.resolve(root, pins.BlockKart.localHint) : null,
    },
  };
  for (const [name, fact] of Object.entries(expected)) {
    const found = snapshot[name] ?? null;
    if (!found) {
      failActivePlanFreshness(`active plan snapshot is missing ${name}`, {
        file: 'lang/docs/dev/voplay-code-engineering-quality-plan.md',
        line: lineOf(planText, '当前 checkout snapshot'),
        name,
      });
    }
    if (fact.expectedCommit && found !== fact.expectedCommit) {
      failActivePlanFreshness(`active plan snapshot for ${name} is stale`, {
        file: 'lang/docs/dev/voplay-code-engineering-quality-plan.md',
        line: lineOf(planText, name),
        name,
        expected: fact.expectedCommit,
        found,
      });
    }
    const head = fact.repoRoot ? gitHead(fact.repoRoot) : null;
    if (head && found !== head) {
      failActivePlanFreshness(`active plan snapshot for ${name} does not match checkout HEAD`, {
        file: 'lang/docs/dev/voplay-code-engineering-quality-plan.md',
        line: lineOf(planText, name),
        name,
        expected: head,
        found,
        repoRoot: fact.repoRoot,
      });
    }
  }
}

function run(command, args, description) {
  const result = spawnSync(command, args, {
    cwd: root,
    stdio: 'inherit',
  });
  if (result.error) {
    throw new Error(`${description} failed to start: ${result.error.message}`);
  }
  if (result.status !== 0) {
    throw new Error(`${description} failed`);
  }
}

assertActivePlanSnapshotFreshness();
run(
  'cargo',
  ['run', '-q', '-p', 'vo-dev', '--', 'lint', 'docs'],
  'vo-dev docs lint',
);
run(
  process.execPath,
  ['scripts/ci/docs_sync.mjs', '--check'],
  'docs sync check',
);
