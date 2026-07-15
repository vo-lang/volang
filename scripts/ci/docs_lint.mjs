import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import path from 'node:path';
import { pathToFileURL } from 'node:url';
import {
  acceptedVolangPlanSnapshotCommits,
  activePlanSnapshotContract,
  runActivePlanSnapshotContractSelftest,
} from './active_plan_snapshot.mjs';

const root = path.resolve(new URL('../..', import.meta.url).pathname);
const activePlanPath = path.join(root, 'lang/docs/dev/voplay-code-engineering-quality-plan.md');
const projectPath = path.join(root, 'eng/project.toml');

function failActivePlanFreshness(message, details) {
  const issue = {
    owner: 'Docs',
    subsystem: 'Readiness',
    severity: 'P1',
    file: details.file,
    line: details.line ?? 1,
    reason: message,
    requiredFix: 'Update the active voplay quality plan snapshot from current Volang and pinned sibling source commits and rerun docs-lint.',
    details,
  };
  throw new Error(`active plan snapshot freshness failed: ${JSON.stringify(issue)}`);
}

export function assertActivePlanSnapshotFreshnessFromSources({
  planText,
  projectText,
  rootPath,
  acceptedVolangCommits,
  gitHeadFn,
}) {
  const contract = activePlanSnapshotContract({
    planText,
    projectText,
    rootPath,
    acceptedVolangCommits,
    ...(gitHeadFn ? { gitHeadFn } : {}),
  });
  if (contract.issues.length > 0) {
    const [issue] = contract.issues;
    failActivePlanFreshness(issue.reason, issue);
  }
  return contract;
}

function assertActivePlanSnapshotFreshness() {
  assertActivePlanSnapshotFreshnessFromSources({
    planText: readFileSync(activePlanPath, 'utf8'),
    projectText: readFileSync(projectPath, 'utf8'),
    rootPath: root,
    acceptedVolangCommits: acceptedVolangPlanSnapshotCommits(root),
  });
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

function main() {
  runActivePlanSnapshotContractSelftest();
  if (process.argv.includes('--snapshot-selftest')) {
    console.log('docs active plan snapshot selftest: ok');
    return;
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
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? '').href) {
  main();
}
