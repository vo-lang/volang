import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';

const VOLANG_EVIDENCE_PATHS = [
  ':(exclude)lang/docs/dev/vm-production-gate-evidence/**',
  ':(exclude)lang/docs/dev/vm-production-readiness.md',
];

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
