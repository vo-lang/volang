import { spawnSync } from 'node:child_process';
import path from 'node:path';

const root = path.resolve(new URL('../..', import.meta.url).pathname);

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
