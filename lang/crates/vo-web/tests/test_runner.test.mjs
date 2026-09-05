import assert from 'node:assert/strict';
import { mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

const project = resolve(dirname(fileURLToPath(import.meta.url)), '..');

for (const runner of ['test_runner.mjs', 'aot_test_runner.mjs']) {
  test(`${runner} drains a failing matrix JSON report through a pipe`, async () => {
    const temporary = await mkdtemp(join(tmpdir(), 'volang-runner-report-'));
    try {
      const plan = join(temporary, 'plan.json');
      const jobs = Array.from({ length: 2_000 }, (_, index) => ({
        id: `absent-${index}::wasm`, case_id: `absent-${index}`, kind: 'file',
        path: `target/ci/absent-${index}.vo`, target: 'wasm', backend: 'vo-web',
        timeout_sec: 1,
      }));
      await writeFile(plan, JSON.stringify({ schema: 'volang.test-plan.v1', suite: 'lang', jobs }));
      const child = spawnSync(process.execPath, [join(project, runner), '--plan', plan, '--format', 'json'], {
        encoding: 'utf8', maxBuffer: 8 * 1024 * 1024, timeout: 30_000,
      });
      assert.ifError(child.error);
      assert.equal(child.status, 1, child.stderr);
      assert.ok(child.stdout.length > 1024 * 1024, 'exercise a report larger than the pipe buffer');
      const report = JSON.parse(child.stdout);
      assert.equal(report.failed, jobs.length);
      assert.equal(report.passed, 0);
      assert.deepEqual(report.jobs.map(job => job.id), jobs.map(job => job.id));
      assert.ok(report.jobs.every(job => job.status === 'failed' && job.error.includes('file not found')));
    } finally {
      await rm(temporary, { recursive: true, force: true });
    }
  });
}
