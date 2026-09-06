import assert from 'node:assert/strict';
import { mkdir, mkdtemp, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

import { hostPlatform } from '../test_runner_host.mjs';

const project = resolve(dirname(fileURLToPath(import.meta.url)), '..');

for (const runner of ['test_runner.mjs', 'aot_test_runner.mjs']) {
  test(`${runner} drains a failing matrix JSON report through a pipe`, async () => {
    const temporary = await mkdtemp(join(tmpdir(), 'volang-runner-report-'));
    try {
      const plan = join(temporary, 'plan.json');
      const jobs = Array.from({ length: 2_000 }, (_, index) => ({
        id: `absent-${index}::wasm`, case_id: `absent-${index}`, kind: 'file',
        path: `target/ci/absent-${index}.vo`, target: 'wasm', backend: 'vo-web',
        timeout_sec: 1, requires_host: [], resource_group: null,
      }));
      await writeFile(plan, JSON.stringify({ schema: 'volang.test-plan.v2', suite: 'lang', host_platform: hostPlatform, jobs }));
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

// Worker supervisor contracts use real worker isolates and process termination.
test('Wasm workers bound concurrency, isolate environments and terminate synchronous loops', async () => {
  const { executeWorker, mapBounded, parseWorkers } = await import('../test_runner_pool.mjs');
  const worker = new URL('data:text/javascript,' + encodeURIComponent(`
    import { parentPort, workerData } from 'node:worker_threads';
    if (workerData.spin) { while (true) {} }
    const inherited = process.env.VO_WORKER_ISOLATION;
    const memory = new Uint8Array(new WebAssembly.Instance(workerData.module).exports.memory.buffer);
    const initialMemory = memory[0];
    memory[0] = workerData.index + 1;
    process.env.VO_WORKER_ISOLATION = String(workerData.index);
    await new Promise(resolve => setTimeout(resolve, workerData.delay));
    parentPort.postMessage({ index: workerData.index, inherited, initialMemory });
  `));
  // One compiled module, with one exported memory, is cloned into every case.
  const module = await WebAssembly.compile(Uint8Array.from([
    0, 97, 115, 109, 1, 0, 0, 0, 5, 3, 1, 0, 1,
    7, 10, 1, 6, 109, 101, 109, 111, 114, 121, 2, 0,
  ]));
  let active = 0, peak = 0;
  const previous = process.env.VO_WORKER_ISOLATION;
  delete process.env.VO_WORKER_ISOLATION;
  try {
    const output = await mapBounded([0, 1, 2, 3, 4, 5], 2, async index => {
      active++; peak = Math.max(peak, active);
      try { return (await executeWorker(worker, { index, module, delay: index % 2 ? 5 : 30 }, 5000)).value; }
      finally { active--; }
    });
    assert.equal(peak, 2);
    assert.deepEqual(output.map(item => item.index), [0, 1, 2, 3, 4, 5]);
    assert.ok(output.every(item => item.inherited === undefined));
    assert.ok(output.every(item => item.initialMemory === 0));
    assert.equal(process.env.VO_WORKER_ISOLATION, undefined);
    await assert.rejects(executeWorker(worker, { spin: true }, 100), /timed out/);
    assert.equal((await executeWorker(worker, { index: 7, module, delay: 0 }, 5000)).value.index, 7);
  } finally {
    if (previous === undefined) delete process.env.VO_WORKER_ISOLATION;
    else process.env.VO_WORKER_ISOLATION = previous;
  }
  for (const invalid of [0, 9, 'bad', 1.5, undefined]) assert.throws(() => parseWorkers(invalid));
});

test('Wasm worker failures cannot masquerade as successful case results', async () => {
  const { executeWorker } = await import('../test_runner_pool.mjs');
  const run = source => executeWorker(new URL('data:text/javascript,' + encodeURIComponent(
    "import { parentPort } from 'node:worker_threads';" + source)), {}, 5000);
  await assert.rejects(run(''), /without a result/);
  await assert.rejects(run('parentPort.postMessage(1); parentPort.postMessage(2);'), /duplicate/);
  await assert.rejects(run('parentPort.postMessage(1); process.exit(7);'), /status 7/);
  await assert.rejects(run('throw new Error("fixture crash")'), /fixture crash/);
  await assert.rejects(run('console.error("before crash"); throw new Error("fixture crash")'), error => {
    assert.match(error.workerStderr, /before crash/);
    return /fixture crash/.test(error.message);
  });
  await assert.rejects(run('for (let i=0;i<100;i++) console.log("x".repeat(1024*1024));'), /output exceeded/);
});

test('Wasm VM synchronous infinite loop times out and the next real case still executes', async () => {
  const root = resolve(project, '../../..');
  const parent = join(root, 'target/ci');
  await mkdir(parent, { recursive: true });
  const temporary = await mkdtemp(join(parent, 'wasm-runner-deadline-'));
  try {
    await writeFile(join(temporary, 'spin.vo'), 'package main\nfunc main() { for {} }\n');
    await writeFile(join(temporary, 'next.vo'), 'package main\nfunc main() { println("after-timeout") }\n');
    const jobs = ['spin', 'next'].map(name => ({
      id: name, case_id: name, kind: 'file', path: join(temporary, name + '.vo'),
      target: 'wasm', backend: 'vo-web', requires_host: [], resource_group: null, timeout_sec: name === 'spin' ? 1 : 5,
    }));
    const plan = join(temporary, 'plan.json');
    await writeFile(plan, JSON.stringify({ schema: 'volang.test-plan.v2', suite: 'lang', host_platform: hostPlatform, jobs }));
    const child = spawnSync(process.execPath, [join(project, 'test_runner.mjs'), '--plan', plan, '--format', 'json', '--jobs', '1'], {
      encoding: 'utf8', maxBuffer: 2 * 1024 * 1024, timeout: 15_000,
    });
    assert.ifError(child.error);
    assert.equal(child.status, 1, child.stderr);
    const report = JSON.parse(child.stdout);
    assert.equal(report.failed, 1);
    assert.equal(report.passed, 1);
    assert.match(report.jobs[0].error, /timed out after 1s/);
    assert.equal(report.jobs[1].stdout, 'after-timeout\n');
    assert.equal(report.jobs[1].status, 'passed');
  } finally { await rm(temporary, { recursive: true, force: true }); }
});

test('declared resources serialize collisions without occupying unrelated workers', async () => {
  const { mapBounded } = await import('../test_runner_pool.mjs');
  let release;
  const blocked = new Promise(resolve => { release = resolve; });
  const order = [];
  const jobs = [{ resource_group: 'shared' }, { resource_group: 'shared' }, { resource_group: null }];
  const result = await mapBounded(jobs, 2, async (_, index) => {
    order.push(index);
    if (index === 0) await blocked;
    if (index === 2) release();
    return index;
  });
  assert.deepEqual(order, [0, 2, 1]);
  assert.deepEqual(result, [0, 1, 2]);
});

test('host contracts reject missing identity, unknown capabilities and forged probe environment', async () => {
  const { validateHostPlan, jobHost } = await import('../test_runner_host.mjs');
  const job = { id: 'fixture', requires_host: [], resource_group: null, tags: ['symlink'] };
  const plan = { host_platform: hostPlatform, jobs: [job] };
  validateHostPlan(plan);
  assert.throws(() => validateHostPlan({ ...plan, host_platform: 'wrong' }));
  for (const requires_host of [undefined, ['unknown'], ['symlink', 'symlink']]) {
    assert.throws(() => validateHostPlan({ ...plan, jobs: [{ ...job, requires_host }] }));
  }
  assert.throws(() => validateHostPlan({ ...plan, jobs: [{ ...job, env: { VO_TEST_HOST_SYMLINK: 'supported' } }] }));
  assert.throws(() => validateHostPlan({ ...plan, jobs: [{ ...job, env: { Vo_Test_Host_Symlink: 'supported' } }] }));
  const unavailable = { symlink: { status: 'unavailable', detail: 'fixture permission denied' } };
  assert.equal(jobHost(job, unavailable).failure_kind, null);
  assert.equal(jobHost({ ...job, requires_host: ['symlink'] }, unavailable).failure_kind, 'portability');
  assert.equal(jobHost(job, {}).failure_kind, 'infrastructure');
});
