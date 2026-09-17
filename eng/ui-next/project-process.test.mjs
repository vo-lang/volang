import assert from 'node:assert/strict';
import test from 'node:test';
import { mkdtemp, readFile, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { execute } from './project.mjs';

test('cancelled compiler execution settles only after the child releases its output', async () => {
  const directory = await mkdtemp(join(tmpdir(), 'ui-compiler-cancellation-'));
  const ready = join(directory, 'ready'), finished = join(directory, 'finished');
  const lifetime = new AbortController(), reason = new Error('build replaced');
  const execution = execute(process.execPath, ['-e', `
    const { writeFileSync } = require('node:fs');
    process.on('SIGTERM', () => setTimeout(() => {
      writeFileSync(${JSON.stringify(finished)}, 'released'); process.exit(0);
    }, 80));
    writeFileSync(${JSON.stringify(ready)}, 'ready');
    setInterval(() => {}, 1000);
  `], { signal: lifetime.signal });
  execution.catch(() => {});
  try {
    const deadline = Date.now() + 5000;
    while (true) {
      try { await readFile(ready); break; }
      catch (error) { if (error.code !== 'ENOENT' || Date.now() > deadline) throw error; }
      await delay(10);
    }
    lifetime.abort(reason);
    await assert.rejects(execution, error => error === reason);
    assert.equal(await readFile(finished, 'utf8'), 'released');
  } finally {
    lifetime.abort(reason);
    await execution.catch(() => {});
    await rm(directory, { recursive: true, force: true });
  }
  await assert.rejects(execute('/missing/ui-compiler', []), { code: 'ENOENT' });
  await assert.rejects(execute(process.execPath, ['-e', 'process.exit(0)'], { signal: lifetime.signal }), error => error === reason);
});
