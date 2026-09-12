import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';
import test from 'node:test';

// The shared worker builds a CLI-produced image and executes it with the real
// Core Wasm host. Runtime panic assertions belong here; the language manifest's
// fail expectation describes compile diagnostics.
test('Core Wasm nil goroutine invocation belongs to the child Fiber', () => {
  const work = mkdtempSync(join(tmpdir(), 'volang-vm-contract-'));
  try {
    const source = join(work, 'main.vo');
    writeFileSync(source, `package main

func launch() {
	defer func() {
		if recover() != nil {
			panic("launcher caught child panic")
		}
	}()
	var f func()
	go f()
	println("launch returned")
}

func main() {
	launch()
}
`);
    const worker = fileURLToPath(new URL('../aot_test_worker.mjs', import.meta.url));
    const output = spawnSync(process.execPath, [worker], {
      input: JSON.stringify({ path: source }),
      encoding: 'utf8',
      timeout: 60_000,
      maxBuffer: 16 * 1024 * 1024,
      env: { ...process.env, VOWORK: 'off' },
    });
    assert.ifError(output.error);
    assert.equal(output.status, 0, output.stderr);
    const result = JSON.parse(output.stdout);
    assert.equal(result.phase, 'run', result.stderr);
    assert.equal(result.status, 'error');
    assert.equal(result.stdout, 'launch returned\n');
    assert.equal(result.stderr, 'unhandled panic: runtime error: call of nil function');
  } finally {
    rmSync(work, { recursive: true, force: true });
  }
});


test('Wasm VM concurrent runs retain their own output across host waits', async () => {
  const wasm = await import('../pkg/vo_web.js');
  await wasm.default({ module_or_path: readFileSync(new URL('../pkg/vo_web_bg.wasm', import.meta.url)) });
  const program = (name, delay) => `package main
import "time"
func main() {
  println("${name}:before")
  time.Sleep(${delay} * time.Millisecond)
  println("${name}:after")
}`;
  const [first, second] = await Promise.all([
    wasm.compileAndRun(program('first', 30), 'first.vo'),
    wasm.compileAndRun(program('second', 5), 'second.vo'),
  ]);
  for (const [name, result] of [['first', first], ['second', second]]) {
    assert.equal(result.status, 'ok', result.stderr);
    assert.equal(result.stdout, `${name}:before\n${name}:after\n`);
  }
});
