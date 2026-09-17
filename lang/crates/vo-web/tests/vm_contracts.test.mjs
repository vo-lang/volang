import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import test from 'node:test';

async function runtime() {
  const wasm = await import('../pkg/vo_web.js');
  await wasm.default({ module_or_path: readFileSync(new URL('../pkg/vo_web_bg.wasm', import.meta.url)) });
  return wasm;
}

// Runtime panic assertions belong here; language manifest fail expectations
// describe compile diagnostics.
test('Wasm VM nil goroutine invocation belongs to the child Fiber', async () => {
  const wasm = await runtime();
  const result = await wasm.compileAndRun(`package main

func launch() {
	defer func() {
		if recover() != nil {
			panic("launcher caught child panic")
		}
	}()
	var f func()
	println("before spawn")
	go f()
	println("launch returned")
}

func main() {
	launch()
}
`, 'nil-goroutine.vo');
  assert.equal(result.status, 'error');
  // The scheduler may run the child before the launcher continues.
  assert.match(result.stdout, /^before spawn\n(?:launch returned\n)?$/);
  assert.match(result.stderr, /call of nil function/);
  assert.doesNotMatch(result.stderr, /launcher caught child panic/);
});

test('Wasm VM concurrent runs retain their own output across host waits', async () => {
  const wasm = await runtime();
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
