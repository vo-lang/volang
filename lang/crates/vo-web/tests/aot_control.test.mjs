import assert from 'node:assert/strict';
import test from 'node:test';
import { compileAotSource } from '../aot_test_compiler.mjs';
import { runAot } from '../dist/index.js';

test('typed counted loops preserve exact fuel cost and exhaustion', async () => {
  const costs = [];
  for (const iterations of [10, 11]) {
    const source = `package main
func count(n int) int {
    sum := 0
    for i := 0; i < n; i++ { sum += i }
    return sum
}
func main() { println(count(${iterations})) }
`;
    const module = await WebAssembly.compile(compileAotSource(source));
    const run = fuel => runAot(module, { fuel });
    const initial = await run(1000n);
    assert.equal(initial.result.status, 'ok', initial.result.stderr);
    assert.equal(initial.result.stdout, `${iterations * (iterations - 1) / 2}\n`);
    const cost = 1000n - initial.instance.exports.vo_fuel.value;
    assert.ok(cost > 0n && cost < 1000n);
    const exact = await run(cost);
    assert.equal(exact.result.status, 'ok', exact.result.stderr);
    assert.equal(exact.instance.exports.vo_fuel.value, 0n);
    assert.equal(exact.result.stdout, initial.result.stdout);
    const exhausted = await run(cost - 1n);
    assert.equal(exhausted.exitCode, 15);
    assert.equal(exhausted.result.status, 'error');
    assert.match(exhausted.result.stderr, /fuel exhausted/);
    costs.push(cost);
  }
  assert.equal(costs[1] - costs[0], 1n, 'one extra iteration enters one extra guest block');
});
