import assert from 'node:assert/strict';
import { readFile, writeFile, mkdir, mkdtemp, rm } from 'node:fs/promises';
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { resolve } from 'node:path';
import { compilerPath } from '../../../lang/crates/vo-web/test_compiler.mjs';
import init, { run } from '../../../target/ui-next/wasm-runtime/vo_web.js';
import { root } from '../server.mjs';

// Reuse the exact app/row definitions. Only the transport loop is replaced by
// explicit phase timers; this diagnostic is separate from browser latency data.
const source = await readFile(resolve(root, 'ui/next/examples/benchmark/main.vo'), 'utf8');
const marker = 'func main() {';
assert.equal(source.split(marker).length, 2);
const prefix = source.slice(0, source.indexOf(marker)).replace('"github.com/vo-lang/ui/next/host"', '"encoding/json"\n"fmt"\n"time"\n"github.com/vo-lang/ui/next/wire"');
const directory = resolve(root, 'target/ui-next/benchmark/phases');
await mkdir(directory, { recursive: true });
await writeFile(`${directory}/main.vo`, prefix + await readFile(new URL('./phases.vo.txt', import.meta.url), 'utf8'));
function execute(args) {
  const result = spawnSync(compilerPath(), args, { cwd: root, env: { ...process.env, VOWORK: 'off' }, encoding: 'utf8', timeout: 120000, maxBuffer: 16 * 1024 * 1024 });
  assert.ifError(result.error); assert.equal(result.status, 0, result.stderr); return result.stdout;
}
// Compile within the UI module so canonical self-imports resolve with VOWORK=off.
// Keep temporary sources outside the framework snapshot/development watch tree.
const temporary = await mkdtemp(resolve(root, 'ui/.phase-profile-'));
try {
  await writeFile(`${temporary}/main.vo`, await readFile(`${directory}/main.vo`));
  execute(['emit', 'bytecode', temporary, '-o', `${directory}/app.vob`]);
} finally { await rm(temporary, { recursive: true }); }
await init({ module_or_path: await readFile(resolve(root, 'target/ui-next/wasm-runtime/vo_web_bg.wasm')) });
const results = {};
const vm = run(await readFile(`${directory}/app.vob`));
try { assert.equal(vm.status, 'ok', vm.stderr); results.vm = JSON.parse(vm.stdout); } finally { vm.free(); }
await writeFile(`${directory}/report.json`, JSON.stringify({ schema: 'volang.ui-next-phase-diagnostic.v1',
  appSourceSha256: createHash('sha256').update(source).digest('hex'),
  method: 'same app/row definitions; separate instrumented image; Node Wasm VM, no DOM; five update repetitions; added time/fmt imports may affect optimization/layout/GC; phase attribution only', results }, null, 2) + '\n');
console.log(JSON.stringify(results));
