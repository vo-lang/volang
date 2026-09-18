import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {copyFile, mkdir, readFile, writeFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {root} from './repository-paths.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';

const directory = resolve(root, 'target/ui-next/native-session');
const runtime = resolve(directory, 'aot-runtime');
await mkdir(resolve(runtime, 'src'), {recursive: true});
const steps = [];
async function run(name, executable, args, options = {}) {
  console.log(`Native UI session: ${name}`);
  const started = performance.now();
  const result = spawnSync(executable, args, {cwd: root, env: {...process.env, VOWORK: 'off'},
    encoding: 'utf8', timeout: 600_000, maxBuffer: 16 * 1024 * 1024, ...options});
  await writeFile(resolve(directory, `${name}.log`), result.stdout + result.stderr);
  steps.push({name, command: [executable, ...args], milliseconds: Math.round(performance.now() - started), passed: result.status === 0});
  await writeFile(resolve(directory, 'build.json'), JSON.stringify({passed: false, steps}, null, 2) + '\n');
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stderr);
}

await run('unit-defaults', 'cargo', ['test', '-p', 'vo-ui-native', '--no-default-features', '--locked']);
await run('unit-contracts', 'cargo', ['test', '-p', 'vo-ui-native', '--features', 'jit', '--locked']);
await run('native-driver', 'cargo', ['build', '-p', 'vo-ui-native', '--features', 'jit', '--example', 'exchange', '--locked']);
const driverName = process.platform === 'win32' ? 'exchange.exe' : 'exchange';
await copyFile(resolve(root, 'target/debug/examples', driverName), resolve(directory, driverName));
await run('application', compilerPath(), ['emit', 'bytecode', 'ui/next/examples/interaction', '-o', resolve(root, 'target/ui-next/interaction.vob')]);

// Resolve the fixture from the repository's authenticated exact versions. A
// separate manifest supplies only the process entry, never a second UI runtime.
const dependency = (name, features = []) => `${name} = { path = ${JSON.stringify(resolve(root, 'lang/crates', name))}, default-features = false, features = ${JSON.stringify(features)} }`;
await writeFile(resolve(runtime, 'Cargo.toml'), `[package]\nname = "vo-ui-native-aot-probe"\nversion = "0.0.0"\nedition = "2021"\n[workspace]\n[lib]\ncrate-type = ["staticlib"]\n[dependencies]\n${[
  dependency('vo-ui-native', ['aot']), dependency('vo-ui-bridge'), dependency('vo-aot-runtime-core'),
  dependency('vo-runtime', ['std']), dependency('vo-vm', ['native']),
].join('\n')}\n`);
await copyFile(resolve(root, 'Cargo.lock'), resolve(runtime, 'Cargo.lock'));
await writeFile(resolve(runtime, 'src/lib.rs'), `#[path = ${JSON.stringify(resolve(root, 'lang/crates/vo-ui-native/examples/support/mod.rs'))}]\nmod support;\ninclude!(${JSON.stringify(resolve(root, 'lang/crates/vo-ui-native/examples/support/aot-entry.rs'))});\n`);
await run('resolve-aot-runtime', 'cargo', ['update', '--workspace', '--offline', '--manifest-path', resolve(runtime, 'Cargo.toml')]);
const lockedPackages = text => new Set(text.split('[[package]]').filter(block => /^source = "registry\+/m.test(block))
  .map(block => ['name', 'version', 'source', 'checksum'].map(key => block.match(new RegExp(`^${key} = "([^"]+)"`, 'm'))?.[1]).join('\n')));
const canonical = lockedPackages(await readFile(resolve(root, 'Cargo.lock'), 'utf8'));
for (const entry of lockedPackages(await readFile(resolve(runtime, 'Cargo.lock'), 'utf8'))) {
  assert(canonical.has(entry), `AOT probe changed a locked dependency:\n${entry}`);
}
await run('aot-runtime', 'cargo', ['build', '--locked', '--offline', '--manifest-path', resolve(runtime, 'Cargo.toml'), '--target-dir', resolve(root, 'target')]);
await run('aot-dependencies', 'cargo', ['tree', '--locked', '--offline', '--manifest-path', resolve(runtime, 'Cargo.toml'), '-e', 'normal']);
const dependencies = await readFile(resolve(directory, 'aot-dependencies.log'), 'utf8');
// The existing native stdlib's module resolver includes vo-analysis. This is
// shared native-runtime policy; keep the UI probe independent of its redesign.
for (const dependency of ['vo-codegen ', 'cranelift-codegen ']) {
  assert(!dependencies.includes(dependency), `native AOT runtime unexpectedly includes ${dependency}`);
}
const archive = resolve(root, 'target/debug', process.platform === 'win32' ? 'vo_ui_native_aot_probe.lib' : 'libvo_ui_native_aot_probe.a');
const executable = resolve(directory, process.platform === 'win32' ? 'interaction-aot.exe' : 'interaction-aot');
await run('linked-aot', compilerPath(), ['build', 'ui/next/examples/interaction', '--no-cache', `--runtime=${archive}`, '-o', executable]);
await run('failure-bytecode', compilerPath(), ['emit', 'bytecode', 'ui/next/tests/native_failure', '-o', resolve(directory, 'failure.vob')]);
const failure = resolve(directory, process.platform === 'win32' ? 'failure-aot.exe' : 'failure-aot');
await run('failure-linked-aot', compilerPath(), ['build', 'ui/next/tests/native_failure', '--no-cache', `--runtime=${archive}`, '-o', failure]);
for (const [backend, command, args] of [
  ['vm', resolve(directory, driverName), ['vm', resolve(directory, 'failure.vob')]],
  ['jit', resolve(directory, driverName), ['jit', resolve(directory, 'failure.vob')]],
  ['aot', failure, []],
]) {
  const result = spawnSync(command, args, {cwd: root, env: {...process.env,VOWORK:'off'}, encoding:'utf8',timeout:30_000});
  await writeFile(resolve(directory, `failure-${backend}.log`), result.stdout + result.stderr);
  assert.ifError(result.error);
  assert.equal(result.status, 1);
  assert.equal(result.stdout, '');
  assert.match(result.stderr, /PanicUnwound/);
  assert.match(result.stderr, /native UI 中文 failure regression/);
  assert.match(result.stderr, /loc: Some/);
  steps.push({name:`failure-${backend}`, command:[command,...args],expectedExit:1,passed:true});
}
await writeFile(resolve(directory, 'build.json'), JSON.stringify({passed: true, steps}, null, 2) + '\n');
console.log(`Native UI session artifacts ready: ${executable}`);
