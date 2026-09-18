import assert from 'node:assert/strict';
import { spawn, spawnSync } from 'node:child_process';
import { resolve } from 'node:path';
import { root } from './server.mjs';

const args=process.argv.slice(2);
assert(args.every(arg=>['--compiler','--install-tools'].includes(arg)) && new Set(args).size===args.length,
  'Usage: node eng/ui-next/build-runtime.mjs [--compiler] [--install-tools]');
const compiler=args.includes('--compiler');
const features=['--no-default-features',...(compiler ? ['--features','compiler'] : [])];
const directory=resolve(root,'target/ui-next',compiler ? 'wasm-compiler' : 'wasm-runtime');
// Both Web packages own only the transport provider. Enforce the same narrow
// UI boundary for execution-only and compiler builds.
const graph=spawnSync('cargo',['tree','--locked','-p','vo-web',...features,
  '--target','wasm32-unknown-unknown','--edges','normal,no-proc-macro','--prefix','none'],{cwd:root,encoding:'utf8'});
assert.ifError(graph.error);assert.equal(graph.status,0,graph.stderr);
const uiPackages=new Set(graph.stdout.split('\n').map(line=>line.split(' ')[0]).filter(name=>name.startsWith('vo-ui-')));
assert.deepEqual([...uiPackages],['vo-ui-bridge'],'Web package includes an unexpected UI dependency');

// The compiler is downloaded only for Playground runs. The public npm package
// builds its compiler-enabled Web API into vo-web/pkg.
const child = spawn('wasm-pack', ['build', 'lang/crates/vo-web', '--target', 'web', '--release',
  '--mode', args.includes('--install-tools') ? 'normal' : 'no-install', '--out-dir', directory,
  '--', ...features, '--locked'], { cwd: root, env: { ...process.env, VOWORK: 'off' }, stdio: 'inherit' });
const code = await new Promise((resolve, reject) => { child.once('error', reject); child.once('exit', resolve); });
assert.equal(code, 0, 'Web package build failed');
