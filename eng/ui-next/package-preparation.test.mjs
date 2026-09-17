import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {cp, mkdir, mkdtemp, readFile, rm, writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import test from 'node:test';
import {root} from './repository-paths.mjs';

test('npm preparation rejects a partial runtime before changing archive ignore rules', async () => {
  const directory=await mkdtemp(join(tmpdir(),'volang-partial-package-'));
  try {
    await cp(join(root,'lang/crates/vo-web/prepare-package.mjs'),join(directory,'prepare-package.mjs'));
    await writeFile(join(directory,'package.json'),JSON.stringify({exports:{'./wasm':{import:'./pkg/runtime.js',types:'./pkg/runtime.d.ts'}}}));
    await mkdir(join(directory,'pkg'));
    await writeFile(join(directory,'pkg/runtime.js'),'export {};');
    await writeFile(join(directory,'pkg/.npmignore'),'existing rule\n');
    const missingTypes=spawnSync(process.execPath,['prepare-package.mjs'],{cwd:directory,encoding:'utf8',timeout:10_000});
    assert.ifError(missingTypes.error);assert.notEqual(missingTypes.status,0);assert.match(missingTypes.stderr,/runtime\.d\.ts/);
    await writeFile(join(directory,'pkg/runtime.d.ts'),'export {};');
    const missingWasm=spawnSync(process.execPath,['prepare-package.mjs'],{cwd:directory,encoding:'utf8',timeout:10_000});
    assert.ifError(missingWasm.error);assert.notEqual(missingWasm.status,0);assert.match(missingWasm.stderr,/vo_web_bg\.wasm/);
    assert.equal(await readFile(join(directory,'pkg/.npmignore'),'utf8'),'existing rule\n');
  } finally {await rm(directory,{recursive:true,force:true});}
});
