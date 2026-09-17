import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdir,mkdtemp,readFile,rm,symlink,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {decodeToolchainManifest,resolveToolchain,portablePath,toolchainPathNames,toolchainSchema} from './toolchain-manifest.mjs';
import {inventory,verifyToolchain} from './toolchain-inventory.mjs';
import {buildToolchain} from './toolchain-build.mjs';

const manifest = () => ({schema:toolchainSchema,platform:process.platform,arch:process.arch,nodeMajor:24,wireVersion:18,serverProtocol:4,
  paths:Object.fromEntries(toolchainPathNames.map(name => [name,'resource/' + name]))});
test('portable manifest binds the complete platform and resource layout',() => {
  const value = manifest();
  assert.equal(resolveToolchain(value,'/installation').kind,'packaged');
  for (const path of ['', '/', '../compiler', 'a/../b', 'a//b', 'C:/compiler', 'a\\b', 'a\0b']) assert.equal(portablePath(path),false,path);
  for (const patch of [{schema:'old'},{platform:'another'},{arch:'another'},{nodeMajor:1},{wireVersion:0},{serverProtocol:0},{paths:{}}]) assert.throws(() => resolveToolchain({...value,...patch},'/installation'),/manifest/);
  assert.throws(() => resolveToolchain({...value,paths:{...value.paths,compiler:'../vo'}},'/installation'),/manifest/);
  assert.throws(() => decodeToolchainManifest(new Uint8Array(8*1024*1024+1)),/8 MiB/);
  assert.throws(() => decodeToolchainManifest(new Uint8Array([255])),/encoded data/);
});
test('packaging preserves existing destinations and cancels before writing',async () => {
  const directory=await mkdtemp(join(tmpdir(),'ui-toolchain-existing-'));
  try {
    await writeFile(join(directory,'keep'),'retained');
    await assert.rejects(buildToolchain(directory),/existing files are preserved/);
    assert.equal(await readFile(join(directory,'keep'),'utf8'),'retained');
    const controller=new AbortController();controller.abort(new Error('packaging cancelled'));
    await assert.rejects(buildToolchain(join(directory,'new'),{signal:controller.signal}),/packaging cancelled/);
  } finally {await rm(directory,{recursive:true,force:true});}
});
test('inventory catches missing, altered, unexpected and linked resources',async () => {
  const directory = await mkdtemp(join(tmpdir(),'ui-toolchain-inventory-'));
  try {
    await mkdir(join(directory,'tools'));await mkdir(join(directory,'resource'));
    for (const name of toolchainPathNames) await writeFile(join(directory,'resource',name),name);
    const value = {...manifest(),artifacts:await inventory(directory)};
    await writeFile(join(directory,'tools/toolchain.json'),JSON.stringify(value));
    assert.equal((await verifyToolchain(directory)).files,toolchainPathNames.length);
    await writeFile(join(directory,'resource/compiler'),'altered');
    await assert.rejects(verifyToolchain(directory),/artifact mismatch: resource\/compiler/);
    await writeFile(join(directory,'resource/compiler'),'compiler');
    await writeFile(join(directory,'unexpected'),'extra');
    await assert.rejects(verifyToolchain(directory),/artifact mismatch: unexpected/);
    await rm(join(directory,'unexpected'));await rm(join(directory,'resource/compiler'));
    await assert.rejects(verifyToolchain(directory),/artifact missing: resource\/compiler/);
    await symlink('host',join(directory,'resource/compiler'));
    await assert.rejects(verifyToolchain(directory),/link or special file/);
    await rm(join(directory,'resource/compiler'));await writeFile(join(directory,'resource/compiler'),'compiler');
    const before = await readFile(join(directory,'tools/toolchain.json'));
    const duplicate = {...value,artifacts:[...value.artifacts,value.artifacts[0]]};
    await writeFile(join(directory,'tools/toolchain.json'),JSON.stringify(duplicate));
    await assert.rejects(verifyToolchain(directory),/artifact inventory/);
    await writeFile(join(directory,'tools/toolchain.json'),before);
    assert.equal((await verifyToolchain(directory)).files,toolchainPathNames.length);
  } finally {await rm(directory,{recursive:true,force:true});}
});
