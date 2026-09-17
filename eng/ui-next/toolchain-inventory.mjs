import {createHash} from 'node:crypto';
import {createReadStream} from 'node:fs';
import {readFile,readdir,stat} from 'node:fs/promises';
import {join} from 'node:path';
import {decodeToolchainManifest,portablePath,resolveToolchain} from './toolchain-manifest.mjs';

export async function inventory(directory,{signal} = {}) {
  const files = [];
  async function visit(relative) {
    for (const entry of await readdir(join(directory,relative),{withFileTypes:true})) {
      signal?.throwIfAborted();
      const path = relative ? relative + '/' + entry.name : entry.name;
      if (path === 'tools/toolchain.json') continue;
      if (!portablePath(path)) throw new Error(`Nonportable toolchain path: ${path}`);
      if (entry.isDirectory()) await visit(path);
      else if (entry.isFile()) {
        if (files.length >= 30000) throw new Error('UI toolchain exceeds 30,000 files.');
        const sha = createHash('sha256');
        for await (const bytes of createReadStream(join(directory,path),{signal})) sha.update(bytes);
        files.push({path,bytes:(await stat(join(directory,path))).size,sha256:sha.digest('hex')});
      } else throw new Error(`UI toolchain contains a link or special file: ${path}`);
    }
  }
  await visit('');
  return files.sort((a,b) => a.path < b.path ? -1 : a.path > b.path ? 1 : 0);
}

export async function verifyToolchain(directory,options = {}) {
  const bytes = await readFile(join(directory,'tools/toolchain.json'));
  const manifest = decodeToolchainManifest(bytes);
  resolveToolchain(manifest,directory);
  const expected = manifest.artifacts;
  if (!Array.isArray(expected) || !expected.length || expected.length > 30000
      || new Set(expected.map(file => file?.path)).size !== expected.length
      || expected.some(file => !file || !portablePath(file.path) || !Number.isSafeInteger(file.bytes) || file.bytes < 0 || !/^[a-f0-9]{64}$/.test(file.sha256))) throw new Error('Invalid UI toolchain artifact inventory.');
  for (const path of Object.values(manifest.paths)) {
    if (!expected.some(file => file.path === path || file.path.startsWith(path + '/'))) throw new Error(`Missing toolchain resource in inventory: ${path}`);
  }
  const actual = await inventory(directory,options), byPath = new Map(expected.map(file => [file.path,file]));
  for (const file of actual) {
    const known = byPath.get(file.path);
    if (!known || known.bytes !== file.bytes || known.sha256 !== file.sha256) throw new Error(`UI toolchain artifact mismatch: ${file.path}`);
    byPath.delete(file.path);
  }
  if (byPath.size) throw new Error(`UI toolchain artifact missing: ${byPath.keys().next().value}`);
  return {files:actual.length,bytes:actual.reduce((sum,file) => sum + file.bytes,0),platform:manifest.platform,arch:manifest.arch,wireVersion:manifest.wireVersion};
}
