import {createReadStream, createWriteStream} from 'node:fs';
import {readdir, stat, rm} from 'node:fs/promises';
import {join, extname, relative} from 'node:path';
import {pipeline} from 'node:stream/promises';
import {createBrotliCompress, createGzip, constants} from 'node:zlib';

const extensions = new Set(['.html','.css','.js','.mjs','.json','.svg','.wasm','.vob']);

// Only unpublished build output is compressed. Each stream owns one file and
// cancellation joins it before the caller removes the build directory.
export async function precompressAssets(directory, {signal} = {}) {
  const outputs = [];
  async function visit(current) {
    for (const entry of await readdir(current,{withFileTypes:true})) {
      signal?.throwIfAborted();
      const path = join(current,entry.name);
      if (entry.isDirectory()) {await visit(path); continue;}
      if (!entry.isFile() || !extensions.has(extname(entry.name))) continue;
      const original = await stat(path);
      if (original.size < 1024) continue;
      const output = {path:relative(directory,path).replaceAll('\\','/'),bytes:original.size};
      for (const [encoding, extension, compress] of [
        ['gzip','.gz',()=>createGzip({level:6})],
        ['br','.br',()=>createBrotliCompress({params:{[constants.BROTLI_PARAM_QUALITY]:4,
          [constants.BROTLI_PARAM_SIZE_HINT]:Math.min(original.size,0xffffffff)}})],
      ]) {
        // Exclusive creation catches authored files that collide with a generated
        // representation. A failed build leaves the published directory alone.
        await pipeline(createReadStream(path),compress(),createWriteStream(path+extension,{flags:'wx'}),{signal});
        const size = (await stat(path+extension)).size;
        if (size + 64 < original.size) output[encoding] = size;
        else await rm(path+extension);
      }
      outputs.push(output);
    }
  }
  await visit(directory);
  return outputs.sort((a,b)=>a.path.localeCompare(b.path,'en'));
}
