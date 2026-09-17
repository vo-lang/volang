import {createHash} from 'node:crypto';
import {readFile} from 'node:fs/promises';
import {join} from 'node:path';

// These separately loaded modules execute the browser tests and ship in Studio.
// Bind their JavaScript and Wasm bytes alongside the compiled application images.
export async function webArtifactInventory(root) {
  const files=[['wasm-runtime','wasm'],['wasm-compiler','compiler']].flatMap(([source,destination]) =>
    ['vo_web.js','vo_web_bg.wasm'].map(name => ({
      path:`target/ui-next/${source}/${name}`,deliveryPath:`public/${destination}/${name}`,
    })));
  return Promise.all(files.map(async value => {
    const bytes=await readFile(join(root,value.path));
    return {...value,bytes:bytes.length,sha256:createHash('sha256').update(bytes).digest('hex')};
  }));
}
