import {readFile} from 'node:fs/promises';
import {dirname,join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {decodeToolchainManifest,resolveToolchain} from './toolchain-manifest.mjs';

const directory = dirname(fileURLToPath(import.meta.url));
let selected;
try {
  const bytes = await readFile(join(directory,'toolchain.json'));
  selected = resolveToolchain(decodeToolchainManifest(bytes),dirname(directory));
} catch (error) {
  if (error.code !== 'ENOENT') throw error;
  try { selected = (await import('./toolchain-source.mjs')).default; }
  catch (cause) { throw new Error('UI toolchain manifest is missing. Restore the complete toolchain installation.',{cause}); }
}
export const toolchain = Object.freeze(selected);
export const compilerPath = () => toolchain.compiler;
