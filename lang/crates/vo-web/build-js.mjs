import {rm} from 'node:fs/promises';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';

const directory = new URL('./', import.meta.url);
// Removed source modules must not survive into upgraded npm/runtime packages.
await rm(new URL('dist/', directory), {recursive:true, force:true});
const result = spawnSync(process.execPath, [fileURLToPath(new URL('node_modules/typescript/bin/tsc', directory))], {
  cwd:fileURLToPath(directory), stdio:'inherit',
});
if (result.error) throw result.error;
if (result.signal) throw new Error(`TypeScript build stopped by ${result.signal}`);
process.exitCode = result.status ?? 1;
