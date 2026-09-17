import {readdir} from 'node:fs/promises';
import {dirname,join} from 'node:path';
import {fileURLToPath} from 'node:url';

// Every top-level unit contract belongs to the core gate. Integration drivers
// remain explicit because they own different builds, browsers and lifetimes.
export async function unitContracts() {
  const directory = dirname(fileURLToPath(import.meta.url));
  return (await readdir(directory)).filter(name => name.endsWith('.test.mjs')).sort().map(name => join(directory,name));
}
