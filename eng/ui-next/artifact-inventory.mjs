import {createHash} from 'node:crypto';
import {readFile,readdir} from 'node:fs/promises';
import {join,relative} from 'node:path';

export async function artifactInventory(directory, base = directory) {
  const result = [];
  for (const entry of await readdir(directory, {withFileTypes:true})) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) result.push(...await artifactInventory(path, base));
    else {
      const bytes = await readFile(path);
      result.push({path:relative(base, path).replaceAll('\\','/'), bytes:bytes.length,
        sha256:createHash('sha256').update(bytes).digest('hex')});
    }
  }
  return result.sort((a,b) => a.path.localeCompare(b.path,'en'));
}
