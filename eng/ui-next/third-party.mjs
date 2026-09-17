import {readFile, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';

// Metafile inputs are relative to the bundler's working directory. Toolchain
// dependencies outside an application may already have absolute input paths.
export async function thirdPartyNotices({inputs, workingDirectory, directory}) {
  const roots = new Set(Object.keys(inputs).flatMap(path => {
    const match = resolve(workingDirectory, path).replaceAll('\\', '/').match(/^(.*\/node_modules\/(?:@[^/]+\/)?[^/]+)\//);
    return match ? [match[1]] : [];
  }));
  const packages = [], notices = [];
  for (const path of [...roots].sort()) {
    const metadata = JSON.parse(await readFile(join(path, 'package.json'), 'utf8'));
    const license = await readFile(join(path, 'LICENSE'), 'utf8');
    packages.push({name:metadata.name, version:metadata.version, license:metadata.license});
    notices.push(`${metadata.name} ${metadata.version}\n\n${license.trim()}\n`);
  }
  if (packages.length) await writeFile(join(directory, 'THIRD_PARTY_NOTICES.txt'), notices.join('\n----------\n\n'));
  return packages;
}
