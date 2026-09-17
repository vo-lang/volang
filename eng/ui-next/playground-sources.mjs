import { readdir, readFile, writeFile, rename, mkdir } from 'node:fs/promises';
import { resolve, relative, join } from 'node:path';
import { root } from './server.mjs';

/** Package the current experimental module for explicit offline workspace
 * compilation. This asset is fetched only when a UI example is run. */
export async function buildPlaygroundSources() {
  const directory = resolve(root, 'ui/next'), files = [];
  async function visit(path) {
    for (const entry of await readdir(path, { withFileTypes: true })) {
      const next = join(path, entry.name);
      if (entry.isDirectory() && !['tests', 'examples', 'templates'].includes(entry.name)) await visit(next);
      else if (entry.isFile() && entry.name.endsWith('.vo')) {
        files.push({ path: 'vendor/ui/next/' + relative(directory, next).replaceAll('\\', '/'), text: await readFile(next, 'utf8') });
      }
    }
  }
  await visit(directory);
  files.sort((a, b) => a.path.localeCompare(b.path, 'en'));
  const manifest = await readFile(resolve(root, 'ui/vo.mod'), 'utf8');
  const version = manifest.match(/^version = "([0-9.]+)"$/m)?.[1];
  if (!version) throw new Error('Cannot read packaged UI module version');
  files.push({ path: 'vendor/ui/vo.mod', text: manifest },
    { path: 'vo.mod', text: `format = 1\nmodule = "local/studio-preview"\nversion = "0.1.0"\nvo = "0.1.4"\n\n[dependencies]\n"github.com/vo-lang/ui" = "^${version}"\n` },
    { path: 'vo.work', text: 'format = 1\nmembers = [".", "vendor/ui"]\n' });
  const schema = JSON.parse(await readFile(resolve(root, 'ui/next/wire.schema.json'), 'utf8'));
  const output = resolve(root, 'target/ui-next/playground-ui.json');
  await mkdir(resolve(root, 'target/ui-next'), { recursive: true });
  await writeFile(`${output}.${process.pid}.tmp`, JSON.stringify({ format: 1, wireVersion: schema.version, files }));
  await rename(`${output}.${process.pid}.tmp`, output);
  return output;
}
