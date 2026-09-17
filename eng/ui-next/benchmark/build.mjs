import { build } from 'esbuild';
import { compile } from 'svelte/compiler';
import { parse, compileScript } from '@vue/compiler-sfc';
import { readFile, writeFile, mkdir, copyFile } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { createHash } from 'node:crypto';
import { gzipSync } from 'node:zlib';
import { root } from '../server.mjs';

const compiler = { name: 'reference-components', setup(build) {
  build.onLoad({ filter: /\.(svelte|vue)$/ }, async ({ path }) => {
    const source = await readFile(path, 'utf8');
    let contents;
    if (path.endsWith('.svelte')) {
      const result = compile(source, { filename: path, generate: 'client', dev: false });
      if (result.warnings.length) throw new Error(JSON.stringify(result.warnings));
      contents = result.js.code;
    } else {
      const { descriptor, errors } = parse(source, { filename: path });
      if (errors.length) throw new Error(JSON.stringify(errors));
      contents = compileScript(descriptor, { id: path, inlineTemplate: true, isProd: true }).content;
    }
    return { contents, resolveDir: dirname(path), loader: 'js' };
  });
} };
const outputs = [];
for (const driver of ['vue', 'svelte', 'vm']) {
  const directory = resolve(root, `target/ui-next/benchmark/${driver}`);
  await mkdir(directory, { recursive: true });
  const result = await build({ entryPoints: [resolve(root, `eng/ui-next/benchmark/${driver}.js`)], outfile: `${directory}/app.js`,
    bundle: true, minify: true, format: 'esm', platform: 'browser', target: 'es2022', conditions: ['browser'],
    define: { 'process.env.NODE_ENV': '"production"', __VUE_OPTIONS_API__: 'false', __VUE_PROD_DEVTOOLS__: 'false', __VUE_PROD_HYDRATION_MISMATCH_DETAILS__: 'false' },
    plugins: [compiler], metafile: true });
  await writeFile(`${directory}/bundle-meta.json`, JSON.stringify(result.metafile, null, 2) + '\n');
  await copyFile(resolve(root, 'eng/ui-next/benchmark/index.html'), `${directory}/index.html`);
  const files = ['index.html', 'app.js'];
  if (driver === 'vm') {
    await copyFile(resolve(root, 'target/ui-next/wasm-runtime/vo_web_bg.wasm'), `${directory}/vo_web_bg.wasm`);
    files.push('vo_web_bg.wasm');
  }
  if (['vm'].includes(driver)) {
    const extension = 'vob';
    await copyFile(resolve(root, `target/ui-next/benchmark.${extension}`), `${directory}/app.${extension}`);
    files.push(`app.${extension}`);
  }
  for (const file of files) {
    const bytes = await readFile(`${directory}/${file}`);
    outputs.push({ driver, file, bytes: bytes.length, gzipBytes: gzipSync(bytes, { level: 9 }).length,
      sha256: createHash('sha256').update(bytes).digest('hex') });
  }
}
const lock = JSON.parse(await readFile(resolve(root, 'eng/ui-next/package-lock.json'), 'utf8'));
await writeFile(resolve(root, 'target/ui-next/benchmark/build.json'), JSON.stringify({
  schema: 'volang.ui-next-comparison-build.v1', versions: Object.fromEntries(['vue', 'svelte', 'esbuild'].map(name => [name, lock.packages[`node_modules/${name}`].version])),
  outputs, options: { production: true, minify: true, target: 'es2022', bundled: true, voCompiler: 'bytecode emitted by current CLI' },
}, null, 2) + '\n');
console.log('Three production comparison pages built');
