import {projectTemplates,templateNames} from './project-templates.mjs';
import { createHash, randomUUID } from 'node:crypto';
import { access, cp, mkdir, mkdtemp, readFile, readdir, rename, rm, writeFile } from 'node:fs/promises';
import { basename, dirname, extname, join, relative, resolve } from 'node:path';
import { build as bundle } from './node_modules/esbuild/lib/main.js';
import {compilerPath,toolchain} from './toolchain.mjs';
import { renderHtml, assetsMarker } from './prerender.mjs';
import { maxPagesHtmlBytes } from './prerender-pages.mjs';
import { buildServerHost } from './server-build.mjs';
import { serverProtocol } from './server-request.mjs';
import { precompressAssets } from './precompress.mjs';
import {projectHost} from './project-features.mjs';
import {thirdPartyNotices} from './third-party.mjs';
import {entryAssets} from './project-entries.mjs';
import {loadProject,prepareProjectWeb} from './project-config.mjs';
export {readProject} from './project-config.mjs';
import {execute} from './execute.mjs';
export {execute} from './execute.mjs';

export async function createProject(directory, { template = 'default', signal } = {}) {
  signal?.throwIfAborted();
  if (!Object.hasOwn(projectTemplates, template)) throw new Error('Template must be ' + templateNames.join(', ') + '.');
  directory = resolve(directory);
  const parent = dirname(directory);
  await access(parent);
  await mkdir(directory); // Existing projects, including empty directories, are never overwritten.
  try {
    await cp(join(toolchain.ui, 'next/templates/default'), directory, { recursive: true });
    const selected=projectTemplates[template];
    if (template !== 'default' && selected.directory) await cp(join(toolchain.ui,'next',selected.directory),directory,{recursive:true});
    for (const [source,destination] of selected.files ?? []) {
      signal?.throwIfAborted();
      await cp(join(toolchain.ui,'next',source),join(directory,destination));
    }
    const name = basename(directory).toLowerCase().replace(/[^a-z0-9-]/g, '-').replace(/^-+|-+$/g, '') || 'app';
    const module = `local/${name}`;
    for (const path of await files(directory)) {
      if (path.endsWith('.vo')) {
        const source = await readFile(join(directory, path), 'utf8');
        if (source.includes('{{module}}')) await writeFile(join(directory, path), source.replaceAll('{{module}}', () => module));
      }
    }
    await writeFile(join(directory, 'tests/browser/fixtures.mjs'),
      `const module = process.env.VO_UI_TESTING_MODULE;\nif (!module) throw new Error('Run these tests through the UI test command.');\nexport const {test, expect} = await import(module);\n`);
    const vendor = join(directory, 'vendor/ui');
    await mkdir(vendor, { recursive: true });
    const manifest = await readFile(join(toolchain.ui, 'vo.mod'), 'utf8');
    const version = manifest.match(/^version = "([0-9.]+)"$/m)?.[1];
    if (!version) throw new Error('Cannot read the experimental UI module version.');
    await writeFile(join(vendor, 'vo.mod'), manifest);
    await cp(toolchain.license, join(vendor, 'LICENSE'));
    const source = join(toolchain.ui, 'next');
    await cp(source, join(vendor, 'next'), { recursive: true, filter: path => {
      const parts = relative(source, path).split(/[\\/]/);
      return !parts.some(part => ['tests', 'examples', 'templates'].includes(part));
    } });
    await writeFile(join(directory, 'vo.mod'), `format = 1\nmodule = "local/${name}"\nversion = "0.1.0"\nvo = "0.1.4"\n\n[dependencies]\n"github.com/vo-lang/ui" = "^${version}"\n`);
    await writeFile(join(directory, 'vo.work'), 'format = 1\nmembers = [".", "vendor/ui"]\n');
    const schema = JSON.parse(await readFile(join(toolchain.ui, 'next/wire.schema.json'), 'utf8'));
    const templateConfig=projectTemplates[template].config ?? {};
    await writeFile(join(directory, 'ui-next.json'), JSON.stringify({ format: 1, wireVersion: schema.version,
      developmentEntry: 'development', prerenderEntry: 'prerender',
      document: { title: 'Your next idea · Volang UI', description: 'A small idea, built with Volang UI.' },
      ...(!templateConfig.serverEntry && !templateConfig.pageEntries
        ? {desktop:{identifier:`dev.volang.app${randomUUID().replaceAll('-','')}`}} : {}),
      ...templateConfig,
    }, null, 2) + '\n');
    // The shared application is ordinary Vo. Separate entry points let the
    // compiler omit the inspector and its dependencies from production.
    const entry = packageName => `package main\n\nimport (\n\t"${module}/app"\n\t"github.com/vo-lang/ui/next/${packageName}"\n)\n\nfunc main() {\n\t${packageName}.RunWithData(app.View)\n}\n`;
    await writeFile(join(directory, 'main.vo'), entry('host'));
    await mkdir(join(directory, 'development'));
    await writeFile(join(directory, 'development/main.vo'), entry('develop'));
    await mkdir(join(directory, 'prerender'));
    await writeFile(join(directory, 'prerender/main.vo'), `package main\n\nimport (\n\t"${module}/app"\n\t"github.com/vo-lang/ui/next/prerender"\n)\n\nfunc main() {\n\tprerender.Run(app.View)\n}\n`);
    await writeFile(join(directory, '.gitignore'), '/target/\n.volang/\n.vo-cache/\n');
    signal?.throwIfAborted();
    await execute(compilerPath(), ['work', 'sync', directory], { cwd: directory, signal, env: { ...process.env, VOWORK: join(directory, 'vo.work') } });
  } catch (error) {
    await rm(directory, { recursive: true, force: true });
    throw error;
  }
  return directory;
}

export async function checkProject(directory, { signal } = {}) {
  const project=await loadProject(directory);
  ({directory}=project);
  await prepareProjectWeb(project);
  await prepareProjectWeb(project,{development:true});
  const sources=new Set();
  for(const entry of project.entries.values()) {
    for(const key of ['entry','developmentEntry','prerenderEntry'])if(entry[key])sources.add(entry[key]);
  }
  for(const entry of [project.serverEntry,project.desktopEntry])if(entry)sources.add(entry);
  const env={...process.env,VOWORK:join(directory,'vo.work')},diagnostics=[];
  diagnostics.push(await execute(compilerPath(),['fmt','--check',directory],{cwd:directory,env,signal}));
  for(const entry of sources)diagnostics.push(await execute(compilerPath(),['check','--read-only','--',entry],{cwd:directory,env,signal}));
  signal?.throwIfAborted();
  const assets=join(directory,'target/ui-next/check/assets');
  await bundleProjectHost(project,{assets,write:false});
  if(project.config.developmentEntry)await bundleProjectHost(project,{assets,development:true,inspection:true,write:false});
  signal?.throwIfAborted();
  return {directory,entries:sources.size,diagnostics:diagnostics.filter(value=>value.trim()).join('\n')};
}

function bundleProjectHost({directory,features},{assets,development=false,inspection=false,write=true}) {
  return bundle({
    absWorkingDir:directory,entryPoints:[join(directory,'web/boot.js')],outdir:assets,
    entryNames:'app',chunkNames:'chunks/[name]-[hash]',assetNames:'[name]-[hash]',
    bundle:true,splitting:true,format:'esm',platform:'browser',target:'es2020',
    minify:!development,sourcemap:development,metafile:true,write,
    plugins:[projectHost({features,development:inspection})],
  });
}

async function files(directory, base = directory) {
  const result = [];
  for (const entry of await readdir(directory, { withFileTypes: true })) {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) result.push(...await files(path, base));
    else if (entry.isFile()) result.push(relative(base, path).replaceAll('\\', '/'));
  }
  return result.sort();
}

// Build into an unpublished directory. Only successful complete outputs replace
// the previous build; failed compilation never destroys a working preview.
export async function buildProject(directory, { development = false, developmentRevision, signal } = {}) {
  const project=await loadProject(directory);
  ({directory}=project);
  const {features,serverEntry,pages}=project;
  const {inspection,used,compiled,prerender,template,composeHtml}=await prepareProjectWeb(project,{development});
  if (developmentRevision !== undefined && (!development || !serverEntry || typeof developmentRevision !== 'string' || !/^[a-z0-9-]{1,80}$/.test(developmentRevision))) {
    throw new Error('A development revision requires a server entry and a bounded revision token.');
  }
  const outputs = join(directory, 'target/ui-next');
  await mkdir(outputs, { recursive: true });
  const stage = await mkdtemp(join(outputs, 'staging-'));
  const publicDirectory = serverEntry ? join(stage, 'public') : stage;
  const destination = developmentRevision === undefined ? join(outputs, development ? 'dev' : 'dist')
    : join(outputs, 'dev-revisions', developmentRevision);
  await mkdir(dirname(destination), {recursive:true});
  const backup = stage + '-previous';
  const prepared = stage + '.prerender';
  let previous = false;
  try {
    const assets = join(publicDirectory, 'assets');
    await mkdir(assets, {recursive:true});
    const env = { ...process.env, VOWORK: join(directory, 'vo.work') };
    if (prerender) await mkdir(prepared);
    const entryReports = new Map();
    for (const entry of compiled) {
      const image = entryAssets(entry.id);
      await mkdir(dirname(join(publicDirectory, image)), {recursive:true});
      if (inspection) await execute(compilerPath(), ['check', entry.entry], {cwd:directory, env, signal});
      await execute(compilerPath(), ['emit', 'bytecode', inspection ? entry.developmentEntry : entry.entry, '-o', join(publicDirectory, image + '.vob')], {cwd:directory, env, signal});
      const report = {id:entry.id, entry:relative(directory, entry.entry).replaceAll('\\', '/') || '.',
        bytecode:relative(stage, join(publicDirectory, image + '.vob')).replaceAll('\\', '/'),
        developmentEntry:inspection ? relative(directory, entry.developmentEntry).replaceAll('\\', '/') : undefined,
        prerender:null};
      if (prerender && entry.prerenderEntry) {
        await execute(compilerPath(), ['emit', 'bytecode', entry.prerenderEntry, '-o', join(prepared, entry.id + '.vob')], {cwd:directory, env, signal});
        const bytes = await readFile(join(prepared, entry.id + '.vob'));
        report.prerender = {entry:relative(directory, entry.prerenderEntry).replaceAll('\\', '/'),
          artifact:{bytes:bytes.length, sha256:createHash('sha256').update(bytes).digest('hex')}};
      }
      entryReports.set(entry.id, report);
    }
    let serverArtifact;
    if (serverEntry) {
      const serverDirectory = join(stage, 'server');
      const serverTemplate = development ? template.replace('</body>', `<script type="module" src="${assetsMarker}assets/development.js"></script></body>`) : template;
      await buildServerHost(serverDirectory, serverTemplate, {entries:[...used]});
      const path = join(serverDirectory, 'app.vob');
      await execute(compilerPath(), ['emit', 'bytecode', serverEntry, '-o', path], {cwd:directory, env, signal});
      const bytes = await readFile(path);
      serverArtifact = {bytes:bytes.length, sha256:createHash('sha256').update(bytes).digest('hex')};
    }
    signal?.throwIfAborted();
    await cp(join(directory, 'web'), publicDirectory, { recursive: true, filter: path => path !== join(directory, 'web/boot.js') });
    if (serverEntry) await rm(join(publicDirectory, 'index.html'));
    const renderedPages = [];
    let rootHtml, rootEntry, totalHtmlBytes = 0;
    for (const page of pages) {
      signal?.throwIfAborted();
      const entry = entryReports.get(page.entry ?? 'default');
      const html = entry.prerender ? await renderHtml(compilerPath(), ['run', join(prepared, entry.id + '.vob')], { cwd: directory, env, signal, input: page.data }) : undefined;
      if (page.path === '/') { rootHtml = html; rootEntry = entry; }
      let document = composeHtml(html, page);
      if (development) document = document.replace('</body>', `<script type="module" src="${page.assets}assets/development.js"></script></body>`);
      const bytes = Buffer.byteLength(document);
      totalHtmlBytes += bytes;
      if (totalHtmlBytes > maxPagesHtmlBytes) throw new Error('Static pages exceed the combined 64 MiB HTML budget.');
      const destination = join(publicDirectory, page.file);
      if (page.path !== '/') {
        try { await access(destination); throw new Error(`Static page ${page.path} collides with an authored web file.`); }
        catch (error) { if (error.code !== 'ENOENT') throw error; }
      }
      await mkdir(dirname(destination), { recursive: true });
      await writeFile(destination, document);
      renderedPages.push({ path: page.path, entry:entry.id, file: page.file, bytes, sha256: createHash('sha256').update(document).digest('hex'),
        title: page.title, description: page.description,
        dataBytes: Buffer.byteLength(page.data), dataSha256: createHash('sha256').update(page.data).digest('hex') });
    }
    await cp(join(directory, 'vendor/ui/next/kit/theme.css'), join(publicDirectory, 'theme.css'));
    await cp(toolchain.vm, join(assets, 'vm'), {
      recursive: true, filter: path => !basename(path).startsWith('.') && !['.ts', '.json', '.md'].includes(extname(path)),
    });
    const bundled=await bundleProjectHost(project,{assets,development,inspection});
    // The optional regexp provider is a lazy Wasm-bindgen module whose binary
    // resolves alongside its emitted JavaScript chunk.
    await mkdir(join(assets, 'chunks'), { recursive: true });
    if (development) {
      await cp(join(toolchain.host, 'development.js'), join(assets, 'development.js'));
    }
    const thirdParty = await thirdPartyNotices({inputs:bundled.metafile.inputs, workingDirectory:directory, directory:stage});
    const compressed = development ? [] : await precompressAssets(publicDirectory,{signal});
    const artifacts = [];
    for (const path of await files(stage)) {
      const bytes = await readFile(join(stage, path));
      artifacts.push({ path, bytes: bytes.length, sha256: createHash('sha256').update(bytes).digest('hex') });
    }
    await writeFile(join(stage, 'build-report.json'), JSON.stringify({
      schema: 'volang.ui-next-application.v1', mode: development ? 'development' : 'production',
      inspection,
      features, thirdParty,
      server: serverEntry ? {entry:relative(directory, serverEntry).replaceAll('\\', '/'), artifact:serverArtifact,
        host:'server/entry.mjs', public:'public', requestProtocol:serverProtocol} : null,
      prerender: rootHtml === undefined ? null : { ...rootEntry.prerender,
        htmlBytes: Buffer.byteLength(rootHtml), sha256: createHash('sha256').update(rootHtml).digest('hex') },
      entries:[...entryReports.values()],
      pages: renderedPages,
      backends: development ? ['vm'] : ['vm'], artifacts, compressed,
      javascriptBytes: Object.values(bundled.metafile.outputs).reduce((sum, output) => sum + output.bytes, 0),
    }, null, 2) + '\n');
    signal?.throwIfAborted();
    try { await rename(destination, backup); previous = true; } catch (error) { if (error.code !== 'ENOENT') throw error; }
    try { await rename(stage, destination); } catch (error) { if (previous) { await rename(backup, destination); previous = false; } throw error; }
    if (previous) { await rm(backup, { recursive: true, force: true }); previous = false; }
    return destination;
  } finally {
    await rm(prepared, { recursive:true, force: true });
    await rm(stage, { recursive: true, force: true });
  }
}
