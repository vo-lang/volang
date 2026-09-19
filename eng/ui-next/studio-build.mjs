import {access, cp, mkdir, mkdtemp, readFile, rename, rm, writeFile} from 'node:fs/promises';
import {basename, extname, join, resolve} from 'node:path';
import {build as bundle} from './node_modules/esbuild/lib/main.js';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {root} from './server.mjs';
import {artifactInventory} from './artifact-inventory.mjs';
import {execute} from './project.mjs';
import {buildPlaygroundSources} from './playground-sources.mjs';
import {verifyStudioDocuments} from './studio-documents.mjs';
import {buildServerHost} from './server-build.mjs';
import {serverProtocol} from './server-request.mjs';
import {precompressAssets} from './precompress.mjs';
import {thirdPartyNotices} from './third-party.mjs';
import {assetsMarker, contentMarker, dataMarker, descriptionMarker, modeMarker, prepareHtml, titleMarker} from './prerender.mjs';

// Keep Studio's application and worker entry points. Resolve the lab's host
// imports at build time; delivered modules contain no repository URL or path.
export const studioHostImports = {
  name:'studio-host-imports',
  setup(build) {
    build.onResolve({filter:/^\/artifacts\/(editor)-library\.js$/}, ({path}) => ({path:resolve(root, 'eng/ui-next', basename(path, '.js') + '.mjs')}));
    build.onResolve({filter:/^\/host\//}, async ({path}) => {
      const source = resolve(root, 'lang/crates/vo-web/js', path.slice('/host/'.length));
      try {await access(source); return {path:source};}
      catch (error) {if (error.code !== 'ENOENT') throw error;}
      return {path:source.replace(/\.js$/, '.ts')};
    });
  },
};

export async function buildStudio({signal} = {}) {
  await verifyStudioDocuments(root);
  const output = resolve(root, 'target/ui-next');
  await mkdir(output, {recursive:true});
  const stage = await mkdtemp(join(output, 'studio-staging-'));
  const destination = join(output, 'studio-distribution'), backup = stage + '-previous';
  const publicDirectory = join(stage, 'public'), assets = join(publicDirectory, 'studio-assets');
  const source = resolve(root, 'apps/studio/next');
  const env = {...process.env, VOWORK:resolve(root, 'vo.work')};
  let previous = false;
  try {
    await mkdir(assets, {recursive:true});
    await mkdir(join(publicDirectory, 'artifacts'));
    for (const [args, name] of [
      [['emit','bytecode',source], 'studio.vob'],
    ]) await execute(compilerPath(), [...args,'-o',join(publicDirectory,'artifacts',name)], {env,signal});
    let template = await readFile(join(source,'index.html'),'utf8');
    template = template.replace('<div id="root"></div>', `<div id="root" data-rendering="${modeMarker}">${contentMarker}</div>`)
      .replace('<title>Volang Studio · Make something good</title>', `<title>${titleMarker}</title>`)
      .replace(/<meta name="description" content="[^"]*">/, `<meta name="description" content="${descriptionMarker}">`)
      .replace('type="application/json">""</script>', `type="application/json">${dataMarker}</script>`)
      .replaceAll('href="/', `href="${assetsMarker}`).replaceAll('src="/', `src="${assetsMarker}`);
    prepareHtml(template, true)('', {data:'', assets:'/', title:'', description:''});
    await buildServerHost(join(stage,'server'), template, {fixedBase:'/',
      assetDirectories:['artifacts','studio-assets','studio-docs','ui-kit','wasm','compiler','host']});
    await execute(compilerPath(), ['emit','bytecode',join(source,'server'),'-o',join(stage,'server/app.vob')], {env,signal});
    await cp(await buildPlaygroundSources(), join(publicDirectory,'artifacts/playground-ui.json'));
    await mkdir(join(publicDirectory,'studio-docs'));
    const docs = JSON.parse(await readFile(join(source,'documentation/index.json'),'utf8'));
    for (const item of [...docs.pages,docs.search]) await cp(join(source,'documentation',item.Asset), join(publicDirectory,'studio-docs',item.Asset));
    for (const name of ['studio.css','preview.html']) await cp(join(source,name),join(assets,name));
    await cp(join(source,'cache-retirement-worker.js'),join(publicDirectory,'service-worker.js'));
    await mkdir(join(publicDirectory,'ui-kit'));
    await cp(resolve(root,'ui/next/kit/theme.css'),join(publicDirectory,'ui-kit/theme.css'));
    for (const [from,to] of [['target/ui-next/wasm-runtime','wasm'],['target/ui-next/wasm-compiler','compiler']]) {
      await cp(resolve(root,from),join(publicDirectory,to),{recursive:true, filter:path =>
        !basename(path).startsWith('.') && !['.ts','.json','.md'].includes(extname(path))});
    }
    const bundled = await bundle({
      absWorkingDir:root, entryPoints:Object.fromEntries(['boot','studio-worker','redirect','runner','preview','ui-runner','language-worker'].map(name=>[name,join(source,name+'.js')])),
      outdir:assets, chunkNames:'chunks/[name]-[hash]', bundle:true, splitting:true,
      format:'esm', platform:'browser', target:'es2022', minify:true, metafile:true,
      define:{STUDIO_COMPRESSED:'true'},
      plugins:[studioHostImports], external:['/compiler/*','/wasm/*'],
    });
    await mkdir(join(assets,'chunks'),{recursive:true});
    await cp(resolve(root,'LICENSE'),join(stage,'LICENSE'));
    await writeFile(join(stage,'README.md'), '# Volang Studio\n\nRequires Node 24 and a matching Volang CLI. Run `node server/entry.mjs`, then\nopen `/studio/gallery`. `HOST`, `PORT` and `VO_EXECUTABLE` configure the host.\nDeploy at the origin root; subdirectory mounting is not supported by this build.\nKeep this entire directory immutable while running. Source and node_modules are\nnot required. Console and UI compilers download when an example is run or code information is requested.\n');
    const compressed = await precompressAssets(publicDirectory,{signal});
    const thirdParty = await thirdPartyNotices({inputs:bundled.metafile.inputs, workingDirectory:root, directory:stage});
    const report = {schema:'volang.studio-next-distribution.v1',wireVersion:JSON.parse(await readFile(resolve(root,'ui/next/wire.schema.json'))).version,
      serverProtocol, backends:['vm'], base:'/', documents:docs.pages.length,
      compiler:(await execute(compilerPath(),['version'],{signal})).trim(),
      browserInputs:Object.keys(bundled.metafile.inputs).sort(), thirdParty, compressed, artifacts:await artifactInventory(stage)};
    await writeFile(join(stage,'build-report.json'), JSON.stringify(report,null,2)+'\n');
    signal?.throwIfAborted();
    try {await rename(destination,backup); previous = true;} catch(error) {if(error.code !== 'ENOENT') throw error;}
    try {await rename(stage,destination);} catch(error) {if(previous) {await rename(backup,destination); previous=false;} throw error;}
    if(previous) await rm(backup,{recursive:true,force:true});
    return destination;
  } finally {await rm(stage,{recursive:true,force:true});}
}
