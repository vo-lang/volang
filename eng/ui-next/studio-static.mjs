import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cp, mkdir, mkdtemp, readFile, readdir, rename, rm, writeFile} from 'node:fs/promises';
import {dirname, join, resolve, sep} from 'node:path';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {root} from './server.mjs';
import {artifactInventory} from './artifact-inventory.mjs';
import {prepareHtml, renderOutput} from './prerender.mjs';
import {maxPages, maxPagesHtmlBytes, staticPagePath} from './prerender-pages.mjs';
import {renderPage} from './server-response.mjs';
import {precompressAssets} from './precompress.mjs';

export function studioStaticPages(paths) {
  if (!Array.isArray(paths) || !paths.length || paths.length > maxPages) throw new Error('Invalid Studio static page count.');
  const seen = new Set();
  return paths.map(path => {
    const page = staticPagePath(path);
    if (!page.path.startsWith('/studio/')) throw new Error('Studio pages must live under /studio/.');
    const key = page.path.toLowerCase();
    if (seen.has(key)) throw new Error('Duplicate Studio static page: ' + path);
    seen.add(key);
    return page;
  });
}

export function studioStaticRedirects(values,pages) {
  if (!Array.isArray(values) || pages.length+values.length > maxPages) throw new Error('Invalid Studio redirect count.');
  const targets=new Set(pages.map(page=>page.path)),seen=new Set([...targets].map(path=>path.toLowerCase()));
  const result=values.map(value=>{
    if (!value || typeof value !== 'object' || Object.keys(value).sort().join(',') !== 'from,to') throw new Error('Invalid Studio redirect.');
    const from=staticPagePath(value.from),to=staticPagePath(value.to);
    if (!targets.has(to.path) || seen.has(from.path.toLowerCase())) throw new Error('Studio redirect collides with a page or has no target.');
    seen.add(from.path.toLowerCase());return {...from,to:to.path};
  });
  if (!result.some(page=>page.path==='/')) throw new Error('Studio redirects must include the root.');
  return result;
}

function redirectDocument(to) {
  const href=to.split('/').map(encodeURIComponent).join('/');
  return `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Volang Studio</title><link rel="stylesheet" href="/ui-kit/theme.css"><link rel="stylesheet" href="/studio-assets/studio.css">
<noscript><meta http-equiv="refresh" content="0;url=${href}"></noscript></head>
<body class="vui"><main class="studio-message studio-intro"><p class="studio-eyebrow">VOLANG STUDIO</p>
<h1>Continue exploring.</h1><p>Follow the link to the current Studio.</p>
<a id="studio-redirect" href="${href}">Open Studio</a></main>
<script type="module" src="/studio-assets/redirect.js"></script></body></html>\n`;
}

/** Export one verified native distribution. Rendering uses its compiled route
 * catalog and server protocol directly, without opening a listener. */
export async function exportStudio({source = join(root,'target/ui-next/studio-distribution'),
  destination = join(root,'target/ui-next/studio-static'), executable = compilerPath(), signal} = {}) {
  source = resolve(source); destination = resolve(destination);
  if (source === destination || source.startsWith(destination+sep) || destination.startsWith(source+sep)) throw new Error('Studio export must be separate from its source distribution.');
  signal?.throwIfAborted();
  const reportBytes = await readFile(join(source,'build-report.json'));
  const build = JSON.parse(reportBytes);
  if (build.schema !== 'volang.studio-next-distribution.v1') throw new Error('Unsupported Studio distribution.');
  const verify = async () => assert.deepEqual((await artifactInventory(source)).filter(item=>item.path !== 'build-report.json'), build.artifacts, 'Studio distribution changed; rebuild it before exporting.');
  await verify();
  const options = {cwd:source, env:{...process.env,VOWORK:'off'}, signal};
  const artifact = join(source,'server/app.vob');
  const paths = JSON.parse(await renderOutput(executable,['run',artifact,'--','--static-paths'],{...options,maxBytes:65536}));
  const pages = studioStaticPages(paths);
  const redirects = studioStaticRedirects(JSON.parse(await renderOutput(executable,['run',artifact,'--','--static-redirects'],{...options,maxBytes:65536})),pages);
  const template = await readFile(join(source,'server/document.html'),'utf8');
  if (!template.includes('studio-assets/boot.js')) throw new Error('Studio static boot outlet is missing.');
  const compose = prepareHtml(template,true);
  // One static 404 serves every unknown URL. Its native links work without
  // starting a guest whose route would disagree with that shared HTML.
  const composeMissing = prepareHtml(template.replace(/\s*<script\b[^>]*>[\s\S]*?<\/script>/g,'')
    .replace(/\s*<(p|div) id="status"[^>]*>[\s\S]*?<\/\1>/,''),true);
  await mkdir(dirname(destination),{recursive:true});
  const stage = await mkdtemp(join(dirname(destination),'studio-static-staging-')), backup = stage+'-previous';
  let previous = false, htmlBytes = 0;
  try {
    const results = [];
    for (const location of [...pages,{path:'/missing',file:'404.html'}]) {
      signal?.throwIfAborted();
      const page = await renderPage(executable,artifact,{method:'GET',url:location.path,basePath:'/',headers:{},body:''},options);
      const expected = location.file === '404.html' ? 404 : 200;
      if (page.kind !== 'html' || page.status !== expected || !page.html || page.entry !== 'default' || Object.keys(page.headers).length) throw new Error('Studio page cannot be exported: ' + location.path);
      const html = (expected === 404 ? composeMissing : compose)(page.html,{data:page.data,assets:'/',title:page.title,description:page.description});
      htmlBytes += Buffer.byteLength(html);
      if (htmlBytes > maxPagesHtmlBytes) throw new Error('Studio static HTML exceeds the build budget.');
      const file = join(stage,location.file);
      await mkdir(dirname(file),{recursive:true});
      await writeFile(file,html,{flag:'wx'});
      results.push({path:location.path,file:location.file,status:page.status,title:page.title});
    }
    for (const redirect of redirects) {
      const html=redirectDocument(redirect.to);
      htmlBytes+=Buffer.byteLength(html);
      if (htmlBytes > maxPagesHtmlBytes) throw new Error('Studio static HTML exceeds the build budget.');
      const file=join(stage,redirect.file);await mkdir(dirname(file),{recursive:true});
      await writeFile(file,html,{flag:'wx'});
    }
    const compressed = await precompressAssets(stage,{signal});
    for (const name of await readdir(join(source,'public'))) await cp(join(source,'public',name),join(stage,name),{recursive:true,errorOnExist:true,force:false});
    for (const name of ['LICENSE','THIRD_PARTY_NOTICES.txt']) await cp(join(source,name),join(stage,name));
    await writeFile(join(stage,'.nojekyll'),'');
    await writeFile(join(stage,'README.md'),'# Volang Studio static preview\n\nDeploy this entire directory at the origin root on a static HTTP host with\ndirectory indexes. Use 404.html as its missing-page document, keeping HTTP 404.\nServe Wasm as application/wasm and JavaScript modules with a JavaScript MIME type.\nThe host needs no Volang compiler, Node runtime, source checkout or API service.\nGallery and all chapters include HTML before JavaScript; interactive examples\ncompile locally in browser workers. Chapter paths work without JavaScript. Publish all files together and revalidate stable URLs.\nThe .gz and .br siblings are optional precompressed representations; configure\nContent-Encoding when serving them. Subdirectory deployment is not supported.\n');
    await verify();
    assert.deepEqual(await readFile(join(source,'build-report.json')),reportBytes,'Studio build identity changed during export.');
    const report = {schema:'volang.studio-next-static.v1',sourceBuildSha256:createHash('sha256').update(reportBytes).digest('hex'),
      wireVersion:build.wireVersion,backends:build.backends,base:'/',documents:build.documents,
      pages:results,redirects,compressed:[...build.compressed,...compressed],artifacts:await artifactInventory(stage)};
    await writeFile(join(stage,'build-report.json'),JSON.stringify(report,null,2)+'\n');
    signal?.throwIfAborted();
    try {await rename(destination,backup);previous=true;} catch(error) {if(error.code !== 'ENOENT') throw error;}
    try {await rename(stage,destination);} catch(error) {if(previous) {await rename(backup,destination);previous=false;} throw error;}
    if(previous) await rm(backup,{recursive:true,force:true});
    return destination;
  } finally {await rm(stage,{recursive:true,force:true});}
}
