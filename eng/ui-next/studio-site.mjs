import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cp,mkdir,mkdtemp,readFile,realpath,rename,rm,writeFile} from 'node:fs/promises';
import {basename,dirname,join,resolve,sep} from 'node:path';
import {artifactInventory} from './artifact-inventory.mjs';

const hash=bytes=>createHash('sha256').update(bytes).digest('hex');
const identity=(path,bytes)=>({path,bytes:bytes.length,sha256:hash(bytes)});
const sort=values=>values.sort((a,b)=>a.path.localeCompare(b.path,'en'));
async function outputPath(path) {
  try{return await realpath(path);}catch(error){
    if(error.code!=='ENOENT')throw error;
    return join(await outputPath(dirname(path)),basename(path));
  }
}

// Resolve existing ancestors as well as the final path, including /tmp aliases
// and symlinks, before creating any output directories or reports.
export async function separateSiteDirectories(source,destination) {
  source=await realpath(source);destination=await outputPath(resolve(destination));
  if(source===destination||source.startsWith(destination+sep)||destination.startsWith(source+sep)) throw new Error('Site output must be separate from the input distribution.');
  return {source,destination};
}
function domainName(value) {
  if(typeof value!=='string'||value.length>253||value!==value.toLowerCase()||!value.includes('.')||value.split('.').some(label=>!/^([a-z0-9]|[a-z0-9][a-z0-9-]{0,61}[a-z0-9])$/.test(label))) throw new Error('Studio domain must be a lowercase DNS hostname without a scheme, path or port.');
  return value;
}
function buildRecord(bytes) {
  const build=JSON.parse(bytes);
  if(build.schema!=='volang.studio-next-static.v1'||!Array.isArray(build.artifacts)) throw new Error('Expected a verified static Studio distribution.');
  if(!Number.isSafeInteger(build.wireVersion)||build.wireVersion<1) throw new Error('Invalid Studio wire identity.');
  for(const entry of build.artifacts) {
    if(typeof entry.path!=='string'||entry.path.includes('\\')||entry.path.includes(':')||entry.path.split('/').some(part=>!part||part==='.'||part==='..')||!Number.isSafeInteger(entry.bytes)||entry.bytes<0||!/^[a-f0-9]{64}$/.test(entry.sha256)) throw new Error('Invalid Studio artifact identity.');
    if(['CNAME','site-manifest.json','build-report.json'].includes(entry.path)) throw new Error('Static Studio contains a reserved site file.');
  }
  return build;
}

/** Stage hosting metadata around an unchanged, verified static export. */
export async function stageStudioSite({source,destination,domain,signal}) {
  domain=domainName(domain);({source,destination}=await separateSiteDirectories(source,destination));
  signal?.throwIfAborted();
  const bytes=await readFile(join(source,'build-report.json')),build=buildRecord(bytes);
  assert.deepEqual(await artifactInventory(source),sort([...build.artifacts,identity('build-report.json',bytes)]),'Static Studio changed after verification.');
  await mkdir(dirname(destination),{recursive:true});
  const stage=await mkdtemp(join(dirname(destination),'studio-site-staging-')),backup=stage+'-previous';let previous=false;
  try {
    await cp(source,stage,{recursive:true});
    await writeFile(join(stage,'CNAME'),domain+'\n',{flag:'wx'});
    await writeFile(join(stage,'site-manifest.json'),JSON.stringify({schema:'volang.studio-site.v1',domain,buildSha256:hash(bytes)},null,2)+'\n',{flag:'wx'});
    await verifyStudioSite(stage);signal?.throwIfAborted();
    let exists=false;
    try{await realpath(destination);exists=true;}catch(error){if(error.code!=='ENOENT')throw error;}
    if(exists){
      const previousSite=JSON.parse(await readFile(join(destination,'site-manifest.json')));
      if(previousSite.schema!=='volang.studio-site.v1')throw new Error('Output is not an owned Studio site.');
      await rename(destination,backup);previous=true;
    }
    try{await rename(stage,destination);}catch(error){if(previous){await rename(backup,destination);previous=false;}throw error;}
    if(previous)await rm(backup,{recursive:true,force:true});
    return destination;
  }finally{await rm(stage,{recursive:true,force:true});}
}

/** Verify one complete candidate without rebuilding or rewriting its bytes. */
export async function verifyStudioSite(directory) {
  const bytes=await readFile(join(directory,'build-report.json')),build=buildRecord(bytes);
  const siteBytes=await readFile(join(directory,'site-manifest.json')),site=JSON.parse(siteBytes);
  if(site.schema!=='volang.studio-site.v1'||site.buildSha256!==hash(bytes)) throw new Error('Studio site has a different static build.');
  const cname=Buffer.from(domainName(site.domain)+'\n');
  const files=sort([...build.artifacts,identity('build-report.json',bytes),identity('site-manifest.json',siteBytes),identity('CNAME',cname)]);
  assert.deepEqual(await artifactInventory(directory),files,'Studio site files changed after staging.');
  return {schema:'volang.studio-site-verified.v1',domain:site.domain,wireVersion:build.wireVersion,buildSha256:site.buildSha256,files};
}

/** Compare the actual HTTP representations with one already verified candidate. */
export async function verifyStudioOrigin(directory,baseURL,{signal,fetch:request=fetch}={}) {
  signal?.throwIfAborted();
  const expected=await verifyStudioSite(directory),base=new URL(baseURL);
  if(!['http:','https:'].includes(base.protocol)||base.username||base.password||base.pathname!=='/'||base.search||base.hash) throw new Error('Studio deployment URL must name an HTTP origin.');
  const lifetime=new AbortController(),active=signal?AbortSignal.any([signal,lifetime.signal]):lifetime.signal;
  // Hosting configuration remains bound locally and in the certified artifact;
  // Pages need not expose it over HTTP. Every other file must be served intact.
  const hostingMetadata=expected.files.filter(entry=>['CNAME','.nojekyll'].includes(entry.path));
  const files=expected.files.filter(entry=>!hostingMetadata.includes(entry));
  let next=0,total=0;
  const check=async()=>{
    while(next<files.length) {
      active.throwIfAborted();const entry=files[next++];
      const url=new URL(entry.path.split('/').map(encodeURIComponent).join('/'),base);
      const response=await request(url,{signal:AbortSignal.any([active,AbortSignal.timeout(30000)]),redirect:'error',cache:'no-store',headers:{'Accept-Encoding':'identity'}});
      if(response.status!==200||!response.body) throw new Error('Deployed Studio file is unavailable: '+entry.path+' ('+response.status+')');
      const type=response.headers.get('content-type')??'';
      if(entry.path.endsWith('.wasm')&&!/^application\/wasm(?:;|$)/i.test(type)||/\.(?:m?js)$/.test(entry.path)&&!/^(?:text|application)\/javascript(?:;|$)/i.test(type)||entry.path.endsWith('.css')&&!/^text\/css(?:;|$)/i.test(type)) throw new Error('Deployed Studio has an incorrect content type: '+entry.path);
      const digest=createHash('sha256');let bytes=0;
      for await(const chunk of response.body){bytes+=chunk.length;if(bytes>entry.bytes)throw new Error('Deployed Studio file is too large: '+entry.path);digest.update(chunk);}
      if(bytes!==entry.bytes||digest.digest('hex')!==entry.sha256) throw new Error('Deployed Studio file differs from the candidate: '+entry.path);
      total+=bytes;
    }
  };
  const outcomes=await Promise.allSettled(Array.from({length:4},async()=>{try{await check();}catch(error){lifetime.abort(error);throw error;}}));
  if(outcomes.some(value=>value.status==='rejected'))throw lifetime.signal.reason;
  return {passed:true,origin:base.origin,domain:expected.domain,wireVersion:expected.wireVersion,buildSha256:expected.buildSha256,files:files.length,bytes:total,hostingMetadata};
}
