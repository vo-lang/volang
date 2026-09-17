import {mkdir,writeFile} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {stageStudioSite,verifyStudioSite,verifyStudioOrigin,separateSiteDirectories} from './studio-site.mjs';
import {studioSiteBudgets} from './studio-site-budgets.mjs';

export const siteUsage=`Usage: node eng/ui-next/studio-site-cli.mjs stage --source <static-directory> --output <site-directory> --domain <hostname>
       node eng/ui-next/studio-site-cli.mjs verify --directory <site-directory> [--origin <URL>]
       node eng/ui-next/studio-site-cli.mjs check --directory <site-directory> --output <report-directory> [--origin <URL>]
These commands prepare or verify files and HTTP responses. They do not deploy.`;

export function siteArguments(args) {
  const [command,...values]=args;
  if(['--help','help'].includes(command)||['stage','verify','check'].includes(command)&&values.length===1&&values[0]==='--help') return {help:true};
  const names={stage:['source','output','domain'],verify:['directory','origin'],check:['directory','output','origin']}[command];
  if(!names||values.length%2)throw new Error(siteUsage);
  const options={};
  for(let index=0;index<values.length;index+=2) {
    const name=values[index].slice(2),value=values[index+1];
    if(!values[index].startsWith('--')||!names.includes(name)||Object.hasOwn(options,name)||!value||value.startsWith('--'))throw new Error(siteUsage);
    options[name]=value;
  }
  const required=command==='stage'?['source','output','domain']:command==='check'?['directory','output']:['directory'];
  if(required.some(name=>!options[name]))throw new Error(siteUsage);
  return {command,options};
}

export async function checkStudioSite({directory,output,origin,signal}) {
  ({source:directory,destination:output}=await separateSiteDirectories(directory,output));
  await mkdir(output,{recursive:true});
  const file=join(output,'report.json');let browser,server;
  const stop=()=>{void browser?.close().catch(()=>{});};
  const write=value=>writeFile(file,JSON.stringify(value,null,2)+'\n');
  await write({schema:'volang.browser-result.v1',passed:false,report:{passed:false,complete:false,checks:[]}});
  signal?.addEventListener('abort',stop,{once:true});
  try {
    signal?.throwIfAborted();
    const budgets=studioSiteBudgets(await verifyStudioSite(directory));
    if(!origin){const {serveFiles}=await import('./static-server.mjs');server=await serveFiles(directory,{notFoundDocument:'404.html'});origin=server.url;}
    const before=await verifyStudioOrigin(directory,origin,{signal});
    process.env.PLAYWRIGHT_BROWSERS_PATH??=resolve('target/playwright-browsers');
    const {chromium}=await import('../browser/node_modules/playwright/index.mjs');
    const {checkStudio}=await import('./studio-contracts.mjs');
    browser=await chromium.launch();signal?.throwIfAborted();
    const application=await checkStudio(browser,new URL(origin).origin,output);
    if(!application.length||application.some(value=>!value.passed)||[...new Set(application.map(value=>value.backend))].sort().join(',')!=='vm')throw new Error('Studio site browser coverage is incomplete.');
    const after=await verifyStudioOrigin(directory,origin,{signal});
    if(JSON.stringify(before)!==JSON.stringify(after))throw new Error('Studio site changed during its browser check.');
    const result={schema:'volang.browser-result.v1',passed:true,report:{passed:true,complete:true,scope:'studio-site',
      checks:['candidate-files','deployment-budgets','http-file-identities','wasm-js-css-content-types','gallery-docs-playground','wasm-vm','post-journey-identities'],
      browserVersion:browser.version(),candidate:after,budgets,application}};
    await write(result);return result;
  }catch(error){await write({schema:'volang.browser-result.v1',passed:false,error:String(error),report:{passed:false,complete:false,checks:[]}});throw error;}
  finally{signal?.removeEventListener('abort',stop);try{await browser?.close();}finally{await server?.close();}}
}

export async function runSiteCommand(args) {
  const parsed=siteArguments(args);if(parsed.help){console.log(siteUsage);return;}
  const lifetime=new AbortController(),stop=()=>lifetime.abort(new Error('Studio site command cancelled.'));
  for(const signal of ['SIGINT','SIGTERM'])process.once(signal,stop);
  try {
    const {command,options}=parsed;
    let result;
    if(command==='stage')result={directory:await stageStudioSite({source:options.source,destination:options.output,domain:options.domain,signal:lifetime.signal})};
    else if(command==='check')result=await checkStudioSite({...options,signal:lifetime.signal});
    else {
      const candidate=await verifyStudioSite(options.directory),budgets=studioSiteBudgets(candidate);
      result={candidate:options.origin?await verifyStudioOrigin(options.directory,options.origin,{signal:lifetime.signal}):candidate,budgets};
    }
    console.log(JSON.stringify(result,null,2));
  }finally{for(const signal of ['SIGINT','SIGTERM'])process.off(signal,stop);}
}

if(process.argv[1]&&import.meta.url===pathToFileURL(process.argv[1]).href)await runSiteCommand(process.argv.slice(2)).catch(error=>{console.error(error.message);process.exitCode=1;});
