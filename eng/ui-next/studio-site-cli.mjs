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

export function siteFailureDetails(error,phase) {
  const causes=[],seen=new Set();
  for(let cause=error;cause&&!seen.has(cause)&&causes.length<6;cause=cause.cause) {
    seen.add(cause);
    causes.push({name:String(cause.name??'Error').slice(0,128),code:String(cause.code??'').slice(0,128),message:String(cause.message??cause).slice(0,4096)});
  }
  return {phase,causes,stack:String(error?.stack??error).slice(0,8192)};
}

async function failedPages(browser,output) {
  if(!browser)return [];
  return Promise.all(browser.contexts().flatMap(context=>context.pages()).slice(-3).map(async(page,index)=>{
    const result={url:page.url()};
    const text=selector=>page.locator(selector).first().textContent({timeout:1000}).then(value=>value?.slice(0,8192)).catch(()=>null);
    [result.consoleOutput,result.previewStatus]=await Promise.all([text('[data-output]'),text('[data-preview-status]')]);
    const path=join(output,`failure-page-${index}.png`);
    try {await page.screenshot({path,timeout:3000});result.screenshot=path;}catch(error){result.screenshotError=String(error).slice(0,1024);}
    return result;
  }));
}

export async function checkStudioSite({directory,output,origin,signal}) {
  ({source:directory,destination:output}=await separateSiteDirectories(directory,output));
  await mkdir(output,{recursive:true});
  const file=join(output,'report.json');let browser,server,phase='candidate';
  const stop=()=>{void browser?.close().catch(()=>{});};
  const write=value=>writeFile(file,JSON.stringify(value,null,2)+'\n');
  await write({schema:'volang.browser-result.v1',passed:false,report:{passed:false,complete:false,checks:[]}});
  signal?.addEventListener('abort',stop,{once:true});
  try {
    signal?.throwIfAborted();
    const budgets=studioSiteBudgets(await verifyStudioSite(directory));
    if(!origin){const {serveFiles}=await import('./static-server.mjs');server=await serveFiles(directory,{notFoundDocument:'404.html'});origin=server.url;}
    phase='origin-before';
    const before=await verifyStudioOrigin(directory,origin,{signal});
    phase='browser-start';
    process.env.PLAYWRIGHT_BROWSERS_PATH??=resolve('target/playwright-browsers');
    const {chromium}=await import('../browser/node_modules/playwright/index.mjs');
    const {checkStudio}=await import('./studio-contracts.mjs');
    browser=await chromium.launch();signal?.throwIfAborted();
    phase='browser-journey';
    const application=await checkStudio(browser,new URL(origin).origin,output);
    if(!application.length||application.some(value=>!value.passed)||[...new Set(application.map(value=>value.backend))].sort().join(',')!=='vm')throw new Error('Studio site browser coverage is incomplete.');
    phase='origin-after';
    const after=await verifyStudioOrigin(directory,origin,{signal});
    if(JSON.stringify(before)!==JSON.stringify(after))throw new Error('Studio site changed during its browser check.');
    const result={schema:'volang.browser-result.v1',passed:true,report:{passed:true,complete:true,scope:'studio-site',
      checks:['candidate-files','deployment-budgets','http-file-identities','wasm-js-css-content-types','gallery-docs-playground','wasm-vm','post-journey-identities'],
      browserVersion:browser.version(),candidate:after,budgets,application}};
    await write(result);return result;
  }catch(error){
    const failure=siteFailureDetails(error,phase);
    try {failure.pages=await failedPages(browser,output);}catch(diagnosticError){failure.diagnosticError=String(diagnosticError).slice(0,1024);}
    await write({schema:'volang.browser-result.v1',passed:false,error:String(error),failure,report:{passed:false,complete:false,checks:[]}});throw error;
  }
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

if(process.argv[1]&&import.meta.url===pathToFileURL(process.argv[1]).href)await runSiteCommand(process.argv.slice(2)).catch(error=>{console.error(error);process.exitCode=1;});
