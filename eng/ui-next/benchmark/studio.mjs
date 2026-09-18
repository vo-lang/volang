import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {mkdir,readFile,writeFile} from 'node:fs/promises';
import {cpus,platform,release,arch} from 'node:os';
import {join} from 'node:path';
import {root} from '../server.mjs';
import {serveFiles} from '../static-server.mjs';
import {artifactInventory} from '../artifact-inventory.mjs';
import {distribution} from './statistics.mjs';

const rounds=Number(process.env.UI_BENCH_ROUNDS ?? 3),samples=Number(process.env.UI_BENCH_SAMPLES ?? 20);
assert(Number.isSafeInteger(rounds) && rounds>0 && Number.isSafeInteger(samples) && samples>0);
const directory=join(root,'target/ui-next/studio-static'),output=join(root,'target/ui-next/benchmark/studio');
await mkdir(output,{recursive:true});
const buildBytes=await readFile(join(directory,'build-report.json')),build=JSON.parse(buildBytes);
const inventory=async()=>assert.deepEqual((await artifactInventory(directory)).filter(value=>value.path!=='build-report.json'),build.artifacts);
await inventory();
process.env.PLAYWRIGHT_BROWSERS_PATH??=join(root,'target/playwright-browsers');
const {chromium}=await import('../../browser/node_modules/playwright/index.mjs');
const server=await serveFiles(directory,{notFoundDocument:'404.html'});
let browser;
const report={schema:'volang.studio-next-performance.v1',passed:false,measuredAt:new Date().toISOString(),build,
  buildReportSha256:createHash('sha256').update(buildBytes).digest('hex'),
  environment:{cpu:cpus()[0].model,cores:cpus().length,platform:platform(),release:release(),arch:arch(),node:process.version},
  method:{rounds,samplesPerScenarioPerPage:samples,warmups:3,viewport:{width:1280,height:900},
    load:'canonical Gallery navigation to actual application-ready assignment; real Brotli/gzip HTTP on localhost',
    cache:'cold uses a fresh browser context; warm opens a second page in the same context; URLs revalidate normally; process/OS/Wasm caches may remain warm',
    interaction:'synthetic native button/checkbox click to matching DOM and forced layout; excludes paint and trusted-input overhead',
    dialog:'open measures native open state; close waits for the actual exit motion between samples and is not timed',
    instrumentation:'test-only application-state setter records readiness; page Resource Timing plus browser-context request sizes include dedicated Worker traffic',
    limits:['one host and Chromium engine','three loads per backend/cache by default','no network or mobile CPU throttling','no field INP/LCP or product certification']},runs:[]};

async function measure(page,name,count) {
  return page.evaluate(async({name,count})=>{
    const values=[];
    const waitFor=(act,check)=>new Promise((resolve,reject)=>{
      const timer=setTimeout(()=>{observer.disconnect();reject(new Error('Studio measurement timed out: '+name));},30000);
      const complete=()=>{
        if(!check()) return;
        document.querySelector('main').getBoundingClientRect();
        clearTimeout(timer);observer.disconnect();resolve(performance.now()-started);
      };
      const observer=new MutationObserver(complete);observer.observe(document.getElementById('root'),{subtree:true,attributes:true,childList:true,characterData:true});
      const started=performance.now();act();complete();
    });
    for(let index=0;index<count;index++) {
      await new Promise(requestAnimationFrame);
      if(name==='counter') {
        const output=document.querySelector('[data-demo-count]'),next=parseInt(output.textContent,10)+1;
        const button=[...document.querySelectorAll('button')].find(value=>value.textContent==='Make it happen');
        values.push(await waitFor(()=>button.click(),()=>parseInt(output.textContent,10)===next));
        if(output!==document.querySelector('[data-demo-count]')) throw new Error('Counter DOM was replaced.');
      } else if(name==='theme') {
        const shell=document.querySelector('.studio[data-theme]'),next=shell.dataset.theme==='dark'?'light':'dark';
        values.push(await waitFor(()=>document.getElementById('studio-theme').click(),()=>shell.dataset.theme===next));
      } else {
        values.push(await waitFor(()=>document.querySelector('[data-open-dialog]').click(),()=>document.getElementById('gallery-dialog')?.open));
        await waitFor(()=>document.querySelector('[data-close-dialog]').click(),()=>!document.getElementById('gallery-dialog')?.open);
      }
    }
    return values;
  },{name,count});
}

try {
  browser=await chromium.launch({headless:true});
  report.environment.browser=browser.version();
  for(let round=0;round<rounds;round++) for(const backend of ['vm']) {
    const context=await browser.newContext({viewport:report.method.viewport});
    try {
      await context.addInitScript(()=>{
        window.studioPerformance={};let state;
        Object.defineProperty(window,'__studioNext',{configurable:true,get:()=>state,set:value=>{
          state=value;let ready=value.ready;
          Object.defineProperty(value,'ready',{configurable:true,enumerable:true,get:()=>ready,set:next=>{
            ready=next;if(next && window.studioPerformance.readyAt===undefined) window.studioPerformance.readyAt=performance.now();
          }});
        }});
      });
      for(const cache of ['cold','warm']) {
        const page=await context.newPage(),errors=[],network=[];
        page.on('pageerror',error=>errors.push(error.message));
        const finished=request=>network.push(request.sizes().then(sizes=>({url:request.url(),...sizes})));
        context.on('requestfinished',finished);
        try {
          await page.goto(server.url+'studio/gallery/?backend='+backend);
          await page.waitForFunction(()=>window.__studioNext?.ready || window.__studioNext?.error);
          assert.equal(await page.evaluate(()=>window.__studioNext.error),null);
          const load=await page.evaluate(()=>{
            document.querySelector('main').getBoundingClientRect();
            const entries=[...performance.getEntriesByType('navigation'),...performance.getEntriesByType('resource')];
            return {readyMs:window.studioPerformance.readyAt,paints:performance.getEntriesByType('paint').map(value=>({name:value.name,ms:value.startTime})),
              resources:entries.map(value=>({url:value.name,transferBytes:value.transferSize,encodedBytes:value.encodedBodySize,decodedBytes:value.decodedBodySize}))};
          });
          load.network=await Promise.all(network);
          assert(load.network.some(value=>/vo_web_bg.wasm/.test(value.url)), 'runtime traffic was not observed');
          assert(Number.isFinite(load.readyMs));
          assert(!load.network.some(value=>/\/compiler\/|editor-library/.test(value.url)),'Gallery eagerly loaded an optional tool');
          const run={backend,cache,round,load,scenarios:{}};
          for(const name of ['counter','theme','dialog']) {await measure(page,name,3);run.scenarios[name]=await measure(page,name,samples);}
          assert.equal(await page.evaluate(()=>window.__studioNext.error),null);assert.deepEqual(errors,[]);
          report.runs.push(run);await writeFile(join(output,'report.partial.json'),JSON.stringify(report,null,2)+'\n');
          console.log(`${backend} ${cache} round ${round+1}: ready ${load.readyMs.toFixed(1)} ms; `+Object.entries(run.scenarios).map(([name,values])=>`${name} ${distribution(values).p50.toFixed(2)} ms`).join(', '));
        } finally {context.off('requestfinished',finished);await Promise.allSettled(network);await page.close();}
      }
    } finally {await context.close();}
  }
  report.summary={};
  for(const backend of ['vm']) {
    const runs=report.runs.filter(value=>value.backend===backend);
    report.summary[backend]={loads:Object.fromEntries(['cold','warm'].map(cache=>{
      const selected=runs.filter(value=>value.cache===cache);
      return [cache,{readyMs:distribution(selected.map(value=>value.load.readyMs)),
        responseBytes:distribution(selected.map(value=>value.load.network.reduce((sum,item)=>sum+item.responseHeadersSize+item.responseBodySize,0)))}];
    })),scenarios:Object.fromEntries(['counter','theme','dialog'].map(name=>[name,distribution(runs.flatMap(value=>value.scenarios[name]))]))};
  }
  await inventory();assert.deepEqual(await readFile(join(directory,'build-report.json')),buildBytes);
  report.passed=true;
} catch(error) {
  report.error=String(error?.stack ?? error);throw error;
} finally {
  try {await browser?.close();} finally {await server.close();}
  await writeFile(join(output,'report.json'),JSON.stringify(report,null,2)+'\n');
}
