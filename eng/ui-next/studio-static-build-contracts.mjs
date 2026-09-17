import assert from 'node:assert/strict';
import {access,readFile,readdir,writeFile} from 'node:fs/promises';
import {setTimeout as delay} from 'node:timers/promises';
import {join} from 'node:path';
import {root} from './server.mjs';
import {exportStudio} from './studio-static.mjs';
import {artifactInventory} from './artifact-inventory.mjs';

const output=join(root,'target/ui-next');
const directory=await exportStudio();
const first=await artifactInventory(directory);
await exportStudio();
assert.deepEqual(await artifactInventory(directory),first,'identical native inputs produced different static files');
const existing=new Set(await readdir(output));
const controller=new AbortController(),reason=new Error('Cancel an active static render');
let rendered=false;
const observing=(async()=>{
  while(!controller.signal.aborted) {
    for(const name of await readdir(output)) {
      if(!name.startsWith('studio-static-staging-') || existing.has(name)) continue;
      try {await access(join(output,name,'studio/index.html'));}
      catch(error) {if(error.code==='ENOENT') continue;throw error;}
      rendered=true;controller.abort(reason);return;
    }
    await delay(10);
  }
})().catch(error=>controller.abort(error));
const deadline=setTimeout(()=>controller.abort(new Error('No static page was rendered')),30000);
try {
  await assert.rejects(exportStudio({signal:controller.signal}),error=>error===reason);
  assert(rendered,'cancellation must happen after native rendering has started');
  assert.deepEqual(await artifactInventory(directory),first,'cancelled export changed the previous site');
} finally {controller.abort();clearTimeout(deadline);await observing;}
for(const name of await readdir(output)) assert(existing.has(name) || !name.startsWith('studio-static-staging-'),'abandoned static export: '+name);
const build=JSON.parse(await readFile(join(directory,'build-report.json')));
await writeFile(join(output,'studio-static-build-report.json'),JSON.stringify({passed:true,build,
  repeatable:true,cancelledAfterNativeRender:true,lastSitePreserved:true,stagingRemoved:true},null,2)+'\n');
console.log('Studio static export: identical files on repeat; cancellation preserves the previous site and joins native work');
