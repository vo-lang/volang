import assert from 'node:assert/strict';
import {access, readFile, readdir, rm, writeFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {setTimeout as delay} from 'node:timers/promises';
import {root} from './server.mjs';
import {buildStudio} from './studio-build.mjs';

// Only the server entry receives the temporary invalid file. This does not
// mutate the shared application or the delivered browser/preview source pack.
const output = resolve(root,'target/ui-next');
const report = resolve(output,'studio-distribution/build-report.json');
const before = await readFile(report);
const existing = new Set(await readdir(output));
const controller = new AbortController(), reason = new Error('Build cancelled after client output');
let staged = false;
const observing = (async () => {
  while (!controller.signal.aborted) {
    for (const directory of await readdir(output)) {
      if (!directory.startsWith('studio-staging-') || existing.has(directory)) continue;
      try {await access(resolve(output,directory,'public/artifacts/studio.vob'));}
      catch (error) {if(error.code === 'ENOENT') continue; throw error;}
      staged = true; controller.abort(reason); return;
    }
    await delay(50);
  }
})().catch(error=>controller.abort(error));
const deadline = setTimeout(()=>controller.abort(new Error('Client output did not appear')),30000);
try {
  await assert.rejects(buildStudio({signal:controller.signal}),error=>error===reason);
  assert(staged);
  assert.deepEqual(await readFile(report),before);
} finally {controller.abort(); clearTimeout(deadline); await observing;}
const invalid = resolve(root,`apps/studio/next/server/build_probe_${process.pid}.vo`);
let created = false;
try {
  await writeFile(invalid,'package main\nfunc unfinished(\n',{flag:'wx'}); created=true;
  await assert.rejects(buildStudio(),/parse error/);
  assert.deepEqual(await readFile(report),before);
} finally {if(created) await rm(invalid);}
for (const path of await readdir(output)) assert(existing.has(path) || !path.startsWith('studio-staging-'),`abandoned build: ${path}`);
await access(resolve(output,'studio-distribution/server/app.vob'));
await writeFile(resolve(output,'studio-build-contracts-report.json'),JSON.stringify({passed:true,
  cancelledAfterClientOutput:true,failedServerCompilation:true,lastDistributionPreserved:true,stagingRemoved:true},null,2)+'\n');
console.log('Studio builds: cancellation and native compile failure preserve the last complete distribution');
