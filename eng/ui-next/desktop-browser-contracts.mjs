import assert from 'node:assert/strict';
import {readFile, writeFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {resolve} from 'node:path';
import {root} from './repository-paths.mjs';
import {desktopAssets} from './desktop-assets.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
const browsers=await import('../browser/node_modules/playwright/index.mjs');
const directory=resolve(root,'target/ui-next/desktop');
await desktopAssets(resolve(directory,'assets'),{check:true});
const script=await readFile(resolve(directory,'assets/desktop.js'),'utf8');
const report={passed:false,scriptSha256:createHash('sha256').update(script).digest('hex'),browserVersions:{},cases:[]};
const save=()=>writeFile(resolve(directory,'browser-report.json'),JSON.stringify(report,null,2)+'\n');
await save();
for(const engine of ['chromium','firefox','webkit']) {
  const browser=await browsers[engine].launch({headless:true});
  report.browserVersions[engine]=browser.version();
  try {
    for(const name of ['orderly-close-replies-once','failure-is-literal-and-disposes','overlap-cancels-pending','invalid-sequence-fails','document-unload-releases-session']) {
      const page=await browser.newPage();
      try {
        await page.setContent('<div id="root"></div><script id="volang-desktop-config" type="application/json">{"token":"0123456789abcdef0123456789abcdef"}</script>');
        await page.evaluate(()=>{window.messages=[];window.ipc={postMessage(text){window.messages.push(JSON.parse(text));}};});
        await page.addScriptTag({content:script});
        assert.deepEqual(await page.evaluate(()=>window.messages),[{token:'0123456789abcdef0123456789abcdef',message:{kind:'start'}}]);
        assert.equal(await page.locator('#volang-desktop-config').count(),0);
        if(name==='invalid-sequence-fails') {
          await page.evaluate(()=>window.__volangDesktop.receive(2,''));
          assert.match(await page.locator('[role=alert]').innerText(),/Unexpected desktop exchange identity/);
          assert.equal(await page.evaluate(()=>window.messages[1].message.kind),'failure');
        } else {
          await page.evaluate(()=>{window.pending=window.__volangDesktop.receive(1,'');});
          if(name==='orderly-close-replies-once') {
            await page.evaluate(async()=>{window.__volangDesktop.close();await window.pending;window.__volangDesktop.dispose();});
            assert.deepEqual(await page.evaluate(()=>window.messages.map(item=>item.message)),[{kind:'start'},{kind:'reply',id:1,data:''}]);
          } else if(name==='failure-is-literal-and-disposes') {
            const message='中文 <img src=x onerror=alert(1)> failure';
            await page.evaluate(async message=>{window.__volangDesktop.failHost(message);await window.pending;await window.__volangDesktop.receive(2,'');},message);
            assert.equal(await page.locator('[role=alert] pre').innerText(),message);
            assert.equal(await page.locator('img').count(),0);
            assert.equal(await page.evaluate(()=>window.messages.length),1);
          } else if(name==='document-unload-releases-session') {
            await page.evaluate(async()=>{window.dispatchEvent(new PageTransitionEvent('pagehide'));await window.pending;});
            assert.deepEqual(await page.evaluate(()=>window.messages.map(item=>item.message.kind)),['start','failure']);
            assert.match(await page.locator('[role=alert]').innerText(),/application document closed/);
          } else {
            await page.evaluate(async()=>{await window.__volangDesktop.receive(2,'');await window.pending;});
            assert.deepEqual(await page.evaluate(()=>window.messages.map(item=>item.message.kind)),['start','failure']);
            assert.match(await page.locator('[role=alert]').innerText(),/Unexpected desktop exchange identity/);
          }
          assert.equal(await page.evaluate(()=>window.__volangDesktop.ready),false);
        }
        report.cases.push({engine,name,passed:true});await save();
      } finally {await page.close();}
    }
  } finally {await browser.close();}
}
report.passed=true;await save();console.log(`Desktop host: ${report.cases.length} browser contracts passed`);
