import assert from 'node:assert/strict';
import {cp,readFile,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';

// Exercise actual persistent WebView storage using production launchers. A
// repackage and relocation keep one identity; a second identity starts empty.
export async function checkDesktopStorage({project,work,stamp,compiler,run}) {
  const configPath=join(project,'ui-next.json');
  const bytes=await readFile(configPath),config=JSON.parse(bytes);
  const scriptPath=join(project,'web/check.js'),script=await readFile(scriptPath);
  const first=`dev.volang.storage-a-${stamp}`,second=`dev.volang.storage-b-${stamp}`;
  try {
    for(const [phase,identifier] of [['seed',first],['reopen',first],['isolate',second]]) {
      await writeFile(configPath,JSON.stringify({...config,desktop:{...config.desktop,identifier}}));
      await writeFile(scriptPath,`(async()=>{
        const host=window.__volangDesktop;
        try {
          if(!await host.ready)throw new Error('storage application closed before activation');
          const key='volang.storage.contract',value='Saved 中文 🌿';
          const actual=localStorage.getItem(key);
          if(${JSON.stringify(phase)}==='reopen') {
            if(actual!==value)throw new Error('draft did not survive reopening and relocation: '+actual);
          } else {
            if(actual!==null)throw new Error('application inherited another storage profile: '+actual);
            localStorage.setItem(key,value);
          }
          const db=await new Promise((resolve,reject)=>{
            const request=indexedDB.open('volang-storage-contract',1);
            request.onupgradeneeded=()=>request.result.createObjectStore('drafts');
            request.onsuccess=()=>resolve(request.result);request.onerror=()=>reject(request.error);
          });
          try {
            await new Promise((resolve,reject)=>{
              const transaction=db.transaction('drafts','readwrite'),store=transaction.objectStore('drafts');
              let failure;
              const request=store.get(key);
              request.onsuccess=()=>{
                const expected=${JSON.stringify(phase)}==='reopen'?value:undefined;
                if(request.result!==expected){failure=new Error('IndexedDB profile mismatch: '+request.result);transaction.abort();}
                else store.put(value,key);
              };
              transaction.oncomplete=resolve;
              transaction.onabort=()=>reject(failure??transaction.error??new Error('storage transaction aborted'));
              transaction.onerror=()=>reject(transaction.error);
            });
          } finally {db.close();}
          host.close();
        } catch(error){host.fail(String(error));}
      })();\n`);
      await run(`package-storage-${phase}`,compiler,['ui','package',project,'--backend','vm']);
      const output=join(project,'target/ui-desktop/dist-vm');
      const receipt=JSON.parse(await readFile(join(output,'build-report.json'),'utf8'));
      const resources=join(output,process.platform==='darwin'?'Application.app/Contents/Resources':'resources');
      const manifest=JSON.parse(await readFile(join(resources,'desktop.json'),'utf8'));
      assert.equal(manifest.identifier,identifier);
      const moved=join(work,`Storage ${phase} moved 中文`);
      await cp(output,moved,{recursive:true});
      await run(`window-storage-${phase}`,join(moved,receipt.executable),['--exit-on-failure'],{cwd:tmpdir(),env:{VO_UI_TOOLCHAIN:'missing'}});
    }
  } finally {
    await writeFile(configPath,bytes);await writeFile(scriptPath,script);
  }
}
