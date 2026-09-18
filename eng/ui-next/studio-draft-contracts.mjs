import {setTimeout as delay} from 'node:timers/promises';

// Inspect committed storage independently of the Studio service implementation.
async function queryStudioDraft({key}) {
  const db=await new Promise((resolve,reject)=>{
    const request=indexedDB.open('volang.studio.next.drafts.v1',1);
    request.onupgradeneeded=()=>request.transaction.abort();
    request.onsuccess=()=>resolve(request.result);request.onerror=()=>request.error?.name==='AbortError'?resolve(null):reject(request.error);
  });
  try {
    if(!db?.objectStoreNames.contains('values'))return null;
    return await new Promise((resolve,reject)=>{
      const tx=db.transaction('values','readonly'),request=tx.objectStore('values').get(key);
      tx.oncomplete=()=>resolve(request.result??null);tx.onabort=()=>reject(tx.error);
    });
  } finally {db?.close();}
}

export async function waitStudioDraft(page,value,key='volang.studio.next.draft.v1',{timeout=30000}={}) {
  const deadline=performance.now()+timeout;
  // waitForFunction treats a Promise as truthy before its result is known.
  // Await each committed read here, then decide whether another poll is needed.
  for (;;) {
    if(await readStudioDraft(page,key)===value)return;
    const remaining=deadline-performance.now();
    if(remaining<=0)throw new Error(`Studio draft ${key} did not commit within ${timeout} ms`);
    await delay(Math.min(25,remaining));
  }
}

export async function readStudioDraft(page,key='volang.studio.next.draft.v1') {
  return page.evaluate(queryStudioDraft,{key});
}

// Seed the same committed store the application reads; no compatibility import.
export async function seedStudioDrafts(page, values) {
  await page.addInitScript(values=>{
    if (window !== window.top) return;
    window.seededStudioDrafts=new Promise((resolve,reject)=>{
      const opening=indexedDB.open('volang.studio.next.drafts.v1',1);
      opening.onupgradeneeded=()=>opening.result.createObjectStore('values');
      opening.onerror=()=>reject(opening.error);
      opening.onsuccess=()=>{
        const database=opening.result,transaction=database.transaction('values','readwrite');
        for(const [key,value] of Object.entries(values))transaction.objectStore('values').put(value,key);
        transaction.oncomplete=()=>{database.close();resolve();};
        transaction.onabort=()=>{database.close();reject(transaction.error);};
      };
    });
  },values);
}
