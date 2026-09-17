// Inspect committed storage independently of the Studio service implementation.
export async function waitStudioDraft(page,value,key='volang.studio.next.draft.v1') {
  await page.waitForFunction(async({key,value})=>{
    const db=await new Promise((resolve,reject)=>{
      const request=indexedDB.open('volang.studio.next.drafts.v1',1);
      request.onupgradeneeded=()=>request.transaction.abort();
      request.onsuccess=()=>resolve(request.result);request.onerror=()=>request.error?.name==='AbortError'?resolve(null):reject(request.error);
    });
    try {
      if(!db?.objectStoreNames.contains('values'))return false;
      return await new Promise((resolve,reject)=>{
        const tx=db.transaction('values','readonly'),request=tx.objectStore('values').get(key);
        tx.oncomplete=()=>resolve(request.result===value);tx.onabort=()=>reject(tx.error);
      });
    } finally {db?.close();}
  },{key,value});
}
