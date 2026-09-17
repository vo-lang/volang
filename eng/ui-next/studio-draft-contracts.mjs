// Inspect committed storage independently of the Studio service implementation.
async function queryStudioDraft({key,value,compare}) {
  const db=await new Promise((resolve,reject)=>{
    const request=indexedDB.open('volang.studio.next.drafts.v1',1);
    request.onupgradeneeded=()=>request.transaction.abort();
    request.onsuccess=()=>resolve(request.result);request.onerror=()=>request.error?.name==='AbortError'?resolve(null):reject(request.error);
  });
  try {
    if(!db?.objectStoreNames.contains('values'))return compare?false:null;
    return await new Promise((resolve,reject)=>{
      const tx=db.transaction('values','readonly'),request=tx.objectStore('values').get(key);
      tx.oncomplete=()=>resolve(compare?request.result===value:request.result??null);tx.onabort=()=>reject(tx.error);
    });
  } finally {db?.close();}
}

export async function waitStudioDraft(page,value,key='volang.studio.next.draft.v1') {
  await page.waitForFunction(queryStudioDraft,{key,value,compare:true});
}

export async function readStudioDraft(page,key='volang.studio.next.draft.v1') {
  return page.evaluate(queryStudioDraft,{key,compare:false});
}
