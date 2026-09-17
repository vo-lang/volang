// One optional editor owns one lazy worker and at most one pending query. Abort
// terminates synchronous Wasm analysis too; the next request starts cleanly.
export function studioLanguageService(workers) {
  return ({input,signal:lifetime})=>{
    if(!['playground-source','ui-playground-source'].includes(input.id))return undefined;
    const ui=input.id==='ui-playground-source';let worker,pending,sequence=0;
    const stop=()=>{if(worker){worker.terminate();worker=undefined;workers.stopped++;}};
    const cancel=()=>{pending?.finish(new DOMException('Code query cancelled.','AbortError'));stop();};
    lifetime.addEventListener('abort',cancel,{once:true});
    function request(kind,{source,position,signal}) {
      if(lifetime.aborted||signal.aborted)return Promise.reject(new DOMException('Code query cancelled.','AbortError'));
      if(pending)cancel();
      return new Promise((resolve,reject)=>{
        const id=++sequence;let timer,finished=false;
        const abort=()=>{finish(new DOMException('Code query cancelled.','AbortError'));stop();};
        const finish=(error,result)=>{
          if(finished)return;finished=true;clearTimeout(timer);signal.removeEventListener('abort',abort);
          if(pending?.id===id)pending=undefined;
          if(error){stop();reject(error);}else resolve(result);
        };
        pending={id,finish};signal.addEventListener('abort',abort,{once:true});
        try {
          if(!worker) {
            worker=new Worker('/studio-assets/language-worker.js',{type:'module',name:'volang-code-information'});workers.started++;
            worker.onmessage=event=>{if(event.data.id===pending?.id)pending.finish(event.data.error?new Error(event.data.error):null,event.data.result);};
            worker.onerror=event=>{event.preventDefault();pending?.finish(new Error(event.message||'Code information could not start.'));stop();};
            worker.onmessageerror=()=>{pending?.finish(new Error('Code information could not be read.'));stop();};
          }
          timer=setTimeout(()=>finish(new Error('Code information took too long. Try again.')),20000);
          worker.postMessage({id,kind,source,position,ui});
        }catch(error){finish(error);}
      });
    }
    return {complete:query=>request('complete',query),definition:query=>request('definition',query)};
  };
}
