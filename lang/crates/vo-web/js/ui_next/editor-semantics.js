import {validSourceRange} from './editor-service.js';

/** @param {any} library
 * @param {import('./editor-service.js').EditorLanguageService | undefined} service
 * @param {AbortSignal} lifetime */
export function editorSemanticExtensions(library,service,lifetime) {
  if (!service) return [];
  const {EditorView,ViewPlugin,StateEffect,StateField,showPanel,keymap,autocompletion}=library;
  const showDefinition=StateEffect.define();
  const peek=StateField.define({
    create:()=>null,
    update(value,transaction) {
      if (transaction.docChanged) value=null;
      for (const effect of transaction.effects) if (effect.is(showDefinition)) value=effect.value;
      return value;
    },
    provide:field=>showPanel.from(field,target=>target ? view=>{
      const document=view.dom.ownerDocument,dom=document.createElement('section');
      dom.className='vui-code-definition';dom.setAttribute('role','region');dom.setAttribute('aria-label','Definition');
      const header=document.createElement('div'),title=document.createElement('strong'),close=document.createElement('button');
      const {source,range,file}=target;
      const before=source.slice(0,range.start),after=source.slice(range.end);
      title.textContent=file+' · line '+before.split('\n').length;
      close.type='button';close.textContent='Back to source';
      close.addEventListener('click',()=>{view.dispatch({effects:showDefinition.of(null)});view.focus();});
      header.append(title,close);
      const code=document.createElement('pre'),mark=document.createElement('mark');
      mark.textContent=source.slice(range.start,range.end);
      code.append(before.split('\n').slice(-3).join('\n'),mark,after.split('\n').slice(0,3).join('\n'));
      dom.append(header,code);return {dom};
    }:null),
  });
  const requests=ViewPlugin.define(view=>{
    const pending=new Set();let disposed=false;
    const cancel=()=>{for(const controller of pending)controller.abort();pending.clear();};
    return {
      update(update){if(update.docChanged||update.selectionSet||update.focusChanged&&!view.hasFocus)cancel();},
      destroy(){disposed=true;cancel();},
      async query(kind,position,context) {
        if(disposed||lifetime.aborted||view.composing||!view.hasFocus) return null;
        if(kind==='complete'&&view.state.readOnly) return null;
        const document=view.state.doc,selection=view.state.selection.main.head,source=document.toString();
        const controller=new AbortController(),abort=()=>controller.abort();pending.add(controller);
        lifetime.addEventListener('abort',abort,{once:true});
        context?.addEventListener('abort',abort,{onDocChange:true});
        try {
          const result=await service[kind]({source,position,signal:controller.signal});
          if(!result||controller.signal.aborted||disposed||context?.aborted||!view.hasFocus
            ||view.state.doc!==document||view.state.selection.main.head!==selection)return null;
          if(result.source!==source)throw new Error('The language service returned a different source revision.');
          if(kind==='complete') {
            if(!validSourceRange(source,result.range)||result.range.start>position||result.range.end<position
              ||!Array.isArray(result.items)||result.items.length>10000
              ||result.items.some(item=>typeof item.label!=='string'||!item.label||item.label.length>1024
                ||item.detail!==undefined&&typeof item.detail!=='string'||item.kind!==undefined&&typeof item.kind!=='string'))
              throw new Error('The language service returned invalid completions.');
          }else {
            const target=result.target;
            if(!target||typeof target.source!=='string'||target.source.length>1000000||typeof target.file!=='string'
              ||!target.file||target.file.length>4096||!validSourceRange(target.source,target.range)
              ||typeof target.local!=='boolean'||target.local&&target.source!==source)
              throw new Error('The language service returned an invalid definition.');
          }
          return result;
        }catch(error){
          if(!controller.signal.aborted&&!disposed&&!lifetime.aborted&&view.hasFocus)
            view.dispatch({effects:EditorView.announce.of('Code information is unavailable. Try again.')});
          return null;
        }finally{pending.delete(controller);lifetime.removeEventListener('abort',abort);}
      },
    };
  });
  const extensions=[peek,requests,EditorView.theme({
    '.vui-code-definition':{padding:'12px 16px',maxHeight:'240px',overflow:'auto'},
    '.vui-code-definition > div':{display:'flex',alignItems:'center',justifyContent:'space-between',gap:'12px',flexWrap:'wrap'},
    '.vui-code-definition strong':{overflowWrap:'anywhere'},
    '.vui-code-definition pre':{whiteSpace:'pre-wrap',overflowWrap:'anywhere',font:'inherit',marginBottom:'0'},
    '.vui-code-definition mark':{color:'inherit',backgroundColor:'var(--vui-muted, #eaf0e5)'},
    '.vui-code-definition button':{font:'inherit',color:'inherit',background:'var(--vui-surface, white)',border:'1px solid var(--vui-border, #d9e0d4)',borderRadius:'6px',padding:'4px 8px'},
  })];
  if(service.complete)extensions.push(autocompletion({activateOnTyping:false,override:[async context=>{
    const result=await context.view?.plugin(requests)?.query('complete',context.pos,context);
    return result?{from:result.range.start,to:result.range.end,filter:false,
      options:result.items.map(item=>({label:item.label,type:item.kind,detail:item.detail}))}:null;
  }]}));
  if(service.definition)extensions.push(keymap.of([
    {key:'F12',run:view=>{
      const owner=view.plugin(requests),state=view.state;
      void owner?.query('definition',state.selection.main.head).then(result=>{
        if(!result||view.plugin(requests)!==owner||view.state.doc!==state.doc
          ||view.state.selection.main.head!==state.selection.main.head||!view.hasFocus)return;
        // The service explicitly identifies a same-document destination by source
        // identity. External files remain a read-only definition panel.
        const target=result.target;
        if(target.local) {
          view.dispatch({selection:{anchor:target.range.start,head:target.range.end},scrollIntoView:true,effects:showDefinition.of(null)});
        }else view.dispatch({effects:[showDefinition.of(target),EditorView.announce.of('Definition from '+target.file)]});
      });return true;
    }},
    {key:'Escape',run:view=>{if(!view.state.field(peek))return false;view.dispatch({effects:showDefinition.of(null)});return true;}},
  ]));
  return extensions;
}
