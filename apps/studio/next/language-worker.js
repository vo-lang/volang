import {prepareUiCompiler,writeCompilerFile} from './compiler-workspace.js';

let environment,analysis,analyzedSource,analyzedUi,revision=0;
async function initialize(ui) {
  const [runtime,filesystem]=await Promise.all([import('/compiler/vo_web.js'),import('/host/vfs.js')]);
  await runtime.default();await filesystem.vfs.init();filesystem.registerVFSBindings();
  if(ui) {
    await prepareUiCompiler(runtime,filesystem,'/editor');
    // The public overlay API replaces an existing captured file. This worker
    // owns a fresh memory project; each query supplies its unsaved main source.
    writeCompilerFile(filesystem,'/editor/main.vo','package main\n');
  }
  return runtime;
}
function offset(source,position) {
  let start=0;
  for(let line=0;line<position.line;line++) {
    const end=source.indexOf('\n',start);if(end<0)throw new Error('Unknown definition line.');start=end+1;
  }
  return start+position.character;
}
function range(source,location) {return {start:offset(source,location.start),end:offset(source,location.end)};}
async function query({kind,source,position,ui}) {
  if(!['complete','definition'].includes(kind)||typeof source!=='string'||source.length>100000
    ||!Number.isSafeInteger(position)||position<0||position>source.length||typeof ui!=='boolean')throw new Error('Invalid code query.');
  environment??=initialize(ui);
  const runtime=await environment;
  if(analysis&&analyzedUi!==ui)throw new Error('An editor changed its workspace identity.');
  if(!analysis||analyzedSource!==source) {
    analysis?.free();analysis=undefined;
    if(revision===0xffffffff)throw new Error('Please reopen this editor.');
    revision++;
    analysis=ui?runtime.createEditorProject('main.vo','/editor',revision,'','main.vo',source):runtime.createEditorSource(source,'main.vo',revision);
    analyzedSource=source;analyzedUi=ui;
  }
  const lines=source.slice(0,position).split('\n'),line=lines.length-1,character=lines.at(-1).length;
  const json=kind==='complete'?analysis.completionsJson(revision,'main.vo',line,character):analysis.definitionJson(revision,'main.vo',line,character);
  if(!json)return null;
  const result=JSON.parse(json);
  if(result.version!==1||result.revision!==revision||result.positionEncoding!=='utf-16')throw new Error('Unknown code query response.');
  if(kind==='complete') {
    if(result.replace.file!=='main.vo')throw new Error('Completion belongs to a different document.');
    return {source,range:range(source,result.replace),items:result.items.map(({label,kind,detail})=>({label,kind,detail}))};
  }
  const target=result.location,targetSource=analysis.sourceText(revision,target.file);
  if(targetSource===undefined)return null;
  return {source,target:{file:target.file,source:targetSource,range:range(targetSource,target),local:target.file==='main.vo'&&targetSource===source}};
}
let queue=Promise.resolve();
self.onmessage=event=>{
  const request=event.data;
  queue=queue.then(async()=>{
    try{self.postMessage({id:request.id,result:await query(request)});}
    catch(error){self.postMessage({id:request.id,error:String(error?.message??error)});}
  });
};
