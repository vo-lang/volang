import assert from 'node:assert/strict';

// Serialized into a fresh worker; all source and module state stays local to it.
async function editorChecks(origin) {
  const runtime=await import(origin+'/compiler/vo_web.js');
  const {vfs,registerVFSBindings}=await import(origin+'/host/vfs.js');
  await runtime.default();await vfs.init();registerVFSBindings();
  const write=(path,source)=>{
    const error=vfs.mkdirAll(path.slice(0,path.lastIndexOf('/')),0o755)??vfs.writeFile(path,new TextEncoder().encode(source),0o644);
    if(error)throw new Error(error);
  };
  const main='package main\nimport "local/editor/app"\nfunc main() { println(app.Value) }\n';
  const dependency='package app\nconst Value = 7\ntype Thing struct { Name string }\n';
  write('/editor/vo.mod','format = 1\nmodule = "local/editor"\nversion = "0.1.0"\nvo = "0.1.4"\n');
  write('/editor/vo.work','format = 1\nmembers = ["."]\n');
  write('/editor/main.vo',main);write('/editor/app/main.vo',dependency);
  const position=(source,offset)=>{
    const lines=source.slice(0,offset).split('\n');return [lines.length-1,lines.at(-1).length];
  };
  const snapshots=[];const progress=[];
  try {
    for(const [index,suffix] of ['app.','app.Va','println(app.Value)'].entries()) {
      const source='package main\nimport "local/editor/app"\nfunc main() { _ = "中文🙂"; '+suffix;
      const snapshot=runtime.createEditorProject('main.vo','/editor',index+1,'','main.vo',source);
      snapshots.push(snapshot);
      const completion=JSON.parse(snapshot.completionsJson(index+1,'main.vo',...position(source,source.length-(index===2?1:0))));
      progress.push({revision:snapshot.revision,complete:snapshot.complete,source:snapshot.sourceText(index+1,'main.vo'),completion});
    }
    const oldSource=snapshots[0].sourceText(1,'main.vo');
    if(oldSource!==progress[0].source)throw new Error('A later snapshot replaced an earlier source.');
    if(snapshots[1].completionsJson(1,'main.vo',2,1)!==undefined)throw new Error('A stale revision returned completions.');
    const saved=runtime.createEditorProject('main.vo','/editor',8,'');
    try {
      const offset=main.indexOf('Value');
      const definition=JSON.parse(saved.definitionJson(8,'main.vo',...position(main,offset)));
      const dependencySource=saved.sourceText(8,definition.location.file);
      if(dependencySource!==dependency)throw new Error('Definition source identity changed.');
      const byteSource=new TextEncoder().encode(dependencySource);
      if(new TextDecoder().decode(byteSource.slice(definition.location.startByte,definition.location.endByte))!=='Value')throw new Error('Wrong source definition range.');
      const qualifier=JSON.parse(saved.definitionJson(8,'main.vo',...position(main,main.indexOf('app.Value'))));
      if(qualifier.location.file!=='main.vo')throw new Error('Package qualifier did not resolve to its import.');
      const importSource=new TextDecoder().decode(new TextEncoder().encode(main).slice(qualifier.location.startByte,qualifier.location.endByte));
      if(!importSource.includes('"local/editor/app"'))throw new Error('Package qualifier has the wrong declaration range.');
      progress.push({saved:saved.complete,definition,source:saved.sourceText(8,'main.vo')});
    }finally{saved.free();}
    write('/editor/app/main.vo','package app\nvar Value Missing\n');
    const failed=runtime.compileProject('main.vo','/editor','');
    try {
      if(failed.success)throw new Error('An invalid dependency compiled.');
      progress.push({dependencyDiagnostics:JSON.parse(failed.diagnosticsJson)});
    }finally{failed.free();}
    const single=runtime.createEditorSource('package main\nimport "fmt"\nfunc main() { fmt.','main.vo',9);
    try {progress.push({stdlib:JSON.parse(single.completionsJson(9,'main.vo',2,18))});}
    finally{single.free();}
    const warning=runtime.createEditorSource('package main\nfunc main() { unused := 1 }','main.vo',10);
    try {progress.push({complete:warning.complete,warnings:JSON.parse(warning.diagnosticsJson)});}
    finally{warning.free();}
    return progress;
  }finally{for(const snapshot of snapshots)snapshot.free();}
}

export async function checkEditorCompiler(page,url) {
  await page.route('**/__editor-compiler',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Editor semantics</title>'}));
  await page.goto(url+'/__editor-compiler');
  const result=await page.evaluate(async source=>{
    const script=URL.createObjectURL(new Blob([`self.onmessage=async event=>{try{self.postMessage({result:await (${source})(event.data)});}catch(error){self.postMessage({error:String(error?.stack??error)});}}`],{type:'text/javascript'}));
    const worker=new Worker(script,{type:'module'});let timeout;
    try {
      return await new Promise((resolve,reject)=>{
        timeout=setTimeout(()=>reject(new Error('Editor compiler query exceeded 30 seconds.')),30000);
        worker.onmessage=event=>resolve(event.data);
        worker.onerror=event=>{event.preventDefault();reject(new Error(event.message));};
        worker.postMessage(location.origin);
      });
    }finally{clearTimeout(timeout);worker.terminate();URL.revokeObjectURL(script);}
  },editorChecks.toString());
  assert.equal(result.error,undefined);
  const [empty,prefix,call,saved,failed,single,warning]=result.result;
  for(const [index,step] of [empty,prefix,call].entries()) {
    assert.equal(step.revision,index+1);assert.equal(step.complete,false);
    assert.equal(step.completion.revision,index+1);assert.equal(step.completion.positionEncoding,'utf-16');
    assert(step.completion.items.some(item=>item.label==='Value'));
  }
  assert.equal(empty.completion.replace.startByte,empty.completion.replace.endByte);
  assert.deepEqual(prefix.completion.items.map(item=>item.label),['Value']);
  assert.equal(saved.saved,true);assert(saved.source.endsWith('println(app.Value) }\n'));
  assert.equal(saved.definition.location.file,'app/main.vo');
  assert(failed.dependencyDiagnostics.items.filter(item=>item.severity==='error').every(item=>item.location.file==='app/main.vo'));
  assert(single.stdlib.items.some(item=>item.label==='Println'));
  assert(warning.complete&&warning.warnings.items.some(item=>item.severity==='warning'));
  return {passed:true,contracts:['incomplete-source-completion','compiler-owned-member-visibility','cross-file-definition',
    'utf16-editor-positions','editor-revision-isolation','same-basename-dependency-diagnostic','stdlib-completion','successful-analysis-warnings']};
}
