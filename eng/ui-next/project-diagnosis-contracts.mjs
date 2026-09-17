import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {mkdir,readFile,readdir,rm,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {execute} from './execute.mjs';

async function snapshot(directory) {
  const result=[];
  async function visit(path) {
    for(const entry of await readdir(join(directory,path),{withFileTypes:true})) {
      const name=path?`${path}/${entry.name}`:entry.name;
      if(entry.isDirectory()) {result.push([name,'directory']);await visit(name);}
      else result.push([name,createHash('sha256').update(await readFile(join(directory,name))).digest('hex')]);
    }
  }
  await visit('');return result.sort(([a],[b])=>a.localeCompare(b));
}

// Called by both relocated Web-toolchain and native-desktop delivery gates.
// A prerender program that panics proves check never executes guest code.
export async function checkProjectDiagnosis(compiler,directory,{env,desktop=false}={}) {
  const invoke=(...args)=>execute(compiler,['ui',...args],{cwd:directory,env});
  const configPath=join(directory,'ui-next.json'),configBytes=await readFile(configPath),config=JSON.parse(configBytes);
  const prerender=join(directory,config.prerenderEntry,'main.vo'),original=await readFile(prerender);
  const boot=join(directory,'web/boot.js'),bootBytes=await readFile(boot);
  const native=join(directory,'diagnosis-native');
  try {
    await writeFile(prerender,'package main\n\nfunc main() {\n\tpanic("UI check must not run prerender")\n}\n');
    const before=await snapshot(directory);
    assert.match(await invoke('check'),/Checked \d+ source entries/);
    assert.deepEqual(await snapshot(directory),before,'check changed source or created build outputs');
    const web=JSON.parse(await invoke('doctor','--json'));
    assert.equal(web.schema,'volang.ui-project-diagnosis.v1');assert.equal(web.passed,true);
    assert(web.checks.every(check=>check.status==='passed'));
    assert.deepEqual(await snapshot(directory),before,'doctor modified the project');
    if(desktop)assert.equal(JSON.parse(await invoke('doctor','--json','--target','desktop')).passed,true);
    await writeFile(configPath,'null');
    await assert.rejects(invoke('doctor','--json'),error=>{
      const report=JSON.parse(error.message);
      assert.equal(report.passed,false);assert.equal(report.checks.find(check=>check.id==='project').status,'failed');
      assert.equal(report.checks.find(check=>check.id==='compiler').status,'passed');return true;
    });
    await writeFile(configPath,configBytes);
    await writeFile(boot,'import "./missing-host-module.js";\n');
    await assert.rejects(invoke('check'),/missing-host-module/);
    await writeFile(boot,bootBytes);
    await mkdir(native);
    await writeFile(join(native,'main.vo'),'package main\n\nfunc main() {\n\tvar count int = "invalid desktop type"\n\tprintln(count)\n}\n');
    await writeFile(configPath,JSON.stringify({...config,desktop:{...config.desktop,entry:'diagnosis-native'}}));
    await assert.rejects(invoke('check'),/cannot use|mismatch|assign|type/i);
    await rm(native,{recursive:true});await writeFile(configPath,configBytes);
    assert.deepEqual(await snapshot(directory),before,'failed checks changed source or build outputs');
    return {passed:true,readOnly:true,noGuestExecution:true,hostImports:true,desktopEntry:true,diagnosis:true,failureDiagnostics:true,desktop};
  } finally {
    await writeFile(prerender,original);await writeFile(boot,bootBytes);await writeFile(configPath,configBytes);
    await rm(native,{recursive:true,force:true});
  }
}
