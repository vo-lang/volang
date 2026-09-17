import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {readFile,writeFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {resolve} from 'node:path';
import {root} from './repository-paths.mjs';

const directory=resolve(root,'target/ui-next/desktop');
const preview=resolve(directory,process.platform==='win32'?'preview.exe':'preview');
const report={schema:'volang.ui-next-desktop.v1',passed:false,platform:process.platform,arch:process.arch,
  acceptance:'system-webview-dom-and-native-execution',physicalInputVerified:false,paintVerified:false,cases:[]};
const save=()=>writeFile(resolve(directory,'report.json'),JSON.stringify(report,null,2)+'\n');
await save();
for(const backend of ['vm','jit',...(process.platform==='win32'?[]:['aot'])]) {
  for(const scenario of ['interaction','failure']) {
    console.log(`Desktop adapter: ${backend}/${scenario}`);
    const command=backend==='aot'?resolve(directory,`${scenario}-aot`):preview;
    const args=backend==='aot'?[]:[backend,resolve(directory,`${scenario}.vob`),resolve(directory,'assets')];
    const result=spawnSync(command,args,{cwd:root,env:{...process.env,VOWORK:'off'},encoding:'utf8',timeout:120_000,maxBuffer:4*1024*1024});
    const output=(result.stdout??'')+(result.stderr??'');
    await writeFile(resolve(directory,`${backend}-${scenario}.log`),output);
    const row={backend,scenario,command:[command,...args],sha256:createHash('sha256').update(await readFile(command)).digest('hex'),exit:result.status,passed:false};
    report.cases.push(row);await save();
    assert.ifError(result.error);assert.equal(result.status,scenario==='failure'?1:0,output);
    if(scenario==='failure') {
      assert.match(output,/native UI 中文 failure regression/);assert.match(output,/loc: Some/);
    } else {
      assert.match(output,/desktop completed: Completed/);
      if(backend!=='vm') assert.match(output,/function_entries: [1-9]\d*/);
      if(backend==='aot') {
        assert.match(output,/aot_continuation_entries: [1-9]\d*/);
        assert.match(output,/function_compilations: 0/);assert.match(output,/loop_compilations: 0/);
      }
      row.checks=['initial-effects','continuous-clicks','unicode-immediate-submit','keyed-identity','failure-retry','orderly-close'];
    }
    row.passed=true;await save();
  }
}
report.passed=true;await save();console.log('Desktop adapter: all system WebView contracts passed');
