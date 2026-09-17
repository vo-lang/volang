import assert from 'node:assert/strict';
import {spawnContract} from './ci-process.mjs';
import {cp,mkdir,readFile,readdir,rename,writeFile} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {tmpdir} from 'node:os';
import {toolchain} from './toolchain.mjs';
import {buildToolchain} from './toolchain-build.mjs';
import {desktopArtifact,readDesktopSdk} from './desktop-sdk-manifest.mjs';
import {checkProjectDiagnosis} from './project-diagnosis-contracts.mjs';
import {checkDesktopShell} from './desktop-shell-contracts.mjs';
import {checkDesktopStorage} from './desktop-storage-contracts.mjs';
import {compilerPath} from './toolchain.mjs';

const sdk=await readDesktopSdk(toolchain.desktop);
const root=toolchain.root,base=resolve(root,'target/ui-next/desktop-delivery');
await mkdir(base,{recursive:true});
const stamp=Date.now().toString(36),work=join(base,`run-${stamp}`);
await mkdir(work);
const report={schema:'volang.ui-desktop-delivery.v1',passed:false,platform:process.platform,arch:process.arch,
  acceptance:'packaged-native-execution-and-system-webview-dom',physicalInputVerified:false,paintVerified:false,cases:[]};
const save=()=>writeFile(join(base,'report.json'),JSON.stringify(report,null,2)+'\n');
await save();
let active;
const lifetime=new AbortController();
for(const signal of ['SIGINT','SIGTERM']) process.once(signal,()=>{lifetime.abort(new Error(`Desktop delivery stopped by ${signal}`));active?.stop(signal);process.exitCode=130;});
async function run(name,command,args,{cwd=work,success=true,env={}}={}) {
  lifetime.signal.throwIfAborted();
  console.log(`Desktop delivery: ${name}`);
  active=spawnContract(command,args,{cwd,env:{...process.env,VOWORK:'off',...env},stdio:['ignore','pipe','pipe']});
  const {child}=active;
  let output='',error;
  const append=bytes=>{output=(output+bytes).slice(-1024*1024);};child.stdout.on('data',append);child.stderr.on('data',append);
  const deadline=name.startsWith('window-') ? 90_000 : 600_000;
  const timeout=setTimeout(()=>{error=new Error(`${name} exceeded ${deadline} ms`);active.stop('SIGKILL');},deadline);
  const code=await new Promise(resolve=>{child.once('error',cause=>{error=cause;});child.once('close',resolve);});clearTimeout(timeout);active=undefined;
  await writeFile(join(work,`${name}.log`),output);
  lifetime.signal.throwIfAborted();
  if(error)throw error;
  if(success)assert.equal(code,0,output);else assert.notEqual(code,0,output);
  report.cases.push({name,passed:true,exit:code});await save();return output;
}
const installed=join(work,'tools');
await checkDesktopShell({directory:work,compiler:compilerPath(),profile:sdk.profile,run});
await buildToolchain(installed,{desktop:toolchain.desktop,signal:lifetime.signal});
const movedTools=join(work,'Moved tools 中文');await rename(installed,movedTools);
const compiler=join(movedTools,'bin',process.platform==='win32'?'vo.exe':'vo');
await run('verify-moved-toolchain',compiler,['ui','verify']);
const project=join(work,'A desktop 中文');
await run('create',compiler,['ui','create',project]);
report.projectDiagnosis=await checkProjectDiagnosis(compiler,project,{env:process.env,desktop:true});await save();
for(const file of await readdir(join(root,'ui/next/examples/interaction'))) if(file.endsWith('.vo')) {
  await cp(join(root,'ui/next/examples/interaction',file),join(project,file));
}
await cp(join(root,'eng/ui-next/desktop-check.js'),join(project,'web/check.js'));
const htmlPath=join(project,'web/index.html');
await writeFile(htmlPath,(await readFile(htmlPath,'utf8')).replace('</body>','<script src="/check.js" defer></script></body>'));
for(const backend of ['vm','jit',...(sdk.runtime?['aot']:[])]) {
  await run(`package-${backend}`,compiler,['ui','package','--project',project,'--backend',backend]);
  const built=join(project,`target/ui-desktop/dist-${backend}`);
  const receipt=JSON.parse(await readFile(join(built,'build-report.json'),'utf8'));
  assert.equal(receipt.backend,backend);assert.equal(receipt.profile,sdk.profile);
  assert(!receipt.artifacts.some(file=>/\.wasm$|node_modules|vo-ui-desktop-sdk/.test(file.path)));
  const relocated=join(work,`Moved application ${backend} 中文`);await cp(built,relocated,{recursive:true});
  const executable=join(relocated,receipt.executable);
  await run(`verify-${backend}`,executable,['--check'],{cwd:tmpdir(),env:{VO_UI_TOOLCHAIN:'missing',PATH:process.platform==='win32'?process.env.PATH:'/usr/bin:/bin'}});
  const log=await run(`window-${backend}`,executable,['--diagnostics','--exit-on-failure'],{cwd:tmpdir(),env:{VO_UI_TOOLCHAIN:'missing'}});
  if(backend!=='vm')assert.match(log,/function_entries: [1-9]\d*/);
  if(backend==='aot'){assert.match(log,/aot_continuation_entries: [1-9]\d*/);assert.match(log,/function_compilations: 0/);}
  if(process.platform==='darwin')await run(`plist-${backend}`,'/usr/bin/plutil',['-lint',join(relocated,'Application.app/Contents/Info.plist')]);
  const resources=join(relocated,process.platform==='darwin'?'Application.app/Contents/Resources':'resources');
  await writeFile(join(resources,'theme.css'),'corrupted');
  assert.match(await run(`corrupt-${backend}`,executable,['--check'],{success:false}),/integrity mismatch/);
  report.cases.find(row=>row.name===`window-${backend}`).artifact=await desktopArtifact(relocated,receipt.executable);await save();
}
await run('public-run-default-jit',compiler,['ui','run',project]);
await run('public-run-current-directory-vm',compiler,['ui','run','--backend','vm'],{cwd:project});
const previous=await readFile(join(project,'target/ui-desktop/dist-vm/build-report.json'),'utf8');
const source=await readFile(join(project,'main.vo'),'utf8');
await writeFile(join(project,'main.vo'),'package main\nfunc broken(\n');
await run('failed-build-preserves-previous',compiler,['ui','package',project,'--backend','vm'],{success:false});
assert.equal(await readFile(join(project,'target/ui-desktop/dist-vm/build-report.json'),'utf8'),previous);
await writeFile(join(project,'main.vo'),source);
await run('reject-unknown-backend',compiler,['ui','package',project,'--backend','unknown'],{success:false});
await checkDesktopStorage({project,work,stamp,compiler,run});
// A full document load must release its one native session instead of leaving
// an executor waiting forever on the unloaded transport.
await writeFile(join(project,'web/check.js'),`(async()=>{while(!window.__volangDesktop)await new Promise(r=>setTimeout(r,10));await window.__volangDesktop.ready;location.reload();})();\n`);
await run('package-document-unload',compiler,['ui','package',project,'--backend','jit']);
const unloadOutput=join(project,'target/ui-desktop/dist-jit'),unloadReceipt=JSON.parse(await readFile(join(unloadOutput,'build-report.json'),'utf8'));
assert.match(await run('window-document-unload',join(unloadOutput,unloadReceipt.executable),['--exit-on-failure'],{success:false}),/application document closed/);
// Optional packs use the same authored boot and their real native guest image.
for(const template of ['canvas','plot','listening','variable-list']) {
  const project=join(work,template);
  await run(`create-${template}`,compiler,['ui','create',project,'--template',template]);
  if(template==='plot') {
    // Keep local widget failures in the retained process log as well as the
    // application's fallback UI, so a platform import failure is diagnosable.
    const sourcePath=join(project,'app/app.vo'),source=await readFile(sourcePath,'utf8');
    assert(source.includes('failure.Set(message)'));
    await writeFile(sourcePath,source.replace('failure.Set(message)','println("Desktop plot widget: " + message)\n\t\t\t\tfailure.Set(message)'));
  }
  const path=join(project,'web/index.html');
  await writeFile(path,(await readFile(path,'utf8')).replace('</body>',`<script src="/check.js" data-template="${template}" defer></script></body>`));
  await cp(join(root,'eng/ui-next/desktop-template-check.js'),join(project,'web/check.js'));
  await run(`package-${template}`,compiler,['ui','package',project,'--backend','jit']);
  const output=join(project,'target/ui-desktop/dist-jit'),receipt=JSON.parse(await readFile(join(output,'build-report.json'),'utf8'));
  await run(`window-${template}`,join(output,receipt.executable),['--exit-on-failure']);
}
const failure=join(work,'Failure application');
await run('create-failure',compiler,['ui','create',failure]);
await cp(join(root,'ui/next/tests/native_failure/main.vo'),join(failure,'main.vo'));
for(const backend of ['vm','jit',...(sdk.runtime ? ['aot'] : [])]) {
  await run(`package-failure-${backend}`,compiler,['ui','package',failure,'--backend',backend]);
  const output=join(failure,`target/ui-desktop/dist-${backend}`),receipt=JSON.parse(await readFile(join(output,'build-report.json'),'utf8'));
  const log=await run(`window-failure-${backend}`,join(output,receipt.executable),['--exit-on-failure'],{success:false});
  assert.match(log,/native UI 中文 failure regression/);
}
report.profile=sdk.profile;report.passed=true;report.directory=work;await save();console.log(`Desktop delivery: ${report.cases.length} contracts passed. ${work}`);
