import assert from 'node:assert/strict';
import {readFile,writeFile,mkdir} from 'node:fs/promises';
import {resolve,join} from 'node:path';
import {tmpdir} from 'node:os';
import {spawnContract} from './ci-process.mjs';
import {root} from './repository-paths.mjs';
import {buildDesktopStudio} from './studio-desktop.mjs';
import {desktopArtifact} from './desktop-sdk-manifest.mjs';

const backends=process.argv.slice(2);
if(!backends.length)backends.push('vm','jit','aot');
assert(backends.every(value=>['vm','jit','aot'].includes(value))&&new Set(backends).size===backends.length,'Choose vm, jit and/or aot');
const output=resolve(root,'target/ui-next/studio-desktop');await mkdir(output,{recursive:true});
const report={schema:'volang.studio-desktop-contracts.v1',passed:false,platform:process.platform,arch:process.arch,
  acceptance:'native-application-and-system-webview-dom',physicalInputVerified:false,paintVerified:false,cases:[]};
const save=()=>writeFile(join(output,'report.json'),JSON.stringify(report,null,2)+'\n');
await save();
const lifetime=new AbortController();let active;
for(const signal of ['SIGINT','SIGTERM'])process.once(signal,()=>{lifetime.abort();active?.stop(signal);process.exitCode=130;});
for(const backend of backends) {
  console.log(`Studio desktop: ${backend}`);
  const directory=await buildDesktopStudio({backend,check:true,signal:AbortSignal.any([lifetime.signal,AbortSignal.timeout(600_000)])});
  const receipt=JSON.parse(await readFile(join(directory,'build-report.json'),'utf8'));
  active=spawnContract(join(directory,receipt.executable),['--diagnostics','--exit-on-failure'],
    {cwd:tmpdir(),env:{...process.env,VO_UI_TOOLCHAIN:'missing'},stdio:['ignore','pipe','pipe']});
  const {child}=active;let log='',failure;
  const append=bytes=>{log=(log+bytes).slice(-1024*1024);};child.stdout.on('data',append);child.stderr.on('data',append);
  const timeout=setTimeout(()=>{failure=new Error('Studio desktop exceeded 180 seconds');active.stop('SIGKILL');},180_000);
  const code=await new Promise(resolve=>{child.once('error',error=>{failure=error;});child.once('close',resolve);});
  clearTimeout(timeout);active=undefined;await writeFile(join(output,`${backend}-window.log`),log);
  if(failure)throw failure;
  assert.equal(code,0,log);
  if(backend!=='vm')assert.match(log,/function_entries: [1-9]\d*/);
  if(backend==='aot'){assert.match(log,/aot_continuation_entries: [1-9]\d*/);assert.match(log,/function_compilations: 0/);}
  report.cases.push({backend,profile:receipt.profile,passed:true,artifact:await desktopArtifact(directory,receipt.executable),
    bundleReceipt:await desktopArtifact(directory,'build-report.json'),
    contracts:['gallery-state','client-navigation','offline-documents','optional-editor','saved-draft','compiler-worker','compile-error-recovery','worker-cancellation','ui-preview','preview-interaction','worker-disposal','editor-disposal']});
  await save();
}
report.passed=true;await save();console.log(`Studio desktop: ${report.cases.length} backends passed`);
