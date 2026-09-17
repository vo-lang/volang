import assert from 'node:assert/strict';
import {open,mkdir,readFile,rm,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {root} from './server.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {unitContracts} from './core-contracts.mjs';
import {collectCoreEvidence} from './ci-report.mjs';
import {spawnContract} from './ci-process.mjs';

// Prerequisite builds are declared in eng/ci.toml so the normal CI execution
// receipt captures their commands, failures and deadlines individually.
const directory=join(root,'target/ui-next/ci');
const result=join(root,'target/ci/results/ui-web-rewrite.json');
await mkdir(directory,{recursive:true});await mkdir(join(root,'target/ci/results'),{recursive:true});
await rm(result,{force:true});
const commands={passed:false,nativeJit:false,unitTests:0,unitPassed:0,steps:[]};
let active, interrupted;
const handlers=new Map(['SIGINT','SIGTERM'].map(signal => [signal,() => {
  interrupted=signal;
  active?.stop(signal);
}]));
for (const [signal,handler] of handlers) process.on(signal,handler);
async function run(name,executable,args,environment={}) {
  if (interrupted) throw new Error('UI CI stopped by ' + interrupted);
  const started=performance.now(),path=join(directory,name + '.log');
  console.log('UI core: ' + name);
  const log=await open(path,'w');
  try {
    await new Promise((resolve,reject) => {
      active=spawnContract(executable,args,{cwd:root,env:{...process.env,VOWORK:'off',...environment},stdio:['ignore',log.fd,log.fd]});
      active.child.once('error',reject);
      active.child.once('close',(code,signal) => code === 0 ? resolve() : reject(new Error(`${name} failed (${signal ?? code}); see ${path}`)));
    });
    commands.steps.push({name,passed:true,milliseconds:Math.round(performance.now()-started),log:'target/ui-next/ci/' + name + '.log'});
    console.log(`UI core: ${name} passed in ${Math.round((performance.now()-started)/1000)} s`);
  } finally {active=undefined;await log.close();}
  return path;
}
const node=(name,...args) => run(name,process.execPath,args);
try {
  await node('build','eng/ui-next/cli.mjs','build');
  const units=await node('unit-contracts','--test','--test-reporter=tap',...await unitContracts());
  const output=await readFile(units,'utf8');
  commands.unitTests=Number(output.match(/^# tests (\d+)$/m)?.[1]);
  commands.unitPassed=Number(output.match(/^# pass (\d+)$/m)?.[1]);
  assert(commands.unitTests > 0 && commands.unitTests === commands.unitPassed,'all core unit tests must execute');
  const jit=await run('native-jit',compilerPath(),['run','ui/next/tests/runtime','--mode=jit']);
  assert.equal(await readFile(jit,'utf8'),'ui-next runtime contracts: ok\n');commands.nativeJit=true;
  for (const engine of ['chromium','firefox','webkit']) await run(engine,process.execPath,['eng/ui-next/check.mjs'],{UI_NEXT_BROWSER:engine});
  await node('pointer-development','eng/ui-next/pointer-development-contracts.mjs');
  await node('public-host','eng/ui-next/public-host-contracts.mjs');
  await node('desktop-host','eng/ui-next/desktop-browser-contracts.mjs');
  await node('portable-toolchain','eng/ui-next/toolchain-contracts.mjs');
  await node('studio-build','eng/ui-next/cli.mjs','build','--studio');
  await node('studio-delivery','eng/ui-next/asset-delivery-contracts.mjs');
  await node('studio-static-build','eng/ui-next/studio-static-build-contracts.mjs');
  await node('studio-static','eng/ui-next/studio-static-contracts.mjs');
  await node('studio-upgrade','eng/ui-next/studio-upgrade-contracts.mjs');
  await rm(join(directory,'toolchain'),{recursive:true,force:true});
  await node('package','eng/ui-next/cli.mjs','package',join(directory,'toolchain'));
  commands.passed=true;
  await writeFile(join(directory,'commands.json'),JSON.stringify(commands,null,2)+'\n');
  const evidence=await collectCoreEvidence(root);
  // Tar retains the native compiler's executable mode when CI transports it.
  await run('archive','tar',['-czf',join(root,'target/ci/artifacts/ui-web-rewrite-toolchain.tar.gz'),'-C',directory,'toolchain']);
  await run('archive-studio','tar',['-czf',join(root,'target/ci/artifacts/ui-web-rewrite-studio-static.tar.gz'),'-C',join(root,'target/ui-next/studio-static'),'.']);
  await writeFile(result,JSON.stringify(evidence,null,2)+'\n');
  console.log('UI core passed; evidence: ' + result);
} catch (error) {
  commands.passed=false;commands.error=String(error.stack ?? error);
  await writeFile(join(directory,'commands.json'),JSON.stringify(commands,null,2)+'\n');
  await writeFile(result,JSON.stringify({schema:'volang.browser-result.v1',passed:false,error:commands.error},null,2)+'\n');
  throw error;
} finally {
  for (const [signal,handler] of handlers) process.off(signal,handler);
}
