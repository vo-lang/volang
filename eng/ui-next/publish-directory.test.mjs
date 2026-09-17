import assert from 'node:assert/strict';
import test from 'node:test';
import fs from 'node:fs/promises';
import {mkdtemp,mkdir,readFile,readdir,rm,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {publishNewDirectory,renameDirectory} from './publish-directory.mjs';

function platform(t,value) {
  const descriptor=Object.getOwnPropertyDescriptor(process,'platform');
  Object.defineProperty(process,'platform',{...descriptor,value});
  t.after(()=>Object.defineProperty(process,'platform',descriptor));
}

test('Windows directory publication recovers from transient sharing locks',async t=>{
  platform(t,'win32');
  const root=await mkdtemp(join(tmpdir(),'vo-publish-sharing-'));
  t.after(()=>rm(root,{recursive:true,force:true}));
  const stage=join(root,'stage'),destination=join(root,'Application 中文'),rename=fs.rename;
  await mkdir(stage);await writeFile(join(stage,'asset'),'complete');
  const failures=['EPERM','EACCES','EBUSY'];
  const attempt=t.mock.method(fs,'rename',async(...args)=>{
    const code=failures.shift();
    if(code)throw Object.assign(new Error('sharing violation'),{code});
    return rename(...args);
  });
  await renameDirectory(stage,destination);
  assert.equal(attempt.mock.callCount(),4);
  assert.equal(await readFile(join(destination,'asset'),'utf8'),'complete');
});

test('persistent Windows publication failure is bounded and preserves the filesystem error',async t=>{
  platform(t,'win32');
  const failure=Object.assign(new Error('access denied'),{code:'EPERM',path:'stage',dest:'destination'});
  const attempt=t.mock.method(fs,'rename',async()=>{throw failure;});
  await assert.rejects(renameDirectory('stage','destination'),error=>error===failure);
  assert.equal(attempt.mock.callCount(),9);
});

test('directory publication does not retry conflicting destinations or non-Windows failures',async t=>{
  platform(t,'win32');
  let failure;
  const attempt=t.mock.method(fs,'rename',async()=>{throw failure;});
  for(const code of ['EEXIST','ENOTEMPTY','ENOENT','EXDEV']) {
    failure=Object.assign(new Error(code),{code});
    await assert.rejects(renameDirectory('stage','destination'),error=>error===failure);
  }
  Object.defineProperty(process,'platform',{value:'linux'});
  failure=Object.assign(new Error('access denied'),{code:'EPERM'});
  await assert.rejects(renameDirectory('stage','destination'),error=>error===failure);
  assert.equal(attempt.mock.callCount(),5);
});

test('cancelling a sharing-lock wait preserves the cancellation reason and stops publication',async t=>{
  platform(t,'win32');
  const lifetime=new AbortController(),reason=new Error('build replaced');
  const attempt=t.mock.method(fs,'rename',async()=>{
    setImmediate(()=>lifetime.abort(reason));
    throw Object.assign(new Error('sharing violation'),{code:'EBUSY'});
  });
  await assert.rejects(renameDirectory('stage','destination',{signal:lifetime.signal}),error=>error===reason);
  await assert.rejects(renameDirectory('stage','destination',{signal:lifetime.signal}),error=>error===reason);
  assert.equal(attempt.mock.callCount(),1);
});

test('directory publication preserves existing destinations and complete winners',async()=>{
  const root=await mkdtemp(join(tmpdir(),'vo-publish-'));
  try {
    const stage=join(root,'stage'),destination=join(root,'Application 中文');
    await mkdir(stage);await writeFile(join(stage,'asset'),'complete');
    await mkdir(destination);
    await assert.rejects(publishNewDirectory(stage,destination));
    assert.deepEqual(await readdir(destination),[]);
    await rm(destination,{recursive:true});await writeFile(destination,'owned');
    await assert.rejects(publishNewDirectory(stage,destination));
    assert.equal(await readFile(destination,'utf8'),'owned');
    await rm(destination);
    await publishNewDirectory(stage,destination);
    assert.equal(await readFile(join(destination,'asset'),'utf8'),'complete');
    await mkdir(stage);await writeFile(join(stage,'asset'),'replacement');
    await assert.rejects(publishNewDirectory(stage,destination));
    assert.equal(await readFile(join(destination,'asset'),'utf8'),'complete');
    assert.equal(await readFile(join(stage,'asset'),'utf8'),'replacement');
  } finally {await rm(root,{recursive:true,force:true});}
});

test('concurrent directory publishers retain one complete winner',async()=>{
  const root=await mkdtemp(join(tmpdir(),'vo-publish-race-'));
  try {
    const stages=['first','second'].map(name=>join(root,name)),destination=join(root,'destination');
    for(const [i,stage] of stages.entries()) {
      await mkdir(stage);await writeFile(join(stage,'asset'),String(i));
    }
    const results=await Promise.allSettled(stages.map(stage=>publishNewDirectory(stage,destination)));
    assert.equal(results.filter(result=>result.status==='fulfilled').length,1);
    const winner=results.findIndex(result=>result.status==='fulfilled'),loser=1-winner;
    assert.equal(await readFile(join(destination,'asset'),'utf8'),String(winner));
    assert.equal(await readFile(join(stages[loser],'asset'),'utf8'),String(loser));
  } finally {await rm(root,{recursive:true,force:true});}
});
