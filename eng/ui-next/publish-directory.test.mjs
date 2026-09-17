import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdtemp,mkdir,readFile,readdir,rm,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {publishNewDirectory} from './publish-directory.mjs';

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
