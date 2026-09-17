import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdir,mkdtemp,rm,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {serverEntries,readServerEntries} from './server-entries.mjs';

test('server image manifest has one bounded, canonical declared entry set',async()=>{
  assert.deepEqual(serverEntries(),['default']);
  assert.deepEqual(serverEntries(['notes','default','about']),['default','about','notes']);
  for(const entries of [null,[],['notes'],['default','default'],['default','../notes'],['default','con'],['default',...Array.from({length:33},(_,i)=>`page-${i}`)]]) {
    assert.throws(()=>serverEntries(entries),/Server entries/);
  }
  const directory=await mkdtemp(join(tmpdir(),'ui-server-entries-'));
  try {
    await mkdir(join(directory,'server'));
    await assert.rejects(readServerEntries(directory),/missing its page entry manifest/);
    const manifest=join(directory,'server/entries.json');
    for(const value of [{version:2,entries:['default']},{version:1},{version:1,entries:['default'],unexpected:true}]) {
      await writeFile(manifest,JSON.stringify(value));await assert.rejects(readServerEntries(directory),/Invalid server entry manifest/);
    }
    await writeFile(manifest,' '.repeat(8193));await assert.rejects(readServerEntries(directory),/8 KiB/);
    await writeFile(manifest,JSON.stringify({version:1,entries:['default','notes']}));
    assert.deepEqual(await readServerEntries(directory),['default','notes']);
  }finally{await rm(directory,{recursive:true,force:true});}
});
