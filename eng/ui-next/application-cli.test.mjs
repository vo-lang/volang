import assert from 'node:assert/strict';
import test from 'node:test';
import {execFile} from 'node:child_process';
import {mkdtemp,readdir,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {fileURLToPath} from 'node:url';
import {promisify} from 'node:util';

const execute=promisify(execFile),entry=fileURLToPath(new URL('./application-cli.mjs',import.meta.url));

test('application subcommand help and malformed creation never write a project', async () => {
  const cwd=await mkdtemp(join(tmpdir(),'ui-command-help-'));
  try {
    const env={...process.env,VO_TEST_COMPILER:join(cwd,'missing-compiler')};
    for (const command of ['create','check','build','dev','preview','test','verify','browsers','doctor']) {
      for (const help of ['--help','-h']) {
        const result=await execute(process.execPath,[entry,command,help],{cwd,env});
        assert.match(result.stdout,/Usage: ui create/);assert.equal(result.stderr,'');
      }
    }
    for (const args of [['create','--unknown'],['create',''],['create','app','--template'],['create','app','--template','missing']]) {
      await assert.rejects(execute(process.execPath,[entry,...args],{cwd,env}));
    }
    assert.deepEqual(await readdir(cwd),[]);
  } finally {await rm(cwd,{recursive:true,force:true});}
});
