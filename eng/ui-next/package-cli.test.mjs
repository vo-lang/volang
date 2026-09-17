import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {mkdtemp,readFile,readdir,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import test from 'node:test';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {root} from './server.mjs';

test('package help and invalid options write nothing; an explicit compiler overrides the checkout default',async () => {
  const directory=await mkdtemp(join(tmpdir(),'volang-ui-package-'));
  function run(args) {
    const result=spawnSync(process.execPath,[join(root,'eng/ui-next/cli.mjs'),'package',...args],{
      cwd:directory,env:{...process.env,VO_TEST_COMPILER:join(directory,'missing-compiler')},
      encoding:'utf8',timeout:120000,maxBuffer:1024*1024,
    });
    assert.ifError(result.error);return result;
  }
  try {
    const help=run(['--help']);assert.equal(help.status,0,help.stderr);assert.match(help.stdout,/--compiler/);
    for(const args of [[],['--unknown'],['output','--unknown','value'],['output','--compiler'],
      ['output','--compiler',join(directory,'missing')],['output','--compiler',directory]]) {
      const result=run(args);assert.notEqual(result.status,0,result.stdout);
    }
    assert.deepEqual(await readdir(directory),[],'help or invalid input created a package');
    const compiler=compilerPath(),output=join(directory,'Selected compiler 工具包');
    const result=run([output,'--compiler',compiler]);assert.equal(result.status,0,result.stderr);
    const manifest=JSON.parse(await readFile(join(output,'tools/toolchain.json')));
    const selected=manifest.artifacts.find(item=>item.path===manifest.paths.compiler);
    const bytes=await readFile(compiler);
    assert.equal(selected.bytes,bytes.length);assert.equal(selected.sha256,createHash('sha256').update(bytes).digest('hex'));
  } finally {await rm(directory,{recursive:true,force:true});}
});
