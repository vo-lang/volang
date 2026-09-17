import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdtemp,mkdir,writeFile,readFile,readdir,realpath,rm,cp} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {loadProject,prepareProjectWeb} from './project-config.mjs';
import {doctorArguments,diagnoseProject} from './project-doctor.mjs';
import {toolchain} from './toolchain.mjs';

async function fixture(action) {
  const directory=await mkdtemp(join(tmpdir(),'ui-project-config-'));
  try {
    await mkdir(join(directory,'web'));
    for(const name of ['vo.mod','vo.lock','vo.work','web/boot.js'])await writeFile(join(directory,name),'');
    await cp(join(toolchain.ui,'next/templates/default/web/index.html'),join(directory,'web/index.html'));
    const schema=JSON.parse(await readFile(join(toolchain.ui,'next/wire.schema.json'),'utf8'));
    const config={format:1,wireVersion:schema.version,document:{title:'A project'}};
    const save=async value=>writeFile(join(directory,'ui-next.json'),JSON.stringify(value));
    await save(config);await action({directory,config,save});
  } finally {await rm(directory,{recursive:true,force:true});}
}
test('project operations keep a validated manifest and document snapshot without output',()=>fixture(async({directory,config,save})=>{
  const before=await readdir(directory,{recursive:true});
  const project=await loadProject(directory);
  await save({...config,features:['missing']});
  await writeFile(join(directory,'web/index.html'),'broken');
  const plan=await prepareProjectWeb(project);
  assert(plan.composeHtml(undefined,project.pages[0]).includes('<title>A project</title>'));
  await assert.rejects(loadProject(directory),/features must/);
  assert.deepEqual(await readdir(directory,{recursive:true}),before);
}));
test('project validation covers declared desktop sources, metadata and authored output collisions',()=>fixture(async({directory,config,save})=>{
  for(const value of [null,[],{...config,document:{title:5}},{...config,desktop:{entry:'missing'}},{...config,serverEntry:'missing'}]) {
    await save(value);await assert.rejects(loadProject(directory));
  }
  await mkdir(join(directory,'native'));
  await save({...config,desktop:{entry:'native',identifier:'dev.example.test'}});
  assert.equal((await loadProject(directory)).desktopEntry,await realpath(join(directory,'native')));
  await mkdir(join(directory,'web/assets'));
  await assert.rejects(prepareProjectWeb(await loadProject(directory)),/reserved/);
  await rm(join(directory,'web/assets'),{recursive:true});
  await mkdir(join(directory,'prerender'));await mkdir(join(directory,'web/notes'));
  await writeFile(join(directory,'web/notes/index.html'),'owned');
  await save({...config,prerenderEntry:'prerender',prerenderPages:[{path:'/'},{path:'/notes'}]});
  await assert.rejects(prepareProjectWeb(await loadProject(directory)),/collides/);
}));
test('doctor accumulates actionable failures without creating or changing project files',()=>fixture(async({directory,save})=>{
  await save(null);
  const before=await readdir(directory,{recursive:true});
  const report=await diagnoseProject(directory,{installation:{kind:'checkout',compiler:process.execPath,
    vm:join(directory,'missing-runtime'),testCLI:join(directory,'missing-tests'),testModule:join(directory,'missing-module')}});
  assert.equal(report.passed,false);
  assert.deepEqual(report.checks.filter(row=>row.status==='failed').map(row=>row.id),['compiler','project','web-runtime','browser-test-runner']);
  assert(report.checks.filter(row=>row.status==='failed').every(row=>row.repair));
  assert.deepEqual(await readdir(directory,{recursive:true}),before);
  assert.equal(await readFile(join(directory,'ui-next.json'),'utf8'),'null');
}));
test('doctor preserves explicit paths and rejects ambiguous options',()=>{
  assert.deepEqual(doctorArguments(['--project','A project 中文','--json','--target','desktop']),{directory:'A project 中文',target:'desktop',json:true});
  for(const args of [[],['--project'],['--project','a','--target'],['--project','a','--target','vm'],['--project','a','--json','--json']])assert.throws(()=>doctorArguments(args));
});
