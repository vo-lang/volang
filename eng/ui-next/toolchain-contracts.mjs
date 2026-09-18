import {deliveredExampleFiles} from './project-templates.mjs';
import {checkPlotReload} from './plot-contracts.mjs';
import {checkVariableListReload} from './variable-list-contracts.mjs';
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {cp,mkdir,mkdtemp,readFile,readdir,rename,rm,writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join,resolve} from 'node:path';
import {buildToolchain} from './toolchain-build.mjs';
import {verifyToolchain} from './toolchain-inventory.mjs';
import {execute} from './project.mjs';
import {root} from './server.mjs';
import {sourceEditor} from './editor-controls.mjs';
import {guideApplication} from './guide-source.mjs';
import {checkNativeAuthoring} from './native-authoring-contracts.mjs';
import {checkProjectDiagnosis} from './project-diagnosis-contracts.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
const engines = await import('../browser/node_modules/playwright/index.mjs');
const temporary = await mkdtemp(join(tmpdir(),'volang-ui-tools-'));
const evidence = resolve(root,'target/ui-next/toolchain-project');
await mkdir(evidence,{recursive:true});
const env = {...process.env,VOWORK:'off',PATH:'/usr/bin:/bin:/usr/sbin:/sbin',
  VO_TEST_COMPILER:join(temporary,'missing-compiler'),CARGO_TARGET_DIR:join(temporary,'missing-cargo-target')};
delete env.VO_UI_TOOLCHAIN;
env.VO_UI_NODE = process.execPath;
let kit, browser, publicTestingCases = 0;
const running = new Set(), results = [];
const hash = bytes => createHash('sha256').update(bytes).digest('hex');
const executable = () => join(kit,'bin',process.platform === 'win32' ? 'vo.exe' : 'vo');
const cli = (...args) => execute(executable(),['ui',...args],{cwd:temporary,env});

async function start(command,project) {
  const child = spawn(executable(),['ui',command],{cwd:project,env,stdio:['ignore','pipe','pipe']});
  let output = '', ready, failed, timer;
  const url = new Promise((resolve,reject) => {ready=resolve; failed=reject;});
  const done = new Promise((resolve,reject) => {child.once('error',reject);child.once('close',(code,signal) => resolve({code,signal}));});
  const collect = bytes => {
    output = (output + bytes).slice(-65536);
    const match = output.match(/http:\/\/127\.0\.0\.1:\d+\/[^\s]*/);
    if (match) {clearTimeout(timer);ready(match[0]);}
  };
  child.stdout.on('data',collect);child.stderr.on('data',collect);
  child.once('error',failed);child.once('close',() => {clearTimeout(timer);failed(new Error(output || 'UI command closed before serving.'));});
  timer = setTimeout(() => failed(new Error('UI command did not serve: ' + output)),120000);
  const service = {get output(){return output;},async close(){
    if (!running.delete(service)) return;
    clearTimeout(timer);child.kill('SIGTERM');
    let timeout;
    try {
      const status = await Promise.race([done,new Promise((_,reject) => {timeout=setTimeout(() => {child.kill('SIGKILL');reject(new Error('UI command did not stop.'));},10000);})]);
      assert.equal(status.signal,null);assert.equal(status.code,0);
    } finally {clearTimeout(timeout);}
  }};
  running.add(service);
  try {return Object.assign(service,{url:await url});}
  catch (error) {await service.close();throw error;}
}
async function saveBuild(project,name) {
  const distribution = join(project,'target/ui-next/dist');
  const build = JSON.parse(await readFile(join(distribution,'build-report.json')));
  for (const artifact of build.artifacts) {
    const bytes = await readFile(join(distribution,artifact.path));
    assert.equal(bytes.length,artifact.bytes);assert.equal(hash(bytes),artifact.sha256);
    if (artifact.path.endsWith('.js') || artifact.path.endsWith('.mjs')) {
      assert(!bytes.includes(Buffer.from(root)),artifact.path + ' retains a checkout path');
      assert(!bytes.includes(Buffer.from(temporary)),artifact.path + ' retains an installation path');
    }
  }
  await rm(join(evidence,name),{recursive:true,force:true});
  await cp(distribution,join(evidence,name),{recursive:true});
  return build;
}
async function publicTests(project,name,expected = 3) {
  console.log(`Portable toolkit: ${name} public check/build/test`);
  const log = name === 'default'
    ? await execute(executable(),['ui','test'],{cwd:project,env})
    : await cli('test',project);
  await writeFile(join(evidence,name + '-test.log'),log);
  const path = log.match(/Browser test report: (.+)\/report\.json/)[1];
  const report = JSON.parse(await readFile(join(path,'report.json')));
  assert.equal(report.stats.expected,expected);assert.equal(report.stats.unexpected,0);assert.equal(report.stats.skipped,0);
  publicTestingCases += report.stats.expected;
  await cp(path,join(evidence,name + '-tests'),{recursive:true});
  return saveBuild(project,name);
}
const deliveredExamples={};
async function exampleProject(name,features = [],cases = 6) {
  const directory=join(temporary,name + ' 中文');await cli('create',directory,'--template',name);
  const sources={};
  for(const [source,target] of [['app.vo','app/app.vo'],['app.css','web/app.css'],['tests/browser/app.test.mjs','tests/browser/app.test.mjs']]) {
    const canonical=await readFile(join(root,'ui/next/examples',name,source));
    assert.deepEqual(await readFile(join(kit,'ui/next/examples',name,source)),canonical,'packaged example differs from canonical source');
    assert.deepEqual(await readFile(join(directory,target)),canonical,'public create changed the delivered example');
    sources[source]=hash(canonical);
  }
  assert.deepEqual(JSON.parse(await readFile(join(directory,'ui-next.json'))).features ?? [],features);
  deliveredExamples[name]=sources;
  const source=await readFile(join(directory,'app/app.vo'),'utf8');
  return {directory,source,build:await publicTests(directory,name,cases)};
}

try {
  kit = await buildToolchain(join(temporary,'original-kit'));
  const inventory=JSON.parse(await readFile(join(kit,'tools/toolchain.json')));
  assert(!inventory.artifacts.some(file => file.path === 'tools/toolchain-source.mjs'
    || file.path.startsWith('tools/studio') || file.path.endsWith('/test_compiler.mjs')
    || file.path.startsWith('ui/next/tests/')
    || /^tools\/[^/]+\.test\.mjs$/.test(file.path)));
  assert.deepEqual(inventory.artifacts.filter(file=>file.path.startsWith('ui/next/examples/')).map(file=>file.path).sort(),deliveredExampleFiles.map(path=>'ui/next/'+path).sort());
  const catalog=JSON.parse(await readFile(join(kit,'tools/project-templates.json')));
  for(const help of [await cli('create','--help'),await execute(process.execPath,[join(kit,'ui.mjs'),'create','--help'],{cwd:temporary,env})]) {
    assert.deepEqual(help.match(/--template ([^\]]+)\]/)[1].split('|').sort(),Object.keys(catalog).sort());
  }
  assert(!inventory.packages.some(item => ['vue','svelte','fflate'].includes(item.name)));
  assert(inventory.artifacts.some(file => file.path === 'tools/project-backend.mjs'));
  for (const command of ['create','check','build','dev','preview','test','verify','browsers']) {
    assert.match(await cli(command,'--help'),/usage: vo ui create/);
  }
  let project = join(temporary,'original-project');
  await cli('create',project);
  const applicationId=JSON.parse(await readFile(join(project,'ui-next.json'))).desktop.identifier;
  assert.match(applicationId,/^dev\.volang\.app[0-9a-f]{32}$/);
  assert(!(await readFile(join(project,'tests/browser/fixtures.mjs'),'utf8')).includes(kit));
  await rename(kit,join(temporary,'Moved tools 中文'));kit=join(temporary,'Moved tools 中文');
  await rename(project,join(temporary,'Moved project 中文'));project=join(temporary,'Moved project 中文');
  assert.equal(JSON.parse(await readFile(join(project,'ui-next.json'))).desktop.identifier,applicationId);
  console.log(await cli('verify'));
  const nativeAuthoring = await checkNativeAuthoring(executable(),project,{env});
  const projectDiagnosis=await checkProjectDiagnosis(executable(),project,{env});
  assert(inventory.artifacts.some(file => file.path === 'editors/volang-ui-authoring.vsix'));
  const lock = await readFile(join(project,'vo.lock'),'utf8');
  const ordinaryConfigPath=join(project,'ui-next.json'),ordinaryConfig=JSON.parse(await readFile(ordinaryConfigPath));
  await writeFile(ordinaryConfigPath,JSON.stringify({...ordinaryConfig,wireVersion:0}));
  try {
    for (const command of ['build','dev','test']) {
      await assert.rejects(execute(executable(),['ui',command],{cwd:project,env}),/matching experimental UI host/);
    }
  } finally {await writeFile(ordinaryConfigPath,JSON.stringify(ordinaryConfig));}
  await execute(executable(),['ui','check'],{cwd:project,env});
  await cli('build',project);
  await writeFile(ordinaryConfigPath,JSON.stringify({...ordinaryConfig,defaultBackend:'vm'}));
  const ordinary = await publicTests(project,'default');
  assert((await readFile(join(project,'target/ui-next/dist/index.html'),'utf8')).includes('<meta name="ui-next-backend" content="vm">'));
  assert.equal(await readFile(join(project,'vo.lock'),'utf8'),lock);
  const productionGuest=await readFile(join(project,'target/ui-next/dist/assets/app.vob'));
  for(const name of ['inspect','develop']) assert(!productionGuest.includes(Buffer.from(`github.com/vo-lang/ui/next/${name}`)),`production linked ${name}`);
  console.log('Portable toolkit: declared props through the delivered development CLI');
  const starterDevelopment=await start('dev',project);
  browser=await engines.chromium.launch({headless:true});const starterPage=await browser.newPage();
  await starterPage.goto(starterDevelopment.url);
  const inspector=starterPage.locator('[data-ui-inspector]');
  await inspector.getByText('Inspect components',{exact:true}).click();
  await inspector.getByText('starter.App',{exact:true}).click();
  await inspector.locator('[data-prop=initial]').waitFor();
  assert.equal(await inspector.locator('[data-prop=initial] td').last().textContent(),'""');
  await starterPage.getByRole('button',{name:'Make it happen',exact:true}).click();
  await starterPage.waitForFunction(()=>document.querySelector('[data-count]').textContent.startsWith('1 '));
  await inspector.getByRole('button',{name:'Capture snapshot',exact:true}).click();
  await starterPage.waitForFunction(()=>{
    const root=document.querySelector('[data-ui-inspector]').shadowRoot;
    return [...root.querySelectorAll('tbody tr')].some(row=>row.firstElementChild.textContent==='count'&&row.lastElementChild.textContent==='1');
  });
  await browser.close();browser=undefined;await starterDevelopment.close();

  const guideProject=join(temporary,'First steps 中文');await cli('create',guideProject);
  assert.notEqual(JSON.parse(await readFile(join(guideProject,'ui-next.json'))).desktop.identifier,applicationId);
  const guideSource=guideApplication(await readFile(join(kit,'ui/next/guides/first-steps.md'),'utf8'));
  await writeFile(join(guideProject,'app/app.vo'),guideSource);
  await cp(join(root,'eng/ui-next/fixtures/guide-app.test.mjs'),join(guideProject,'tests/browser/app.test.mjs'));
  const guideBuild=await publicTests(guideProject,'first-steps');

  const formsProject=join(temporary,'Forms 中文');await cli('create',formsProject);
  const formsSource=guideApplication(await readFile(join(kit,'ui/next/guides/forms.md'),'utf8'));
  await writeFile(join(formsProject,'app/app.vo'),formsSource);
  await cp(join(root,'eng/ui-next/fixtures/forms-app.test.mjs'),join(formsProject,'tests/browser/app.test.mjs'));
  const formsBuild=await publicTests(formsProject,'forms',6);

  const pages = join(temporary,'Pages 中文');await cli('create',pages,'--template','pages');
  const pageBuild = await publicTests(pages,'pages');assert.equal(pageBuild.entries.length,2);
  const dataBlockProject=join(temporary,'Document data 中文');await cli('create',dataBlockProject);
  const dataBlockSource=await readFile(join(root,'eng/ui-next/fixtures/data-block-app.vo'),'utf8');
  await writeFile(join(dataBlockProject,'app/app.vo'),dataBlockSource);
  await cp(join(root,'eng/ui-next/fixtures/data-block-app.test.mjs'),join(dataBlockProject,'tests/browser/app.test.mjs'));
  const dataBlockBuild=await publicTests(dataBlockProject,'document-data',6);
  const kitProject=join(temporary,'Native kit 中文');await cli('create',kitProject);
  const kitSource=await readFile(join(root,'eng/ui-next/fixtures/kit-composition-app.vo'),'utf8');
  await writeFile(join(kitProject,'app/app.vo'),kitSource);
  await cp(join(root,'eng/ui-next/fixtures/kit-composition-app.test.mjs'),join(kitProject,'tests/browser/app.test.mjs'));
  const kitBuild=await publicTests(kitProject,'kit-composition',6);
  const fieldnotes = join(temporary,'Reading room 中文');await cli('create',fieldnotes,'--template','fieldnotes');
  const serverBuild = await publicTests(fieldnotes,'fieldnotes');assert.equal(serverBuild.server.requestProtocol,4);
  assert((await readFile(join(fieldnotes,'target/ui-next/dist/server/document.html'),'utf8')).includes('<meta name="ui-next-backend" content="vm">'));

  const listening = join(temporary,'Listening room 中文');await cli('create',listening,'--template','listening');
  const mediaBuild = await publicTests(listening,'listening');

  const {directory:canvasProject,source:canvasSource,build:canvasBuild}=await exampleProject('canvas',['canvas']);
  assert.deepEqual(canvasBuild.features,['canvas']);assert.deepEqual(canvasBuild.thirdParty,[]);
  const {directory:plotProject,source:plotSource,build:plotBuild}=await exampleProject('plot',['plot']);
  assert.deepEqual(plotBuild.features,['plot']);
  assert.deepEqual(plotBuild.thirdParty.map(({name,version,license})=>({name,version,license})),[{name:'uplot',version:'1.6.32',license:'MIT'}]);
  assert(!ordinary.thirdParty.some(value=>value.name==='uplot'));
  const {directory:scrollProject,source:scrollSource,build:scrollBuild}=await exampleProject('scroll-position');
  assert.deepEqual(scrollBuild.features,[]);
  const {directory:variableProject,source:variableSource,build:variableBuild}=await exampleProject('variable-list',[],9);
  assert.deepEqual(variableBuild.features,[]);

  const editorProject = join(temporary,'Notebook 中文');await cli('create',editorProject);
  await writeFile(join(editorProject,'app/app.vo'),await readFile(resolve(root,'eng/ui-next/fixtures/editor-app.vo.txt')));
  const configPath = join(editorProject,'ui-next.json'),config=JSON.parse(await readFile(configPath));
  await writeFile(configPath,JSON.stringify({...config,features:['editor'],prerenderPages:[{path:'/',data:'closed'},{path:'/edit',data:'open'}]},null,2));
  await execute(join(kit,'bin',process.platform === 'win32' ? 'vo.exe' : 'vo'),['fmt',editorProject],{cwd:temporary,env:{...env,VOWORK:join(editorProject,'vo.work')}});
  console.log('Portable toolkit: optional editor and native SSR');
  await cli('check','--project',editorProject);
  await cli('build','--project',editorProject);
  const editorBuild = await saveBuild(editorProject,'editor');
  assert.equal(editorBuild.thirdParty.length,14);
  assert(!ordinary.thirdParty.some(item => item.name.includes('codemirror')));
  const preview = await start('preview',editorProject);
  for (const engine of ['chromium','firefox','webkit']) {
    browser = await engines[engine].launch({headless:true});
    const native = await browser.newPage({javaScriptEnabled:false});await native.goto(new URL('edit/',preview.url).href);
    await native.getByRole('textbox',{name:'Source code'}).fill('Native editor');await native.close();
    for (const backend of ['vm']) {
      const page = await browser.newPage(),errors=[],requests=[];
      page.on('pageerror',error => errors.push(error.message));page.on('request',request => requests.push(request.url()));
      await page.goto(new URL('?backend=' + backend,preview.url).href);
      await page.waitForFunction(() => document.getElementById('status')?.textContent === '');
      assert(!requests.some(url => /editor-library.*\.js/.test(url)));
      await page.getByRole('button',{name:'Show editor',exact:true}).click();
      await page.locator('.cm-content').waitFor();
      const control=sourceEditor(page,'project-source');await control.fill('package main\nfunc main() { println("Portable 中文") }');
      await page.waitForFunction(() => document.querySelector('[data-source]')?.textContent.includes('Portable 中文'));
      assert.deepEqual(errors,[]);await page.close();results.push({engine,backend,editor:true});
    }
    await browser.close();browser=undefined;
  }
  await preview.close();

  console.log('Portable toolkit: named-page source reload through the delivered CLI');
  const development = await start('dev',pages);
  browser = await engines.chromium.launch({headless:true});const page = await browser.newPage(),documents=[];
  page.on('framenavigated',frame => {if(frame === page.mainFrame()) documents.push(frame.url());});
  await page.goto(new URL('notes/morning?backend=vm',development.url).href);
  await page.waitForFunction(() => document.getElementById('status')?.textContent === '');
  const field = page.getByRole('textbox',{name:'Your note'}),savedData='A saved </script><!--<script> & 中文';
  await field.fill(savedData);await page.getByRole('button',{name:'Keep this thought'}).click();
  await page.waitForFunction(saved=>JSON.parse(document.querySelector('#page-data').textContent).saved===saved,savedData);
  await field.fill('A portable draft 中文');
  await field.evaluate(element => {element.setSelectionRange(2,8,'backward');});
  const initialDocuments=documents.length;
  const notePath=join(pages,'notes/app/app.vo'),source=await readFile(notePath,'utf8');
  assert(source.includes('A little room to write.'));await writeFile(notePath,source.replace('A little room to write.','A travelling notebook'));
  await page.getByRole('heading',{name:'A travelling notebook',exact:true}).waitFor();
  await page.locator('#ui-development-reload').waitFor();
  assert.equal(documents.length,initialDocuments,'compiler cache writes caused a document reload');
  assert.equal(await field.inputValue(),'A portable draft 中文');
  assert.deepEqual(await field.evaluate(element => [element.selectionStart,element.selectionEnd,element.selectionDirection]),[2,8,'backward']);
  assert.deepEqual(await page.locator('#page-data').evaluate(element=>JSON.parse(element.textContent)),{name:'A travelling notebook',saved:savedData});
  const invalid=join(pages,'notes/invalid.vo');await writeFile(invalid,'package main\nfunc unfinished(\n');
  await page.locator('#ui-development-error').waitFor();
  assert.equal(await field.inputValue(),'A portable draft 中文');await rm(invalid);
  await page.locator('#ui-development-error').waitFor({state:'hidden'});
  assert.equal(documents.length,initialDocuments,'compile recovery discarded the current document');
  assert.equal((development.output.match(/Application ready in /g) ?? []).length,3,'compiler cache writes scheduled extra builds');
  await browser.close();browser=undefined;await development.close();

  console.log('Portable toolkit: Canvas source reload through the delivered CLI');
  const canvasDevelopment=await start('dev',canvasProject);
  browser=await engines.chromium.launch({headless:true});const bitmapPage=await browser.newPage();
  await bitmapPage.goto(canvasDevelopment.url);await bitmapPage.locator('canvas').waitFor();
  await bitmapPage.getByRole('button',{name:'Change palette'}).click();
  await bitmapPage.getByRole('img',{name:/copper Mandelbrot/}).waitFor();
  const pixels=await bitmapPage.locator('canvas').evaluate(canvas=>{window.previousCanvas=canvas;return canvas.toDataURL();});
  await writeFile(join(canvasProject,'app/app.vo'),canvasSource.replace('A world in small pixels.','A world of small wonders.'));
  await bitmapPage.getByRole('heading',{name:'A world of small wonders.'}).waitFor();
  await bitmapPage.getByRole('img',{name:/copper Mandelbrot/}).waitFor();
  assert.equal(await bitmapPage.locator('canvas').evaluate(canvas=>canvas.toDataURL()),pixels);
  assert.deepEqual(await bitmapPage.evaluate(()=>[window.previousCanvas.width,window.previousCanvas.height]),[0,0]);
  await browser.close();browser=undefined;await canvasDevelopment.close();

  console.log('Portable toolkit: plot source reload through the delivered CLI');
  const plotDevelopment=await start('dev',plotProject);
  browser=await engines.chromium.launch({headless:true});
  const plotReload=await checkPlotReload(await browser.newPage(),plotDevelopment.url,()=>
    writeFile(join(plotProject,'app/app.vo'),plotSource.replace('A little more green.','A little room to grow.')));
  await browser.close();browser=undefined;await plotDevelopment.close();

  console.log('Portable toolkit: scroll position source reload through the delivered CLI');
  const scrollDevelopment=await start('dev',scrollProject);
  browser=await engines.chromium.launch({headless:true});const scrollPage=await browser.newPage();
  await scrollPage.goto(scrollDevelopment.url);
  await scrollPage.getByRole('button',{name:'Extend and jump'}).click();
  await scrollPage.waitForFunction(()=>document.querySelector('[data-position]')?.textContent==='0,2500');
  await writeFile(join(scrollProject,'app/app.vo'),scrollSource.replace('Find your place.','A little space to explore.'));
  await scrollPage.getByRole('heading',{name:'A little space to explore.'}).waitFor();
  assert.equal(await scrollPage.locator('#scroll-area').evaluate(element=>element.scrollTop),2500,'reload animated or lost the native position');
  await scrollPage.getByRole('button',{name:'Jump',exact:true}).click();
  await scrollPage.waitForFunction(()=>document.querySelector('[data-position]')?.textContent==='40,480');
  await browser.close();browser=undefined;await scrollDevelopment.close();

  console.log('Portable toolkit: measured list source reload through the delivered CLI');
  const variableDevelopment=await start('dev',variableProject);
  browser=await engines.chromium.launch({headless:true});
  const variableReload=await checkVariableListReload(await browser.newPage(),variableDevelopment.url,()=>
    writeFile(join(variableProject,'app/app.vo'),variableSource.replace('Room for every thought.','Space for the next thought.').replace('width = "260.25px"','width = "220.75px"')));
  await browser.close();browser=undefined;await variableDevelopment.close();

  // Missing metadata must not load the checkout-only fallback in the package.
  const manifestPath=join(kit,'tools/toolchain.json'),manifest=await readFile(manifestPath);
  await rename(manifestPath,manifestPath + '.saved');
  await assert.rejects(cli('verify'),/Web UI tools are not installed/);
  await assert.rejects(execute(process.execPath,[join(kit,'ui.mjs'),'verify'],{cwd:temporary,env}),/manifest is missing/);await rename(manifestPath + '.saved',manifestPath);
  const verification=await verifyToolchain(kit);
  await writeFile(join(evidence,'toolchain-manifest.json'),manifest);
  await writeFile(join(evidence,'report.json'),JSON.stringify({passed:true,toolchain:verification,publicTestingCases,editorCases:results,deliveredExamples,nativeAuthoring,projectDiagnosis,
    firstSteps:{passed:true,publicCases:3,deliveredGuide:true,sourceSha256:hash(guideSource)},
    forms:{passed:true,publicCases:6,deliveredGuide:true,sourceSha256:hash(formsSource)},
    development:{passed:true,namedEntry:'notes',state:true,selection:true,errorRecovery:true,cleanShutdown:true},
    documentData:{passed:true,publicCases:9,rawCases:6,sourceReload:true,savedRetained:true,newMetadataApplied:true,sourceSha256:hash(source),rawSourceSha256:hash(dataBlockSource)},
    kitComposition:{passed:true,publicCases:6,nativeModifiers:true,retainedItems:true,customContent:true,sourceSha256:hash(kitSource)},
    plot:{...plotReload,publicCases:6,sourceSha256:hash(plotSource)},
    canvas:{passed:true,publicCases:6,sourceReload:true,paletteRetained:true,backingReleased:true,sourceSha256:hash(canvasSource)},
    scrollPosition:{passed:true,publicCases:6,sourceReload:true,positionRetained:true,newRequest:true,sourceSha256:hash(scrollSource)},
    variableList:{...variableReload,publicCases:9,sourceSha256:hash(variableSource)},
    builds:{default:ordinary,'first-steps':guideBuild,forms:formsBuild,pages:pageBuild,'document-data':dataBlockBuild,'kit-composition':kitBuild,fieldnotes:serverBuild,listening:mediaBuild,editor:editorBuild,canvas:canvasBuild,plot:plotBuild,'scroll-position':scrollBuild,'variable-list':variableBuild},
    contracts:['toolchain-and-project-relocation','no-checkout-or-cargo-required','fixed-compiler-identity','manifest-required','same-package-inventory-after-use',
      'production-inspector-excluded','development-declared-props','development-independent-state','complete-delivered-guide-application','complete-delivered-example-applications',
      'native-cli-relative-discovery','standard-project-cli','portable-browser-fixtures','three-browser-engines','wasm-vm','named-page-images','request-time-ssr',
      'direct-project-commands','web-command-alias','subcommand-help','configured-production-backend',
      'manifest-project-selection','current-directory-project-commands','positional-project-commands',
      'native-document-data','variable-list-and-pinned-focus','scroll-position-and-guard','optional-plot-and-lifetime','optional-canvas-and-lifetime','optional-editor-and-notices','no-editor-in-basic-app','native-ssr-controls','source-reload','compiler-cache-does-not-reload-page','failed-compile-recovery','graceful-command-shutdown']},null,2) + '\n');
  console.log(`Portable toolchain passed: ${publicTestingCases} public browser cases, 3 editor cases and named-page source reload`);
} finally {
  await browser?.close();for (const service of [...running]) await service.close();
  await rm(temporary,{recursive:true,force:true});
}
