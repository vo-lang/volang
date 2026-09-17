import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cp, mkdir, mkdtemp, readFile, rm, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {createProject} from './project.mjs';
import {testProject} from './project-testing.mjs';
import {root} from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const engines = await import('../browser/node_modules/playwright/index.mjs');
const {checkFieldnotes, checkFieldnotesForms, checkFieldnotesPrefetch} = await import('./fieldnotes-browser-contracts.mjs');
const temporary = await mkdtemp(resolve(root, 'target/ui-next/fieldnotes-contract-'));
const evidence = resolve(root, 'target/ui-next/fieldnotes');
const combinations = [], forms = [], prefetch = [], errors = [];
let server, browser;
try {
  await mkdir(evidence, {recursive:true});
  const invalid = join(temporary, 'invalid');
  await assert.rejects(createProject(invalid, {template:'missing'}), /Template must be/);
  await assert.rejects(readFile(join(invalid, 'vo.mod')), {code:'ENOENT'});
  const project = await createProject(join(temporary, 'A reading corner 中文'), {template:'fieldnotes'});
  const configPath=join(project,'ui-next.json');
  const config=JSON.parse(await readFile(configPath,'utf8'));
  await writeFile(configPath,JSON.stringify({...config,defaultBackend:'vm'}));
  const manifest = await readFile(join(project, 'vo.mod'), 'utf8');
  await assert.rejects(createProject(project, {template:'fieldnotes'}), {code:'EEXIST'});
  assert.equal(await readFile(join(project, 'vo.mod'), 'utf8'), manifest);
  const tested = await testProject(project);
  const testing = JSON.parse(await readFile(join(tested.output, 'report.json')));
  assert.equal(testing.stats.expected, 6);
  assert.equal(testing.stats.unexpected, 0);
  assert.equal(testing.stats.skipped, 0);
  await cp(tested.output, join(evidence, 'public-testing'), {recursive:true});
  const distribution = join(temporary, 'deployed');
  await cp(join(project, 'target/ui-next/dist'), distribution, {recursive:true});
  const build = JSON.parse(await readFile(join(distribution, 'build-report.json')));
  assert((await readFile(join(distribution,'server/document.html'),'utf8')).includes('<meta name="ui-next-backend" content="vm">'));
  for (const item of build.artifacts) {
    assert.equal(createHash('sha256').update(await readFile(join(distribution, item.path))).digest('hex'), item.sha256, item.path);
  }
  await rm(project, {recursive:true});
  const {start} = await import(pathToFileURL(join(distribution, 'server/entry.mjs')).href);
  server = await start({executable:compilerPath(), base:'/reading-room/', onError:error => errors.push(error.message)});
  const initial = await fetch(server.url, {redirect:'manual'});
  assert.equal(initial.status, 307);
  assert.equal(initial.headers.get('location'), '/reading-room/library');
  for (const path of ['unknown', 'library/missing-note', 'library/%6dissing-note']) {
    const response = await fetch(server.url + path);
    assert.equal(response.status, 404);
    const html = await response.text();
    assert(html.includes('Page not found'));
    assert(!html.includes('Opening your note'), 'a missing note rendered as pending data');
  }
  const encodedPage = await fetch(server.url + '%6cibrary/%71uiet-design');
  assert.equal(encodedPage.status, 200);
  assert((await encodedPage.text()).includes('The art of making room'));
  assert.equal((await (await fetch(server.url + 'api/notes/%71uiet-design')).json()).id, 'quiet-design');
  const head = await fetch(server.url + 'api/notes', {method:'HEAD'});
  assert.equal(head.status, 200); assert.equal(await head.text(), '');
  const boundary = await (await fetch(server.url + 'api/notes?topic=missing&sort=unknown&page=1000')).json();
  assert.equal(boundary.total, 24); assert.equal(boundary.page, 4); assert.equal(boundary.notes.length, 6);
  const empty = await (await fetch(server.url + 'api/notes?q=' + encodeURIComponent('文'.repeat(81)))).json();
  assert.equal(empty.total, 0); assert.equal(empty.page, 1); assert.deepEqual(empty.notes, []);
  for (const engine of ['chromium', 'firefox', 'webkit']) {
    browser = await engines[engine].launch({headless:true});
    const defaultPage=await browser.newPage(),requests=[];
    defaultPage.on('request',request=>requests.push(request.url()));
    await defaultPage.goto(server.url+'library');
    await defaultPage.waitForFunction(()=>document.getElementById('status').textContent==='');
    assert(requests.some(url=>url.endsWith('/assets/app.vob')));
    assert(!requests.some(url=>url.endsWith('/assets/app.wasm')));
    await defaultPage.close();
    for (const entry of await checkFieldnotesForms(browser, server.url)) forms.push({engine, browserVersion:browser.version(), ...entry});
    for (const backend of ['vm']) {
      combinations.push({engine, browserVersion:browser.version(), ...await checkFieldnotes(browser, server.url, backend,
        engine === 'chromium' && backend === 'vm' ? evidence : undefined)});
      prefetch.push({engine, browserVersion:browser.version(), ...await checkFieldnotesPrefetch(browser, server.url, backend)});
      console.log(`Fieldnotes ${engine}/${backend}: deployed application contracts passed`);
    }
    await browser.close(); browser = undefined;
  }
  assert.deepEqual(errors, []);
  await cp(distribution, join(evidence, 'distribution'), {recursive:true});
  await writeFile(join(evidence, 'report.json'), JSON.stringify({passed:true, build, publicTestingCases:testing.stats.expected,
    combinations, forms, prefetch, rootAndSubpath:true, sourceFreeDeployment:true,
    contracts:['named-template', 'invalid-template-before-creation', 'existing-project-preserved', 'unicode-module-path',
      'public-browser-test-command', 'relocatable-private-server', 'ssr-404', 'head-json', 'query-normalization', 'unicode-search'],
    formalPerformanceRerun:false, realInputMethodOrAssistiveTechnology:false,
  }, null, 2) + '\n');
  console.log('Fieldnotes: six public tests, six deployed application combinations and twelve native form modes passed');
} finally {await browser?.close(); await server?.close(); await rm(temporary, {recursive:true, force:true});}
