import assert from 'node:assert/strict';
import {cp, mkdir, mkdtemp, readFile, rm, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {createProject, buildProject, checkProject, execute} from './project.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {serveApplication} from './server-adapter.mjs';
import {decodePage, renderPage} from './server-response.mjs';
import {renderOutput} from './prerender.mjs';
import {testProject} from './project-testing.mjs';
import {serverProtocol} from './server-request.mjs';
import {checkServerForms} from './server-form-contracts.mjs';
import {checkServerBodies} from './server-body-contracts.mjs';
import {checkPageHistory} from './page-history-contracts.mjs';
import {checkHTTP} from './http-contracts.mjs';
import {root} from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const browsers = await import('../browser/node_modules/playwright/index.mjs');
const temporary = await mkdtemp(resolve(root, 'target/ui-next/server-contract-'));
let server, browser;
const reports = [], history = [], forms = [], http = [], errors = [];
try {
  const project = await createProject(join(temporary, 'A dynamic site 中文'));
  const module = (await readFile(join(project, 'vo.mod'), 'utf8')).match(/^module = "([^"]+)"/m)[1];
  await writeFile(join(project, 'app/app.vo'), await readFile(resolve(root, 'eng/ui-next/fixtures/server-app.vo.txt')));
  await writeFile(join(project, 'web/away.html'), '<!doctype html><title>Another document</title><h1>Another document</h1>');
  await mkdir(join(project, 'server'));
  await writeFile(join(project, 'server/main.vo'), (await readFile(resolve(root, 'eng/ui-next/fixtures/server-entry.vo.txt'), 'utf8')).replace('{{module}}', module));
  const configPath = join(project, 'ui-next.json'), config = JSON.parse(await readFile(configPath));
  await writeFile(configPath, JSON.stringify({...config, serverEntry:'server'}, null, 2) + '\n');
  const env = {...process.env, VOWORK:join(project, 'vo.work')};
  await execute(compilerPath(), ['fmt', project], {cwd:project, env});
  await checkProject(project);
  const output = await buildProject(project);
  const build = JSON.parse(await readFile(join(output, 'build-report.json')));
  assert.equal(build.server.requestProtocol, serverProtocol);
  assert.equal(build.prerender, null);
  assert.deepEqual(build.pages, []);
  assert(build.artifacts.some(item => item.path === 'server/app.vob'));
  assert(build.artifacts.some(item => item.path === 'public/assets/app.vob'));
  const previous = await readFile(join(output, 'build-report.json'), 'utf8');
  await writeFile(join(project, 'server/invalid.vo'), 'package main\nfunc unfinished(\n');
  await assert.rejects(buildProject(project));
  assert.equal(await readFile(join(output, 'build-report.json'), 'utf8'), previous, 'invalid server replaced the complete distribution');
  await rm(join(project, 'server/invalid.vo'));
  await writeFile(configPath, JSON.stringify({...config, serverEntry:'server', prerenderPages:[{path:'/'}]}));
  await assert.rejects(buildProject(project), /serverEntry or prerenderPages/);
  await writeFile(configPath, JSON.stringify({...config, serverEntry:'server'}));

  await writeFile(join(project, 'tests/browser/app.test.mjs'), `import {test, expect} from './fixtures.mjs';
test('a server page activates with its initial data', async ({page, appURL}) => {
  const url = new URL(appURL); url.pathname += 'profile'; url.searchParams.set('name', 'Browser');
  await page.goto(url.href);
  await page.getByRole('textbox', {name:'Your name'}).fill('After startup');
  await expect(page.getByRole('heading', {name:'Hello, After startup.', exact:true})).toBeVisible();
  await page.getByRole('button', {name:'One more', exact:true}).click();
  await expect(page.locator('output')).toHaveText('1');
  await expect(page.locator('[data-profile]')).toHaveText('Profile for Browser');
});\n`);
  const tested = await testProject(project);
  const testing = JSON.parse(await readFile(join(tested.output, 'report.json')));
  assert.equal(testing.stats.expected, 6); assert.equal(testing.stats.unexpected, 0);
  await cp(tested.output, resolve(root, 'target/ui-next/server-testing'), {recursive:true});

  // Move only distributable files. The bundled Node entry resolves its own files
  // and the prepared bytecode works without source/workspace lookup at request time.
  const deployed = join(temporary, 'deployed');
  await cp(output, deployed, {recursive:true});
  await rm(project, {recursive:true});
  const hostSource = await readFile(join(deployed, 'server/entry.mjs'), 'utf8');
  assert(!hostSource.includes(root) && !hostSource.includes(temporary), 'deployed host contains build-machine paths');
  const {start} = await import(pathToFileURL(join(deployed, 'server/entry.mjs')).href);
  const request = {method:'GET', url:'/', basePath:'/', headers:{}, body:''};
  const nativeOptions = {cwd:deployed, env:{...process.env, VOWORK:'off'}};
  const jit = decodePage(await renderOutput(compilerPath(), ['run', join(deployed, 'server/app.vob'), '--mode=jit'], {
    ...nativeOptions, input:JSON.stringify({version:serverProtocol, request:{...request, url:'/profile?name=NativeJit'}}),
  }));
  assert.equal(jit.headers['x-server-visit'][0], '1');
  assert(jit.html.includes('Profile for NativeJit'));
  for (const envelope of [{version:1, request}, {version:serverProtocol, request:{...request, method:'TRACE'}},
    {version:serverProtocol, request:{...request, url:'https://example.com'}},
    {version:serverProtocol, request:{...request, headers:{'Uppercase':['x']}}}]) {
    await assert.rejects(renderOutput(compilerPath(), ['run', join(deployed, 'server/app.vob')], {...nativeOptions, input:JSON.stringify(envelope)}));
  }
  for (const [path, expected] of [['bad-header', /framing header/], ['bad-data', /page data or metadata/], ['bad-status', /page status/]]) {
    await assert.rejects(renderPage(compilerPath(), join(deployed, 'server/app.vob'), {...request, url:'/' + path}, nativeOptions), expected);
  }
  // JSON can expand an accepted body past the old 1 MiB render-input budget.
  const expanded = {...request, method:'POST', url:'/no-content', body:'\0'.repeat(1024 * 1024)};
  assert.equal((await renderPage(compilerPath(), join(deployed, 'server/app.vob'), expanded, nativeOptions)).status, 204);
  for (const invalid of [{...expanded, body:expanded.body + 'x'}, {...request, body:'unexpected'}]) {
    await assert.rejects(renderPage(compilerPath(), join(deployed, 'server/app.vob'), invalid, nativeOptions));
  }
  server = await start({executable:compilerPath(), base:'/ideas/', onError:error => errors.push(error.message)});
  const names = ['Ada <&" 中文', 'Grace'];
  const isolated = await Promise.all(names.map(async name => {
    const response = await fetch(server.url + 'profile?name=' + encodeURIComponent(name));
    assert.equal(response.status, 200);
    assert.equal(response.headers.get('x-server-visit'), '1');
    assert.equal(response.headers.get('cache-control'), 'no-store');
    return response.text();
  }));
  assert(isolated[0].includes('Ada &lt;&amp;&quot; 中文') && !isolated[0].includes('Grace'));
  assert(isolated[1].includes('Profile for Grace') && !isolated[1].includes('Ada'));
  const head = await fetch(server.url + 'profile?name=Head', {method:'HEAD'});
  assert.equal(head.status, 200); assert.equal(await head.text(), '');
  assert.equal((await fetch(server.url + 'unknown')).status, 404);
  assert.equal((await fetch(server.url + 'assets/missing.js')).status, 404);
  assert.equal((await fetch(server.url + 'server/app.vob')).status, 404);
  assert.equal((await fetch(server.url + 'profile', {method:'DELETE'})).status, 405);
  const noContent = await fetch(server.url + 'no-content');
  assert.equal(noContent.status, 204); assert.equal(await noContent.text(), '');
  const cookies = await fetch(server.url + 'cookies');
  assert.deepEqual(cookies.headers.getSetCookie(), ['a=1; HttpOnly', 'b=2; SameSite=Lax']);
  const redirect = await fetch(server.url + 'redirect', {redirect:'manual'});
  assert.equal(redirect.status, 303); assert.equal(redirect.headers.get('location'), '/ideas/profile?name=Redirected');
  const failure = await fetch(server.url + 'error');
  assert.equal(failure.status, 500); assert(!((await failure.text()).includes('private loader failure')));
  assert(errors.some(message => message.includes('private loader failure')), JSON.stringify(errors));
  errors.length = 0;

  for(const method of ['GET','HEAD','POST','PUT','PATCH','DELETE','OPTIONS']) {
    const response = await fetch(server.url + 'api/http',{method,
      ...(!['GET','HEAD'].includes(method) ? {body:JSON.stringify({name:'Method 中文'}),headers:{'content-type':'application/json'}} : {})});
    assert.equal(response.status,['GET','HEAD'].includes(method) ? 200 : 201);
    assert.equal(response.headers.get('content-type'),'application/json; charset=utf-8');
    assert.equal(response.headers.get('cache-control'),'no-store');
    if(method === 'HEAD') assert.equal(await response.text(),'');
    else assert.equal((await response.json()).method,method);
  }

  for (const engine of ['chromium', 'firefox', 'webkit']) {
    browser = await browsers[engine].launch({headless:true,
      ...(engine === 'chromium' ? {ignoreDefaultArgs:['--disable-back-forward-cache']} : {})});
    const readable = await browser.newPage({javaScriptEnabled:false});
    await readable.goto(server.url + 'profile?name=Before%20Wasm');
    await readable.getByRole('heading', {name:'Hello, Before Wasm.', exact:true}).waitFor();
    assert.equal(await readable.locator('[data-profile]').textContent(), 'Profile for Before Wasm');
    assert.equal(await readable.title(), 'Before Wasm · Request page');
    await readable.close();
    for (const backend of ['vm']) {
      const page = await browser.newPage(), browserErrors = [], requests = [];
      page.on('pageerror', error => browserErrors.push(error.message));
      page.on('request', request => requests.push(request.url()));
      let release;
      const gate = new Promise(resolve => {release = resolve;});
      await page.route('**/assets/app.*', async route => {await gate; await route.continue();});
      try {
        const name = `Visitor ${engine} 中文`;
        await page.goto(`${server.url}profile?name=${encodeURIComponent(name)}&backend=${backend}`, {waitUntil:'commit'});
        const input = page.getByRole('textbox', {name:'Your name'});
        await input.fill('Edited before startup');
        await input.evaluate(element => {window.serverInput = element;});
        assert.equal(await page.title(), name + ' · Request page');
        assert.equal(await page.locator('meta[name="description"]').getAttribute('content'), 'Profile for ' + name);
        release();
        await page.getByRole('heading', {name:'Hello, Edited before startup.', exact:true}).waitFor();
        assert.equal(await input.evaluate(element => element === window.serverInput), true);
        await page.getByRole('button', {name:'One more', exact:true}).click();
        await page.waitForFunction(() => document.querySelector('output')?.textContent === '1');
        assert.equal(await page.locator('[data-profile]').textContent(), 'Profile for ' + name);
        assert.equal(await page.locator('[data-server-visit]').textContent(), '1');
        assert(!requests.some(value => value.includes('/api/profile?')), 'hydration repeated a fresh server data request');
        assert.deepEqual(browserErrors, []);
        reports.push({engine, browserVersion:browser.version(), backend, passed:true});
      } finally {release(); await page.close();}
    }
    forms.push(...(await checkServerForms(browser, server.url)).map(result => ({engine, ...result})));
    for(const backend of ['vm']) http.push({engine,browserVersion:browser.version(),...await checkHTTP(browser,server.url,backend)});
    for (const backend of ['vm']) history.push({engine, browserVersion:browser.version(), ...await checkPageHistory(browser, server.url, backend)});
    await browser.close(); browser = undefined;
    console.log(engine, 'request-time SSR, metadata, cache and early input passed');
  }
  await server.close(); server = undefined;

  const bodies = await checkServerBodies(deployed);
  let active = 0, aborted = 0, beginSlow;
  const slowStarted = new Promise(resolve => {beginSlow = resolve;});
  const run = async (...args) => {
    active++;
    if (args[2].url.includes('/slow')) beginSlow();
    args[3].signal.addEventListener('abort', () => {aborted++;}, {once:true});
    try {return await renderPage(...args);} finally {active--;}
  };
  server = await serveApplication(deployed, {executable:compilerPath(), concurrency:1, queued:1, render:run, onError:error => errors.push(error.message)});
  const disconnect = new AbortController();
  const slow = fetch(server.url + 'slow', {signal:disconnect.signal}).catch(error => error);
  await slowStarted;
  const waiting = [fetch(server.url + 'profile?name=One'), fetch(server.url + 'profile?name=Two')];
  assert.equal((await Promise.race(waiting)).status, 503);
  disconnect.abort();
  await slow;
  assert.deepEqual((await Promise.all(waiting)).map(response => response.status).sort(), [200, 503]);
  await server.close(); server = undefined;
  assert.equal(active, 0); assert(aborted >= 1);
  server = await serveApplication(deployed, {executable:compilerPath(), timeoutMilliseconds:150, onError:error => errors.push(error.message)});
  assert.equal((await fetch(server.url + 'slow')).status, 504);
  await server.close(); server = undefined;
  assert.deepEqual(errors, []);
  const retained = resolve(root, 'target/ui-next/server-distribution');
  await rm(retained, {recursive:true, force:true});
  await cp(deployed, retained, {recursive:true});
  await writeFile(resolve(root, 'target/ui-next/server-report.json'), JSON.stringify({
    passed:true, build, reports, history, forms, http, bodies, nativeJit:true, noJavaScriptEngines:3, publicTestingCases:testing.stats.expected,
    contracts:['private-compiled-server-entry', 'relocatable-node-host', 'request-isolation', 'get-head-post-status-redirect', 'body-only-standard-forms', 'post-redirect-get', 'native-validation-422', 'page-history-lifetime',
      'escaped-metadata-and-data', 'shared-server-client-data-cache', 'early-input-hydration', 'failed-build-keeps-distribution',
      'bounded-fifo-admission', 'queued-overflow', 'disconnect-cancels-native-process', 'request-deadline', 'close-joins-owned-work'],
  }, null, 2) + '\n');
  console.log('Request-time server contracts passed');
} finally {
  await browser?.close(); await server?.close();
  await rm(temporary, {recursive:true, force:true});
}
