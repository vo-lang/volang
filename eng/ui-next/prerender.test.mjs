import assert from 'node:assert/strict';
import test from 'node:test';
import { prepareHtml, renderHtml, dataMarker, assetsMarker, titleMarker, descriptionMarker } from './prerender.mjs';

const run = (source, options) => renderHtml(process.execPath, ['-e', source], options);

test('static template markers are unique and replacement preserves literal source text', () => {
  const template = '<meta name="ui-next-render" content="<!--ui-next:mode-->"><div><!--ui-next:content--></div>';
  const render = prepareHtml(template, true);
  assert.equal(render(), '<meta name="ui-next-render" content="client"><div></div>');
  assert.equal(render('<p>$& $` $\' 中文</p>'), '<meta name="ui-next-render" content="server"><div><p>$& $` $\' 中文</p></div>');
  assert.throws(() => prepareHtml('<div></div>', true), /exactly one/);
  assert.throws(() => prepareHtml(template + '<!--ui-next:content-->', false), /exactly one/);
  assert.equal(prepareHtml('<div></div>', false)(), '<div></div>');
});

test('native prerender captures complete UTF-8 stdout independently of diagnostics', async () => {
  const html = '<main data-vo-id="1">中文</main>';
  const result = await run(`process.stderr.write('a diagnostic'); for (const byte of Buffer.from(${JSON.stringify(html)})) process.stdout.write(Buffer.from([byte]));`);
  assert.equal(result, html);
});

test('page data stays inert and asset paths do not rewrite application HTML', () => {
  const render = prepareHtml(`<meta content="<!--ui-next:mode-->"><script type="application/json">${dataMarker}</script><script src="${assetsMarker}assets/app.js"></script><div><!--ui-next:content--></div>`, true);
  const data = '</script><script>globalThis.injected=true</script> 中文 $& \u2028';
  const html = render('<p><!--ui-next:assets--></p>', { data, assets: '../../' });
  const serialized = html.match(/application\/json">([^]*?)<\/script>/)[1];
  assert(!serialized.includes('<'));
  assert.equal(JSON.parse(serialized), data);
  assert(html.includes('src="../../assets/app.js"'));
  assert(html.endsWith('<div><p><!--ui-next:assets--></p></div>'));
  assert.throws(() => prepareHtml(dataMarker + dataMarker, false), /at most one/);
  assert.throws(() => prepareHtml('', false)(undefined, { data: 'name' }), /Initial data requires/);
  assert.throws(() => prepareHtml('', false)(undefined, { assets: '../' }), /Nested static pages require/);
});

test('page metadata escapes text and attributes while retaining legacy documents', () => {
  const render = prepareHtml(`<title>${titleMarker}</title><meta name="description" content="${descriptionMarker}">`, false);
  assert.equal(render(undefined, { title: '</title> A&B', description: '" onload="bad' }),
    '<title>&lt;/title&gt; A&amp;B</title><meta name="description" content="&quot; onload=&quot;bad">');
  assert.equal(prepareHtml('<title>Authored title</title>', false)(), '<title>Authored title</title>');
  assert.throws(() => prepareHtml('', false)(undefined, { title: 'Missing slot' }), /metadata requires/);
  assert.throws(() => prepareHtml(titleMarker + titleMarker, false), /at most one/);
});

test('prerender delivers a complete bounded initial-data stream', async () => {
  const input = '中文🙂'.repeat(10000);
  const html = await run(`let input='';process.stdin.setEncoding('utf8');process.stdin.on('data',x=>input+=x);process.stdin.on('end',()=>process.stdout.write('<main data-vo-id="1">'+input+'</main>'));`, { input });
  assert.equal(html, '<main data-vo-id="1">' + input + '</main>');
  await assert.rejects(run('process.exit(0)', { input: 'x'.repeat(1024 * 1024 + 1) }), /within 1 MiB/);
});

test('prerender failures, invalid output and resource limits reject', async () => {
  await assert.rejects(run("process.stderr.write('render failed'); process.exit(2)"), /render failed/);
  await assert.rejects(run("console.log('unexpected output')"), /only host.HTML/);
  await assert.rejects(run("console.log('<!--vo:r:1--><!--vo:/r:1-->')"), /only host.HTML/);
  await assert.rejects(run('process.stdout.write(Buffer.from([255]))'), /encoded data/);
  await assert.rejects(run("process.stdout.write('x'.repeat(5000)); setInterval(() => {}, 1000)", { maxBytes: 1024 }), /exceeds 1024 bytes/);
  await assert.rejects(run('setInterval(() => {}, 1000)', { timeoutMilliseconds: 100 }), /exceeded 100 ms/);
  await assert.rejects(renderHtml('/a/missing/prerender-executable', []), { code: 'ENOENT' });
});

test('cancelling prerender ends the child and preserves the cancellation reason', async () => {
  const lifetime = new AbortController();
  const waiting = run('setInterval(() => {}, 1000)', { signal: lifetime.signal });
  const reason = new Error('build superseded');
  lifetime.abort(reason);
  await assert.rejects(waiting, error => error === reason);
  await assert.rejects(run('process.exit(42)', { signal: lifetime.signal }), error => error === reason);
});
