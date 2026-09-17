import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {createHash} from 'node:crypto';
import {createRequire} from 'node:module';
import {readFile,realpath,writeFile} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {pathToFileURL} from 'node:url';

const require = createRequire(new URL('../../ui/editors/vscode/package.json',import.meta.url));
const {createMessageConnection,StreamMessageReader,StreamMessageWriter} = require('vscode-jsonrpc/node');

async function within(promise,label,timeout = 30000) {
  let timer;
  try {return await Promise.race([promise,new Promise((_,reject) => {timer=setTimeout(() => reject(new Error('Timed out: '+label)),timeout);})]);}
  finally {clearTimeout(timer);}
}

export async function checkNativeAuthoring(compiler,project,{env = process.env} = {}) {
  compiler = await realpath(compiler);
  // Project commands resolve their directory before selecting an explicit
  // workspace. Match that authority here, including macOS /var -> /private/var.
  project = await realpath(project);
  const path = join(project,'app/app.vo'),uri = pathToFileURL(path).href;
  const saved = await readFile(path);
  const child = spawn(compiler,['lsp','--stdio'],{cwd:project,env:{...env,VOWORK:join(project,'vo.work')},stdio:['pipe','pipe','pipe']});
  let stderr = '';
  child.stderr.on('data',data => {stderr=(stderr+data).slice(-16384);});
  const exited = new Promise((resolve,reject) => {child.once('error',reject);child.once('close',(code,signal) => resolve({code,signal}));});
  exited.catch(() => {});
  const connection = createMessageConnection(new StreamMessageReader(child.stdout),new StreamMessageWriter(child.stdin));
  const received = [];
  connection.onNotification('textDocument/publishDiagnostics',params => received.push(params));
  connection.listen();
  let revision = 1,source = '',closed = false;
  const timings = [];
  async function request(method,params) {
    const start = performance.now();
    const result = await within(connection.sendRequest(method,params),method);
    timings.push({method,milliseconds:Math.round(performance.now()-start)});
    return result;
  }
  async function set(text) {
    source = text;
    await connection.sendNotification('textDocument/didChange',{textDocument:{uri,version:++revision},contentChanges:[{text}]});
  }
  function at(needle,delta = 1) {
    const offset = source.lastIndexOf(needle) + delta;
    assert(offset >= delta);
    const lines = source.slice(0,offset).split(/\r\n|\r|\n/);
    return {textDocument:{uri},position:{line:lines.length-1,character:lines.at(-1).length}};
  }
  async function diagnostic(predicate) {
    const deadline = Date.now() + 30000;
    while (Date.now() < deadline) {
      const found = received.findLast(item => item.uri === uri && item.version === revision);
      if (found && predicate(found.diagnostics)) return found;
      await new Promise(resolve => setTimeout(resolve,40));
    }
    throw new Error(`Timed out: versioned diagnostics ${JSON.stringify(received.at(-1))}; ${stderr}`);
  }
  try {
    const initialized = await request('initialize',{processId:process.pid,rootUri:pathToFileURL(project).href,capabilities:{}});
    assert.equal(initialized.capabilities.positionEncoding,'utf-16');
    assert.equal(initialized.capabilities.textDocumentSync.change,1);
    await connection.sendNotification('initialized',{});
    source = 'package app\r\nimport ui "github.com/vo-lang/ui/next"\r\nfunc Example() ui.View { println("中文🙂"); return ui.Te }\r\n';
    await connection.sendNotification('textDocument/didOpen',{textDocument:{uri,languageId:'volang',version:revision,text:source}});
    const completion = await request('textDocument/completion',at('ui.Te',5));
    if (!completion) {
      const report = await diagnostic(() => true);
      assert.fail('Framework completion unavailable: '+JSON.stringify(report.diagnostics)+'; '+stderr);
    }
    const textCompletion = completion.items.find(item => item.label === 'Text');
    assert(textCompletion);
    assert.deepEqual(textCompletion.textEdit.range,{start:at('ui.Te',3).position,end:at('ui.Te',5).position});
    await set('package app\nimport ui "github.com/vo-lang/ui/next"\nfunc Example() ui.View { return ui.Text("中文🙂") }\n');
    const definition = await request('textDocument/definition',at('Text'));
    assert.equal(definition.uri,pathToFileURL(join(project,'vendor/ui/next/view.vo')).href);
    await set('package app\nimport ui "github.com/vo-lang/ui/next"\nfunc Example() ui.View { println("中文🙂"); return ui.Text(7) }\n');
    const errors = await diagnostic(items => items.some(item => item.severity === 1));
    const typeError = errors.diagnostics.find(item => item.message.includes('string'));
    assert(typeError);
    assert.deepEqual(typeError.range.start,at('7',0).position);
    await set('package app\nimport (ui "github.com/vo-lang/ui/next"; "fmt")\nfunc Example() ui.View { return ui.Text("ok") }\n');
    await diagnostic(items => items.some(item => item.severity === 2) && items.every(item => item.severity !== 1));
    await set('package app\nimport (ui "github.com/vo-lang/ui/next"; "fmt")\nfunc Example() ui.View { fmt.Println("中文🙂"); return ui.Text("ok") }\n');
    const standard = await request('textDocument/definition',at('Println'));
    assert(standard.uri.startsWith('volang-source:/'));
    assert((await request('volang/source',{uri:standard.uri})).includes('func Println'));
    await diagnostic(items => items.length === 0);
    await connection.sendNotification('textDocument/didClose',{textDocument:{uri}});
    await request('shutdown',null);
    await connection.sendNotification('exit',null);
    child.stdin.end();
    const exit = await within(exited,'language server shutdown',10000);
    assert.equal(exit.code,0,stderr);closed=true;
    assert.deepEqual(await readFile(path),saved,'authoring changed saved application source');
    return {passed:true,completion:true,frameworkDefinition:true,typeDiagnostics:true,warnings:true,
      stdlibSource:true,unicode:true,unchangedDisk:true,cleanShutdown:true,timings,
      compilerSha256:createHash('sha256').update(await readFile(compiler)).digest('hex')};
  } finally {
    connection.dispose();
    if (!closed) {child.kill();await within(exited,'failed server cleanup',10000).catch(() => child.kill('SIGKILL'));}
  }
}

if (process.argv[1] && pathToFileURL(resolve(process.argv[1])).href === import.meta.url) {
  const [compiler,project,report] = process.argv.slice(2);
  if (!compiler || !project || !report || process.argv.length !== 5) throw new Error('Usage: native-authoring-contracts.mjs <compiler> <project> <report>');
  const result = await checkNativeAuthoring(resolve(compiler),resolve(project));
  await writeFile(report,JSON.stringify(result,null,2)+'\n');
  console.log(`Native authoring passed: ${report}`);
}
