import assert from 'node:assert/strict';
import {readFile,writeFile} from 'node:fs/promises';
import {join} from 'node:path';
import {root} from './repository-paths.mjs';
import {desktopAssets} from './desktop-assets.mjs';

// The development-only callback records browser hand-offs. CI exercises real
// WebView navigation without launching the user's browser or fetching a site.
export async function checkDesktopShell({directory,compiler,profile,run}) {
  const assets=join(directory,'shell-assets');
  await desktopAssets(assets,{check:true});
  const path=join(assets,'check.js');
  await writeFile(path,`window.__volangShellCheck = async () => {
    const visit = (href, target = '') => {
      const link = document.createElement('a');
      link.href = href; link.target = target;
      document.body.append(link); link.click(); link.remove();
    };
    visit('#shell-check');
    visit('https://example.com/same?text=中文#one');
    visit('https://example.com/new?value=a&b=two', '_blank');
    visit('file:///volang-does-not-exist');
  };\n`+await readFile(path,'utf8'));
  await run('shell-preview-build','cargo',['build','--locked','--offline','--profile',profile,
    '-p','vo-ui-webview','--features','jit','--example','preview'],{cwd:root});
  const bytecode=join(directory,'shell.vob');
  await run('shell-bytecode',compiler,['emit','bytecode',join(root,'ui/next/examples/interaction'),'-o',bytecode]);
  const preview=join(root,'target',profile==='dev'?'debug':profile,'examples',process.platform==='win32'?'preview.exe':'preview');
  for(const backend of ['vm','jit']) {
    const log=await run(`window-shell-${backend}`,preview,[backend,bytecode,assets,'--check-shell']);
    assert.match(log,/desktop shell: external navigation and application continuity passed/);
    assert.match(log,/desktop completed: Completed/);
  }
}
