import {test} from 'node:test';
import assert from 'node:assert/strict';
import {mkdtemp,writeFile,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {desktopConfig,desktopHtml} from './project-desktop.mjs';
import {desktopArtifact,readDesktopSdk} from './desktop-sdk-manifest.mjs';
import {resolveToolchain,toolchainSchema,toolchainPathNames} from './toolchain-manifest.mjs';

const html='<head><title><!--ui-next:title--></title><meta name="ui-next-backend" content="<!--ui-next:backend-->"></head><body><main id="root"><!--ui-next:content--></main><script type="application/json" id="ui-next-data"><!--ui-next:data--></script><script type="module" src="<!--ui-next:assets-->assets/app.js"></script></body>';
test('desktop document retains authored structure, escaped title and initial data',()=>{
  const built=desktopHtml(html,{desktop:{title:'A <桌面>',data:'</script> 🌿',identifier:'dev.example.test'}});
  assert.match(built,/A &lt;桌面&gt;/);assert.match(built,/volang-desktop-bootstrap/);
  assert.match(built,/\\u003c\/script\\u003e/);assert(!built.includes('assets/app.js'));
  assert.throws(()=>desktopHtml(html+html,{desktop:{identifier:'dev.example.test'}}));assert.throws(()=>desktopHtml('<main></main>',{desktop:{identifier:'dev.example.test'}}));
});
test('desktop project contract requires explicit server adaptation and bounded window settings',()=>{
  assert.equal(desktopConfig({desktop:{identifier:'dev.example.test'}}).entry,'.');
  assert.throws(()=>desktopConfig({}),/stable desktop.identifier/);
  for(const desktop of [{width:0},{height:16385},{title:'\n'},{identifier:'a/b'},{entry:'/outside'},{data:5},{unknown:true},[]])assert.throws(()=>desktopConfig({desktop:Array.isArray(desktop)?desktop:{identifier:'dev.example.test',...desktop}}));
  assert.throws(()=>desktopConfig({serverEntry:'server'}),/declare desktop.entry/);
  assert.equal(desktopConfig({serverEntry:'server',desktop:{entry:'desktop',identifier:'dev.example.test'}}).entry,'desktop');
});
test('optional native SDK keeps existing Web-only toolchain manifests valid',()=>{
  const value={schema:toolchainSchema,platform:process.platform,arch:process.arch,nodeMajor:24,wireVersion:25,serverProtocol:1,
    paths:Object.fromEntries(toolchainPathNames.map(name=>[name,name]))};
  assert.equal(resolveToolchain(value,'/tools').desktop,undefined);
  value.paths.desktop='desktop';assert.match(resolveToolchain(value,'/tools').desktop,/desktop$/);
  value.paths.desktop='../escape';assert.throws(()=>resolveToolchain(value,'/tools'));
});
test('SDK integrity is checked before native tools execute',async()=>{
  const directory=await mkdtemp(join(tmpdir(),'vo-desktop-sdk-'));
  try {
    await writeFile(join(directory,'runner'),'binary');
    const sdk={schema:'volang.ui-desktop-sdk.v2',platform:process.platform,arch:process.arch,profile:'dev',wireVersion:25,
      runner:await desktopArtifact(directory,'runner'),runtime:null,nativeLink:[]};
    await writeFile(join(directory,'desktop-sdk.json'),JSON.stringify(sdk));
    assert.equal((await readDesktopSdk(directory)).runtime,null);
    await writeFile(join(directory,'desktop-sdk.json'),JSON.stringify({...sdk,schema:'volang.ui-desktop-sdk.v1'}));
    await assert.rejects(readDesktopSdk(directory),/incompatible/);
    await writeFile(join(directory,'desktop-sdk.json'),JSON.stringify(sdk));
    await writeFile(join(directory,'runner'),'damage');await assert.rejects(readDesktopSdk(directory),/mismatch/);
    sdk.platform='other';await writeFile(join(directory,'desktop-sdk.json'),JSON.stringify(sdk));await assert.rejects(readDesktopSdk(directory),/incompatible/);
  } finally {await rm(directory,{recursive:true,force:true});}
});
