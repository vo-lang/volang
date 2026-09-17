import assert from 'node:assert/strict';
import {test} from 'node:test';
import {desktopRuntimeName,desktopAotArguments} from './desktop-link.mjs';

test('native SDK and compiler link boundary cover Windows, macOS and Linux',()=>{
  for(const platform of ['win32','darwin','linux']) {
    const runtime=desktopRuntimeName(platform),nativeLink=platform==='win32'?['user32.lib','windowsapp.lib']:['-framework','WebKit','-lobjc'];
    assert.equal(runtime,platform==='win32'?'vo_ui_desktop_runtime.lib':'libvo_ui_desktop_runtime.a');
    const args=desktopAotArguments({entry:'Moved project 中文',runtime:`Tools 中文/${runtime}`,output:'Application folder/应用.exe',nativeLink,platform});
    assert.deepEqual(args,['build','Moved project 中文',`--runtime=Tools 中文/${runtime}`,...nativeLink.map(value=>`--link-arg=${value}`),
      ...(platform==='win32'?['--windows-gui']:[]),'-o','Application folder/应用.exe']);
  }
  assert.throws(()=>desktopRuntimeName('unknown'),/Unsupported/);
});
