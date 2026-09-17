import assert from 'node:assert/strict';
import {test} from 'node:test';
import {mkdtemp,mkdir,readFile,writeFile,rm,rename} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {nativeBuildRequirements,bundleNativeLibraries} from './desktop-native-libraries.mjs';
import {desktopArtifact,readDesktopSdk} from './desktop-sdk-manifest.mjs';
import {desktopAotArguments} from './desktop-link.mjs';
import {execute} from './execute.mjs';

test('Cargo search paths survive large structured output while diagnostics stay bounded',async()=>{
  const work=await mkdtemp(join(tmpdir(),'vo-cargo-output-'));
  try {
    const source=join(work,'Cargo cache 中文'),directory=join(work,'SDK'),stdoutFile=join(work,'cargo.jsonl');
    await mkdir(source);await mkdir(directory);
    await writeFile(join(source,'windows.0.48.5.lib'),'import library');
    const script=`const fs=require('node:fs');
      fs.writeSync(1,JSON.stringify({reason:'build-script-executed',linked_paths:[${JSON.stringify('native='+source)}]})+'\\n');
      for(let i=0;i<1000;i++)fs.writeSync(1,JSON.stringify({reason:'compiler-artifact',features:['x'.repeat(256)]})+'\\n');
      fs.writeSync(2,'diagnostic '.repeat(10000)+'\\nnote: native-static-libs: windows.0.48.5.lib user32.lib\\n');`;
    const diagnostics=await execute(process.execPath,['-e',script],{stdoutFile});
    assert(diagnostics.length<=65536);
    const messages=await readFile(stdoutFile,'utf8');assert(messages.length>65536);
    const requirements=nativeBuildRequirements(messages+'\n'+diagnostics);
    assert.deepEqual(requirements.searchPaths,[source]);
    assert.deepEqual(requirements.nativeLink,['windows.0.48.5.lib','user32.lib']);
    const libraries=await bundleNativeLibraries(directory,{...requirements,platform:'win32'});
    assert.deepEqual(libraries.map(value=>value.path),['native/windows.0.48.5.lib']);
  } finally {await rm(work,{recursive:true,force:true});}
});

test('failed structured commands retain stdout and identify the complete diagnostic file',async()=>{
  const work=await mkdtemp(join(tmpdir(),'vo-cargo-failure-')),stdoutFile=join(work,'cargo.jsonl');
  try {
    await assert.rejects(execute(process.execPath,['-e',"process.stdout.write('retained');process.exitCode=1"],{stdoutFile}),error=>{
      assert(error.message.includes(stdoutFile));return true;
    });
    assert.equal(await readFile(stdoutFile,'utf8'),'retained');
  } finally {await rm(work,{recursive:true,force:true});}
});

test('Cargo native requirements preserve argument order and cached dependency search paths',()=>{
  const log=[
    JSON.stringify({reason:'build-script-executed',linked_paths:['native=C:\\Cargo cache 中文\\lib','framework=/system','C:\\Other']}),
    JSON.stringify({reason:'build-script-executed',linked_paths:['native=C:\\Cargo cache 中文\\lib']}),
    JSON.stringify({reason:'compiler-message',message:{message:'native-static-libs: windows.0.48.5.lib user32.lib windows.0.48.5.lib'}}),
  ].join('\n');
  assert.deepEqual(nativeBuildRequirements(log),{nativeLink:['windows.0.48.5.lib','user32.lib','windows.0.48.5.lib'],searchPaths:['C:\\Cargo cache 中文\\lib','C:\\Other']});
  assert.deepEqual(nativeBuildRequirements('note: native-static-libs: -framework WebKit -lobjc\n').nativeLink,['-framework','WebKit','-lobjc']);
  assert.throws(()=>nativeBuildRequirements('Finished build'),/did not report/);
});

test('SDK import libraries survive relocation, are authenticated, and reject ambiguous providers',async()=>{
  const work=await mkdtemp(join(tmpdir(),'vo-native-libraries-'));
  try {
    const source=join(work,'Cargo'),other=join(work,'Other'),directory=join(work,'SDK');
    for(const path of [source,other,directory])await mkdir(path);
    await writeFile(join(source,'windows.0.48.5.lib'),'import library');
    await writeFile(join(source,'unused.lib'),'unused');
    const nativeLink=['windows.0.48.5.lib','user32.lib','windows.0.48.5.lib'];
    const libraries=await bundleNativeLibraries(directory,{nativeLink,searchPaths:[source],platform:'win32'});
    assert.deepEqual(libraries.map(resource=>resource.path),['native/windows.0.48.5.lib']);
    await writeFile(join(directory,'runner'),'runner');
    const sdk={schema:'volang.ui-desktop-sdk.v3',platform:process.platform,arch:process.arch,profile:'dev',wireVersion:25,
      runner:await desktopArtifact(directory,'runner'),runtime:null,nativeLink,libraries};
    await writeFile(join(directory,'desktop-sdk.json'),JSON.stringify(sdk));
    const moved=join(work,'Moved SDK 中文');await rename(directory,moved);await rm(source,{recursive:true});
    await readDesktopSdk(moved);
    const args=desktopAotArguments({entry:'main.vo',runtime:'runtime.lib',output:'app.exe',nativeLink,libraries,sdkDirectory:moved,platform:'win32'});
    const bundled=`--link-arg=${join(moved,'native/windows.0.48.5.lib')}`;
    assert.equal(args.filter(arg=>arg===bundled).length,2);
    assert(args.includes(`--link-arg=/LIBPATH:${join(moved,'native')}`));
    assert(args.includes('--link-arg=user32.lib'));
    await writeFile(join(moved,libraries[0].path),'changed');
    await assert.rejects(readDesktopSdk(moved),/artifact mismatch/);
    await writeFile(join(other,'windows.0.48.5.lib'),'different');
    await assert.rejects(bundleNativeLibraries(directory,{nativeLink,searchPaths:[join(moved,'native'),other],platform:'win32'}),/Conflicting native link libraries/);
    await writeFile(join(moved,'desktop-sdk.json'),JSON.stringify({...sdk,libraries:[...libraries,...libraries]}));
    await writeFile(join(moved,libraries[0].path),'import library');
    await assert.rejects(readDesktopSdk(moved),/Duplicate/);
  } finally {await rm(work,{recursive:true,force:true});}
});
