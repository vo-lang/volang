import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {copyFile, mkdir, readFile, writeFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {root} from './repository-paths.mjs';
import {desktopAssets} from './desktop-assets.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';

const directory = resolve(root, 'target/ui-next/desktop');
const assets = resolve(directory,'assets');
const runtime = resolve(directory,'aot-runtime');
await desktopAssets(assets,{check:true});
await desktopAssets(resolve(directory,'manual'));
await mkdir(resolve(runtime,'src'),{recursive:true});
await desktopAssets(resolve(runtime,'assets'),{check:true});
const report = {passed:false,platform:process.platform,arch:process.arch,steps:[]};
const save = () => writeFile(resolve(directory,'build.json'),JSON.stringify(report,null,2)+'\n');
await save();
async function run(name,command,args) {
  console.log(`Desktop adapter: ${name}`);
  const started=performance.now();
  const result=spawnSync(command,args,{cwd:root,env:{...process.env,VOWORK:'off'},encoding:'utf8',timeout:600_000,maxBuffer:16*1024*1024});
  const output=(result.stdout??'')+(result.stderr??'');
  await writeFile(resolve(directory,`${name}.log`),output);
  report.steps.push({name,command:[command,...args],milliseconds:Math.round(performance.now()-started),passed:result.status===0});
  await save(); assert.ifError(result.error);assert.equal(result.status,0,output);
  return output;
}
await run('core-contracts','cargo',['test','-p','vo-ui-webview','-p','vo-ui-native','--locked','--offline']);
await run('window-contracts','cargo',['test','-p','vo-ui-webview','-p','vo-ui-native','--all-features','--locked','--offline']);
await run('preview','cargo',['build','-p','vo-ui-webview','--features','jit','--example','preview','--locked','--offline']);
const preview=process.platform==='win32'?'preview.exe':'preview';
await copyFile(resolve(root,'target/debug/examples',preview),resolve(directory,preview));
await run('application',compilerPath(),['emit','bytecode','ui/next/examples/interaction','-o',resolve(directory,'interaction.vob')]);
await run('failure-application',compilerPath(),['emit','bytecode','ui/next/tests/native_failure','-o',resolve(directory,'failure.vob')]);

// Explicit Unix link probe until the production CLI owns desktop runtime
// manifests. Consume rustc's actual native link requirements; no hand-maintained
// list of platform frameworks or inference from the previous UI runtime.
if (process.platform !== 'win32') {
  const dependency=(name,features=[])=>`${name} = { path = ${JSON.stringify(resolve(root,'lang/crates',name))}, default-features=false, features=${JSON.stringify(features)} }`;
  await writeFile(resolve(runtime,'Cargo.toml'),`[package]\nname="vo-ui-webview-aot-probe"\nversion="0.0.0"\nedition="2021"\n[workspace]\n[lib]\ncrate-type=["staticlib"]\n[dependencies]\n${[
    dependency('vo-ui-webview',['aot']),dependency('vo-ui-bridge'),dependency('vo-aot-runtime-core'),
  ].join('\n')}\n`);
  await writeFile(resolve(runtime,'src/lib.rs'),`include!(${JSON.stringify(resolve(root,'lang/crates/vo-ui-webview/examples/support/aot-entry.rs'))});\n`);
  await copyFile(resolve(root,'Cargo.lock'),resolve(runtime,'Cargo.lock'));
  await run('aot-resolve','cargo',['update','--workspace','--offline','--manifest-path',resolve(runtime,'Cargo.toml')]);
  const locked=text=>new Set(text.split('[[package]]').filter(block=>/^source = "registry\+/m.test(block))
    .map(block=>['name','version','source','checksum'].map(key=>block.match(new RegExp(`^${key} = "([^"]+)"`,'m'))?.[1]).join('\n')));
  const canonical=locked(await readFile(resolve(root,'Cargo.lock'),'utf8'));
  for(const entry of locked(await readFile(resolve(runtime,'Cargo.lock'),'utf8'))) assert(canonical.has(entry),`changed locked dependency: ${entry}`);
  const output=await run('aot-runtime','cargo',['rustc','--locked','--offline','--manifest-path',resolve(runtime,'Cargo.toml'),'--target-dir',resolve(root,'target'),'--','--print','native-static-libs']);
  const link=output.match(/native-static-libs: ([^\n]+)/)?.[1].trim().split(/\s+/);
  assert(link?.length,'rustc did not report native static link requirements');
  const tree=await run('aot-dependencies','cargo',['tree','--locked','--offline','--manifest-path',resolve(runtime,'Cargo.toml'),'-e','normal']);
  for(const old of ['vo-ui-runtime ','vo-ui-vm ','vo-ui-integration ','vo-codegen ','cranelift-codegen ']) assert(!tree.includes(old),`desktop AOT includes ${old}`);
  for(const [name,source] of [['interaction','ui/next/examples/interaction'],['failure','ui/next/tests/native_failure']]) {
    const object=resolve(directory,`${name}.o`);
    await run(`${name}-aot-object`,compilerPath(),['build',source,'--kind=object','--no-cache','-o',object]);
    await run(`${name}-aot-link`,process.env.CC??'cc',[object,resolve(root,'target/debug/libvo_ui_webview_aot_probe.a'),'-o',resolve(directory,`${name}-aot`),...link]);
  }
}
report.passed=true;await save();
console.log(`Desktop adapter artifacts ready: ${directory}`);
