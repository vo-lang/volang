import assert from 'node:assert/strict';
import {copyFile,mkdir,readFile,writeFile} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {root} from './repository-paths.mjs';
import {desktopArtifact,readDesktopSdk} from './desktop-sdk-manifest.mjs';

const backends=['vm','jit','aot'];
const studioChecks=['gallery-state','client-navigation','offline-documents','optional-editor','saved-draft','compiler-worker','compile-error-recovery','worker-cancellation','ui-preview','preview-interaction','worker-disposal','editor-disposal'];
export function desktopDeliveryChecks(platform) {
  assert(['darwin','linux','win32'].includes(platform));
  return ['shell-preview-build','shell-bytecode','window-shell-vm','window-shell-jit','verify-moved-toolchain','create',
    ...backends.flatMap(backend=>['package','verify','window',...(platform==='darwin'?['plist']:[]),'corrupt'].map(name=>`${name}-${backend}`)),
    'public-run-default-jit','public-run-current-directory-vm','failed-build-preserves-previous','reject-unknown-backend',
    ...['seed','reopen','isolate'].flatMap(phase=>[`package-storage-${phase}`,`window-storage-${phase}`]),
    'package-document-unload','window-document-unload',
    ...['canvas','plot','listening','variable-list'].flatMap(template=>['create','package','window'].map(name=>`${name}-${template}`)),
    'create-failure',...backends.flatMap(backend=>['package-failure','window-failure'].map(name=>`${name}-${backend}`))];
}

export function desktopEvidence(delivery,studio,{platform,arch,profile}) {
  for(const key of ['passed','readOnly','noGuestExecution','hostImports','desktopEntry','diagnosis','failureDiagnostics','desktop']) {
    assert.equal(delivery.projectDiagnosis?.[key],true,'desktop project diagnosis '+key);
  }
  for(const report of [delivery,studio]) {
    assert.equal(report.passed,true,'desktop scenario failed');
    assert.equal(report.platform,platform,'desktop platform mismatch');
    assert.equal(report.arch,arch,'desktop architecture mismatch');
    assert.equal(report.physicalInputVerified,false);
    assert.equal(report.paintVerified,false);
  }
  assert.equal(delivery.schema,'volang.ui-desktop-delivery.v1');
  assert.equal(studio.schema,'volang.studio-desktop-contracts.v1');
  assert.equal(delivery.profile,profile);
  const expected=desktopDeliveryChecks(platform);
  assert.deepEqual(delivery.cases.map(item=>item.name).sort(),expected.toSorted(),'complete desktop delivery coverage');
  for(const item of delivery.cases) {
    assert.equal(item.passed,true,item.name);
    const fails=/^(corrupt-|window-failure-|failed-build-|reject-)|^window-document-unload$/.test(item.name);
    assert(Number.isInteger(item.exit) && (fails ? item.exit!==0 : item.exit===0),item.name+' exit status');
  }
  assert.deepEqual(studio.cases.map(item=>item.backend).sort(),backends.toSorted(),'complete Studio backend coverage');
  for(const item of studio.cases) {
    assert.equal(item.passed,true,item.backend);
    assert.equal(item.profile,profile);
    assert.deepEqual(item.contracts.toSorted(),studioChecks.toSorted(),'complete Studio interactions');
  }
  return {schema:'volang.browser-result.v1',passed:true,platform,arch,profile,
    report:{passed:true,complete:true,acceptance:'packaged-native-execution-and-system-webview-dom',
      physicalInputVerified:false,paintVerified:false,
      checks:[...expected,...backends.flatMap(backend=>studioChecks.map(name=>`studio-${backend}-${name}`))]}};
}

export async function collectDesktopEvidence() {
  const {compilerPath,toolchain}=await import('./toolchain.mjs');
  const sdkDirectory=toolchain.desktop;
  const sdk=await readDesktopSdk(sdkDirectory);
  assert(sdk.runtime,'desktop CI requires Native AOT');
  const deliveryPath='target/ui-next/desktop-delivery/report.json',studioPath='target/ui-next/studio-desktop/report.json';
  const delivery=JSON.parse(await readFile(join(root,deliveryPath),'utf8'));
  const studio=JSON.parse(await readFile(join(root,studioPath),'utf8'));
  const result=desktopEvidence(delivery,studio,sdk);
  for(const backend of backends) {
    const item=delivery.cases.find(row=>row.name===`window-${backend}`);
    assert(item.artifact,backend+' delivered executable identity');
    assert.deepEqual(await desktopArtifact(join(delivery.directory,`Moved application ${backend} 中文`),item.artifact.path),item.artifact);
    const application=studio.cases.find(row=>row.backend===backend);
    for(const resource of [application.artifact,application.bundleReceipt]) {
      assert(resource,backend+' Studio identity');
      assert.deepEqual(await desktopArtifact(join(root,'target/ui-next/studio-desktop',`check-${backend}`),resource.path),resource);
    }
  }
  const {basename,dirname}=await import('node:path');
  result.compiler=await desktopArtifact(dirname(compilerPath()),basename(compilerPath()));
  result.sdk=await desktopArtifact(sdkDirectory,'desktop-sdk.json');
  const artifacts=join(root,'target/ci/artifacts/ui-desktop-rewrite');
  await mkdir(artifacts,{recursive:true});
  for(const [path,name] of [[deliveryPath,'delivery.json'],[studioPath,'studio.json']]) await copyFile(join(root,path),join(artifacts,name));
  result.reports=await Promise.all(['delivery.json','studio.json'].map(name=>desktopArtifact(artifacts,name)));
  const path=join(root,'target/ci/results/ui-desktop-rewrite.json');
  await mkdir(join(root,'target/ci/results'),{recursive:true});
  await writeFile(path,JSON.stringify(result,null,2)+'\n');
  return path;
}

if(process.argv[1]&&import.meta.url===pathToFileURL(resolve(process.argv[1])).href) {
  console.log(await collectDesktopEvidence());
}
