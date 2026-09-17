import assert from 'node:assert/strict';
import test from 'node:test';
import {createHash} from 'node:crypto';
import {browserEvidence,staticStudioEvidence,studioUpgradeEvidence,nativeAuthoringEvidence,publicHostEvidence} from './ci-report.mjs';

test('public host evidence requires installed package, both VM runtimes and complete root lifetimes', () => {
  const report={schema:'volang.ui-public-host.v1',passed:true,
    package:{name:'vo-web',version:'0.1.4',files:1,export:{types:'./dist/ui_next/index.d.ts',import:'./dist/ui_next/index.js'}},
    checks:Object.fromEntries(['freshPackageStaging','archiveInstalled','existingExportsRetained','strictConsumerTypes','browserBundle','legacyUiExcluded','heavyWidgetsExcluded'].map(key=>[key,true])),
    cases:['chromium','firefox','webkit'].flatMap(engine=>[['vm','minimal'],['vm','package']].flatMap(([backend,runtime])=>[false,true].map(hydrate=>({
      engine,backend,runtime,hydrate,browserVersion:'tested-browser',passed:true,independentState:true,nativeInput:true,regexp:true,
      scopedProviders:true,closeIsolation:true,remount:true,staleCompletion:true,cleanShutdown:true,
    })))),
    inputs:[{path:'package.json'},{path:'fixture.vo'}],
    artifacts:['app.html','app.vob','bundle.json','vo-web-0.1.4.tgz'].map(path=>({path:'target/ui-next/public-host-check/'+path})),
  };
  publicHostEvidence(report);
  for (const change of [
    value=>{value.passed=false;}, value=>{value.cases.pop();}, value=>{value.cases[0].runtime='package';},
    value=>{value.cases[0].browserVersion='';}, value=>{value.inputs.pop();}, value=>{value.inputs[1]=value.inputs[0];},
    value=>{value.artifacts.pop();}, value=>{value.package.export.import='./dist/ui_dom.js';},
    ...Object.keys(report.checks).map(key=>value=>{value.checks[key]=false;}),
    ...['passed','independentState','nativeInput','regexp','scopedProviders','closeIsolation','remount','staleCompletion','cleanShutdown'].map(key=>value=>{value.cases[0][key]=false;}),
  ]) {
    const incomplete=structuredClone(report);change(incomplete);
    assert.throws(()=>publicHostEvidence(incomplete));
  }
});

test('native authoring evidence requires complete behavior and the delivered compiler', () => {
  const keys = ['passed','completion','frameworkDefinition','typeDiagnostics','warnings','stdlibSource','unicode','unchangedDisk','cleanShutdown'];
  const digest = 'a'.repeat(64);
  const report = {...Object.fromEntries(keys.map(key => [key,true])),compilerSha256:digest,
    timings:['initialize','textDocument/completion','textDocument/definition','textDocument/definition','volang/source','shutdown'].map(method => ({method,milliseconds:1}))};
  nativeAuthoringEvidence(report,digest);
  for (const key of keys) assert.throws(() => nativeAuthoringEvidence({...report,[key]:false},digest));
  assert.throws(() => nativeAuthoringEvidence(report,'b'.repeat(64)),/delivered compiler/);
  assert.throws(() => nativeAuthoringEvidence({...report,timings:[]},digest));
});

const editorResults=()=>['vm'].flatMap(backend=>['client','delayed','failure','close'].map(mode=>({
  backend,mode,passed:true,diagnostics:mode!=='close',previewDiagnostics:mode==='client'||mode==='failure',
  restoredDraft:mode==='client'||mode==='failure',
  warnings:mode==='client'||mode==='failure',
})));

const languageResults=()=>['vm'].flatMap(backend=>['console','ui'].map(mode=>({
  backend,mode,passed:true,completion:true,definition:true,lazy:true,workersReleased:true,retry:mode==='console',
})));

function complete() {
  const report = {schema:'volang.ui-next-prototype.v1',engine:'chromium',passed:true,browserVersion:'test-browser',
    build:{outputs:[{path:'runtime.vob',sha256:'tested-image'}],webPackages:[{path:'compiler.wasm',sha256:'tested-compiler'}]},nativeDefaults:['native-defaults'],
    collectionContracts:['virtual-list-unmount'],viewport:{initial:true,close:true}};
  for (const name of ['domBoundary','nativeMedia','nativeText','dataBlocks','indeterminate','canvas','plot','scrollPosition','textSelection','size','delegatedInputs','pointerBoundary','multipleSelection','reloadBoundary','fileBoundary','customElements','lazyWidgets',
    'widgetCommit','codeEditor','editorLanguage','shadowFocus','measurement','navigation','scrollNavigation','modalBoundary','motionBoundary',
    'workerUi','inspectionPanel','localApplicationCompiler','editorCompiler','popoverBoundary','popoverMotion','keyboardBoundary','mount']) report[name]={passed:true};
  for (const name of ['backends','inspection','styles','workbench','lazyWidgetApplications','studioEditor','studioRecovery','studio']) {
    report[name]=['vm'].map(backend => ({backend,passed:true}));
  }
  report.pointerBoundary.contracts=['explicit-latest-pointer-delivery'];
  report.nativeMedia.properties={passed:true,cases:12};
  report.mount.contracts=['shared-runtime-initialization-and-cancellation'];
  report.mount.startup={passed:true,reset:{commit:true,failedCommitRetry:true},cases:['success','retry','cancel','runtime-load','runtime-init'].map(outcome=>({backend:'vm',outcome,passed:true}))};
  report.localApplicationCompiler.contracts=['source-diagnostics-with-severity-and-code','errors-before-warnings','type-only-import-use','compile-after-invalid-draft','structured-diagnostics-utf16','unsaved-overlay-diagnostics','diagnostic-snapshot-isolation','successful-compile-and-analysis-warnings'];
  report.editorCompiler.contracts=['incomplete-source-completion','compiler-owned-member-visibility','cross-file-definition','utf16-editor-positions','editor-revision-isolation','same-basename-dependency-diagnostic','stdlib-completion','successful-analysis-warnings'];
  report.editorLanguage.contracts=['explicit-semantic-completion','native-value-after-completion','local-definition-selection','external-definition-panel','cancel-on-edit','cancel-on-selection','cancel-on-blur','cancel-on-disposal','invalid-result-rejected','query-failure-keeps-editor'];
  report.studioLanguage=languageResults();
  report.keyedOrder={passed:true,cases:['vm'].flatMap(backend=>[false,true].map(hydrate=>({backend,hydrate,
    passed:true,fragmentRanges:true,state:true,focus:true,selection:true,mixedMembership:true,disposal:true}))) };
  report.nativeText={passed:true,cases:[false,true].map(hydrate=>({hydrate,passed:true})),fixtureSha256:'a'.repeat(64)};
  report.dataBlocks={passed:true,cases:[false,true].map(hydrate=>({hydrate,passed:true})),fixtureSha256:'d'.repeat(64),contracts:['pre-boot-json','json-character-roundtrip','retained-data-identity','inert-mutation-order','no-script-execution-or-fetch','atomic-invalid-data','special-parser-diagnostics','iframe-document-ownership','hydration-data-validation']};
  report.size={passed:true,cases:[{hydrate:false,passed:true},{hydrate:true,passed:true}],contracts:['fractional-border-box','transforms-ignored','logical-writing-mode','hidden-restored','unlisten-pending-size','disposed-pending-size']};
  report.indeterminate.cases=6;
  report.canvas={passed:true,cases:2,fixtureSha256:'c'.repeat(64)};
  report.fileBoundary.contracts=['file-sibling-identity'];
  report.scrollPosition={passed:true,cases:1,contracts:['ordered-requests','stale-scroll-preserved','guard-checked-before-layout','new-identity-guard-skipped','instant-reload-position']};
  report.textSelection={passed:true,contracts:['utf16-source-range','stale-source-preserved','unacknowledged-input-preserved','composition-preserved','invalid-range-atomicity','final-control-type']};
  report.codeEditor.contracts=['source-selection-projection','stale-source-selection','native-label-dependencies'];
  report.studioEditor=editorResults();
  report.backends.push(...['vm'].map(backend=>({backend,mode:'hydrate',passed:true,contracts:['early-mixed-choice']})));
  return report;
}

test('browser evidence requires actual build identity and every engine boundary', () => {
  const report=complete();browserEvidence(report,'chromium',report.build);
  assert.throws(() => browserEvidence(report,'firefox',report.build));
  assert.throws(() => browserEvidence(report,'chromium',{outputs:[]}));
  const different=structuredClone(report.build);different.webPackages[0].sha256='other-compiler';
  assert.throws(() => browserEvidence(report,'chromium',different),/Web runtime packages/);
  const missingPackages=structuredClone(report);delete missingPackages.build.webPackages;
  assert.throws(() => browserEvidence(missingPackages,'chromium',missingPackages.build),/package identities/);
  for (const key of Object.keys(report)) {
    const missing=structuredClone(report);delete missing[key];
    assert.throws(() => browserEvidence(missing,'chromium',report.build),key);
  }
});

test('partial backend coverage and failed nested checks cannot become a passing receipt', () => {
  for (const alter of [
    report => {report.studioEditor=report.studioEditor.filter(value=>value.mode!=='failure');},
    report => {report.studioEditor[0].diagnostics=false;},
    report => {report.studioEditor[0].previewDiagnostics=false;},
    report => {report.studioEditor[0].restoredDraft=false;},
    report => {report.studioEditor[0].warnings=false;},
    report => {report.studioRecovery[0].passed=false;},
    report => {report.fileBoundary.passed=false;},
    report => {report.mount.contracts=[];},
    report => {delete report.mount.startup;},
    report => {report.mount.startup.passed=false;},
    report => {report.mount.startup.cases.pop();},
    report => {report.mount.startup.cases[0].passed=false;},
    report => {delete report.mount.startup.reset;},
    report => {report.mount.startup.reset.commit=false;},
    report => {report.mount.startup.reset.failedCommitRetry=false;},
    report => {report.localApplicationCompiler.contracts.pop();},
    report => {report.editorCompiler.contracts.pop();},
    report => {report.editorLanguage.contracts.pop();},
    report => {report.studioLanguage.pop();},
    report => {report.keyedOrder.cases.pop();},
    report => {report.keyedOrder.cases[0].selection=false;},
    report => {report.studioLanguage[0].definition=false;},
    report => {report.viewport.close=false;},
    report => {report.nativeDefaults=[];},
    report => {delete report.nativeMedia.properties;},
    report => {report.nativeMedia.properties.passed=false;},
    report => {report.nativeMedia.properties.cases=11;},
    report => {report.nativeText.cases.pop();},
    report => {report.nativeText.cases[0].passed=false;},
    report => {report.nativeText.fixtureSha256='different';},
    report => {report.dataBlocks.cases.pop();},
    report => {report.dataBlocks.cases[0].passed=false;},
    report => {report.dataBlocks.contracts.pop();},
    report => {report.dataBlocks.fixtureSha256='invalid';},
    report => {report.indeterminate.cases=5;},
    report => {report.canvas.cases=1;},
    report => {report.canvas.fixtureSha256='invalid';},
    report => {report.scrollPosition.contracts.pop();},
    report => {report.textSelection.contracts.pop();},
    report => {report.codeEditor.contracts.pop();},
    report => {report.size.contracts.pop();},
    report => {report.backends=report.backends.filter(value=>value.mode !== 'hydrate');},
    report => {report.backends.find(value=>value.mode === 'hydrate').contracts=[];},
    report => {report.collectionContracts=[false];},
  ]) {
    const report=complete();alter(report);
    assert.throws(() => browserEvidence(report,'chromium',report.build));
  }
});

test('static Studio evidence binds its native producer and every delivered backend',()=>{
  const source=Buffer.from('native distribution');
  const build={schema:'volang.studio-next-static.v1',sourceBuildSha256:createHash('sha256').update(source).digest('hex')};
  const report={passed:true,relocated:true,build,results:['chromium','firefox','webkit'].map(engine=>({
    engine,browserVersion:'test',passed:true,rootRedirect:true,refresh:true,missingPage:true,noScript:true,
    ...Object.fromEntries(['studio','recovery'].map(name=>[name,['vm'].map(backend=>({backend,passed:true}))])),editor:editorResults(),language:languageResults(),
  }))};
  staticStudioEvidence(report,build,source);
  assert.throws(()=>staticStudioEvidence(report,build,Buffer.from('another native build')),/native build/);
  for(const alter of [value=>value.results.pop(),value=>value.results[0].editor.shift(),value=>value.results[0].language[0].workersReleased=false,
    value=>value.results[0].recovery[0].passed=false,value=>value.results[0].noScript=false,
    value=>value.build.sourceBuildSha256='different',value=>value.relocated=false]) {
    const changed=structuredClone(report);alter(changed);
    assert.throws(()=>staticStudioEvidence(changed,build,source));
  }
});

test('Studio upgrade evidence requires data preservation, all engines and both retries',()=>{
  const fixture=Buffer.from('previous worker'),build={schema:'volang.studio-next-static.v1',artifacts:[]};
  const report={passed:true,nativeRedirects:true,readOnlyMethods:true,build,
    fixtureSha256:createHash('sha256').update(fixture).digest('hex'),
    results:['chromium','firefox','webkit'].map(engine=>({engine,browserVersion:'test',passed:true,
      workerRetired:true,otherRegistrationRetained:true,assetCacheRetired:true,otherCacheRetained:true,
      openDraftRetained:true,projectFilesRetained:true,oldLinks:true,noScriptLegacy:true,unknownTopic:true,
      retries:['vm'].map(backend=>({backend,passed:true})),
    }))};
  studioUpgradeEvidence(report,build,fixture);
  assert.throws(()=>studioUpgradeEvidence(report,build,Buffer.from('different worker')),/legacy worker/);
  for (const alter of [value=>value.results.pop(),value=>value.results[0].retries.pop(),
    value=>value.results[0].retries[0].passed=false,value=>value.results[0].projectFilesRetained=false,
    value=>value.results[0].otherCacheRetained=false,value=>value.nativeRedirects=false,
    value=>value.build.artifacts.push('different')]) {
    const changed=structuredClone(report);alter(changed);
    assert.throws(()=>studioUpgradeEvidence(changed,build,fixture));
  }
});
