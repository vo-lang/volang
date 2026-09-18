import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {createReadStream} from 'node:fs';
import {readFile,lstat,mkdir,writeFile} from 'node:fs/promises';
import {dirname,join} from 'node:path';
import {verifyToolchain} from './toolchain-inventory.mjs';
import {webArtifactInventory} from './web-artifacts.mjs';
import {guideApplication} from './guide-source.mjs';

const engines = ['chromium','firefox','webkit'];

export function toolchainBuildCoverage(builds) {
  const expected=['canvas','default','document-data','editor','fieldnotes','first-steps',
    'forms','kit-composition','listening','pages','plot','scroll-position','variable-list'];
  assert.deepEqual(Object.keys(builds).sort(),expected.sort(),'portable toolchain build coverage');
}

export function publicHostEvidence(report) {
  assert.equal(report.schema,'volang.ui-public-host.v1');
  assert.equal(report.passed,true);
  for (const key of ['freshPackageStaging','archiveInstalled','existingExportsRetained','strictConsumerTypes','browserBundle','heavyWidgetsExcluded']) {
    assert.equal(report.checks?.[key],true,'public host '+key);
  }
  assert.equal(report.package?.name,'vo-web');
  assert(Number.isSafeInteger(report.package.files) && report.package.files > 0);
  assert.deepEqual(report.package.export,{types:'./dist/ui_next/index.d.ts',import:'./dist/ui_next/index.js'});
  const expected=engines.flatMap(engine=>['vm-minimal','vm-package'].flatMap(runtime=>[false,true].map(hydrate=>`${engine}-${runtime}-${hydrate}`))).sort();
  assert.deepEqual(report.cases.map(value=>`${value.engine}-${value.backend}-${value.runtime}-${value.hydrate}`).sort(),expected);
  for (const value of report.cases) {
    assert(typeof value.browserVersion === 'string' && value.browserVersion.length);
    for (const key of ['passed','independentState','nativeInput','regexp','scopedProviders','closeIsolation','remount','staleCompletion','cleanShutdown']) {
      assert.equal(value[key],true,'public host '+key);
    }
  }
  assert(report.inputs.length > report.package.files);
  assert.equal(new Set(report.inputs.map(value=>value.path)).size,report.inputs.length);
  assert.deepEqual(report.artifacts.map(value=>value.path).sort(),[
    'app.html','app.vob','bundle.json',`vo-web-${report.package.version}.tgz`,
  ].map(path=>'target/ui-next/public-host-check/'+path).sort());
}

export function nativeAuthoringEvidence(report,compilerSha256) {
  for (const key of ['passed','completion','frameworkDefinition','typeDiagnostics','warnings','stdlibSource','unicode','unchangedDisk','cleanShutdown']) {
    assert.equal(report?.[key],true,'native authoring '+key);
  }
  assert.match(compilerSha256 ?? '',/^[a-f0-9]{64}$/,'native authoring compiler identity');
  assert.equal(report.compilerSha256,compilerSha256,'native authoring must test the delivered compiler');
  assert.deepEqual(report.timings?.map(item => item.method),['initialize','textDocument/completion','textDocument/definition','textDocument/definition','volang/source','shutdown']);
  assert(report.timings.every(item => Number.isSafeInteger(item.milliseconds) && item.milliseconds >= 0));
}
const backends = ['vm'];
const applications = ['runtime','keyed-order','interaction','workbench','inspection','styling','benchmark','studio'];
const rawBoundaries = ['domBoundary','nativeMedia','nativeText','dataBlocks','indeterminate','canvas','plot','scrollPosition','textSelection','size','delegatedInputs','pointerBoundary','multipleSelection','reloadBoundary','fileBoundary','customElements','lazyWidgets',
  'widgetCommit','codeEditor','editorLanguage','shadowFocus','measurement','navigation','scrollNavigation','modalBoundary','motionBoundary',
  'workerUi','inspectionPanel','localApplicationCompiler','editorCompiler','popoverBoundary','popoverMotion','keyboardBoundary','mount'];

function editorDiagnosticsEvidence(reports,context) {
  for (const backend of backends) for (const mode of ['client','delayed','failure']) {
    const result=reports.find(item=>item.backend===backend&&item.mode===mode);
    assert.equal(result?.diagnostics,true,context+' '+backend+' '+mode+' diagnostics');
    if (mode!=='delayed') {
      assert.equal(result.previewDiagnostics,true,context+' '+backend+' '+mode+' preview diagnostics');
      assert.equal(result.restoredDraft,true,context+' '+backend+' '+mode+' restored draft diagnostics');
      assert.equal(result.warnings,true,context+' '+backend+' '+mode+' successful and runtime-failure warnings');
    }
  }
}

function studioLanguageEvidence(reports,context) {
  assert.deepEqual(reports.map(value=>value.backend+'-'+value.mode).sort(),backends.flatMap(backend=>['console','ui'].map(mode=>backend+'-'+mode)).sort(),context+' semantic coverage');
  for(const result of reports) {
    for(const key of ['passed','completion','definition','lazy','workersReleased']) assert.equal(result[key],true,context+' '+key);
    if(result.mode==='console')assert.equal(result.retry,true,context+' semantic retry');
  }
}

export function browserEvidence(report,engine,build) {
  assert.equal(report.schema,'volang.ui-next-prototype.v1');assert.equal(report.engine,engine);assert.equal(report.passed,true);
  assert.equal(typeof report.browserVersion,'string');assert(report.browserVersion.length);
  assert.deepEqual(report.build.outputs,build.outputs,'browser tests used a different build');
  assert(Array.isArray(build.webPackages) && build.webPackages.length,'browser runtime package identities are missing');
  assert.deepEqual(report.build.webPackages,build.webPackages,'browser tests used different Web runtime packages');
  for (const name of ['nativeDefaults','collectionContracts']) {
    assert(Array.isArray(report[name]) && report[name].length,engine + ' ' + name);
    assert(report[name].every(item => typeof item === 'string' && item.length),engine + ' ' + name);
  }
  assert(report.viewport && Object.keys(report.viewport).length);
  assert(Object.values(report.viewport).every(value => value === true),engine + ' viewport');
  for (const name of rawBoundaries) assert.equal(report[name]?.passed,true,engine + ' ' + name);
  assert(report.mount.contracts?.includes('shared-runtime-initialization-and-cancellation'),engine+' shared VM initialization');
  assert.equal(report.mount.startup?.passed,true,engine+' startup interactions');
  assert.deepEqual(report.mount.startup.cases.map(value=>value.backend+'-'+value.outcome).sort(),
    ['success','retry','cancel','runtime-load','runtime-init'].map(outcome=>'vm-'+outcome).sort());
  assert(report.mount.startup.cases.every(value=>value.passed===true),engine+' startup cases');
  assert.equal(report.mount.startup.reset?.commit,true,engine+' reset during startup');
  assert.equal(report.mount.startup.reset?.failedCommitRetry,true,engine+' reset and failed startup retry');
  for (const contract of ['source-diagnostics-with-severity-and-code','errors-before-warnings','type-only-import-use','compile-after-invalid-draft','structured-diagnostics-utf16','unsaved-overlay-diagnostics','diagnostic-snapshot-isolation','successful-compile-and-analysis-warnings']) {
    assert(report.localApplicationCompiler.contracts.includes(contract),engine + ' compiler ' + contract);
  }
  assert(report.pointerBoundary.contracts.includes('explicit-latest-pointer-delivery'),engine+' latest pointer delivery');
  assert.equal(report.nativeMedia.properties?.passed,true,engine + ' native media properties');
  assert.equal(report.nativeMedia.properties?.cases,12,engine + ' native media property coverage');
  assert(Array.isArray(report.nativeText.cases) && report.nativeText.cases.length === 2,engine + ' native text coverage');
  assert.deepEqual(report.nativeText.cases.map(value=>value.hydrate).sort(),[false,true]);
  assert(report.nativeText.cases.every(value=>value.passed === true));
  for (const contract of ['incomplete-source-completion','compiler-owned-member-visibility','cross-file-definition','utf16-editor-positions','editor-revision-isolation','same-basename-dependency-diagnostic','stdlib-completion','successful-analysis-warnings']) assert(report.editorCompiler.contracts.includes(contract),engine+' editor compiler '+contract);
  for (const contract of ['explicit-semantic-completion','native-value-after-completion','local-definition-selection','external-definition-panel','cancel-on-edit','cancel-on-selection','cancel-on-blur','cancel-on-disposal','invalid-result-rejected','query-failure-keeps-editor']) assert(report.editorLanguage.contracts.includes(contract),engine+' editor language '+contract);
  assert.match(report.nativeText.fixtureSha256,/^[a-f0-9]{64}$/);
  assert.deepEqual(report.dataBlocks.cases.map(value=>value.hydrate).sort(),[false,true]);
  assert(report.dataBlocks.cases.every(value=>value.passed));
  assert.match(report.dataBlocks.fixtureSha256,/^[a-f0-9]{64}$/);
  for(const contract of ['pre-boot-json','json-character-roundtrip','retained-data-identity','inert-mutation-order','no-script-execution-or-fetch','atomic-invalid-data','special-parser-diagnostics','iframe-document-ownership','hydration-data-validation']) assert(report.dataBlocks.contracts.includes(contract),engine+' data blocks '+contract);
  assert.equal(report.indeterminate.cases,6,engine + ' indeterminate coverage');
  assert.equal(report.canvas.cases,2,engine + ' Canvas coverage');
  assert(report.fileBoundary.contracts?.includes('file-sibling-identity'),engine + ' file field identity');
  assert.match(report.canvas.fixtureSha256,/^[a-f0-9]{64}$/);
  assert.deepEqual(report.size.cases.map(value=>value.hydrate).sort(),[false,true]);
  assert(report.size.cases.every(value=>value.passed));
  for(const contract of ['fractional-border-box','transforms-ignored','logical-writing-mode','hidden-restored','unlisten-pending-size','disposed-pending-size']) assert(report.size.contracts.includes(contract),engine+' size '+contract);
  assert.equal(report.scrollPosition.cases,1,engine + ' scroll position coverage');
  for (const contract of ['ordered-requests','stale-scroll-preserved','guard-checked-before-layout','new-identity-guard-skipped','instant-reload-position']) {
    assert(report.scrollPosition.contracts.includes(contract),engine + ' scroll position missing '+contract);
  }
  for (const contract of ['utf16-source-range','stale-source-preserved','unacknowledged-input-preserved','composition-preserved','invalid-range-atomicity','final-control-type']) {
    assert(report.textSelection.contracts.includes(contract),engine + ' text selection missing ' + contract);
  }
  for (const contract of ['source-selection-projection','stale-source-selection','native-label-dependencies']) {
    assert(report.codeEditor.contracts.includes(contract),engine + ' editor missing ' + contract);
  }
  for (const name of ['backends','inspection','styles','workbench','lazyWidgetApplications','studioEditor','studio']) {
    assert(Array.isArray(report[name]) && report[name].length,engine + ' ' + name);
    assert(report[name].every(item => item.passed === true),engine + ' ' + name);
    assert.deepEqual([...new Set(report[name].map(item => item.backend))].sort(),backends,engine + ' ' + name);
  }
  const adopted=report.backends.filter(value=>value.mode === 'hydrate');
  assert.deepEqual(adopted.map(value=>value.backend).sort(),backends,engine + ' adopted backends');
  assert(adopted.every(value=>value.contracts?.includes('early-mixed-choice')),engine + ' early mixed choice');
  assert.equal(report.keyedOrder?.passed,true,engine+' retained order');
  assert.deepEqual(report.keyedOrder.cases.map(value=>value.backend+'-'+value.hydrate).sort(),backends.flatMap(backend=>[false,true].map(hydrate=>backend+'-'+hydrate)).sort());
  for (const result of report.keyedOrder.cases) for (const contract of ['passed','fragmentRanges','state','focus','selection','mixedMembership','disposal']) {
    assert.equal(result[contract],true,engine+' retained order '+contract);
  }
  editorDiagnosticsEvidence(report.studioEditor,engine+' Studio');
  studioLanguageEvidence(report.studioLanguage,engine+' Studio');
}

async function artifact(directory,value) {
  const path = value.path;
  assert(typeof path === 'string' && !path.includes('\\') && path.split('/').every(part => part && part !== '.' && part !== '..') && !path.includes(':'),'invalid artifact path');
  assert(Number.isSafeInteger(value.bytes) && value.bytes >= 0 && /^[a-f0-9]{64}$/.test(value.sha256),'invalid artifact identity');
  const file = join(directory,path),metadata = await lstat(file);
  assert(metadata.isFile());assert.equal(metadata.size,value.bytes,path);
  const hash = createHash('sha256');for await (const bytes of createReadStream(file)) hash.update(bytes);
  assert.equal(hash.digest('hex'),value.sha256,path);
}

export function staticStudioEvidence(report,build,sourceBytes) {
  assert.equal(report.passed,true);assert.equal(report.relocated,true);
  assert.equal(build.schema,'volang.studio-next-static.v1');
  assert.deepEqual(report.build,build,'static Studio tests used another export');
  assert.equal(build.sourceBuildSha256,createHash('sha256').update(sourceBytes).digest('hex'),'static Studio used another native build');
  assert.deepEqual(report.results.map(value=>value.engine).sort(),engines);
  for (const result of report.results) {
    for (const name of ['passed','rootRedirect','refresh','missingPage','noScript']) assert.equal(result[name],true,result.engine+' '+name);
    assert.equal(typeof result.browserVersion,'string');assert(result.browserVersion.length);
    for (const name of ['studio','editor']) {
      assert(result[name].every(value=>value.passed === true));
      assert.deepEqual([...new Set(result[name].map(value=>value.backend))].sort(),backends,result.engine+' static '+name);
    }
    editorDiagnosticsEvidence(result.editor,result.engine+' static Studio');
    studioLanguageEvidence(result.language,result.engine+' static Studio');
  }
}

export async function collectCoreEvidence(root) {
  const output = join(root,'target/ui-next');
  const read = async path => JSON.parse(await readFile(join(output,path)));
  const build = await read('build-report.json');assert.equal(build.nativeContracts,true);
  assert.deepEqual(build.webPackages,await webArtifactInventory(root),'Web runtime packages changed after the build');
  const expected = [...applications.map(name => `target/ui-next/${name}.vob`),'target/ui-next/playground-ui.json'].sort();
  assert.deepEqual(build.outputs.map(value => value.path).sort(),expected);
  for (const value of build.outputs) await artifact(root,value);
  const reports = [],versions = {};
  async function add(path) {
    const bytes = await readFile(join(output,path));
    const archived='target/ci/artifacts/ui-web-rewrite/reports/' + path;
    await mkdir(dirname(join(root,archived)),{recursive:true});
    await writeFile(join(root,archived),bytes);
    reports.push({path:archived,bytes:bytes.length,sha256:createHash('sha256').update(bytes).digest('hex')});
  }
  await add('build-report.json');
  for (const engine of engines) {
    const path = engine === 'chromium' ? 'browser-report.json' : engine + '/browser-report.json';
    const report = await read(path);browserEvidence(report,engine,build);
    versions[engine]=report.browserVersion;await add(path);
  }
  const toolkit = await read('toolchain-project/report.json');
  assert(toolkit.passed && toolkit.publicTestingCases === 60 && toolkit.development.passed && toolkit.development.cleanShutdown);
  assert.deepEqual(Object.keys(toolkit.deliveredExamples).sort(),['canvas','plot','scroll-position','variable-list']);
  for(const [name,sources]of Object.entries(toolkit.deliveredExamples)) {
    assert.deepEqual(Object.keys(sources).sort(),['app.css','app.vo','tests/browser/app.test.mjs']);
    for(const [path,digest]of Object.entries(sources)) assert.equal(digest,createHash('sha256').update(await readFile(join(root,'ui/next/examples',name,path))).digest('hex'));
  }
  assert(toolkit.contracts.includes('complete-delivered-example-applications'));
  assert(toolkit.firstSteps.passed && toolkit.firstSteps.deliveredGuide && toolkit.firstSteps.publicCases === 3);
  assert.equal(toolkit.firstSteps.sourceSha256,createHash('sha256').update(guideApplication(await readFile(join(root,'ui/next/guides/first-steps.md'),'utf8'))).digest('hex'));
  assert(toolkit.forms.passed && toolkit.forms.deliveredGuide && toolkit.forms.publicCases === 6);
  assert.equal(toolkit.forms.sourceSha256,createHash('sha256').update(guideApplication(await readFile(join(root,'ui/next/guides/forms.md'),'utf8'))).digest('hex'));
  assert.equal(toolkit.kitComposition.publicCases,6);
  for(const key of ['passed','nativeModifiers','retainedItems','customContent']) assert(toolkit.kitComposition[key],'kit composition '+key);
  assert.equal(toolkit.kitComposition.sourceSha256,createHash('sha256').update(await readFile(join(root,'eng/ui-next/fixtures/kit-composition-app.vo'))).digest('hex'));
  assert.equal(toolkit.documentData.publicCases,9);assert.equal(toolkit.documentData.rawCases,6);
  for(const key of ['passed','sourceReload','savedRetained','newMetadataApplied']) assert(toolkit.documentData[key],'document data '+key);
  assert.equal(toolkit.documentData.sourceSha256,createHash('sha256').update(await readFile(join(root,'ui/next/templates/pages/notes/app/app.vo'))).digest('hex'));
  assert.equal(toolkit.documentData.rawSourceSha256,createHash('sha256').update(await readFile(join(root,'eng/ui-next/fixtures/data-block-app.vo'))).digest('hex'));
  assert.equal(toolkit.plot.publicCases,6);
  for(const key of ['passed','sourceReload','stateRetained','oldCanvasReleased','stylesReleased']) assert(toolkit.plot[key],'plot '+key);
  assert.equal(toolkit.plot.sourceSha256,createHash('sha256').update(await readFile(join(root,'ui/next/examples/plot/app.vo'))).digest('hex'));
  assert(toolkit.canvas.passed && toolkit.canvas.publicCases === 6 && toolkit.canvas.sourceReload && toolkit.canvas.paletteRetained && toolkit.canvas.backingReleased);
  assert.equal(toolkit.canvas.sourceSha256,createHash('sha256').update(await readFile(join(root,'ui/next/examples/canvas/app.vo'))).digest('hex'));
  assert(toolkit.scrollPosition.passed && toolkit.scrollPosition.publicCases === 6 && toolkit.scrollPosition.sourceReload && toolkit.scrollPosition.positionRetained && toolkit.scrollPosition.newRequest);
  assert.equal(toolkit.variableList.publicCases,9);
  for(const key of ['passed','sourceReload','keyRetained','insetRetained','widthChanged','focusRetained','newMeasurements']) assert(toolkit.variableList[key],'variable list '+key);
  assert.equal(toolkit.variableList.sourceSha256,createHash('sha256').update(await readFile(join(root,'ui/next/examples/variable-list/app.vo'))).digest('hex'));
  assert.equal(toolkit.scrollPosition.sourceSha256,createHash('sha256').update(await readFile(join(root,'ui/next/examples/scroll-position/app.vo'))).digest('hex'));
  assert(toolkit.contracts.includes('compiler-cache-does-not-reload-page'));
  for (const contract of ['direct-project-commands','subcommand-help','configured-production-backend',
    'manifest-project-selection','current-directory-project-commands','positional-project-commands']) {
    assert(toolkit.contracts.includes(contract),'portable toolkit is missing '+contract);
  }
  assert.deepEqual(toolkit.editorCases.map(value => `${value.engine}-${value.backend}`).sort(),engines.flatMap(engine => backends.map(backend => `${engine}-${backend}`)).sort());
  assert(toolkit.editorCases.every(value => value.editor === true));
  toolchainBuildCoverage(toolkit.builds);
  for (const [name,app] of Object.entries(toolkit.builds)) for (const value of app.artifacts) await artifact(join(output,'toolchain-project',name),value);
  await add('toolchain-project/report.json');await add('toolchain-project/toolchain-manifest.json');
  const packaged = await read('ci/toolchain/tools/toolchain.json'),tested = await read('toolchain-project/toolchain-manifest.json');
  nativeAuthoringEvidence(toolkit.nativeAuthoring,tested.artifacts.find(file => file.path === tested.paths.compiler)?.sha256);
  for(const key of ['passed','readOnly','noGuestExecution','hostImports','desktopEntry','diagnosis','failureDiagnostics']) {
    assert.equal(toolkit.projectDiagnosis?.[key],true,'project diagnosis '+key);
  }
  assert(tested.artifacts.some(file => file.path === 'editors/volang-ui-authoring.vsix'),'portable toolchain must deliver its editor extension');
  assert.deepEqual(packaged.artifacts,tested.artifacts,'CI package differs from the tested toolchain');
  await verifyToolchain(join(output,'ci/toolchain'));await add('ci/toolchain/tools/toolchain.json');
  const studio = await read('studio-distribution/build-report.json'),assets = await read('asset-delivery-report.json');
  assert.equal(assets.passed,true);assert.deepEqual(assets.build.artifacts,studio.artifacts);
  assert.deepEqual(assets.combinations.map(value => `${value.engine}-${value.backend}`).sort(),engines.flatMap(engine => backends.map(backend => `${engine}-${backend}`)).sort());
  assert(assets.combinations.every(value => value.uiSchemaWorker && value.uiFileWorker));
  for (const value of studio.artifacts) await artifact(join(output,'studio-distribution'),value);
  for (const value of build.webPackages) {
    const delivered=studio.artifacts.find(item => item.path === value.deliveryPath);
    assert(delivered,'Studio runtime package is missing: ' + value.deliveryPath);
    assert.equal(delivered.bytes,value.bytes,value.deliveryPath);
    assert.equal(delivered.sha256,value.sha256,'Studio differs from the browser-tested runtime: ' + value.deliveryPath);
  }
  await add('studio-distribution/build-report.json');await add('asset-delivery-report.json');
  const staticBuild=await read('studio-static/build-report.json'),staticReport=await read('studio-static-check/report.json');
  const staticOwnership=await read('studio-static-build-report.json');
  assert.deepEqual(staticOwnership.build,staticBuild,'static export ownership tests used another build');
  for (const name of ['passed','repeatable','cancelledAfterNativeRender','lastSitePreserved','stagingRemoved']) assert.equal(staticOwnership[name],true,'static export '+name);
  staticStudioEvidence(staticReport,staticBuild,await readFile(join(output,'studio-distribution/build-report.json')));
  for (const value of staticBuild.artifacts) await artifact(join(output,'studio-static'),value);
  await add('studio-static/build-report.json');await add('studio-static-check/report.json');await add('studio-static-build-report.json');
  const commands = await read('ci/commands.json');
  const pointerDevelopment = await read('pointer-development-check/report.json');
  for (const name of ['passed','widthRetained','dragReset','newDrag']) assert.equal(pointerDevelopment[name],true,'pointer development '+name);
  assert.equal(pointerDevelopment.exampleSha256,createHash('sha256').update(await readFile(join(root,'ui/next/examples/interaction/pointer.vo'))).digest('hex'),'pointer reload tested another example');
  await add('pointer-development-check/report.json');
  const publicHost=await read('public-host-check/report.json');
  publicHostEvidence(publicHost);
  for (const value of publicHost.inputs) await artifact(root,value);
  for (const value of publicHost.artifacts) {
    await artifact(root,value);
    await add(value.path.slice('target/ui-next/'.length));
  }
  await add('public-host-check/report.json');
  assert(commands.passed && commands.nativeJit && commands.unitTests > 0 && commands.unitPassed === commands.unitTests);
  await add('ci/commands.json');
  return {schema:'volang.browser-result.v1',passed:true,report:{passed:true,complete:true,
    scope:'experimental-web-ui-core',browserVersions:versions,checks:['native-vm','native-jit','wasm-vm',
      'core-node-contracts','three-engine-ui-matrices','portable-native-cli','request-time-server-template','optional-editor',
      'standalone-cache-and-reload','pointer-development-reset','public-host-package-and-isolation','studio-compressed-delivery','studio-static-delivery','offline-schema-and-file-worker','browser-runtime-package-identity','artifact-identity'],
    evidence:reports,toolchain:toolkit.toolchain,formalPerformanceBenchmark:false,productCertification:false}};
}
