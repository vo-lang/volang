import assert from 'node:assert/strict';
import test from 'node:test';
import {desktopEvidence,desktopDeliveryChecks} from './desktop-ci-report.mjs';

const checks=['gallery-state','client-navigation','offline-documents','optional-editor','saved-draft','compiler-worker','compile-error-recovery','worker-cancellation','ui-preview','preview-interaction','worker-disposal','editor-disposal'];
function fixture(platform='darwin') {
  const identity={platform,arch:'arm64',profile:'dev'};
  const base={...identity,passed:true,physicalInputVerified:false,paintVerified:false};
  const delivery={...base,schema:'volang.ui-desktop-delivery.v1',projectDiagnosis:{passed:true,readOnly:true,noGuestExecution:true,hostImports:true,desktopEntry:true,diagnosis:true,failureDiagnostics:true,desktop:true},cases:desktopDeliveryChecks(platform).map(name=>({name,passed:true,
    exit:/^(corrupt-|window-failure-|failed-build-|reject-)|^window-document-unload$/.test(name)?1:0}))};
  const studio={...base,schema:'volang.studio-desktop-contracts.v1',cases:['vm','jit','aot'].map(backend=>({backend,profile:'dev',passed:true,contracts:[...checks]}))};
  return [delivery,studio,identity];
}
test('desktop evidence distinguishes system WebView assertions from physical acceptance',()=>{
  for(const platform of ['darwin','linux','win32']) {
    const result=desktopEvidence(...fixture(platform));
    assert.equal(result.report.checks.length,platform==='darwin'?88:85);
    assert.equal(result.report.physicalInputVerified,false);
    assert.equal(result.report.paintVerified,false);
  }
});
test('partial, repeated, failed and mixed-platform desktop evidence fails closed',()=>{
  for(const mutate of [
    ([delivery])=>delete delivery.projectDiagnosis,
    ([delivery])=>delivery.cases.pop(),
    ([delivery])=>delivery.cases.push(delivery.cases[0]),
    ([delivery])=>delivery.cases[0].exit=1,
    ([delivery])=>delivery.cases.find(item=>item.name==='corrupt-aot').exit=0,
    ([delivery])=>delivery.cases[0].passed=false,
    ([,studio])=>studio.cases.pop(),
    ([,studio])=>studio.cases[2].backend='jit',
    ([,studio])=>studio.cases[0].contracts.pop(),
    ([,studio])=>studio.platform='linux',
    ([,studio])=>studio.cases[1].profile='release-native',
  ]) {const data=fixture();mutate(data);assert.throws(()=>desktopEvidence(...data));}
});
