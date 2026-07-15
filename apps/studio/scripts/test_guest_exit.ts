import assert from 'node:assert/strict';

import {
  guestExitCode,
  observeGuestExitVm,
  type GuestExitObservableVm,
} from '../src/lib/guest_exit.ts';

function exitError(exitCode: unknown): Error {
  return Object.assign(new Error(`guest exit ${String(exitCode)}`), { exitCode });
}

for (const [value, expected] of [
  [-0x80000000, -0x80000000],
  [0x7fffffff, 0x7fffffff],
  [-1, -1],
  [0, 0],
  [0x80000000, null],
  [-0x80000001, null],
  [1.5, null],
  [Number.NaN, null],
  [Number.POSITIVE_INFINITY, null],
  ['37', null],
  [37n, null],
  [undefined, null],
] as const) {
  assert.equal(guestExitCode(exitError(value)), expected);
}
assert.equal(guestExitCode(null), null);
assert.equal(guestExitCode('exitCode=37'), null);
assert.equal(guestExitCode(Object.defineProperty({}, 'exitCode', {
  get() {
    throw new Error('hostile getter');
  },
})), null);

const exactExit = -0x80000000;
const reported: number[] = [];
const terminalVm: GuestExitObservableVm = {
  exitCode: undefined,
  run() {
    throw exitError(exactExit);
  },
  runInit() {
    throw exitError(exactExit);
  },
  runScheduled() {
    throw exitError(exactExit);
  },
};
const observedTerminalVm = observeGuestExitVm(terminalVm, (code) => reported.push(code));
assert.equal(observedTerminalVm.run(), `Exited(${exactExit})`);
assert.equal(observedTerminalVm.runInit(), `Exited(${exactExit})`);
assert.equal(observedTerminalVm.runScheduled(), `Exited(${exactExit})`);
assert.deepEqual(reported, [exactExit]);

let successfulExitCode: number | undefined;
const successfulVm: GuestExitObservableVm = {
  get exitCode() {
    return successfulExitCode;
  },
  run() {
    successfulExitCode = 0x7fffffff;
    return 'Exited';
  },
  runInit: () => 'Completed',
  runScheduled: () => 'Completed',
};
const successfulReports: number[] = [];
const observedSuccessfulVm = observeGuestExitVm(successfulVm, (code) => successfulReports.push(code));
assert.equal(observedSuccessfulVm.run(), 'Exited');
assert.deepEqual(successfulReports, [0x7fffffff]);

const ordinaryFailure = new Error('transport failed');
const failingVm: GuestExitObservableVm = {
  exitCode: undefined,
  run() {
    throw ordinaryFailure;
  },
  runInit: () => 'Completed',
  runScheduled: () => 'Completed',
};
const observedFailingVm = observeGuestExitVm(failingVm, () => {
  assert.fail('ordinary failures must not report a guest exit');
});
assert.throws(() => observedFailingVm.run(), (error) => error === ordinaryFailure);

const surfaceIdentity = {};
const surfaceVm = {
  exitCode: undefined,
  run: () => 'Completed',
  runInit: () => 'Completed',
  runScheduled: () => 'Completed',
  get privatePointerProbe() {
    assert.equal(this, surfaceVm);
    return surfaceIdentity;
  },
  callWithPrivatePointerThis() {
    assert.equal(this, surfaceVm);
    return surfaceIdentity;
  },
};
const observedSurfaceVm = observeGuestExitVm(surfaceVm, () => {
  assert.fail('an active VM must not report a guest exit');
});
assert.equal(observedSurfaceVm.privatePointerProbe, surfaceIdentity);
assert.equal(observedSurfaceVm.callWithPrivatePointerThis(), surfaceIdentity);

console.log('studio guest-exit protocol: ok');
