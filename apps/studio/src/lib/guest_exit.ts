const I32_MIN = -0x80000000;
const I32_MAX = 0x7fffffff;

function normalizeGuestExitCode(value: unknown): number | null {
  return typeof value === 'number'
    && Number.isInteger(value)
    && value >= I32_MIN
    && value <= I32_MAX
    ? value
    : null;
}

/** Extract the exact status attached to a structured WASM guest-exit error. */
export function guestExitCode(error: unknown): number | null {
  if ((typeof error !== 'object' && typeof error !== 'function') || error === null) {
    return null;
  }
  try {
    return normalizeGuestExitCode((error as { exitCode?: unknown }).exitCode);
  } catch {
    return null;
  }
}

export interface GuestExitObservableVm {
  readonly exitCode: number | undefined;
  run(): string;
  runInit(): string;
  runScheduled(): string;
}

const observedRunMethods = new Set<PropertyKey>(['run', 'runInit', 'runScheduled']);

/**
 * Observe terminal status on every VM execution entry while retaining the
 * complete concrete VM surface (including wasm-bindgen methods unknown here).
 */
export function observeGuestExitVm<T extends GuestExitObservableVm>(
  vm: T,
  onGuestExit: ((exitCode: number) => void) | undefined,
): T {
  if (!onGuestExit) {
    return vm;
  }

  let reported = false;
  const reportExit = (error?: unknown): number | null => {
    let vmExitCode: number | null = null;
    try {
      vmExitCode = normalizeGuestExitCode(vm.exitCode);
    } catch {
      // A freed or invalid wasm-bindgen receiver must not hide the original
      // execution failure; its structured error can still carry exitCode.
    }
    const exitCode = vmExitCode ?? guestExitCode(error);
    if (exitCode !== null && !reported) {
      reported = true;
      onGuestExit(exitCode);
    }
    return exitCode;
  };
  const run = (operation: () => string): string => {
    let outcome: string;
    try {
      outcome = operation();
    } catch (error) {
      const exitCode = reportExit(error);
      if (exitCode !== null) {
        // The owner callback invalidates the full GUI session. A stable outcome
        // lets the renderer finish its current stack while teardown is queued.
        return `Exited(${exitCode})`;
      }
      throw error;
    }
    reportExit();
    return outcome;
  };

  return new Proxy(vm, {
    get(target, property) {
      const value = Reflect.get(target, property, target) as unknown;
      if (typeof value !== 'function') {
        return value;
      }
      if (observedRunMethods.has(property)) {
        return (...args: unknown[]) => run(() => Reflect.apply(value, target, args) as string);
      }
      return value.bind(target);
    },
  });
}
