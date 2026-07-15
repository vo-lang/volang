#!/usr/bin/env node

import assert from 'node:assert/strict';
import {
  collectQuickplayLocalJavaScriptGraph,
  parseQuickplayJavaScriptModule,
  QUICKPLAY_RENDERER_LIMITS,
  QuickplayRendererContractError,
  validateHostWaitKeyDeclarations,
  validateVoplayHostWaitKeyContract,
  validateVoplayRendererContract,
} from './quickplay_renderer_contract.mjs';

const BRIDGE_PATH = 'js/dist/render_bootstrap.js';
const DECLARATIONS = {
  path: 'js/dist/render_bootstrap.d.ts',
  source: `
    export interface VoVm {
      takePendingHostEvents(): Array<{
        /* HostWaitKey survives comments between declaration tokens. */
        key /* stable wake identity */ : string;
        delayMs: number;
      }>;
      wakeHostEvent(key: string): void;
    }
  `,
};

const VALID_BRIDGE = `
  export class RenderIsland {
    constructor() {
      this.hostTimers = new Map();
      this.displayPulseWaiters = new Map();
    }

    scheduleHostEvents() {
      const events = this.vm.takePendingHostEvents();
      for (const ev of events) {
        if (this.hostTimers.has(ev.key)) continue;
        if (ev.delayMs === DISPLAY_PULSE_DELAY_MS) {
          this.displayPulseWaiters.set(ev.key, { afterSerial: 1 });
          this.hostTimers.set(ev.key, { kind: "displayPulse" });
        } else {
          const id = window.setTimeout(() => this.wakeHostEvent(ev.key, ev.delayMs), ev.delayMs);
          this.hostTimers.set(ev.key, { kind: "timeout", id });
        }
      }
    }

    wakeHostEvent(key, delayMs) {
      this.vm.wakeHostEvent(key);
      this.vm.runScheduled();
    }
  }
`;

function mapReader(files) {
  const byPath = new Map(Object.entries(files));
  return (path) => {
    const source = byPath.get(path);
    return source === undefined ? undefined : { path, source };
  };
}

function expectContractError(action, pattern) {
  assert.throws(action, (error) => (
    error instanceof QuickplayRendererContractError
    && pattern.test(error.message)
  ));
}

assert.deepEqual(QUICKPLAY_RENDERER_LIMITS, {
  maxFileSourceBytes: 16 * 1024 * 1024,
  maxGraphFiles: 4096,
  maxGraphSourceBytes: 64 * 1024 * 1024,
  maxImportsPerFile: 10_000,
  maxGraphEdges: 100_000,
  maxSpecifierBytes: 8 * 1024,
  maxAstNodes: 1_000_000,
});

const validFiles = {
  'js/dist/renderer.js': `
    import "./side-effect.js";
    export * from "./all.js";
    export { RenderIsland } from "./render_bootstrap";
    export { helper } from "./helper.js";
    void import(\`./lazy.js\`);
  `,
  [BRIDGE_PATH]: VALID_BRIDGE,
  'js/dist/all.js': 'export const all = true;',
  'js/dist/helper.js': 'export const helper = 1;',
  'js/dist/lazy.js': 'export const lazy = true;',
  'js/dist/side-effect.js': 'globalThis.rendererLoaded = true;',
};
const valid = validateVoplayRendererContract({
  entryPath: 'js/dist/renderer.js',
  readSource: mapReader(validFiles),
  declarations: DECLARATIONS,
  label: 'valid renderer',
});
assert.equal(valid.graph.fileCount, 6);
assert.equal(valid.graph.totalImportEdges, 5);
assert.deepEqual(
  new Set(valid.graph.files.map((file) => file.path)),
  new Set(Object.keys(validFiles)),
);
const entryImports = valid.graph.files.find((file) => file.path === 'js/dist/renderer.js').imports;
assert.deepEqual(
  new Set(entryImports.map((entry) => entry.kind)),
  new Set(['import', 'export-all', 'export-named', 'import-expression']),
);
assert.equal(valid.hostWaitKey.semantics.hostTimersHasEventKey, true);
assert.equal(valid.hostWaitKey.semantics.vmWakeMethodKey, true);

const sourceNoise = parseQuickplayJavaScriptModule({
  path: 'noise.js',
  source: `
    const stringNoise = "import './missing-string.js'";
    // export * from "./missing-line-comment.js";
    /* import("./missing-block-comment.js"); */
    export const value = stringNoise;
  `,
}, { label: 'source noise' });
assert.equal(sourceNoise.imports.length, 0);

const forgedGraph = collectQuickplayLocalJavaScriptGraph({
  entryPath: BRIDGE_PATH,
  readSource: mapReader({
    [BRIDGE_PATH]: `
      const forged = [
        "this.hostTimers.has(ev.key)",
        "this.displayPulseWaiters.set(ev.key, {})",
        "this.hostTimers.set(ev.key, {})",
        "this.wakeHostEvent(ev.key)",
        "this.vm.wakeHostEvent(key)",
      ];
      /*
        this.hostTimers.has(ev.key);
        this.displayPulseWaiters.set(ev.key, {});
        this.hostTimers.set(ev.key, {});
        this.wakeHostEvent(ev.key);
        this.vm.wakeHostEvent(key);
      */
      export { forged };
    `,
  }),
  label: 'forged renderer',
});
expectContractError(
  () => validateVoplayHostWaitKeyContract({
    graph: forgedGraph,
    declarations: DECLARATIONS,
    label: 'forged renderer',
  }),
  /directly export exactly one RenderIsland class/,
);

const tokenGraph = collectQuickplayLocalJavaScriptGraph({
  entryPath: BRIDGE_PATH,
  readSource: mapReader({
    [BRIDGE_PATH]: VALID_BRIDGE.replace(
      'if (this.hostTimers.has(ev.key)) continue;',
      'const legacyToken = ev.token;\n        if (this.hostTimers.has(ev.key)) continue;',
    ),
  }),
  label: 'legacy token renderer',
});
expectContractError(
  () => validateVoplayHostWaitKeyContract({
    graph: tokenGraph,
    declarations: DECLARATIONS,
    label: 'legacy token renderer',
  }),
  /token-only HostWait/,
);

const deadFactTokenAlias = VALID_BRIDGE
  .replace(
    'if (this.hostTimers.has(ev.key)) continue;',
    'const legacy = ev;\n        if (this.hostTimers.has(legacy.token)) continue;',
  )
  .replace(
    'if (ev.delayMs === DISPLAY_PULSE_DELAY_MS) {',
    'if (false) {\n          this.hostTimers.has(ev.key);\n          this.displayPulseWaiters.set(ev.key, {});\n          this.hostTimers.set(ev.key, {});\n          this.wakeHostEvent(ev.key);\n        }\n        if (ev.delayMs === DISPLAY_PULSE_DELAY_MS) {',
  );
expectContractError(
  () => validateVoplayRendererContract({
    entryPath: BRIDGE_PATH,
    readSource: mapReader({ [BRIDGE_PATH]: deadFactTokenAlias }),
    declarations: DECLARATIONS,
    label: 'dead fact token alias',
  }),
  /token-only HostWait/,
);

const deadOnlyFacts = `
  export class RenderIsland {
    scheduleHostEvents() {
      const events = this.vm.takePendingHostEvents();
      for (const ev of events) {
        if (false) {
          if (this.hostTimers.has(ev.key)) continue;
          if (ev.delayMs === DISPLAY_PULSE_DELAY_MS) {
            this.displayPulseWaiters.set(ev.key, {});
            this.hostTimers.set(ev.key, {});
          } else {
            window.setTimeout(() => this.wakeHostEvent(ev.key), 0);
            this.hostTimers.set(ev.key, {});
          }
        }
      }
    }
    wakeHostEvent(key) { this.vm.wakeHostEvent(key); }
  }
`;
expectContractError(
  () => validateVoplayRendererContract({
    entryPath: BRIDGE_PATH,
    readSource: mapReader({ [BRIDGE_PATH]: deadOnlyFacts }),
    declarations: DECLARATIONS,
    label: 'dead-only facts',
  }),
  /missing HostWaitKey AST semantics/,
);

const wrongWake = VALID_BRIDGE.replace(
  'this.vm.wakeHostEvent(key);',
  'if (false) this.vm.wakeHostEvent(key);\n      this.vm.wakeHostEvent("wrong");',
);
expectContractError(
  () => validateVoplayRendererContract({
    entryPath: BRIDGE_PATH,
    readSource: mapReader({ [BRIDGE_PATH]: wrongWake }),
    declarations: DECLARATIONS,
    label: 'wrong wake argument',
  }),
  /vm\.wakeHostEvent must use the pending event key/,
);

expectContractError(
  () => validateHostWaitKeyDeclarations({
    path: DECLARATIONS.path,
    source: `
      // takePendingHostEvents(): Array<{ key: string }>;
      const fake = "wakeHostEvent(key: string): void";
      export interface VoVm { run(): void; }
    `,
  }, { label: 'forged declarations' }),
  /takePendingHostEvents declaration/,
);

expectContractError(
  () => validateHostWaitKeyDeclarations({
    path: DECLARATIONS.path,
    source: `
      export interface VoVm {
        takePendingHostEvents(): Array<{ key: string }>;
        wakeHostEvent(token: string): void;
      }
    `,
  }, { label: 'token declarations' }),
  /wakeHostEvent declaration must accept key/,
);

expectContractError(
  () => validateHostWaitKeyDeclarations({
    path: DECLARATIONS.path,
    source: `
      export interface Decoy {
        takePendingHostEvents(): Array<{ key: string }>;
        wakeHostEvent(key: string): void;
      }
      export interface VoVm {
        takePendingHostEvents(): Array<{ token: string }>;
        wakeHostEvent(token: string): void;
      }
    `,
  }, { label: 'decoy declarations' }),
  /takePendingHostEvents declaration/,
);

expectContractError(
  () => validateHostWaitKeyDeclarations({
    path: DECLARATIONS.path,
    source: 'declare const forged = /takePendingHostEvents(): Array<{ key: string }>; wakeHostEvent(key: string): void/;',
  }, { label: 'regular expression declarations' }),
  /directly export exactly one non-inherited VoVm interface/,
);

expectContractError(
  () => parseQuickplayJavaScriptModule({
    path: 'large.js',
    source: `export const payload = "${'x'.repeat(64)}";`,
  }, {
    label: 'single file byte limit',
    limits: { maxFileSourceBytes: 32 },
  }),
  /exceeds 32 UTF-8 source bytes/,
);

expectContractError(
  () => collectQuickplayLocalJavaScriptGraph({
    entryPath: 'entry.js',
    readSource: mapReader({
      'entry.js': 'import "./a.js"; import "./b.js";',
      'a.js': 'export const a = 1;',
      'b.js': 'export const b = 1;',
    }),
    limits: { maxGraphFiles: 2 },
    label: 'graph file limit',
  }),
  /exceeds 2 files/,
);

expectContractError(
  () => collectQuickplayLocalJavaScriptGraph({
    entryPath: 'entry.js',
    readSource: mapReader({
      'entry.js': 'import "./child.js"; export const root = 1;',
      'child.js': 'export const child = 1234567890;',
    }),
    limits: { maxGraphSourceBytes: 64 },
    label: 'graph source byte limit',
  }),
  /exceeds 64 UTF-8 source bytes/,
);

expectContractError(
  () => collectQuickplayLocalJavaScriptGraph({
    entryPath: 'entry.js',
    readSource: mapReader({
      'entry.js': 'import "./a.js"; import "./b.js";',
      'a.js': '',
      'b.js': '',
    }),
    limits: { maxImportsPerFile: 1 },
    label: 'per-file import limit',
  }),
  /exceeds 1 import edges/,
);

expectContractError(
  () => parseQuickplayJavaScriptModule({
    path: 'dynamic-imports.js',
    source: 'import(firstPath); import(secondPath);',
  }, {
    limits: { maxImportsPerFile: 1 },
    label: 'dynamic import limit',
  }),
  /exceeds 1 import edges/,
);

expectContractError(
  () => collectQuickplayLocalJavaScriptGraph({
    entryPath: 'entry.js',
    readSource: mapReader({
      'entry.js': 'import "./a.js";',
      'a.js': 'import "./b.js";',
      'b.js': '',
    }),
    limits: { maxGraphEdges: 1 },
    label: 'graph edge limit',
  }),
  /exceeds 1 static import edges/,
);

expectContractError(
  () => parseQuickplayJavaScriptModule({
    path: 'specifier.js',
    source: 'import "./long.js";',
  }, {
    limits: { maxSpecifierBytes: 4 },
    label: 'specifier byte limit',
  }),
  /specifier exceeds 4 UTF-8 bytes/,
);

expectContractError(
  () => parseQuickplayJavaScriptModule({
    path: 'nodes.js',
    source: 'const first = 1; const second = 2;',
  }, {
    limits: { maxAstNodes: 8 },
    label: 'AST node limit',
  }),
  /(token count exceeds the AST budget 8|AST exceeds 8 nodes)/,
);

expectContractError(
  () => parseQuickplayJavaScriptModule({
    path: 'broken.js',
    source: 'export const = ;',
  }, { label: 'syntax validation' }),
  /is not a valid JavaScript module/,
);

expectContractError(
  () => collectQuickplayLocalJavaScriptGraph({
    entryPath: 'entry.js',
    readSource: mapReader({ 'entry.js': 'import "../escape.js";' }),
    label: 'package escape',
  }),
  /local import escapes the module package/,
);

console.log('quickplay renderer contract selftest: ok');
