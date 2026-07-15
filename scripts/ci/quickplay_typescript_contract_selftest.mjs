import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

import { validateStudioQuickplayTypeScriptContract } from './quickplay_typescript_contract.mjs';

const ciDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(ciDir, '..', '..');
const quickplayPath = join(repoRoot, 'apps', 'studio', 'src', 'lib', 'quickplay.ts');
const currentSource = readFileSync(quickplayPath, 'utf8');
const staticFunctionStart = currentSource.indexOf('export function staticPackageUrl');
const staticFunctionBodyStart = currentSource.indexOf('{', staticFunctionStart);
const staticFunctionEnd = currentSource.indexOf(
  '\n}\n\nexport const BLOCKKART_PROJECT_PACKAGE_URL',
  staticFunctionBodyStart,
);

assert.notEqual(staticFunctionStart, -1, 'current quickplay.ts must contain staticPackageUrl');
assert.notEqual(staticFunctionBodyStart, -1, 'staticPackageUrl must have a body');
assert.notEqual(staticFunctionEnd, -1, 'staticPackageUrl must end before project package export');

function withStaticPackageUrlBody(lines) {
  return `${currentSource.slice(0, staticFunctionBodyStart + 1)}\n${lines.join('\n')}${currentSource.slice(staticFunctionEnd)}`;
}

let checks = 0;

function accept(name, source) {
  assert.doesNotThrow(
    () => validateStudioQuickplayTypeScriptContract(source, { label: name }),
    `${name} should satisfy the contract`,
  );
  checks += 1;
}

function reject(name, source, expected) {
  assert.throws(
    () => validateStudioQuickplayTypeScriptContract(source, { label: name }),
    expected,
    `${name} should violate the contract`,
  );
  checks += 1;
}

accept('current apps/studio/src/lib/quickplay.ts', currentSource);

reject(
  'comment cannot forge build versioning',
  currentSource.replace(
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    "  // url.searchParams.set('build', __STUDIO_BUILD_ID__);",
  ),
  /must directly call url\.searchParams\.set/,
);

reject(
  'string cannot forge build versioning',
  currentSource.replace(
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    "  const decoy = \"url.searchParams.set('build', __STUDIO_BUILD_ID__)\";",
  ),
  /must directly call url\.searchParams\.set/,
);

reject(
  'nested function cannot forge build versioning',
  currentSource.replace(
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    "  function decoy() { url.searchParams.set('build', __STUDIO_BUILD_ID__); }",
  ),
  /must directly call url\.searchParams\.set/,
);

reject(
  'build id must be the declared identifier',
  currentSource.replace('__STUDIO_BUILD_ID__', "'forged-build'"),
  /must directly call url\.searchParams\.set/,
);

reject(
  'source-local build id alias is rejected',
  `const __STUDIO_BUILD_ID__ = 'forged-build';\n${currentSource}`,
  /must directly call url\.searchParams\.set/,
);

reject(
  'source-local URL constructor alias is rejected',
  `const URL = class ForgedUrl {};\n${currentSource}`,
  /URL binding must be const url = new URL/,
);

reject(
  'URL binding must be an ordinary const declaration',
  currentSource.replace(
    '  const url = new URL(path, window.location.origin);',
    '  await using url = new URL(path, window.location.origin);',
  ),
  /const URL binding and build query call must be the first two function statements/,
);

reject(
  'bound query cannot be discarded by returning path',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  return path;',
  ]),
  /every staticPackageUrl return must preserve the bound URL path and build query/,
);

reject(
  'one branch cannot discard the bound query',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  if (url.origin === window.location.origin) {',
    '    return `${url.pathname}${url.search}${url.hash}`;',
    '  }',
    '  return path;',
  ]),
  /every staticPackageUrl return must preserve the bound URL path and build query/,
);

reject(
  'branch-local set cannot masquerade as dominating set',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    '  if (url.origin === window.location.origin) {',
    "    url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  }',
    '  return url.toString();',
  ]),
  /must directly call url\.searchParams\.set/,
);

reject(
  'dead set after an early return cannot satisfy dominance',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    '  return path;',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  return url.toString();',
  ]),
  /first two function statements/,
);

reject(
  'return must use the checked URL binding',
  `const otherUrl = new URL('/other', window.location.origin);\n${withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  return otherUrl.toString();',
  ])}`,
  /every staticPackageUrl return must preserve the bound URL path and build query/,
);

reject(
  'branch condition cannot remove the build query',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    "  if (url.searchParams.delete('build')) {",
    '    return url.toString();',
    '  }',
    '  return url.toString();',
  ]),
  /branches after build binding must be side-effect free/,
);

reject(
  'query cannot be cleared after binding',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    "  url.search = '';",
    '  return url.toString();',
  ]),
  /permits only side-effect-free branches and URL-derived returns/,
);

reject(
  'conditional return must preserve query on both branches',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  return path.length > 0 ? url.toString() : path;',
  ]),
  /every staticPackageUrl return must preserve the bound URL path and build query/,
);

reject(
  'dead return branch is rejected',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  return url.toString();',
    '  return path;',
  ]),
  /contains dead code after a guaranteed return/,
);

reject(
  'every control-flow path must return the bound URL',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  if (url.origin === window.location.origin) {',
    '    return url.toString();',
    '  }',
  ]),
  /must return the build-versioned URL on every control-flow path/,
);

accept(
  'explicit searchParams serialization preserves the binding',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    "  return url.pathname + '?' + url.searchParams.toString() + url.hash;",
  ]),
);

reject(
  'serialized params require a leading query delimiter',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  return url.pathname + url.searchParams.toString();',
  ]),
  /every staticPackageUrl return must preserve the bound URL path and build query/,
);

reject(
  'query delimiter cannot follow serialized params',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    "  return url.pathname + url.searchParams.toString() + '?';",
  ]),
  /every staticPackageUrl return must preserve the bound URL path and build query/,
);

reject(
  'search query cannot be appended after the fragment',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    '  return `${url.pathname}${url.hash}${url.search}`;',
  ]),
  /every staticPackageUrl return must preserve the bound URL path and build query/,
);

reject(
  'serialized params cannot be appended after the fragment',
  withStaticPackageUrlBody([
    '  const url = new URL(path, window.location.origin);',
    "  url.searchParams.set('build', __STUDIO_BUILD_ID__);",
    "  return url.pathname + '?' + url.hash + url.searchParams.toString();",
  ]),
  /every staticPackageUrl return must preserve the bound URL path and build query/,
);

reject(
  'project URL is exact',
  currentSource.replace('/quickplay/blockkart/project.json', '/quickplay/blockkart/project-v2.json'),
  /BLOCKKART_PROJECT_PACKAGE_URL must equal staticPackageUrl/,
);

reject(
  'dependency URL is exact',
  currentSource.replace('/quickplay/blockkart/deps.json', '/quickplay/blockkart/dependencies.json'),
  /BLOCKKART_DEPS_PACKAGE_URL must equal staticPackageUrl/,
);

reject(
  'string literal cannot hard-code artifact URL',
  `${currentSource}\nconst forbidden = '/quickplay/blockkart/artifacts/runtime.wasm';\n`,
  /hard-coded BlockKart artifact URL is forbidden/,
);

reject(
  'no-substitution template cannot hard-code artifact URL',
  `${currentSource}\nconst forbidden = \`/quickplay/blockkart/artifacts/runtime.wasm\`;\n`,
  /hard-coded BlockKart artifact URL is forbidden/,
);

reject(
  'escaped string cannot hide artifact URL',
  `${currentSource}\nconst forbidden = '\\u002fquickplay/blockkart/artifacts/runtime.wasm';\n`,
  /hard-coded BlockKart artifact URL is forbidden/,
);

reject(
  'constant concatenation cannot hide artifact URL',
  `${currentSource}\nconst forbidden = '/quickplay/blockkart/' + 'artifacts/runtime.wasm';\n`,
  /constant-folded hard-coded BlockKart artifact URL is forbidden/,
);

reject(
  'constant aliases cannot hide artifact URL',
  `${currentSource}
export const forbiddenBase = '/quickplay/blockkart/';
export const forbiddenAlias = forbiddenBase + 'artifacts/runtime.wasm';
`,
  /constant-folded hard-coded BlockKart artifact URL is forbidden/,
);

accept(
  'benign constant concatenation remains declarative',
  `${currentSource}\nexport const benignPackagePath = '/quickplay/blockkart/' + 'project.json';\n`,
);

reject(
  'top-level prototype mutation cannot change URL return semantics',
  `Object.defineProperty(URL.prototype, 'origin', {
  get() {
    this.searchParams.delete('build');
    return window.location.origin;
  },
});
${currentSource}`,
  /top-level Quickplay code permits only/,
);

reject(
  'exported const initializer cannot conceal a side-effect call',
  `${currentSource}
export const forbiddenMutation = Object.defineProperty(URL.prototype, 'origin', {
  value: window.location.origin,
});
`,
  /unsupported executable syntax in pure top-level constant expression: CallExpression/,
);

reject(
  'top-level await using is excluded from pure const declarations',
  `${currentSource}\nexport await using forbiddenResource = [];\n`,
  /top-level Quickplay constants must be single-name const declarations with initializers/,
);

reject(
  'path parameter requires an explicit string type',
  currentSource.replace(
    'staticPackageUrl(path: string)',
    'staticPackageUrl(path)',
  ),
  /path parameter must be explicitly typed as string/,
);

reject(
  'path parameter decorator cannot execute during module evaluation',
  currentSource.replace(
    'staticPackageUrl(path: string)',
    "staticPackageUrl(@Object.defineProperty(URL.prototype, 'origin', { value: window.location.origin }) path: string)",
  ),
  /path parameter cannot have decorators or modifiers/,
);

reject(
  'duplicate target export is rejected',
  `${currentSource}\nexport { staticPackageUrl };\n`,
  /expected exactly one named export staticPackageUrl, found 2/,
);

reject(
  'parse diagnostics are rejected',
  `${currentSource}\nexport const malformed = ;\n`,
  /TypeScript parse error/,
);

const nestedExpression = `${'('.repeat(300)}0${')'.repeat(300)}`;
reject(
  'AST depth is bounded',
  `${currentSource}\nconst deeplyNested = ${nestedExpression};\n`,
  /AST exceeds depth 256/,
);

const constantFoldNestedExpression = `${'('.repeat(140)}0${')'.repeat(140)}`;
reject(
  'constant-fold depth is independently bounded',
  `${currentSource}\nconst deeplyFolded = ${constantFoldNestedExpression};\n`,
  /constant folding exceeds depth 128/,
);

reject(
  'AST node count is bounded',
  `${currentSource}\n${';'.repeat(500_001)}`,
  /AST exceeds 500000 nodes/,
);

reject(
  'source byte size is bounded',
  `${currentSource}\n/*${'x'.repeat(16 * 1024 * 1024)}*/`,
  /source exceeds 16777216 UTF-8 bytes/,
);

console.log(`quickplay TypeScript contract self-test passed (${checks} checks)`);
