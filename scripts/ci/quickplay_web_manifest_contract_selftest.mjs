#!/usr/bin/env node
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import {
  QUICKPLAY_WEB_MANIFEST_LIMITS,
  canonicalWebSourceDigest,
  parseBoundedStrictJsonBytes,
  parseVoModWebMetadata,
  validateWebManifestContract,
  validateWebManifestVoModContract,
  validatePortablePathComponent,
} from './quickplay_web_manifest_contract.mjs';

const digest = (bytes) => `sha256:${createHash('sha256').update(bytes).digest('hex')}`;

function sourceEntry(path, bytes) {
  return { path, size: bytes.byteLength, digest: digest(bytes) };
}

function minimalManifest(sourceBytes = Buffer.from('module github.com/acme/lib\nvo ^0.1.0\n')) {
  const source = [sourceEntry('vo.mod', sourceBytes)];
  return {
    schema_version: 1,
    module: 'github.com/acme/lib',
    version: 'v1.2.3',
    commit: '0123456789abcdef0123456789abcdef01234567',
    module_root: '.',
    vo: '^0.1.0',
    require: [],
    source_digest: canonicalWebSourceDigest(source),
    source,
    artifacts: [],
    web: null,
    extension: null,
  };
}

function clone(value) {
  return structuredClone(value);
}

function reject(mutator, pattern) {
  const value = clone(minimalManifest());
  mutator(value);
  assert.throws(() => validateWebManifestContract(value), pattern);
}

let checks = 0;
function check(name, callback) {
  callback();
  checks += 1;
  process.stdout.write(`ok ${checks} - ${name}\n`);
}

check('accepts and normalizes the canonical minimal Rust protocol value', () => {
  const value = minimalManifest();
  const normalized = validateWebManifestContract(value, 'fixture', {
    sourceBytes: (path) => {
      assert.equal(path, 'vo.mod');
      return Buffer.from('module github.com/acme/lib\nvo ^0.1.0\n');
    },
    expectedModule: 'github.com/acme/lib',
    expectedVersion: 'v1.2.3',
    expectedCommit: value.commit,
    expectedVo: '^0.1.0',
  });
  assert.equal(normalized.source.reduce((total, entry) => total + entry.size, 0), value.source[0].size);
  assert.deepEqual(normalized.require, []);
  assert.equal(normalized.web, null);
  assert.equal(normalized.extension, null);
});

check('strict JSON parser rejects duplicate keys and enforces bounded Unicode scalar input', () => {
  const parse = (source, options = {}) => parseBoundedStrictJsonBytes(
    Buffer.from(source, 'utf8'),
    'strict JSON fixture',
    options,
  );
  assert.deepEqual(parse(String.raw`{"emoji":"\ud83d\ude80","nested":{"ok":"\ud834\udd1e"}}`), {
    emoji: '🚀',
    nested: { ok: '𝄞' },
  });
  assert.throws(() => parse('{"outer":{"digest":1,"digest":2}}'), /duplicate object key "digest"/);
  assert.throws(() => parse(String.raw`{"a":1,"\u0061":2}`), /duplicate object key "a"/);
  assert.throws(() => parse(String.raw`{"\ud800":1}`), /object key must contain only Unicode scalar values/);
  assert.throws(() => parse(String.raw`{"value":"\ud800"}`), /JSON string must contain only Unicode scalar values/);
  assert.throws(() => parse(String.raw`{"nested":[{"value":"\udfff"}]}`), /JSON string must contain only Unicode scalar values/);
  assert.throws(
    () => parseBoundedStrictJsonBytes(Buffer.from([0xff]), 'strict JSON fixture'),
    /valid UTF-8/,
  );
  assert.throws(() => parse('[[[0]]]', { maxDepth: 2 }), /depth limit/);
  assert.doesNotThrow(() => parse(`${'['.repeat(127)}0${']'.repeat(127)}`));
  assert.throws(() => parse(`${'['.repeat(128)}0${']'.repeat(128)}`), /127-level JSON depth limit/);
  assert.throws(() => parse('[0,1,2]', { maxTokens: 3 }), /token JSON limit/);
  assert.throws(() => parse('{"size":1e400}'), /outside the finite f64 range/);
  assert.throws(() => parse('{"long":1}', { maxObjectKeyBytes: 3 }), /object-key limit/);
  assert.throws(
    () => parseBoundedStrictJsonBytes(Buffer.from('{}'), 'strict JSON fixture', { maxBytes: 1 }),
    /at most 1 bytes/,
  );
});

check('rejects unknown and missing top-level fields', () => {
  reject((value) => { value.surprise = true; }, /unsupported field/);
  reject((value) => { delete value.source; }, /missing required field "source"/);
  reject((value) => { value.schema_version = 2; }, /must equal 1/);
});

check('binds module path, version major, module root, commit, and toolchain spelling', () => {
  reject((value) => { value.version = 'v2.0.0'; }, /incompatible/);
  reject((value) => { value.module_root = 'nested'; }, /must equal/);
  reject((value) => { value.commit = value.commit.toUpperCase(); }, /lowercase/);
  reject((value) => { value.vo = 'v0.1.0'; }, /without v/);
  reject((value) => { value.module = 'github.com/Acme/lib'; }, /lowercase ASCII/);

  const nested = minimalManifest();
  nested.module = 'github.com/acme/mono/graphics/v2';
  nested.version = 'v2.3.4';
  nested.module_root = 'graphics/v2';
  assert.equal(validateWebManifestContract(nested).module_root, 'graphics/v2');
});

check('enforces canonical dependency edges', () => {
  reject((value) => {
    value.require = [
      { module: 'github.com/acme/z', constraint: '^1.0.0' },
      { module: 'github.com/acme/a', constraint: '^1.0.0' },
    ];
  }, /sorted/);
  reject((value) => {
    value.require = [{ module: value.module, constraint: '^1.0.0' }];
  }, /must not require/);
  reject((value) => {
    value.require = [{ module: 'github.com/acme/dep/v2', constraint: '^1.0.0' }];
  }, /incompatible/);
  reject((value) => {
    value.require = [{ module: 'github.com/acme/dep', constraint: '1.0.0' }];
  }, /must start/);
});

check('enforces the canonical source set and source digest', () => {
  reject((value) => { value.source = []; }, /must contain vo.mod/);
  reject((value) => { value.source[0].path = 'artifacts/evil.vo'; }, /source-set-excluded/);
  reject((value) => { value.source[0].path = 'VO.WEB.JSON'; }, /source-set-excluded/);
  reject((value) => { value.source[0].path = 'vo.web.jſon'; }, /source-set-excluded/);
  reject((value) => { value.source[0].path = 'artifactſ/evil.vo'; }, /source-set-excluded/);
  reject((value) => { value.source[0].path = '.vo-ſource-digest/evil.vo'; }, /source-set-excluded/);
  reject((value) => { value.source[0].digest = `sha256:${'A'.repeat(64)}`; }, /canonical lowercase/);
  reject((value) => { value.source_digest = `sha256:${'0'.repeat(64)}`; }, /digest mismatch/);
  reject((value) => { value.source[0].size = QUICKPLAY_WEB_MANIFEST_LIMITS.sourceFileBytes + 1; }, /safe integer/);
});

check('allows nested reserved-looking names after full Unicode folding', () => {
  const bytes = Buffer.from('x');
  const source = [
    sourceEntry('nested/vo.web.jſon', bytes),
    sourceEntry('vo.mod', bytes),
  ];
  const value = minimalManifest(bytes);
  value.source = source;
  value.source_digest = canonicalWebSourceDigest(source);
  assert.deepEqual(validateWebManifestContract(value).source, source);
});

check('matches Rust Unicode boundary whitespace and source-file limits', () => {
  assert.equal(validatePortablePathComponent('\ufeffsource.vo'), '\ufeffsource.vo');
  assert.equal(validatePortablePathComponent('source\ufeff.vo'), 'source\ufeff.vo');
  assert.throws(() => validatePortablePathComponent('\u00a0source.vo'), /portable path component/);
  assert.throws(() => validatePortablePathComponent('source.vo\u00a0'), /portable path component/);
  const value = minimalManifest();
  value.source[0].size = QUICKPLAY_WEB_MANIFEST_LIMITS.sourceFileBytes + 1;
  assert.throws(() => validateWebManifestContract(value), /safe integer/);
});

check('rejects source order, prefix, ASCII-case, and full-fold collisions', () => {
  const bytes = Buffer.from('x');
  for (const paths of [
    ['z.vo', 'a.vo', 'vo.mod'],
    ['dir', 'dir/file.vo', 'vo.mod'],
    ['Readme.vo', 'README.vo', 'vo.mod'],
    ['vo.mod', 'ϑ.vo', 'ϴ.vo'],
    ['vo.mod', 'ss.vo', 'ß.vo'],
  ]) {
    const source = paths.map((path) => sourceEntry(path, bytes));
    const value = minimalManifest(bytes);
    value.source = source;
    value.source_digest = canonicalWebSourceDigest(source);
    assert.throws(() => validateWebManifestContract(value), /sorted|descends through|conflicts/);
  }
});

check('rejects oversized collections before traversing entries', () => {
  const value = minimalManifest();
  value.source = new Array(QUICKPLAY_WEB_MANIFEST_LIMITS.files + 1).fill(null);
  assert.throws(() => validateWebManifestContract(value), /more than 20000/);
});

check('verifies optional source byte callbacks and callback contracts', () => {
  const value = minimalManifest();
  assert.throws(
    () => validateWebManifestContract(value, 'fixture', { sourceBytes: () => Buffer.from('wrong') }),
    /declared size|declared digest/,
  );
  assert.throws(
    () => validateWebManifestContract(value, 'fixture', { sourceBytes: () => 'wrong type' }),
    /must return Uint8Array/,
  );
  assert.throws(
    () => validateWebManifestContract(value, 'fixture', { digestBytes: () => 'nope' }),
    /computed digest/,
  );
});

check('validates project web metadata as a closed portable contract', () => {
  const value = minimalManifest();
  value.web = { entry: 'Main', include: ['assets', 'pages/home.vo'] };
  assert.deepEqual(validateWebManifestContract(value).web, value.web);
  value.web.unknown = true;
  assert.throws(() => validateWebManifestContract(value), /unsupported field/);
  delete value.web.unknown;
  value.web.include = ['Assets', 'assets'];
  assert.throws(() => validateWebManifestContract(value), /conflicts/);
});

check('validates browser artifact identity, order, path, bytes, and size', () => {
  const bytes = Buffer.from('wasm');
  const value = minimalManifest();
  value.artifacts = [{
    kind: 'extension-wasm',
    target: 'wasm32-unknown-unknown',
    name: 'demo.wasm',
    path: 'web-artifacts/demo.wasm',
    size: bytes.length,
    digest: digest(bytes),
  }];
  assert.equal(validateWebManifestContract(value, 'fixture', {
    artifactBytes: () => bytes,
  }).artifacts.length, 1);
  const native = clone(value);
  native.artifacts[0].kind = 'extension-native';
  native.artifacts[0].target = 'x86_64-unknown-linux-gnu';
  assert.throws(() => validateWebManifestContract(native), /must not list native/);
  const tooLarge = clone(value);
  tooLarge.artifacts[0].size = QUICKPLAY_WEB_MANIFEST_LIMITS.fileBytes + 1;
  assert.throws(() => validateWebManifestContract(tooLarge), /safe integer/);
  assert.throws(
    () => validateWebManifestContract(value, 'fixture', { artifactBytes: () => Buffer.from('drift') }),
    /declared size|declared digest/,
  );
});

check('validates the full extension structure and cross-file collisions', () => {
  const value = minimalManifest();
  value.extension = {
    name: 'demo',
    include: ['js/dist'],
    wasm: {
      kind: 'Bindgen',
      wasm: 'demo.wasm',
      js_glue: 'demo.js',
      local_wasm: 'web-artifacts/demo.wasm',
      local_js_glue: 'web-artifacts/demo.js',
    },
    web: {
      entry: 'Run',
      capabilities: ['widget'],
      js_modules: { renderer: 'js/dist/renderer.js' },
    },
  };
  assert.equal(validateWebManifestContract(value).extension.wasm.kind, 'Bindgen');
  const duplicateCapability = clone(value);
  duplicateCapability.extension.web.capabilities = ['widget', 'widget'];
  assert.throws(() => validateWebManifestContract(duplicateCapability), /must not contain duplicates/);
  const collision = clone(value);
  collision.extension.web.js_modules.renderer = 'WEB-ARTIFACTS/DEMO.WASM';
  assert.throws(() => validateWebManifestContract(collision), /conflicts/);
  const standaloneGlue = clone(value);
  standaloneGlue.extension.wasm.kind = 'Standalone';
  assert.throws(() => validateWebManifestContract(standaloneGlue), /must not declare js_glue/);
  const missingInclude = clone(value);
  delete missingInclude.extension.include;
  assert.throws(() => validateWebManifestContract(missingInclude), /missing required field/);
});

const fullVoMod = String.raw`module github.com/acme/lib

vo ^0.1.0

[web]
entry = "Main"
include = ["assets", 'pages']

[extension]
name = 'demo'
include = [
  "js/dist",
]

[extension.native]
path = 'rust/target/{profile}/libdemo'

[[extension.native.targets]]
target = "x86_64-unknown-linux-gnu"
library = 'libdemo.so'

[extension.wasm]
type = "bindgen"
wasm = 'demo.wasm'
js_glue = "demo.js"
local_wasm = 'web-artifacts/demo.wasm'
local_js_glue = "web-artifacts/demo.js"

[extension.web]
entry = "Run"
capabilities = ["widget"]

[extension.web.js]
renderer = 'js/dist/renderer.js'
`;

check('parses basic and literal TOML strings into the Rust metadata shape', () => {
  const metadata = parseVoModWebMetadata(fullVoMod, 'fixture vo.mod');
  assert.deepEqual(metadata.web, { entry: 'Main', include: ['assets', 'pages'] });
  assert.equal(metadata.extension.name, 'demo');
  assert.equal(metadata.extension.native.targets[0].library, 'libdemo.so');
  assert.equal(metadata.webManifestExtension.wasm.kind, 'Bindgen');
  assert.equal(metadata.webManifestExtension.web.js_modules.renderer, 'js/dist/renderer.js');
  assert.deepEqual(metadata.declaredArtifacts.map((artifact) => artifact.kind), [
    'extension-js-glue',
    'extension-native',
    'extension-wasm',
  ]);
});

const rustAcceptedEquivalentMetadata = [
  String.raw`[extension]
name = "demo"
include = ["js/dist"]

[extension.web]
entry = "Run"
capabilities = ["widget"]

[extension.web.js]
renderer = "js/dist/renderer.js"
`,
  String.raw`[extension]
name = "demo"
include = ["js/dist"]
web.entry = "Run"
web.capabilities = ["widget"]
web.js.renderer = "js/dist/renderer.js"
`,
  String.raw`[extension]
name = "demo"
include = ["js/dist"]
web = { entry = "Run", capabilities = ["widget"], js = { renderer = "js/dist/renderer.js" } }
`,
  String.raw`["extension"]
"name" = "demo"
"include" = ["js/dist"]

["extension"."web"]
"entry" = "Run"
"capabilities" = ["widget"]
"js"."renderer" = "js/dist/renderer.js"
`,
];

check('matches Rust-accepted table, dotted-key, inline-table, and quoted-key forms', () => {
  const normalized = rustAcceptedEquivalentMetadata.map((source) => (
    parseVoModWebMetadata(source, 'Rust parity fixture').webManifestExtension
  ));
  for (const value of normalized.slice(1)) {
    assert.equal(JSON.stringify(value), JSON.stringify(normalized[0]));
  }
  assert.equal(normalized[0].web.js_modules.renderer, 'js/dist/renderer.js');
});

check('matches Rust multiline basic and literal string semantics', () => {
  const source = String.raw`[extension]
name = """demo"""
include = ["""\
js/dist"""]

[extension.wasm]
type = '''bindgen'''
wasm = '''demo.wasm'''
js_glue = """demo.js"""
local_wasm = """\
web-artifacts/demo.wasm"""
local_js_glue = '''web-artifacts/demo.js'''
`;
  const metadata = parseVoModWebMetadata(source, 'multiline Rust parity fixture');
  assert.equal(metadata.extension.name, 'demo');
  assert.deepEqual(metadata.extension.include, ['js/dist']);
  assert.equal(metadata.extension.wasm.wasm, 'demo.wasm');
  assert.equal(metadata.extension.wasm.local_wasm, 'web-artifacts/demo.wasm');
});

check('merges implicit tables and accepts both array-table and inline target arrays', () => {
  const arrayTableSource = String.raw`[[extension.native.targets]]
target = "x86_64-unknown-linux-gnu"
library = "libdemo.so"

[[extension.native.targets]]
target = "aarch64-apple-darwin"
library = "libdemo.dylib"

[extension]
name = "demo"

[extension.native]
path = "rust/libdemo"
`;
  const inlineArraySource = String.raw`[extension]
name = "demo"
native = { path = "rust/libdemo", targets = [
  { target = "x86_64-unknown-linux-gnu", library = "libdemo.so" },
  { target = "aarch64-apple-darwin", library = "libdemo.dylib" },
] }
`;
  const fromArrayTable = parseVoModWebMetadata(arrayTableSource, 'array-table parity fixture');
  const fromInlineArray = parseVoModWebMetadata(inlineArraySource, 'inline-array parity fixture');
  assert.deepEqual(fromArrayTable.extension.native, fromInlineArray.extension.native);
  assert.deepEqual(fromArrayTable.extension.native.targets, [{
    target: 'x86_64-unknown-linux-gnu',
    library: 'libdemo.so',
  }, {
    target: 'aarch64-apple-darwin',
    library: 'libdemo.dylib',
  }]);
});

check('treats U+FEFF according to Rust Unicode 16 whitespace and control boundaries', () => {
  const metadata = parseVoModWebMetadata(String.raw`[extension]
name = "demo"

[extension.wasm]
type = "standalone"
wasm = "﻿demo.wasm"
`);
  assert.equal(metadata.extension.wasm.wasm, '\ufeffdemo.wasm');
  assert.throws(() => parseVoModWebMetadata(`[extension]\nname = "demo"\n[extension.wasm]\ntype = "standalone"\nwasm = " \t"\n`), /must not be empty/);
  assert.throws(() => parseVoModWebMetadata(String.raw`[extension]
name = "demo"
[extension.wasm]
type = "standalone"
wasm = "\u0001demo.wasm"
`), /portable path component/);
});

check('rejects malformed and unknown relevant TOML tables and fields', () => {
  for (const [source, pattern] of [
    ['[extension.wasm]]\ntype = "standalone"\nwasm = "x.wasm"\n', /malformed|trailing/],
    ['[extension.magic]\nvalue = "x"\n', /unsupported field/],
    ['["extension.web".js]\nrenderer = "x.js"\n', /unsupported field/],
    ['[extension]\nname = "x"\nname = "y"\n', /duplicate/],
    ['[extension]\nname = "x"\nunknown = "y"\n', /unsupported field/],
    ['[extension]\nname = "x"\n[extension.wasm]\ntype = "standalone"\n', /wasm.*required/],
    ['[extension]\nname = "x"\nweb.js.renderer = "x.js"\n[extension.web]\n', /duplicate or conflicting table/],
    ['[extension]\nname = "x"\nweb = { js = { renderer = "x.js" } }\n[extension.web]\n', /duplicate or conflicting table/],
    ['[extension]\nname = "x"\nnative.targets = [{ target = "x86_64-unknown-linux-gnu", library = "x.so" }]\n[[extension.native.targets]]\ntarget = "aarch64-apple-darwin"\nlibrary = "x.dylib"\n', /array table conflicts/],
    ['[extension]\nname = "x"\nweb = { js = { renderer = "x.js", renderer = "y.js" } }\n', /duplicates key/],
    ['[extension]\nname = "x"\nweb = { js = { renderer = "x.js", } }\n', /trailing comma/],
    ['[extension]\nname = """unterminated\n', /unterminated basic string/],
    ['["""extension"""]\nname = "x"\n', /single-line quoted key/],
    ['[extension]\nname = "x"\nweb = { mystery = "y" }\n', /unsupported field/],
  ]) {
    assert.throws(() => parseVoModWebMetadata(source), pattern);
  }
});

check('binds web manifest metadata and exact browser artifact IDs to vo.mod', () => {
  const voModBytes = Buffer.from(fullVoMod);
  const value = minimalManifest(voModBytes);
  const metadata = parseVoModWebMetadata(fullVoMod);
  value.web = metadata.web;
  value.extension = metadata.webManifestExtension;
  const artifactBytes = new Map([
    ['extension-js-glue', Buffer.from('javascript')],
    ['extension-wasm', Buffer.from('wasm')],
  ]);
  value.artifacts = metadata.declaredArtifacts
    .filter((artifact) => artifact.kind !== 'extension-native')
    .map((artifact) => {
      const bytes = artifactBytes.get(artifact.kind);
      return {
        ...artifact,
        path: `web-artifacts/${artifact.name}`,
        size: bytes.byteLength,
        digest: digest(bytes),
      };
    });
  const result = validateWebManifestVoModContract(value, fullVoMod, 'fixture', {
    sourceBytes: () => voModBytes,
    artifactBytes: (artifact) => artifactBytes.get(artifact.kind),
  });
  assert.equal(result.manifest.extension.name, 'demo');

  const missingArtifact = clone(value);
  missingArtifact.artifacts.pop();
  assert.throws(
    () => validateWebManifestVoModContract(missingArtifact, fullVoMod),
    /artifact identities/,
  );
  const metadataDrift = clone(value);
  metadataDrift.extension.name = 'other';
  assert.throws(
    () => validateWebManifestVoModContract(metadataDrift, fullVoMod),
    /extension metadata/,
  );
});

process.stdout.write(`1..${checks}\n`);
