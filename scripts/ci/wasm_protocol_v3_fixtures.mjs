const I32 = 0x7f;
const I64 = 0x7e;

function u32(value) {
  const bytes = [];
  let remaining = value >>> 0;
  do {
    let byte = remaining & 0x7f;
    remaining >>>= 7;
    if (remaining !== 0) byte |= 0x80;
    bytes.push(byte);
  } while (remaining !== 0);
  return bytes;
}

function vector(entries) {
  return [...u32(entries.length), ...entries.flat()];
}

function wasmName(value) {
  const bytes = [...Buffer.from(value, 'utf8')];
  return [...u32(bytes.length), ...bytes];
}

function section(id, payload) {
  return [id, ...u32(payload.length), ...payload];
}

function sameType(left, right) {
  return JSON.stringify(left) === JSON.stringify(right);
}

function zeroResultBody(results) {
  const body = [];
  for (const result of results) {
    if (result === I32) body.push(0x41, 0x00);
    else if (result === I64) body.push(0x42, 0x00);
    else throw new Error(`unsupported fixture result type ${result}`);
  }
  body.push(0x0b);
  return body;
}

/** Build small, valid modules used only by CI negative-contract tests. */
export function protocolWasmFixture({
  includeProtocol = true,
  protocolParameters = [],
  protocolResults = [I32],
  protocolVersion = 3,
  bindgenExportKeys = null,
  bindgenWrapperParameters = [I32, I32],
  bindgenWrapperResults = [I32, I32],
  standaloneExportKeys = null,
  extraFunctionExports = [],
  imports = [],
  customSectionText = null,
} = {}) {
  const functions = [];
  if (includeProtocol) {
    const opcode = protocolResults[0] === I64 ? 0x42 : 0x41;
    functions.push({
      name: 'vo_ext_protocol_version',
      parameters: protocolParameters,
      results: protocolResults,
      body: [opcode, protocolVersion, 0x0b],
    });
  }
  if (bindgenExportKeys !== null) {
    functions.push(...bindgenExportKeys.map((name) => ({
      name,
      parameters: bindgenWrapperParameters,
      results: bindgenWrapperResults,
      body: zeroResultBody(bindgenWrapperResults),
    })));
  }
  if (standaloneExportKeys !== null) {
    functions.push(
      { name: 'vo_alloc', parameters: [I32], results: [I32], body: [0x20, 0x00, 0x0b] },
      { name: 'vo_dealloc', parameters: [I32, I32], results: [], body: [0x0b] },
      ...standaloneExportKeys.map((name) => ({
        name,
        parameters: [I32, I32, I32],
        results: [I32],
        body: [0x41, 0x00, 0x0b],
      })),
    );
  }
  functions.push(...extraFunctionExports.map((name) => ({
    name,
    parameters: [],
    results: [],
    body: [0x0b],
  })));

  const types = [];
  const typeIndex = (parameters, results) => {
    const type = { parameters, results };
    let index = types.findIndex((candidate) => sameType(candidate, type));
    if (index < 0) {
      index = types.length;
      types.push(type);
    }
    return index;
  };
  let importedFunctionCount = 0;
  let importedMemoryCount = 0;
  let importedGlobalCount = 0;
  const importEntries = imports.map((entry) => {
    const prefix = [...wasmName(entry.module), ...wasmName(entry.name)];
    const kind = entry.kind ?? 'function';
    if (kind === 'function') {
      importedFunctionCount += 1;
      return [
        ...prefix,
        0x00,
        ...u32(typeIndex(entry.parameters ?? [], entry.results ?? [])),
      ];
    }
    if (kind === 'table') return [...prefix, 0x01, 0x70, 0x00, 0x01];
    if (kind === 'memory') {
      importedMemoryCount += 1;
      return [...prefix, 0x02, 0x00, 0x01];
    }
    if (kind === 'global') {
      importedGlobalCount += 1;
      return [...prefix, 0x03, I32, 0x00];
    }
    throw new Error(`unsupported fixture import kind ${kind}`);
  });
  const typeIndices = functions.map(({ parameters, results }) => typeIndex(parameters, results));
  const typePayload = vector(types.map(({ parameters, results }) => [
    0x60,
    ...vector(parameters),
    ...vector(results),
  ]));
  const importSection = imports.length === 0 ? [] : section(2, vector(importEntries));
  const functionPayload = vector(typeIndices.map((index) => u32(index)));
  const exports = functions.map(({ name }, index) => [
    ...wasmName(name),
    0x00,
    ...u32(importedFunctionCount + index),
  ]);
  const memorySection = standaloneExportKeys === null ? [] : section(5, [0x01, 0x00, 0x01]);
  const standaloneGlobals = standaloneExportKeys === null ? [] : [
    { name: '__data_end', body: [I32, 0x00, 0x41, 0x00, 0x0b] },
    { name: '__heap_base', body: [I32, 0x00, 0x41, 0x00, 0x0b] },
  ];
  const globalSection = standaloneGlobals.length === 0
    ? []
    : section(6, vector(standaloneGlobals.map(({ body }) => body)));
  if (standaloneExportKeys !== null) {
    exports.push([...wasmName('memory'), 0x02, ...u32(importedMemoryCount)]);
    standaloneGlobals.forEach(({ name }, index) => {
      exports.push([...wasmName(name), 0x03, ...u32(importedGlobalCount + index)]);
    });
  }
  const exportPayload = vector(exports);
  const codePayload = vector(functions.map(({ body }) => [
    ...u32(body.length + 1),
    0x00,
    ...body,
  ]));
  const customSection = customSectionText === null
    ? []
    : section(0, wasmName(customSectionText));
  return Buffer.from([
    0x00, 0x61, 0x73, 0x6d,
    0x01, 0x00, 0x00, 0x00,
    ...customSection,
    ...section(1, typePayload),
    ...importSection,
    ...section(3, functionPayload),
    ...memorySection,
    ...globalSection,
    ...section(7, exportPayload),
    ...section(10, codePayload),
  ]);
}

export function bindgenGlueFixture(exportKeys, { malformedKey = null, extraKeys = [] } = {}) {
  const wrappers = [...exportKeys, ...extraKeys].map((key) => {
    if (key === malformedKey) {
      return `export async function ${key}(input) { return input; }`;
    }
    return `
export function ${key}(input) {
  const ptr0 = passArray8ToWasm0(input, wasm.__wbindgen_malloc);
  const len0 = WASM_VECTOR_LEN;
  const ret = wasm.${key}(ptr0, len0);
  const result = getArrayU8FromWasm0(ret[0], ret[1]).slice();
  wasm.__wbindgen_free(ret[0], ret[1] * 1, 1);
  return result;
}`;
  }).join('\n');
  return Buffer.from(`
let wasm;
let WASM_VECTOR_LEN = 0;
function passArray8ToWasm0() { return 0; }
function getArrayU8FromWasm0() { return new Uint8Array(); }
${wrappers}
export function __voDispose() {}
export function __voInit() {}
export function initSync() { return wasm; }
export default function init() { return wasm; }
`, 'utf8');
}
