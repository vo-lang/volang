#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import {
  lstatSync,
  readFileSync,
  realpathSync,
} from 'node:fs';
import path from 'node:path';
import {
  canonicalGitRepositoryRoot,
  cleanGitEnvironment,
} from './repo_roots.mjs';
import { parseGitFileList } from './source_bound_evidence.mjs';
import { portableCaseKey } from './portable_path_key.mjs';
import {
  UNICODE_ALNUM_RANGES,
  UNICODE_ALPHABETIC_RANGES,
  UNICODE_CONTROL_RANGES,
  UNICODE_WHITE_SPACE_RANGES,
} from './unicode_casefold_data.mjs';

export const WASM_EXTENSION_PROTOCOL_EXPORT = 'vo_ext_protocol_version';
export const WASM_EXTENSION_PROTOCOL_VERSION = 3;
export const WASM_EXTENSION_EXPORT_PREFIX = '__vo_ext_';
export const MAX_PROTOCOL_WASM_BYTES = 256 * 1024 * 1024;
export const MAX_PROTOCOL_JS_BYTES = 35_000_000;

const UTF8 = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });
const SIMPLE_VALUE_TYPES = new Set([
  0x7f, // i32
  0x7e, // i64
  0x7d, // f32
  0x7c, // f64
  0x7b, // v128
  0x70, // funcref
  0x6f, // externref
]);
const I32 = 0x7f;
const F64 = 0x7c;
const MAX_VO_SOURCE_FILES = 4096;
const MAX_VO_SOURCE_BYTES = 64 * 1024 * 1024;
const MAX_SOURCE_DEPTH = 64;
const MAX_CANONICAL_MODULE_OWNER_BYTES = 255;
const MAX_CANONICAL_PACKAGE_PATH_BYTES = 4 * 1024;
const MAX_PORTABLE_PACKAGE_COMPONENT_BYTES = 255;
const WINDOWS_DEVICE_STEMS = new Set(['con', 'prn', 'aux', 'nul', 'conin$', 'conout$']);
const VO_KEYWORDS = new Set([
  'break', 'case', 'chan', 'const', 'continue', 'default', 'defer', 'else', 'errdefer',
  'fail', 'fallthrough', 'for', 'func', 'go', 'goto', 'if', 'import', 'interface',
  'island', 'map', 'package', 'port', 'range', 'return', 'select', 'struct', 'switch',
  'type', 'var',
]);
const BINDGEN_STANDARD_EXPORTS = Object.freeze([
  '__voDispose',
  '__voInit',
  'default',
  'initSync',
].sort(compareUtf8));
const STANDALONE_LINKER_EXPORTS = Object.freeze([
  Object.freeze({ name: '__data_end', kind: 0x03 }),
  Object.freeze({ name: '__heap_base', kind: 0x03 }),
]);

function frozenFunctionImport(module, name, parameters = [], results = []) {
  return Object.freeze({
    module,
    name,
    parameters: Object.freeze(parameters),
    results: Object.freeze(results),
  });
}

function frozenHostImport(name, parameters = [], results = []) {
  return frozenFunctionImport('env', name, parameters, results);
}

/** Trusted protocol-v3 host ABI implemented by Studio for Vogui standalone WASM. */
export const VOGUI_STANDALONE_HOST_IMPORTS_V3 = Object.freeze([
  frozenHostImport('host_start_timeout', [I32, I32]),
  frozenHostImport('host_clear_timeout', [I32]),
  frozenHostImport('host_start_interval', [I32, I32]),
  frozenHostImport('host_clear_interval', [I32]),
  frozenHostImport('host_has_host_capability', [I32, I32], [I32]),
  frozenHostImport('host_navigate', [I32, I32]),
  frozenHostImport('host_get_current_path', [I32], [I32]),
  frozenHostImport('host_set_title', [I32, I32]),
  frozenHostImport('host_set_meta', [I32, I32, I32, I32]),
  frozenHostImport('host_toast', [I32, I32, I32, I32, I32]),
  frozenHostImport('host_start_anim_frame', [I32]),
  frozenHostImport('host_cancel_anim_frame', [I32]),
  frozenHostImport('host_start_game_loop', [I32]),
  frozenHostImport('host_stop_game_loop', [I32]),
  frozenHostImport('host_measure_text', [I32, I32, I32, I32, F64, F64, I32, I32], [I32]),
  frozenHostImport('host_measure_text_lines', [I32, I32, I32, I32, F64, F64, I32, I32], [I32]),
  frozenHostImport('host_audio_load_bytes', [I32, I32], [I32]),
  frozenHostImport('host_audio_free', [I32]),
  frozenHostImport('host_audio_play_sound', [I32, F64, F64]),
  frozenHostImport('host_audio_set_listener', [F64, F64, F64, F64, F64, F64, F64, F64, F64]),
  frozenHostImport('host_audio_play_sound_3d', [I32, F64, F64, F64, F64, F64, F64]),
  frozenHostImport('host_audio_create_source_3d', [I32, F64, F64, F64, F64, F64, F64], [I32]),
  frozenHostImport('host_audio_update_spatial'),
  frozenHostImport('host_audio_set_source_3d_pos', [I32, F64, F64, F64]),
  frozenHostImport('host_audio_set_source_3d_params', [I32, F64, F64]),
  frozenHostImport('host_audio_remove_source_3d', [I32]),
  frozenHostImport('host_audio_play_music', [I32, F64]),
  frozenHostImport('host_audio_stop_music'),
  frozenHostImport('host_audio_pause_music'),
  frozenHostImport('host_audio_resume_music'),
  frozenHostImport('host_audio_set_sfx_volume', [F64]),
  frozenHostImport('host_audio_set_music_volume', [F64]),
]);

function errorDetail(error) {
  return String(error?.stderr || error?.message || error).trim();
}

function boundedBytes(value, label, maxBytes) {
  if (!(value instanceof Uint8Array)) {
    throw new Error(`${label} bytes must be a Uint8Array`);
  }
  if (
    !Number.isSafeInteger(maxBytes)
    || maxBytes <= 0
    || maxBytes > MAX_PROTOCOL_WASM_BYTES
  ) {
    throw new Error(`${label} has an invalid protocol byte limit`);
  }
  if (value.byteLength === 0 || value.byteLength > maxBytes) {
    throw new Error(`${label} must contain 1..${maxBytes} bytes`);
  }
  return Buffer.from(value.buffer, value.byteOffset, value.byteLength);
}

function boundedUtf8(value, label, maxBytes) {
  if (!(value instanceof Uint8Array)) {
    throw new Error(`${label} bytes must be a Uint8Array`);
  }
  if (!Number.isSafeInteger(maxBytes) || maxBytes <= 0 || value.byteLength > maxBytes) {
    throw new Error(`${label} exceeds its ${maxBytes}-byte limit`);
  }
  try {
    return UTF8.decode(value);
  } catch (error) {
    throw new Error(`${label} is not valid UTF-8: ${errorDetail(error)}`);
  }
}

function compareUtf8(left, right) {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function sortedUnique(values, label) {
  const sorted = [...values].sort(compareUtf8);
  for (let index = 1; index < sorted.length; index += 1) {
    if (sorted[index - 1] === sorted[index]) {
      throw new Error(`${label} contains duplicate ${sorted[index]}`);
    }
  }
  return sorted;
}

function isScalarString(value) {
  if (typeof value !== 'string') return false;
  for (let index = 0; index < value.length; index += 1) {
    const unit = value.charCodeAt(index);
    if (unit >= 0xd800 && unit <= 0xdbff) {
      const next = value.charCodeAt(index + 1);
      if (!(next >= 0xdc00 && next <= 0xdfff)) return false;
      index += 1;
    } else if (unit >= 0xdc00 && unit <= 0xdfff) return false;
  }
  return true;
}

function codePointInRanges(codePoint, ranges) {
  let low = 0;
  let high = ranges.length / 2;
  while (low < high) {
    const middle = low + Math.floor((high - low) / 2);
    const start = ranges[middle * 2];
    const end = ranges[middle * 2 + 1];
    if (codePoint < start) high = middle;
    else if (codePoint > end) low = middle + 1;
    else return true;
  }
  return false;
}

function hasUnicodeProperty(character, ranges) {
  return codePointInRanges(character.codePointAt(0), ranges);
}

function hasUnicodeWhiteSpaceBoundary(value) {
  const characters = [...value];
  return characters.length > 0
    && (
      hasUnicodeProperty(characters[0], UNICODE_WHITE_SPACE_RANGES)
      || hasUnicodeProperty(characters.at(-1), UNICODE_WHITE_SPACE_RANGES)
    );
}

function containsUnicodeControl(value) {
  return [...value].some((character) => (
    hasUnicodeProperty(character, UNICODE_CONTROL_RANGES)
  ));
}

function isWindowsDeviceStem(value) {
  const stem = portableCaseKey(value.split('.')[0]);
  return WINDOWS_DEVICE_STEMS.has(stem)
    || /^(?:com|lpt)(?:[1-9]|[¹²³])$/u.test(stem);
}

function validModuleSegment(value, index) {
  return Buffer.byteLength(value, 'utf8') <= MAX_CANONICAL_MODULE_OWNER_BYTES
    && /^[a-z0-9][a-z0-9._-]*$/u.test(value)
    && !value.endsWith('.')
    && !isWindowsDeviceStem(value)
    && (index < 3 || (!value.includes('..') && !value.endsWith('.lock')));
}

function validModulePath(value) {
  if (
    typeof value !== 'string'
    || value.length === 0
    || value !== value.normalize('NFC')
    || Buffer.byteLength(value, 'utf8') > MAX_CANONICAL_MODULE_OWNER_BYTES
    || !value.startsWith('github.com/')
    || value.startsWith('/')
    || value.endsWith('/')
  ) return false;
  const segments = value.split('/');
  if (segments.length < 3 || !segments.every(validModuleSegment)) return false;
  if (segments.length > 3) {
    const major = /^v([0-9]+)$/u.exec(segments.at(-1));
    if (major) {
      const digits = major[1];
      if (
        digits === '0'
        || digits === '1'
        || (digits.length > 1 && digits.startsWith('0'))
        || digits.length > 20
      ) return false;
      try {
        if (BigInt(digits) > 0xffff_ffff_ffff_ffffn) return false;
      } catch {
        return false;
      }
    }
  }
  return true;
}

function validPortablePackageComponent(value) {
  if (
    value.length === 0
    || value === '.'
    || value === '..'
    || value !== value.normalize('NFC')
    || Buffer.byteLength(value, 'utf8') > MAX_PORTABLE_PACKAGE_COMPONENT_BYTES
    || hasUnicodeWhiteSpaceBoundary(value)
    || value.endsWith('.')
    || value.includes('\\')
    || value.includes('@')
    || [...value].some((character) => '<>:"|?*'.includes(character))
    || containsUnicodeControl(value)
  ) return false;
  return !isWindowsDeviceStem(value);
}

function validCanonicalPackagePath(value) {
  if (
    typeof value !== 'string'
    || !isScalarString(value)
    || value.length === 0
    || Buffer.byteLength(value, 'utf8') > MAX_CANONICAL_PACKAGE_PATH_BYTES
    || value !== value.normalize('NFC')
  ) return false;
  const components = value.split('/');
  if (!components.every(validPortablePackageComponent)) return false;
  if (components[0] === 'local') {
    return components.length === 2
      && /^[a-z0-9][a-z0-9._-]*$/u.test(components[1]);
  }
  if (components[0]?.includes('.')) {
    return components.length >= 3 && validModulePath(components.slice(0, 3).join('/'));
  }
  return components[0] !== 'std';
}

function validLocalPackage(value) {
  return typeof value === 'string'
    && value.length > 0
    && value.split('/').every((part) => /^[A-Za-z_][A-Za-z0-9_]*$/u.test(part));
}

function validFunctionName(value) {
  if (
    !isScalarString(value)
    || value === '_'
    || VO_KEYWORDS.has(value)
  ) return false;
  const characters = [...value];
  if (characters.length === 0) return false;
  const first = characters[0];
  if (
    first !== '_'
    && !hasUnicodeProperty(first, UNICODE_ALPHABETIC_RANGES)
  ) return false;
  return characters.slice(1).every((character) => (
    character === '_'
    || hasUnicodeProperty(character, UNICODE_ALNUM_RANGES)
  ));
}

export function canonicalExternName(packagePath, functionName) {
  if (!validCanonicalPackagePath(packagePath)) {
    throw new Error(`invalid canonical extern package ${String(packagePath)}`);
  }
  if (!validFunctionName(functionName)) {
    throw new Error(`invalid canonical extern function ${String(functionName)}`);
  }
  const packageBytes = Buffer.byteLength(packagePath, 'utf8');
  const functionBytes = Buffer.byteLength(functionName, 'utf8');
  const canonical = `vo1:${packageBytes}:${packagePath}:${functionBytes}:${functionName}`;
  if (Buffer.byteLength(canonical, 'utf8') > 4096) {
    throw new Error('canonical extern name exceeds the 4096-byte protocol limit');
  }
  return canonical;
}

export function wasmExtensionExportKey(canonicalName) {
  if (typeof canonicalName !== 'string' || !canonicalName.startsWith('vo1:')) {
    throw new Error('canonical extern name is invalid');
  }
  return `${WASM_EXTENSION_EXPORT_PREFIX}${Buffer.from(canonicalName, 'utf8').toString('hex')}`;
}

function canonicalPackage(modulePath, extensionName, localPackage) {
  if (!validModulePath(modulePath)) throw new Error(`invalid extension module ${modulePath}`);
  if (!validLocalPackage(extensionName)) throw new Error(`invalid extension name ${extensionName}`);
  if (!validLocalPackage(localPackage)) throw new Error(`invalid local package ${localPackage}`);
  if (localPackage === extensionName) return modulePath;
  if (localPackage.startsWith(`${extensionName}/`)) {
    return `${modulePath}${localPackage.slice(extensionName.length)}`;
  }
  throw new Error(
    `extension entry package ${localPackage} escapes owner ${extensionName} (${modulePath})`,
  );
}

function stripVoComments(source) {
  let result = '';
  let state = 'code';
  let blockDepth = 0;
  for (let index = 0; index < source.length; index += 1) {
    const char = source[index];
    const next = source[index + 1];
    if (state === 'line-comment') {
      if (char === '\n') {
        state = 'code';
        result += '\n';
      } else result += ' ';
      continue;
    }
    if (state === 'block-comment') {
      if (char === '/' && next === '*') {
        blockDepth += 1;
        result += '  ';
        index += 1;
      } else if (char === '*' && next === '/') {
        blockDepth -= 1;
        result += '  ';
        index += 1;
        if (blockDepth === 0) state = 'code';
      } else result += char === '\n' ? '\n' : ' ';
      continue;
    }
    if (state === 'string') {
      if (char === '\\') {
        result += ' ';
        if (index + 1 < source.length) {
          result += source[index + 1] === '\n' ? '\n' : ' ';
          index += 1;
        }
      } else {
        result += char === '\n' ? '\n' : ' ';
        if (char === '"') state = 'code';
      }
      continue;
    }
    if (state === 'raw-string') {
      result += char === '\n' ? '\n' : ' ';
      if (char === '`') state = 'code';
      continue;
    }
    if (state === 'rune') {
      if (char === '\\') {
        result += ' ';
        if (index + 1 < source.length) {
          result += source[index + 1] === '\n' ? '\n' : ' ';
          index += 1;
        }
      } else {
        result += char === '\n' ? '\n' : ' ';
        if (char === "'") state = 'code';
      }
      continue;
    }
    if (char === '/' && next === '/') {
      state = 'line-comment';
      result += '  ';
      index += 1;
    } else if (char === '/' && next === '*') {
      state = 'block-comment';
      blockDepth = 1;
      result += '  ';
      index += 1;
    } else {
      if (char === '"') {
        result += ' ';
        state = 'string';
      } else if (char === '`') {
        result += ' ';
        state = 'raw-string';
      } else if (char === "'") {
        result += ' ';
        state = 'rune';
      } else result += char;
    }
  }
  if (state === 'block-comment') throw new Error('Vo source contains an unterminated block comment');
  if (state === 'string') throw new Error('Vo source contains an unterminated string literal');
  if (state === 'raw-string') throw new Error('Vo source contains an unterminated raw string literal');
  if (state === 'rune') throw new Error('Vo source contains an unterminated rune literal');
  return result;
}

function bodylessVoFunctions(bytes, label) {
  const source = stripVoComments(boundedUtf8(bytes, label, MAX_VO_SOURCE_BYTES));
  const functions = [];
  const lines = source.split(/\r?\n/u);
  let declaration = null;
  let parentheses = 0;
  let sawParameters = false;
  for (const line of lines) {
    if (declaration === null) {
      const match = line.match(/^[ \t\r]*func[ \t]+([^ \t\r(]+)[ \t]*\(/u);
      if (!match || !validFunctionName(match[1])) continue;
      declaration = { name: match[1], text: line };
    } else {
      declaration.text += `\n${line}`;
    }
    for (const char of line) {
      if (char === '(') {
        parentheses += 1;
        sawParameters = true;
      } else if (char === ')') parentheses -= 1;
      if (parentheses < 0) throw new Error(`${label} has an unbalanced function declaration`);
    }
    if (sawParameters && parentheses === 0) {
      if (!declaration.text.includes('{')) functions.push(declaration.name);
      declaration = null;
      sawParameters = false;
    }
  }
  if (declaration !== null) throw new Error(`${label} has an unterminated function declaration`);
  return functions;
}

/** Derive the exact browser wrapper list from authenticated Vo source bytes. */
export function extensionExportCatalogFromVoFiles(modulePath, files) {
  if (!validModulePath(modulePath)) throw new Error(`invalid extension module ${modulePath}`);
  const canonicalNames = [];
  let sourceFiles = 0;
  let sourceBytes = 0;
  for (const [relative, bytes] of files) {
    if (!relative.endsWith('.vo')) continue;
    sourceFiles += 1;
    sourceBytes += bytes.byteLength;
    if (sourceFiles > MAX_VO_SOURCE_FILES || sourceBytes > MAX_VO_SOURCE_BYTES) {
      throw new Error(`${modulePath} Vo extern catalog exceeds its source budget`);
    }
    const directory = path.posix.dirname(relative);
    const packagePath = directory === '.' ? modulePath : `${modulePath}/${directory}`;
    for (const functionName of bodylessVoFunctions(bytes, `${modulePath} ${relative}`)) {
      canonicalNames.push(canonicalExternName(packagePath, functionName));
    }
  }
  const names = sortedUnique(canonicalNames, `${modulePath} Vo extern catalog`);
  return Object.freeze({
    canonicalNames: Object.freeze(names),
    exportKeys: Object.freeze(names.map(wasmExtensionExportKey).sort(compareUtf8)),
  });
}

function rustTokens(source, label) {
  const tokens = [];
  for (let index = 0; index < source.length;) {
    const char = source[index];
    const next = source[index + 1];
    if (/\s/u.test(char)) {
      index += 1;
      continue;
    }
    if (char === '/' && next === '/') {
      index += 2;
      while (index < source.length && source[index] !== '\n') index += 1;
      continue;
    }
    if (char === '/' && next === '*') {
      index += 2;
      let depth = 1;
      while (index < source.length && depth > 0) {
        if (source[index] === '/' && source[index + 1] === '*') {
          depth += 1;
          index += 2;
        } else if (source[index] === '*' && source[index + 1] === '/') {
          depth -= 1;
          index += 2;
        } else index += 1;
      }
      if (depth !== 0) throw new Error(`${label} contains an unterminated block comment`);
      continue;
    }
    if (char === '"') {
      const start = index;
      index += 1;
      while (index < source.length) {
        if (source[index] === '\\') index += 2;
        else if (source[index++] === '"') break;
      }
      const raw = source.slice(start, index);
      try {
        tokens.push({ kind: 'string', value: JSON.parse(raw) });
      } catch (error) {
        throw new Error(`${label} contains an unsupported Rust string: ${errorDetail(error)}`);
      }
      continue;
    }
    const identifier = source.slice(index).match(/^[A-Za-z_][A-Za-z0-9_]*/u)?.[0];
    if (identifier) {
      tokens.push({ kind: 'identifier', value: identifier });
      index += identifier.length;
      continue;
    }
    tokens.push({ kind: 'punctuation', value: char });
    index += 1;
  }
  return tokens;
}

function rustAttributeOpening(tokens, closing, label) {
  let depth = 0;
  for (let index = closing; index >= 0; index -= 1) {
    if (tokens[index].value === ']') depth += 1;
    else if (tokens[index].value === '[') {
      depth -= 1;
      if (depth === 0) {
        if (tokens[index - 1]?.value !== '#') {
          throw new Error(`${label} import-module attribute is missing its # marker`);
        }
        return index;
      }
    }
  }
  throw new Error(`${label} import-module attribute has unbalanced brackets`);
}

function rustAttributeStringAssignment(tokens, name, label) {
  const values = [];
  for (let index = 0; index + 2 < tokens.length; index += 1) {
    if (
      tokens[index].kind === 'identifier'
      && tokens[index].value === name
      && tokens[index + 1].value === '='
      && tokens[index + 2].kind === 'string'
    ) values.push(tokens[index + 2].value);
  }
  if (values.length !== 1) {
    throw new Error(`${label} import-module attribute must assign ${name} exactly once`);
  }
  return values[0];
}

function rustWasmAbiType(tokens, start, label) {
  const token = tokens[start];
  if (token?.kind === 'identifier') {
    if (token.value === 'i32' || token.value === 'u32') return { type: I32, next: start + 1 };
    if (token.value === 'f64') return { type: F64, next: start + 1 };
  }
  if (
    token?.value === '*'
    && ['const', 'mut'].includes(tokens[start + 1]?.value)
    && ['u8', 'u32'].includes(tokens[start + 2]?.value)
  ) return { type: I32, next: start + 3 };
  throw new Error(`${label} contains an unsupported Rust host ABI type`);
}

function rustHostExternBlock(tokens, externIndex, label) {
  if (tokens[externIndex - 1]?.value !== ']') {
    throw new Error(`${label} extern block must immediately follow its cfg_attr import-module attribute`);
  }
  const attributeOpen = rustAttributeOpening(tokens, externIndex - 1, label);
  const attribute = tokens.slice(attributeOpen + 1, externIndex - 1);
  if (
    attribute[0]?.value !== 'cfg_attr'
    || !attribute.some((token) => token.kind === 'identifier' && token.value === 'link')
    || rustAttributeStringAssignment(attribute, 'target_arch', label) !== 'wasm32'
  ) {
    throw new Error(`${label} host extern block must use cfg_attr(target_arch = "wasm32", link(...))`);
  }
  const module = rustAttributeStringAssignment(attribute, 'wasm_import_module', label);
  if (module !== 'env') throw new Error(`${label} host extern block must import from module env`);

  let index = externIndex + 3;
  const imports = [];
  const names = new Set();
  while (tokens[index]?.value !== '}') {
    if (tokens[index]?.kind !== 'identifier' || tokens[index].value !== 'fn') {
      throw new Error(`${label} host extern block may contain function declarations only`);
    }
    const name = tokens[index + 1];
    if (name?.kind !== 'identifier' || !/^host_[a-z0-9_]+$/u.test(name.value)) {
      throw new Error(`${label} contains an invalid host function name`);
    }
    if (names.has(name.value)) throw new Error(`${label} contains duplicate host function ${name.value}`);
    names.add(name.value);
    if (tokens[index + 2]?.value !== '(') {
      throw new Error(`${label} host function ${name.value} is missing its parameter list`);
    }
    index += 3;
    const parameters = [];
    while (tokens[index]?.value !== ')') {
      if (tokens[index]?.kind !== 'identifier' || tokens[index + 1]?.value !== ':') {
        throw new Error(`${label} host function ${name.value} has a malformed parameter`);
      }
      const parsed = rustWasmAbiType(tokens, index + 2, `${label} host function ${name.value}`);
      parameters.push(parsed.type);
      index = parsed.next;
      if (tokens[index]?.value === ',') index += 1;
      else if (tokens[index]?.value !== ')') {
        throw new Error(`${label} host function ${name.value} has a malformed parameter separator`);
      }
    }
    index += 1;
    const results = [];
    if (tokens[index]?.value === '-' && tokens[index + 1]?.value === '>') {
      const parsed = rustWasmAbiType(tokens, index + 2, `${label} host function ${name.value}`);
      results.push(parsed.type);
      index = parsed.next;
    }
    if (tokens[index]?.value !== ';') {
      throw new Error(`${label} host function ${name.value} must be a bodyless extern declaration`);
    }
    imports.push(frozenFunctionImport(module, name.value, parameters, results));
    index += 1;
  }
  if (imports.length === 0) throw new Error(`${label} host extern block is empty`);
  return { imports, end: index };
}

/** Derive the exact standalone WASM ABI from one canonical Rust env extern block. */
export function standaloneHostImportsFromRustSource(bytes, label = 'standalone Rust host ABI') {
  const source = boundedUtf8(bytes, label, MAX_VO_SOURCE_BYTES);
  const tokens = rustTokens(source, label);
  const candidates = [];
  for (let index = 0; index + 2 < tokens.length; index += 1) {
    if (
      tokens[index].kind === 'identifier'
      && tokens[index].value === 'extern'
      && tokens[index + 1].kind === 'string'
      && tokens[index + 1].value === 'C'
      && tokens[index + 2].value === '{'
      && tokens[index - 1]?.value === ']'
    ) candidates.push(rustHostExternBlock(tokens, index, label));
  }
  if (candidates.length !== 1) {
    throw new Error(`${label} must contain exactly one attributed extern "C" host block`);
  }
  return Object.freeze(candidates[0].imports);
}

/** Derive the native/runtime entry authority from `vo_extension_entry!` calls. */
export function extensionExportCatalogFromRustEntries(
  modulePath,
  extensionName,
  bytes,
  label = 'extension entry source',
) {
  const source = boundedUtf8(bytes, label, MAX_VO_SOURCE_BYTES);
  const tokens = rustTokens(source, label);
  const canonicalNames = [];
  for (let index = 0; index < tokens.length; index += 1) {
    if (
      tokens[index].kind !== 'identifier'
      || tokens[index].value !== 'vo_extension_entry'
      || tokens[index + 1]?.value !== '!'
      || tokens[index + 2]?.value !== '('
    ) continue;
    const strings = [];
    let depth = 1;
    index += 3;
    for (; index < tokens.length && depth > 0; index += 1) {
      if (tokens[index].value === '(') depth += 1;
      else if (tokens[index].value === ')') depth -= 1;
      else if (depth === 1 && tokens[index].kind === 'string') strings.push(tokens[index].value);
    }
    index -= 1;
    if (depth !== 0 || strings.length !== 2) {
      throw new Error(`${label} contains a malformed vo_extension_entry! invocation`);
    }
    const [localPackage, functionName] = strings;
    canonicalNames.push(canonicalExternName(
      canonicalPackage(modulePath, extensionName, localPackage),
      functionName,
    ));
  }
  const names = sortedUnique(canonicalNames, `${label} extern catalog`);
  if (names.length === 0) throw new Error(`${label} contains no vo_extension_entry! invocations`);
  return Object.freeze({
    canonicalNames: Object.freeze(names),
    exportKeys: Object.freeze(names.map(wasmExtensionExportKey).sort(compareUtf8)),
  });
}

function gitSourceFileList(root, arguments_, label) {
  const result = spawnSync(
    'git',
    ['-C', root, 'ls-files', '-z', ...arguments_],
    {
      encoding: 'buffer',
      env: cleanGitEnvironment(),
      maxBuffer: 32 * 1024 * 1024,
      timeout: 30_000,
      windowsHide: true,
    },
  );
  if (result.error || result.status !== 0) {
    throw new Error(`could not enumerate ${label} in ${root}: ${errorDetail(result.error || result.stderr)}`);
  }
  const files = parseGitFileList(result.stdout, root);
  for (const relative of files) {
    if (
      relative.length === 0
      || relative !== relative.normalize('NFC')
      || relative.startsWith('/')
      || relative.endsWith('/')
      || relative.includes('\\')
      || relative.includes('\0')
      || relative.split('/').some((component) => (
        component === '' || component === '.' || component === '..'
      ))
    ) {
      throw new Error(`Git source closure contains a non-canonical relative path: ${JSON.stringify(relative)}`);
    }
  }
  return files;
}

function currentGitSourceFiles(root) {
  const files = gitSourceFileList(
    root,
    ['--cached', '--others', '--exclude-standard'],
    'Git source files',
  );
  const ignoredCompilerInputs = gitSourceFileList(
    root,
    [
      '--others',
      '--ignored',
      '--exclude-standard',
      '--',
      ':(glob)*.vo',
      ':(glob)**/*.vo',
      ':(glob)vo.mod',
      ':(glob)**/vo.mod',
    ],
    'ignored Vo compiler inputs',
  );
  if (ignoredCompilerInputs.length > 0) {
    throw new Error(
      `Git-ignored Vo compiler inputs are outside the authenticated source closure: ${ignoredCompilerInputs.join(', ')}`,
    );
  }
  return files;
}

function assertStableRegularSourceFile(root, relative, maxBytes, label) {
  const absolute = path.join(root, ...relative.split('/'));
  const before = lstatSync(absolute, { bigint: true });
  if (!before.isFile() || before.isSymbolicLink()) {
    throw new Error(`${label} must be a regular file without symbolic links`);
  }
  let resolved;
  try {
    resolved = realpathSync.native(absolute);
  } catch (error) {
    throw new Error(`${label} cannot be resolved: ${errorDetail(error)}`);
  }
  if (resolved !== absolute) throw new Error(`${label} reaches its source through a path alias`);
  if (before.size > BigInt(maxBytes)) throw new Error(`${label} exceeds its ${maxBytes}-byte limit`);
  const bytes = readFileSync(absolute);
  const after = lstatSync(absolute, { bigint: true });
  for (const field of ['dev', 'ino', 'mode', 'nlink', 'size', 'mtimeNs', 'ctimeNs']) {
    if (before[field] !== after[field]) throw new Error(`${label} changed while it was read`);
  }
  if (!after.isFile() || after.isSymbolicLink() || BigInt(bytes.byteLength) !== after.size) {
    throw new Error(`${label} changed kind or size while it was read`);
  }
  return bytes;
}

function collectVoFiles(root) {
  const canonicalRoot = canonicalGitRepositoryRoot(root, 'extension source root', {
    requireVoMod: true,
  });
  const sourceFilesBefore = currentGitSourceFiles(canonicalRoot);
  const nestedModuleRoots = [];
  for (const relative of sourceFilesBefore) {
    const basename = path.posix.basename(relative);
    if (basename.toLowerCase() === 'vo.mod' && basename !== 'vo.mod') {
      throw new Error(`extension source module boundary must use the exact basename vo.mod: ${relative}`);
    }
    if (relative !== 'vo.mod' && basename === 'vo.mod') {
      assertStableRegularSourceFile(
        canonicalRoot,
        relative,
        MAX_VO_SOURCE_BYTES,
        `nested module boundary ${relative}`,
      );
      nestedModuleRoots.push(`${path.posix.dirname(relative)}/`);
    }
  }

  const files = new Map();
  let totalBytes = 0;
  for (const relative of sourceFilesBefore) {
    if (!relative.endsWith('.vo')) continue;
    if (relative.split('/').length - 1 > MAX_SOURCE_DEPTH) {
      throw new Error(`Vo source path exceeds its depth limit: ${relative}`);
    }
    if (nestedModuleRoots.some((prefix) => relative.startsWith(prefix))) continue;
    if (files.size >= MAX_VO_SOURCE_FILES) throw new Error('Vo source tree has too many files');
    const bytes = assertStableRegularSourceFile(
      canonicalRoot,
      relative,
      MAX_VO_SOURCE_BYTES,
      `Vo source ${relative}`,
    );
    totalBytes += bytes.byteLength;
    if (totalBytes > MAX_VO_SOURCE_BYTES) throw new Error('Vo source tree exceeds its byte limit');
    files.set(relative, bytes);
  }
  const sourceFilesAfter = currentGitSourceFiles(canonicalRoot);
  if (JSON.stringify(sourceFilesAfter) !== JSON.stringify(sourceFilesBefore)) {
    throw new Error(`Git source file closure changed while the extern catalog was read: ${canonicalRoot}`);
  }
  return Object.freeze({
    canonicalRoot,
    files,
    sourceFiles: Object.freeze(sourceFilesBefore),
  });
}

/** Bind the language declarations to the final Rust extension-entry table. */
export function extensionExportCatalogFromDirectory(
  root,
  {
    modulePath,
    extensionName,
    rustEntrySource,
  },
) {
  const collected = collectVoFiles(root);
  const voCatalog = extensionExportCatalogFromVoFiles(modulePath, collected.files);
  if (
    typeof rustEntrySource !== 'string'
    || rustEntrySource.length === 0
    || path.posix.isAbsolute(rustEntrySource)
    || rustEntrySource.split('/').some((component) => component === '' || component === '.' || component === '..')
  ) throw new Error('extension Rust entry source must be a canonical relative path');
  if (!collected.sourceFiles.includes(rustEntrySource)) {
    throw new Error(`${modulePath} Rust entry source is outside the authenticated Git source closure: ${rustEntrySource}`);
  }
  const rustCatalog = extensionExportCatalogFromRustEntries(
    modulePath,
    extensionName,
    assertStableRegularSourceFile(
      collected.canonicalRoot,
      rustEntrySource,
      MAX_VO_SOURCE_BYTES,
      `${modulePath} ${rustEntrySource}`,
    ),
    `${modulePath} ${rustEntrySource}`,
  );
  if (JSON.stringify(voCatalog.canonicalNames) !== JSON.stringify(rustCatalog.canonicalNames)) {
    const voOnly = voCatalog.canonicalNames.filter((name) => !rustCatalog.canonicalNames.includes(name));
    const rustOnly = rustCatalog.canonicalNames.filter((name) => !voCatalog.canonicalNames.includes(name));
    throw new Error(
      `${modulePath} Vo extern declarations and Rust entry table differ`
      + ` (Vo-only: ${voOnly.join(', ') || '(none)'}; Rust-only: ${rustOnly.join(', ') || '(none)'})`,
    );
  }
  return voCatalog;
}

class WasmReader {
  constructor(bytes, start = 0, end = bytes.byteLength, label = 'WASM binary') {
    this.bytes = bytes;
    this.offset = start;
    this.end = end;
    this.label = label;
  }

  get remaining() {
    return this.end - this.offset;
  }

  readByte() {
    if (this.offset >= this.end) throw new Error(`${this.label} ended unexpectedly`);
    return this.bytes[this.offset++];
  }

  readBytes(length) {
    if (!Number.isSafeInteger(length) || length < 0 || length > this.remaining) {
      throw new Error(`${this.label} contains an out-of-bounds byte range`);
    }
    const start = this.offset;
    this.offset += length;
    return this.bytes.subarray(start, this.offset);
  }

  readVarUint32() {
    let value = 0;
    for (let index = 0; index < 5; index += 1) {
      const byte = this.readByte();
      const payload = byte & 0x7f;
      if (index === 4 && payload > 0x0f) {
        throw new Error(`${this.label} contains an overflowing varuint32`);
      }
      value += payload * (2 ** (index * 7));
      if ((byte & 0x80) === 0) return value;
    }
    throw new Error(`${this.label} contains an unterminated varuint32`);
  }

  readVarUint64() {
    let value = 0n;
    for (let index = 0; index < 10; index += 1) {
      const byte = this.readByte();
      const payload = BigInt(byte & 0x7f);
      if (index === 9 && payload > 1n) throw new Error(`${this.label} contains an overflowing varuint64`);
      value |= payload << BigInt(index * 7);
      if ((byte & 0x80) === 0) return value;
    }
    throw new Error(`${this.label} contains an unterminated varuint64`);
  }

  readName() {
    const length = this.readVarUint32();
    try {
      return UTF8.decode(this.readBytes(length));
    } catch (error) {
      throw new Error(`${this.label} contains an invalid UTF-8 name: ${errorDetail(error)}`);
    }
  }

  subsection(length, label) {
    if (!Number.isSafeInteger(length) || length < 0 || length > this.remaining) {
      throw new Error(`${this.label} contains an out-of-bounds section`);
    }
    const start = this.offset;
    this.offset += length;
    return new WasmReader(this.bytes, start, start + length, label);
  }

  assertEnd() {
    if (this.remaining !== 0) throw new Error(`${this.label} has ${this.remaining} trailing byte(s)`);
  }
}

function readValueType(reader) {
  const valueType = reader.readByte();
  if (!SIMPLE_VALUE_TYPES.has(valueType)) {
    throw new Error(
      `${reader.label} uses unsupported value type 0x${valueType.toString(16).padStart(2, '0')}`,
    );
  }
  return valueType;
}

function readValueTypeVector(reader) {
  const count = reader.readVarUint32();
  if (count > reader.remaining) throw new Error(`${reader.label} has an impossible type vector`);
  return Array.from({ length: count }, () => readValueType(reader));
}

function parseTypeSection(reader) {
  const count = reader.readVarUint32();
  if (count > reader.remaining) throw new Error(`${reader.label} has an impossible type count`);
  const types = [];
  for (let index = 0; index < count; index += 1) {
    if (reader.readByte() !== 0x60) {
      throw new Error(`${reader.label} type ${index} is not a function type`);
    }
    types.push({
      parameters: readValueTypeVector(reader),
      results: readValueTypeVector(reader),
    });
  }
  reader.assertEnd();
  return types;
}

function readLimits(reader) {
  const flags = reader.readVarUint32();
  if ((flags & ~0x07) !== 0) throw new Error(`${reader.label} has unsupported limits flags`);
  const memory64 = (flags & 0x04) !== 0;
  if (memory64) reader.readVarUint64();
  else reader.readVarUint32();
  if ((flags & 0x01) !== 0) {
    if (memory64) reader.readVarUint64();
    else reader.readVarUint32();
  }
}

function parseImportSection(reader) {
  const count = reader.readVarUint32();
  const imports = [];
  const functionTypeIndices = [];
  for (let index = 0; index < count; index += 1) {
    const module = reader.readName();
    const name = reader.readName();
    const kind = reader.readByte();
    let typeIndex = null;
    if (kind === 0x00) {
      typeIndex = reader.readVarUint32();
      functionTypeIndices.push(typeIndex);
    } else if (kind === 0x01) {
      readValueType(reader);
      readLimits(reader);
    } else if (kind === 0x02) readLimits(reader);
    else if (kind === 0x03) {
      readValueType(reader);
      reader.readByte();
    } else if (kind === 0x04) {
      reader.readByte();
      typeIndex = reader.readVarUint32();
    } else throw new Error(`${reader.label} import ${index} has unknown kind ${kind}`);
    imports.push({ module, name, kind, typeIndex });
  }
  reader.assertEnd();
  return { functionTypeIndices, imports };
}

function parseFunctionSection(reader) {
  const count = reader.readVarUint32();
  if (count > reader.remaining) throw new Error(`${reader.label} has an impossible function count`);
  const result = Array.from({ length: count }, () => reader.readVarUint32());
  reader.assertEnd();
  return result;
}

function parseExportSection(reader) {
  const count = reader.readVarUint32();
  const exports = [];
  for (let index = 0; index < count; index += 1) {
    exports.push({
      name: reader.readName(),
      kind: reader.readByte(),
      index: reader.readVarUint32(),
    });
  }
  reader.assertEnd();
  return exports;
}

function parseCodeSection(reader, selectedDefinedIndex) {
  const count = reader.readVarUint32();
  let selectedBody = null;
  for (let index = 0; index < count; index += 1) {
    const size = reader.readVarUint32();
    const body = reader.readBytes(size);
    if (index === selectedDefinedIndex) selectedBody = body;
  }
  reader.assertEnd();
  return selectedBody;
}

function parseWasmStructure(bytes, label) {
  const reader = new WasmReader(bytes, 0, bytes.byteLength, label);
  const header = reader.readBytes(8);
  if (
    header[0] !== 0x00 || header[1] !== 0x61 || header[2] !== 0x73 || header[3] !== 0x6d
    || header[4] !== 0x01 || header[5] !== 0x00 || header[6] !== 0x00 || header[7] !== 0x00
  ) throw new Error(`${label} has an invalid WebAssembly header`);

  const structure = {
    types: [],
    imports: [],
    importedFunctionTypeIndices: [],
    definedFunctionTypeIndices: [],
    exports: [],
    protocolBody: null,
  };
  while (reader.remaining > 0) {
    const sectionId = reader.readByte();
    const size = reader.readVarUint32();
    const section = reader.subsection(size, `${label} section ${sectionId}`);
    if (sectionId === 1) structure.types = parseTypeSection(section);
    else if (sectionId === 2) {
      const imports = parseImportSection(section);
      structure.imports = imports.imports;
      structure.importedFunctionTypeIndices = imports.functionTypeIndices;
    }
    else if (sectionId === 3) structure.definedFunctionTypeIndices = parseFunctionSection(section);
    else if (sectionId === 7) structure.exports = parseExportSection(section);
    else if (sectionId === 10) {
      const protocol = structure.exports.find(
        ({ name, kind }) => name === WASM_EXTENSION_PROTOCOL_EXPORT && kind === 0x00,
      );
      const selectedDefinedIndex = protocol
        && protocol.index >= structure.importedFunctionTypeIndices.length
        ? protocol.index - structure.importedFunctionTypeIndices.length
        : -1;
      structure.protocolBody = parseCodeSection(section, selectedDefinedIndex);
    }
  }
  return structure;
}

function reflectedModule(bytes, label) {
  try {
    return new WebAssembly.Module(bytes);
  } catch (error) {
    throw new Error(`${label} is not a valid WebAssembly module: ${errorDetail(error)}`);
  }
}

function exportedFunction(structure, name, label) {
  const exported = structure.exports.find((entry) => entry.name === name);
  if (!exported) throw new Error(`${label} is missing ${name}`);
  if (exported.kind !== 0x00) throw new Error(`${label} ${name} export kind must be function`);
  const importedCount = structure.importedFunctionTypeIndices.length;
  if (exported.index < importedCount) throw new Error(`${label} ${name} must be defined by the module`);
  const definedIndex = exported.index - importedCount;
  const typeIndex = structure.definedFunctionTypeIndices[definedIndex];
  const type = structure.types[typeIndex];
  if (!type) throw new Error(`${label} ${name} has an unresolved function type`);
  return { definedIndex, exported, type };
}

function assertFunctionType(actual, parameters, results, label) {
  if (
    JSON.stringify(actual.parameters) !== JSON.stringify(parameters)
    || JSON.stringify(actual.results) !== JSON.stringify(results)
  ) {
    throw new Error(`${label} has an invalid WebAssembly function signature`);
  }
}

function assertPureProtocolVersionBody(structure, definedIndex, label) {
  const protocol = structure.exports.find(
    ({ name, kind }) => name === WASM_EXTENSION_PROTOCOL_EXPORT && kind === 0x00,
  );
  const expectedDefinedIndex = protocol.index - structure.importedFunctionTypeIndices.length;
  if (definedIndex !== expectedDefinedIndex) throw new Error(`${label} body index is inconsistent`);
  const body = structure.protocolBody;
  if (!body) throw new Error(`${label} has no function body`);
  const reader = new WasmReader(body, 0, body.byteLength, `${label} body`);
  if (reader.readVarUint32() !== 0) throw new Error(`${label} must not declare local variables`);
  const instructions = reader.readBytes(reader.remaining);
  if (
    instructions.byteLength !== 3
    || instructions[0] !== 0x41 // i32.const
    || instructions[1] !== WASM_EXTENSION_PROTOCOL_VERSION
    || instructions[2] !== 0x0b // end
  ) {
    throw new Error(`${label} must be the pure body i32.const ${WASM_EXTENSION_PROTOCOL_VERSION}; end`);
  }
}

function inspectProtocolWasm(bytes, label, maxBytes) {
  const bounded = boundedBytes(bytes, label, maxBytes);
  const module = reflectedModule(bounded, label);
  const reflected = WebAssembly.Module.exports(module).find(
    ({ name }) => name === WASM_EXTENSION_PROTOCOL_EXPORT,
  );
  if (!reflected) throw new Error(`${label} is missing ${WASM_EXTENSION_PROTOCOL_EXPORT}`);
  if (reflected.kind !== 'function') {
    throw new Error(`${label} ${WASM_EXTENSION_PROTOCOL_EXPORT} export kind must be function`);
  }
  const structure = parseWasmStructure(bounded, label);
  const protocol = exportedFunction(structure, WASM_EXTENSION_PROTOCOL_EXPORT, label);
  assertFunctionType(
    protocol.type,
    [],
    [I32],
    `${label} ${WASM_EXTENSION_PROTOCOL_EXPORT}`,
  );
  assertPureProtocolVersionBody(
    structure,
    protocol.definedIndex,
    `${label} ${WASM_EXTENSION_PROTOCOL_EXPORT}`,
  );
  return { bounded, module, structure };
}

/** Strict static protocol check: export, type, and a side-effect-free constant-v3 body. */
export function assertWasmExtensionProtocolV3(
  bytes,
  { label = 'WASM extension', maxBytes = MAX_PROTOCOL_WASM_BYTES } = {},
) {
  inspectProtocolWasm(bytes, label, maxBytes);
  return Object.freeze({
    exportName: WASM_EXTENSION_PROTOCOL_EXPORT,
    version: WASM_EXTENSION_PROTOCOL_VERSION,
  });
}

function validateExpectedExportKeys(expectedExportKeys, label) {
  if (!Array.isArray(expectedExportKeys)) throw new Error(`${label} export catalog must be an array`);
  const keys = sortedUnique(expectedExportKeys, `${label} export catalog`);
  for (const key of keys) {
    if (!new RegExp(`^${WASM_EXTENSION_EXPORT_PREFIX}(?:[0-9a-f]{2})+$`, 'u').test(key)) {
      throw new Error(`${label} export catalog contains invalid key ${key}`);
    }
  }
  return keys;
}

function validateExpectedFunctionImports(expectedImports, label) {
  if (!Array.isArray(expectedImports)) {
    throw new Error(`${label} expected host imports must be an array from a trusted protocol authority`);
  }
  const byName = new Map();
  for (const entry of expectedImports) {
    if (
      entry === null
      || typeof entry !== 'object'
      || Array.isArray(entry)
      || JSON.stringify(Object.keys(entry).sort())
        !== JSON.stringify(['module', 'name', 'parameters', 'results'])
      || typeof entry.module !== 'string'
      || entry.module.length === 0
      || typeof entry.name !== 'string'
      || entry.name.length === 0
      || !Array.isArray(entry.parameters)
      || !Array.isArray(entry.results)
      || [...entry.parameters, ...entry.results].some((valueType) => !SIMPLE_VALUE_TYPES.has(valueType))
    ) {
      throw new Error(`${label} expected host import ABI contains an invalid entry`);
    }
    if (byName.has(entry.name)) {
      throw new Error(`${label} expected host import ABI contains duplicate ${entry.name}`);
    }
    byName.set(entry.name, entry);
  }
  return byName;
}

function assertStandaloneHostImportPolicy(structure, expectedImports, label) {
  const expectedByName = validateExpectedFunctionImports(expectedImports, label);
  const importedNames = new Set();
  for (const entry of structure.imports) {
    if (importedNames.has(entry.name)) {
      throw new Error(`${label} contains duplicate host import ${entry.name}`);
    }
    importedNames.add(entry.name);
    if (entry.kind !== 0x00) {
      throw new Error(`${label} may import functions only; ${entry.module}.${entry.name} has kind ${entry.kind}`);
    }
    const expected = expectedByName.get(entry.name);
    if (!expected) {
      throw new Error(`${label} contains unexpected host import ${entry.module}.${entry.name}`);
    }
    if (entry.module !== expected.module) {
      throw new Error(
        `${label} host import ${entry.name} must be imported from module ${expected.module}`,
      );
    }
    const actualType = structure.types[entry.typeIndex];
    if (
      !actualType
      || JSON.stringify(actualType.parameters) !== JSON.stringify(expected.parameters)
      || JSON.stringify(actualType.results) !== JSON.stringify(expected.results)
    ) {
      throw new Error(`${label} host import ${entry.name} has an invalid WebAssembly function signature`);
    }
  }
  for (const name of expectedByName.keys()) {
    if (!importedNames.has(name)) throw new Error(`${label} is missing host import ${name}`);
  }
}

/** Validate the complete standalone C ABI, including its exact wrapper set. */
export function assertStandaloneWasmExtensionV3(
  bytes,
  {
    expectedExportKeys,
    expectedImports,
    label = 'standalone WASM extension',
    maxBytes = MAX_PROTOCOL_WASM_BYTES,
  },
) {
  const expected = validateExpectedExportKeys(expectedExportKeys, label);
  const { structure } = inspectProtocolWasm(bytes, label, maxBytes);
  assertStandaloneHostImportPolicy(structure, expectedImports, label);
  const expectedExports = [
    { name: WASM_EXTENSION_PROTOCOL_EXPORT, kind: 0x00 },
    { name: 'memory', kind: 0x02 },
    { name: 'vo_alloc', kind: 0x00 },
    { name: 'vo_dealloc', kind: 0x00 },
    ...STANDALONE_LINKER_EXPORTS,
    ...expected.map((name) => ({ name, kind: 0x00 })),
  ].sort((left, right) => compareUtf8(left.name, right.name));
  const actualExports = structure.exports
    .map(({ name, kind }) => ({ name, kind }))
    .sort((left, right) => compareUtf8(left.name, right.name));
  if (JSON.stringify(actualExports) !== JSON.stringify(expectedExports)) {
    const identity = ({ name, kind }) => `${name} (kind 0x${kind.toString(16).padStart(2, '0')})`;
    const expectedIdentities = new Set(expectedExports.map(identity));
    const actualIdentities = new Set(actualExports.map(identity));
    const missing = expectedExports.map(identity).filter((entry) => !actualIdentities.has(entry));
    const unexpected = actualExports.map(identity).filter((entry) => !expectedIdentities.has(entry));
    throw new Error(
      `${label} exact standalone export allowlist differs from its Vo extern catalog`
      + ` (missing: ${missing.join(', ') || '(none)'}; unexpected: ${unexpected.join(', ') || '(none)'})`,
    );
  }
  assertFunctionType(exportedFunction(structure, 'vo_alloc', label).type, [I32], [I32], `${label} vo_alloc`);
  assertFunctionType(
    exportedFunction(structure, 'vo_dealloc', label).type,
    [I32, I32],
    [],
    `${label} vo_dealloc`,
  );
  for (const key of expected) {
    assertFunctionType(
      exportedFunction(structure, key, label).type,
      [I32, I32, I32],
      [I32],
      `${label} ${key}`,
    );
  }
  return Object.freeze({ exportKeys: Object.freeze(expected), version: 3 });
}

const JS_EXPORT_PARSER = String.raw`
import { readFileSync } from 'node:fs';
import vm from 'node:vm';
const source = readFileSync(0, 'utf8');
const module = new vm.SourceTextModule(source, { identifier: 'extension-glue.mjs' });
await module.link((specifier) => { throw new Error('unexpected module import: ' + specifier); });
const names = Reflect.ownKeys(module.namespace).filter((name) => typeof name === 'string');
const bindingTypes = Object.create(null);
for (const name of names) {
  try {
    bindingTypes[name] = typeof module.namespace[name];
  } catch {
    bindingTypes[name] = 'uninitialized';
  }
}
process.stdout.write(JSON.stringify({ names, bindingTypes }));
`;

function parsedJavaScriptExports(source, label) {
  const result = spawnSync(process.execPath, [
    '--max-old-space-size=128',
    '--experimental-vm-modules',
    '--no-warnings',
    '--input-type=module',
    '--eval',
    JS_EXPORT_PARSER,
  ], {
    encoding: 'utf8',
    input: source,
    maxBuffer: 8 * 1024 * 1024,
    timeout: 30_000,
    windowsHide: true,
  });
  if (result.error || result.status !== 0) {
    throw new Error(`${label} cannot be parsed as a self-contained ESM module: ${errorDetail(result.error || result.stderr)}`);
  }
  try {
    const parsed = JSON.parse(result.stdout);
    if (
      parsed === null
      || typeof parsed !== 'object'
      || Array.isArray(parsed)
      || !Array.isArray(parsed.names)
      || parsed.names.some((name) => typeof name !== 'string')
      || parsed.bindingTypes === null
      || typeof parsed.bindingTypes !== 'object'
      || Array.isArray(parsed.bindingTypes)
      || JSON.stringify(Object.keys(parsed.bindingTypes).sort(compareUtf8))
        !== JSON.stringify([...parsed.names].sort(compareUtf8))
      || Object.values(parsed.bindingTypes).some((kind) => typeof kind !== 'string')
    ) throw new Error('invalid export binding description');
    return parsed;
  } catch (error) {
    throw new Error(`${label} export parser returned invalid JSON: ${errorDetail(error)}`);
  }
}

function maskJavaScriptCommentsAndStrings(source) {
  const chars = source.split('');
  let state = 'code';
  for (let index = 0; index < chars.length; index += 1) {
    const char = source[index];
    const next = source[index + 1];
    if (state === 'line-comment') {
      if (char === '\n') state = 'code';
      else chars[index] = ' ';
    } else if (state === 'block-comment') {
      if (char === '*' && next === '/') {
        chars[index] = ' ';
        chars[index + 1] = ' ';
        index += 1;
        state = 'code';
      } else if (char !== '\n') chars[index] = ' ';
    } else if (state !== 'code') {
      if (char === '\\') {
        chars[index] = ' ';
        if (index + 1 < chars.length) chars[++index] = ' ';
      } else if (
        (state === 'single' && char === "'")
        || (state === 'double' && char === '"')
        || (state === 'template' && char === '`')
      ) {
        chars[index] = ' ';
        state = 'code';
      } else if (char !== '\n') chars[index] = ' ';
    } else if (char === '/' && next === '/') {
      chars[index] = ' ';
      chars[index + 1] = ' ';
      index += 1;
      state = 'line-comment';
    } else if (char === '/' && next === '*') {
      chars[index] = ' ';
      chars[index + 1] = ' ';
      index += 1;
      state = 'block-comment';
    } else if (char === "'") {
      chars[index] = ' ';
      state = 'single';
    } else if (char === '"') {
      chars[index] = ' ';
      state = 'double';
    } else if (char === '`') {
      chars[index] = ' ';
      state = 'template';
    }
  }
  return chars.join('');
}

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/gu, '\\$&');
}

function matchingBrace(source, opening) {
  let depth = 0;
  for (let index = opening; index < source.length; index += 1) {
    if (source[index] === '{') depth += 1;
    else if (source[index] === '}' && --depth === 0) return index;
  }
  return -1;
}

function assertBindgenWrapperShape(maskedSource, key, label) {
  const declaration = new RegExp(
    `\\bexport\\s+function\\s+${escapeRegExp(key)}\\s*\\(([_$A-Za-z][_$A-Za-z0-9]*)\\)\\s*\\{`,
    'gu',
  );
  const matches = [...maskedSource.matchAll(declaration)];
  if (matches.length !== 1) throw new Error(`${label} ${key} must be one direct synchronous function export`);
  const opening = matches[0].index + matches[0][0].lastIndexOf('{');
  const closing = matchingBrace(maskedSource, opening);
  if (closing < 0) throw new Error(`${label} ${key} has an unterminated body`);
  let remainder = maskedSource.slice(opening + 1, closing);
  const take = (expression) => {
    const match = expression.exec(remainder);
    if (!match) return null;
    remainder = remainder.slice(match[0].length);
    return match;
  };
  const identifier = '([_$A-Za-z][_$A-Za-z0-9]*)';
  const inputName = escapeRegExp(matches[0][1]);
  const pointer = take(new RegExp(
    `^\\s*(?:const|let|var)\\s+${identifier}\\s*=\\s*passArray8ToWasm0\\s*\\(\\s*${inputName}\\s*,\\s*wasm\\s*\\.\\s*__wbindgen_malloc\\s*\\)\\s*;`,
    'u',
  ));
  const length = take(new RegExp(
    `^\\s*(?:const|let|var)\\s+${identifier}\\s*=\\s*WASM_VECTOR_LEN\\s*;`,
    'u',
  ));
  const returnedPair = pointer && length && take(new RegExp(
    `^\\s*(?:const|let|var)\\s+${identifier}\\s*=\\s*wasm\\s*\\.\\s*${escapeRegExp(key)}\\s*\\(\\s*${escapeRegExp(pointer[1])}\\s*,\\s*${escapeRegExp(length[1])}\\s*\\)\\s*;`,
    'u',
  ));
  const result = returnedPair && take(new RegExp(
    `^\\s*(?:const|let|var)\\s+${identifier}\\s*=\\s*getArrayU8FromWasm0\\s*\\(\\s*${escapeRegExp(returnedPair[1])}\\s*\\[\\s*0\\s*\\]\\s*,\\s*${escapeRegExp(returnedPair[1])}\\s*\\[\\s*1\\s*\\]\\s*\\)\\s*\\.\\s*slice\\s*\\(\\s*\\)\\s*;`,
    'u',
  ));
  const freed = result && take(new RegExp(
    `^\\s*wasm\\s*\\.\\s*__wbindgen_free\\s*\\(\\s*${escapeRegExp(returnedPair[1])}\\s*\\[\\s*0\\s*\\]\\s*,\\s*${escapeRegExp(returnedPair[1])}\\s*\\[\\s*1\\s*\\](?:\\s*\\*\\s*1)?\\s*,\\s*1\\s*\\)\\s*;`,
    'u',
  ));
  const returned = freed && take(new RegExp(
    `^\\s*return\\s+${escapeRegExp(result[1])}\\s*;`,
    'u',
  ));
  if (!returned || !/^\s*$/u.test(remainder)) {
    throw new Error(`${label} ${key} is not the canonical synchronous Uint8Array wrapper shape`);
  }
}

/** Validate bindgen raw protocol plus the complete synchronous JS wrapper set. */
export function assertBindgenWasmExtensionV3(
  wasmBytes,
  jsBytes,
  {
    expectedExportKeys,
    label = 'bindgen WASM extension',
    maxWasmBytes = MAX_PROTOCOL_WASM_BYTES,
    maxJsBytes = MAX_PROTOCOL_JS_BYTES,
  },
) {
  const expected = validateExpectedExportKeys(expectedExportKeys, label);
  const { structure } = inspectProtocolWasm(wasmBytes, `${label} raw WASM`, maxWasmBytes);
  const rawWrappers = structure.exports
    .map(({ name }) => name)
    .filter((name) => name.startsWith(WASM_EXTENSION_EXPORT_PREFIX))
    .sort(compareUtf8);
  if (JSON.stringify(rawWrappers) !== JSON.stringify(expected)) {
    throw new Error(`${label} raw WASM exact wrapper export set differs from its Vo extern catalog`);
  }
  for (const key of expected) {
    assertFunctionType(
      exportedFunction(structure, key, `${label} raw WASM`).type,
      [I32, I32],
      [I32, I32],
      `${label} raw WASM ${key}`,
    );
  }
  const source = boundedUtf8(jsBytes, `${label} JS glue`, maxJsBytes);
  const parsedExports = parsedJavaScriptExports(source, `${label} JS glue`);
  const expectedJsExports = [...BINDGEN_STANDARD_EXPORTS, ...expected].sort(compareUtf8);
  const actualJsExports = [...parsedExports.names].sort(compareUtf8);
  if (JSON.stringify(actualJsExports) !== JSON.stringify(expectedJsExports)) {
    throw new Error(`${label} JS exact export allowlist differs from its Vo extern catalog`);
  }
  for (const name of BINDGEN_STANDARD_EXPORTS) {
    if (parsedExports.bindingTypes[name] !== 'function') {
      throw new Error(`${label} JS standard export ${name} must bind to a function declaration`);
    }
  }
  const masked = maskJavaScriptCommentsAndStrings(source);
  for (const key of expected) assertBindgenWrapperShape(masked, key, `${label} JS glue`);
  return Object.freeze({ exportKeys: Object.freeze(expected), version: 3 });
}
