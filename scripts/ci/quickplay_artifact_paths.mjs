import { portableCaseKey } from './portable_path_key.mjs';

const QUICKPLAY_PREFIX = '/quickplay/blockkart/';
const PORTABLE_COMPONENT_BYTES = 255;
const PORTABLE_PATH_BYTES = 4 * 1024;
const SUPPORTED_ARTIFACT_KINDS = new Set([
  'extension-native',
  'extension-wasm',
  'extension-js-glue',
]);
const MAX_U64 = 18_446_744_073_709_551_615n;

function invalid(label, value) {
  throw new Error(`Invalid ${label}: ${JSON.stringify(value)}`);
}

function hasUnicodeWhiteSpaceBoundary(value) {
  return /^(?:\p{White_Space})|(?:\p{White_Space})$/u.test(value);
}

function hasUnpairedSurrogate(value) {
  for (let index = 0; index < value.length; index += 1) {
    const code = value.charCodeAt(index);
    if (code >= 0xd800 && code <= 0xdbff) {
      const next = value.charCodeAt(index + 1);
      if (!(next >= 0xdc00 && next <= 0xdfff)) return true;
      index += 1;
    } else if (code >= 0xdc00 && code <= 0xdfff) {
      return true;
    }
  }
  return false;
}

/** Keep generated quickplay paths aligned with vo-module's wire-path rules. */
export function validatePortableComponent(value, label = 'path component') {
  if (
    typeof value !== 'string'
    || value.length === 0
    || Buffer.byteLength(value, 'utf8') > PORTABLE_COMPONENT_BYTES
    || hasUnpairedSurrogate(value)
    || value === '.'
    || value === '..'
    || hasUnicodeWhiteSpaceBoundary(value)
    || value.endsWith('.')
    || value.normalize('NFC') !== value
    || value.includes('/')
    || value.includes('\\')
    || /[\p{Cc}<>:"|?*]/u.test(value)
  ) {
    invalid(label, value);
  }

  const stem = portableCaseKey(value.split('.', 1)[0]);
  if (
    ['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].includes(stem)
    || /^(?:com|lpt)(?:[1-9]|[¹²³])$/.test(stem)
  ) {
    invalid(label, value);
  }
  return value;
}

export function validatePortableRelativePath(value, label = 'relative path') {
  if (
    typeof value !== 'string'
    || value.length === 0
    || Buffer.byteLength(value, 'utf8') > PORTABLE_PATH_BYTES
    || hasUnpairedSurrogate(value)
    || hasUnicodeWhiteSpaceBoundary(value)
    || value.includes('\\')
  ) {
    invalid(label, value);
  }
  const components = value.split('/');
  for (const component of components) {
    validatePortableComponent(component, `${label} component`);
  }
  return components;
}

export function validateArtifactIdentity(artifact) {
  if (artifact == null || typeof artifact !== 'object') {
    invalid('artifact identity', artifact);
  }
  const { kind, target, name } = artifact;
  if (!SUPPORTED_ARTIFACT_KINDS.has(kind)) {
    invalid('artifact kind', kind);
  }
  validatePortableComponent(target, 'artifact target');
  validatePortableComponent(name, 'artifact name');
  if (
    (kind === 'extension-wasm' || kind === 'extension-js-glue')
    && target !== 'wasm32-unknown-unknown'
  ) {
    invalid(`${kind} target`, target);
  }
  if (
    kind === 'extension-native'
    && (
      target.split('-').length < 3
      || target.split('-').some((segment) => !/^[a-z0-9_.]+$/.test(segment))
    )
  ) {
    invalid('extension-native target', target);
  }
  return { kind, target, name };
}

function validateExactVersion(version) {
  if (typeof version !== 'string' || Buffer.byteLength(version, 'utf8') > PORTABLE_COMPONENT_BYTES) {
    invalid('exact module version', version);
  }
  validatePortableComponent(version, 'exact module version');
  const match = /^v(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)(?:-([0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?$/.exec(version);
  if (!match) {
    invalid('exact module version', version);
  }
  const numeric = match.slice(1, 4);
  if (numeric.some((part) => BigInt(part) > MAX_U64)) {
    invalid('exact module version', version);
  }
  if (
    match[4]?.split('.').some((part) => (
      /^[0-9]+$/.test(part)
      && ((part.length > 1 && part[0] === '0') || BigInt(part) > MAX_U64)
    ))
  ) {
    invalid('exact module version', version);
  }
  return BigInt(match[1]);
}

function validateModulePath(module, versionMajor) {
  if (typeof module !== 'string' || !module.startsWith('github.com/')) {
    invalid('module path', module);
  }
  const segments = module.split('/');
  if (segments.length < 3) {
    invalid('module path', module);
  }
  for (const segment of segments) {
    validatePortableComponent(segment, 'module path component');
    if (!/^[a-z0-9][a-z0-9._-]*$/.test(segment)) {
      invalid('module path component', segment);
    }
  }
  const cacheComponent = module.replaceAll('/', '@');
  validatePortableComponent(cacheComponent, 'module cache key');

  const suffix = /^v([0-9]+)$/.exec(segments.at(-1));
  if (suffix) {
    const suffixMajor = BigInt(suffix[1]);
    if (suffixMajor < 2n || suffixMajor > MAX_U64 || suffixMajor !== versionMajor) {
      invalid('module major-version suffix', segments.at(-1));
    }
  } else if (versionMajor > 1n) {
    invalid('unsuffixed module major version', versionMajor.toString());
  }
  return cacheComponent;
}

export function moduleCacheDir(module, version) {
  const versionMajor = validateExactVersion(version);
  const cacheComponent = validateModulePath(module, versionMajor);
  return `${cacheComponent}/${version}`;
}

export function artifactKey(artifact) {
  const { kind, target, name } = validateArtifactIdentity(artifact);
  return `${kind}\u0000${target}\u0000${name}`;
}

export function artifactCachePath(artifact) {
  const { kind, target, name } = validateArtifactIdentity(artifact);
  return `artifacts/${kind}/${target}/${name}`;
}

export function artifactOutputRelativePath(cacheDir, artifact) {
  const cacheComponents = validatePortableRelativePath(cacheDir, 'module cache directory');
  const { kind, target, name } = validateArtifactIdentity(artifact);
  return ['artifacts', ...cacheComponents, kind, target, name].join('/');
}

function encodePortableRelativePath(value) {
  return validatePortableRelativePath(value).map(encodeUrlPathComponent).join('/');
}

function encodeUrlPathComponent(value) {
  return encodeURIComponent(value).replace(/[!'()*]/g, (character) => (
    `%${character.charCodeAt(0).toString(16).toUpperCase()}`
  ));
}

export function quickplayArtifactUrl(cacheDir, artifact) {
  return `${QUICKPLAY_PREFIX}${encodePortableRelativePath(artifactOutputRelativePath(cacheDir, artifact))}`;
}

/** Decode only the canonical component-wise encoding emitted by quickplayArtifactUrl. */
export function quickplayArtifactRelativePathFromUrl(url) {
  if (typeof url !== 'string' || !url.startsWith(QUICKPLAY_PREFIX)) {
    invalid('quickplay artifact URL', url);
  }
  const encodedPath = url.slice(QUICKPLAY_PREFIX.length);
  if (encodedPath.length === 0 || encodedPath.includes('?') || encodedPath.includes('#')) {
    invalid('quickplay artifact URL', url);
  }

  const decoded = encodedPath.split('/').map((component) => {
    if (component.length === 0) {
      invalid('quickplay artifact URL component', component);
    }
    let value;
    try {
      value = decodeURIComponent(component);
    } catch {
      invalid('quickplay artifact URL component', component);
    }
    validatePortableComponent(value, 'quickplay artifact URL component');
    if (encodeUrlPathComponent(value) !== component) {
      invalid('non-canonical quickplay artifact URL component', component);
    }
    return value;
  });
  return decoded.join('/');
}
