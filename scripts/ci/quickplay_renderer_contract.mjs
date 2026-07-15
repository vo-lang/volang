import { createRequire } from 'node:module';
import { posix } from 'node:path';

const studioRequire = createRequire(new URL('../../apps/studio/package.json', import.meta.url));
const { parse: parseWithAcorn } = studioRequire('acorn');
const ts = studioRequire('typescript');

export const QUICKPLAY_RENDERER_LIMITS = Object.freeze({
  maxFileSourceBytes: 16 * 1024 * 1024,
  maxGraphFiles: 4096,
  maxGraphSourceBytes: 64 * 1024 * 1024,
  maxImportsPerFile: 10_000,
  maxGraphEdges: 100_000,
  maxSpecifierBytes: 8 * 1024,
  maxAstNodes: 1_000_000,
});

const LIMIT_NAMES = Object.freeze(Object.keys(QUICKPLAY_RENDERER_LIMITS));
const DEFAULT_BRIDGE_PATH = 'js/dist/render_bootstrap.js';
const GRAPH_BRAND = Symbol('quickplay-local-javascript-graph');

export class QuickplayRendererContractError extends Error {
  constructor(message) {
    super(message);
    this.name = 'QuickplayRendererContractError';
  }
}

function fail(label, message) {
  throw new QuickplayRendererContractError(`${label}: ${message}`);
}

function normalizeLabel(label) {
  return typeof label === 'string' && label.length > 0
    ? label
    : 'quickplay renderer contract';
}

function normalizeLimits(overrides, label) {
  if (overrides === undefined) return QUICKPLAY_RENDERER_LIMITS;
  if (overrides === null || typeof overrides !== 'object' || Array.isArray(overrides)) {
    fail(label, 'limits must be an object');
  }
  for (const name of Object.keys(overrides)) {
    if (!LIMIT_NAMES.includes(name)) fail(label, `unknown resource limit ${name}`);
  }
  const limits = { ...QUICKPLAY_RENDERER_LIMITS };
  for (const name of LIMIT_NAMES) {
    if (!Object.hasOwn(overrides, name)) continue;
    const value = overrides[name];
    if (!Number.isSafeInteger(value) || value <= 0) {
      fail(label, `${name} must be a positive safe integer`);
    }
    if (value > QUICKPLAY_RENDERER_LIMITS[name]) {
      fail(label, `${name} cannot exceed the production limit ${QUICKPLAY_RENDERER_LIMITS[name]}`);
    }
    limits[name] = value;
  }
  return Object.freeze(limits);
}

function utf8Size(value) {
  return Buffer.byteLength(value, 'utf8');
}

function requireSourceFile(file, label) {
  if (file === null || typeof file !== 'object' || Array.isArray(file)) {
    fail(label, 'source file must be a { path, source } object');
  }
  const { path, source } = file;
  if (typeof path !== 'string' || path.length === 0) {
    fail(label, 'source file path must be a non-empty string');
  }
  if (typeof source !== 'string') {
    fail(label, `${path} source must be a string`);
  }
  return { path, source };
}

function requirePortableModulePath(path, label) {
  if (
    path.length === 0
    || path.includes('\0')
    || path.includes('\\')
    || posix.isAbsolute(path)
    || posix.normalize(path) !== path
    || path === '.'
    || path === '..'
    || path.startsWith('../')
    || path.endsWith('/')
  ) {
    fail(label, `module path must be a normalized portable relative path: ${JSON.stringify(path)}`);
  }
}

function forEachChildNode(node, visit) {
  for (const value of Object.values(node)) {
    if (Array.isArray(value)) {
      for (let index = value.length - 1; index >= 0; index -= 1) {
        const child = value[index];
        if (child && typeof child === 'object' && typeof child.type === 'string') visit(child);
      }
    } else if (value && typeof value === 'object' && typeof value.type === 'string') {
      visit(value);
    }
  }
}

function walkAst(ast, maxNodes, label, visitor = undefined) {
  const queue = [ast];
  let cursor = 0;
  while (cursor < queue.length) {
    const node = queue[cursor];
    cursor += 1;
    if (cursor > maxNodes) fail(label, `AST exceeds ${maxNodes} nodes`);
    visitor?.(node);
    forEachChildNode(node, (child) => queue.push(child));
  }
  return cursor;
}

function staticStringValue(node) {
  if (node?.type === 'Literal' && typeof node.value === 'string') return node.value;
  if (
    node?.type === 'TemplateLiteral'
    && node.expressions.length === 0
    && node.quasis.length === 1
  ) {
    const cooked = node.quasis[0].value.cooked;
    return typeof cooked === 'string' ? cooked : undefined;
  }
  return undefined;
}

function isLocalSpecifier(specifier) {
  return specifier === '.'
    || specifier === '..'
    || specifier.startsWith('./')
    || specifier.startsWith('../');
}

function collectStaticImports(ast, limits, label) {
  const imports = [];
  let importCount = 0;
  walkAst(ast, limits.maxAstNodes, label, (node) => {
    let kind;
    let sourceNode;
    if (node.type === 'ImportDeclaration') {
      kind = 'import';
      sourceNode = node.source;
    } else if (node.type === 'ExportNamedDeclaration' && node.source) {
      kind = 'export-named';
      sourceNode = node.source;
    } else if (node.type === 'ExportAllDeclaration') {
      kind = 'export-all';
      sourceNode = node.source;
    } else if (node.type === 'ImportExpression') {
      kind = 'import-expression';
      sourceNode = node.source;
    } else {
      return;
    }

    importCount += 1;
    if (importCount > limits.maxImportsPerFile) {
      fail(label, `file exceeds ${limits.maxImportsPerFile} import edges`);
    }

    const specifier = staticStringValue(sourceNode);
    if (specifier === undefined) return;
    const specifierBytes = utf8Size(specifier);
    if (specifierBytes > limits.maxSpecifierBytes) {
      fail(label, `import specifier exceeds ${limits.maxSpecifierBytes} UTF-8 bytes at offset ${node.start}`);
    }
    imports.push(Object.freeze({
      kind,
      local: isLocalSpecifier(specifier),
      specifier,
    }));
  });
  return Object.freeze({ importCount, imports: Object.freeze(imports) });
}

/**
 * Parse one caller-provided JavaScript module and extract its real static
 * import/export/import() string literals. Comments and string contents are
 * never searched as source text.
 */
export function parseQuickplayJavaScriptModule(file, options = {}) {
  const label = normalizeLabel(options.label);
  const limits = normalizeLimits(options.limits, label);
  const normalized = requireSourceFile(file, label);
  requirePortableModulePath(normalized.path, label);
  const sourceBytes = utf8Size(normalized.source);
  if (sourceBytes > limits.maxFileSourceBytes) {
    fail(label, `${normalized.path} exceeds ${limits.maxFileSourceBytes} UTF-8 source bytes`);
  }

  let tokenCount = 0;
  let ast;
  try {
    ast = parseWithAcorn(normalized.source, {
      allowHashBang: true,
      ecmaVersion: 'latest',
      onToken() {
        tokenCount += 1;
        if (tokenCount > limits.maxAstNodes) {
          fail(label, `${normalized.path} token count exceeds the AST budget ${limits.maxAstNodes}`);
        }
      },
      sourceType: 'module',
    });
  } catch (error) {
    if (error instanceof QuickplayRendererContractError) throw error;
    const detail = error instanceof Error ? error.message : String(error);
    fail(label, `${normalized.path} is not a valid JavaScript module: ${detail}`);
  }

  const astNodes = walkAst(ast, limits.maxAstNodes, `${label} ${normalized.path}`);
  const extracted = collectStaticImports(ast, limits, `${label} ${normalized.path}`);
  return Object.freeze({
    ast,
    astNodes,
    importCount: extracted.importCount,
    imports: extracted.imports,
    path: normalized.path,
    source: normalized.source,
    sourceBytes,
  });
}

function normalizeReadResult(result, requestedPath, label) {
  if (result === undefined || result === null) return undefined;
  if (typeof result === 'string') return { path: requestedPath, source: result };
  const file = requireSourceFile(result, label);
  if (file.path !== requestedPath) {
    fail(label, `readSource(${JSON.stringify(requestedPath)}) returned path ${JSON.stringify(file.path)}`);
  }
  return file;
}

function localImportCandidates(fromPath, specifier, label) {
  if (specifier.includes('\\') || specifier.includes('\0')) {
    fail(label, `local import must use a portable specifier: ${JSON.stringify(specifier)}`);
  }
  const base = posix.normalize(posix.join(posix.dirname(fromPath), specifier));
  if (base === '..' || base.startsWith('../') || posix.isAbsolute(base)) {
    fail(label, `local import escapes the module package: ${JSON.stringify(specifier)} from ${fromPath}`);
  }
  const candidates = posix.extname(base)
    ? [base]
    : [`${base}.js`, `${base}/index.js`, base];
  for (const candidate of candidates) requirePortableModulePath(candidate, label);
  return candidates;
}

/**
 * Build a bounded local JavaScript import graph. readSource(path) must return
 * a source string, a { path, source } object, or null/undefined when absent.
 * The callback owns all package/file access; this module performs no Quickplay
 * filesystem reads.
 */
export function collectQuickplayLocalJavaScriptGraph({
  entryPath,
  readSource,
  limits: limitOverrides,
  label: rawLabel,
}) {
  const label = normalizeLabel(rawLabel);
  const limits = normalizeLimits(limitOverrides, label);
  if (typeof entryPath !== 'string' || entryPath.length === 0) {
    fail(label, 'entryPath must be a non-empty string');
  }
  requirePortableModulePath(entryPath, label);
  if (typeof readSource !== 'function') fail(label, 'readSource must be a function');

  const readCache = new Map();
  function read(path) {
    if (readCache.has(path)) return readCache.get(path);
    let value;
    try {
      value = normalizeReadResult(readSource(path), path, label);
    } catch (error) {
      if (error instanceof QuickplayRendererContractError) throw error;
      const detail = error instanceof Error ? error.message : String(error);
      fail(label, `readSource(${JSON.stringify(path)}) failed: ${detail}`);
    }
    readCache.set(path, value);
    return value;
  }

  if (!read(entryPath)) fail(label, `renderer entry is missing: ${entryPath}`);

  const queue = [entryPath];
  const queued = new Set(queue);
  const files = [];
  let cursor = 0;
  let totalAstNodes = 0;
  let totalImportEdges = 0;
  let totalSourceBytes = 0;

  while (cursor < queue.length) {
    const path = queue[cursor];
    cursor += 1;
    if (files.length >= limits.maxGraphFiles) {
      fail(label, `local JavaScript graph exceeds ${limits.maxGraphFiles} files`);
    }
    const sourceFile = read(path);
    if (!sourceFile) fail(label, `queued JavaScript module disappeared: ${path}`);
    const remainingAstNodes = limits.maxAstNodes - totalAstNodes;
    if (remainingAstNodes <= 0) fail(label, `graph exceeds ${limits.maxAstNodes} AST nodes`);
    const parsed = parseQuickplayJavaScriptModule(sourceFile, {
      label,
      limits: { ...limits, maxAstNodes: remainingAstNodes },
    });
    totalAstNodes += parsed.astNodes;
    totalSourceBytes += parsed.sourceBytes;
    if (totalSourceBytes > limits.maxGraphSourceBytes) {
      fail(label, `local JavaScript graph exceeds ${limits.maxGraphSourceBytes} UTF-8 source bytes`);
    }
    totalImportEdges += parsed.importCount;
    if (totalImportEdges > limits.maxGraphEdges) {
      fail(label, `local JavaScript graph exceeds ${limits.maxGraphEdges} static import edges`);
    }
    files.push(parsed);

    for (const imported of parsed.imports) {
      if (!imported.local) continue;
      const candidates = localImportCandidates(path, imported.specifier, label);
      const resolved = candidates.find((candidate) => read(candidate) !== undefined);
      if (!resolved) {
        fail(label, `cannot resolve local import ${JSON.stringify(imported.specifier)} from ${path}`);
      }
      if (!queued.has(resolved)) {
        queued.add(resolved);
        queue.push(resolved);
      }
    }
  }

  return Object.freeze({
    [GRAPH_BRAND]: true,
    entryPath,
    fileCount: files.length,
    files: Object.freeze(files),
    totalAstNodes,
    totalImportEdges,
    totalSourceBytes,
  });
}

function staticPropertyName(member) {
  if (member.type !== 'MemberExpression') return undefined;
  if (!member.computed && member.property.type === 'Identifier') return member.property.name;
  return staticStringValue(member.property);
}

function unwrapChain(node) {
  let current = node;
  while (current?.type === 'ChainExpression') current = current.expression;
  return current;
}

function memberChain(node) {
  let current = unwrapChain(node);
  const parts = [];
  while (current?.type === 'MemberExpression') {
    const property = staticPropertyName(current);
    if (property === undefined) return undefined;
    parts.unshift(property);
    current = unwrapChain(current.object);
  }
  if (current?.type === 'ThisExpression') return { root: 'this', parts };
  if (current?.type === 'Identifier') return { root: current.name, parts };
  return undefined;
}

function chainEquals(node, root, parts) {
  const chain = memberChain(node);
  return chain?.root === root
    && chain.parts.length === parts.length
    && chain.parts.every((part, index) => part === parts[index]);
}

function isIdentifier(node, name) {
  return unwrapChain(node)?.type === 'Identifier' && unwrapChain(node).name === name;
}

function callMatches(node, root, parts) {
  return node.type === 'CallExpression' && chainEquals(node.callee, root, parts);
}

function methodName(node) {
  if (node.type !== 'MethodDefinition' && node.type !== 'PropertyDefinition') return undefined;
  if (!node.computed && node.key.type === 'Identifier') return node.key.name;
  return staticStringValue(node.key);
}

const HOST_WAIT_VALUE = Object.freeze({
  eventList: 'event-list',
  event: 'event',
  key: 'key',
  token: 'token',
  delay: 'delay',
});

function expressionHostWaitValue(node, bindings) {
  const current = unwrapChain(node);
  if (!current) return undefined;
  if (current.type === 'Identifier') return bindings.get(current.name);
  if (current.type === 'MemberExpression') {
    const objectKind = expressionHostWaitValue(current.object, bindings);
    if (objectKind !== HOST_WAIT_VALUE.event) return undefined;
    const property = staticPropertyName(current);
    if (property === 'key') return HOST_WAIT_VALUE.key;
    if (property === 'token') return HOST_WAIT_VALUE.token;
    if (property === 'delayMs') return HOST_WAIT_VALUE.delay;
    return undefined;
  }
  if (callMatches(current, 'this', ['vm', 'takePendingHostEvents'])) {
    return HOST_WAIT_VALUE.eventList;
  }
  return undefined;
}

function patternPropertyName(property) {
  if (property.type !== 'Property') return undefined;
  if (!property.computed && property.key.type === 'Identifier') return property.key.name;
  return staticStringValue(property.key);
}

function bindHostWaitPattern(pattern, kind, bindings, label) {
  if (pattern.type === 'Identifier') {
    if (bindings.has(pattern.name)) {
      fail(label, `HostWait binding ${pattern.name} is shadowed at offset ${pattern.start}`);
    }
    if (kind !== undefined) bindings.set(pattern.name, kind);
    return;
  }
  if (pattern.type === 'AssignmentPattern') {
    bindHostWaitPattern(pattern.left, kind, bindings, label);
    return;
  }
  if (pattern.type !== 'ObjectPattern' || kind !== HOST_WAIT_VALUE.event) return;
  for (const property of pattern.properties) {
    if (property.type === 'RestElement') {
      bindHostWaitPattern(property.argument, undefined, bindings, label);
      continue;
    }
    const name = patternPropertyName(property);
    const valueKind = name === 'key'
      ? HOST_WAIT_VALUE.key
      : name === 'token'
        ? HOST_WAIT_VALUE.token
        : name === 'delayMs'
          ? HOST_WAIT_VALUE.delay
          : undefined;
    if (valueKind === HOST_WAIT_VALUE.token) {
      fail(label, `token-only HostWait binding at offset ${property.start}`);
    }
    bindHostWaitPattern(property.value, valueKind, bindings, label);
  }
}

function extendHostWaitBindings(root, bindings, label) {
  const declarators = [];
  walkAst(root, QUICKPLAY_RENDERER_LIMITS.maxAstNodes, label, (node) => {
    if (node.type === 'VariableDeclarator') declarators.push(node);
  });
  declarators.sort((left, right) => left.start - right.start);
  for (const declarator of declarators) {
    const kind = expressionHostWaitValue(declarator.init, bindings);
    bindHostWaitPattern(declarator.id, kind, bindings, label);
  }
  walkAst(root, QUICKPLAY_RENDERER_LIMITS.maxAstNodes, label, (node) => {
    if (
      node.type === 'MemberExpression'
      && expressionHostWaitValue(node.object, bindings) === HOST_WAIT_VALUE.event
      && staticPropertyName(node) === 'token'
    ) {
      fail(label, `token-only HostWait property at offset ${node.start}`);
    }
    if (
      (node.type === 'AssignmentExpression' || node.type === 'UpdateExpression')
      && node.left?.type === 'Identifier'
      && bindings.has(node.left.name)
    ) {
      fail(label, `HostWait binding ${node.left.name} is reassigned at offset ${node.start}`);
    }
  });
}

function exportedRenderIslandClass(ast, label) {
  const classes = ast.body
    .filter((node) => node.type === 'ExportNamedDeclaration')
    .map((node) => node.declaration)
    .filter((node) => node?.type === 'ClassDeclaration' && node.id?.name === 'RenderIsland');
  if (classes.length !== 1) {
    fail(label, 'bridge must directly export exactly one RenderIsland class');
  }
  return classes[0];
}

function classMethod(classNode, name, label) {
  const methods = classNode.body.body.filter((node) => (
    node.type === 'MethodDefinition'
    && node.kind === 'method'
    && node.static === false
    && methodName(node) === name
  ));
  if (methods.length !== 1 || methods[0].value?.body?.type !== 'BlockStatement') {
    fail(label, `RenderIsland must define exactly one concrete ${name} method`);
  }
  return methods[0].value;
}

function statementList(node) {
  return node?.type === 'BlockStatement' ? node.body : node ? [node] : [];
}

function statementDefinitelyExits(node) {
  if (!node) return false;
  if (['ContinueStatement', 'ReturnStatement', 'ThrowStatement'].includes(node.type)) return true;
  if (node.type !== 'BlockStatement') return false;
  for (const statement of node.body) {
    if (statement.type === 'EmptyStatement') continue;
    return statementDefinitelyExits(statement);
  }
  return false;
}

function collectExpressionCalls(node, calls) {
  const current = unwrapChain(node);
  if (!current) return;
  if (
    current.type === 'ArrowFunctionExpression'
    || current.type === 'FunctionExpression'
    || current.type === 'FunctionDeclaration'
    || current.type === 'ClassExpression'
    || current.type === 'ClassDeclaration'
  ) return;
  if (current.type === 'CallExpression') calls.push(current);
  forEachChildNode(current, (child) => collectExpressionCalls(child, calls));
}

function directStatementCalls(node) {
  const calls = [];
  for (const statement of statementList(node)) {
    if (statement.type === 'ExpressionStatement') {
      collectExpressionCalls(statement.expression, calls);
    } else if (statement.type === 'VariableDeclaration') {
      for (const declaration of statement.declarations) {
        collectExpressionCalls(declaration.init, calls);
      }
    }
  }
  return calls;
}

function isDisplayPulseCondition(node, bindings) {
  const current = unwrapChain(node);
  if (current?.type !== 'BinaryExpression' || current.operator !== '===') return false;
  return (
    expressionHostWaitValue(current.left, bindings) === HOST_WAIT_VALUE.delay
    && isIdentifier(current.right, 'DISPLAY_PULSE_DELAY_MS')
  ) || (
    expressionHostWaitValue(current.right, bindings) === HOST_WAIT_VALUE.delay
    && isIdentifier(current.left, 'DISPLAY_PULSE_DELAY_MS')
  );
}

function requireKeyArgument(call, bindings, label, description) {
  const kind = expressionHostWaitValue(call.arguments[0], bindings);
  if (kind !== HOST_WAIT_VALUE.key) {
    fail(label, `${description} must use the pending event key at offset ${call.start}`);
  }
}

function validateLoopRelevantCalls(loop, bindings, label) {
  walkAst(loop.body, QUICKPLAY_RENDERER_LIMITS.maxAstNodes, label, (node) => {
    if (node.type !== 'CallExpression') return;
    const description = callMatches(node, 'this', ['hostTimers', 'has'])
      ? 'hostTimers.has'
      : callMatches(node, 'this', ['hostTimers', 'set'])
        ? 'hostTimers.set'
        : callMatches(node, 'this', ['displayPulseWaiters', 'set'])
          ? 'displayPulseWaiters.set'
          : callMatches(node, 'this', ['wakeHostEvent'])
            ? 'wakeHostEvent'
            : undefined;
    if (description !== undefined) requireKeyArgument(node, bindings, label, description);
  });
}

function timerCallbackUsesEventKey(call, bindings, label) {
  if (!callMatches(call, 'window', ['setTimeout'])) return false;
  const callback = unwrapChain(call.arguments[0]);
  if (callback?.type !== 'ArrowFunctionExpression' || callback.params.length !== 0) return false;
  const calls = callback.body.type === 'BlockStatement'
    ? directStatementCalls(callback.body)
    : (() => {
        const found = [];
        collectExpressionCalls(callback.body, found);
        return found;
      })();
  const wakes = calls.filter((candidate) => callMatches(candidate, 'this', ['wakeHostEvent']));
  for (const wake of wakes) requireKeyArgument(wake, bindings, label, 'wakeHostEvent');
  return wakes.length === 1;
}

function scheduleLoopFacts(loop, eventName, label) {
  const bindings = new Map([[eventName, HOST_WAIT_VALUE.event]]);
  extendHostWaitBindings(loop.body, bindings, label);
  validateLoopRelevantCalls(loop, bindings, label);
  const statements = statementList(loop.body);
  const guard = statements.find((statement) => (
    statement.type === 'IfStatement'
    && callMatches(unwrapChain(statement.test), 'this', ['hostTimers', 'has'])
    && expressionHostWaitValue(statement.test.arguments[0], bindings) === HOST_WAIT_VALUE.key
    && statementDefinitelyExits(statement.consequent)
  ));
  const pulse = statements.find((statement) => (
    statement.type === 'IfStatement'
    && isDisplayPulseCondition(statement.test, bindings)
    && statement.alternate !== null
  ));
  if (!pulse) {
    return {
      displayPulseWaitersSetEventKey: false,
      hostTimersHasEventKey: guard !== undefined,
      hostTimersSetEventKey: false,
      scheduleWakeEventKey: false,
    };
  }
  const displayCalls = directStatementCalls(pulse.consequent);
  const timeoutCalls = directStatementCalls(pulse.alternate);
  const displaySet = displayCalls.find((call) => callMatches(call, 'this', ['displayPulseWaiters', 'set']));
  const displayHostSet = displayCalls.find((call) => callMatches(call, 'this', ['hostTimers', 'set']));
  const timeoutHostSet = timeoutCalls.find((call) => callMatches(call, 'this', ['hostTimers', 'set']));
  for (const [call, description] of [
    [displaySet, 'displayPulseWaiters.set'],
    [displayHostSet, 'display hostTimers.set'],
    [timeoutHostSet, 'timeout hostTimers.set'],
  ]) {
    if (call) requireKeyArgument(call, bindings, label, description);
  }
  const timeoutWake = timeoutCalls.some((call) => timerCallbackUsesEventKey(call, bindings, label));
  return {
    displayPulseWaitersSetEventKey: displaySet !== undefined,
    hostTimersHasEventKey: guard !== undefined,
    hostTimersSetEventKey: displayHostSet !== undefined && timeoutHostSet !== undefined,
    scheduleWakeEventKey: timeoutWake,
  };
}

function analyzeScheduleHostEvents(method, label) {
  if (method.params.length !== 0) fail(label, 'scheduleHostEvents must not accept parameters');
  const bindings = new Map();
  const candidateLoops = [];
  for (const statement of method.body.body) {
    if (statement.type === 'VariableDeclaration') {
      for (const declarator of statement.declarations) {
        bindHostWaitPattern(
          declarator.id,
          expressionHostWaitValue(declarator.init, bindings),
          bindings,
          label,
        );
      }
      continue;
    }
    if (statement.type !== 'ForOfStatement') continue;
    if (expressionHostWaitValue(statement.right, bindings) !== HOST_WAIT_VALUE.eventList) continue;
    const declaration = statement.left.type === 'VariableDeclaration'
      && statement.left.declarations.length === 1
      ? statement.left.declarations[0]
      : undefined;
    if (declaration?.id.type !== 'Identifier') {
      fail(label, 'pending HostWait events loop must bind one event identifier');
    }
    candidateLoops.push({ loop: statement, eventName: declaration.id.name });
  }
  if (candidateLoops.length !== 1) {
    fail(label, 'scheduleHostEvents must contain exactly one direct pending-event loop');
  }
  return scheduleLoopFacts(candidateLoops[0].loop, candidateLoops[0].eventName, label);
}

function reachableWakeCalls(statements, calls) {
  for (const statement of statements) {
    if (statement.type === 'ExpressionStatement') {
      collectExpressionCalls(statement.expression, calls);
    } else if (statement.type === 'VariableDeclaration') {
      for (const declaration of statement.declarations) collectExpressionCalls(declaration.init, calls);
    } else if (statement.type === 'BlockStatement') {
      reachableWakeCalls(statement.body, calls);
    } else if (statement.type === 'TryStatement') {
      reachableWakeCalls(statement.block.body, calls);
      if (statement.finalizer) reachableWakeCalls(statement.finalizer.body, calls);
    }
    if (statement.type === 'ReturnStatement' || statement.type === 'ThrowStatement') return;
  }
}

function analyzeWakeHostEvent(method, label) {
  const keyParameter = method.params[0];
  if (keyParameter?.type !== 'Identifier') {
    fail(label, 'wakeHostEvent must bind its first parameter as an identifier');
  }
  const bindings = new Map([[keyParameter.name, HOST_WAIT_VALUE.key]]);
  extendHostWaitBindings(method.body, bindings, label);
  walkAst(method.body, QUICKPLAY_RENDERER_LIMITS.maxAstNodes, label, (node) => {
    if (node.type !== 'CallExpression' || !callMatches(node, 'this', ['vm', 'wakeHostEvent'])) return;
    requireKeyArgument(node, bindings, label, 'vm.wakeHostEvent');
  });
  const calls = [];
  reachableWakeCalls(method.body.body, calls);
  return calls.some((call) => (
    callMatches(call, 'this', ['vm', 'wakeHostEvent'])
    && expressionHostWaitValue(call.arguments[0], bindings) === HOST_WAIT_VALUE.key
  ));
}

function analyzeHostWaitKeyAst(file, label) {
  const renderIsland = exportedRenderIslandClass(file.ast, label);
  const schedule = analyzeScheduleHostEvents(
    classMethod(renderIsland, 'scheduleHostEvents', label),
    label,
  );
  const facts = {
    ...schedule,
    vmWakeMethodKey: analyzeWakeHostEvent(
      classMethod(renderIsland, 'wakeHostEvent', label),
      label,
    ),
  };
  const missing = Object.entries(facts)
    .filter(([, present]) => !present)
    .map(([name]) => name);
  if (missing.length > 0) fail(label, `missing HostWaitKey AST semantics: ${missing.join(', ')}`);
  return Object.freeze(facts);
}

function hasModifier(node, kind) {
  return node.modifiers?.some((modifier) => modifier.kind === kind) ?? false;
}

function declarationMemberName(member) {
  if (ts.isIdentifier(member.name) || ts.isStringLiteral(member.name)) return member.name.text;
  return undefined;
}

function unwrapTypeNode(node) {
  let current = node;
  while (current && ts.isParenthesizedTypeNode(current)) current = current.type;
  return current;
}

function pendingEventReturnHasKey(typeNode) {
  const returned = unwrapTypeNode(typeNode);
  if (
    !returned
    || !ts.isTypeReferenceNode(returned)
    || !ts.isIdentifier(returned.typeName)
    || returned.typeName.text !== 'Array'
    || returned.typeArguments?.length !== 1
  ) return false;
  const element = unwrapTypeNode(returned.typeArguments[0]);
  if (!element || !ts.isTypeLiteralNode(element)) return false;
  const keyMembers = element.members.filter((member) => (
    ts.isPropertySignature(member) && declarationMemberName(member) === 'key'
  ));
  return keyMembers.length === 1
    && keyMembers[0].questionToken === undefined
    && keyMembers[0].type?.kind === ts.SyntaxKind.StringKeyword;
}

function parseDeclarationSource(normalized, limits, label) {
  if (normalized.source.includes('\0')) fail(label, 'declaration source contains NUL');
  const sourceFile = ts.createSourceFile(
    normalized.path,
    normalized.source,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.TS,
  );
  if (sourceFile.parseDiagnostics.length > 0) {
    const diagnostic = sourceFile.parseDiagnostics[0];
    fail(label, `declaration source is invalid at offset ${diagnostic.start ?? 0}: ${diagnostic.messageText}`);
  }
  let nodes = 0;
  const visit = (node) => {
    nodes += 1;
    if (nodes > limits.maxAstNodes) {
      fail(label, `declaration AST exceeds ${limits.maxAstNodes} nodes`);
    }
    ts.forEachChild(node, visit);
  };
  visit(sourceFile);
  return sourceFile;
}

/** Validate the TypeScript declaration half of the HostWaitKey contract. */
export function validateHostWaitKeyDeclarations(file, options = {}) {
  const label = normalizeLabel(options.label);
  const limits = normalizeLimits(options.limits, label);
  const normalized = requireSourceFile(file, label);
  requirePortableModulePath(normalized.path, label);
  if (utf8Size(normalized.source) > limits.maxFileSourceBytes) {
    fail(label, `${normalized.path} exceeds ${limits.maxFileSourceBytes} UTF-8 source bytes`);
  }
  const sourceFile = parseDeclarationSource(normalized, limits, label);
  const interfaces = sourceFile.statements.filter((statement) => (
    ts.isInterfaceDeclaration(statement)
    && statement.name.text === 'VoVm'
    && hasModifier(statement, ts.SyntaxKind.ExportKeyword)
  ));
  if (interfaces.length !== 1 || interfaces[0].heritageClauses?.length) {
    fail(label, 'declarations must directly export exactly one non-inherited VoVm interface');
  }
  const methods = (name) => interfaces[0].members.filter((member) => (
    ts.isMethodSignature(member) && declarationMemberName(member) === name
  ));
  const pending = methods('takePendingHostEvents');
  const pendingHasKey = pending.length === 1
    && pending[0].parameters.length === 0
    && pendingEventReturnHasKey(pending[0].type);
  if (!pendingHasKey) {
    fail(label, 'takePendingHostEvents declaration must return records containing key: string');
  }
  const wake = methods('wakeHostEvent');
  const wakeParameter = wake[0]?.parameters[0];
  const wakeAcceptsKey = wake.length === 1
    && wake[0].parameters.length === 1
    && wakeParameter.name?.kind === ts.SyntaxKind.Identifier
    && wakeParameter.name.text === 'key'
    && wakeParameter.type?.kind === ts.SyntaxKind.StringKeyword
    && wakeParameter.questionToken === undefined
    && wakeParameter.dotDotDotToken === undefined
    && wakeParameter.initializer === undefined
    && wake[0].type?.kind === ts.SyntaxKind.VoidKeyword;
  if (!wakeAcceptsKey) {
    fail(label, 'wakeHostEvent declaration must accept key: string and return void');
  }
  return Object.freeze({ pendingHasKey, wakeAcceptsKey });
}

/**
 * Validate semantic HostWaitKey use in the parsed renderer bridge plus its
 * declaration file. The required calls must exist as AST nodes; comments and
 * literal strings cannot satisfy the contract.
 */
export function validateVoplayHostWaitKeyContract({
  graph,
  declarations,
  bridgePath = DEFAULT_BRIDGE_PATH,
  limits,
  label: rawLabel,
}) {
  const label = normalizeLabel(rawLabel);
  normalizeLimits(limits, label);
  if (
    graph === null
    || typeof graph !== 'object'
    || graph[GRAPH_BRAND] !== true
    || !Array.isArray(graph.files)
  ) {
    fail(label, 'graph must be the result of collectQuickplayLocalJavaScriptGraph');
  }
  requirePortableModulePath(bridgePath, label);
  const bridge = graph.files.find((file) => file.path === bridgePath);
  if (!bridge) fail(label, `renderer graph must reach ${bridgePath}`);
  const semantics = analyzeHostWaitKeyAst(bridge, `${label} ${bridgePath}`);
  const declarationContract = validateHostWaitKeyDeclarations(declarations, { label, limits });
  return Object.freeze({
    bridgePath,
    declarationContract,
    semantics,
  });
}

/** Convenience API used by the validator when it has a package read callback. */
export function validateVoplayRendererContract({
  entryPath,
  readSource,
  declarations,
  bridgePath = DEFAULT_BRIDGE_PATH,
  limits,
  label,
}) {
  const graph = collectQuickplayLocalJavaScriptGraph({ entryPath, readSource, limits, label });
  const hostWaitKey = validateVoplayHostWaitKeyContract({
    graph,
    declarations,
    bridgePath,
    limits,
    label,
  });
  return Object.freeze({ graph, hostWaitKey });
}
