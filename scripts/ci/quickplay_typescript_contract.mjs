import { createRequire } from 'node:module';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const ciDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(ciDir, '..', '..');
const studioRequire = createRequire(join(repoRoot, 'apps', 'studio', 'package.json'));
const ts = studioRequire('typescript');

const MAX_SOURCE_BYTES = 16 * 1024 * 1024;
const MAX_AST_NODES = 500_000;
const MAX_AST_DEPTH = 256;
const MAX_CONSTANT_FOLD_STEPS = 100_000;
const MAX_CONSTANT_FOLD_DEPTH = 128;
const MAX_CONSTANT_STRING_BYTES = 1024 * 1024;
const MAX_CONSTANT_FOLD_OUTPUT_BYTES = 32 * 1024 * 1024;
const MAX_CONSTANT_FOLD_WORK_BYTES = 64 * 1024 * 1024;
const MAX_CONSTANT_COLLECTION_ENTRIES = 100_000;
const BLOCKKART_ARTIFACT_URL_PREFIX = '/quickplay/blockkart/artifacts/';
const CONTRACT_FILE_NAME = '/__volang_ci__/quickplay.ts';

const EXPECTED_PACKAGE_URLS = new Map([
  ['BLOCKKART_PROJECT_PACKAGE_URL', '/quickplay/blockkart/project.json'],
  ['BLOCKKART_DEPS_PACKAGE_URL', '/quickplay/blockkart/deps.json'],
]);
const TARGET_EXPORTS = new Set(['staticPackageUrl', ...EXPECTED_PACKAGE_URLS.keys()]);
const SIDE_EFFECT_FREE_BINARY_OPERATORS = new Set([
  ts.SyntaxKind.EqualsEqualsToken,
  ts.SyntaxKind.ExclamationEqualsToken,
  ts.SyntaxKind.EqualsEqualsEqualsToken,
  ts.SyntaxKind.ExclamationEqualsEqualsToken,
  ts.SyntaxKind.LessThanToken,
  ts.SyntaxKind.LessThanEqualsToken,
  ts.SyntaxKind.GreaterThanToken,
  ts.SyntaxKind.GreaterThanEqualsToken,
  ts.SyntaxKind.AmpersandAmpersandToken,
  ts.SyntaxKind.BarBarToken,
  ts.SyntaxKind.QuestionQuestionToken,
]);
const URL_RETURN_PROPERTY_TOKENS = new Map([
  ['href', 'full-url'],
  ['origin', 'origin'],
  ['pathname', 'path'],
  ['search', 'query'],
  ['hash', 'hash'],
]);
const UNKNOWN_CONSTANT = Object.freeze({ known: false });

function fail(label, message) {
  throw new Error(`${label}: ${message}`);
}

function hasModifier(node, kind) {
  return node.modifiers?.some((modifier) => modifier.kind === kind) === true;
}

function isDirectNamedExport(node) {
  return hasModifier(node, ts.SyntaxKind.ExportKeyword)
    && !hasModifier(node, ts.SyntaxKind.DefaultKeyword);
}

function isPlainConstDeclarationList(declarationList) {
  return declarationList.flags === ts.NodeFlags.Const;
}

function visitBindingIdentifiers(name, visitor) {
  if (ts.isIdentifier(name)) {
    visitor(name.text);
    return;
  }
  for (const element of name.elements) {
    if (!ts.isOmittedExpression(element)) {
      visitBindingIdentifiers(element.name, visitor);
    }
  }
}

function incrementTargetExport(exportCounts, name) {
  if (TARGET_EXPORTS.has(name)) {
    exportCounts.set(name, exportCounts.get(name) + 1);
  }
}

function collectTargetExports(sourceFile, label) {
  const exportCounts = new Map([...TARGET_EXPORTS].map((name) => [name, 0]));

  for (const statement of sourceFile.statements) {
    if (ts.isExportDeclaration(statement)) {
      if (statement.exportClause === undefined) {
        fail(label, 'export-star declarations cannot prove unique Quickplay target exports');
      }
      if (ts.isNamedExports(statement.exportClause)) {
        for (const element of statement.exportClause.elements) {
          incrementTargetExport(exportCounts, element.name.text);
        }
      }
      continue;
    }

    if (!isDirectNamedExport(statement)) {
      continue;
    }
    if (ts.isVariableStatement(statement)) {
      for (const declaration of statement.declarationList.declarations) {
        visitBindingIdentifiers(declaration.name, (name) => incrementTargetExport(exportCounts, name));
      }
      continue;
    }
    if (statement.name !== undefined && ts.isIdentifier(statement.name)) {
      incrementTargetExport(exportCounts, statement.name.text);
    }
  }

  for (const [name, count] of exportCounts) {
    if (count !== 1) {
      fail(label, `expected exactly one named export ${name}, found ${count}`);
    }
  }
}

function firstParseDiagnostic(sourceFile) {
  const diagnostic = sourceFile.parseDiagnostics?.[0];
  if (diagnostic === undefined) {
    return undefined;
  }
  const message = ts.flattenDiagnosticMessageText(diagnostic.messageText, '\n');
  if (diagnostic.start === undefined) {
    return message;
  }
  const position = sourceFile.getLineAndCharacterOfPosition(diagnostic.start);
  return `${position.line + 1}:${position.character + 1}: ${message}`;
}

function validateAstResourcesAndLiterals(sourceFile, label) {
  let nodeCount = 0;

  function visit(node, depth) {
    nodeCount += 1;
    if (nodeCount > MAX_AST_NODES) {
      fail(label, `TypeScript AST exceeds ${MAX_AST_NODES} nodes`);
    }
    if (depth > MAX_AST_DEPTH) {
      fail(label, `TypeScript AST exceeds depth ${MAX_AST_DEPTH}`);
    }

    if ((ts.isStringLiteral(node) || ts.isNoSubstitutionTemplateLiteral(node))
      && node.text.includes(BLOCKKART_ARTIFACT_URL_PREFIX)) {
      fail(label, `hard-coded BlockKart artifact URL is forbidden: ${BLOCKKART_ARTIFACT_URL_PREFIX}`);
    }

    ts.forEachChild(node, (child) => visit(child, depth + 1));
  }

  visit(sourceFile, 0);
}

function unwrapParentheses(expression) {
  let current = expression;
  while (ts.isParenthesizedExpression(current)) {
    current = current.expression;
  }
  return current;
}

function createTypeChecker(sourceFile) {
  const options = {
    module: ts.ModuleKind.ESNext,
    noLib: true,
    noResolve: true,
    target: ts.ScriptTarget.Latest,
  };
  const host = ts.createCompilerHost(options, true);
  host.fileExists = (fileName) => fileName === CONTRACT_FILE_NAME;
  host.getSourceFile = (fileName) => (fileName === CONTRACT_FILE_NAME ? sourceFile : undefined);
  host.readFile = (fileName) => (fileName === CONTRACT_FILE_NAME ? sourceFile.text : undefined);
  const program = ts.createProgram({
    rootNames: [CONTRACT_FILE_NAME],
    options,
    host,
  });
  return program.getTypeChecker();
}

function sameBoundSymbol(checker, left, right) {
  const leftSymbol = checker.getSymbolAtLocation(left);
  return leftSymbol !== undefined && leftSymbol === checker.getSymbolAtLocation(right);
}

function buildSearchParamUrlIdentifier(node) {
  if (!ts.isCallExpression(node) || node.questionDotToken !== undefined) {
    return undefined;
  }
  const callee = unwrapParentheses(node.expression);
  if (!ts.isPropertyAccessExpression(callee)
    || callee.questionDotToken !== undefined
    || callee.name.text !== 'set') {
    return undefined;
  }
  const searchParams = unwrapParentheses(callee.expression);
  if (!ts.isPropertyAccessExpression(searchParams)
    || searchParams.questionDotToken !== undefined
    || searchParams.name.text !== 'searchParams') {
    return undefined;
  }
  const url = unwrapParentheses(searchParams.expression);
  if (!ts.isIdentifier(url) || url.text !== 'url'
    || node.arguments.length < 1
    || !ts.isStringLiteral(unwrapParentheses(node.arguments[0]))
    || unwrapParentheses(node.arguments[0]).text !== 'build') {
    return undefined;
  }
  return url;
}

function isBuildSearchParamCall(node) {
  return buildSearchParamUrlIdentifier(node) !== undefined;
}

function isExactBuildSearchParamCall(checker, sourceFile, node) {
  const buildId = node.arguments.length === 2
    ? unwrapParentheses(node.arguments[1])
    : undefined;
  const buildIdSymbol = buildId === undefined ? undefined : checker.getSymbolAtLocation(buildId);
  return isBuildSearchParamCall(node)
    && node.arguments.length === 2
    && ts.isIdentifier(buildId)
    && buildId.text === '__STUDIO_BUILD_ID__'
    && !buildIdSymbol?.declarations?.some((declaration) => declaration.getSourceFile() === sourceFile);
}

function collectBuildCallsOutsideNestedFunctions(body) {
  const calls = [];

  function visit(node) {
    if (isBuildSearchParamCall(node)) {
      calls.push(node);
    }
    if (ts.isFunctionLike(node) || ts.isClassLike(node)) {
      return;
    }
    ts.forEachChild(node, visit);
  }

  for (const statement of body.statements) {
    visit(statement);
  }
  return calls;
}

function hasDeclarationInSource(checker, identifier, sourceFile) {
  return checker.getSymbolAtLocation(identifier)?.declarations
    ?.some((declaration) => declaration.getSourceFile() === sourceFile) === true;
}

function isWindowLocationOrigin(checker, sourceFile, expression) {
  const origin = unwrapParentheses(expression);
  if (!ts.isPropertyAccessExpression(origin)
    || origin.questionDotToken !== undefined
    || origin.name.text !== 'origin') {
    return false;
  }
  const location = unwrapParentheses(origin.expression);
  const windowIdentifier = ts.isPropertyAccessExpression(location)
    ? unwrapParentheses(location.expression)
    : undefined;
  return ts.isPropertyAccessExpression(location)
    && location.questionDotToken === undefined
    && location.name.text === 'location'
    && ts.isIdentifier(windowIdentifier)
    && windowIdentifier.text === 'window'
    && !hasDeclarationInSource(checker, windowIdentifier, sourceFile);
}

function validateUrlBinding(checker, sourceFile, declaration, buildCall, label) {
  const body = declaration.body;
  const urlUse = buildSearchParamUrlIdentifier(buildCall);
  const urlSymbol = checker.getSymbolAtLocation(urlUse);
  if (urlSymbol === undefined) {
    fail(label, 'staticPackageUrl build query must target a locally bound URL');
  }
  const urlDeclarations = (urlSymbol.declarations ?? []).filter(ts.isVariableDeclaration);
  if (urlDeclarations.length !== 1) {
    fail(label, `staticPackageUrl URL binding must have one declaration, found ${urlDeclarations.length}`);
  }

  const urlDeclaration = urlDeclarations[0];
  const declarationList = urlDeclaration.parent;
  const variableStatement = declarationList.parent;
  if (!ts.isVariableDeclarationList(declarationList)
    || !ts.isVariableStatement(variableStatement)
    || variableStatement.parent !== body
    || declarationList.declarations.length !== 1
    || !isPlainConstDeclarationList(declarationList)
    || body.statements[0] !== variableStatement
    || body.statements[1] === undefined
    || !ts.isExpressionStatement(body.statements[1])
    || unwrapParentheses(body.statements[1].expression) !== buildCall) {
    fail(label, 'the const URL binding and build query call must be the first two function statements');
  }

  if (declaration.parameters.length !== 1
    || !ts.isIdentifier(declaration.parameters[0].name)
    || declaration.parameters[0].name.text !== 'path') {
    fail(label, 'staticPackageUrl must have exactly one path parameter');
  }
  const pathParameter = declaration.parameters[0].name;
  const initializer = urlDeclaration.initializer === undefined
    ? undefined
    : unwrapParentheses(urlDeclaration.initializer);
  const isNewUrl = initializer !== undefined && ts.isNewExpression(initializer);
  const constructor = isNewUrl ? unwrapParentheses(initializer.expression) : undefined;
  const args = isNewUrl ? initializer.arguments : undefined;
  if (!isNewUrl
    || !ts.isIdentifier(constructor)
    || constructor.text !== 'URL'
    || hasDeclarationInSource(checker, constructor, sourceFile)
    || args === undefined
    || args.length !== 2
    || !ts.isIdentifier(unwrapParentheses(args[0]))
    || !sameBoundSymbol(checker, pathParameter, unwrapParentheses(args[0]))
    || !isWindowLocationOrigin(checker, sourceFile, args[1])) {
    fail(label, 'URL binding must be const url = new URL(path, window.location.origin)');
  }

  return urlSymbol;
}

function isSideEffectFreeCondition(expression) {
  const node = unwrapParentheses(expression);
  if (ts.isIdentifier(node)
    || ts.isStringLiteral(node)
    || ts.isNumericLiteral(node)
    || node.kind === ts.SyntaxKind.TrueKeyword
    || node.kind === ts.SyntaxKind.FalseKeyword
    || node.kind === ts.SyntaxKind.NullKeyword) {
    return true;
  }
  if (ts.isPropertyAccessExpression(node)) {
    return node.questionDotToken === undefined
      && isSideEffectFreeCondition(node.expression);
  }
  if (ts.isPrefixUnaryExpression(node)) {
    return node.operator === ts.SyntaxKind.ExclamationToken
      && isSideEffectFreeCondition(node.operand);
  }
  if (ts.isConditionalExpression(node)) {
    return isSideEffectFreeCondition(node.condition)
      && isSideEffectFreeCondition(node.whenTrue)
      && isSideEffectFreeCondition(node.whenFalse);
  }
  if (ts.isBinaryExpression(node)) {
    return SIDE_EFFECT_FREE_BINARY_OPERATORS.has(node.operatorToken.kind)
      && isSideEffectFreeCondition(node.left)
      && isSideEffectFreeCondition(node.right);
  }
  return false;
}

function pushLiteralReturnToken(text, tokens) {
  if (text.length === 0) {
    return true;
  }
  if (text === '?') {
    tokens.push('query-delimiter');
    return true;
  }
  return false;
}

function collectReturnTokens(checker, expression, urlSymbol, tokens) {
  const node = unwrapParentheses(expression);
  if (ts.isStringLiteral(node)
    || ts.isNoSubstitutionTemplateLiteral(node)) {
    return pushLiteralReturnToken(node.text, tokens);
  }

  if (ts.isPropertyAccessExpression(node)) {
    if (node.questionDotToken !== undefined) {
      return false;
    }
    const receiver = unwrapParentheses(node.expression);
    if (!ts.isIdentifier(receiver) || checker.getSymbolAtLocation(receiver) !== urlSymbol) {
      return false;
    }
    const token = URL_RETURN_PROPERTY_TOKENS.get(node.name.text);
    if (token === undefined) {
      return false;
    }
    tokens.push(token);
    return true;
  }

  if (ts.isCallExpression(node)
    && node.questionDotToken === undefined
    && node.arguments.length === 0) {
    const callee = unwrapParentheses(node.expression);
    if (ts.isPropertyAccessExpression(callee)
      && callee.questionDotToken === undefined
      && callee.name.text === 'toString') {
      const receiver = unwrapParentheses(callee.expression);
      if (ts.isIdentifier(receiver) && checker.getSymbolAtLocation(receiver) === urlSymbol) {
        tokens.push('full-url');
        return true;
      }
      if (ts.isPropertyAccessExpression(receiver)
        && receiver.questionDotToken === undefined
        && receiver.name.text === 'searchParams'
        && ts.isIdentifier(unwrapParentheses(receiver.expression))
        && checker.getSymbolAtLocation(unwrapParentheses(receiver.expression)) === urlSymbol) {
        tokens.push('serialized-params');
        return true;
      }
    }
    return false;
  }

  if (ts.isTemplateExpression(node)) {
    if (!pushLiteralReturnToken(node.head.text, tokens)) {
      return false;
    }
    for (const span of node.templateSpans) {
      if (!collectReturnTokens(checker, span.expression, urlSymbol, tokens)
        || !pushLiteralReturnToken(span.literal.text, tokens)) {
        return false;
      }
    }
    return true;
  }

  if (ts.isBinaryExpression(node) && node.operatorToken.kind === ts.SyntaxKind.PlusToken) {
    return collectReturnTokens(checker, node.left, urlSymbol, tokens)
      && collectReturnTokens(checker, node.right, urlSymbol, tokens);
  }

  return false;
}

function returnTokensPreserveUrl(tokens) {
  if (tokens.length === 1 && tokens[0] === 'full-url') {
    return true;
  }

  let stage = 'prefix';
  let originCount = 0;
  let pathCount = 0;
  let queryCount = 0;
  let hashCount = 0;

  for (const token of tokens) {
    if (token === 'origin' && stage === 'prefix' && originCount === 0) {
      originCount += 1;
      continue;
    }
    if (token === 'path' && stage === 'prefix' && pathCount === 0) {
      pathCount += 1;
      stage = 'after-path';
      continue;
    }
    if (token === 'query' && stage === 'after-path' && queryCount === 0) {
      queryCount += 1;
      stage = 'after-query';
      continue;
    }
    if (token === 'query-delimiter' && stage === 'after-path' && queryCount === 0) {
      stage = 'after-query-delimiter';
      continue;
    }
    if (token === 'serialized-params'
      && stage === 'after-query-delimiter'
      && queryCount === 0) {
      queryCount += 1;
      stage = 'after-query';
      continue;
    }
    if (token === 'hash' && stage === 'after-query' && hashCount === 0) {
      hashCount += 1;
      stage = 'after-hash';
      continue;
    }
    return false;
  }

  return pathCount === 1 && queryCount === 1
    && (stage === 'after-query' || stage === 'after-hash');
}

function returnExpressionPreservesUrl(checker, expression, urlSymbol) {
  const tokens = [];
  return collectReturnTokens(checker, expression, urlSymbol, tokens)
    && returnTokensPreserveUrl(tokens);
}

function validatePostBuildStatements(checker, statements, urlSymbol, label) {
  let alwaysReturns = false;
  for (const statement of statements) {
    if (alwaysReturns) {
      fail(label, 'staticPackageUrl contains dead code after a guaranteed return');
    }
    alwaysReturns = validatePostBuildStatement(checker, statement, urlSymbol, label);
  }
  return alwaysReturns;
}

function validatePostBuildStatement(checker, statement, urlSymbol, label) {
  if (ts.isBlock(statement)) {
    return validatePostBuildStatements(checker, statement.statements, urlSymbol, label);
  }
  if (ts.isIfStatement(statement)) {
    if (!isSideEffectFreeCondition(statement.expression)) {
      fail(label, 'staticPackageUrl branches after build binding must be side-effect free');
    }
    const thenReturns = validatePostBuildStatement(
      checker,
      statement.thenStatement,
      urlSymbol,
      label,
    );
    const elseReturns = statement.elseStatement === undefined
      ? false
      : validatePostBuildStatement(checker, statement.elseStatement, urlSymbol, label);
    return thenReturns && elseReturns;
  }
  if (ts.isReturnStatement(statement) && statement.expression !== undefined) {
    if (!returnExpressionPreservesUrl(checker, statement.expression, urlSymbol)) {
      fail(label, 'every staticPackageUrl return must preserve the bound URL path and build query');
    }
    return true;
  }
  fail(label, 'staticPackageUrl permits only side-effect-free branches and URL-derived returns after binding');
}

function validateStaticPackageUrl(sourceFile, checker, label) {
  const declarations = sourceFile.statements.filter((statement) => (
    ts.isFunctionDeclaration(statement)
    && statement.name?.text === 'staticPackageUrl'
    && isDirectNamedExport(statement)
  ));
  if (declarations.length !== 1) {
    fail(label, `expected one directly exported function staticPackageUrl, found ${declarations.length}`);
  }

  const declaration = declarations[0];
  if (declaration.body === undefined) {
    fail(label, 'staticPackageUrl must have an implementation body');
  }
  const pathParameter = declaration.parameters[0];
  if (declaration.asteriskToken !== undefined
    || hasModifier(declaration, ts.SyntaxKind.AsyncKeyword)
    || declaration.typeParameters !== undefined
    || declaration.type?.kind !== ts.SyntaxKind.StringKeyword
    || pathParameter?.initializer !== undefined
    || pathParameter?.questionToken !== undefined
    || pathParameter?.dotDotDotToken !== undefined) {
    fail(label, 'staticPackageUrl must be a synchronous, non-generic string function');
  }
  if (pathParameter !== undefined
    && pathParameter.type?.kind !== ts.SyntaxKind.StringKeyword) {
    fail(label, 'staticPackageUrl path parameter must be explicitly typed as string');
  }
  if ((pathParameter?.modifiers?.length ?? 0) !== 0) {
    fail(label, 'staticPackageUrl path parameter cannot have decorators or modifiers');
  }

  const directBuildCalls = declaration.body.statements
    .filter(ts.isExpressionStatement)
    .map((statement) => unwrapParentheses(statement.expression))
    .filter(isBuildSearchParamCall);
  const allBuildCalls = collectBuildCallsOutsideNestedFunctions(declaration.body);
  if (allBuildCalls.length !== 1 || directBuildCalls.length !== 1
    || !isExactBuildSearchParamCall(checker, sourceFile, directBuildCalls[0])) {
    fail(
      label,
      "staticPackageUrl must directly call url.searchParams.set('build', __STUDIO_BUILD_ID__) exactly once",
    );
  }

  const urlSymbol = validateUrlBinding(checker, sourceFile, declaration, directBuildCalls[0], label);
  const statementsAfterBuild = declaration.body.statements.slice(2);
  if (statementsAfterBuild.length === 0) {
    fail(label, 'staticPackageUrl must return the build-versioned URL');
  }
  if (!validatePostBuildStatements(checker, statementsAfterBuild, urlSymbol, label)) {
    fail(label, 'staticPackageUrl must return the build-versioned URL on every control-flow path');
  }

  const functionSymbol = checker.getSymbolAtLocation(declaration.name);
  if (functionSymbol === undefined) {
    fail(label, 'staticPackageUrl export must resolve to its function declaration');
  }
  return functionSymbol;
}

function validatePackageUrlInitializer(checker, functionSymbol, declaration, expectedUrl, exportName, label) {
  if (declaration.initializer === undefined) {
    fail(label, `${exportName} must have an initializer`);
  }
  const initializer = unwrapParentheses(declaration.initializer);
  if (!ts.isCallExpression(initializer)
    || !ts.isIdentifier(unwrapParentheses(initializer.expression))
    || unwrapParentheses(initializer.expression).text !== 'staticPackageUrl'
    || checker.getSymbolAtLocation(unwrapParentheses(initializer.expression)) !== functionSymbol
    || initializer.arguments.length !== 1
    || !ts.isStringLiteral(unwrapParentheses(initializer.arguments[0]))
    || unwrapParentheses(initializer.arguments[0]).text !== expectedUrl) {
    fail(label, `${exportName} must equal staticPackageUrl('${expectedUrl}')`);
  }
}

function validatePackageUrlExports(sourceFile, checker, functionSymbol, label) {
  const declarations = new Map([...EXPECTED_PACKAGE_URLS.keys()].map((name) => [name, []]));

  for (const statement of sourceFile.statements) {
    if (!ts.isVariableStatement(statement) || !isDirectNamedExport(statement)) {
      continue;
    }
    const isConst = isPlainConstDeclarationList(statement.declarationList);
    for (const declaration of statement.declarationList.declarations) {
      if (!ts.isIdentifier(declaration.name) || !declarations.has(declaration.name.text)) {
        continue;
      }
      if (!isConst) {
        fail(label, `${declaration.name.text} must be declared with const`);
      }
      declarations.get(declaration.name.text).push(declaration);
    }
  }

  for (const [exportName, expectedUrl] of EXPECTED_PACKAGE_URLS) {
    const matches = declarations.get(exportName);
    if (matches.length !== 1) {
      fail(label, `expected one directly exported const ${exportName}, found ${matches.length}`);
    }
    validatePackageUrlInitializer(checker, functionSymbol, matches[0], expectedUrl, exportName, label);
  }
}

function knownConstant(value) {
  return Object.freeze({ known: true, value });
}

function unwrapConstantExpression(expression, context, label, depth) {
  let current = expression;
  let currentDepth = depth;
  while (ts.isParenthesizedExpression(current)
    || ts.isAsExpression(current)
    || ts.isTypeAssertionExpression(current)
    || ts.isNonNullExpression(current)
    || ts.isSatisfiesExpression(current)) {
    current = current.expression;
    currentDepth += 1;
    consumeConstantFoldStep(context, currentDepth, label);
  }
  return { depth: currentDepth, node: current };
}

function consumeConstantFoldStep(context, depth, label) {
  context.steps += 1;
  if (context.steps > MAX_CONSTANT_FOLD_STEPS) {
    fail(label, `constant folding exceeds ${MAX_CONSTANT_FOLD_STEPS} steps`);
  }
  if (depth > MAX_CONSTANT_FOLD_DEPTH) {
    fail(label, `constant folding exceeds depth ${MAX_CONSTANT_FOLD_DEPTH}`);
  }
}

function recordConstantString(value, context, label) {
  if (value.includes(BLOCKKART_ARTIFACT_URL_PREFIX)) {
    fail(
      label,
      `constant-folded hard-coded BlockKart artifact URL is forbidden: ${BLOCKKART_ARTIFACT_URL_PREFIX}`,
    );
  }

  const bytes = Buffer.byteLength(value, 'utf8');
  if (bytes > MAX_CONSTANT_STRING_BYTES) {
    fail(label, `constant-folded string exceeds ${MAX_CONSTANT_STRING_BYTES} UTF-8 bytes`);
  }
  context.outputBytes += bytes;
  if (context.outputBytes > MAX_CONSTANT_FOLD_OUTPUT_BYTES) {
    fail(
      label,
      `constant folding exceeds ${MAX_CONSTANT_FOLD_OUTPUT_BYTES} UTF-8 output bytes`,
    );
  }
  return value;
}

function constantStringValue(value, label) {
  if (value === null
    || typeof value === 'string'
    || typeof value === 'number'
    || typeof value === 'boolean') {
    return String(value);
  }
  fail(label, 'template and string concatenation operands must fold to primitive constants');
}

function chargeConstantStringWork(values, context, label) {
  for (const value of values) {
    if (typeof value === 'string') {
      context.workBytes += Buffer.byteLength(value, 'utf8');
    }
  }
  if (context.workBytes > MAX_CONSTANT_FOLD_WORK_BYTES) {
    fail(label, `constant folding exceeds ${MAX_CONSTANT_FOLD_WORK_BYTES} UTF-8 work bytes`);
  }
}

function concatenateConstantStrings(left, right, context, label) {
  const leftBytes = Buffer.byteLength(left, 'utf8');
  const rightBytes = Buffer.byteLength(right, 'utf8');
  context.workBytes += leftBytes + rightBytes;
  if (context.workBytes > MAX_CONSTANT_FOLD_WORK_BYTES) {
    fail(label, `constant folding exceeds ${MAX_CONSTANT_FOLD_WORK_BYTES} UTF-8 work bytes`);
  }
  if (leftBytes + rightBytes > MAX_CONSTANT_STRING_BYTES) {
    fail(label, `constant-folded string exceeds ${MAX_CONSTANT_STRING_BYTES} UTF-8 bytes`);
  }
  return recordConstantString(left + right, context, label);
}

function requireKnownConstant(result, description, label) {
  if (!result.known) {
    fail(label, `${description} must fold to a known primitive constant`);
  }
  return result.value;
}

function evaluateConstantBinary(operator, leftResult, rightResult, context, label) {
  const left = requireKnownConstant(leftResult, 'binary expression operand', label);
  const right = requireKnownConstant(rightResult, 'binary expression operand', label);
  chargeConstantStringWork([left, right], context, label);

  switch (operator) {
    case ts.SyntaxKind.PlusToken:
      if (typeof left === 'string' || typeof right === 'string') {
        return knownConstant(concatenateConstantStrings(
          constantStringValue(left, label),
          constantStringValue(right, label),
          context,
          label,
        ));
      }
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left + right);
      }
      break;
    case ts.SyntaxKind.MinusToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left - right);
      }
      break;
    case ts.SyntaxKind.AsteriskToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left * right);
      }
      break;
    case ts.SyntaxKind.SlashToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left / right);
      }
      break;
    case ts.SyntaxKind.PercentToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left % right);
      }
      break;
    case ts.SyntaxKind.AsteriskAsteriskToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left ** right);
      }
      break;
    case ts.SyntaxKind.LessThanToken:
      return knownConstant(left < right);
    case ts.SyntaxKind.LessThanEqualsToken:
      return knownConstant(left <= right);
    case ts.SyntaxKind.GreaterThanToken:
      return knownConstant(left > right);
    case ts.SyntaxKind.GreaterThanEqualsToken:
      return knownConstant(left >= right);
    case ts.SyntaxKind.EqualsEqualsEqualsToken:
      return knownConstant(left === right);
    case ts.SyntaxKind.ExclamationEqualsEqualsToken:
      return knownConstant(left !== right);
    case ts.SyntaxKind.AmpersandAmpersandToken:
      return knownConstant(left && right);
    case ts.SyntaxKind.BarBarToken:
      return knownConstant(left || right);
    case ts.SyntaxKind.QuestionQuestionToken:
      return knownConstant(left ?? right);
    case ts.SyntaxKind.LessThanLessThanToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left << right);
      }
      break;
    case ts.SyntaxKind.GreaterThanGreaterThanToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left >> right);
      }
      break;
    case ts.SyntaxKind.GreaterThanGreaterThanGreaterThanToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left >>> right);
      }
      break;
    case ts.SyntaxKind.AmpersandToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left & right);
      }
      break;
    case ts.SyntaxKind.BarToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left | right);
      }
      break;
    case ts.SyntaxKind.CaretToken:
      if (typeof left === 'number' && typeof right === 'number') {
        return knownConstant(left ^ right);
      }
      break;
    default:
      fail(label, `unsupported operator in pure top-level constant expression: ${ts.SyntaxKind[operator]}`);
  }

  fail(label, `operator ${ts.SyntaxKind[operator]} received unsupported constant operand types`);
}

function foldConstantExpression(expression, environment, context, label, depth = 0) {
  consumeConstantFoldStep(context, depth, label);
  const unwrapped = unwrapConstantExpression(expression, context, label, depth);
  const { node } = unwrapped;
  const childDepth = unwrapped.depth + 1;

  if (ts.isStringLiteral(node) || ts.isNoSubstitutionTemplateLiteral(node)) {
    return knownConstant(recordConstantString(node.text, context, label));
  }
  if (ts.isNumericLiteral(node)) {
    return knownConstant(Number(node.text.replaceAll('_', '')));
  }
  if (node.kind === ts.SyntaxKind.TrueKeyword) {
    return knownConstant(true);
  }
  if (node.kind === ts.SyntaxKind.FalseKeyword) {
    return knownConstant(false);
  }
  if (node.kind === ts.SyntaxKind.NullKeyword) {
    return knownConstant(null);
  }
  if (ts.isIdentifier(node)) {
    if (!environment.has(node.text)) {
      fail(label, `pure top-level constant expression references unavailable binding ${node.text}`);
    }
    return environment.get(node.text);
  }
  if (ts.isArrayLiteralExpression(node)) {
    context.collectionEntries += node.elements.length;
    if (context.collectionEntries > MAX_CONSTANT_COLLECTION_ENTRIES) {
      fail(label, `constant folding exceeds ${MAX_CONSTANT_COLLECTION_ENTRIES} collection entries`);
    }
    for (const element of node.elements) {
      if (ts.isSpreadElement(element)) {
        fail(label, 'pure top-level constant arrays cannot contain spread elements');
      }
      if (!ts.isOmittedExpression(element)) {
        foldConstantExpression(element, environment, context, label, childDepth);
      }
    }
    return UNKNOWN_CONSTANT;
  }
  if (ts.isTemplateExpression(node)) {
    let value = recordConstantString(node.head.text, context, label);
    for (const span of node.templateSpans) {
      const spanResult = foldConstantExpression(
        span.expression,
        environment,
        context,
        label,
        childDepth,
      );
      value = concatenateConstantStrings(
        value,
        constantStringValue(
          requireKnownConstant(spanResult, 'template expression', label),
          label,
        ),
        context,
        label,
      );
      value = concatenateConstantStrings(value, span.literal.text, context, label);
    }
    return knownConstant(value);
  }
  if (ts.isPrefixUnaryExpression(node)) {
    const operand = requireKnownConstant(
      foldConstantExpression(node.operand, environment, context, label, childDepth),
      'prefix unary operand',
      label,
    );
    if (node.operator === ts.SyntaxKind.ExclamationToken) {
      return knownConstant(!operand);
    }
    if (typeof operand === 'number') {
      if (node.operator === ts.SyntaxKind.PlusToken) {
        return knownConstant(+operand);
      }
      if (node.operator === ts.SyntaxKind.MinusToken) {
        return knownConstant(-operand);
      }
      if (node.operator === ts.SyntaxKind.TildeToken) {
        return knownConstant(~operand);
      }
    }
    fail(label, `unsupported prefix operator in pure top-level constant expression: ${ts.SyntaxKind[node.operator]}`);
  }
  if (ts.isBinaryExpression(node)) {
    const left = foldConstantExpression(node.left, environment, context, label, childDepth);
    const right = foldConstantExpression(node.right, environment, context, label, childDepth);
    return evaluateConstantBinary(node.operatorToken.kind, left, right, context, label);
  }
  if (ts.isConditionalExpression(node)) {
    const condition = requireKnownConstant(
      foldConstantExpression(node.condition, environment, context, label, childDepth),
      'conditional expression condition',
      label,
    );
    const whenTrue = foldConstantExpression(
      node.whenTrue,
      environment,
      context,
      label,
      childDepth,
    );
    const whenFalse = foldConstantExpression(
      node.whenFalse,
      environment,
      context,
      label,
      childDepth,
    );
    return condition ? whenTrue : whenFalse;
  }

  fail(
    label,
    `unsupported executable syntax in pure top-level constant expression: ${ts.SyntaxKind[node.kind]}`,
  );
}

function hasOnlyExportModifier(node) {
  return node.modifiers?.length === 1
    && node.modifiers[0].kind === ts.SyntaxKind.ExportKeyword;
}

function validatePureTopLevel(sourceFile, label) {
  const environment = new Map();
  const context = {
    collectionEntries: 0,
    outputBytes: 0,
    steps: 0,
    workBytes: 0,
  };

  for (const statement of sourceFile.statements) {
    if (ts.isFunctionDeclaration(statement)
      && statement.name?.text === 'staticPackageUrl'
      && isDirectNamedExport(statement)
      && hasOnlyExportModifier(statement)) {
      continue;
    }

    if (!ts.isVariableStatement(statement)) {
      fail(
        label,
        'top-level Quickplay code permits only the exported staticPackageUrl function and exported const declarations',
      );
    }
    const declarations = statement.declarationList.declarations;
    if (!isPlainConstDeclarationList(statement.declarationList)
      || declarations.length !== 1
      || !ts.isIdentifier(declarations[0].name)
      || declarations[0].initializer === undefined) {
      fail(label, 'top-level Quickplay constants must be single-name const declarations with initializers');
    }

    const declaration = declarations[0];
    const name = declaration.name.text;
    if (environment.has(name)) {
      fail(label, `top-level Quickplay constant ${name} is declared more than once`);
    }

    const result = EXPECTED_PACKAGE_URLS.has(name)
      ? UNKNOWN_CONSTANT
      : foldConstantExpression(declaration.initializer, environment, context, label);

    if (!isDirectNamedExport(statement) || !hasOnlyExportModifier(statement)) {
      fail(label, 'top-level Quickplay const declarations must be direct named exports');
    }
    environment.set(name, result);
  }
}

export function validateStudioQuickplayTypeScriptContract(source, { label = 'Studio quickplay.ts' } = {}) {
  if (typeof source !== 'string') {
    fail(label, 'source must be a string');
  }
  if (typeof label !== 'string' || label.length === 0) {
    throw new Error('Studio Quickplay TypeScript contract label must be a non-empty string');
  }

  const sourceBytes = Buffer.byteLength(source, 'utf8');
  if (sourceBytes > MAX_SOURCE_BYTES) {
    fail(label, `source exceeds ${MAX_SOURCE_BYTES} UTF-8 bytes`);
  }

  const sourceFile = ts.createSourceFile(
    CONTRACT_FILE_NAME,
    source,
    ts.ScriptTarget.Latest,
    true,
    ts.ScriptKind.TS,
  );
  const parseDiagnostic = firstParseDiagnostic(sourceFile);
  if (parseDiagnostic !== undefined) {
    fail(label, `TypeScript parse error at ${parseDiagnostic}`);
  }

  validateAstResourcesAndLiterals(sourceFile, label);
  collectTargetExports(sourceFile, label);
  const checker = createTypeChecker(sourceFile);
  const functionSymbol = validateStaticPackageUrl(sourceFile, checker, label);
  validatePackageUrlExports(sourceFile, checker, functionSymbol, label);
  validatePureTopLevel(sourceFile, label);

  return Object.freeze({
    sourceBytes,
    exports: Object.freeze([...TARGET_EXPORTS]),
  });
}
