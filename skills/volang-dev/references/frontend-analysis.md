# Syntax And Analysis Developer Notes

## Contents

- [Syntax Crate](#syntax-crate)
- [Parser Data Flow](#parser-data-flow)
- [Inline Module Syntax](#inline-module-syntax)
- [Analysis Crate](#analysis-crate)
- [Import And VFS Flow](#import-and-vfs-flow)
- [Type Checker Flow](#type-checker-flow)
- [Common Change Recipes](#common-change-recipes)
- [Caveats](#caveats)

## Syntax Crate

`lang/crates/vo-syntax` owns lexing, parsing, AST, parse diagnostics, and formatting support.

Important files:

Paths in this list are relative to `lang/crates/vo-syntax`.

- `src/lib.rs`: public exports: `Lexer`, `Parser`, `parse`, `parse_with_interner`, `Token`, `TokenKind`, and AST types.
- `src/token.rs`: token definitions and keyword classification.
- `src/lexer.rs`: `Lexer::next_token`, comments, literals, automatic semicolon insertion.
- `src/ast.rs`: `File`, `Decl`, `StmtKind`, `ExprKind`, `TypeExprKind`, `ExprId`, `TypeExprId`, `IdentId`.
- `src/parser/mod.rs`: parser state and `Parser::parse_file`.
- `src/parser/decl.rs`: declarations and imports.
- `src/parser/expr.rs`: Pratt expression parsing, postfix operators, dynamic access, try unwrap.
- `src/parser/stmt.rs`: statements, control flow, defer/errdefer/fail/go/island/port syntax.
- `src/parser/types.rs`: type expressions.
- `src/inline_mod.rs`: leading `/*vo:mod ... */` syntax extraction.
- `src/display.rs`: source formatter.

Vo-specific tokens include `fail`, `errdefer`, `port`, `island`, postfix `?`, and dynamic access `~>`. Do not assume the keyword set equals Go's.

## Parser Data Flow

Parser constructors take source text and a global base offset:

- `Parser::new(source, base)`
- `Parser::with_interner(source, base, interner)`
- `Parser::with_interner_and_ids(source, base, interner, ids)`

The base offset normally comes from `SourceMap::file_base`, so all spans across a multi-file package share one global coordinate space.

`Parser` owns:

- source text
- `Lexer`
- current and peek tokens
- `SymbolInterner`
- `DiagnosticSink`
- `allow_composite_lit`
- `IdState` for expression/type/identifier IDs
- optional `InlineModMetadata`

`Parser::parse_file` emits:

- optional package identifier
- `inline_mod`
- imports
- top-level declarations
- file span

Diagnostics are collected rather than panicking. Parser recovery uses declaration/statement synchronization so multiple errors can be reported.

AST IDs matter. `ExprId`, `TypeExprId`, and `IdentId` are how analysis records type info, definitions, uses, selections, scopes, and dynamic access metadata. For multi-file packages, parse through APIs that preserve shared `IdState`.

## Inline Module Syntax

There are two related inline module parsers:

- `lang/crates/vo-syntax/src/inline_mod.rs`: extracts syntactic metadata into `File.inline_mod`.
- `lang/crates/vo-module/src/inline_mod.rs`: parses inline module identity and requirements for module policy.

Do not model inline modules as AST declarations. The lexer still skips the comment. Dependency semantics are not in `vo-syntax`.

## Analysis Crate

`lang/crates/vo-analysis` owns project analysis, import loading, type checking, object/type arenas, scopes, selections, and result maps.

Important files:

Paths in this list are relative to `lang/crates/vo-analysis`.

- `src/lib.rs`: public exports.
- `src/project.rs`: `analyze_project`, `analyze_project_with_options`, `Project`, `AnalysisError`.
- `src/check/checker.rs`: `Checker` and main type-check state.
- `src/check/type_info.rs`: `TypeInfo`.
- `src/check/resolver.rs`: file scopes, imports, package declarations.
- `src/check/decl.rs`, `expr.rs`, `stmt.rs`, `typexpr.rs`, `assignment.rs`: checker subsystems.
- `src/importer.rs`: abstract `Importer` trait and `ImportResult`.
- `src/vfs.rs`: stdlib/module/current-module package resolution.
- `src/objects.rs`: `TCObjects` arena and typed keys.
- `src/obj.rs`: `LangObj`, builtins, entity kinds.
- `src/scope.rs`: lexical scopes and parent lookup.
- `src/typ.rs`: `Type`.
- `src/universe.rs`: universe/builtin definitions.

`Project` contains:

- `tc_objs`
- shared `SymbolInterner`
- packages in dependency order
- main package key
- main `TypeInfo`
- parsed main files
- imported files and type infos keyed by package path
- `SourceMap`
- discovered extension manifests

## Import And VFS Flow

The analysis entry point is `analyze_project_with_options(files, vfs, options)`.

High-level sequence:

1. Create `ProjectState` with `TCObjects`, `SymbolInterner`, `SourceMap`, parser `IdState`, package cache, import in-progress set, checked package list, and extension manifests.
2. Parse main files with shared interner and ID state.
3. Discover extension manifests in the main package root.
4. Preload core package `errors` plus all source imports.
5. Type-check main package after swapping shared `TCObjects` into `Checker`.
6. Return `Project` with imported packages in dependency order.

`ProjectImporter` implements `Importer`. It:

- validates import path shape through `vo_module::identity`
- checks `internal` visibility
- calls a `Resolver`
- parses package files
- recursively preloads imports
- checks package declarations
- caches `PackageKey` by path

Resolvers in `vfs.rs` include:

- `StdSource`: stdlib packages, usually embedded.
- `ModSource`: external module cache.
- `PackageResolverMixed`: stdlib plus module resolver.
- replacing/current-module wrappers used by module and workspace compile contexts.

Relative imports (`./x`, `../x`), absolute imports, and `@version` import syntax are rejected. Do not document project-relative imports as supported.

## Type Checker Flow

`Checker::check` roughly follows:

1. package name validation
2. collect objects, imports, methods, file scopes
3. package object typing
4. delayed checks
5. initialization order
6. unused imports
7. untyped expression recording
8. escape analysis
9. go/island sendability checks

Important relationships:

- `TCObjects` stores packages, scopes, objects, types, and declaration metadata in arenas.
- `Scope` links lexical parents and object definitions.
- `LangObj` represents constants, vars, funcs, types, labels, packages, builtins, etc.
- `TypeInfo` maps AST IDs to inferred types and semantic facts.
- `Selection` records selector/method resolution.
- `Operand` and `OperandMode` track expression category during checking.

Expression checking entry points are in `check/expr.rs`. Statement checking entry points are in `check/stmt.rs`. Type expression conversion is in `check/typexpr.rs`. Assignments and multi-value expansion live in `check/assignment.rs`.

Dynamic access syntax forms `ExprKind::DynAccess` and `DynAccessOp::{Field, Index, Call, MethodCall}` in the parser. The checker records dynamic access resolution and result typing. Runtime reflection/fallback behavior belongs to codegen/runtime.

## Common Change Recipes

Adding syntax:

1. Add token/keyword in `token.rs` and lexing in `lexer.rs` if needed.
2. Add or extend AST nodes in `ast.rs`.
3. Parse in the relevant `parser/*.rs` file.
4. Update `display.rs` if formatting should preserve or normalize it.
5. Add type checker behavior.
6. Add codegen/runtime behavior if executable.
7. Add tests under `tests/lang`.
8. Run focused `./d.py test both <test-file>` and format/check commands.

Changing type rules:

1. Inspect `TypeInfo` consumers in `vo-codegen`.
2. Update checker and diagnostics.
3. Add expected-failure manifest entries, such as `expect = { fail = [...] }`, when diagnostics matter.
4. Add runtime tests when the rule affects emitted bytecode.

Changing import behavior:

1. Check `vo-module::identity` and `vo-analysis::vfs`.
2. Check `vo-engine` project context and module cache behavior.
3. Check web compile path if browser-visible.
4. Add module/project tests, not only single-file tests.

## Caveats

- `vo-syntax/README.md` has old API examples.
- `object` is not currently a keyword just because older text may mention it.
- `TryUnwrapNoErrorReturn` exists as a type error, but verify the exact checker path before claiming every `?` constraint is implemented.
- `fail` and `errdefer` have checker support; dynamic `?` and LHS-driven runtime type checks cross into codegen/runtime.
- Spec text can describe intended behavior more completely than current implementation.
