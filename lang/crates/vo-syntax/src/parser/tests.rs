use super::*;

fn parse_str(source: &str) -> (File, DiagnosticSink) {
    let (file, diagnostics, _) = parse(source, 0);
    (file, diagnostics)
}

fn parse_ok(source: &str) -> File {
    let (file, diags) = parse_str(source);
    if diags.has_errors() {
        panic!("parse errors: {:?}", diags.iter().collect::<Vec<_>>());
    }
    file
}

#[test]
fn test_reject_removed_external_import_syntax() {
    let (_, diags) = parse_str(r#"import @"github.com/vo-lang/zip""#);
    assert!(diags.has_errors());
    let messages: Vec<_> = diags.iter().map(|diag| diag.message.as_str()).collect();
    assert!(messages
        .iter()
        .any(|msg| msg.contains("no longer supported")));
}

// =========================================================================
// File structure tests
// =========================================================================

#[test]
fn test_empty_file() {
    let (file, diags) = parse_str("");
    assert!(diags.is_empty());
    assert!(file.package.is_none());
    assert!(file.imports.is_empty());
    assert!(file.decls.is_empty());
}

#[test]
fn test_package_only() {
    let file = parse_ok("package main");
    assert!(file.package.is_some());
}

#[test]
fn test_package_with_import() {
    let file = parse_ok(
        r#"
            package main
            import "fmt"
        "#,
    );
    assert!(file.package.is_some());
    assert_eq!(file.imports.len(), 1);
}

#[test]
fn test_file_exposes_inline_mod_metadata() {
    let file = parse_ok(
            "/*vo:mod\nmodule local/demo\nvo ^0.1.0\nrequire github.com/acme/lib ^1.2.0\n*/\npackage main\n",
        );
    let inline_mod = file.inline_mod.expect("expected inline mod metadata");
    assert_eq!(inline_mod.module.value, "local/demo");
    assert_eq!(inline_mod.vo.value, "^0.1.0");
    assert_eq!(inline_mod.require.len(), 1);
}

#[test]
fn test_parse_reports_inline_mod_error_with_span() {
    let (_, diagnostics) = parse_str("/*vo:mod\nmodule local/demo\n*/\npackage main\n");
    assert!(diagnostics.has_errors());
    let diagnostic = diagnostics.iter().next().unwrap();
    assert_eq!(diagnostic.code, Some(SyntaxError::InvalidInlineMod.code()));
    assert!(!diagnostic.labels.is_empty());
    assert!(!diagnostic.labels[0].span.is_dummy());
}

#[test]
fn test_parse_surfaces_lexer_diagnostics() {
    let (_, diagnostics) = parse_str("package main\nvar s = \"unterminated\n");
    assert!(diagnostics.has_errors());
    assert!(diagnostics.iter().any(|diag| diag.code == Some(1010)));
}

#[test]
fn test_multiple_imports() {
    let file = parse_ok(
        r#"
            package main
            import "fmt"
            import "os"
            import "strings"
        "#,
    );
    assert_eq!(file.imports.len(), 3);
}

// =========================================================================
// Variable declaration tests
// =========================================================================

#[test]
fn test_var_decl_with_type() {
    let file = parse_ok("var x int");
    assert_eq!(file.decls.len(), 1);
    match &file.decls[0] {
        Decl::Var(v) => {
            assert_eq!(v.specs.len(), 1);
            assert_eq!(v.specs[0].names.len(), 1);
            assert!(v.specs[0].ty.is_some());
            assert!(v.specs[0].values.is_empty());
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_var_decl_with_init() {
    let file = parse_ok("var x = 42");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(v.specs[0].ty.is_none());
            assert_eq!(v.specs[0].values.len(), 1);
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_var_decl_multiple_names() {
    let file = parse_ok("var x, y, z int");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert_eq!(v.specs[0].names.len(), 3);
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_var_decl_grouped() {
    let file = parse_ok(
        r#"
            var (
                a int
                b = 1
                c, d string
            )
        "#,
    );
    match &file.decls[0] {
        Decl::Var(v) => {
            assert_eq!(v.specs.len(), 3);
        }
        _ => panic!("expected var decl"),
    }
}

// =========================================================================
// Const declaration tests
// =========================================================================

#[test]
fn test_const_decl() {
    let file = parse_ok("const x = 1");
    assert_eq!(file.decls.len(), 1);
    match &file.decls[0] {
        Decl::Const(_) => {}
        _ => panic!("expected const decl"),
    }
}

#[test]
fn test_const_decl_grouped() {
    let file = parse_ok(
        r#"
            const (
                a = 1
                b
                c = 3
            )
        "#,
    );
    match &file.decls[0] {
        Decl::Const(c) => {
            assert_eq!(c.specs.len(), 3);
        }
        _ => panic!("expected const decl"),
    }
}

// =========================================================================
// Type declaration tests
// =========================================================================

#[test]
fn test_type_decl_alias() {
    let file = parse_ok("type MyInt int");
    match &file.decls[0] {
        Decl::Type(t) => {
            assert!(matches!(t.ty.kind, TypeExprKind::Ident(_)));
        }
        _ => panic!("expected type decl"),
    }
}

#[test]
fn test_type_decl_struct() {
    let file = parse_ok(
        r#"
            type Person struct {
                name string
                age int
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Type(t) => match &t.ty.kind {
            TypeExprKind::Struct(s) => {
                assert_eq!(s.fields.len(), 2);
            }
            _ => panic!("expected struct type"),
        },
        _ => panic!("expected type decl"),
    }
}

#[test]
fn test_type_decl_pointer() {
    let file = parse_ok(
        r#"
            type CounterRef *Counter
        "#,
    );
    match &file.decls[0] {
        Decl::Type(t) => {
            assert!(matches!(t.ty.kind, TypeExprKind::Pointer(_)));
        }
        _ => panic!("expected type decl"),
    }
}

// =========================================================================
// Function declaration tests
// =========================================================================

#[test]
fn test_func_decl_simple() {
    let file = parse_ok("func main() {}");
    match &file.decls[0] {
        Decl::Func(f) => {
            assert!(f.receiver.is_none());
            assert!(f.sig.params.is_empty());
            assert!(f.sig.results.is_empty());
            assert!(f.body.is_some());
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_func_decl_with_params() {
    let file = parse_ok("func add(a, b int) int { return a + b }");
    match &file.decls[0] {
        Decl::Func(f) => {
            assert_eq!(f.sig.params.len(), 1);
            assert_eq!(f.sig.params[0].names.len(), 2);
            assert_eq!(f.sig.results.len(), 1);
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_func_decl_with_receiver() {
    let file = parse_ok("func (c Counter) Inc() { c.count++ }");
    match &file.decls[0] {
        Decl::Func(f) => {
            assert!(f.receiver.is_some());
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_func_decl_variadic() {
    let file = parse_ok("func printf(format string, args ...int) {}");
    match &file.decls[0] {
        Decl::Func(f) => {
            assert!(f.sig.variadic);
        }
        _ => panic!("expected func decl"),
    }
}

// =========================================================================
// Interface type tests (via type declaration)
// =========================================================================

#[test]
fn test_interface_type() {
    let file = parse_ok(
        r#"
            type Reader interface {
                Read(p []byte) int
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Type(t) => match &t.ty.kind {
            TypeExprKind::Interface(i) => {
                assert_eq!(i.elems.len(), 1);
            }
            _ => panic!("expected interface type"),
        },
        _ => panic!("expected type decl"),
    }
}

// =========================================================================
// Expression tests
// =========================================================================

#[test]
fn test_expr_binary() {
    let file = parse_ok("var x = 1 + 2 * 3");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(matches!(v.specs[0].values[0].kind, ExprKind::Binary(_)));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_expr_call() {
    let file = parse_ok("var x = foo(1, 2, 3)");
    match &file.decls[0] {
        Decl::Var(v) => match &v.specs[0].values[0].kind {
            ExprKind::Call(c) => {
                assert_eq!(c.args.len(), 3);
            }
            _ => panic!("expected call expr"),
        },
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_expr_index() {
    let file = parse_ok("var x = arr[0]");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(matches!(v.specs[0].values[0].kind, ExprKind::Index(_)));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_expr_slice() {
    let file = parse_ok("var x = arr[1:3]");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(matches!(v.specs[0].values[0].kind, ExprKind::Slice(_)));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_expr_selector() {
    let file = parse_ok("var x = obj.field");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(matches!(v.specs[0].values[0].kind, ExprKind::Selector(_)));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_expr_composite_lit() {
    let file = parse_ok("var x = Point{x: 1, y: 2}");
    match &file.decls[0] {
        Decl::Var(v) => match &v.specs[0].values[0].kind {
            ExprKind::CompositeLit(c) => {
                assert_eq!(c.elems.len(), 2);
            }
            _ => panic!("expected composite lit"),
        },
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_expr_func_lit() {
    let file = parse_ok("var f = func(x int) int { return x * 2 }");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(matches!(v.specs[0].values[0].kind, ExprKind::FuncLit(_)));
        }
        _ => panic!("expected var decl"),
    }
}

// =========================================================================
// Type expression tests
// =========================================================================

#[test]
fn test_type_array() {
    let file = parse_ok("var x [10]int");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(matches!(
                v.specs[0].ty.as_ref().unwrap().kind,
                TypeExprKind::Array(_)
            ));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_type_slice() {
    let file = parse_ok("var x []int");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(matches!(
                v.specs[0].ty.as_ref().unwrap().kind,
                TypeExprKind::Slice(_)
            ));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_type_map() {
    let file = parse_ok("var x map[string]int");
    match &file.decls[0] {
        Decl::Var(v) => {
            assert!(matches!(
                v.specs[0].ty.as_ref().unwrap().kind,
                TypeExprKind::Map(_)
            ));
        }
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_type_chan() {
    let file = parse_ok("var x chan int");
    match &file.decls[0] {
        Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
            TypeExprKind::Chan(c) => {
                assert_eq!(c.dir, ChanDir::Both);
            }
            _ => panic!("expected chan type"),
        },
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_type_island() {
    let file = parse_ok("var x island");
    match &file.decls[0] {
        Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
            TypeExprKind::Island => {}
            _ => panic!("expected island type"),
        },
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_make_chan() {
    // Reference: make(chan int) should work
    let file = parse_ok("func main() { c := make(chan int, 10) }");
    assert_eq!(file.decls.len(), 1);
}

#[test]
fn test_type_send_chan() {
    let file = parse_ok("var x chan<- int");
    match &file.decls[0] {
        Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
            TypeExprKind::Chan(c) => {
                assert_eq!(c.dir, ChanDir::Send);
            }
            _ => panic!("expected chan type"),
        },
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_type_recv_chan() {
    let file = parse_ok("var x <-chan int");
    match &file.decls[0] {
        Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
            TypeExprKind::Chan(c) => {
                assert_eq!(c.dir, ChanDir::Recv);
            }
            _ => panic!("expected chan type"),
        },
        _ => panic!("expected var decl"),
    }
}

#[test]
fn test_type_func() {
    let file = parse_ok("var x func(int) string");
    match &file.decls[0] {
        Decl::Var(v) => match &v.specs[0].ty.as_ref().unwrap().kind {
            TypeExprKind::Func(f) => {
                assert_eq!(f.params.len(), 1);
                assert_eq!(f.results.len(), 1);
            }
            _ => panic!("expected func type"),
        },
        _ => panic!("expected var decl"),
    }
}

// =========================================================================
// Statement tests
// =========================================================================

#[test]
fn test_stmt_if() {
    let file = parse_ok(
        r#"
            func main() {
                if x > 0 {
                    return 1
                }
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            // Find the if statement (there may be empty statements from semicolons)
            let if_stmts: Vec<_> = body
                .stmts
                .iter()
                .filter(|s| matches!(s.kind, StmtKind::If(_)))
                .collect();
            assert_eq!(if_stmts.len(), 1);
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_if_else() {
    let file = parse_ok(
        r#"
            func main() {
                if x > 0 {
                    return 1
                } else {
                    return 0
                }
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            match &body.stmts[0].kind {
                StmtKind::If(i) => {
                    assert!(i.else_.is_some());
                }
                _ => panic!("expected if stmt"),
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_for_cond() {
    let file = parse_ok(
        r#"
            func main() {
                for x < 10 {
                    x++
                }
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            match &body.stmts[0].kind {
                StmtKind::For(f) => {
                    assert!(matches!(f.clause, ForClause::Cond(_)));
                }
                _ => panic!("expected for stmt"),
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_for_three() {
    let file = parse_ok(
        r#"
            func main() {
                for i := 0; i < 10; i++ {
                    x++
                }
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            match &body.stmts[0].kind {
                StmtKind::For(f) => {
                    assert!(matches!(f.clause, ForClause::Three { .. }));
                }
                _ => panic!("expected for stmt"),
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_switch() {
    let file = parse_ok(
        r#"
            func main() {
                switch x {
                case 1:
                    return 1
                case 2:
                    return 2
                default:
                    return 0
                }
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            match &body.stmts[0].kind {
                StmtKind::Switch(s) => {
                    assert_eq!(s.cases.len(), 3);
                }
                _ => panic!("expected switch stmt"),
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_short_var_decl() {
    let file = parse_ok(
        r#"
            func main() {
                x := 1
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            assert!(matches!(body.stmts[0].kind, StmtKind::ShortVar(_)));
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_assign() {
    let file = parse_ok(
        r#"
            func main() {
                x = 1
                y += 2
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            assert!(matches!(body.stmts[0].kind, StmtKind::Assign(_)));
            assert!(matches!(body.stmts[1].kind, StmtKind::Assign(_)));
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_inc_dec() {
    let file = parse_ok(
        r#"
            func main() {
                x++
                y--
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            match &body.stmts[0].kind {
                StmtKind::IncDec(i) => assert!(i.is_inc),
                _ => panic!("expected inc/dec stmt"),
            }
            match &body.stmts[1].kind {
                StmtKind::IncDec(i) => assert!(!i.is_inc),
                _ => panic!("expected inc/dec stmt"),
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_return() {
    let file = parse_ok(
        r#"
            func main() {
                return 1, 2
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            match &body.stmts[0].kind {
                StmtKind::Return(r) => {
                    assert_eq!(r.values.len(), 2);
                }
                _ => panic!("expected return stmt"),
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_go_defer() {
    let file = parse_ok(
        r#"
            func main() {
                go foo()
                defer bar()
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            assert!(matches!(body.stmts[0].kind, StmtKind::Go(_)));
            assert!(matches!(body.stmts[1].kind, StmtKind::Defer(_)));
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_stmt_go_island_syntax() {
    // Test go @(island) syntax with space
    let file = parse_ok(
        r#"
            func main() {
                go @(i) foo()
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            if let StmtKind::Go(go_stmt) = &body.stmts[0].kind {
                assert!(go_stmt.target_island.is_some());
                assert!(matches!(go_stmt.call.kind, ExprKind::Call(_)));
            } else {
                panic!("expected Go statement");
            }
        }
        _ => panic!("expected func decl"),
    }

    // Test without space: go@(island)
    let file2 = parse_ok(
        r#"
            func main() {
                go@(x) bar()
            }
        "#,
    );
    match &file2.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            if let StmtKind::Go(go_stmt) = &body.stmts[0].kind {
                assert!(go_stmt.target_island.is_some());
            } else {
                panic!("expected Go statement");
            }
        }
        _ => panic!("expected func decl"),
    }

    // Test standard go (no island) still works
    let file3 = parse_ok(
        r#"
            func main() {
                go foo()
            }
        "#,
    );
    match &file3.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            if let StmtKind::Go(go_stmt) = &body.stmts[0].kind {
                assert!(go_stmt.target_island.is_none());
            } else {
                panic!("expected Go statement");
            }
        }
        _ => panic!("expected func decl"),
    }
}

// =========================================================================
// Error handling tests
// =========================================================================

#[test]
fn test_error_missing_semicolon() {
    let (_, diags) = parse_str("var x int var y int");
    assert!(diags.has_errors());
}

#[test]
fn test_error_unexpected_token() {
    let (_, diags) = parse_str("func main() { + }");
    assert!(diags.has_errors());
}

// =========================================================================
// Global position tests
// =========================================================================

#[test]
fn test_global_positions() {
    // Parse with a base offset of 100
    let (file, _, _) = parse("var x int", 100);

    // Check that spans have been offset
    assert!(file.span.start.0 >= 100);
}

// =========================================================================
// Error handling statement tests
// =========================================================================

#[test]
fn test_fail_statement_simple() {
    let file = parse_ok(
        r#"
            func foo() error {
                fail err
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            assert!(matches!(body.stmts[0].kind, StmtKind::Fail(_)));
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_fail_statement_with_call() {
    let file = parse_ok(
        r#"
            func foo() error {
                fail errors.New("failed")
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            assert!(matches!(body.stmts[0].kind, StmtKind::Fail(_)));
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_errdefer_statement() {
    let file = parse_ok(
        r#"
            func foo() error {
                errdefer cleanup()
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            assert!(matches!(body.stmts[0].kind, StmtKind::ErrDefer(_)));
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_errdefer_with_method_call() {
    let file = parse_ok(
        r#"
            func foo() error {
                errdefer tx.Rollback()
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            assert!(matches!(body.stmts[0].kind, StmtKind::ErrDefer(_)));
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_try_unwrap_expression() {
    let file = parse_ok(
        r#"
            func foo() error {
                x := bar()?
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            if let StmtKind::ShortVar(sv) = &body.stmts[0].kind {
                assert!(matches!(sv.values[0].kind, ExprKind::TryUnwrap(_)));
            } else {
                panic!("expected short var decl");
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_try_unwrap_chained() {
    let file = parse_ok(
        r#"
            func foo() error {
                x := a()?.b()?
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            if let StmtKind::ShortVar(sv) = &body.stmts[0].kind {
                // The outer expression should be TryUnwrap
                assert!(matches!(sv.values[0].kind, ExprKind::TryUnwrap(_)));
            } else {
                panic!("expected short var decl");
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_try_unwrap_as_statement() {
    let file = parse_ok(
        r#"
            func foo() error {
                doSomething()?
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            if let StmtKind::Expr(expr) = &body.stmts[0].kind {
                assert!(matches!(expr.kind, ExprKind::TryUnwrap(_)));
            } else {
                panic!("expected expr stmt");
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_combined_error_handling() {
    // Test a complete error handling pattern
    let file = parse_ok(
        r#"
            func processFile(path string) error {
                f := open(path)?
                defer f.Close()
                errdefer cleanup(path)
                
                data := read(f)?
                result := process(data)?
                
                if result == nil {
                    fail ErrNotFound
                }
                
                return nil
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            // Should have multiple statements including TryUnwrap, Defer, ErrDefer, Fail
            let has_errdefer = body
                .stmts
                .iter()
                .any(|s| matches!(s.kind, StmtKind::ErrDefer(_)));
            let has_defer = body
                .stmts
                .iter()
                .any(|s| matches!(s.kind, StmtKind::Defer(_)));
            let has_fail = body.stmts.iter().any(|s| {
                if let StmtKind::If(if_stmt) = &s.kind {
                    if_stmt
                        .then
                        .stmts
                        .iter()
                        .any(|s| matches!(s.kind, StmtKind::Fail(_)))
                } else {
                    false
                }
            });
            assert!(has_errdefer, "should have errdefer");
            assert!(has_defer, "should have defer");
            assert!(has_fail, "should have fail");
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_fail_in_if_block() {
    let file = parse_ok(
        r#"
            func foo() error {
                if x == nil {
                    fail ErrNil
                }
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            if let StmtKind::If(if_stmt) = &body.stmts[0].kind {
                assert!(matches!(if_stmt.then.stmts[0].kind, StmtKind::Fail(_)));
            } else {
                panic!("expected if stmt");
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_multiple_errdefer() {
    let file = parse_ok(
        r#"
            func foo() error {
                errdefer cleanup1()
                errdefer cleanup2()
                errdefer cleanup3()
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Func(f) => {
            let body = f.body.as_ref().unwrap();
            assert_eq!(body.stmts.len(), 3);
            for stmt in &body.stmts {
                assert!(matches!(stmt.kind, StmtKind::ErrDefer(_)));
            }
        }
        _ => panic!("expected func decl"),
    }
}

#[test]
fn test_struct_field_tag() {
    let file = parse_ok(
        r#"
            package main
            type Person struct {
                Name string `json:"name"`
                Age  int    `json:"age"`
            }
        "#,
    );
    match &file.decls[0] {
        Decl::Type(t) => {
            if let crate::ast::TypeExprKind::Struct(s) = &t.ty.kind {
                assert_eq!(s.fields.len(), 2);
                assert!(s.fields[0].tag.is_some(), "Name field should have tag");
                assert_eq!(s.fields[0].tag.as_ref().unwrap().value, r#"json:"name""#);
                assert!(s.fields[1].tag.is_some(), "Age field should have tag");
                assert_eq!(s.fields[1].tag.as_ref().unwrap().value, r#"json:"age""#);
            } else {
                panic!("expected struct type");
            }
        }
        _ => panic!("expected type decl"),
    }
}
