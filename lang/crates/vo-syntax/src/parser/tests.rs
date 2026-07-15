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
fn public_parse_with_state_rejects_unrepresentable_source_ranges() {
    let (file, diagnostics, _, ids) = parse_with_state(
        "package main",
        u32::MAX,
        SymbolInterner::new(),
        IdState::default(),
    );
    assert!(file.decls.is_empty());
    assert_eq!(ids.expr_id, 0);
    let matching: Vec<_> = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == Some(SyntaxError::SourceTooLarge.code()))
        .collect();
    assert_eq!(matching.len(), 1, "position error must be reported once");
    assert!(matching[0].message.contains("source range base"));
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

#[test]
fn test_func_type_preserves_variadic_marker() {
    let file = parse_ok("type Handler func(args ...int) int");
    match &file.decls[0] {
        Decl::Type(decl) => match &decl.ty.kind {
            TypeExprKind::Func(func) => {
                assert!(func.variadic);
                assert_eq!(func.params.len(), 1);
            }
            _ => panic!("expected function type"),
        },
        _ => panic!("expected type declaration"),
    }
}

#[test]
fn test_anonymous_prefix_before_variadic_parameter() {
    for source in [
        "type Handler func(int, ...string) int",
        "func handle(int, ...string) int { return 1 }",
    ] {
        let file = parse_ok(source);
        let (params, variadic) = match &file.decls[0] {
            Decl::Type(decl) => match &decl.ty.kind {
                TypeExprKind::Func(func) => (&func.params, func.variadic),
                _ => panic!("expected function type"),
            },
            Decl::Func(func) => (&func.sig.params, func.sig.variadic),
            _ => panic!("expected function declaration"),
        };
        assert!(variadic, "{source}");
        assert_eq!(params.len(), 2, "{source}");
        assert!(
            params.iter().all(|param| param.names.is_empty()),
            "{source}"
        );
    }
}

#[test]
fn test_grouped_names_are_preserved_in_function_types() {
    let file = parse_ok("type Pair func(left, right int) (first, second int)");
    let Decl::Type(decl) = &file.decls[0] else {
        panic!("expected type declaration");
    };
    let TypeExprKind::Func(func) = &decl.ty.kind else {
        panic!("expected function type");
    };
    assert_eq!(func.params.len(), 1);
    assert_eq!(func.params[0].names.len(), 2);
    assert_eq!(func.results.len(), 1);
    assert_eq!(func.results[0].names.len(), 2);
}

#[test]
fn test_rejects_non_identifier_parameter_and_result_names() {
    for source in [
        "func bad([]int string) {}",
        "func bad([]int ...string) {}",
        "type Bad func([]int string)",
        "type Bad func([]int ...string)",
        "func bad() ([]int string) { return nil }",
    ] {
        let (_, diagnostics) = parse_str(source);
        assert!(diagnostics.has_errors(), "{source}");
        assert!(
            diagnostics
                .iter()
                .any(|diag| diag.message.contains("expected identifier")),
            "missing identifier diagnostic for {source}: {:?}",
            diagnostics.iter().collect::<Vec<_>>()
        );
    }
}

#[test]
fn test_rejects_multiple_names_on_variadic_parameter() {
    for source in [
        "func bad(first, second ...int) {}",
        "func bad(prefix int, first, second ...string) {}",
        "type Bad func(first, second ...int)",
        "type Bad func(prefix int, first, second ...string)",
    ] {
        let (_, diagnostics) = parse_str(source);
        assert!(diagnostics.has_errors(), "{source}");
        assert!(
            diagnostics.iter().any(|diag| diag
                .message
                .contains("variadic parameter accepts at most one name")),
            "missing variadic-name diagnostic for {source}: {:?}",
            diagnostics.iter().collect::<Vec<_>>()
        );
    }
}

#[test]
fn test_rejects_variadic_function_results() {
    for source in [
        "type Bad func() (...int)",
        "type Bad func() ...int",
        "func bad() (value ...int) {}",
        "func bad() (int, ...string) {}",
        "func bad() ...int {}",
    ] {
        let (_, diagnostics) = parse_str(source);
        assert!(diagnostics.has_errors(), "{source}");
        assert!(
            diagnostics
                .iter()
                .any(|diag| diag.message.contains("function results cannot be variadic")),
            "missing variadic-result diagnostic for {source}: {:?}",
            diagnostics.iter().collect::<Vec<_>>()
        );
    }
}

#[test]
fn test_rejects_lossy_range_and_select_forms() {
    for (source, expected) in [
        (
            "func main() { for a, b, c := range xs {} }",
            "at most two iteration variables",
        ),
        (
            "func main() { for a += range xs {} }",
            "not a compound assignment",
        ),
        (
            "func main() { select { case a, b, c := <-ch: } }",
            "at most two assignment targets",
        ),
        (
            "func main() { select { case a += <-ch: } }",
            "not a compound assignment",
        ),
    ] {
        let (_, diags) = parse_str(source);
        assert!(
            diags.iter().any(|diag| diag.message.contains(expected)),
            "missing diagnostic containing {expected:?}: {:?}",
            diags.iter().collect::<Vec<_>>()
        );
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

#[test]
fn generic_visitor_reaches_all_executable_statement_children() {
    let source = r#"
        package main
        func main() {
            var rangeKey int
            var rangeValue int
            var values []int
            var items []int
            var sendChan chan int
            var recvChan chan int
            var recvLhs int

            for rangeKey, rangeValue = range values {}
            _ = items[sliceLow:sliceHigh:sliceMax]
            _ = map[int]int{compositeKey: compositeValue}
            select {
            case sendChan <- sendValue:
            case recvLhs = <-recvChan:
            }
        }
    "#;
    let (file, diagnostics, interner) = parse(source, 0);
    assert!(
        !diagnostics.has_errors(),
        "parse errors: {:?}",
        diagnostics.iter().collect::<Vec<_>>()
    );

    struct IdentCollector<'a> {
        interner: &'a vo_common::symbol::SymbolInterner,
        names: Vec<String>,
    }

    impl Visitor for IdentCollector<'_> {
        fn visit_ident(&mut self, ident: &Ident) {
            self.names.push(
                self.interner
                    .resolve(ident.symbol)
                    .expect("identifier symbol should be interned")
                    .to_string(),
            );
        }
    }

    let mut collector = IdentCollector {
        interner: &interner,
        names: Vec::new(),
    };
    collector.visit_file(&file);

    for expected in [
        "rangeKey",
        "rangeValue",
        "sliceMax",
        "compositeKey",
        "sendValue",
        "recvLhs",
        "recvChan",
    ] {
        assert!(
            collector.names.iter().any(|name| name == expected),
            "visitor did not reach {expected}: {:?}",
            collector.names
        );
    }
}

fn assert_nesting_limit_diagnostic(parser: &mut Parser<'_>) {
    assert_eq!(
        parser.recursion_depth, 0,
        "parser recursion budget must be restored after an error"
    );
    let diagnostics = parser.take_diagnostics();
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic.code == Some(SyntaxError::NestingTooDeep.code())
            && diagnostic.message.contains("maximum parser depth")
    }));
}

#[test]
fn parser_limits_deep_parenthesized_and_unary_expressions() {
    let excessive = MAX_PARSER_RECURSION_DEPTH + 32;
    for source in [
        format!("{}1{}", "(".repeat(excessive), ")".repeat(excessive)),
        format!("{}1", "!".repeat(excessive)),
    ] {
        let mut parser = Parser::new(&source, 0);
        assert!(parser.parse_expr().is_err());
        assert_nesting_limit_diagnostic(&mut parser);
    }

    let moderate = 64;
    let source = format!("{}1{}", "(".repeat(moderate), ")".repeat(moderate));
    let mut parser = Parser::new(&source, 0);
    assert!(parser.parse_expr().is_ok());
    assert_eq!(parser.recursion_depth, 0);
    assert!(!parser.take_diagnostics().has_errors());
}

#[test]
fn parser_limits_deep_type_expressions() {
    let excessive = MAX_PARSER_RECURSION_DEPTH + 32;
    let source = format!("{}int", "[]".repeat(excessive));
    let mut parser = Parser::new(&source, 0);
    assert!(parser.parse_type().is_err());
    assert_nesting_limit_diagnostic(&mut parser);

    let source = format!("{}int", "[]".repeat(64));
    let mut parser = Parser::new(&source, 0);
    assert!(parser.parse_type().is_ok());
    assert_eq!(parser.recursion_depth, 0);
    assert!(!parser.take_diagnostics().has_errors());
}

#[test]
fn parser_limits_left_deep_selector_and_call_trees() {
    let excessive = MAX_PARSER_RECURSION_DEPTH + 32;
    let selector = format!("value{}", ".field".repeat(excessive));
    let call = format!("value{}", "()".repeat(excessive));
    let nested_function = format!("func() {{ value{}; }}{}", "()".repeat(80), "()".repeat(80));

    for source in [selector, call, nested_function] {
        let mut parser = Parser::new(&source, 0);
        assert!(parser.parse_expr().is_err(), "{source}");
        assert_nesting_limit_diagnostic(&mut parser);
    }
}

#[test]
fn parser_accepts_expression_trees_at_the_structural_depth_boundary() {
    let selector = format!("value{}", ".field".repeat(MAX_PARSER_RECURSION_DEPTH - 1));
    let call = format!("value{}", "()".repeat(MAX_PARSER_RECURSION_DEPTH - 1));

    for source in [selector, call] {
        let mut parser = Parser::new(&source, 0);
        let expression = parser.parse_expr().expect("boundary expression must parse");
        assert_eq!(parser.expr_depth(&expression), MAX_PARSER_RECURSION_DEPTH);
        assert_eq!(parser.recursion_depth, 0);
        assert!(!parser.take_diagnostics().has_errors(), "{source}");
    }
}

#[test]
fn parser_accepts_flat_binary_chains_through_their_independent_boundary() {
    // This covers the 263-operator generated chain that originally exposed the
    // unified-depth regression, as well as the complete resource boundary.
    for operators in [263, MAX_BINARY_EXPRESSION_PATH] {
        let source = vec!["value"; operators + 1].join(" + ");
        let mut parser = Parser::new(&source, 0);
        let expression = parser.parse_expr().expect("flat binary chain must parse");
        assert_eq!(parser.expr_depth(&expression), 2);
        assert_eq!(
            parser.expr_binary_depths.get(&expression.id),
            Some(&operators)
        );
        assert_eq!(parser.recursion_depth, 0);
        assert!(!parser.take_diagnostics().has_errors());
    }
}

#[test]
fn generic_ast_visitor_walks_every_operand_at_the_binary_resource_boundary() {
    struct OperandCounter(usize);

    impl Visitor for OperandCounter {
        fn visit_ident(&mut self, _ident: &Ident) {
            self.0 += 1;
        }
    }

    let source = vec!["value"; MAX_BINARY_EXPRESSION_PATH + 1].join(" + ");
    let mut parser = Parser::new(&source, 0);
    let expression = parser.parse_expr().expect("boundary expression must parse");
    let mut counter = OperandCounter(0);
    counter.visit_expr(&expression);
    assert_eq!(counter.0, MAX_BINARY_EXPRESSION_PATH + 1);
}

#[test]
fn parser_rejects_flat_binary_chains_beyond_their_resource_boundary() {
    let operators = MAX_BINARY_EXPRESSION_PATH + 1;
    let source = vec!["value"; operators + 1].join(" + ");
    let mut parser = Parser::new(&source, 0);
    assert!(parser.parse_expr().is_err());
    assert_eq!(parser.recursion_depth, 0);
    let diagnostics = parser.take_diagnostics();
    assert!(diagnostics.iter().any(|diagnostic| {
        diagnostic.code == Some(SyntaxError::ExpressionTooComplex.code())
            && diagnostic.message.contains(&format!(
                "resource limit of {} operators",
                MAX_BINARY_EXPRESSION_PATH
            ))
    }));
}

#[test]
fn full_file_binary_limit_reports_one_stable_error_and_recovers_at_the_next_declaration() {
    let operators = MAX_BINARY_EXPRESSION_PATH + 1;
    let expression = vec!["value"; operators + 1].join(" + ");
    let source =
        format!("package main\nfunc oversized() {{ _ = {expression} }}\nfunc after() {{}}\n");
    let (file, diagnostics, interner) = parse(&source, 0);
    let complexity_errors = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.code == Some(SyntaxError::ExpressionTooComplex.code()))
        .collect::<Vec<_>>();

    assert_eq!(complexity_errors.len(), 1, "{complexity_errors:?}");
    assert!(complexity_errors[0]
        .message
        .contains("binary expression path exceeds the resource limit"));
    assert_eq!(
        diagnostics.iter().count(),
        1,
        "resource exhaustion must not produce recovery cascades"
    );
    assert!(file.decls.iter().any(|decl| {
        matches!(
            decl,
            Decl::Func(function)
                if function.name.as_str(&interner) == Some("after")
        )
    }));
}

#[test]
fn binary_path_limit_cannot_be_bypassed_with_precedence_or_postfix_wrappers() {
    let inner_operators = MAX_BINARY_EXPRESSION_PATH;
    let multiplicative = vec!["value"; inner_operators + 1].join(" * ");
    let sources = [
        // The RHS chain has a different precedence from the outer node.
        format!("head + {multiplicative}"),
        // Parentheses and a call carry the complete inner path into the outer
        // binary expression.
        format!("({multiplicative})() + tail"),
    ];

    for source in sources {
        let mut parser = Parser::new(&source, 0);
        assert!(parser.parse_expr().is_err(), "{source}");
        let diagnostics = parser.take_diagnostics();
        assert!(diagnostics.iter().any(|diagnostic| {
            diagnostic.code == Some(SyntaxError::ExpressionTooComplex.code())
                && diagnostic.message.contains("binary expression path")
        }));
    }
}

#[test]
fn parser_limits_deep_statements_and_restores_the_shared_budget() {
    let excessive = MAX_PARSER_RECURSION_DEPTH + 32;
    let labels = (0..excessive)
        .map(|index| format!("label{index}: "))
        .collect::<String>();
    let source = format!("{labels}return;");
    let mut parser = Parser::new(&source, 0);
    assert!(parser.parse_stmt().is_err());
    assert_nesting_limit_diagnostic(&mut parser);

    let source = format!("{}{}", "{".repeat(excessive), "}".repeat(excessive));
    let mut parser = Parser::new(&source, 0);
    let _ = parser.parse_stmt();
    assert_nesting_limit_diagnostic(&mut parser);

    let labels = (0..64)
        .map(|index| format!("label{index}: "))
        .collect::<String>();
    let source = format!("{labels}return;");
    let mut parser = Parser::new(&source, 0);
    assert!(parser.parse_stmt().is_ok());
    assert_eq!(parser.recursion_depth, 0);
    assert!(!parser.take_diagnostics().has_errors());
}

#[test]
fn malformed_conditions_restore_composite_literal_policy_for_recovery() {
    for source in ["if ;", "for value := range ;", "switch value + ;"] {
        let mut parser = Parser::new(source, 0);
        assert!(parser.parse_stmt().is_err(), "{source}");
        assert!(
            parser.allow_composite_lit,
            "condition parser state leaked after `{source}`"
        );
    }

    // The same invariant matters in a real block: after recovering from the
    // malformed condition, the following declaration must still recognize a
    // composite literal as one expression.
    let source = "{ if ; var value = Item{}; }";
    let mut parser = Parser::new(source, 0);
    let block_stmt = parser
        .parse_stmt()
        .expect("block recovery must preserve the following declaration");
    let StmtKind::Block(block) = block_stmt.kind else {
        panic!("expected block statement");
    };
    assert!(block.stmts.iter().any(|stmt| {
        matches!(
            &stmt.kind,
            StmtKind::Var(decl)
                if matches!(
                    decl.specs.first().and_then(|spec| spec.values.first()),
                    Some(Expr { kind: ExprKind::CompositeLit(_), .. })
                )
        )
    }));
}

#[test]
fn malformed_token_corpus_is_total_and_deterministic() {
    // Keep this corpus independent of a fuzzing runtime so every ordinary test
    // run exercises parser progress and recovery. Pairwise composition covers
    // every token class on both sides of an error boundary without making the
    // test suite probabilistic or disproportionately expensive.
    const ATOMS: &[&str] = &[
        "",
        "name",
        "_",
        "package",
        "func",
        "var",
        "type",
        "if",
        "for",
        "switch",
        "select",
        "case",
        "default",
        "return",
        "range",
        "(",
        ")",
        "[",
        "]",
        "{",
        "}",
        ",",
        ";",
        ":",
        ":=",
        "=",
        "+",
        "++",
        "<-",
        "...",
        "?",
        "~>",
        "0",
        "0x",
        "0b102",
        "1e+",
        "1__2",
        "\"ok\"",
        "\"\\x\"",
        "\"unterminated",
        "'x'",
        "'\\U00110000'",
        "`raw`",
        "`unterminated",
        "/* comment */",
        "/* unterminated",
        "π",
        "\u{200d}",
    ];

    fn signature(source: &str) -> (String, Vec<String>) {
        let (file, diagnostics, interner) = parse(source, 19);
        let ast = format!("{file:?}|{interner:?}");
        let diagnostics = diagnostics
            .iter()
            .map(|diagnostic| {
                format!(
                    "{:?}:{:?}:{}:{:?}:{:?}:{:?}",
                    diagnostic.severity,
                    diagnostic.code,
                    diagnostic.message,
                    diagnostic.labels,
                    diagnostic.notes,
                    diagnostic.suggestions
                )
            })
            .collect();
        (ast, diagnostics)
    }

    fn assert_total_and_deterministic(source: &str) {
        let first = std::panic::catch_unwind(|| signature(source))
            .unwrap_or_else(|_| panic!("parser panicked for malformed source {source:?}"));
        let second = std::panic::catch_unwind(|| signature(source))
            .unwrap_or_else(|_| panic!("parser panicked on replay for source {source:?}"));
        assert_eq!(first, second, "parser drifted for source {source:?}");
    }

    for (left_index, left) in ATOMS.iter().enumerate() {
        for (right_index, right) in ATOMS.iter().enumerate() {
            let fragment = format!("{left} {right}");
            let templates = [
                fragment.clone(),
                format!(
                    "package main\nfunc probe() {{ {fragment}; }}\nfunc after() {{ return }}\n"
                ),
                format!("package main\nvar probe = {fragment};\nvar after int\n"),
                format!("package main\ntype Probe {fragment};\ntype After int\n"),
                format!("package main\nfunc probe({fragment}) {{}}\nfunc after() {{}}\n"),
            ];
            for source in templates {
                assert_total_and_deterministic(&source);
            }

            // Add deterministic three-token paths without expanding to the
            // full Cartesian cube. This rotates every atom through the middle
            // and terminal positions across the pairwise matrix.
            let third = ATOMS[(left_index * 17 + right_index * 31) % ATOMS.len()];
            assert_total_and_deterministic(&format!(
                "package main\nfunc probe() {{ switch {left} {{ case {right}: {third}; default: }} }}\nfunc after() {{}}\n"
            ));
        }
    }
}

#[test]
fn full_parse_retains_one_bounded_syntax_diagnostic_stream() {
    let source = "~ ".repeat(crate::errors::MAX_SYNTAX_DIAGNOSTICS + 32);
    let (_, diagnostics, _) = parse(&source, 0);
    assert_eq!(diagnostics.len(), crate::errors::MAX_SYNTAX_DIAGNOSTICS + 1);
    let sentinel = diagnostics.iter().last().expect("limit sentinel");
    assert_eq!(
        sentinel.code,
        Some(SyntaxError::DiagnosticLimitExceeded.code())
    );
    assert!(sentinel.message.contains("further diagnostics suppressed"));
}
