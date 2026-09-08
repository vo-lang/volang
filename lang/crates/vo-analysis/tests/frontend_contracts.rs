//! Frontend contracts that previously disagreed across syntax variants or passes.

use std::path::PathBuf;
use vo_analysis::check::type_info::{CallInfo, CallKind};
use vo_analysis::check::TypeError;
use vo_analysis::vfs::{ModSource, PackageResolver, StdSource};
use vo_analysis::{analyze_project, AnalysisError, Project};
use vo_common::vfs::{FileSet, MemoryFs};
use vo_syntax::ast::{self, Expr, ExprKind, Visitor};

fn analyze_files(
    sources: &[(&str, &str)],
    dependencies: &[(&str, &str)],
) -> Result<Project, AnalysisError> {
    let mut files = FileSet::new(PathBuf::from("."));
    for &(path, source) in sources {
        files.files.insert(PathBuf::from(path), source.to_owned());
    }
    let mut module_fs = MemoryFs::new().with_file(
        "github.com/acme/lib/vo.mod",
        "format = 1\nmodule = \"github.com/acme/lib\"\nversion = \"0.1.0\"\nvo = \"0.1.0\"\n",
    );
    for &(path, source) in dependencies {
        module_fs = module_fs.with_file(path, source);
    }
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", "package errors\n")),
        r#mod: ModSource::with_fs(module_fs),
    };
    analyze_project(files, &resolver)
}

fn analyze(source: &str) -> Result<Project, AnalysisError> {
    analyze_files(&[("main.vo", source)], &[])
}

fn reject(source: &str, code: TypeError, span_text: &str) {
    let error = match analyze(source) {
        Err(error) => error,
        Ok(_) => panic!("unexpectedly accepted: {source}"),
    };
    let diagnostics = error.diagnostics().expect("structured diagnostics");
    let sources = error.source_map().expect("diagnostic sources");
    assert!(
        diagnostics.iter().any(|d| d.code == Some(code as u16)
            && d.labels
                .iter()
                .any(|label| sources.span_text(label.span).map(str::trim) == Some(span_text))),
        "missing {:?} at {span_text:?}: {error}; labels: {:?}",
        code,
        diagnostics
            .iter()
            .map(|d| (
                d.code,
                d.labels
                    .iter()
                    .map(|l| sources.span_text(l.span))
                    .collect::<Vec<_>>()
            ))
            .collect::<Vec<_>>()
    );
}

macro_rules! rejects {
    ($name:ident, $source:expr, $code:ident, $span:expr) => {
        #[test]
        fn $name() {
            reject($source, TypeError::$code, $span);
        }
    };
}

macro_rules! accepts {
    ($name:ident, $source:expr) => {
        #[test]
        fn $name() {
            let source = $source;
            analyze(source).unwrap_or_else(|e| panic!("{source}\n{e}"));
        }
    };
}

rejects!(
    select_const_target,
    r#"package main
const x int = 1
func main() { ch := make(chan int); select { case x = <-ch: default: } }"#,
    CannotAssign,
    "x"
);

rejects!(
    select_type_target,
    r#"package main
type X int
func main() { ch := make(chan X); select { case X = <-ch: default: } }"#,
    CannotAssign,
    "X"
);

rejects!(
    select_function_target,
    r#"package main
func f() {}
func main() { ch := make(chan func()); select { case f = <-ch: default: } }"#,
    CannotAssign,
    "f"
);

rejects!(
    select_const_ok_target,
    r#"package main
const ok bool = true
func main() { ch := make(chan int); select { case _, ok = <-ch: default: } }"#,
    CannotAssign,
    "ok"
);

rejects!(
    conversion_statement,
    r#"package main
func main() { x := 1; int64(x) }"#,
    InvalidExprStatement,
    "int64(x)"
);

rejects!(
    conversion_alias_statement,
    r#"package main
type T = int
func main() { T(1) }"#,
    InvalidExprStatement,
    "T(1)"
);

rejects!(
    conversion_parenthesized_statement,
    r#"package main
func main() { (int(1)) }"#,
    InvalidExprStatement,
    "(int(1))"
);

rejects!(
    conversion_stmt_paren,
    r#"package main
func main(){(int)(1)}"#,
    InvalidExprStatement,
    "(int)(1)"
);

rejects!(
    defer_conversion,
    r#"package main
func main() { defer int(1) }"#,
    CannotCall,
    "int(1)"
);

rejects!(
    go_conversion,
    r#"package main
func main(){go int(1)}"#,
    CannotCall,
    "int(1)"
);

rejects!(
    errdefer_conversion,
    r#"package main
func f() error {errdefer int(1);return nil};func main(){}"#,
    CannotCall,
    "int(1)"
);

rejects!(
    defer_conversion_alias,
    r#"package main
type T int;func main(){defer T(1)}"#,
    CannotCall,
    "T(1)"
);

rejects!(
    type_conversion_spread,
    r#"package main
func main() { _=int(1...) }"#,
    InvalidOp,
    "int(1...)"
);

rejects!(
    pointer_int_paren,
    r#"package main
func main(){_=(*(int))(nil)}"#,
    PointerToNonStruct,
    "(int)"
);

rejects!(
    pointer_to_pointer_conversion,
    r#"package main
type S struct{};func main(){_=(**S)(nil)}"#,
    PointerToNonStruct,
    "*S"
);

rejects!(
    main_variadic,
    r#"package main
func main(x ...int) {}"#,
    InvalidMainSignature,
    "main"
);

rejects!(
    init_variadic,
    r#"package main
func init(x ...int){}; func main(){}"#,
    InvalidInitSignature,
    "init"
);

accepts!(
    copy_tuple,
    r#"package main
func f()([]int,[]int){return []int{1},[]int{2}}; func main(){_=copy(f())}"#
);

accepts!(
    delete_tuple,
    r#"package main
func f()(map[int]int,int){return map[int]int{},1}; func main(){delete(f())}"#
);

accepts!(
    append_tuple_valid,
    r#"package main
func f()([]int,int){return []int{1},2};func main(){_=append(f())}"#
);

accepts!(
    builtin_statement_control,
    r#"package main
func main() { len("abc"); make([]int, 2) }"#
);

accepts!(
    named_pointer_deref,
    r#"package main
type S struct { A int }; type P *S
func main() { var p P = new(S); (*p).A = 1 }"#
);

accepts!(
    pointer_struct_conversion,
    r#"package main
type S struct{};func main(){_=(*S)(nil)}"#
);

accepts!(
    pointer_paren_type,
    r#"package main
type S struct{};func main(){_=(*(S))(nil)}"#
);

accepts!(
    pointer_extra_parens,
    r#"package main
type S struct{};func main(){_=(((*S)))(nil)}"#
);

accepts!(
    pointer_struct_literal,
    r#"package main
func main(){_=(*struct{})(nil)}"#
);

accepts!(
    conversion_callee_paren,
    r#"package main
func main(){_=(int)(1)}"#
);

accepts!(
    shadow_make_literal,
    r#"package main
func main(){make:=func(x int)int{return x};_=make(1)}"#
);

accepts!(
    shadow_new_literal,
    r#"package main
func main(){new:=func(x int)int{return x};_=new(1)}"#
);

accepts!(
    shadow_make_expr,
    r#"package main
func main(){make:=func(x int)int{return x};a:=1;_=make(a+1)}"#
);

accepts!(
    shadow_make_var,
    r#"package main
func main(){make:=func(x int)int{return x};a:=1;_=make(a)}"#
);

accepts!(
    builtin_make_paren,
    r#"package main
func main(){_=(make)([]int,1)}"#
);

accepts!(
    builtin_new_paren,
    r#"package main
type S struct{};func main(){_=(new)(S)}"#
);

accepts!(
    map_any_int_width,
    r#"package main
func main(){_=map[any]int{int8(1):1,int64(1):2}}"#
);

accepts!(
    map_any_named,
    r#"package main
type N int;func main(){_=map[any]int{N(1):1,int(1):2}}"#
);

accepts!(
    map_any_string_named,
    r#"package main
type S string;func main(){_=map[any]int{S("a"):1,"a":2}}"#
);

accepts!(
    map_any_bool_named,
    r#"package main
type B bool;func main(){_=map[any]int{B(true):1,true:2}}"#
);

accepts!(
    map_distinct_typed,
    r#"package main
func main(){_=map[any]int{1:1,1.0:2}}"#
);

accepts!(
    composite_duplicate_any_numeric,
    r#"package main
func main() { _ = map[any]int{1:1,1.0:2} }"#
);

accepts!(
    select_assignment_control,
    r#"package main
func main() { ch := make(chan int); x := 0; select { case x = <-ch: default: }; _ = x }"#
);

accepts!(
    fn_variadic_tuple,
    r#"package main
func f(a ...int){}; func g()(int,int){return 1,2}; func main(){ f(g()) }"#
);

accepts!(
    recursive_pointer,
    r#"package main
type S struct { X *S }; func main() {}"#
);

accepts!(
    recursive_interface_signature,
    r#"package main
type I interface { F(I) I }; func main() {}"#
);

accepts!(
    interface_signature_cycle,
    r#"package main
type I interface{F() J};type J interface{F() I};func main(){}"#
);

accepts!(
    closure_late_shadow,
    r#"package main
func main(){x:=1;f:=func()int{return x};{x:=2;_=x;_=f}}"#
);

accepts!(
    named_function_conversion,
    r#"package main
type F func();func g(){};func main(){_=F(g)}"#
);

accepts!(
    const_group_iota_fixed,
    r#"package main
const(A=iota;B;C;);func main(){assert(A==0&&B==1&&C==2)}"#
);

#[test]
fn each_invalid_expression_statement_is_checked_independently() {
    for expression in [
        "x",
        "x + 1",
        "(x + 1)",
        "[1]int{x}",
        "S{}",
        "func() {}",
        "int64(x)",
    ] {
        let source =
            format!("package main\ntype S struct{{}}; func main(){{ x:=1; _=x; {expression} }}");
        reject(&source, TypeError::InvalidExprStatement, expression);
    }
}

#[test]
fn bounds_and_builtin_errors_are_independent() {
    for (expression, code, span) in [
        ("cap(\"abc\")", TypeError::InvalidLenCapArg, "\"abc\""),
        ("cap(s)", TypeError::InvalidLenCapArg, "s"),
        (
            "make([]int, uint64(18446744073709551615))",
            TypeError::InvalidOp,
            "uint64(18446744073709551615)",
        ),
        (
            "make(chan int, uint64(18446744073709551615))",
            TypeError::InvalidOp,
            "uint64(18446744073709551615)",
        ),
        (
            "make(map[int]int, uint64(18446744073709551615))",
            TypeError::InvalidOp,
            "uint64(18446744073709551615)",
        ),
        (
            "make(port int, uint64(18446744073709551615))",
            TypeError::InvalidOp,
            "uint64(18446744073709551615)",
        ),
    ] {
        reject(
            &format!("package main\nfunc main(){{ s:=\"abc\"; _=s; _={expression} }}"),
            code,
            span,
        );
    }
}

#[test]
fn constant_key_equality_includes_conversion_and_dynamic_type() {
    for literal in [
        "map[int]int{1:1, 1.0:2}",
        "map[any]int{1:1, int(1):2}",
        "map[float32]int{16777216:1, 16777217:2}",
        "map[any]int{float32(16777216):1, float32(16777217):2}",
        "map[float64]int{0.0:1, -0.0:2}",
    ] {
        let source = format!("package main\nfunc main(){{ _={literal} }}");
        let error = analyze(&source).err().expect("duplicate key must fail");
        assert!(
            error
                .diagnostics()
                .unwrap()
                .iter()
                .any(|d| d.code == Some(TypeError::InvalidOp as u16)
                    && d.message.contains("duplicate key")),
            "{source}: {error}"
        );
    }
}

#[test]
fn invalid_tuple_element_does_not_become_a_valid_call_result() {
    reject(
        "package main\nfunc pair()([]int,string){return nil,\"bad\"};func main(){_=append(pair())}",
        TypeError::CannotAssign,
        "pair()",
    );
    reject(
        "package main\nfunc pair()(int,int){return 1,2};func main(){_=1+pair()}",
        TypeError::InvalidOp,
        "pair()",
    );
}

#[test]
fn signatures_are_visited_by_sendability_checks() {
    let bound = "[len([1]func(){func(){i:=make(island);var a any;go @(i) func(v any){}(a)}})]int";
    for declaration in [
        format!("func f(x {bound}){{}}"),
        format!("type F func(x {bound})"),
        format!("var f = func(x {bound}){{}}"),
        format!("type I interface{{F(x {bound});}}"),
        format!("func f() {{ type F func(x {bound}) }}"),
    ] {
        reject(
            &format!("package main\n{declaration};func main(){{}}"),
            TypeError::GoIslandNotSendable,
            "a",
        );
    }
}

#[test]
fn type_expressions_record_nested_closure_captures() {
    let closure = "func(){_ = <-ch}";
    let bound = format!(
        "[len([1]func(){{func(){{i:=make(island);ch:=make(chan int);go @(i) {closure}()}}}})]int"
    );
    for declaration in [
        format!("func f(x {bound}){{}}"),
        format!("type F func(x {bound})"),
        format!("var f = func(x {bound}){{}}"),
        format!("type I interface{{F(x {bound});}}"),
        format!("func f() {{ type F func(x {bound}) }}"),
        format!("var x {bound}"),
        format!("func f() {{ var x {bound}; _ = x }}"),
        format!("var x = {bound}{{}}"),
        format!("func f(x any) {{ _ = x.({bound}) }}"),
        format!("func f(x any) {{ switch x.(type) {{ case {bound}: }} }}"),
        format!("func f() {{ _ = new({bound}) }}"),
    ] {
        reject(
            &format!("package main\n{declaration};func main(){{}}"),
            TypeError::GoIslandNotSendable,
            closure,
        );
    }
}

#[test]
fn stable_initialization_order_reconsiders_newly_ready_declarations() {
    let project = analyze("package main\nvar a=mark(b+10);var b=mark(1);var c=mark(2);func mark(v int)int{return v};func main(){}").unwrap();
    let names: Vec<_> = project
        .main()
        .type_info
        .init_order
        .iter()
        .map(|init| project.tc_objs.lobjs[init.lhs[0]].name())
        .collect();
    assert_eq!(names, ["b", "a", "c"]);
}

#[test]
fn checked_arguments_include_untyped_constants_and_tuple_components() {
    let project = analyze(
        r#"package main
        func pair()([]int,int){return nil,1}
        func main(){
            _=copy([]byte{}, "abc")
            _=append(pair())
            _=append([]any{}, 1, "s")
            delete(map[any]int{}, 1)
            _=len("abc")
            _=make([]int, 1, 2)
        }
    "#,
    )
    .unwrap();
    struct Calls<'a> {
        info: &'a vo_analysis::check::TypeInfo,
        calls: Vec<CallInfo>,
    }
    impl Visitor for Calls<'_> {
        fn visit_expr(&mut self, expr: &Expr) {
            if let ExprKind::Call(_) = &expr.kind {
                self.calls.push(
                    self.info
                        .call(expr)
                        .expect("every checked call has metadata")
                        .clone(),
                );
            }
            ast::walk_expr(self, expr);
        }
    }
    let mut visitor = Calls {
        info: &project.main().type_info,
        calls: Vec::new(),
    };
    for file in &project.main().files {
        visitor.visit_file(file);
    }
    let builtins: Vec<_> = visitor
        .calls
        .iter()
        .filter_map(|call| match call.kind {
            CallKind::Builtin(id) => Some((id, call)),
            _ => None,
        })
        .collect();
    use vo_analysis::Builtin;
    assert_eq!(
        builtins
            .iter()
            .map(|(id, call)| (*id, call.arguments.len()))
            .collect::<Vec<_>>(),
        [
            (Builtin::Copy, 2),
            (Builtin::Append, 2),
            (Builtin::Append, 3),
            (Builtin::Delete, 2),
            (Builtin::Len, 1),
            (Builtin::Make, 2)
        ]
    );
    let tuple = builtins[1].1;
    assert_eq!(
        tuple
            .arguments
            .iter()
            .map(|a| (a.source_index, a.tuple_index))
            .collect::<Vec<_>>(),
        [(0, Some(0)), (0, Some(1))]
    );
    assert!(vo_analysis::typ::is_interface(
        builtins[2].1.arguments[1].parameter_type,
        &project.tc_objs
    ));
    assert!(vo_analysis::typ::is_interface(
        builtins[3].1.arguments[1].parameter_type,
        &project.tc_objs
    ));
}

#[test]
fn success_preserves_root_and_dependency_warnings() {
    let project = analyze_files(
        &[(
            "main.vo",
            "package main\nimport \"github.com/acme/lib\";func main(){x:=1;lib.F()}",
        )],
        &[("github.com/acme/lib/lib.vo", "package lib\nfunc F(){y:=1}")],
    )
    .unwrap();
    let mut names = project
        .diagnostics
        .iter()
        .filter(|d| d.code == Some(TypeError::UnusedVar as u16))
        .map(|d| project.source_map.span_text(d.labels[0].span).unwrap())
        .collect::<Vec<_>>();
    names.sort();
    assert_eq!(names, ["x", "y"]);
}

#[test]
fn dependency_errors_preserve_codes_spans_and_sources() {
    for (source, parse_error) in [
        ("package lib\nfunc F( {}", true),
        ("package lib\nfunc F(){missing()}", false),
    ] {
        let error = analyze_files(
            &[(
                "main.vo",
                "package main\nimport \"github.com/acme/lib\";func main(){lib.F()}",
            )],
            &[("github.com/acme/lib/lib.vo", source)],
        )
        .err()
        .expect("invalid dependency");
        assert_eq!(matches!(error, AnalysisError::Parse(..)), parse_error);
        assert_eq!(matches!(error, AnalysisError::Check(..)), !parse_error);
        let diagnostics = error.diagnostics().unwrap();
        let sources = error.source_map().unwrap();
        assert!(diagnostics.iter().any(|d| d.code.is_some()
            && d.labels.iter().any(|label| sources
                .lookup_span(label.span)
                .is_some_and(|file| file.name().ends_with("lib.vo")))));
        if !parse_error {
            assert!(diagnostics
                .iter()
                .any(|d| d.code == Some(TypeError::Undeclared as u16)));
        }
    }
}

#[test]
fn interner_identity_survives_many_files_and_imports() {
    let mut sources = vec![(
        "main.vo".to_string(),
        "package main\nimport \"github.com/acme/lib\";func main(){lib.F();_=v63}".to_string(),
    )];
    for i in 0..64 {
        sources.push((
            format!("part{i:02}.vo"),
            format!("package main\nvar v{i} = {i}"),
        ));
    }
    let sources: Vec<_> = sources
        .iter()
        .map(|(path, source)| (path.as_str(), source.as_str()))
        .collect();
    let project = analyze_files(
        &sources,
        &[("github.com/acme/lib/lib.vo", "package lib\nfunc F(){}")],
    )
    .unwrap();
    for package in project.packages() {
        for (&id, &object) in &package.type_info.uses {
            let _ = id;
            assert!(!project.tc_objs.lobjs[object].name().is_empty());
        }
    }
    assert_eq!(project.main().files.len(), 65);
    assert!(project.package_by_path("github.com/acme/lib").is_some());
}

#[test]
fn project_assembly_rejects_incomplete_or_ambiguous_package_sets() {
    use vo_analysis::arena::ArenaKey;
    use vo_analysis::{objects::TCObjects, AnalyzedPackage};
    use vo_common::{diagnostics::DiagnosticSink, symbol::SymbolInterner, SourceMap};
    let assemble = |objects, packages| {
        Project::from_packages(
            objects,
            SymbolInterner::new(),
            packages,
            SourceMap::new(),
            DiagnosticSink::new(),
            Vec::new(),
        )
    };
    let package = |key| AnalyzedPackage {
        key,
        files: Vec::new(),
        type_info: Default::default(),
    };
    assert!(assemble(TCObjects::new(), Vec::new()).is_err());
    assert!(assemble(
        TCObjects::new(),
        vec![package(vo_analysis::objects::PackageKey::null())]
    )
    .is_err());
    let mut objects = TCObjects::new();
    let root = objects.new_package("main".into(), "main".into());
    assert!(assemble(objects, vec![package(root), package(root)]).is_err());
    let mut objects = TCObjects::new();
    let root = objects.new_package("main".into(), "main".into());
    let duplicate = objects.new_package("main".into(), "main".into());
    assert!(assemble(objects, vec![package(root), package(duplicate)]).is_err());
    for include_dependency in [false, true] {
        let mut objects = TCObjects::new();
        let dependency = objects.new_package("dep".into(), "dep".into());
        let root = objects.new_package("main".into(), "main".into());
        objects.pkgs[root].set_imports(vec![dependency]);
        let mut packages = vec![package(root)];
        if include_dependency {
            packages.push(package(dependency));
        }
        assert!(assemble(objects, packages).is_err());
    }
}

#[test]
fn no_value_callees_fail_during_analysis_in_every_call_context() {
    for (callee, span) in [
        ("f()", "f()"),
        ("(f())", "f()"),
        ("S{}.F()", "S{}.F()"),
        ("factory()()", "factory()()"),
    ] {
        for prefix in ["", "go ", "defer "] {
            let source = format!("package main\ntype S struct{{}};func f(){{}};func(S)F(){{}};func factory()func(){{return f}};func main(){{{prefix}{callee}()}}");
            reject(&source, TypeError::InvalidOp, span);
        }
    }
}

#[test]
fn composite_keys_require_values_with_or_without_parentheses() {
    for key in ["int", "S", "len"] {
        for expression in [key.to_string(), format!("({key})")] {
            reject(&format!("package main\ntype S struct{{}};func main(){{_=map[any]int{{{expression}:1}}}}"), TypeError::InvalidOp, key);
        }
    }
}

#[test]
fn closure_captures_contain_only_runtime_variables() {
    let project = analyze(
        r#"package main
func main() {
    x := 17
    {
        const x = 3
        type Number int
        value := 9
        _ = func() int { return x + int(Number(2)) + value }
    }
    _ = x
}"#,
    )
    .unwrap();
    let captures = &project.main().type_info.closure_captures;
    assert_eq!(captures.len(), 1);
    let objects = captures.values().next().unwrap();
    assert_eq!(objects.len(), 1);
    assert_eq!(project.tc_objs.lobjs[objects[0]].name(), "value");
    assert!(project.tc_objs.lobjs[objects[0]].entity_type().is_var());
}

#[test]
fn island_closures_can_use_local_type_names_without_capturing_them() {
    for (declaration, expression) in [
        ("type C chan int", "C(nil)"),
        ("type F func()", "F(nil)"),
        ("type A interface{}", "A(nil)"),
        ("type S struct{ch chan int}", "new(S)"),
    ] {
        let source = format!("package main\nfunc main(){{{declaration};i:=make(island);go @(i)func(){{_={expression}}}()}}");
        let project = analyze(&source).unwrap_or_else(|e| panic!("{source}: {e}"));
        assert!(project
            .main()
            .type_info
            .closure_captures
            .values()
            .all(Vec::is_empty));
    }
}

#[test]
fn constant_assert_conditions_preserve_complete_argument_bindings() {
    let project = analyze(r#"package main
func pair()(bool,int8,string){return true,1,"message"}
func main(){b:=true;assert(true,int8(1),"message");assert(b,int8(1),"message");assert(pair());assert(true);_=len("abc")}
"#).unwrap();
    struct Calls<'a> {
        project: &'a Project,
        asserts: Vec<Vec<String>>,
    }
    impl Visitor for Calls<'_> {
        fn visit_expr(&mut self, expr: &Expr) {
            if let ExprKind::Call(_) = &expr.kind {
                let call = self
                    .project
                    .main()
                    .type_info
                    .call(expr)
                    .expect("checked call");
                if call.kind == CallKind::Builtin(vo_analysis::Builtin::Assert) {
                    self.asserts.push(
                        call.arguments
                            .iter()
                            .map(|arg| {
                                if arg.source_index != 0 || arg.tuple_index.is_some_and(|i| i != 0)
                                {
                                    assert!(vo_analysis::typ::is_interface(
                                        arg.parameter_type,
                                        &self.project.tc_objs
                                    ));
                                } else {
                                    assert!(vo_analysis::typ::is_boolean(
                                        arg.parameter_type,
                                        &self.project.tc_objs
                                    ));
                                }
                                vo_analysis::display::type_string(
                                    arg.source_type,
                                    &self.project.tc_objs,
                                )
                            })
                            .collect(),
                    );
                }
            }
            ast::walk_expr(self, expr);
        }
    }
    let mut calls = Calls {
        project: &project,
        asserts: Vec::new(),
    };
    for file in &project.main().files {
        calls.visit_file(file);
    }
    assert_eq!(
        calls.asserts,
        vec![
            vec!["bool", "int8", "string"],
            vec!["bool", "int8", "string"],
            vec!["bool", "int8", "string"],
            vec!["bool"]
        ]
    );
}

#[test]
fn duplicate_nil_cases_preserve_both_source_locations() {
    let source = "package main\nfunc main(){var x any;switch x.(type){case nil:case nil:}}";
    let error = analyze(source).err().expect("duplicate nil must fail");
    let diagnostic = error
        .diagnostics()
        .unwrap()
        .iter()
        .find(|d| d.code == Some(TypeError::DuplicateCase as u16))
        .unwrap();
    assert_eq!(diagnostic.labels.len(), 2);
    let sources = error.source_map().unwrap();
    for label in &diagnostic.labels {
        assert_eq!(sources.span_text(label.span), Some("nil"));
        assert!(sources.lookup_span(label.span).is_some());
    }
    assert_ne!(diagnostic.labels[0].span, diagnostic.labels[1].span);
}
