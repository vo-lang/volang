use crate::arena::ArenaKey;
use crate::constant::{
    try_make_string, Value, MAX_CONSTANT_FOLD_WORK_BYTES, MAX_CONSTANT_STRING_BYTES,
};
use crate::importer::NullImporter;
use crate::objects::PackageKey;
use crate::operand::OperandMode;
use crate::typ::BasicType;
use crate::Checker;
use std::path::PathBuf;
use vo_syntax::ast::{Decl, Expr, ExprId, ExprKind, File};
use vo_syntax::parser;

use super::super::errors::TypeError;

fn check_source(source: &str) -> (Checker, bool) {
    let (file, parse_diags, interner) = parser::parse(source, 0);
    assert!(
        !parse_diags.has_errors(),
        "parse diagnostics: {parse_diags:?}"
    );

    let mut checker = Checker::new_with_trace(PackageKey::null(), interner, false);
    let main_pkg = checker
        .tc_objs
        .new_package("main".to_string(), "main".to_string());
    checker.pkg = main_pkg;
    let mut importer = NullImporter::new(PathBuf::from("."));
    let ok = checker.check_with_importer(&[file], &mut importer).is_ok();
    (checker, ok)
}

fn has_diagnostic(checker: &Checker, code: TypeError) -> bool {
    checker
        .diagnostics
        .borrow()
        .diagnostics()
        .iter()
        .any(|diag| diag.code == Some(code.code()))
}

#[test]
fn type_check_diagnostics_retain_a_bounded_prefix_and_sentinel() {
    let mut source = String::from("package main\nfunc main() {\n");
    for index in 0..(super::super::MAX_TYPE_CHECK_DIAGNOSTICS + 32) {
        source.push_str(&format!("_ = missing{index}\n"));
    }
    source.push_str("}\n");

    let (checker, ok) = check_source(&source);
    assert!(!ok);
    let diagnostics = checker.diagnostics.borrow();
    assert_eq!(
        diagnostics.len(),
        super::super::MAX_TYPE_CHECK_DIAGNOSTICS + 1
    );
    let sentinel = diagnostics.iter().last().expect("limit sentinel");
    assert_eq!(
        sentinel.code,
        Some(TypeError::DiagnosticLimitExceeded.code())
    );
    assert!(sentinel.message.contains("further diagnostics suppressed"));
}

fn assert_check_ok(source: &str) {
    let (checker, ok) = check_source(source);
    assert!(
        ok,
        "type check failed: {:?}",
        checker.diagnostics.borrow().diagnostics()
    );
}

#[test]
fn assignment_diagnostic_names_actual_and_target_types() {
    let (checker, ok) = check_source(
        r#"
            package main

            func consume(value uint64) {}

            func run() {
                var checksum uint32
                consume(checksum)
            }
        "#,
    );
    assert!(
        !ok,
        "typed numeric arguments require an explicit conversion"
    );

    let diagnostics = checker.diagnostics.borrow();
    let diagnostic = diagnostics
        .diagnostics()
        .iter()
        .find(|diagnostic| diagnostic.code == Some(TypeError::CannotAssign.code()))
        .expect("expected E2001 assignment diagnostic");
    assert_eq!(
        diagnostic.message,
        "cannot use value of type uint32 as uint64 in argument"
    );
}

fn check_source_with_file(source: &str) -> (File, Checker, bool) {
    let (file, parse_diags, interner) = parser::parse(source, 0);
    assert!(
        !parse_diags.has_errors(),
        "parse diagnostics: {parse_diags:?}"
    );

    let mut checker = Checker::new_with_trace(PackageKey::null(), interner, false);
    let main_pkg = checker
        .tc_objs
        .new_package("main".to_string(), "main".to_string());
    checker.pkg = main_pkg;
    let mut importer = NullImporter::new(PathBuf::from("."));
    let ok = checker
        .check_with_importer(std::slice::from_ref(&file), &mut importer)
        .is_ok();
    (file, checker, ok)
}

fn first_const_value(file: &File) -> &Expr {
    let Decl::Const(decl) = &file.decls[0] else {
        panic!("first declaration must be const");
    };
    &decl.specs[0].values[0]
}

fn basic_type_of(checker: &Checker, expr_id: ExprId) -> BasicType {
    let type_key = checker
        .result
        .types
        .get(&expr_id)
        .unwrap_or_else(|| panic!("missing TypeInfo for {expr_id:?}"))
        .typ;
    checker.tc_objs.types[type_key]
        .try_as_basic()
        .unwrap_or_else(|| panic!("expression {expr_id:?} must have a basic type"))
        .typ()
}

fn collect_binary_left_spine(expr: &Expr) -> Vec<&Expr> {
    let mut nodes = Vec::new();
    let mut cursor = expr;
    while let ExprKind::Binary(binary) = &cursor.kind {
        nodes.push(cursor);
        cursor = &binary.left;
    }
    nodes
}

#[test]
fn untyped_constant_binary_chain_defaults_at_the_expression_resource_boundary() {
    let operators = vo_syntax::parser::MAX_BINARY_EXPRESSION_PATH;
    let expression = vec!["1"; operators + 1].join(" + ");
    let source = format!("package main\nconst total = {expression}\nvar value int = total\n");
    assert_check_ok(&source);
}

#[test]
fn iterative_binary_chain_preserves_immediate_lhs_for_untyped_promotion() {
    let (file, checker, ok) = check_source_with_file("package main\nconst total = 1 + 2 + 3.5\n");
    assert!(ok, "type check failed: {:?}", checker.diagnostics.borrow());

    let root = first_const_value(&file);
    let ExprKind::Binary(outer) = &root.kind else {
        panic!("constant value must be a binary expression");
    };
    assert!(matches!(outer.left.kind, ExprKind::Binary(_)));
    assert_eq!(basic_type_of(&checker, root.id), BasicType::UntypedFloat);
    assert_eq!(
        basic_type_of(&checker, outer.left.id),
        BasicType::UntypedFloat,
        "the promoted operand must be the complete immediate lhs"
    );
    assert!(matches!(
        checker.result.types[&outer.left.id].mode,
        OperandMode::Constant(_)
    ));
}

#[test]
fn iterative_binary_chain_preserves_immediate_lhs_for_typed_conversion() {
    let (file, checker, ok) =
        check_source_with_file("package main\nconst total = 1 + 2 + int64(3)\n");
    assert!(ok, "type check failed: {:?}", checker.diagnostics.borrow());

    let root = first_const_value(&file);
    let ExprKind::Binary(outer) = &root.kind else {
        panic!("constant value must be a binary expression");
    };
    assert_eq!(basic_type_of(&checker, root.id), BasicType::Int64);
    assert_eq!(
        basic_type_of(&checker, outer.left.id),
        BasicType::Int64,
        "typed conversion must finalize the complete immediate lhs"
    );
}

#[test]
fn iterative_binary_chain_preserves_comparison_operand_promotion() {
    let (file, checker, ok) = check_source_with_file("package main\nconst ordered = 1 + 2 < 3.5\n");
    assert!(ok, "type check failed: {:?}", checker.diagnostics.borrow());

    let root = first_const_value(&file);
    let ExprKind::Binary(comparison) = &root.kind else {
        panic!("constant value must be a comparison");
    };
    assert_eq!(basic_type_of(&checker, root.id), BasicType::UntypedBool);
    assert_eq!(
        basic_type_of(&checker, comparison.left.id),
        BasicType::UntypedFloat,
        "comparison conversion must update the complete arithmetic lhs"
    );
}

#[test]
fn extended_scale_float_comparisons_fold_exactly_in_source() {
    for expression in [
        "1e5000 > 1e4999",
        "1e-4999 > 1e-5000",
        "-1e4999 > -1e5000",
        "1e5000 == 1e5000",
        "1e4097 > 1e4096",
        "1e10000 > 1e9999",
        "0x1p5000 > 0x1p4999",
        "0x1p-4999 > 0x1p-5000",
        "-0x1p4999 > -0x1p5000",
        "0x1p5000 == 0x1p5000",
    ] {
        let source = format!("package main\nconst result = {expression}\n");
        let (file, checker, ok) = check_source_with_file(&source);
        assert!(
            ok,
            "type check failed for {expression}: {:?}",
            checker.diagnostics.borrow()
        );
        let root = first_const_value(&file);
        assert!(matches!(
            checker.result.types[&root.id].mode,
            OperandMode::Constant(Value::Bool(true))
        ));
    }
}

#[test]
fn out_of_budget_float_literal_has_a_stable_diagnostic() {
    for literal in ["1e20000", "1e-20000", "0x1p70000", "0x1p-70000"] {
        let (checker, ok) = check_source(&format!(
            "package main\nconst result = {literal} == {literal}\n"
        ));
        assert!(!ok, "out-of-budget literal unexpectedly passed: {literal}");
        let diagnostics = checker.diagnostics.borrow();
        let resource_errors: Vec<_> = diagnostics
            .diagnostics()
            .iter()
            .filter(|diagnostic| diagnostic.code == Some(TypeError::ConstantResourceLimit.code()))
            .collect();
        assert_eq!(resource_errors.len(), 2, "diagnostics: {diagnostics:?}");
        assert!(resource_errors.iter().all(|diagnostic| diagnostic
            .message
            .contains("exact-arithmetic limit of 65536 bits")));
    }
}

#[test]
fn constant_operation_budget_rejects_growth_and_allows_reduction() {
    assert_check_ok(
        "package main\nconst result = (1e19000 + -1e19000) == 0 && (1e19000 / 1e19000) == 1\n",
    );

    for expression in ["1e19000 * 1e19000", "(1e8000 / 1e-8000) / 1e-8000"] {
        let (checker, ok) = check_source(&format!("package main\nconst result = {expression}\n"));
        assert!(!ok, "oversized fold unexpectedly passed: {expression}");
        assert!(has_diagnostic(&checker, TypeError::ConstantResourceLimit));
    }

    let (checker, ok) = check_source("package main\nconst result = 1 << 65535 << 1\n");
    assert!(!ok, "oversized chained shift unexpectedly passed");
    assert!(has_diagnostic(&checker, TypeError::ConstantResourceLimit));
}

#[test]
fn constant_shift_limit_matches_the_exact_integer_budget() {
    assert_check_ok("package main\nconst result = 1 << 65535\nconst reduced = (-1) >> 65535\n");

    for expression in ["1 << 65536", "(-1) >> 65536"] {
        let (checker, ok) = check_source(&format!("package main\nconst result = {expression}\n"));
        assert!(!ok, "out-of-budget shift unexpectedly passed: {expression}");
        assert!(has_diagnostic(&checker, TypeError::ConstantResourceLimit));
    }
}

#[test]
fn string_constant_single_value_and_package_work_budgets_are_checked() {
    let mut declarations = String::from("package main\nconst s0 = \"x\"\n");
    for index in 1..=21 {
        declarations.push_str(&format!(
            "const s{index} = s{} + s{}\n",
            index - 1,
            index - 1
        ));
    }
    let (checker, ok) = check_source(&declarations);
    assert!(
        !ok,
        "exponentially growing folded string unexpectedly passed"
    );
    assert!(has_diagnostic(&checker, TypeError::ConstantResourceLimit));

    let mut checker = Checker::new_with_trace(PackageKey::null(), Default::default(), false);
    let value = try_make_string(&"x".repeat(MAX_CONSTANT_STRING_BYTES)).unwrap();
    let charges = MAX_CONSTANT_FOLD_WORK_BYTES / MAX_CONSTANT_STRING_BYTES as u64;
    for _ in 0..charges {
        assert!(checker.charge_constant_fold_work(Default::default(), &[&value]));
    }
    assert!(!checker.charge_constant_fold_work(Default::default(), &[&value]));
    let diagnostics = checker.diagnostics.borrow();
    let resource_errors: Vec<_> = diagnostics
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.code == Some(TypeError::ConstantResourceLimit.code()))
        .collect();
    assert_eq!(resource_errors.len(), 1, "diagnostics: {diagnostics:?}");
}

#[test]
fn iterative_binary_chain_keeps_shift_lhs_finalization_on_the_full_operand() {
    let (checker, ok) = check_source(
        r#"
            package main
            func invalidShift(count uint) {
                var result float64 = 1 + 2 << count
                _ = result
            }
        "#,
    );
    assert!(!ok, "a non-integer finalized shift lhs must be rejected");
    assert!(checker
        .diagnostics
        .borrow()
        .diagnostics()
        .iter()
        .any(|diag| {
            diag.code == Some(TypeError::InvalidOp.code())
                && diag.message.contains("shifted operand must be integer")
        }));
}

#[test]
fn iterative_binary_boundary_records_every_node_and_promotes_the_complete_tail_lhs() {
    let operators = vo_syntax::parser::MAX_BINARY_EXPRESSION_PATH;
    let mut operands = vec!["1"; operators];
    operands.push("0.5");
    let expression = operands.join(" + ");
    let source = format!("package main\nconst total = {expression}\n");
    let (file, checker, ok) = check_source_with_file(&source);
    assert!(ok, "type check failed: {:?}", checker.diagnostics.borrow());

    let root = first_const_value(&file);
    let spine = collect_binary_left_spine(root);
    assert_eq!(spine.len(), operators);
    for node in &spine {
        let recorded = checker
            .result
            .types
            .get(&node.id)
            .unwrap_or_else(|| panic!("missing TypeInfo for binary node {:?}", node.id));
        assert!(matches!(recorded.mode, OperandMode::Constant(_)));
    }

    assert_eq!(basic_type_of(&checker, root.id), BasicType::UntypedFloat);
    let ExprKind::Binary(outer) = &root.kind else {
        unreachable!();
    };
    assert_eq!(
        basic_type_of(&checker, outer.left.id),
        BasicType::UntypedFloat,
        "the boundary chain's complete lhs must receive the tail promotion"
    );
}

#[test]
fn unnamed_results_reject_empty_return_but_named_results_allow_it() {
    let (checker, ok) = check_source(
        r#"
            package main
            func bad() int { return }
        "#,
    );
    assert!(!ok, "empty return with unnamed results must fail");
    assert!(has_diagnostic(&checker, TypeError::AssignmentMismatch));

    assert_check_ok(
        r#"
            package main
            func good() (value int) { return }
        "#,
    );
}

#[test]
fn range_assignment_rejects_a_second_target_for_single_value_ranges() {
    for source in [
        r#"
            package main
            func bad(c chan int) {
                var value int
                var ok bool
                for value, ok = range c { _ = value; _ = ok }
            }
        "#,
        r#"
            package main
            func bad() {
                var index int
                var value int
                for index, value = range 3 { _ = index; _ = value }
            }
        "#,
        r#"
            package main
            func bad(c chan int) {
                var value int
                for value, _ = range c { _ = value }
            }
        "#,
        r#"
            package main
            func bad(c chan int) {
                for value, _ := range c { _ = value }
            }
        "#,
    ] {
        let (checker, ok) = check_source(source);
        assert!(!ok, "two-target single-value range must fail");
        assert!(has_diagnostic(&checker, TypeError::InvalidOp));
        assert!(checker
            .diagnostics
            .borrow()
            .diagnostics()
            .iter()
            .any(|diag| diag.message.contains("has no second value")));
    }

    assert_check_ok(
        r#"
            package main
            func good(values []int) {
                var index int
                var value int
                for index, value = range values { _ = index; _ = value }
            }
        "#,
    );
}

#[test]
fn short_declarations_without_new_variables_are_hard_errors() {
    for source in [
        r#"
            package main
            func ordinaryBlank() {
                _ := 1
            }
        "#,
        r#"
            package main
            func selectBlank(c chan int) {
                select { case _ := <-c: }
            }
        "#,
        r#"
            package main
            func rangeBlank(values []int) {
                for _ := range values {}
            }
        "#,
    ] {
        let (checker, ok) = check_source(source);
        assert!(!ok, "NoNewVars must prevent successful type checking");
        assert!(has_diagnostic(&checker, TypeError::NoNewVars));
        assert!(checker
            .diagnostics
            .borrow()
            .diagnostics()
            .iter()
            .any(|diag| diag.code == Some(TypeError::NoNewVars.code()) && diag.is_error()));
    }
}

#[test]
fn short_declarations_reject_repeated_non_blank_names() {
    let (checker, ok) = check_source(
        r#"
            package main
            func bad() {
                existing := 0
                existing, existing, added := 1, 2, 3
                fresh, fresh := 4, 5
                _, _ = existing, added
                _ = fresh
            }
            func badRange(values []int) {
                for index, index := range values { _ = index }
            }
            func badSelect(values chan int) {
                select { case value, value := <-values: _ = value }
            }
            func duplicateWithoutNew() {
                existing := 0
                existing, existing := 1, 2
                _ = existing
            }
        "#,
    );
    assert!(!ok, "repeated short-declaration names must fail");

    let diagnostics = checker.diagnostics.borrow();
    let repeated: Vec<_> = diagnostics
        .diagnostics()
        .iter()
        .filter(|diag| diag.code == Some(TypeError::RepeatedNameInShortDecl.code()))
        .collect();
    assert_eq!(repeated.len(), 5, "diagnostics: {repeated:?}");
    assert!(repeated
        .iter()
        .all(|diag| diag.message.contains("repeated on left side of :=")));
    assert!(!has_diagnostic(&checker, TypeError::Redeclared));
    assert!(!has_diagnostic(&checker, TypeError::NoNewVars));

    assert_check_ok(
        r#"
            package main
            func good() {
                _, _, value := 1, 2, 3
                _ = value
            }
        "#,
    );
}

#[test]
fn three_clause_loop_without_condition_satisfies_result_termination() {
    assert_check_ok(
        r#"
            package main
            func loopsForever() int { for ; ; {} }
        "#,
    );
}

#[test]
fn nested_unlabeled_break_does_not_escape_labeled_outer_loop() {
    assert_check_ok(
        r#"
            package main
            func loopsForever() int {
            Outer:
                for {
                    for { break }
                    continue Outer
                }
            }
        "#,
    );
}

#[test]
fn breaks_targeting_the_infinite_loop_still_require_a_return() {
    for source in [
        r#"
            package main
            func canExit() int { for { break } }
        "#,
        r#"
            package main
            func canExit() int {
            Outer:
                for {
                    for { break Outer }
                }
            }
        "#,
    ] {
        let (checker, ok) = check_source(source);
        assert!(
            !ok,
            "a break targeting the outer loop makes it non-terminating"
        );
        assert!(has_diagnostic(&checker, TypeError::MissingReturn));
    }
}

#[test]
fn nil_type_switch_case_does_not_count_as_default() {
    let (checker, ok) = check_source(
        r#"
            package main
            func needsDefault(value any) int {
                switch value.(type) {
                case nil:
                    return 1
                }
            }
        "#,
    );
    assert!(!ok, "case nil leaves non-nil values uncovered");
    assert!(has_diagnostic(&checker, TypeError::MissingReturn));
}

#[test]
fn parenthesized_panic_call_satisfies_result_termination() {
    assert_check_ok(
        r#"
            package main
            func alwaysPanics() int { (panic("boom")) }
        "#,
    );
}

#[test]
fn labeled_control_requires_an_enclosing_compatible_target() {
    for (source, expected) in [
        (
            r#"
                package main
                func bad() {
                Switch:
                    switch 1 { default: continue Switch }
                }
            "#,
            TypeError::InvalidContinue,
        ),
        (
            r#"
                package main
                func bad() {
                Block:
                    { break Block }
                }
            "#,
            TypeError::InvalidBreak,
        ),
        (
            r#"
                package main
                func bad() {
                    for { break Later }
                Later:
                    for { break }
                }
            "#,
            TypeError::InvalidBreak,
        ),
        (
            r#"
                package main
                func bad() {
                Outer:
                Inner:
                    for { break Outer }
                }
            "#,
            TypeError::InvalidBreak,
        ),
    ] {
        let (checker, ok) = check_source(source);
        assert!(!ok, "incompatible or non-enclosing label must fail");
        assert!(has_diagnostic(&checker, expected));
    }
}

#[test]
fn label_directly_attached_to_loop_is_a_valid_control_target() {
    assert_check_ok(
        r#"
            package main
            func good(done bool) {
            Loop:
                for {
                    if done { break Loop }
                    continue Loop
                }
            }
        "#,
    );
}

#[test]
fn dynamic_access_assignment_peels_parentheses_and_try_wrappers() {
    assert_check_ok(
        r#"
            package main

            func read(box any) error {
                value, err := (((box~>Value)))
                _ = value
                return err
            }

            func unwrapOutside(box any) (any, error) {
                value := (box~>Value)?
                return value, nil
            }

            func unwrapInside(box any) (any, error) {
                value := ((box~>Value?))
                return value, nil
            }
        "#,
    );
}

#[test]
fn dynamic_access_results_use_the_canonical_any_type() {
    let (checker, ok) = check_source(
        r#"
            package main

            func read(box any) error {
                value, err := (((box~>Value)))
                _ = value
                return err
            }
        "#,
    );
    assert!(
        ok,
        "type check failed: {:?}",
        checker.diagnostics.borrow().diagnostics()
    );

    let any_type = checker.tc_objs.universe().any_type();
    let error_type = checker.tc_objs.universe().error_type();
    let mut dynamic_tuple_count = 0;
    for type_and_value in checker.result.types.values() {
        let Some(tuple) = checker.tc_objs.types[type_and_value.typ].try_as_tuple() else {
            continue;
        };
        if tuple.vars().len() != 2 {
            continue;
        }
        let first = checker.tc_objs.lobjs[tuple.vars()[0]].typ();
        let second = checker.tc_objs.lobjs[tuple.vars()[1]].typ();
        if second == Some(error_type)
            && first.is_some_and(|typ| checker.tc_objs.types[typ].try_as_interface().is_some())
        {
            dynamic_tuple_count += 1;
            assert_eq!(first, Some(any_type));
        }
    }
    assert!(
        dynamic_tuple_count > 0,
        "dynamic result tuple was not recorded"
    );
}

#[test]
fn empty_interface_spellings_and_builtins_share_the_canonical_any_type() {
    let (checker, ok) = check_source(
        r#"
            package main

            func pass(value interface{}) any {
                recovered := recover()
                if recovered != nil { return recovered }
                panic(value)
            }
        "#,
    );
    assert!(
        ok,
        "type check failed: {:?}",
        checker.diagnostics.borrow().diagnostics()
    );

    let any_type = checker.tc_objs.universe().any_type();
    let empty_interfaces: Vec<_> = checker
        .tc_objs
        .types
        .iter()
        .filter_map(|(key, typ)| {
            typ.try_as_interface()
                .is_some_and(|interface| interface.is_empty())
                .then_some(key)
        })
        .collect();
    assert_eq!(empty_interfaces, vec![any_type]);
}

#[test]
fn semantic_dependency_depth_is_rejected_without_using_the_host_stack() {
    let depth = super::super::MAX_TYPE_CHECK_DEPTH + 32;

    let mut aliases = String::from("package main\n");
    for index in 0..depth {
        aliases.push_str(&format!("type Alias{index} = Alias{}\n", index + 1));
    }
    aliases.push_str(&format!("type Alias{depth} = int\nfunc main() {{}}\n"));

    let mut interfaces = String::from("package main\n");
    for index in 0..depth {
        interfaces.push_str(&format!(
            "type Interface{index} interface {{ Interface{} }}\n",
            index + 1
        ));
    }
    interfaces.push_str(&format!(
        "type Interface{depth} interface {{}}\nfunc main() {{}}\n"
    ));

    let mut interface_aliases = String::from("package main\n");
    for index in 0..depth {
        interface_aliases.push_str(&format!(
            "type InterfaceAlias{index} = InterfaceAlias{}\n",
            index + 1
        ));
    }
    interface_aliases.push_str(&format!(
        "type InterfaceAlias{depth} = interface {{}}\ntype Root interface {{ InterfaceAlias0 }}\nfunc main() {{}}\n"
    ));

    for source in [aliases, interfaces, interface_aliases] {
        let (checker, ok) = check_source(&source);
        assert!(!ok, "excessive semantic nesting must be rejected");
        assert!(has_diagnostic(&checker, TypeError::TypeNestingTooDeep));
    }
}

#[test]
fn spec_conformant_range_assert_and_embedding_regressions_type_check() {
    assert_check_ok(
        r#"
            package main

            type RuneCell struct { value rune }
            type PromotedFields struct { value int }
            type Promoted struct { PromotedFields }

            func verify(input any) {
                cell := RuneCell{}
                index := -1
                for index, cell.value = range "é" {}

                promoted := Promoted{}
                promoted.value = 1

                switch input.(type) {
                default:
                    unreachable := false
                    assert(unreachable, "runtime-only unreachable branch")
                }
            }

            func main() {}
        "#,
    );
}

#[test]
fn integer_range_preserves_or_selects_the_iteration_type() {
    assert_check_ok(
        r#"
            package main

            type Count int16

            func takeInt8(value int8) {}
            func takeUint8(value uint8) {}
            func takeCount(value Count) {}
            func takeInt(value int) {}

            func typed(a int8, b uint8, c Count) {
                for i := range a { takeInt8(i) }
                for i := range b { takeUint8(i) }
                for i := range c { takeCount(i) }
                for i := range 3 { takeInt(i) }
            }

            func assigned() {
                var i uint8
                for i = range 10 { takeUint8(i) }
            }
        "#,
    );
}

#[test]
fn integer_range_rejects_untyped_bound_that_overflows_assignment_target() {
    let (checker, ok) = check_source(
        r#"
            package main
            func bad() {
                var i uint8
                for i = range 256 {}
            }
        "#,
    );
    assert!(
        !ok,
        "range bound must be representable by the iteration target"
    );
    assert!(has_diagnostic(&checker, TypeError::TypeMismatch));
    assert!(checker
        .diagnostics
        .borrow()
        .diagnostics()
        .iter()
        .any(|diagnostic| diagnostic.message.contains("overflows uint8")));
}

#[test]
fn go_island_postpass_reaches_nested_executable_ast_children() {
    let (checker, ok) = check_source(
        r#"
            package main

            func nested(target island, unsafeQueue chan int, output chan int, values []int) {
                indices := []int{0}

                _ = map[int]int{
                    func() int {
                        go @(target) func(value chan int) {}(unsafeQueue)
                        return 0
                    }(): 0,
                }

                _ = values[:0:func() int {
                    go @(target) func(value chan int) {}(unsafeQueue)
                    return len(values)
                }()]

                select {
                case output <- func() int {
                    go @(target) func(value chan int) {}(unsafeQueue)
                    return 0
                }():
                default:
                }

                for indices[func() int {
                    go @(target) func(value chan int) {}(unsafeQueue)
                    return 0
                }()] = range values {}
            }
        "#,
    );
    assert!(!ok, "all nested cross-island transfers must be checked");
    let count = checker
        .diagnostics
        .borrow()
        .diagnostics()
        .iter()
        .filter(|diagnostic| diagnostic.code == Some(TypeError::GoIslandNotSendable.code()))
        .count();
    assert_eq!(count, 4, "each omitted AST child should be visited once");
}

#[test]
fn go_island_capture_diagnostics_point_to_each_invalid_closure_transfer() {
    let (checker, ok) = check_source(
        r#"
            package main

            func bad(target island) {
                owner := make(port int, 2)
                go @(target) func() { owner <- 1 }()
                go @(target) func() { owner <- 2 }()
            }
        "#,
    );
    assert!(
        !ok,
        "a bidirectional port cannot be captured across islands"
    );

    let diagnostics = checker.diagnostics.borrow();
    let capture_spans: Vec<_> = diagnostics
        .diagnostics()
        .iter()
        .filter(|diagnostic| {
            diagnostic.code == Some(TypeError::GoIslandNotSendable.code())
                && diagnostic.message.contains("cannot capture 'owner'")
        })
        .map(|diagnostic| diagnostic.labels[0].span)
        .collect();

    assert_eq!(
        capture_spans.len(),
        2,
        "each invalid transfer needs one diagnostic"
    );
    assert_ne!(
        capture_spans[0], capture_spans[1],
        "separate closures must point to separate transfer sites"
    );
}

#[test]
fn invalid_semantic_corpus_is_total_and_deterministic() {
    // This is a compact, deterministic fault-injection corpus for the checker.
    // Every input is syntactically valid and deliberately violates one or more
    // name/type rules. Keeping it in the ordinary suite continuously checks the
    // invalid-operand propagation that guards the checker's internal invariants.
    const EXPRESSIONS: &[&str] = &[
        "missing",
        "missing()",
        "missing.field",
        "missing[0]",
        "missing[:]",
        "missing.(int)",
        "<-missing",
        "*missing",
        "&missing",
        "-missing",
        "!missing",
        "missing?",
        "missing~>field",
        "missing~>[0]",
        "missing~>()",
        "Missing{}",
        "[]Missing{}",
        "make(Missing)",
        "new(Missing)",
        "len(missing)",
        "cap(missing)",
        "append(missing, missing)",
        "copy(missing, missing)",
        "delete(missing, missing)",
        "close(missing)",
        "panic(missing)",
        "nil",
        "int",
        "true()",
        "1[0]",
        "(1).field",
        "(1).(int)",
        "(1)~>field",
        "func() {} + missing",
    ];
    const TYPES: &[&str] = &[
        "Missing",
        "[]Missing",
        "[missing]Missing",
        "map[Missing]Missing",
        "chan Missing",
        "<-chan Missing",
        "port Missing",
        "*Missing",
        "func(Missing) Missing",
        "struct { field Missing }",
        "interface { Missing }",
        "interface { Method(Missing) Missing }",
    ];

    fn signature(source: &str) -> (bool, Vec<String>) {
        let (checker, ok) = check_source(source);
        let diagnostics = checker.diagnostics.borrow();
        let diagnostics = diagnostics
            .diagnostics()
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
        (ok, diagnostics)
    }

    fn assert_total_and_deterministic(source: &str) {
        let first = std::panic::catch_unwind(|| signature(source))
            .unwrap_or_else(|_| panic!("checker panicked for invalid source:\n{source}"));
        let second = std::panic::catch_unwind(|| signature(source))
            .unwrap_or_else(|_| panic!("checker panicked on replay for source:\n{source}"));
        assert_eq!(
            first, second,
            "checker drifted for invalid source:\n{source}"
        );
        assert!(
            !first.0,
            "invalid corpus entry unexpectedly passed:\n{source}"
        );
        assert!(
            !first.1.is_empty(),
            "invalid corpus entry produced no diagnostic:\n{source}"
        );
    }

    for expression in EXPRESSIONS {
        for source in [
            format!("package main\nvar value = {expression}\n"),
            format!("package main\nfunc probe() {{ _ = {expression} }}\n"),
            format!("package main\nfunc probe() int {{ return {expression} }}\n"),
            format!("package main\nfunc probe() {{ if ({expression}) {{}} }}\n"),
            format!("package main\nfunc probe() {{ for ({expression}) {{ break }} }}\n"),
            format!(
                "package main\nfunc probe() {{ switch ({expression}) {{ case 0: return; default: return }} }}\n"
            ),
        ] {
            assert_total_and_deterministic(&source);
        }
    }

    for (left_index, left) in EXPRESSIONS.iter().enumerate() {
        for (right_index, right) in EXPRESSIONS.iter().enumerate() {
            let operator = ["+", "==", "&&", "<<"][(left_index * 7 + right_index * 13) % 4];
            assert_total_and_deterministic(&format!(
                "package main\nfunc probe() {{ _ = ({left}) {operator} ({right}) }}\n"
            ));
        }
    }

    for typ in TYPES {
        for source in [
            format!("package main\nvar value {typ}\n"),
            format!("package main\ntype Value {typ}\n"),
            format!("package main\nfunc probe(value {typ}) {{}}\n"),
            format!("package main\nfunc probe() {typ} {{ panic(0) }}\n"),
        ] {
            assert_total_and_deterministic(&source);
        }
    }

    for declarations in [
        "type A B\ntype B A",
        "type A = B\ntype B = A",
        "var a = b\nvar b = a",
        "const a = b\nconst b = a",
        "type A struct { field A }",
        "type A interface { B }\ntype B interface { A }",
        "func duplicate() {}\nfunc duplicate() {}",
        "var duplicate int\ntype duplicate int",
        "func probe() { goto absent }",
        "func probe() { break; continue; fallthrough }",
        "func probe() { value := 1; value := 2 }",
        "func probe() { var value Missing; value = missing }",
        "func probe() { go missing }",
        "func probe() { defer missing; errdefer missing; fail missing }",
        "func probe() { missing <- missing }",
        "func probe() { select { case missing <- missing: return; default: return } }",
        "func probe() { select { case value := <-missing: _ = value } }",
        "func probe() { for key, value = range missing { _, _ = key, value } }",
        "func probe() { switch missing.(type) { case int: return } }",
    ] {
        assert_total_and_deterministic(&format!("package main\n{declarations}\n"));
    }
}
