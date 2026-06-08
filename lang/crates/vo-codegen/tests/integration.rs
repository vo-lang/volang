//! Integration tests: parse → check → codegen → VM

use std::collections::BTreeMap;
use std::path::PathBuf;
use vo_analysis::importer::NullImporter;
use vo_analysis::{AnalysisError, Checker, Project};
use vo_codegen::compile_project;
use vo_common::SourceMap;
use vo_common_core::bytecode::{RETURN_FLAG_ERROR_RETURN, RETURN_FLAG_HEAP_RETURNS};
use vo_runtime::{SlotType, ValueKind, ValueMeta};
use vo_syntax::parser;
use vo_vm::bytecode::{FunctionDef, JitInstructionMetadata};
use vo_vm::instruction::Opcode;
use vo_vm::vm::Vm;

/// Helper: analyze a single source string (no imports) for tests
fn analyze_source(source: &str) -> Result<Project, AnalysisError> {
    use vo_analysis::arena::ArenaKey;
    use vo_analysis::objects::PackageKey;

    let (file, diags, interner) = parser::parse(source, 0);
    if diags.has_errors() {
        return Err(AnalysisError::Parse(diags, SourceMap::new()));
    }

    let mut checker = Checker::new_with_trace(PackageKey::null(), interner.clone(), false);
    let main_pkg_key = checker
        .tc_objs
        .new_package("main".to_string(), "main".to_string());
    checker.pkg = main_pkg_key;

    let mut importer = NullImporter::new(PathBuf::from("."));
    if checker
        .check_with_importer(std::slice::from_ref(&file), &mut importer)
        .is_err()
    {
        let diags = checker.diagnostics.take();
        return Err(AnalysisError::Check(diags, SourceMap::new()));
    }

    Ok(Project {
        tc_objs: checker.tc_objs,
        interner,
        packages: vec![main_pkg_key],
        main_package: main_pkg_key,
        type_info: checker.result,
        files: vec![file],
        imported_files: BTreeMap::new(),
        imported_type_infos: BTreeMap::new(),
        source_map: SourceMap::new(),
        extensions: Vec::new(),
    })
}

/// Helper: compile Vo source to Module
fn compile_source(source: &str) -> vo_vm::bytecode::Module {
    let project = analyze_source(source).expect("analysis failed");
    compile_project(&project).expect("codegen failed")
}

/// Helper: compile and run, verify execution completes
fn compile_and_run(source: &str) {
    let module = compile_source(source);

    println!("Running VM with {} functions", module.functions.len());
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
    }

    let mut vm = Vm::new();
    vm.load(module).unwrap();
    vm.run().expect("VM execution failed");

    println!("✓ VM execution completed");
}

#[test]
fn global_array_expression_loads_into_gcref_slot() {
    let source = r#"
package main

var first = [2]int{10, 20}

func pick(i int) int {
    return first[i]
}

func main() int {
    return pick(1)
}
"#;

    let module = compile_source(source);
    let pick = module
        .functions
        .iter()
        .find(|func| func.name == "pick")
        .expect("pick function should be compiled");
    let global_get = pick
        .code
        .iter()
        .find(|inst| inst.opcode() == Opcode::GlobalGet)
        .expect("global array expression should load the global array reference");

    assert_eq!(
        pick.slot_types[global_get.a as usize],
        vo_runtime::SlotType::GcRef,
        "global arrays are stored as GcRef roots, so GlobalGet destinations must be GcRef locals"
    );
}

#[test]
fn escaped_local_array_slice_does_not_copy_gcref_into_value_temp() {
    let source = r#"
package main

type Small struct {
    a int
    b int
}

func main() int {
    var arr [3]Small
    arr[0] = Small{1, 2}
    arr[1] = Small{3, 4}
    s := arr[:]
    return s[0].a
}
"#;

    let module = compile_source(source);
    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");

    for (pc, inst) in main.code.iter().enumerate() {
        if inst.opcode() == Opcode::Copy {
            assert_eq!(
                main.slot_types[inst.a as usize], main.slot_types[inst.b as usize],
                "Copy at pc {pc} must not cross slot layouts"
            );
        }
    }
}

#[test]
fn named_reference_iface_wrapper_uses_gcref_data_slot() {
    let source = r#"
package main

type IntSlice []int

func (s IntSlice) Sum() int {
    return len(s)
}

type Summer interface {
    Sum() int
}

func main() int {
    var s Summer = IntSlice{1, 2, 3}
    return s.Sum()
}
"#;

    let module = compile_source(source);
    let wrapper = module
        .functions
        .iter()
        .find(|func| func.name == "Sum$iface")
        .expect("value receiver interface wrapper");

    assert_eq!(
        wrapper.slot_types[0],
        SlotType::GcRef,
        "interface wrapper data slot for named slice receiver must be a GcRef"
    );
    for (pc, inst) in wrapper.code.iter().enumerate() {
        if inst.opcode() == Opcode::Copy {
            assert_eq!(
                wrapper.slot_types[inst.a as usize], wrapper.slot_types[inst.b as usize],
                "Copy at pc {pc} in {} must not cross slot layouts",
                wrapper.name
            );
        }
    }
}

#[test]
fn global_named_struct_array_elem_meta_uses_struct_meta_id_not_rttid() {
    let source = r#"
package main

type Pair struct {
    name string
    next *Pair
}

var pairs = [1]Pair{{name: "root"}}

func main() {}
"#;

    let module = compile_source(source);
    let pair_meta = module
        .named_type_metas
        .iter()
        .find(|meta| meta.name == "main.Pair")
        .expect("Pair named type metadata");
    let expected_meta_id = pair_meta.underlying_meta.meta_id();

    let init = module
        .functions
        .iter()
        .find(|func| func.name == "__init__")
        .expect("__init__ function");
    let (array_new_pc, array_new) = init
        .code
        .iter()
        .enumerate()
        .find(|(_, inst)| inst.opcode() == Opcode::ArrayNew)
        .expect("global array initializer should allocate an array");
    let meta_reg = array_new.b;
    let meta_const = init.code[..array_new_pc]
        .iter()
        .rev()
        .find(|inst| inst.opcode() == Opcode::LoadConst && inst.a == meta_reg)
        .expect("ArrayNew metadata register must be loaded from a constant");
    let raw = match module.constants.get(meta_const.b as usize) {
        Some(vo_vm::bytecode::Constant::Int(raw)) => *raw as u32,
        other => panic!("ArrayNew metadata constant must be int, got {other:?}"),
    };
    let actual = ValueMeta::from_raw(raw);

    assert_eq!(actual.value_kind(), ValueKind::Struct);
    assert_eq!(
        actual.meta_id(),
        expected_meta_id,
        "global array element ValueMeta must store StructMeta id, not RTTID"
    );
    assert!(
        (actual.meta_id() as usize) < module.struct_metas.len(),
        "global array element StructMeta id must be in range"
    );
}

#[test]
fn anonymous_interface_runtime_type_uses_exact_interface_meta_id() {
    let source = r#"
package main

type Holder struct {
    r interface {
        Read() int
    }
}

func main() {}
"#;

    let module = compile_source(source);
    let (meta_id, methods) = module
        .runtime_types
        .iter()
        .find_map(|rt| match rt {
            vo_runtime::RuntimeType::Interface { methods, meta_id }
                if methods.iter().any(|m| m.name == "Read") =>
            {
                Some((*meta_id, methods))
            }
            _ => None,
        })
        .expect("anonymous interface runtime type");

    let meta = module
        .interface_metas
        .get(meta_id as usize)
        .expect("anonymous interface runtime type must reference an InterfaceMeta");
    assert_eq!(meta.method_names, vec!["Read".to_string()]);
    assert_eq!(
        methods.len(),
        meta.methods.len(),
        "RuntimeType::Interface must stay aligned with exact InterfaceMeta"
    );
}

#[test]
fn runtime_interface_types_do_not_fallback_to_meta_zero() {
    const TYPE_INTERNER_SOURCE: &str = include_str!("../src/type_interner.rs");
    let source = TYPE_INTERNER_SOURCE
        .split("#[cfg(test)]")
        .next()
        .unwrap_or(TYPE_INTERNER_SOURCE);

    assert!(
        !source.contains("ctx.interface_meta_ids.get(&type_key).copied().unwrap_or(0)"),
        "non-empty interface runtime metadata must not silently fallback to meta 0"
    );
}

#[test]
fn codegen_layout_metadata_does_not_default_unresolved_shapes_to_zero() {
    let sources = [
        ("type_interner.rs", include_str!("../src/type_interner.rs")),
        ("type_info.rs", include_str!("../src/type_info.rs")),
        ("wrapper.rs", include_str!("../src/wrapper.rs")),
        ("lib.rs", include_str!("../src/lib.rs")),
    ];
    let forbidden = [
        "len: arr.len().unwrap_or(0)",
        "let len = arr.len().unwrap_or(0) as usize",
        ".try_as_tuple() .map(|t|",
        ".typ() .map(|sig_type|",
        ".filter_map(|&p|",
        ".filter_map(|&r|",
        ".filter_map(|&v|",
        "RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false, }",
        "debug_assert!",
        "debug_assert_eq!",
        ".min(self.slot_types.len())",
        "vec![SlotType::Value; count]",
    ];

    for (name, source) in sources {
        let source = source.split("#[cfg(test)]").next().unwrap_or(source);
        let normalized = source.split_whitespace().collect::<Vec<_>>().join(" ");
        for pattern in forbidden {
            assert!(
                !normalized.contains(pattern),
                "{name} must fail fast on unresolved codegen layout metadata instead of defaulting to zero"
            );
        }
    }
}

#[test]
fn fail_error_return_temp_uses_interface_slot_layout() {
    let source = r#"
package main

type MyError struct {
    msg string
}

func (e MyError) Error() string {
    return e.msg
}

func makeErr() error {
    return MyError{msg: "boom"}
}

func f(ok bool) (int, error) {
    if !ok {
        fail makeErr()
    }
    return 1, nil
}

func main() int {
    _, err := f(false)
    if err == nil {
        return 1
    }
    return 0
}
"#;

    let module = compile_source(source);
    let make_err_id = module
        .functions
        .iter()
        .position(|func| func.name == "makeErr")
        .expect("makeErr function should be compiled") as u32;
    let func = module
        .functions
        .iter()
        .find(|func| func.name == "f")
        .expect("f function should be compiled");
    let call = func
        .code
        .iter()
        .find(|inst| {
            inst.opcode() == Opcode::Call
                && inst.static_call_func_id() == make_err_id
                && inst.packed_ret_slots() == 2
        })
        .expect("fail makeErr() should call makeErr into a two-slot error temp");
    let ret_start = call.packed_call_ret_start() as usize;

    assert_eq!(
        &func.slot_types[ret_start..ret_start + 2],
        &[
            vo_runtime::SlotType::Interface0,
            vo_runtime::SlotType::Interface1
        ],
        "error/interface temporaries must use the Interface0/Interface1 GC layout"
    );

    let error_return = func
        .code
        .iter()
        .find(|inst| {
            inst.opcode() == Opcode::Return
                && (inst.flags & RETURN_FLAG_ERROR_RETURN) != 0
                && inst.b >= 2
        })
        .expect("fail should lower to an error Return");
    let ret_error_start = error_return.a + error_return.b - 2;
    let error_copy = func
        .code
        .iter()
        .find(|inst| {
            inst.opcode() == Opcode::CopyN && inst.a == ret_error_start && inst.copy_n_count() == 2
        })
        .expect("fail should copy the propagated error into the declared return buffer");
    let error_temp = error_copy.b as usize;

    assert_eq!(
        &func.slot_types[error_temp..error_temp + 2],
        &[
            vo_runtime::SlotType::Interface0,
            vo_runtime::SlotType::Interface1
        ],
        "the propagated fail error temp must use the Interface0/Interface1 GC layout"
    );
}

#[test]
fn fail_error_return_zero_values_keep_declared_typed_layout() {
    let source = r#"
package main

type MyError struct {
    msg string
}

func (e MyError) Error() string {
    return e.msg
}

func makeErr() error {
    return MyError{msg: "boom"}
}

func f() (float64, error) {
    fail makeErr()
}

func main() {}
"#;

    let module = compile_source(source);
    let func = module
        .functions
        .iter()
        .find(|func| func.name == "f")
        .expect("f function should be compiled");
    let error_return = func
        .code
        .iter()
        .find(|inst| {
            inst.opcode() == Opcode::Return
                && (inst.flags & RETURN_FLAG_ERROR_RETURN) != 0
                && inst.b == 3
        })
        .expect("fail should lower to a typed three-slot error return");
    let ret_start = error_return.a as usize;

    assert_eq!(
        &func.slot_types[ret_start..ret_start + 3],
        &[SlotType::Float, SlotType::Interface0, SlotType::Interface1],
        "fail zero-value return buffers must keep the declared result slot layout"
    );
}

#[test]
fn codegen_error_zeroing_goes_through_func_builder_helper() {
    let return_stmt = include_str!("../src/stmt/return_stmt.rs");
    let func_builder = include_str!("../src/func.rs");

    assert!(
        return_stmt.contains("func.emit_zero_slots(ret_start, total_ret_slots);"),
        "fail error returns must use FuncBuilder's zero-slot helper"
    );
    assert!(
        !return_stmt.contains("Opcode::LoadInt"),
        "return statement lowering must not grow a second zero-slot emission loop"
    );
    assert!(
        func_builder.contains("self.emit_zero_slots(dst, result_slots);"),
        "error propagation must share the same zero-slot helper"
    );
}

#[test]
fn test_jit_instruction_metadata_for_dynamic_slice_and_map_ops() {
    let source = r#"
package main

type Big struct {
    a int
    b int
    c int
    d int
    e int
    f int
    g int
    h int
    i string
}

func main() int {
    xs := make([]Big, 1)
    xs[0] = Big{}
    xs = append(xs, Big{})

    m := make(map[Big]Big)
    _, ok := m[Big{}]
    m[Big{}] = Big{}
    delete(m, Big{})

    if ok {
        return len(xs)
    }
    return len(xs)
}
"#;

    let module = compile_source(source);
    let main = module
        .functions
        .iter()
        .find(|func| {
            func.code.iter().any(|inst| {
                matches!(
                    inst.opcode(),
                    Opcode::SliceSet
                        | Opcode::SliceAppend
                        | Opcode::MapGet
                        | Opcode::MapSet
                        | Opcode::MapDelete
                )
            })
        })
        .expect("expected compiled main body with slice/map operations");
    let metadata_dump = || {
        let ops = main
            .code
            .iter()
            .enumerate()
            .map(|(pc, inst)| format!("{pc}: {:?} flags={}", inst.opcode(), inst.flags))
            .collect::<Vec<_>>()
            .join("\n");
        let metadata = main
            .code
            .iter()
            .zip(&main.jit_metadata)
            .enumerate()
            .filter(|(_, (_, meta))| !matches!(meta, JitInstructionMetadata::None))
            .map(|(pc, (inst, meta))| {
                format!("{pc}: {:?} flags={} {:?}", inst.opcode(), inst.flags, meta)
            })
            .collect::<Vec<_>>()
            .join("\n");
        format!("ops:\n{ops}\nmetadata:\n{metadata}")
    };

    assert_eq!(main.code.len(), main.jit_metadata.len());
    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::SliceSet
                    && inst.flags == 0
                    && matches!(
                        meta,
                        JitInstructionMetadata::ElemLayout {
                            elem_bytes: 72,
                            needs_sign_extend: false,
                            slot_layout,
                        }
                        if slot_layout.len() == 9
                    )
            }),
        "dynamic-width SliceSet should carry explicit JIT element metadata; got:\n{}",
        metadata_dump()
    );
    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::SliceAppend
                    && inst.flags == 0
                    && matches!(
                        meta,
                        JitInstructionMetadata::ElemLayout {
                            elem_bytes: 72,
                            needs_sign_extend: false,
                            slot_layout,
                        }
                        if slot_layout.len() == 9
                    )
            }),
        "dynamic-width SliceAppend should carry explicit JIT element metadata"
    );
    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::MapGet
                    && matches!(
                        meta,
                        JitInstructionMetadata::MapGet {
                            key_layout,
                            val_layout,
                            has_ok: true
                        } if key_layout.len() == 9
                            && val_layout.len() == 9
                            && key_layout.iter().any(|st| matches!(st, SlotType::GcRef))
                            && val_layout.iter().any(|st| matches!(st, SlotType::GcRef))
                    )
            }),
        "comma-ok MapGet should carry explicit key/value/ok metadata"
    );
    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::MapSet
                    && matches!(
                        meta,
                        JitInstructionMetadata::MapSet {
                            key_layout,
                            val_layout
                        } if key_layout.len() == 9
                            && val_layout.len() == 9
                            && key_layout.iter().any(|st| matches!(st, SlotType::GcRef))
                            && val_layout.iter().any(|st| matches!(st, SlotType::GcRef))
                    )
            }),
        "MapSet should carry explicit key/value metadata"
    );
    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::MapDelete
                    && matches!(
                        meta,
                        JitInstructionMetadata::MapDelete { key_layout }
                            if key_layout.len() == 9
                                && key_layout.iter().any(|st| matches!(st, SlotType::GcRef))
                    )
            }),
        "MapDelete should carry explicit key metadata"
    );
}

#[test]
fn type_assert_and_shared_closure_calls_emit_precise_jit_layout_metadata() {
    let source = r#"
package main

type Box struct {
    v int
}

func main() int {
    var x any = "hello"
    s := x.(string)

    f := func(v string, p *Box) {}
    d := func(v string) {}
    var n Box

    go f(s, &n)
    defer d(s)
    return len(s)
}
"#;

    let module = compile_source(source);
    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");

    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::IfaceAssert
                    && matches!(
                        meta,
                        JitInstructionMetadata::IfaceAssertLayout { result_layout }
                            if result_layout.as_slice() == [SlotType::GcRef]
                    )
            }),
        "type assertion to string must carry exact IfaceAssert result layout"
    );
    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::GoStart
                    && inst.call_shape_is_closure()
                    && matches!(
                        meta,
                        JitInstructionMetadata::CallLayout {
                            arg_layout,
                            ret_layout
                        } if arg_layout.as_slice() == [SlotType::GcRef, SlotType::GcRef]
                            && ret_layout.is_empty()
                    )
            }),
        "closure go call must carry exact CallLayout argument slots"
    );
    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::DeferPush
                    && inst.call_shape_is_closure()
                    && matches!(
                        meta,
                        JitInstructionMetadata::CallLayout {
                            arg_layout,
                            ret_layout
                        } if arg_layout.as_slice() == [SlotType::GcRef] && ret_layout.is_empty()
                    )
            }),
        "closure defer call must carry exact CallLayout argument slots"
    );
}

#[test]
fn extern_call_string_and_slice_arguments_use_gc_ref_slots() {
    let source = r#"
package main

func consume(s string, b []byte);

func main() {
    consume("hello", []byte("world"))
}
"#;

    let module = compile_source(source);
    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");
    let call = main
        .code
        .iter()
        .find(|inst| inst.opcode() == Opcode::CallExtern)
        .expect("extern call should be emitted");

    assert_eq!(
        &main.slot_types[call.b as usize..call.b as usize + 2],
        &[SlotType::GcRef, SlotType::GcRef],
        "extern call argument buffers must preserve GC-ref layout for string/slice values"
    );

    for inst in main
        .code
        .iter()
        .filter(|inst| inst.opcode() == Opcode::StrNew)
    {
        assert_eq!(
            main.slot_types[inst.a as usize],
            SlotType::GcRef,
            "StrNew destination slot must be a GC root"
        );
    }
}

#[test]
fn string_conversion_uses_gc_ref_source_and_destination_slots() {
    let source = r#"
package main

func main() {
    _ = []byte("hello")
}
"#;

    let module = compile_source(source);
    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");
    let call = main
        .code
        .iter()
        .find(|inst| inst.opcode() == Opcode::CallExtern)
        .expect("string conversion should lower to an extern helper call");

    assert_eq!(
        main.slot_types[call.a as usize],
        SlotType::GcRef,
        "string-to-slice conversion destination must be a GC root"
    );
    assert_eq!(
        main.slot_types[call.b as usize],
        SlotType::GcRef,
        "string-to-slice conversion source argument must be a GC root"
    );
}

#[test]
fn composite_string_comparison_loads_string_slots_as_gc_refs() {
    let source = r#"
package main

type Pair struct {
    name string
    n int
}

func main() bool {
    left := Pair{name: "left", n: 1}
    right := Pair{name: "right", n: 1}
    return left == right
}
"#;

    let module = compile_source(source);
    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");

    let mut saw_str_eq = false;
    for inst in main
        .code
        .iter()
        .filter(|inst| inst.opcode() == Opcode::StrEq)
    {
        saw_str_eq = true;
        assert_eq!(
            main.slot_types[inst.b as usize],
            SlotType::GcRef,
            "StrEq lhs must be loaded into a GC-ref temp"
        );
        assert_eq!(
            main.slot_types[inst.c as usize],
            SlotType::GcRef,
            "StrEq rhs must be loaded into a GC-ref temp"
        );
    }
    assert!(saw_str_eq, "composite string comparison should emit StrEq");
    assert!(
        main.code
            .iter()
            .all(|inst| !matches!(inst.opcode(), Opcode::SlotGet | Opcode::SlotGetN)),
        "heterogeneous composite comparison must use static slot offsets, not SlotGet metadata intended for homogeneous stack arrays"
    );
}

#[test]
fn map_literal_ident_string_key_uses_precise_key_slot_layout() {
    let source = r#"
package main

func main() {
    key := "answer"
    m := map[string]int{key: 42}
    _ = m
}
"#;

    let module = compile_source(source);
    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");
    let map_set = main
        .code
        .iter()
        .find(|inst| inst.opcode() == Opcode::MapSet)
        .expect("map literal should emit MapSet");

    assert_eq!(
        main.slot_types[map_set.b as usize + 1],
        SlotType::GcRef,
        "MapSet meta+key buffer must use the map key's precise slot layout"
    );
    assert!(
        main.code
            .iter()
            .zip(&main.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == Opcode::MapSet
                    && matches!(
                        meta,
                        JitInstructionMetadata::MapSet {
                            key_layout,
                            val_layout,
                        } if key_layout == &[SlotType::GcRef] && val_layout == &[SlotType::Value]
                    )
            }),
        "MapSet JIT metadata must carry exact key/value layouts"
    );
}

#[test]
fn generated_function_defs_satisfy_jit_verifier_invariants() {
    let source = r#"
package main

func host(s string) int;

func helper(v int) (int, error) {
    defer func() {}()
    if v > 0 {
        return host("x"), nil
    }
    return 0, nil
}

func main() int {
    v, err := helper(1)
    if err != nil {
        return 99
    }
    return v
}
"#;

    let module = compile_source(source);
    for func in &module.functions {
        assert_eq!(
            func.local_slots as usize,
            func.slot_types.len(),
            "{} must keep local_slots aligned with slot_types",
            func.name
        );
        assert_eq!(
            func.ret_slots as usize,
            func.ret_slot_types.len(),
            "{} must keep ret_slots aligned with ret_slot_types",
            func.name
        );
        assert_eq!(
            func.code.len(),
            func.jit_metadata.len(),
            "{} must have one JIT metadata entry per instruction",
            func.name
        );
        assert_eq!(
            func.gc_scan_slots,
            FunctionDef::compute_gc_scan_slots(&func.slot_types),
            "{} must serialize derived GC scan slots",
            func.name
        );
        assert_eq!(
            func.borrowed_scan_slots_prefix,
            FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types),
            "{} must serialize derived borrowed scan prefix",
            func.name
        );
        assert_eq!(
            func.has_defer,
            func.code
                .iter()
                .any(|inst| { matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush) }),
            "{} must keep has_defer derived from bytecode",
            func.name
        );
        let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&func.code);
        assert_eq!(func.has_calls, has_calls, "{} has_calls drifted", func.name);
        assert_eq!(
            func.has_call_extern, has_call_extern,
            "{} has_call_extern drifted",
            func.name
        );
        for (pc, inst) in func.code.iter().enumerate() {
            if inst.opcode() == Opcode::Call {
                let callee_id = inst.static_call_func_id();
                let callee = module.functions.get(callee_id as usize).unwrap_or_else(|| {
                    panic!(
                        "{} Call at pc {} targets missing function {}",
                        func.name, pc, callee_id
                    )
                });
                if callee.param_slots <= u8::MAX as u16 && callee.ret_slots <= u8::MAX as u16 {
                    assert_eq!(
                        inst.packed_arg_slots(),
                        callee.param_slots,
                        "{} Call at pc {} must mirror callee {} param_slots",
                        func.name,
                        pc,
                        callee.name
                    );
                    assert_eq!(
                        inst.packed_ret_slots(),
                        callee.ret_slots,
                        "{} Call at pc {} must mirror callee {} ret_slots",
                        func.name,
                        pc,
                        callee.name
                    );
                } else {
                    assert_eq!(
                        inst.c, 0,
                        "{} large Call at pc {} must use zero packed shape mirror",
                        func.name, pc
                    );
                }
            }
            if inst.opcode() == Opcode::Return && (inst.flags & RETURN_FLAG_HEAP_RETURNS) == 0 {
                assert_eq!(
                    inst.b, func.ret_slots,
                    "{} Return at pc {} must encode the declared ret_slots",
                    func.name, pc
                );
            }
        }
    }
}

#[test]
fn fallthrough_return_for_typed_result_preserves_slot_layout() {
    let source = r#"
package main

func pi() float64 {
    return 1.5
}

func main() {
    _ = pi()
}
"#;

    let module = compile_source(source);
    let pi = module
        .functions
        .iter()
        .find(|func| func.name == "pi")
        .expect("pi function should be compiled");
    assert_eq!(pi.ret_slot_types, vec![SlotType::Float]);

    let fallthrough = pi
        .code
        .iter()
        .enumerate()
        .rev()
        .find(|(_, inst)| inst.opcode() == Opcode::Return)
        .expect("pi must have a fallthrough Return");
    assert_eq!(
        pi.slot_types.get(fallthrough.1.a as usize),
        Some(&SlotType::Float),
        "fallthrough Return must use a Float return buffer"
    );
    if let Some(prev) = fallthrough
        .0
        .checked_sub(1)
        .and_then(|pc| pi.code.get(pc).map(|inst| (pc, inst)))
    {
        assert!(
            !(prev.1.opcode() == Opcode::LoadInt && prev.1.a == fallthrough.1.a),
            "fallthrough Return at pc {} must not be preceded by slot-agnostic LoadInt initialization at pc {}",
            fallthrough.0,
            prev.0
        );
    }
    for (pc, inst) in pi.code.iter().enumerate() {
        if inst.opcode() == Opcode::LoadInt && inst.a == fallthrough.1.a {
            assert!(
                pc + 1 != fallthrough.0,
                "fallthrough Return at pc {} must not use LoadInt zero at pc {pc} to initialize its typed return slot",
                fallthrough.0
            );
        }
    }
}

#[test]
fn entry_call_to_returning_main_uses_exact_static_call_shape() {
    let source = r#"
package main

func main() int {
    return 1
}
"#;

    let module = compile_source(source);
    let (main_id, main_func) = module
        .functions
        .iter()
        .enumerate()
        .find(|(_, func)| func.name == "main")
        .expect("main function should be compiled");
    let entry = module
        .functions
        .iter()
        .find(|func| func.name == "__entry__")
        .expect("__entry__ function should be compiled");
    let call = entry
        .code
        .iter()
        .find(|inst| inst.opcode() == Opcode::Call && inst.static_call_func_id() == main_id as u32)
        .expect("__entry__ must call main");

    assert_eq!(call.packed_arg_slots(), main_func.param_slots);
    assert_eq!(call.packed_ret_slots(), main_func.ret_slots);
    assert_eq!(
        &entry.slot_types[call.b as usize..call.b as usize + main_func.ret_slots as usize],
        main_func.ret_slot_types.as_slice(),
        "__entry__ must allocate a discard return buffer with main's exact slot layout"
    );
}

#[test]
fn conditional_tail_terminator_keeps_valid_fallthrough_return_target() {
    let source = r#"
package main

func assert(cond bool) {
    if !cond {
        panic("fail")
    }
}

func main() {
    assert(true)
}
"#;

    let module = compile_source(source);
    let assert_func = module
        .functions
        .iter()
        .find(|func| func.name == "assert")
        .expect("assert function should be compiled");
    assert_eq!(
        assert_func.code.last().map(|inst| inst.opcode()),
        Some(Opcode::Return),
        "conditional tail terminators still need a concrete fallthrough return"
    );

    let code_len = assert_func.code.len() as i64;
    for (pc, inst) in assert_func.code.iter().enumerate() {
        if matches!(
            inst.opcode(),
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot
        ) {
            let target = pc as i64 + i64::from(inst.imm32());
            assert!(
                (0..code_len).contains(&target),
                "{} branch at pc {pc} targets {target}, outside code length {code_len}",
                assert_func.name
            );
        }
    }
}

#[test]
fn generated_transfer_rttids_store_value_kind_tags() {
    let source = r#"
package main

type T00 struct { v int }
type T01 struct { v int }
type T02 struct { v int }
type T03 struct { v int }
type T04 struct { v int }
type T05 struct { v int }
type T06 struct { v int }
type T07 struct { v int }
type T08 struct { v int }
type T09 struct { v int }
type T10 struct { v int }
type T11 struct { v int }
type T12 struct { v int }
type T13 struct { v int }
type T14 struct { v int }
type T15 struct { v int }
type T16 struct { v int }
type T17 struct { v int }
type T18 struct { v int }
type T19 struct { v int }
type T20 struct { v int }
type T21 struct { v int }
type T22 struct { v int }
type T23 struct { v int }
type T24 struct { v int }
type T25 struct { v int }
type T26 struct { v int }
type T27 struct { v int }
type T28 struct { v int }
type T29 struct { v int }

func many(
    a00 T00, a01 T01, a02 T02, a03 T03, a04 T04,
    a05 T05, a06 T06, a07 T07, a08 T08, a09 T09,
    a10 T10, a11 T11, a12 T12, a13 T13, a14 T14,
    a15 T15, a16 T16, a17 T17, a18 T18, a19 T19,
    a20 T20, a21 T21, a22 T22, a23 T23, a24 T24,
    a25 T25, a26 T26, a27 T27, a28 T28, a29 T29,
) {}

func main() {}
"#;

    let module = compile_source(source);
    let many = module
        .functions
        .iter()
        .find(|func| func.name == "many")
        .expect("many function");
    assert!(
        !many.param_types.is_empty(),
        "function parameters must carry transfer metadata for island/defer paths"
    );

    for (idx, transfer) in many.param_types.iter().enumerate() {
        let meta_kind = ValueKind::try_from(transfer.meta_raw as u8)
            .unwrap_or_else(|_| panic!("param_types[{idx}] invalid ValueMeta kind tag"));
        let rttid_kind = ValueKind::try_from(transfer.rttid_raw as u8)
            .unwrap_or_else(|_| panic!("param_types[{idx}] invalid ValueRttid kind tag"));
        assert_eq!(
            rttid_kind, meta_kind,
            "param_types[{idx}] must store packed ValueRttid, not a bare RTTID"
        );
    }
}

#[test]
fn generated_jump_conditions_are_value_slots_for_jit() {
    let source = r#"
package main

type Box struct { v int }

func maybe(ok bool) (int, error) {
    if ok {
        return 1, nil
    }
    return 0, nil
}

func main() int {
    var p *Box
    if p == nil {
        p = &Box{v: 1}
    }
    _, err := maybe(false)
    if err != nil {
        return p.v
    }
    return 0
}
"#;

    let module = compile_source(source);
    for func in &module.functions {
        for inst in &func.code {
            if matches!(inst.opcode(), Opcode::JumpIf | Opcode::JumpIfNot) {
                assert_eq!(
                    func.slot_types[inst.a as usize],
                    SlotType::Value,
                    "{} {:?} condition must be a Value slot, got {:?}",
                    func.name,
                    inst.opcode(),
                    func.slot_types[inst.a as usize]
                );
            }
        }
    }
}

#[test]
fn test_simple_int_literal() {
    let source = r#"
package main

func main() int {
    return 42
}
"#;

    let module = compile_source(source);

    // Verify module structure
    assert!(
        !module.functions.is_empty(),
        "should have at least one function"
    );

    println!("✓ Compiled simple int literal");
    println!("  Functions: {}", module.functions.len());
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
    }
}

#[test]
fn test_simple_arithmetic() {
    let source = r#"
package main

func main() int {
    x := 1 + 2
    return x
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled simple arithmetic");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }
}

#[test]
fn test_variable_declaration() {
    let source = r#"
package main

func main() int {
    var x int = 10
    var y int = 20
    return x + y
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled variable declaration");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
    }
}

#[test]
fn test_if_statement() {
    let source = r#"
package main

func main() int {
    x := 10
    if x > 5 {
        return 1
    }
    return 0
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled if statement");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }
}

#[test]
fn test_for_loop() {
    let source = r#"
package main

func main() int {
    sum := 0
    for i := 0; i < 10; i = i + 1 {
        sum = sum + i
    }
    return sum
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled for loop");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
    }
}

#[test]
fn test_struct_field_access() {
    let source = r#"
package main

type Point struct {
    x int
    y int
}

func main() int {
    p := Point{x: 10, y: 20}
    return p.x + p.y
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled struct field access");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }
}

#[test]
fn test_array_index() {
    let source = r#"
package main

func main() int {
    arr := [3]int{1, 2, 3}
    return arr[0] + arr[1] + arr[2]
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled array index");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
    }
}

#[test]
fn test_go_island_named_function_records_param_types() {
    let source = r#"
package main

func worker(canvasRef string) {
}

func main() {
    i := make(island)
    go @(i) worker("canvas")
}
"#;

    let module = compile_source(source);
    let worker = module
        .functions
        .iter()
        .find(|f| f.name == "worker")
        .expect("worker function should be compiled");

    assert_eq!(
        worker.param_types.len(),
        1,
        "worker should record one transfer param"
    );
    assert_eq!(
        worker.param_types[0].slots, 1,
        "string param should occupy one transfer slot"
    );
}

#[test]
fn test_interface_method_value_wrappers_do_not_collide_across_interfaces() {
    let source = r#"
package main

type Reader interface {
    Read(p []byte)
}

type Flusher interface {
    Read(s string)
}

func main() {
    _ = Reader(nil).Read
    _ = Flusher(nil).Read
}
"#;

    let module = compile_source(source);
    let wrappers: Vec<_> = module
        .functions
        .iter()
        .filter(|f| f.name.starts_with("__method_value_iface_Read_0_t"))
        .collect();

    assert_eq!(
        wrappers.len(),
        2,
        "distinct interface method values must get distinct wrappers"
    );
    assert_eq!(
        wrappers[0].param_types.len(),
        1,
        "method value wrapper transfer params exclude captured receiver"
    );
    assert_eq!(
        wrappers[1].param_types.len(),
        1,
        "method value wrapper transfer params exclude captured receiver"
    );
    assert_ne!(
        wrappers[0].param_types[0].rttid_raw, wrappers[1].param_types[0].rttid_raw,
        "different interface method params must preserve distinct transfer metadata",
    );
}

#[test]
fn test_interface_method_expr_wrappers_do_not_collide_across_interfaces() {
    let source = r#"
package main

type Reader interface {
    Read(p []byte)
}

type Flusher interface {
    Read(s string)
}

func main() {
    _ = Reader.Read
    _ = Flusher.Read
}
"#;

    let module = compile_source(source);
    let wrappers: Vec<_> = module
        .functions
        .iter()
        .filter(|f| f.name.starts_with("Read$mexpr_iface_0_t"))
        .collect();

    assert_eq!(
        wrappers.len(),
        2,
        "distinct interface method expressions must get distinct wrappers"
    );
    assert_eq!(
        wrappers[0].param_types.len(),
        2,
        "method expression wrapper transfer params include receiver plus declared params"
    );
    assert_eq!(
        wrappers[1].param_types.len(),
        2,
        "method expression wrapper transfer params include receiver plus declared params"
    );
    assert_ne!(
        wrappers[0].param_types[0].rttid_raw, wrappers[1].param_types[0].rttid_raw,
        "different interface receivers must preserve distinct transfer metadata",
    );
    assert_ne!(
        wrappers[0].param_types[1].rttid_raw, wrappers[1].param_types[1].rttid_raw,
        "different interface method params must preserve distinct transfer metadata",
    );
}

#[test]
fn test_value_method_value_wrapper_uses_precise_slot_layout_and_box_capture() {
    let source = r#"
package main

type Box struct {
    v any
}

func (b Box) Read(p []byte) []byte {
    return p
}

func main() {
    var b Box
    _ = b.Read
}
"#;

    let module = compile_source(source);
    let wrappers: Vec<_> = module
        .functions
        .iter()
        .filter(|f| f.name.starts_with("__method_value_") && !f.name.contains("iface"))
        .collect();

    assert_eq!(
        wrappers.len(),
        1,
        "expected exactly one static method value wrapper"
    );
    let wrapper = wrappers[0];
    assert_eq!(
        wrapper.capture_slot_types,
        vec![vo_runtime::SlotType::GcRef]
    );
    assert_eq!(
        wrapper.capture_types.len(),
        1,
        "method value wrapper should record captured receiver transfer type"
    );
    assert_eq!(
        wrapper.capture_types[0].slots, 2,
        "boxed value receiver should preserve full receiver slot count"
    );
    assert_eq!(
        wrapper.param_types.len(),
        1,
        "wrapper transfer params exclude captured receiver"
    );
    assert_eq!(
        wrapper.slot_types,
        vec![
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::Interface0,
            vo_runtime::SlotType::Interface1,
            vo_runtime::SlotType::Interface0,
            vo_runtime::SlotType::Interface1,
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::GcRef,
        ],
        "value-receiver method value wrapper must keep receiver and call buffer slot layout precise",
    );
}

#[test]
fn test_interface_method_value_wrapper_uses_box_capture_and_precise_slot_layout() {
    let source = r#"
package main

type Reader interface {
    Read(p []byte) []byte
}

func main() {
    var r Reader
    _ = r.Read
}
"#;

    let module = compile_source(source);
    let wrappers: Vec<_> = module
        .functions
        .iter()
        .filter(|f| f.name.starts_with("__method_value_iface_Read_0_t"))
        .collect();

    assert_eq!(
        wrappers.len(),
        1,
        "expected exactly one interface method value wrapper"
    );
    let wrapper = wrappers[0];
    assert_eq!(
        wrapper.capture_slot_types,
        vec![vo_runtime::SlotType::GcRef]
    );
    assert_eq!(
        wrapper.capture_types.len(),
        1,
        "interface method value wrapper should capture one interface box"
    );
    assert_eq!(
        wrapper.capture_types[0].slots, 2,
        "interface capture box must preserve both interface slots"
    );
    assert_eq!(
        wrapper.param_types.len(),
        1,
        "wrapper transfer params exclude captured receiver"
    );
    assert_eq!(
        wrapper.slot_types,
        vec![
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::Interface0,
            vo_runtime::SlotType::Interface1,
            vo_runtime::SlotType::Value,
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::GcRef,
        ],
        "interface method value wrapper must keep interface temp slots and call buffer precisely typed",
    );
}

#[test]
fn test_defer_iface_wrapper_uses_precise_param_and_call_buffer_slot_layout() {
    let source = r#"
package main

type Reader interface {
    Read(p []byte)
}

func main() {
    var r Reader
    defer r.Read(nil)
}
"#;

    let module = compile_source(source);
    let wrappers: Vec<_> = module
        .functions
        .iter()
        .filter(|f| f.name == "Read$defer_iface_0")
        .collect();

    assert_eq!(
        wrappers.len(),
        1,
        "expected exactly one defer interface wrapper"
    );
    let wrapper = wrappers[0];
    assert_eq!(
        wrapper.slot_types,
        vec![
            vo_runtime::SlotType::Interface0,
            vo_runtime::SlotType::Interface1,
            vo_runtime::SlotType::GcRef,
            vo_runtime::SlotType::Value,
            vo_runtime::SlotType::GcRef,
        ],
        "defer iface wrapper must keep interface receiver and forwarded args precisely typed",
    );
}

#[test]
fn test_function_call() {
    let source = r#"
package main

func add(a int, b int) int {
    return a + b
}

func main() int {
    return add(10, 20)
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled function call");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }
}

#[test]
fn test_builtin_len() {
    let source = r#"
package main

func main() int {
    arr := [5]int{1, 2, 3, 4, 5}
    return len(arr)
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled builtin len");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
    }
}

// === VM Execution Tests ===

#[test]
fn test_vm_simple_return() {
    let source = r#"
package main

func main() int {
    return 42
}
"#;
    compile_and_run(source);
}

#[test]
fn test_vm_arithmetic() {
    let source = r#"
package main

func main() int {
    x := 1 + 2
    return x
}
"#;
    compile_and_run(source);
}

#[test]
fn test_vm_function_call() {
    let source = r#"
package main

func add(a int, b int) int {
    return a + b
}

func main() int {
    return add(10, 20)
}
"#;
    compile_and_run(source);
}

// TODO: test_address_of_struct - Vo 只支持对 struct 取地址 (&x)，不支持 *int

#[test]
fn test_non_escaped_int() {
    // Simple test: no closure, no escape
    let source = r#"
package main

func main() int {
    x := 10
    x = 20
    return x
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled non-escaped int (stack allocation)");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // Should use Copy/LoadInt, not PtrNew/PtrGet
    let main_fn = &module.functions[0];
    let has_ptr_new = main_fn.code.iter().any(|i| i.op == 18); // PtrNew = 18
    assert!(!has_ptr_new, "non-escaped int should NOT use PtrNew");
}

#[test]
fn test_non_escaped_struct() {
    let source = r#"
package main

type Point struct {
    x int
    y int
}

func main() int {
    p := Point{x: 1, y: 2}
    p.x = 10
    return p.x + p.y
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled non-escaped struct (stack allocation)");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }
}

// =====================================================
// === Escape / Pointer / Value Type Tests ===
// =====================================================

/// Test 1: Escaped struct keeps value semantics
/// p is declared as value type, but escapes due to &p
/// Internal state: GcRef (same as pointer)
/// Behavior: value semantics (assignment = deep copy)
#[test]
fn test_escaped_struct_value_semantics() {
    let source = r#"
package main

type Point struct {
    x int
    y int
}

func main() int {
    p := Point{x: 10, y: 20}
    ptr := &p
    
    // p2 is also escaped (assigned from escaped p)
    // Key test: p2 = p should deep copy (value semantics)
    p2 := p
    p2.x = 100
    
    // p.x should still be 10 (not 100) because p2 is a copy
    return p.x
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled escaped struct with value semantics");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // p escapes because &p is taken
    // p2 = p should use PtrClone (deep copy), not Copy (shallow)
    // This ensures value semantics for escaped struct
}

/// Test 2: Closure capture - primitive should escape
#[test]
fn test_escaped_int_closure_capture() {
    let source = r#"
package main

func main() int {
    x := 10
    f := func() int {
        return x
    }
    return f()
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled escaped int (closure capture)");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // x should escape because it's captured by closure
}

/// Test 3: Pointer has reference semantics (contrast with Test 1)
/// ptr is declared as *T (pointer type)
/// Assignment copies the pointer, both point to same data
#[test]
fn test_pointer_reference_semantics() {
    let source = r#"
package main

type Point struct {
    x int
    y int
}

func main() int {
    p := Point{x: 10, y: 20}
    ptr := &p
    
    // ptr2 = ptr copies the pointer (reference semantics)
    // Both ptr and ptr2 point to same p
    ptr2 := ptr
    ptr2.x = 100
    
    // ptr.x should be 100 (same data)
    return ptr.x
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled pointer with reference semantics");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // ptr2 = ptr should use Copy (shallow), not PtrClone
    // Both pointers reference the same heap object
}

/// Test 4: Value semantics - struct assignment should copy
#[test]
fn test_struct_value_copy() {
    let source = r#"
package main

type Point struct {
    x int
    y int
}

func main() int {
    p1 := Point{x: 10, y: 20}
    p2 := p1
    p2.x = 100
    return p1.x
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled struct value copy");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // p1.x should still be 10 (value semantics)
}

/// Test 5: Empty interface assignment with struct
#[test]
fn test_empty_interface_assign_struct() {
    let source = r#"
package main

type Box struct {
    val int
}

func main() int {
    b := Box{val: 42}
    var a interface{}
    a = b
    return 0
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled empty interface assign struct");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // Should see IfaceAssign instruction
}

/// Test 6: Interface method call
#[test]
fn test_interface_method_call() {
    let source = r#"
package main

type Adder interface {
    Add() int
}

type MyNum struct {
    value int
}

func (m MyNum) Add() int {
    return m.value + 100
}

func main() int {
    n := MyNum{value: 42}
    var a Adder
    a = n
    return a.Add()
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled interface method call");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // Should see:
    // 1. IfaceAssign to assign struct to interface
    // 2. CallIface to call interface method
}

/// Test 7: Stack array iteration (for-range)
#[test]
fn test_stack_array_iteration() {
    let source = r#"
package main

func main() int {
    arr := [5]int{1, 2, 3, 4, 5}
    sum := 0
    for i, v := range arr {
        sum = sum + v
    }
    return sum
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled stack array iteration");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // Should see:
    // 1. IterNew (StackArray) to create iterator
    // 2. IterNext to get next element
    // 3. Loop body with sum += v
}

/// Test 8: Stack array index assignment
#[test]
fn test_stack_array_index_assign() {
    let source = r#"
package main

func main() int {
    arr := [5]int{1, 2, 3, 4, 5}
    arr[2] = 100
    return arr[2]
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled stack array index assignment");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // Should see SlotSet for arr[2] = 100
}

/// Test 9: Empty interface with int (no escape needed)
#[test]
fn test_empty_interface_int() {
    let source = r#"
package main

func main() int {
    var a interface{}
    a = 42
    return 0
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled empty interface with int");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // int -> interface{} should NOT allocate (inline in data slot)
}

/// Test 7: Nested struct field access (escaped)
#[test]
fn test_nested_struct_escaped() {
    let source = r#"
package main

type Inner struct {
    val int
}

type Outer struct {
    inner Inner
}

func main() int {
    o := Outer{inner: Inner{val: 42}}
    ptr := &o
    return ptr.inner.val
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled nested struct (escaped)");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }
}

/// Test 8: Multi-slot struct on stack (no escape)
#[test]
fn test_multi_slot_stack_struct() {
    let source = r#"
package main

type Rect struct {
    x int
    y int
    w int
    h int
}

func main() int {
    r := Rect{x: 1, y: 2, w: 10, h: 20}
    return r.x + r.y + r.w + r.h
}
"#;

    let module = compile_source(source);

    println!("✓ Compiled multi-slot stack struct");
    for (i, f) in module.functions.iter().enumerate() {
        println!("  [{i}] {}: {} instructions", f.name, f.code.len());
        for (j, inst) in f.code.iter().enumerate() {
            println!("    [{j}] {:?}", inst);
        }
    }

    // Should use CopyN for struct copy, not PtrNew
    let main_fn = module.functions.iter().find(|f| f.name == "main").unwrap();
    let has_ptr_new = main_fn.code.iter().any(|i| i.op == 13); // PtrNew opcode = 13
    assert!(
        !has_ptr_new,
        "non-escaped multi-slot struct should NOT use PtrNew"
    );
}

#[test]
fn test_vm_interface_method_call() {
    let source = r#"
package main

type Adder interface {
    Add() int
}

type MyNum struct {
    value int
}

func (m MyNum) Add() int {
    return m.value + 100
}

func main() int {
    n := MyNum{value: 42}
    var a Adder
    a = n
    return a.Add()
}
"#;
    compile_and_run(source);
}

/// Test true nil interface equals nil
#[test]
fn test_vm_nil_interface() {
    let source = r#"
package main

type error interface {
    Error() string
}

func foo() error {
    return nil  // true nil interface
}

func main() int {
    if foo() != nil {
        panic("WRONG: nil interface should equal nil")
    }
    return 0
}
"#;
    compile_and_run(source);
}

/// Test typed nil interface (Go's classic gotcha)
/// A nil pointer assigned to interface is NOT a nil interface
#[test]
fn test_vm_typed_nil_interface() {
    let source = r#"
package main

type MyError struct{}

func (e *MyError) Error() string { return "err" }

type error interface {
    Error() string
}

func foo() error {
    var e *MyError = nil
    return e
}

func main() int {
    // typed nil: interface has type *MyError but value is nil
    // so foo() != nil (NOT a nil interface)
    if foo() == nil {
        panic("WRONG: typed nil should NOT equal nil interface")
    }
    return 0
}
"#;
    compile_and_run(source);
}
