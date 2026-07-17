//! Integration tests: parse → check → codegen → VM

use std::collections::BTreeMap;
use std::path::PathBuf;
use vo_analysis::importer::NullImporter;
use vo_analysis::vfs::{ModSource, PackageResolver, StdSource};
use vo_analysis::{
    analyze_project_with_identity, AnalysisError, Checker, PackageIdentity, Project,
};
use vo_codegen::{compile_project, CodegenError};
use vo_common::vfs::{FileSet, MemoryFs};
use vo_common::SourceMap;
use vo_common_core::bytecode::{
    ExtSlotKind, ParamShape, IFACE_ASSIGN_NO_ITAB, RETURN_FLAG_ERROR_RETURN,
    RETURN_FLAG_HEAP_RETURNS,
};
use vo_common_core::verifier::verify_module;
use vo_runtime::{RuntimeType, SlotType, ValueKind, ValueMeta, ValueRttid};
use vo_syntax::parser;
use vo_vm::bytecode::{FunctionDef, JitInstructionMetadata};
use vo_vm::instruction::Opcode;
use vo_vm::vm::Vm;

/// Helper: analyze a single source string (no imports) for tests
fn analyze_source(source: &str) -> Result<Project, AnalysisError> {
    use vo_analysis::arena::ArenaKey;
    use vo_analysis::objects::PackageKey;

    let mut source_map = SourceMap::new();
    source_map.add_file("main.vo", source);
    let (file, diags, interner) = parser::parse(source, 0);
    if diags.has_errors() {
        return Err(AnalysisError::Parse(diags, source_map));
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
        return Err(AnalysisError::Check(diags, source_map));
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
        source_map,
        extensions: Vec::new(),
    })
}

/// Helper: compile Vo source to Module
fn compile_source(source: &str) -> vo_vm::bytecode::Module {
    let project = analyze_source(source).expect("analysis failed");
    compile_project(&project).expect("codegen failed")
}

/// Execution tests use the same always-linked core package set as the CLI.
/// Hand-constructing a single-package `Project` is useful for narrow codegen
/// assertions, but runtime builtins that return `error` require the canonical
/// errors.Error metadata installed by project analysis.
fn compile_source_with_core(source: &str) -> vo_vm::bytecode::Module {
    let mut files = FileSet::new(PathBuf::from("virtual-project"));
    files
        .files
        .insert(PathBuf::from("main.vo"), source.to_string());
    let errors_source = include_str!("../../../stdlib/errors/errors.vo");
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", errors_source)),
        r#mod: ModSource::with_fs(MemoryFs::new()),
    };
    let project = analyze_project_with_identity(
        files,
        &resolver,
        PackageIdentity::new("local/integration-test").unwrap(),
    )
    .expect("project analysis with core packages failed");
    compile_project(&project).expect("codegen with core packages failed")
}

fn compile_project_with_dependency(
    main_source: &str,
    dependency_path: &str,
    dependency_source: &str,
) -> vo_vm::bytecode::Module {
    let module_path = dependency_path
        .split('/')
        .take(3)
        .collect::<Vec<_>>()
        .join("/");
    let mut files = FileSet::new(PathBuf::from("virtual-project"));
    files
        .files
        .insert(PathBuf::from("main.vo"), main_source.to_string());
    let module_fs = MemoryFs::new()
        .with_file(
            format!("{module_path}/vo.mod"),
            format!("module = \"{module_path}\"\nvo = \"^0.1.0\"\n"),
        )
        .with_file(
            format!("{dependency_path}/dependency.vo"),
            dependency_source,
        );
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", "package errors\n")),
        r#mod: ModSource::with_fs(module_fs),
    };
    let project = analyze_project_with_identity(
        files,
        &resolver,
        PackageIdentity::new("github.com/acme/app").unwrap(),
    )
    .expect("project analysis failed");
    compile_project(&project).expect("project codegen failed")
}

#[test]
fn legacy_flattening_collision_produces_distinct_extern_ids() {
    let left_path = "github.com/acme/ext/a/b";
    let right_path = "github.com/acme/ext/a_b";
    let mut files = FileSet::new(PathBuf::from("virtual-project"));
    files.files.insert(
        PathBuf::from("main.vo"),
        format!(
            concat!(
                "package main\n",
                "import left \"{}\"\n",
                "import right \"{}\"\n",
                "func main() {{ left.F(); right.F() }}\n",
            ),
            left_path, right_path
        ),
    );
    let module_fs = MemoryFs::new()
        .with_file(
            "github.com/acme/ext/vo.mod",
            "module = \"github.com/acme/ext\"\n\nvo = \"0.1.0\"\n",
        )
        .with_file(format!("{left_path}/extern.vo"), "package left\nfunc F()\n")
        .with_file(
            format!("{right_path}/extern.vo"),
            "package right\nfunc F()\n",
        );
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", "package errors\n")),
        r#mod: ModSource::with_fs(module_fs),
    };
    let project = analyze_project_with_identity(
        files,
        &resolver,
        PackageIdentity::new("github.com/acme/app").unwrap(),
    )
    .expect("collision fixture analysis");
    let module = compile_project(&project).expect("collision fixture codegen");

    let left_name = vo_common::abi::try_abi_lookup_name(left_path, "F").unwrap();
    let right_name = vo_common::abi::try_abi_lookup_name(right_path, "F").unwrap();
    assert_ne!(left_name, right_name);
    let left_id = module
        .externs
        .iter()
        .position(|external| external.name == left_name)
        .expect("left extern id");
    let right_id = module
        .externs
        .iter()
        .position(|external| external.name == right_name)
        .expect("right extern id");
    assert_ne!(left_id, right_id);
}

#[test]
fn oversized_canonical_extern_name_is_one_stable_target_limit_error() {
    let module_path = "github.com/acme/oversized";
    let target_package_len = vo_common_core::MAX_EXTERN_NAME_BYTES - 11;
    let mut dependency_path = module_path.to_string();
    while dependency_path.len() + 201 <= target_package_len {
        dependency_path.push('/');
        dependency_path.push_str(&"p".repeat(200));
    }
    let final_component_len = target_package_len - dependency_path.len() - 1;
    assert!(
        (1..=vo_common_core::MAX_PORTABLE_PACKAGE_COMPONENT_BYTES).contains(&final_component_len)
    );
    dependency_path.push('/');
    dependency_path.push_str(&"q".repeat(final_component_len));
    assert_eq!(dependency_path.len(), target_package_len);
    let mut files = FileSet::new(PathBuf::from("virtual-project"));
    files.files.insert(
        PathBuf::from("main.vo"),
        format!(
            "package main\nimport oversized \"{dependency_path}\"\nfunc main() {{ oversized.F() }}\n"
        ),
    );
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", "package errors\n")),
        r#mod: ModSource::with_fs(
            MemoryFs::new()
                .with_file(
                    format!("{module_path}/vo.mod"),
                    format!("module = \"{module_path}\"\nvo = \"^0.1.0\"\n"),
                )
                .with_file(
                    format!("{dependency_path}/extern.vo"),
                    "package dep\nfunc F()\n",
                ),
        ),
    };
    let project = analyze_project_with_identity(
        files,
        &resolver,
        PackageIdentity::new("github.com/acme/app").unwrap(),
    )
    .expect("oversized ABI fixture remains a valid source project");

    let error = compile_project(&project).expect_err("ABI wire limit must reject codegen");
    let message = match error {
        CodegenError::TargetLimit(message) => message,
        other => panic!("expected a target-limit diagnostic, got {other:?}"),
    };
    assert!(message.contains("encoded extern name is"));
    assert!(message.contains("exceeding the 4096-byte limit"));
}

fn assert_transfer_metadata_canonical(module: &vo_vm::bytecode::Module) {
    for func in &module.functions {
        for (label, transfers) in [
            ("capture_types", func.capture_types.as_slice()),
            ("param_types", func.param_types.as_slice()),
        ] {
            for (idx, transfer) in transfers.iter().enumerate() {
                let value_rttid = ValueRttid::from_raw(transfer.rttid_raw);
                let canonical_meta = module
                    .canonical_value_meta_for_value_rttid(value_rttid)
                    .unwrap_or_else(|| {
                        panic!(
                            "{} {label}[{idx}] ValueRttid {} must resolve",
                            func.name,
                            value_rttid.rttid()
                        )
                    });
                assert_eq!(
                    transfer.meta_raw,
                    canonical_meta.to_raw(),
                    "{} {label}[{idx}] must use canonical ValueMeta",
                    func.name
                );
                assert_eq!(
                    transfer.slots as usize,
                    module
                        .slot_count_for_value_rttid(value_rttid)
                        .expect("ValueRttid must resolve to slot layout"),
                    "{} {label}[{idx}] slot count must match ValueRttid layout",
                    func.name
                );
            }
        }
    }
}

fn function_elem_layout_dump(func: &FunctionDef, opcode: Opcode) -> String {
    func.code
        .iter()
        .zip(&func.jit_metadata)
        .enumerate()
        .filter(|(_, (inst, _))| inst.opcode() == opcode)
        .map(|(pc, (inst, meta))| {
            let meta_summary = match meta {
                JitInstructionMetadata::ElemLayout {
                    elem_bytes,
                    needs_sign_extend,
                    slot_layout,
                } => format!(
                    "ElemLayout {{ elem_bytes: {elem_bytes}, needs_sign_extend: {needs_sign_extend}, slot_layout_len: {} }}",
                    slot_layout.len()
                ),
                other => format!("{other:?}"),
            };
            format!("{pc}: {:?} flags={} {meta_summary}", inst.opcode(), inst.flags)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn assert_elem_layout_metadata(
    func: &FunctionDef,
    opcode: Opcode,
    elem_bytes: u32,
    slot_count: usize,
) {
    assert!(
        func.code
            .iter()
            .zip(&func.jit_metadata)
            .any(|(inst, meta)| {
                inst.opcode() == opcode
                    && matches!(
                        meta,
                        JitInstructionMetadata::ElemLayout {
                            elem_bytes: actual_bytes,
                            slot_layout,
                            ..
                        } if *actual_bytes == elem_bytes && slot_layout.len() == slot_count
                    )
            }),
        "{opcode:?} must preserve elem_bytes={elem_bytes} and slot_layout len={slot_count}; got:\n{}",
        function_elem_layout_dump(func, opcode)
    );
}

#[test]
fn numeric_conversion_codegen_encodes_signedness_width_and_direct_f32() {
    use vo_common_core::instruction::{
        CONV_FLAG_FLOAT32, CONV_FLAG_UNSIGNED, CONV_WIDTH_32, CONV_WIDTH_8,
    };

    let module = compile_source(
        r#"
package main

type NamedUint uint64

func main() {
    var signed int64 = -1
    var unsigned uint64 = 1
    var named NamedUint = 2
    var wide float64
    var narrow float32
    var smallSigned int8
    var smallUnsigned uint32
    wide = float64(signed)
    wide = float64(unsigned)
    narrow = float32(named)
    smallSigned = int8(wide)
    smallUnsigned = uint32(wide)
    unsigned = unsigned << unsigned
    signed = signed >> signed
    _, _, _, _ = narrow, smallSigned, smallUnsigned, unsigned
}
"#,
    );
    verify_module(&module).expect("conversion module verifies");

    let instructions = module
        .functions
        .iter()
        .flat_map(|function| function.code.iter())
        .collect::<Vec<_>>();
    let i2f_flags = instructions
        .iter()
        .filter(|inst| inst.opcode() == Opcode::ConvI2F)
        .map(|inst| inst.flags)
        .collect::<Vec<_>>();
    assert!(i2f_flags.contains(&0), "signed int64 -> float64");
    assert!(i2f_flags.contains(&CONV_FLAG_UNSIGNED), "uint64 -> float64");
    assert!(
        i2f_flags.contains(&(CONV_FLAG_UNSIGNED | CONV_FLAG_FLOAT32)),
        "named uint64 -> float32 must convert directly"
    );

    let f2i_flags = instructions
        .iter()
        .filter(|inst| inst.opcode() == Opcode::ConvF2I)
        .map(|inst| inst.flags)
        .collect::<Vec<_>>();
    assert!(f2i_flags.contains(&CONV_WIDTH_8), "float64 -> int8");
    assert!(
        f2i_flags.contains(&(CONV_FLAG_UNSIGNED | CONV_WIDTH_32)),
        "float64 -> uint32"
    );
    assert!(instructions.iter().any(|inst| {
        inst.opcode() == Opcode::Shl
            && inst.flags == vo_common_core::instruction::SHIFT_FLAG_RHS_UNSIGNED
    }));
    assert!(instructions
        .iter()
        .any(|inst| inst.opcode() == Opcode::ShrS && inst.flags == 0));
}

fn ptr_new_layouts(module: &vo_vm::bytecode::Module) -> Vec<(String, usize, Vec<SlotType>)> {
    module
        .functions
        .iter()
        .flat_map(|func| {
            func.code
                .iter()
                .zip(&func.jit_metadata)
                .enumerate()
                .filter_map(move |(pc, (inst, meta))| {
                    if inst.opcode() != Opcode::PtrNew {
                        return None;
                    }
                    match meta {
                        JitInstructionMetadata::PtrLayout { value_layout } => {
                            Some((func.name.clone(), pc, value_layout.clone()))
                        }
                        other => panic!(
                            "{}:{pc} PtrNew must carry PtrLayout metadata, got {other:?}",
                            func.name
                        ),
                    }
                })
        })
        .collect()
}

/// Helper: compile and run, verify execution completes
fn compile_and_run(source: &str) {
    let module = compile_source_with_core(source);

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
fn ptr_new_codegen_emits_verifier_layout_metadata_060() {
    let source = r#"
package main

type Node struct {
    next *Node
    value int
}

func main() int {
    n := new(Node)
    m := &Node{next: n, value: 7}
    return m.value
}
"#;

    let module = compile_source(source);
    let ptr_new_layouts = ptr_new_layouts(&module);
    assert!(
        ptr_new_layouts
            .iter()
            .any(|(_, _, layout)| { layout == &[SlotType::GcRef, SlotType::Value] }),
        "PtrNew should carry physical boxed Node layout, got {ptr_new_layouts:?}"
    );
    verify_module(&module).expect("generated PtrNew metadata must satisfy module verifier");
}

#[test]
fn dynamic_method_value_receiver_ptr_new_metadata_fact_is_tracked_061() {
    let source = r#"
package main

type Pair struct {
    a int
    b int
}

func (p Pair) Sum() int {
    return p.a + p.b
}

func main() int {
    p := Pair{a: 3, b: 4}
    var box interface{} = p
    sumMethod, err := box~>Sum
    assert(err == nil, "Sum method lookup on Pair")
    sum := sumMethod.(func() int)
    return sum()
}
"#;

    let module = compile_source(source);
    let main_ptr_new_layouts: Vec<_> = ptr_new_layouts(&module)
        .into_iter()
        .filter(|(func, _, _)| func == "main")
        .collect();

    assert!(
        main_ptr_new_layouts
            .iter()
            .any(|(_, _, layout)| layout == &[SlotType::Value, SlotType::Value]),
        "dynamic method value receiver boxing must carry Pair's two-slot physical layout, got {main_ptr_new_layouts:?}"
    );
    verify_module(&module)
        .expect("generated PtrNew metadata must be backed by tracked constant facts");
}

#[test]
fn interface_arg_conversion_rebuilds_non_empty_itab_before_call_061() {
    let source = r#"
package main

type Reader interface {
    Read() int
}

type Closer interface {
    Close()
}

type ReadCloser interface {
    Reader
    Closer
}

type File struct{}

func (File) Read() int { return 1 }
func (File) Close() {}

func ReadAll(r Reader) int {
    return r.Read()
}

func main() {
    var rc ReadCloser = File{}
    _ = ReadAll(rc)
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("interface arg conversion module should verify");
    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");

    let iface_to_iface_assigns = main
        .code
        .iter()
        .filter(|inst| {
            inst.opcode() == Opcode::IfaceAssign
                && inst.flags == vo_runtime::ValueKind::Interface as u8
        })
        .count();

    assert!(
        iface_to_iface_assigns >= 1,
        "passing ReadCloser to a Reader parameter must rebuild the itab before the call"
    );
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
fn global_array_root_metadata_references_array_runtime_type() {
    let source = r#"
package main

var first = [2]int{10, 20}

func main() {}
"#;

    let module = compile_source(source);
    let global = module
        .globals
        .iter()
        .find(|global| global.name == "first")
        .expect("global array should be registered");

    assert_eq!(ValueKind::try_from(global.value_kind), Ok(ValueKind::Array));
    assert!(
        matches!(
            module.runtime_types.get(global.meta_id as usize),
            Some(vo_runtime::RuntimeType::Array { .. })
        ),
        "global array metadata must reference an array runtime type"
    );
    verify_module(&module).expect("global array module metadata should verify");
}

#[test]
fn package_var_group_commits_only_after_every_rhs_call() {
    let source = r#"
package main

func firstValue() int {
    return 41
}

func secondValue() int {
    panic("stop before package commit")
}

var first, second = firstValue(), secondValue()

func main() {}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("grouped package initializer module should verify");

    let function_id = |name: &str| {
        module
            .functions
            .iter()
            .position(|function| function.name == name)
            .unwrap_or_else(|| panic!("missing function {name}")) as u32
    };
    let global_offset = |name: &str| {
        let mut offset = 0u16;
        for global in &module.globals {
            if global.name == name {
                return offset;
            }
            offset = offset
                .checked_add(global.slots)
                .expect("test global offset must fit u16");
        }
        panic!("missing global {name}");
    };

    let first_value_id = function_id("firstValue");
    let second_value_id = function_id("secondValue");
    let first_offset = global_offset("first");
    let second_offset = global_offset("second");
    let init = module
        .functions
        .iter()
        .find(|function| function.name == "__init__")
        .expect("package init function");

    let call_pc = |callee| {
        init.code
            .iter()
            .position(|inst| inst.opcode() == Opcode::Call && inst.static_call_func_id() == callee)
            .unwrap_or_else(|| panic!("package init must call function {callee}"))
    };
    let set_pc = |offset| {
        init.code
            .iter()
            .position(|inst| {
                matches!(inst.opcode(), Opcode::GlobalSet | Opcode::GlobalSetN) && inst.a == offset
            })
            .unwrap_or_else(|| panic!("package init must write global offset {offset}"))
    };

    let first_call = call_pc(first_value_id);
    let second_call = call_pc(second_value_id);
    let first_set = set_pc(first_offset);
    let second_set = set_pc(second_offset);
    assert!(
        first_call < second_call && second_call < first_set && first_set < second_set,
        "both RHS calls must precede source-order global commits: first_call={first_call}, second_call={second_call}, first_set={first_set}, second_set={second_set}"
    );
}

#[test]
fn large_global_array_commit_uses_a_compact_runtime_loop() {
    let source = r#"
package main

var sparse = [60000]int{59999: 77}

func main() {}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("large sparse package array module should verify");
    let init = module
        .functions
        .iter()
        .find(|function| function.name == "__init__")
        .expect("package init function");

    assert!(
        init.code
            .iter()
            .any(|instruction| instruction.opcode() == Opcode::ForLoop),
        "copying the transactional array value into stable global storage must use a runtime loop"
    );
    assert!(
        init.code.len() < 64,
        "a sparse 60000-element global must keep compact bytecode, got {} instructions",
        init.code.len()
    );
    assert!(
        init.code
            .iter()
            .filter(|instruction| instruction.opcode() == Opcode::ArraySet)
            .count()
            <= 2,
        "the literal write and loop body must remain constant-size"
    );
}

#[test]
fn blank_package_target_is_converted_and_discarded_without_global_storage() {
    let source = r#"
package main

func pair() (int, int) {
    return 41, 42
}

var kept, _ = pair()

func main() {}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("blank package target module should verify");
    assert!(module.globals.iter().any(|global| global.name == "kept"));
    assert!(module.globals.iter().all(|global| global.name != "_"));

    let pair_id = module
        .functions
        .iter()
        .position(|function| function.name == "pair")
        .expect("pair function") as u32;
    let init = module
        .functions
        .iter()
        .find(|function| function.name == "__init__")
        .expect("package init function");
    assert!(init.code.iter().any(|instruction| {
        instruction.opcode() == Opcode::Call && instruction.static_call_func_id() == pair_id
    }));
}

#[test]
fn blank_rhs_panic_call_precedes_every_non_blank_group_commit() {
    let source = r#"
package main

func firstValue() int {
    return 41
}

func stop() int {
    panic("stop before package commit")
}

var kept, _ = firstValue(), stop()

func main() {}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("blank panic initializer module should verify");
    let function_id = |name: &str| {
        module
            .functions
            .iter()
            .position(|function| function.name == name)
            .unwrap_or_else(|| panic!("missing function {name}")) as u32
    };
    let kept_offset = module
        .globals
        .iter()
        .take_while(|global| global.name != "kept")
        .try_fold(0u16, |offset, global| offset.checked_add(global.slots))
        .expect("kept global offset");
    let init = module
        .functions
        .iter()
        .find(|function| function.name == "__init__")
        .expect("package init function");
    let call_pc = |callee| {
        init.code
            .iter()
            .position(|instruction| {
                instruction.opcode() == Opcode::Call && instruction.static_call_func_id() == callee
            })
            .expect("initializer call")
    };
    let set_pc = init
        .code
        .iter()
        .position(|instruction| {
            matches!(instruction.opcode(), Opcode::GlobalSet | Opcode::GlobalSetN)
                && instruction.a == kept_offset
        })
        .expect("kept global commit");

    assert!(call_pc(function_id("firstValue")) < call_pc(function_id("stop")));
    assert!(call_pc(function_id("stop")) < set_pc);
    assert!(module.globals.iter().all(|global| global.name != "_"));
}

#[test]
fn imported_package_blank_target_executes_without_global_storage() {
    let module = compile_project_with_dependency(
        concat!(
            "package main\n",
            "import \"github.com/acme/dep\"\n",
            "var observed = dep.Kept\n",
            "func main() {}\n",
        ),
        "github.com/acme/dep",
        concat!(
            "package dep\n",
            "func pair() (int, int) { return 41, 42 }\n",
            "func sideEffect() int { return 43 }\n",
            "var Kept, _ = pair()\n",
            "var _ = sideEffect()\n",
        ),
    );

    verify_module(&module).expect("imported blank target module should verify");
    assert!(module.globals.iter().any(|global| global.name == "Kept"));
    assert!(module
        .globals
        .iter()
        .any(|global| global.name == "observed"));
    assert!(module.globals.iter().all(|global| global.name != "_"));
    let pair_id = module
        .functions
        .iter()
        .position(|function| function.name == "pair")
        .expect("dependency pair function") as u32;
    let side_effect_id = module
        .functions
        .iter()
        .position(|function| function.name == "sideEffect")
        .expect("dependency side-effect function") as u32;
    let init = module
        .functions
        .iter()
        .find(|function| function.name == "__init__")
        .expect("combined init function");
    assert!(init.code.iter().any(|instruction| {
        instruction.opcode() == Opcode::Call && instruction.static_call_func_id() == pair_id
    }));
    assert!(init.code.iter().any(|instruction| {
        instruction.opcode() == Opcode::Call && instruction.static_call_func_id() == side_effect_id
    }));
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
    verify_module(&module).expect("dynamic slice/map module metadata should verify");
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
fn explicit_return_list_commits_escaped_named_results_after_all_expressions() {
    let source = r#"
package main

type TestError struct {}

func (TestError) Error() string {
    return "boom"
}

func returnWithCall() (result int, err error) {
    errdefer func() {
        result += 1000
    }()
    inner := func() error {
        result = 50
        fail TestError{}
    }
    return result, inner()
}

func callBeforeReturn() (result int, err error) {
    errdefer func() {
        result += 1000
    }()
    inner := func() error {
        result = 50
        fail TestError{}
    }
    err = inner()
    return result, err
}

func partialEscapeBareReturn() (first int, second int) {
    first = 10
    second = 20
    defer func() {
        first++
    }()
    return
}

func partialEscapeLiteral() (int, int) {
    run := func() (first int, second int) {
        first = 30
        second = 40
        defer func() {
            first += 2
        }()
        return 50, first
    }
    return run()
}

func main() {
    result, err := returnWithCall()
    assert(err != nil)
    assert(result == 1000)

    result, err = callBeforeReturn()
    assert(err != nil)
    assert(result == 1050)

    first, second := partialEscapeBareReturn()
    assert(first == 11)
    assert(second == 20)

    first, second = partialEscapeLiteral()
    assert(first == 52)
    assert(second == 30)
}
"#;

    compile_and_run(source);
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
    verify_module(&module).expect(
        "string conversion extern shape must publish the same precise return layout used by CallExtern",
    );
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
fn copy_builtin_extern_uses_fixed_return_layout_062() {
    let source = r#"
package main

func main() int {
    dst := []byte{0, 0}
    src := []byte{1, 2}
    n := copy(dst, src)
    return n
}
"#;

    let module = compile_source(source);
    let copy_extern = module
        .externs
        .iter()
        .find(|extern_def| extern_def.name == "vo_copy")
        .expect("copy builtin must register vo_copy extern");
    assert_eq!(
        copy_extern.returns.slot_types,
        vec![SlotType::Value],
        "vo_copy must publish its fixed scalar return layout"
    );
    verify_module(&module).expect("generated copy builtin ABI must satisfy verifier");
}

#[test]
fn slice_index_preserves_large_element_byte_width_028() {
    let source = r#"
package main

type Big [8192]int

func sliceGet(xs []Big) int {
    v := xs[0]
    return v[0]
}

func sliceSet(xs []Big, v Big) {
    xs[0] = v
}

func main() {}
"#;

    let module = compile_source(source);
    let getter = module
        .functions
        .iter()
        .find(|func| func.name == "sliceGet")
        .expect("sliceGet function");
    let setter = module
        .functions
        .iter()
        .find(|func| func.name == "sliceSet")
        .expect("sliceSet function");

    assert_elem_layout_metadata(getter, Opcode::SliceGet, 65_536, 8192);
    assert_elem_layout_metadata(setter, Opcode::SliceSet, 65_536, 8192);
}

#[test]
fn escaped_array_index_preserves_large_element_byte_width_028() {
    let source = r#"
package main

type Big [8192]int

func heapArrayIndex() int {
    var arr [2]Big
    s := arr[:]
    var v Big
    arr[0] = v
    got := arr[0]
    return got[0] + len(s)
}

func main() {}
"#;

    let module = compile_source(source);
    let func = module
        .functions
        .iter()
        .find(|func| func.name == "heapArrayIndex")
        .expect("heapArrayIndex function");

    assert_elem_layout_metadata(func, Opcode::ArraySet, 65_536, 8192);
    assert_elem_layout_metadata(func, Opcode::ArrayGet, 65_536, 8192);
}

#[test]
fn type_assert_and_shared_closure_calls_emit_precise_jit_layout_metadata() {
    let source = r#"
package main

type Box struct {
    v int
}

func worker(v string, p *Box) {}

func main() int {
    var x any = "hello"
    s := x.(string)

    f := func(v string, p *Box) {}
    d := func(v string) {}
    var n Box

    go f(s, &n)
    go worker(s, &n)
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
                        JitInstructionMetadata::IfaceAssertLayout { result_layout, .. }
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
                inst.opcode() == Opcode::GoStart
                    && !inst.call_shape_is_closure()
                    && matches!(
                        meta,
                        JitInstructionMetadata::CallLayout {
                            arg_layout,
                            ret_layout
                        } if arg_layout.as_slice() == [SlotType::GcRef, SlotType::GcRef]
                            && ret_layout.is_empty()
                    )
            }),
        "static go call must carry exact CallLayout argument slots"
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
fn concrete_to_empty_interface_uses_no_itab_sentinel_even_with_itab_zero() {
    let source = r#"
package main

type Closer interface {
    Close()
}

type Resource struct {}

func (Resource) Close() {}

func main() {
    var c Closer = Resource{}
    var x any = 1
    _ = c
    _ = x
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("IfaceAssign sentinel module should verify");
    assert!(
        !module.itabs.is_empty(),
        "test must create at least one concrete non-empty interface itab"
    );

    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");
    let numeric_empty_iface_assign = main.code.iter().find(|inst| {
        if inst.opcode() != Opcode::IfaceAssign {
            return false;
        }
        let Ok(kind) = ValueKind::try_from(inst.flags) else {
            return false;
        };
        if !matches!(kind, ValueKind::Int | ValueKind::Int64) {
            return false;
        }
        matches!(
            module.constants.get(inst.c as usize),
            Some(vo_vm::bytecode::Constant::Int(raw)) if *raw as u32 == IFACE_ASSIGN_NO_ITAB
        )
    });

    assert!(
        numeric_empty_iface_assign.is_some(),
        "concrete numeric -> empty interface must encode no-itab sentinel instead of borrowing itab 0"
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
        main.slot_types[call.c as usize],
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
fn late_interned_named_runtime_types_are_canonicalized_before_verify() {
    let source = r#"
package main

type Store struct {}

type Tx struct {}

func (s *Store) View(fn func(tx *Tx) error) error {
    return fn(&Tx{})
}

func (tx *Tx) Get(key string) (string, bool, error) {
    return key + "!", true, nil
}

func (s *Store) Get(key string) (string, bool, error) {
    var value string
    var found bool
    err := s.View(func(tx *Tx) error {
        var getErr error
        value, found, getErr = tx.Get(key)
        return getErr
    })
    return value, found, err
}

func main() {
    s := &Store{}
    value, found, err := s.Get("hello")
    assert(err == nil, "Get should succeed")
    assert(found, "found should be true")
    assert(value == "hello!", "value should be captured and assigned")
}
"#;

    let module = compile_source(source);
    assert_transfer_metadata_canonical(&module);
    verify_module(&module).expect("late-interned named runtime types must satisfy verifier");
}

#[test]
fn queue_new_empty_struct_uses_canonical_element_transfer_metadata() {
    let source = r#"
package main

func main() {
    ch := make(chan struct{}, 1)
    ch <- struct{}{}
    <-ch

    p := make(port struct{}, 1)
    p <- struct{}{}
    <-p
}
"#;

    compile_and_run(source);
}

#[test]
fn anonymous_empty_struct_slice_reuses_single_struct_meta_identity() {
    let source = r#"
package main

func accept(data []struct{}) {}

func main() {
    data := make([]struct{}, 5)
    accept(data)
}
"#;

    let module = compile_source(source);
    assert_transfer_metadata_canonical(&module);
    verify_module(&module).expect("anonymous empty struct slice metadata should verify");

    let empty_struct_meta_count = module
        .struct_metas
        .iter()
        .filter(|meta| meta.fields.is_empty() && meta.slot_types == vec![SlotType::Value])
        .count();
    assert_eq!(
        empty_struct_meta_count, 1,
        "structurally identical anonymous empty structs must share one StructMeta id"
    );
}

#[test]
fn discarded_dynamic_calls_encode_callee_return_layout() {
    let source = r#"
package main

type Closer interface {
    Close() error
}

type Sink struct {}

func (s *Sink) Close() error {
    return nil
}

func main() {
    var c Closer = &Sink{}
    c.Close()

    f := func() error {
        return nil
    }
    f()
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("discarded dynamic calls must verify");

    let main = module
        .functions
        .iter()
        .find(|func| func.name == "main")
        .expect("main function");
    let mut saw_iface = false;
    let mut saw_closure = false;
    for (pc, inst) in main.code.iter().enumerate() {
        match inst.opcode() {
            Opcode::CallIface | Opcode::CallClosure => {
                let ret_layout = match (inst.opcode(), &main.jit_metadata[pc]) {
                    (
                        Opcode::CallClosure,
                        JitInstructionMetadata::CallLayout { ret_layout, .. },
                    ) => ret_layout,
                    (
                        Opcode::CallIface,
                        JitInstructionMetadata::CallIfaceLayout { ret_layout, .. },
                    ) => ret_layout,
                    _ => {
                        panic!("dynamic call must carry precise call layout metadata");
                    }
                };
                assert_eq!(
                    inst.packed_ret_slots(),
                    2,
                    "discarded dynamic call must encode error return slots"
                );
                assert_eq!(
                    ret_layout.as_slice(),
                    &[SlotType::Interface0, SlotType::Interface1],
                    "discarded dynamic call must allocate the callee return layout"
                );
                saw_iface |= inst.opcode() == Opcode::CallIface;
                saw_closure |= inst.opcode() == Opcode::CallClosure;
            }
            _ => {}
        }
    }
    assert!(saw_iface, "test must exercise discarded CallIface");
    assert!(saw_closure, "test must exercise discarded CallClosure");
}

#[test]
fn dynamic_field_and_index_externs_are_keyed_by_precise_return_layout_058() {
    let source = r#"
package main

type Data struct {
    Count int
}

func main() {
    var data any = Data{Count: 7}
    fieldAny, err := data~>Count
    var fieldInt int
    fieldInt, err = data~>Count

    var values any = map[string]int{"a": 1, "b": 2}
    indexAny, err := values~>["a"]
    var indexInt int
    indexInt, err = values~>["b"]

    _ = fieldAny
    _ = fieldInt
    _ = indexAny
    _ = indexInt
    _ = err
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("precise dynamic extern return layouts must verify");

    let any_result_layout = vec![
        SlotType::Interface0,
        SlotType::Interface1,
        SlotType::Interface0,
        SlotType::Interface1,
    ];
    let int_result_layout = vec![
        SlotType::Value,
        SlotType::Value,
        SlotType::Interface0,
        SlotType::Interface1,
    ];

    for extern_name in ["dyn_field", "dyn_index"] {
        let extern_defs = module
            .externs
            .iter()
            .filter(|extern_def| extern_def.name == extern_name)
            .collect::<Vec<_>>();
        let return_layouts = extern_defs
            .iter()
            .map(|extern_def| extern_def.returns.slot_types.clone())
            .collect::<Vec<_>>();

        assert!(
            return_layouts
                .iter()
                .any(|layout| layout == &any_result_layout),
            "{extern_name} must keep its any-result layout"
        );
        assert!(
            return_layouts
                .iter()
                .any(|layout| layout == &int_result_layout),
            "{extern_name} must keep its int-result layout"
        );

        for extern_def in extern_defs {
            match extern_name {
                "dyn_field" => {
                    assert_eq!(
                        extern_def.params,
                        ParamShape::Exact { slots: 5 },
                        "dyn_field must keep its exact parameter ABI"
                    );
                    assert_eq!(
                        extern_def.param_kinds,
                        vec![
                            ExtSlotKind::Value,
                            ExtSlotKind::Value,
                            ExtSlotKind::Bytes,
                            ExtSlotKind::Value,
                            ExtSlotKind::Value,
                        ],
                        "dyn_field must keep field-name bytes in the parameter ABI"
                    );
                }
                "dyn_index" => {
                    assert_eq!(
                        extern_def.params,
                        ParamShape::Exact { slots: 6 },
                        "dyn_index must keep its exact parameter ABI"
                    );
                    assert_eq!(
                        extern_def.param_kinds,
                        vec![ExtSlotKind::Value; 6],
                        "dyn_index must keep its six value parameter ABI slots"
                    );
                }
                _ => unreachable!("unexpected dynamic extern"),
            }
        }
    }
}

#[test]
fn dynamic_call_and_method_externs_keep_callsite_param_abi_058() {
    let source = r#"
package main

type Calc struct {
    Value int
}

func (c *Calc) GetInt() int {
    return c.Value
}

func (c *Calc) GetPair() (int, string) {
    return c.Value, "ok"
}

func main() {
    calc := &Calc{Value: 7}
    var obj any = calc
    one, err := obj~>GetInt()
    left, right, err := obj~>GetPair()
    var fn any = func(x int) int {
        return x + 1
    }
    direct, err := fn~>(one.(int))
    _ = one
    _ = left
    _ = right
    _ = direct
    _ = err
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("dynamic call extern ABI must verify");

    let dyn_call_defs = module
        .externs
        .iter()
        .filter(|extern_def| extern_def.name == "dyn_call")
        .collect::<Vec<_>>();
    assert!(
        !dyn_call_defs.is_empty(),
        "source must lower through dyn_call"
    );
    assert!(
        dyn_call_defs
            .iter()
            .any(|extern_def| extern_def.params == ParamShape::Exact { slots: 6 }),
        "dyn_call must keep its one-result callsite parameter ABI"
    );
    for extern_def in dyn_call_defs {
        assert_ne!(
            extern_def.params,
            ParamShape::CallSiteVariadic,
            "dyn_call must not accept arbitrary argument slots"
        );
        assert_eq!(
            extern_def.param_kinds.len(),
            extern_def.params.exact_slots().unwrap() as usize,
            "dyn_call param_kinds must describe every encoded argument slot"
        );
    }

    let dyn_method_defs = module
        .externs
        .iter()
        .filter(|extern_def| extern_def.name == "dyn_method")
        .collect::<Vec<_>>();
    assert!(
        dyn_method_defs.len() >= 2,
        "source must lower one-result and two-result dyn_method calls"
    );
    assert!(
        dyn_method_defs
            .iter()
            .any(|extern_def| extern_def.params == ParamShape::Exact { slots: 7 }),
        "dyn_method must keep its one-result callsite parameter ABI"
    );
    assert!(
        dyn_method_defs
            .iter()
            .any(|extern_def| extern_def.params == ParamShape::Exact { slots: 9 }),
        "dyn_method must keep its two-result callsite parameter ABI"
    );
    for extern_def in dyn_method_defs {
        assert_ne!(
            extern_def.params,
            ParamShape::CallSiteVariadic,
            "dyn_method must not accept arbitrary argument slots"
        );
        assert_eq!(
            extern_def.param_kinds.len(),
            extern_def.params.exact_slots().unwrap() as usize,
            "dyn_method param_kinds must describe every encoded argument slot"
        );
    }
}

#[test]
fn deferred_loop_closure_slice_append_metadata_survives_verifier_058() {
    let mut in_import_group = false;
    let source = include_str!(
        "../../../../tests/lang/cases/skill_debug_vo/2026_01_23_defer_order_complex.vo"
    )
    .lines()
    .filter(|line| {
        let trimmed = line.trim_start();
        if trimmed == "import (" {
            in_import_group = true;
            return false;
        }
        if in_import_group {
            if trimmed == ")" {
                in_import_group = false;
            }
            return false;
        }
        trimmed != "import \"fmt\"" && !trimmed.starts_with("fmt.Println(")
    })
    .collect::<Vec<_>>()
    .join("\n");

    let module = compile_source(&source);
    verify_module(&module).expect("deferred loop closure SliceAppend metadata must verify");
}

#[test]
fn scheduled_interface_wrappers_encode_callee_return_layout_but_discard_results() {
    let source = r#"
package main

type Closer interface {
    Close() error
}

type Sink struct {}

func (s *Sink) Close() error {
    return nil
}

func main() {
    var c Closer = &Sink{}
    defer c.Close()
    go c.Close()
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("scheduled interface wrappers must verify");

    let mut saw_wrapper = false;
    for wrapper in module
        .functions
        .iter()
        .filter(|func| func.name.contains("$defer_iface"))
    {
        saw_wrapper = true;
        assert_eq!(
            wrapper.ret_slots, 0,
            "scheduled interface wrapper {} must discard results",
            wrapper.name
        );
        assert!(
            wrapper.ret_slot_types.is_empty(),
            "scheduled interface wrapper {} must not expose callee results",
            wrapper.name
        );

        let mut saw_call_iface = false;
        let mut saw_empty_return = false;
        for (pc, inst) in wrapper.code.iter().enumerate() {
            match inst.opcode() {
                Opcode::CallIface => {
                    saw_call_iface = true;
                    let JitInstructionMetadata::CallIfaceLayout { ret_layout, .. } =
                        &wrapper.jit_metadata[pc]
                    else {
                        panic!("scheduled interface wrapper CallIface must carry CallIfaceLayout");
                    };
                    assert_eq!(
                        inst.packed_ret_slots(),
                        2,
                        "scheduled interface wrapper CallIface must encode callee error returns"
                    );
                    assert_eq!(
                        ret_layout.as_slice(),
                        &[SlotType::Interface0, SlotType::Interface1],
                        "scheduled interface wrapper CallIface must allocate callee return layout"
                    );
                }
                Opcode::Return => {
                    saw_empty_return = inst.b == 0;
                }
                _ => {}
            }
        }
        assert!(
            saw_call_iface,
            "scheduled interface wrapper {} must call through CallIface",
            wrapper.name
        );
        assert!(
            saw_empty_return,
            "scheduled interface wrapper {} must return no slots",
            wrapper.name
        );
    }
    assert!(
        saw_wrapper,
        "test must generate a scheduled interface wrapper"
    );
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
fn entry_discards_multi_slot_gc_results_with_the_exact_callee_layout() {
    let source = r#"
package main

type Result struct {
    label string
    value any
}

func main() (string, any, Result) {
    return "discarded", []int{1, 2, 3}, Result{label: "done", value: map[string]int{"x": 1}}
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("return-discard entry module must verify");
    let (main_id, main_func) = module
        .functions
        .iter()
        .enumerate()
        .find(|(_, function)| function.name == "main")
        .expect("main function should be compiled");
    let entry = module
        .functions
        .iter()
        .find(|function| function.name == "__entry__")
        .expect("__entry__ function should be compiled");
    let call = entry
        .code
        .iter()
        .find(|instruction| {
            instruction.opcode() == Opcode::Call
                && instruction.static_call_func_id() == main_id as u32
        })
        .expect("__entry__ must call main");

    assert!(
        main_func.ret_slot_types.iter().any(SlotType::is_gc_ref),
        "fixture must exercise GC-backed discarded results"
    );
    assert_eq!(call.packed_arg_slots(), 0);
    assert_eq!(call.packed_ret_slots(), main_func.ret_slots);
    assert_eq!(
        &entry.slot_types[call.b as usize..call.b as usize + main_func.ret_slots as usize],
        main_func.ret_slot_types.as_slice()
    );

    compile_and_run(source);
}

#[test]
fn named_empty_interface_reuses_canonical_metadata_zero() {
    let module = compile_source(
        r#"
package main

type Empty interface{}

var Value Empty

func main() {}
"#,
    );

    let empty = module
        .named_type_metas
        .iter()
        .find(|metadata| metadata.name == "main.Empty")
        .expect("named empty interface metadata");
    assert_eq!(empty.underlying_meta.value_kind(), ValueKind::Interface);
    assert_eq!(empty.underlying_meta.meta_id(), 0);
    assert_eq!(
        module
            .interface_metas
            .iter()
            .filter(|metadata| metadata.methods.is_empty())
            .count(),
        1,
        "metadata id 0 must be the only zero-method interface descriptor"
    );
}

#[test]
fn anonymous_struct_tags_remain_part_of_runtime_type_identity() {
    let module = compile_source(
        r#"
package main

var Json struct {
    Value int `json:"value"`
}

var Database struct {
    Value int `db:"value"`
}

func main() {}
"#,
    );

    let tagged_structs: Vec<_> = module
        .runtime_types
        .iter()
        .enumerate()
        .filter_map(|(rttid, runtime_type)| match runtime_type {
            RuntimeType::Struct { fields, meta_id }
                if fields.len() == 1 && fields[0].name == "Value" && !fields[0].tag.is_empty() =>
            {
                Some((rttid as u32, *meta_id, fields[0].tag.as_str()))
            }
            _ => None,
        })
        .collect();

    assert_eq!(
        tagged_structs.len(),
        2,
        "both tagged types must be interned"
    );
    assert_ne!(tagged_structs[0].0, tagged_structs[1].0);
    assert_ne!(tagged_structs[0].1, tagged_structs[1].1);
    let mut tags: Vec<_> = tagged_structs.iter().map(|entry| entry.2).collect();
    tags.sort_unstable();
    assert_eq!(tags, vec!["db:\"value\"", "json:\"value\""]);
    for (_, meta_id, tag) in tagged_structs {
        assert_eq!(
            module.struct_metas[meta_id as usize].fields[0]
                .tag
                .as_deref(),
            Some(tag),
            "runtime identity and field metadata must preserve the same tag"
        );
    }
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
fn closure_literal_named_param_transfers_use_declared_param_objects() {
    let source = r#"
package main

func loadTexture(path string) int;
func loadTextureBytes(data []byte) int;

type Loader struct {
    LoadFile func(path string) int
    LoadBytes func(data []byte) int
    Free func(id int)
}

type Loaders struct {
    Texture Loader
    TextureLinear Loader
}

func defaultLoaders() Loaders {
    return Loaders{
        Texture: Loader{
            LoadFile: func(path string) int { return loadTexture(path) },
            LoadBytes: func(data []byte) int { return loadTextureBytes(data) },
            Free: func(id int) {},
        },
        TextureLinear: Loader{
            LoadFile: func(path string) int { return loadTexture(path) },
            LoadBytes: func(data []byte) int { return loadTextureBytes(data) },
            Free: func(id int) {},
        },
    }
}

func main() {
    _ = defaultLoaders()
}
"#;

    let module = compile_source(source);
    let load_file_extern = module
        .externs
        .iter()
        .position(|extern_def| extern_def.name.contains("loadTexture"))
        .expect("loadTexture extern") as u16;
    let load_bytes_extern = module
        .externs
        .iter()
        .position(|extern_def| extern_def.name.contains("loadTextureBytes"))
        .expect("loadTextureBytes extern") as u16;

    let closure_param_kind_for_extern = |extern_id: u16| {
        module
            .functions
            .iter()
            .filter(|f| f.name.starts_with("closure_"))
            .find(|f| {
                f.code
                    .iter()
                    .any(|inst| inst.opcode() == Opcode::CallExtern && inst.b == extern_id)
            })
            .and_then(|f| f.param_types.first())
            .map(|transfer| ValueRttid::from_raw(transfer.rttid_raw).value_kind())
    };

    assert_eq!(
        closure_param_kind_for_extern(load_file_extern),
        Some(ValueKind::String),
        "LoadFile closure should record a string transfer parameter"
    );
    assert_eq!(
        closure_param_kind_for_extern(load_bytes_extern),
        Some(ValueKind::Slice),
        "LoadBytes closure should record a slice transfer parameter"
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
fn test_iface_wrapper_records_forwarded_param_transfer_types() {
    let source = r#"
package main

type Reader interface {
    Read(p []byte, n int) []byte
}

type Box struct{}

func (b Box) Read(p []byte, n int) []byte {
    return p
}

func main() {
    var r Reader = Box{}
    _ = r
}
"#;

    let module = compile_source(source);
    let wrapper = module
        .functions
        .iter()
        .find(|f| f.name == "Read$iface")
        .expect("interface dispatch wrapper");

    assert_eq!(
        wrapper.param_types.len(),
        2,
        "$iface wrapper transfer params should include forwarded params and exclude receiver"
    );
    assert_eq!(wrapper.param_types[0].slots, 1);
    assert_eq!(wrapper.param_types[1].slots, 1);
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
        .filter(|f| f.name.starts_with("Read$defer_iface_0_"))
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
fn defer_iface_wrapper_cache_key_includes_receiver_interface_meta_061() {
    let source = r#"
package main

type FileLike interface {
    Close() error
    Stat() int
}

type ConnLike interface {
    Close() error
    ZAddr() int
}

type closer struct {}

func (closer) Close() error {
    return nil
}

func (closer) Stat() int {
    return 0
}

func (closer) ZAddr() int {
    return 0
}

func main() {
    var file FileLike = closer{}
    var conn ConnLike = closer{}
    defer file.Close()
    defer conn.Close()
}
"#;

    let module = compile_source(source);
    let file_iface_meta_id = module
        .interface_metas
        .iter()
        .position(|meta| meta.name.ends_with("FileLike"))
        .expect("FileLike interface meta should be registered") as u32;
    let conn_iface_meta_id = module
        .interface_metas
        .iter()
        .position(|meta| meta.name.ends_with("ConnLike"))
        .expect("ConnLike interface meta should be registered") as u32;

    let wrapper_iface_metas: Vec<_> = module
        .functions
        .iter()
        .filter(|f| f.name.starts_with("Close$defer_iface_0_"))
        .map(|wrapper| {
            wrapper
                .jit_metadata
                .iter()
                .find_map(|metadata| match metadata {
                    JitInstructionMetadata::CallIfaceLayout { iface_meta_id, .. } => {
                        Some(*iface_meta_id)
                    }
                    _ => None,
                })
                .unwrap_or_else(|| panic!("{} must carry CallIfaceLayout metadata", wrapper.name))
        })
        .collect();

    assert_eq!(
        wrapper_iface_metas.len(),
        2,
        "deferred interface Close wrappers with distinct receiver interfaces must not share a cache entry"
    );
    assert!(
        wrapper_iface_metas.contains(&file_iface_meta_id),
        "FileLike defer wrapper metadata missing: {wrapper_iface_metas:?}"
    );
    assert!(
        wrapper_iface_metas.contains(&conn_iface_meta_id),
        "ConnLike defer wrapper metadata missing: {wrapper_iface_metas:?}"
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

#[test]
fn stack_array_copy_slot_accesses_are_index_checked_047() {
    let source = r#"
package main

func main() int {
    left := [3]int{1, 2, 3}
    right := left
    return right[1]
}
"#;

    let module = compile_source(source);

    verify_module(&module).expect(
        "stack array copy lowering must emit verifier-visible IndexCheck facts before SlotGet/SlotSet",
    );
}

#[test]
fn map_new_struct_key_uses_canonical_key_metadata_047() {
    let source = r#"
package main

type PackagePoint struct {
    x int
}

func main() {
    seed()
    _ = map[string]int{"a": 1}
    _ = map[int]string{42: "answer"}
    _ = map[bool]string{true: "yes", false: "no"}
    type Point struct {
        x int
        y int
    }
    m := make(map[Point]string)
    m[Point{1, 2}] = "p1"

    {
        type Point struct {
            label string
        }
        nested := make(map[Point]int)
        nested[Point{"nested"}] = 1
    }
}

func seed() {
    type Point struct {
        x int
        y int
    }
    m := make(map[string]Point)
    m["a"] = Point{1, 2}
}
"#;

    let module = compile_source(source);

    verify_module(&module)
        .expect("MapNew must pack canonical key ValueMeta for its key ValueRttid");
    assert!(module
        .named_type_metas
        .iter()
        .any(|named| named.name == "main.PackagePoint"));

    let local_identities: std::collections::BTreeSet<_> = module
        .named_type_metas
        .iter()
        .map(|named| named.name.as_str())
        .filter(|name| name.ends_with(".Point"))
        .collect();
    assert_eq!(local_identities.len(), 3);
    assert!(local_identities
        .iter()
        .all(|name| name.contains(vo_common_core::LOCAL_TYPE_IDENTITY_MARKER)));

    let encoded = module.serialize().expect("serialize local type identities");
    let decoded =
        vo_vm::bytecode::Module::deserialize(&encoded).expect("deserialize local type identities");
    verify_module(&decoded).expect("round-tripped local type identities must verify");
    let names = |module: &vo_vm::bytecode::Module| {
        module
            .named_type_metas
            .iter()
            .map(|named| named.name.clone())
            .collect::<Vec<_>>()
    };
    assert_eq!(names(&module), names(&decoded));
}

#[test]
fn local_type_identity_is_stable_when_unrelated_source_files_precede_it() {
    fn compile_files(include_unrelated: bool) -> vo_vm::bytecode::Module {
        let mut files = FileSet::new(PathBuf::from("virtual-project"));
        files.files.insert(
            PathBuf::from("z.vo"),
            concat!(
                "package main\n",
                "func main() {\n",
                "    type Stable struct { value int }\n",
                "    _ = Stable{value: 1}\n",
                "}\n",
            )
            .to_string(),
        );
        if include_unrelated {
            // This file sorts before z.vo and changes every later global Span
            // base. A file-local identity must remain unchanged.
            files.files.insert(
                PathBuf::from("a.vo"),
                "package main\nfunc unrelated() string { return \"padding\" }\n".to_string(),
            );
        }
        let resolver = PackageResolver {
            std: StdSource::with_fs(
                MemoryFs::new().with_file("errors/errors.vo", "package errors\n"),
            ),
            r#mod: ModSource::with_fs(MemoryFs::new()),
        };
        let project = analyze_project_with_identity(
            files,
            &resolver,
            PackageIdentity::new("local/identity-stability").unwrap(),
        )
        .expect("identity stability project analysis");
        compile_project(&project).expect("identity stability project codegen")
    }

    let identity = |module: &vo_vm::bytecode::Module| {
        module
            .named_type_metas
            .iter()
            .find(|named| named.name.ends_with(".Stable"))
            .expect("local Stable metadata")
            .name
            .clone()
    };
    let alone = compile_files(false);
    let preceded = compile_files(true);
    assert_eq!(identity(&alone), identity(&preceded));
    assert!(identity(&alone).contains("@local:7a2e766f:"));
}

#[test]
fn promoted_method_codegen_is_byte_deterministic_across_projects() {
    let source = r#"
package main

type LeftLeaf struct{}
func (LeftLeaf) Left() int { return 1 }
type LeftOuter struct { LeftLeaf }

type RightLeaf struct{}
func (RightLeaf) Right() int { return 2 }
type RightOuter struct { RightLeaf }

func main() int {
    var left LeftOuter
    var right RightOuter
    return left.Left() + right.Right()
}
"#;

    let expected = compile_source(source)
        .serialize()
        .expect("serialize first deterministic module");
    for _ in 0..8 {
        let actual = compile_source(source)
            .serialize()
            .expect("serialize repeated deterministic module");
        assert_eq!(actual, expected);
    }
}

#[test]
fn repeated_blank_struct_fields_remain_unselectable_after_roundtrip() {
    let source = r#"
package main

type Padded struct {
    _ int
    _ string
    Visible int
}

func main() {
    _ = Padded{Visible: 1}
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("blank field module must verify");
    let padded = module
        .struct_metas
        .iter()
        .find(|meta| meta.fields.iter().filter(|field| field.name == "_").count() == 2)
        .expect("Padded struct metadata");
    assert!(padded.get_field("_").is_none());
    assert!(!padded.field_index.contains_key("_"));
    assert!(padded.get_field("Visible").is_some());

    let encoded = module.serialize().expect("serialize blank field module");
    let decoded =
        vo_vm::bytecode::Module::deserialize(&encoded).expect("deserialize blank field module");
    verify_module(&decoded).expect("round-tripped blank field module must verify");
    let padded = decoded
        .struct_metas
        .iter()
        .find(|meta| meta.fields.iter().filter(|field| field.name == "_").count() == 2)
        .expect("round-tripped Padded struct metadata");
    assert!(padded.get_field("_").is_none());
    assert!(!padded.field_index.contains_key("_"));
}

#[test]
fn function_literal_local_named_types_are_registered_before_codegen() {
    let source = r#"
package main

func main() {
    worker := func() {
        type Local struct { Value int }
        var boxed any = Local{Value: 7}
        value, err := boxed~>Value
        if err != nil { panic(err) }
        if value.(int) != 7 { panic("wrong local named type metadata") }
    }
    worker()
}
"#;

    compile_and_run(source);
}

#[test]
fn parenthesized_dynamic_assignments_keep_wrapper_type_info() {
    let source = r#"
package main

type Item struct { Value int }

func unwrapOutside(box any) (any, error) {
    value := (box~>Value)?
    return value, nil
}

func unwrapInside(box any) (any, error) {
    value := ((box~>Value?))
    return value, nil
}

func main() {
    var box any = Item{Value: 42}
    value, err := (((box~>Value)))
    if err != nil || value.(int) != 42 { panic("direct") }
    outside, err := unwrapOutside(box)
    if err != nil || outside.(int) != 42 { panic("outside") }
    inside, err := unwrapInside(box)
    if err != nil || inside.(int) != 42 { panic("inside") }
}
"#;

    compile_and_run(source);
}

#[test]
fn dynamic_assignment_statements_are_rejected_061() {
    let source = r#"
package main

func main() {
    m := map[int]string{}
    var box any = m
    box~>[1] = "one"

    s := struct{ name string }{}
    var obj any = &s
    obj~>name = "ok"
}
"#;

    let error = match analyze_source(source) {
        Ok(_) => panic!("dynamic assignment statements must be rejected"),
        Err(error) => error,
    };
    let messages: Vec<_> = error
        .diagnostics()
        .expect("checker diagnostics")
        .iter()
        .map(|diagnostic| diagnostic.message.as_str())
        .collect();

    assert!(messages.iter().any(|message| {
        message.contains("use dyn.SetIndex")
            && message.contains("handle its returned error explicitly")
    }));
    assert!(messages.iter().any(|message| {
        message.contains("use dyn.SetAttr")
            && message.contains("handle its returned error explicitly")
    }));
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

#[test]
fn extern_function_values_lower_to_signature_preserving_trampolines() {
    let source = r#"
package main

func hostUnary(x int) (int, error)
func hostVariadic(prefix string, values ...any) (string, error)

type holder struct {
    unary func(int) (int, error)
}

var globalUnary = hostUnary

func passUnary(f func(int) (int, error)) func(int) (int, error) {
    return f
}

func returnUnary() func(int) (int, error) {
    return hostUnary
}

func scheduleUnary(f func(int) (int, error)) (err error) {
    defer f(1)
    errdefer f(2)
    go f(3)
    return nil
}

func scheduleUnaryOnIsland() {
    worker := make(island)
    go @(worker) hostUnary(4)
}

func main() {
    local := hostUnary
    _, _ = local(1)
    _, _ = passUnary(hostUnary)(2)
    _, _ = returnUnary()(3)
    _, _ = globalUnary(4)

    h := holder{unary: hostUnary}
    _, _ = h.unary(5)
    list := []func(int) (int, error){hostUnary}
    _, _ = list[0](6)
    table := map[string]func(int) (int, error){"unary": hostUnary}
    _, _ = table["unary"](7)

    variadic := hostVariadic
    _, _ = variadic("packed", 1, "two")
    values := []any{3, "four"}
    _, _ = variadic("spread", values...)
    _ = scheduleUnary
    _ = scheduleUnaryOnIsland
}
"#;

    let module = compile_source(source);
    verify_module(&module).expect("extern function-value trampolines must verify");
    assert_transfer_metadata_canonical(&module);

    let unary = module
        .functions
        .iter()
        .find(|func| func.name.starts_with("$extern_value_") && func.name.contains("hostUnary"))
        .expect("hostUnary function value must have a trampoline");
    assert_eq!(unary.param_slots, 1);
    assert_eq!(unary.param_types.len(), 1);
    assert_eq!(unary.ret_slots, 3, "int + error uses three slots");
    assert_eq!(unary.error_ret_slot, 1);
    assert_eq!(
        unary.ret_slot_types,
        vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1]
    );
    assert!(
        unary
            .code
            .iter()
            .any(|inst| inst.opcode() == Opcode::CallExtern),
        "trampoline must forward through CallExtern"
    );

    let variadic = module
        .functions
        .iter()
        .find(|func| func.name.starts_with("$extern_value_") && func.name.contains("hostVariadic"))
        .expect("hostVariadic function value must have a trampoline");
    assert_eq!(
        variadic.param_slots, 2,
        "variadic calls pass prefix and packed slice"
    );
    assert_eq!(variadic.param_types.len(), 2);
    assert_eq!(
        &variadic.slot_types[..2],
        &[SlotType::GcRef, SlotType::GcRef],
        "string and variadic slice parameters retain precise GC layouts"
    );
    assert_eq!(variadic.ret_slots, 3, "string + error uses three slots");
    assert_eq!(variadic.error_ret_slot, 1);
}

#[test]
fn wide_dynamic_calls_emit_zero_mirrors_and_complete_layout_metadata() {
    let source = include_str!(
        "../../../../tests/lang/cases/typechecker/2026_02_18_dynamic_call_slot_count_limit.vo"
    );
    let module = compile_source(source);
    verify_module(&module).expect("wide dynamic call module must verify");

    assert!(module.functions.iter().any(|function| {
        function
            .code
            .iter()
            .zip(&function.jit_metadata)
            .any(|(inst, metadata)| {
                inst.opcode() == Opcode::CallClosure
                    && inst.c == 0
                    && matches!(
                        metadata,
                        JitInstructionMetadata::CallLayout {
                            arg_layout,
                            ret_layout,
                        } if arg_layout.len() == 256 && ret_layout.len() == 256
                    )
            })
    }));
}

#[test]
fn call_iface_emits_full_width_method_index_metadata() {
    let source = include_str!(
        "../../../../tests/lang/cases/typechecker/2026_02_18_call_iface_method_index_u8_limit.vo"
    );
    let module = compile_source(source);
    verify_module(&module).expect("wide CallIface method index module must verify");

    assert!(module.functions.iter().any(|function| {
        function
            .code
            .iter()
            .zip(&function.jit_metadata)
            .any(|(inst, metadata)| {
                inst.opcode() == Opcode::CallIface
                    && inst.flags == 0
                    && matches!(
                        metadata,
                        JitInstructionMetadata::CallIfaceLayout {
                            method_idx: 256,
                            ..
                        }
                    )
            })
    }));
}

#[test]
fn binary_expression_resource_boundary_preserves_left_to_right_evaluation() {
    // Exercising the complete boundary proves that analysis and codegen remain
    // stack-safe for every expression accepted by the parser.
    const OPERATORS: usize = vo_syntax::parser::MAX_BINARY_EXPRESSION_PATH;
    let calls = (0..=OPERATORS)
        .map(|index| format!("step({index})"))
        .collect::<Vec<_>>()
        .join(" + ");
    let expected_sum = OPERATORS * (OPERATORS + 1) / 2;
    let source = format!(
        r#"package main

var next int

func step(expected int) int {{
    if next != expected {{
        panic("binary operands evaluated out of order")
    }}
    next++
    return expected
}}

func main() {{
    sum := {calls}
    if next != {operand_count} || sum != {expected_sum} {{
        panic("binary chain result mismatch")
    }}
}}
"#,
        operand_count = OPERATORS + 1,
    );

    let project = analyze_source(&source).expect("boundary expression analysis failed");
    let module = compile_project(&project).expect("boundary expression codegen failed");
    let main = module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("boundary module must contain main");
    assert!(
        main.local_slots < 128,
        "iterative binary folding retained dead prefix temporaries: {} slots",
        main.local_slots
    );
    let mut vm = Vm::new();
    vm.load(module).expect("boundary module load failed");
    vm.run().expect("boundary VM execution failed");
}

#[test]
fn iterative_binary_codegen_reuses_dead_fold_temporaries_across_scales() {
    let mut measurements = Vec::new();
    for operators in [64usize, 128, 256, 512] {
        let expression = vec!["one"; operators + 1].join(" + ");
        let source = format!(
            "package main\nvar one int = 1\nfunc sum() int {{ return {expression} }}\nfunc main() {{}}\n"
        );
        let module = compile_source(&source);
        let sum = module
            .functions
            .iter()
            .find(|function| function.name == "sum")
            .expect("scale module must contain sum");
        measurements.push((operators, usize::from(sum.local_slots), sum.code.len()));
    }

    for &(operators, slots, _) in &measurements {
        assert!(
            slots < 32,
            "{operators}-operator fold retained dead temporaries in {slots} slots"
        );
    }
    for pair in measurements.windows(2) {
        let (_, previous_slots, previous_code) = pair[0];
        let (operators, slots, code) = pair[1];
        assert!(
            slots <= previous_slots + 2,
            "{operators}-operator fold grew frame width from {previous_slots} to {slots}"
        );
        assert!(
            code <= previous_code.saturating_mul(2).saturating_add(8),
            "{operators}-operator fold grew bytecode superlinearly: {previous_code} -> {code}"
        );
    }
}

#[test]
fn binary_expression_resource_boundary_preserves_short_circuiting() {
    const OPERATORS: usize = vo_syntax::parser::MAX_BINARY_EXPRESSION_PATH;
    let all_true = (0..=OPERATORS)
        .map(|index| format!("step({index}, -1)"))
        .collect::<Vec<_>>()
        .join(" && ");
    let stop = 263;
    let stops_early = (0..=OPERATORS)
        .map(|index| format!("step({index}, {stop})"))
        .collect::<Vec<_>>()
        .join(" && ");
    let all_false = (0..=OPERATORS)
        .map(|index| format!("orStep({index}, -1)"))
        .collect::<Vec<_>>()
        .join(" || ");
    let stops_on_true = (0..=OPERATORS)
        .map(|index| format!("orStep({index}, {stop})"))
        .collect::<Vec<_>>()
        .join(" || ");
    let source = format!(
        r#"package main

var next int

func step(expected int, stop int) bool {{
    if next != expected {{
        panic("short-circuit operands evaluated out of order")
    }}
    next++
    return expected != stop
}}

func orStep(expected int, stop int) bool {{
    if next != expected {{
        panic("or operands evaluated out of order")
    }}
    next++
    return expected == stop
}}

func main() {{
    next = 0
    all := {all_true}
    if !all || next != {operand_count} {{
        panic("true short-circuit chain did not evaluate every operand")
    }}

    next = 0
    stopped := {stops_early}
    if stopped || next != {evaluated_before_stop} {{
        panic("false short-circuit chain evaluated a skipped operand")
    }}

    next = 0
    any := {all_false}
    if any || next != {operand_count} {{
        panic("false or-chain did not evaluate every operand")
    }}

    next = 0
    found := {stops_on_true}
    if !found || next != {evaluated_before_stop} {{
        panic("true or-chain evaluated a skipped operand")
    }}
}}
"#,
        operand_count = OPERATORS + 1,
        evaluated_before_stop = stop + 1,
    );

    compile_and_run(&source);
}

#[test]
fn iterative_binary_codegen_truncates_narrow_integer_intermediates() {
    compile_and_run(
        r#"
package main

func main() {
    var value int8 = 100
    result := value * 2 / 2
    if result != -28 {
        panic("int8 intermediate was not truncated before division")
    }
}
"#,
    );
}

#[test]
fn binary_expression_resource_boundary_preserves_float_left_fold_order() {
    const OPERATORS: usize = vo_syntax::parser::MAX_BINARY_EXPRESSION_PATH;
    let expression = std::iter::once("1.0".to_string())
        .chain((0..OPERATORS).map(|index| format!("divisor({index})")))
        .collect::<Vec<_>>()
        .join(" / ");
    let source = format!(
        r#"package main

var next int

func divisor(expected int) float64 {{
    if next != expected {{
        panic("float operands evaluated out of order")
    }}
    next++
    return 2.0
}}

func main() {{
    actual := {expression}
    expected := 1.0
    for i := 0; i < {operators}; i++ {{
        expected = expected / 2.0
    }}
    if next != {operators} || actual != expected {{
        panic("float binary chain was not evaluated as a left fold")
    }}
}}
"#,
        operators = OPERATORS,
    );

    compile_and_run(&source);
}

#[test]
fn iterative_binary_codegen_preserves_nested_rhs_evaluation_order() {
    const PAIRS: usize = 128;
    let mut next_operand = 1;
    let mut terms = vec!["mark(0)".to_string()];
    for _ in 0..PAIRS {
        terms.push(format!(
            "(mark({}) + mark({}))",
            next_operand,
            next_operand + 1
        ));
        next_operand += 2;
    }
    let expression = terms.join(" + ");
    let expected_sum = next_operand * (next_operand - 1) / 2;
    let source = format!(
        r#"package main

var next int

func mark(expected int) int {{
    if next != expected {{
        panic("nested RHS evaluated out of order")
    }}
    next++
    return expected
}}

func main() {{
    actual := {expression}
    if next != {operand_count} || actual != {expected_sum} {{
        panic("nested RHS result mismatch")
    }}
}}
"#,
        operand_count = next_operand,
    );

    compile_and_run(&source);
}
