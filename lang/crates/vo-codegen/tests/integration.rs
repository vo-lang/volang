//! Integration tests: parse → check → codegen → VM

use std::collections::BTreeMap;
use std::path::PathBuf;
use vo_analysis::importer::NullImporter;
use vo_analysis::{AnalysisError, Checker, Project};
use vo_codegen::compile_project;
use vo_common::SourceMap;
use vo_syntax::parser;
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
        .check_with_importer(&[file.clone()], &mut importer)
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
    vm.load(module);
    vm.run().expect("VM execution failed");

    println!("✓ VM execution completed");
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
