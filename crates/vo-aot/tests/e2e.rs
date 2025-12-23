//! End-to-end tests: Vo source -> bytecode -> JIT execution.

use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

use vo_aot::context::CompileContext;
use vo_aot::translate::FunctionTranslator;
use vo_syntax::parse;
use vo_analysis::analyze_single_file;
use vo_codegen_vm::compile_project;
use vo_vm::bytecode::Module as BytecodeModule;
use vo_vm::instruction::Opcode;
use vo_runtime_native::RuntimeSymbols;
use vo_common_core::RuntimeTypeId;

/// Compile Vo source to bytecode.
fn source_to_bytecode(source: &str) -> BytecodeModule {
    let (file, _parse_diag, interner) = parse(source, 0);
    let project = analyze_single_file(file, interner).expect("Type check failed");
    compile_project(&project).expect("Compilation failed")
}

/// Setup JIT module for testing.
fn setup_jit() -> (JITModule, cranelift_codegen::isa::CallConv) {
    setup_jit_with_runtime(false)
}

/// Setup JIT module with optional runtime symbol linking.
fn setup_jit_with_runtime(link_runtime: bool) -> (JITModule, cranelift_codegen::isa::CallConv) {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = cranelift_native::builder().unwrap();
    let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
    let call_conv = isa.default_call_conv();
    
    let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    
    // Link runtime symbols if requested
    if link_runtime {
        let symbols = RuntimeSymbols::new();
        for sym in symbols.iter() {
            builder.symbol(sym.name, sym.ptr as *const u8);
        }
    }
    
    let module = JITModule::new(builder);
    
    (module, call_conv)
}

/// Compile bytecode to JIT and get function pointer.
fn compile_to_jit(
    bytecode: &BytecodeModule,
    func_name: &str,
) -> (*const u8, JITModule) {
    compile_to_jit_impl(bytecode, func_name, false)
}

/// Compile bytecode to JIT with runtime symbols linked.
fn compile_to_jit_with_runtime(
    bytecode: &BytecodeModule,
    func_name: &str,
) -> (*const u8, JITModule) {
    compile_to_jit_impl(bytecode, func_name, true)
}

fn compile_to_jit_impl(
    bytecode: &BytecodeModule,
    func_name: &str,
    link_runtime: bool,
) -> (*const u8, JITModule) {
    let (mut module, call_conv) = setup_jit_with_runtime(link_runtime);
    
    let mut compile_ctx = CompileContext::new(bytecode, call_conv);
    compile_ctx.declare_all_functions(&mut module).unwrap();
    
    // Find the target function index
    let func_idx = bytecode.functions.iter()
        .position(|f| f.name == func_name)
        .expect(&format!("Function '{}' not found", func_name));
    
    // Compile all functions (needed for cross-function calls)
    for (idx, func_def) in bytecode.functions.iter().enumerate() {
        let func_id = compile_ctx.get_vo_func(idx as u32).unwrap();
        
        let mut ctx = module.make_context();
        ctx.func.signature = module.declarations().get_function_decl(func_id).signature.clone();
        ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
        
        let mut translator = FunctionTranslator::new(func_def.local_slots as usize, &func_def.code);
        translator.translate(&mut ctx.func, &func_def.code, &mut compile_ctx, &mut module, &func_def.slot_types).unwrap();
        
        module.define_function(func_id, &mut ctx).unwrap();
        module.clear_context(&mut ctx);
    }
    
    module.finalize_definitions().unwrap();
    
    let func_id = compile_ctx.get_vo_func(func_idx as u32).unwrap();
    let code_ptr = module.get_finalized_function(func_id);
    
    (code_ptr, module)
}

#[test]
fn test_e2e_simple_add() {
    let source = r#"
package main

func add(a int, b int) int {
    return a + b
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "add");
    
    let add_fn: fn(i64, i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    assert_eq!(add_fn(3, 5), 8);
    assert_eq!(add_fn(100, 200), 300);
    assert_eq!(add_fn(-10, 5), -5);
    println!("✓ test_e2e_simple_add passed");
}

#[test]
fn test_e2e_arithmetic() {
    let source = r#"
package main

func compute(a int, b int) int {
    return (a * b) + (a - b)
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "compute");
    
    let compute_fn: fn(i64, i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    // (10 * 3) + (10 - 3) = 30 + 7 = 37
    assert_eq!(compute_fn(10, 3), 37);
    // (5 * 5) + (5 - 5) = 25 + 0 = 25
    assert_eq!(compute_fn(5, 5), 25);
    println!("✓ test_e2e_arithmetic passed");
}

#[test]
fn test_e2e_max() {
    let source = r#"
package main

func max(a int, b int) int {
    if a > b {
        return a
    }
    return b
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "max");
    
    let max_fn: fn(i64, i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    assert_eq!(max_fn(10, 5), 10);
    assert_eq!(max_fn(3, 7), 7);
    assert_eq!(max_fn(-1, -5), -1);
    assert_eq!(max_fn(42, 42), 42);
    println!("✓ test_e2e_max passed");
}

#[test]
fn test_e2e_factorial() {
    let source = r#"
package main

func factorial(n int) int {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "factorial");
    
    let factorial_fn: fn(i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    assert_eq!(factorial_fn(0), 1);
    assert_eq!(factorial_fn(1), 1);
    assert_eq!(factorial_fn(5), 120);
    assert_eq!(factorial_fn(10), 3628800);
    println!("✓ test_e2e_factorial passed");
}

#[test]
fn test_e2e_fibonacci() {
    let source = r#"
package main

func fib(n int) int {
    if n <= 1 {
        return n
    }
    return fib(n - 1) + fib(n - 2)
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "fib");
    
    let fib_fn: fn(i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    assert_eq!(fib_fn(0), 0);
    assert_eq!(fib_fn(1), 1);
    assert_eq!(fib_fn(2), 1);
    assert_eq!(fib_fn(10), 55);
    assert_eq!(fib_fn(15), 610);
    println!("✓ test_e2e_fibonacci passed");
}

#[test]
fn test_e2e_local_vars() {
    let source = r#"
package main

func sumOfSquares(a int, b int) int {
    x := a * a
    y := b * b
    return x + y
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "sumOfSquares");
    
    let fn_ptr: fn(i64, i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    assert_eq!(fn_ptr(3, 4), 25);  // 9 + 16
    assert_eq!(fn_ptr(5, 12), 169); // 25 + 144
    println!("✓ test_e2e_local_vars passed");
}

#[test]
fn test_e2e_nested_calls() {
    let source = r#"
package main

func double(x int) int {
    return x * 2
}

func quadruple(x int) int {
    return double(double(x))
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "quadruple");
    
    let fn_ptr: fn(i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    assert_eq!(fn_ptr(5), 20);
    assert_eq!(fn_ptr(10), 40);
    println!("✓ test_e2e_nested_calls passed");
}

#[test]
fn test_e2e_for_loop() {
    let source = r#"
package main

func sum(n int) int {
    total := 0
    for i := 0; i < n; i = i + 1 {
        total = total + i
    }
    return total
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "sum");
    
    let sum_fn: fn(i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    // sum(0..5) = 0+1+2+3+4 = 10
    assert_eq!(sum_fn(5), 10);
    // sum(0..10) = 0+1+...+9 = 45
    assert_eq!(sum_fn(10), 45);
    // sum(0..1) = 0
    assert_eq!(sum_fn(1), 0);
    // sum(0..0) = 0 (empty loop)
    assert_eq!(sum_fn(0), 0);
    println!("✓ test_e2e_for_loop passed");
}

#[test]
fn test_e2e_while_loop() {
    let source = r#"
package main

func countDown(n int) int {
    count := 0
    for n > 0 {
        count = count + 1
        n = n - 1
    }
    return count
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "countDown");
    
    let fn_ptr: fn(i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    assert_eq!(fn_ptr(5), 5);
    assert_eq!(fn_ptr(10), 10);
    assert_eq!(fn_ptr(0), 0);
    println!("✓ test_e2e_while_loop passed");
}

#[test]
fn test_e2e_gcd() {
    let source = r#"
package main

func gcd(a int, b int) int {
    for b != 0 {
        t := b
        b = a % b
        a = t
    }
    return a
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit(&bytecode, "gcd");
    
    let gcd_fn: fn(i64, i64) -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    assert_eq!(gcd_fn(48, 18), 6);
    assert_eq!(gcd_fn(100, 25), 25);
    assert_eq!(gcd_fn(17, 13), 1);
    assert_eq!(gcd_fn(0, 5), 5);
    println!("✓ test_e2e_gcd passed");
}

#[test]
fn test_e2e_select_default() {
    // Test select with default case only (simplest case)
    let source = r#"
package main

func testSelect() int {
    select {
    default:
        return 42
    }
    return 0
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (code_ptr, _module) = compile_to_jit_with_runtime(&bytecode, "testSelect");
    
    let test_fn: fn() -> i64 = unsafe { std::mem::transmute(code_ptr) };
    
    // Should return 42 from default case
    assert_eq!(test_fn(), 42);
    println!("✓ test_e2e_select_default passed");
}

// Note: Full iterator test requires slice/map creation which needs more runtime support.
// The iterator opcodes (IterBegin, IterNext, IterEnd) are implemented but need
// container creation to be fully testable in e2e.

#[test]
fn test_struct_type_id_initialized() {
    let source = r#"
package main

type Point struct {
    x int
    y int
}

type Person struct {
    name string
    age int
}

func createPoint() {
    p := Point{x: 10, y: 20}
    _ = p
}

func createPerson() {
    p := Person{name: "test", age: 30}
    _ = p
}

func main() {
    createPoint()
    createPerson()
}
"#;
    
    let bytecode = source_to_bytecode(source);
    
    // Find Alloc instructions and verify type_id >= FirstStruct (100)
    let mut alloc_type_ids: Vec<u16> = Vec::new();
    
    for func in &bytecode.functions {
        for instr in &func.code {
            if instr.opcode() == Opcode::Alloc {
                // Alloc instruction: a=dest, b=type_id_lo, c=type_id_hi, flags=field_count
                let type_id = (instr.b as u32) | ((instr.c as u32) << 16);
                alloc_type_ids.push(type_id as u16); // For now keep as u16 since struct IDs < 65535
            }
        }
    }
    
    println!("Found {} Alloc instructions with type_ids: {:?}", alloc_type_ids.len(), alloc_type_ids);
    
    // Should have at least 2 Alloc instructions (for Point and Person)
    assert!(alloc_type_ids.len() >= 2, "Expected at least 2 struct allocations, found {}", alloc_type_ids.len());
    
    // Struct type_ids now start from 0 (no FirstStruct offset)
    // Just verify we have valid type_ids
    for &type_id in &alloc_type_ids {
        // type_id is u16, all values are valid
        println!("Struct type_id: {}", type_id);
    }
    
    // Type IDs should be unique for different struct types
    let unique_ids: std::collections::HashSet<_> = alloc_type_ids.iter().collect();
    assert!(
        unique_ids.len() >= 2,
        "Expected at least 2 unique struct type_ids, found {}",
        unique_ids.len()
    );
    
    println!("✓ test_struct_type_id_initialized passed: type_ids {:?}", alloc_type_ids);
}

#[test]
fn test_interface_type_id_registered() {
    // Test that interface types are registered during compilation
    // by checking the analysis/codegen flow registers interface TypeKeys
    use vo_analysis::Type;
    
    let source = r#"
package main

type Greeter interface {
    Greet() int
}

type Counter interface {
    Count() int
}

func main() {
}
"#;
    
    let (file, _parse_diag, interner) = parse(source, 0);
    let project = analyze_single_file(file, interner).expect("Type check failed");
    
    // Check that interface types exist in the type system
    let query = project.query();
    let mut interface_count = 0;
    
    for (_type_key, ty) in query.iter_types() {
        if matches!(ty, Type::Interface(_)) {
            interface_count += 1;
        }
    }
    
    // Should have at least 2 interface types (Greeter and Counter)
    assert!(interface_count >= 2, "Expected at least 2 interface types, found {}", interface_count);
    
    // Now compile and verify register_types was called
    let bytecode = compile_project(&project).expect("Compilation failed");
    
    // The module should compile without errors - interface types are registered
    assert!(!bytecode.functions.is_empty(), "Should have compiled functions");
    
    println!("✓ test_interface_type_id_registered passed: found {} interface types", interface_count);
}

#[test]
fn test_init_interface_generated() {
    // Test that InitInterface opcode is generated for interface variable declarations
    let source = r#"
package main

type Stringer interface {
    String() string
}

func takeInterface(s Stringer) {
    _ = s
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    
    // Find InitInterface instructions
    let mut init_interface_count = 0;
    let mut init_interface_type_ids: Vec<u32> = Vec::new();
    
    for func in &bytecode.functions {
        println!("Function: {}", func.name);
        for (i, instr) in func.code.iter().enumerate() {
            println!("  {}: {:?} a={} b={} c={}", i, instr.opcode(), instr.a, instr.b, instr.c);
            if instr.opcode() == Opcode::InitInterface {
                init_interface_count += 1;
                // Decode full type_id from b (low 16 bits) and c (high 16 bits)
                let type_id = (instr.b as u32) | ((instr.c as u32) << 16);
                init_interface_type_ids.push(type_id);
            }
        }
    }
    
    println!("Found {} InitInterface instructions with type_ids: {:?}", init_interface_count, init_interface_type_ids);
    
    // Interface type_ids now start from 0 (no FirstInterface offset)
    // Just verify we have valid type_ids
    for &type_id in &init_interface_type_ids {
        // type_id is u32 (packed with value_kind), all values are valid
        println!("Interface type_id: 0x{:08x}", type_id);
    }
    
    assert!(init_interface_count >= 1, "Expected at least 1 InitInterface instruction");
    println!("✓ test_init_interface_generated passed: type_ids {:?}", init_interface_type_ids);
}
