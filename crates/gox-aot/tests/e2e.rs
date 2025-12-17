//! End-to-end tests: GoX source -> bytecode -> JIT execution.

use cranelift_codegen::settings::{self, Configurable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::Module;

use gox_aot::context::CompileContext;
use gox_aot::translate::FunctionTranslator;
use gox_common::DiagnosticSink;
use gox_syntax::parse;
use gox_analysis::typecheck_file;
use gox_codegen_vm::compile;
use gox_vm::bytecode::Module as BytecodeModule;

/// Compile GoX source to bytecode.
fn source_to_bytecode(source: &str) -> BytecodeModule {
    let (file, _parse_diag, interner) = parse(source, 0);
    let mut diag = DiagnosticSink::new();
    let result = typecheck_file(&file, &interner, &mut diag);
    
    if diag.has_errors() {
        panic!("Type check errors");
    }
    
    compile(&file, &result, &interner).expect("Compilation failed")
}

/// Setup JIT module for testing.
fn setup_jit() -> (JITModule, cranelift_codegen::isa::CallConv) {
    let mut flag_builder = settings::builder();
    flag_builder.set("use_colocated_libcalls", "false").unwrap();
    flag_builder.set("is_pic", "false").unwrap();
    let isa_builder = cranelift_native::builder().unwrap();
    let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
    let call_conv = isa.default_call_conv();
    
    let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    let module = JITModule::new(builder);
    
    (module, call_conv)
}

/// Compile bytecode to JIT and get function pointer.
fn compile_to_jit(
    bytecode: &BytecodeModule,
    func_name: &str,
) -> (*const u8, JITModule) {
    let (mut module, call_conv) = setup_jit();
    
    let mut compile_ctx = CompileContext::new(bytecode, call_conv);
    compile_ctx.declare_all_functions(&mut module).unwrap();
    
    // Find the target function index
    let func_idx = bytecode.functions.iter()
        .position(|f| f.name == func_name)
        .expect(&format!("Function '{}' not found", func_name));
    
    // Compile all functions (needed for cross-function calls)
    for (idx, func_def) in bytecode.functions.iter().enumerate() {
        let func_id = compile_ctx.get_gox_func(idx as u32).unwrap();
        
        let mut ctx = module.make_context();
        ctx.func.signature = module.declarations().get_function_decl(func_id).signature.clone();
        ctx.func.name = cranelift_codegen::ir::UserFuncName::user(0, func_id.as_u32());
        
        let mut translator = FunctionTranslator::new(func_def.local_slots as usize, &func_def.code);
        translator.translate(&mut ctx.func, &func_def.code, &mut compile_ctx, &mut module).unwrap();
        
        module.define_function(func_id, &mut ctx).unwrap();
        module.clear_context(&mut ctx);
    }
    
    module.finalize_definitions().unwrap();
    
    let func_id = compile_ctx.get_gox_func(func_idx as u32).unwrap();
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
