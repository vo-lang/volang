//! End-to-end tests: Vo source -> bytecode -> JIT execution.
//!
//! Tests must run serially because they share global runtime state
//! (function table, GC, globals).

use vo_jit::JitCompiler;
use vo_syntax::parse;
use vo_analysis::analyze_single_file;
use vo_codegen_vm::compile_project;
use vo_vm::bytecode::Module as BytecodeModule;
use std::sync::Mutex;

/// Global lock to ensure tests run serially (shared global runtime state).
static TEST_LOCK: Mutex<()> = Mutex::new(());

/// Compile Vo source to bytecode.
fn source_to_bytecode(source: &str) -> BytecodeModule {
    let (file, _parse_diag, interner) = parse(source, 0);
    let project = analyze_single_file(file, interner).expect("Type check failed");
    compile_project(&project).expect("Compilation failed")
}

/// Compile bytecode to JIT and get function pointer.
/// Returns the lock guard to keep the test serialized until completion.
fn compile_to_jit<F: Copy>(bytecode: &BytecodeModule, func_name: &str) -> (F, JitCompiler, std::sync::MutexGuard<'static, ()>) {
    let guard = TEST_LOCK.lock().unwrap();
    let mut jit = JitCompiler::new().unwrap();
    jit.compile_module(bytecode).unwrap();
    let func: F = unsafe { jit.get_function(func_name).unwrap() };
    (func, jit, guard)
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
    let (add_fn, _jit, _guard): (fn(i64, i64) -> i64, _, _) = compile_to_jit(&bytecode, "add");
    
    assert_eq!(add_fn(3, 5), 8);
    assert_eq!(add_fn(100, 200), 300);
    assert_eq!(add_fn(-10, 5), -5);
    println!("✓ test_e2e_simple_add passed");
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
    let (max_fn, _jit, _guard): (fn(i64, i64) -> i64, _, _) = compile_to_jit(&bytecode, "max");
    
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
    let (factorial_fn, _jit, _guard): (fn(i64) -> i64, _, _) = compile_to_jit(&bytecode, "factorial");
    
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
    let (fib_fn, _jit, _guard): (fn(i64) -> i64, _, _) = compile_to_jit(&bytecode, "fib");
    
    assert_eq!(fib_fn(0), 0);
    assert_eq!(fib_fn(1), 1);
    assert_eq!(fib_fn(2), 1);
    assert_eq!(fib_fn(10), 55);
    assert_eq!(fib_fn(15), 610);
    println!("✓ test_e2e_fibonacci passed");
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
    let (sum_fn, _jit, _guard): (fn(i64) -> i64, _, _) = compile_to_jit(&bytecode, "sum");
    
    assert_eq!(sum_fn(5), 10);
    assert_eq!(sum_fn(10), 45);
    assert_eq!(sum_fn(1), 0);
    assert_eq!(sum_fn(0), 0);
    println!("✓ test_e2e_for_loop passed");
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
    let (gcd_fn, _jit, _guard): (fn(i64, i64) -> i64, _, _) = compile_to_jit(&bytecode, "gcd");
    
    assert_eq!(gcd_fn(48, 18), 6);
    assert_eq!(gcd_fn(100, 25), 25);
    assert_eq!(gcd_fn(17, 13), 1);
    assert_eq!(gcd_fn(0, 5), 5);
    println!("✓ test_e2e_gcd passed");
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
    let (fn_ptr, _jit, _guard): (fn(i64) -> i64, _, _) = compile_to_jit(&bytecode, "quadruple");
    
    assert_eq!(fn_ptr(5), 20);
    assert_eq!(fn_ptr(10), 40);
    println!("✓ test_e2e_nested_calls passed");
}

#[test]
fn test_e2e_assert_pass() {
    let source = r#"
package main

func test_assert(x int) int {
    assert(x > 0)
    assert(x < 100)
    return x * 2
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (fn_ptr, _jit, _guard): (fn(i64) -> i64, _, _) = compile_to_jit(&bytecode, "test_assert");
    
    // These should pass without issues
    assert_eq!(fn_ptr(5), 10);
    assert_eq!(fn_ptr(50), 100);
    println!("✓ test_e2e_assert_pass passed");
}

#[test]
fn test_e2e_assert_with_message() {
    let source = r#"
package main

func validate(x int, y int) int {
    assert(x > 0, x)
    assert(y > 0, y)
    return x + y
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (fn_ptr, _jit, _guard): (fn(i64, i64) -> i64, _, _) = compile_to_jit(&bytecode, "validate");
    
    // These should pass without issues
    assert_eq!(fn_ptr(5, 10), 15);
    assert_eq!(fn_ptr(1, 1), 2);
    println!("✓ test_e2e_assert_with_message passed");
}

#[test]
fn test_e2e_println() {
    let source = r#"
package main

func debug_and_compute(x int) int {
    println(x)
    result := x * 2
    println(result)
    return result
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (fn_ptr, _jit, _guard): (fn(i64) -> i64, _, _) = compile_to_jit(&bytecode, "debug_and_compute");
    
    // Should print values and return correct result
    assert_eq!(fn_ptr(21), 42);
    println!("✓ test_e2e_println passed");
}

#[test]
fn test_e2e_string_len() {
    let source = r#"
package main

func get_len() int {
    s := "hello"
    return len(s)
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (fn_ptr, _jit, _guard): (fn() -> i64, _, _) = compile_to_jit(&bytecode, "get_len");
    
    assert_eq!(fn_ptr(), 5);
    println!("✓ test_e2e_string_len passed");
}

#[test]
fn test_e2e_string_eq() {
    let source = r#"
package main

func compare_strings() int {
    a := "hello"
    b := "hello"
    c := "world"
    if a == b {
        if a != c {
            return 1
        }
    }
    return 0
}

func main() {
}
"#;
    
    let bytecode = source_to_bytecode(source);
    let (fn_ptr, _jit, _guard): (fn() -> i64, _, _) = compile_to_jit(&bytecode, "compare_strings");
    
    assert_eq!(fn_ptr(), 1);
    println!("✓ test_e2e_string_eq passed");
}
