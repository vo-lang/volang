//! WASM tests - run with: wasm-pack test --node

use wasm_bindgen_test::*;

#[wasm_bindgen_test]
fn test_version() {
    let v = vo_web::version();
    assert!(v.starts_with("Vo Web"));
}

#[wasm_bindgen_test]
#[cfg(feature = "compiler")]
fn test_compile_simple() {
    let source = r#"
package main

func add(a, b int) int {
    return a + b
}

func main() {
    x := add(1, 2)
    _ = x
}
"#;
    let result = vo_web::compile(source, None);
    assert!(result.success(), "compile failed: {:?}", result.error_message());
    assert!(result.bytecode().is_some());
}

#[wasm_bindgen_test]
#[cfg(feature = "compiler")]
fn test_compile_with_stdlib() {
    // Test with stdlib (fmt.Sprintf, errors, etc.)
    let source = r#"
package main

import "fmt"

func main() {
    s := fmt.Sprintf("hello %d", 42)
    println(s)
}
"#;
    let result = vo_web::compile(source, None);
    assert!(result.success(), "compile failed: {:?}", result.error_message());
    assert!(result.bytecode().is_some());
}
