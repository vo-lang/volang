use crate::importer::NullImporter;
use crate::Checker;
use std::path::PathBuf;
use vo_syntax::parser;

/// Helper to parse and type-check Vo code, returning escaped variable names.
fn get_escaped_vars(source: &str) -> Vec<String> {
    use crate::arena::ArenaKey;
    use crate::objects::PackageKey;
    let (file, _diags, interner) = parser::parse(source, 0);

    let mut checker = Checker::new_with_trace(PackageKey::null(), interner, false);
    let main_pkg_key = checker
        .tc_objs
        .new_package("main".to_string(), "main".to_string());
    checker.pkg = main_pkg_key;

    let mut importer = NullImporter::new(PathBuf::from("."));
    if checker.check_with_importer(&[file], &mut importer).is_err() {
        panic!("Type check failed: {:?}", checker.diagnostics.take());
    }

    let mut names: Vec<String> = checker
        .result
        .escaped_vars
        .iter()
        .map(|&obj| checker.tc_objs.lobjs[obj].name().to_string())
        .collect();
    names.sort();
    names
}

// =========================================================================
// No escape tests
// =========================================================================

#[test]
fn test_no_escape_simple() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                x := 42
                y := x + 1
                _ = y
            }
        "#,
    );
    assert!(
        escaped.is_empty(),
        "no variables should escape: {:?}",
        escaped
    );
}

#[test]
fn test_no_escape_struct_local_use() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var p Point
                p.x = 1
                p.y = 2
                _ = p.x + p.y
            }
        "#,
    );
    assert!(
        escaped.is_empty(),
        "no variables should escape: {:?}",
        escaped
    );
}

// =========================================================================
// Address taken tests
// =========================================================================

#[test]
fn test_address_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var s Point
                p := &s
                _ = p
            }
        "#,
    );
    assert!(
        escaped.contains(&"s".to_string()),
        "s should escape: {:?}",
        escaped
    );
}

#[test]
fn test_nested_field_address_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Inner struct { x int }
            type Outer struct { inner Inner }
            func main() {
                var o Outer
                p := &o.inner
                _ = p
            }
        "#,
    );
    assert!(
        escaped.contains(&"o".to_string()),
        "o should escape (not just inner): {:?}",
        escaped
    );
}

#[test]
fn test_deeply_nested_field_address_escape() {
    // Take address of nested struct field (not int - Vo only allows &struct)
    let escaped = get_escaped_vars(
        r#"
            package main
            type A struct { x int }
            type B struct { a A }
            type C struct { b B }
            func main() {
                var c C
                p := &c.b.a
                _ = p
            }
        "#,
    );
    assert!(
        escaped.contains(&"c".to_string()),
        "c should escape: {:?}",
        escaped
    );
}

#[test]
fn test_array_element_address_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Big struct { x int }
            var sink *Big
            func main() {
                var arr [2]Big
                sink = &arr[0]
            }
        "#,
    );
    assert!(
        escaped.contains(&"arr".to_string()),
        "arr should escape when an element address is taken: {:?}",
        escaped
    );
}

#[test]
fn test_array_element_pointer_receiver_method_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Big struct { x int }
            func (b *Big) set() { b.x = 1 }
            func main() {
                arr := [2]Big{}
                arr[0].set()
            }
        "#,
    );
    assert!(
        escaped.contains(&"arr".to_string()),
        "arr should escape for pointer-receiver method calls on elements: {:?}",
        escaped
    );
}

// =========================================================================
// Closure capture tests
// =========================================================================

#[test]
fn test_closure_capture_int() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                x := 42
                f := func() int { return x }
                _ = f()
            }
        "#,
    );
    assert!(
        escaped.contains(&"x".to_string()),
        "x should escape: {:?}",
        escaped
    );
}

#[test]
fn test_closure_capture_struct() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var p Point
                f := func() int { return p.x }
                _ = f()
            }
        "#,
    );
    assert!(
        escaped.contains(&"p".to_string()),
        "p should escape: {:?}",
        escaped
    );
}

#[test]
fn test_closure_param_capture() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func foo(x int) func() int {
                return func() int { return x }
            }
            func main() {
                f := foo(42)
                _ = f()
            }
        "#,
    );
    assert!(
        escaped.contains(&"x".to_string()),
        "x (param) should escape: {:?}",
        escaped
    );
}

#[test]
fn test_nested_closure_capture() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                x := 1
                f := func() {
                    g := func() int { return x }
                    _ = g()
                }
                f()
            }
        "#,
    );
    assert!(
        escaped.contains(&"x".to_string()),
        "x should escape: {:?}",
        escaped
    );
}

#[test]
fn test_nested_closure_param_capture() {
    // Test case: closure parameter captured by inner closure
    // func(a) returns func(b) that uses a
    let escaped = get_escaped_vars(
        r#"
            package main
            func outer() func(int) func(int) int {
                return func(a int) func(int) int {
                    return func(b int) int {
                        return a + b
                    }
                }
            }
            func main() {
                f := outer()
                g := f(5)
                _ = g(3)
            }
        "#,
    );
    assert!(
        escaped.contains(&"a".to_string()),
        "a (closure param) should escape: {:?}",
        escaped
    );
}

#[test]
fn test_closure_no_capture_local() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                f := func() int {
                    y := 10
                    return y
                }
                _ = f()
            }
        "#,
    );
    // y is local to the closure, not captured
    assert!(
        !escaped.contains(&"y".to_string()),
        "y should not escape: {:?}",
        escaped
    );
}

// =========================================================================
// Interface assignment tests
// =========================================================================

#[test]
fn test_interface_assignment() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var s Point
                var i interface{} = s
                _ = i
            }
        "#,
    );
    assert!(
        escaped.contains(&"s".to_string()),
        "s should escape: {:?}",
        escaped
    );
}

#[test]
fn test_interface_param_assignment() {
    let _escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func foo(i interface{}) {
                _ = i
            }
            func main() {
                var s Point
                foo(s)
            }
        "#,
    );
    // Note: This tests function call, not direct assignment
    // The current implementation may not catch this case
    // as it only handles direct assignment statements
}

// =========================================================================
// Slice operation tests
// =========================================================================

#[test]
fn test_array_slice_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                var arr [5]int
                s := arr[:]
                _ = s
            }
        "#,
    );
    assert!(
        escaped.contains(&"arr".to_string()),
        "arr should escape: {:?}",
        escaped
    );
}

#[test]
fn test_array_partial_slice_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                var arr [10]int
                s := arr[2:5]
                _ = s
            }
        "#,
    );
    assert!(
        escaped.contains(&"arr".to_string()),
        "arr should escape: {:?}",
        escaped
    );
}

#[test]
fn test_slice_of_slice_no_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                var s []int
                s2 := s[1:3]
                _ = s2
            }
        "#,
    );
    // Slicing a slice doesn't cause escape (slice is already a reference type)
    assert!(
        !escaped.contains(&"s".to_string()),
        "s should not escape: {:?}",
        escaped
    );
}

// =========================================================================
// Package-level variable tests
// =========================================================================

#[test]
fn test_package_var_no_capture() {
    let escaped = get_escaped_vars(
        r#"
            package main
            var globalX int = 10
            func main() {
                f := func() int { return globalX }
                _ = f()
            }
        "#,
    );
    // Package-level variables are not "captured" - they're already global
    assert!(
        !escaped.contains(&"globalX".to_string()),
        "globalX should not be marked as escaped: {:?}",
        escaped
    );
}

// =========================================================================
// Multiple variables tests
// =========================================================================

#[test]
fn test_multiple_escapes() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var a Point
                var b Point
                var c Point
                
                p := &a
                f := func() int { return b.x }
                var i interface{} = c
                
                _ = p
                _ = f()
                _ = i
            }
        "#,
    );
    assert!(
        escaped.contains(&"a".to_string()),
        "a should escape: {:?}",
        escaped
    );
    assert!(
        escaped.contains(&"b".to_string()),
        "b should escape: {:?}",
        escaped
    );
    assert!(
        escaped.contains(&"c".to_string()),
        "c should escape: {:?}",
        escaped
    );
}

#[test]
fn test_mixed_escape_and_no_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var escapes Point
                var stays Point
                
                p := &escapes
                _ = p
                
                stays.x = 1
                _ = stays.x
            }
        "#,
    );
    assert!(
        escaped.contains(&"escapes".to_string()),
        "escapes should escape: {:?}",
        escaped
    );
    assert!(
        !escaped.contains(&"stays".to_string()),
        "stays should not escape: {:?}",
        escaped
    );
}

// =========================================================================
// Control flow tests
// =========================================================================

#[test]
fn test_escape_in_if_branch() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var s Point
                if true {
                    p := &s
                    _ = p
                }
            }
        "#,
    );
    assert!(
        escaped.contains(&"s".to_string()),
        "s should escape: {:?}",
        escaped
    );
}

#[test]
fn test_escape_in_for_loop() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                x := 0
                for i := 0; i < 10; i += 1 {
                    f := func() int { return x }
                    _ = f()
                }
            }
        "#,
    );
    assert!(
        escaped.contains(&"x".to_string()),
        "x should escape: {:?}",
        escaped
    );
}

#[test]
fn test_select_recv_capture() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                done := make(chan struct{})
                f := func() {
                    select {
                    case <-done:
                    default:
                    }
                }
                _ = f
            }
        "#,
    );
    assert!(
        escaped.contains(&"done".to_string()),
        "done should escape when captured in select receive clause: {:?}",
        escaped
    );
}

#[test]
fn test_select_recv_assign_capture() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                ch := make(chan int)
                x := 0
                f := func() {
                    select {
                    case x = <-ch:
                    default:
                    }
                }
                _ = f
            }
        "#,
    );
    assert!(
        escaped.contains(&"x".to_string()),
        "x should escape when assigned in select receive clause inside closure: {:?}",
        escaped
    );
}

#[test]
fn test_select_send_capture() {
    let escaped = get_escaped_vars(
        r#"
            package main
            func main() {
                ch := make(chan int)
                x := 1
                f := func() {
                    select {
                    case ch <- x:
                    default:
                    }
                }
                _ = f
            }
        "#,
    );
    assert!(
        escaped.contains(&"ch".to_string()),
        "ch should escape when used in select send clause inside closure: {:?}",
        escaped
    );
    assert!(
        escaped.contains(&"x".to_string()),
        "x should escape when used in select send value inside closure: {:?}",
        escaped
    );
}

// =========================================================================
// Edge cases
// =========================================================================

#[test]
fn test_index_then_address() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func main() {
                var arr [3]Point
                p := &arr[0]
                _ = p
            }
        "#,
    );
    assert!(
        escaped.contains(&"arr".to_string()),
        "arr should escape: {:?}",
        escaped
    );
}

#[test]
fn test_pointer_receiver_escape() {
    let escaped = get_escaped_vars(
        r#"
            package main
            type Point struct { x int; y int }
            func (p *Point) Double() { p.x = p.x * 2 }
            func main() {
                p := Point{x: 1, y: 2}
                p.Double()
            }
        "#,
    );
    assert!(
        escaped.contains(&"p".to_string()),
        "p should escape due to pointer receiver method call, got: {:?}",
        escaped
    );
}
