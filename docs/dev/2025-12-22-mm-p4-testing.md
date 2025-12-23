# MM Phase 4: Testing and Validation

**Parent**: [2025-12-22-mm-memory-model-plan.md](2025-12-22-mm-memory-model-plan.md)  
**Status**: Not Started  
**Est. Lines**: ~300  
**Depends On**: P2 (Codegen), P3 (Runtime)

## Overview

Comprehensive testing to validate the new memory model works correctly.

## Test Categories

### 4.1 Escape Analysis Unit Tests

Location: `vo-analysis/src/check/escape_test.rs` (new)

```rust
#[test]
fn test_no_escape_local_struct() {
    // var s Point; s.x = 1 → s should NOT escape
}

#[test]
fn test_escape_address_taken() {
    // var s Point; p := &s → s should escape
}

#[test]
fn test_escape_closure_capture() {
    // x := 42; f := func() { println(x) } → x should escape
}

#[test]
fn test_escape_interface_assign() {
    // var s Point; var i interface{} = s → s should escape
}

#[test]
fn test_escape_array_slice() {
    // var arr [5]int; s := arr[:] → arr should escape
}

#[test]
fn test_escape_transitivity() {
    // type Outer { inner Inner }; &outer → inner also escapes
}

#[test]
fn test_escape_size_threshold() {
    // var big [300]int → always escapes (>256 slots)
}
```

### 4.2 Codegen Unit Tests

Location: `vo-codegen-vm/src/tests/escape_test.rs` (new)

```rust
#[test]
fn test_stack_struct_alloc() {
    // Verify non-escaping struct uses inline slots
}

#[test]
fn test_heap_struct_alloc() {
    // Verify escaping struct uses ALLOC
}

#[test]
fn test_stack_field_access() {
    // Verify stack struct uses direct register access
}

#[test]
fn test_heap_field_access() {
    // Verify heap struct uses GET_FIELD/SET_FIELD
}
```

### 4.3 Integration Tests

Location: `test_data/escape/` (new directory)

| Test File | Description |
|-----------|-------------|
| `stack_struct.vo` | Non-escaping struct operations |
| `heap_struct.vo` | Escaping struct operations |
| `closure_capture.vo` | Closure variable capture |
| `interface_boxing.vo` | Interface boxing of struct |
| `array_slice.vo` | Array slicing |
| `nested_struct.vo` | Nested struct escape propagation |
| `boxed_primitives.vo` | Boxed int/float/bool |
| `mixed_escape.vo` | Mix of escaping and non-escaping |

### 4.4 Regression Tests

Re-enable previously skipped tests after implementation:

- `defer_stmt.vo`
- `select_stmt.vo`
- `slice_map.vo`
- etc.

## Test File Examples

### stack_struct.vo

```vo
package main

type Point struct {
    x, y int
}

func main() {
    var p Point  // Should NOT escape
    p.x = 10
    p.y = 20
    assert(p.x + p.y == 30)
    println("[VO:OK]")
}
```

### closure_capture.vo

```vo
package main

func main() {
    x := 42  // Should escape (captured by closure)
    
    f := func() int {
        return x
    }
    
    assert(f() == 42)
    
    x = 100
    assert(f() == 100)  // Closure sees updated value
    
    println("[VO:OK]")
}
```

### boxed_primitives.vo

```vo
package main

func main() {
    // Test boxed int
    x := 1
    inc := func() { x += 1 }
    inc()
    inc()
    assert(x == 3)
    
    // Test boxed float
    y := 1.5
    double := func() { y *= 2 }
    double()
    assert(y == 3.0)
    
    println("[VO:OK]")
}
```

## Validation Checklist

- [ ] All existing tests still pass
- [ ] Escape analysis correctly identifies all escape scenarios
- [ ] Stack structs use inline slot allocation
- [ ] Heap structs use ALLOC instruction
- [ ] Field access uses correct instruction based on allocation
- [ ] Boxed primitives work correctly
- [ ] GC correctly scans both stack and heap values
- [ ] No memory leaks or dangling references

## Performance Validation

Compare before/after:
- Allocation count (should decrease for non-escaping values)
- GC pressure (should decrease)
- Execution time for struct-heavy code

## Tasks Checklist

- [ ] Create escape analysis unit tests
- [ ] Create codegen unit tests
- [ ] Create integration test files
- [ ] Add tests to `_tests.yaml`
- [ ] Run full test suite
- [ ] Re-enable previously skipped tests
- [ ] Performance benchmarking (optional)
