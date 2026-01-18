# Vo Native FFI Guide

This guide explains how to implement Vo extern functions in Rust.

## Overview

Vo provides two macros for implementing extern functions:

| Macro | Use Case |
|-------|----------|
| `#[vo_extern]` | Simple functions with automatic parameter mapping |
| `#[vo_extern_ctx]` | Functions needing GC access, type metadata, or complex types |

## Quick Start

### Simple Functions with `#[vo_extern]`

```rust
use vo_ext::prelude::*;

#[vo_extern("mylib/math", "Add")]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[vo_extern("mylib/strings", "Concat")]
fn concat(a: &str, b: &str) -> String {
    format!("{}{}", a, b)
}
```

Supported parameter types:
- `i64`, `i32`, `i16`, `i8`, `isize`
- `u64`, `u32`, `u16`, `u8`, `usize`
- `f64`, `f32`
- `bool`
- `&str` (Vo `string`)
- `&[u8]` (Vo `[]byte`)
- `GcRef` (any reference type)
- `AnySlot` (Vo `any` or `interface{}`)

Supported return types:
- All primitive types above
- `String`
- `Vec<u8>`
- `Vec<String>`
- `AnySlot`, `ErrorSlot`
- Tuples of the above

### Context Functions with `#[vo_extern_ctx]`

Use this when you need:
- GC allocation (`ctx.alloc_str()`, `ctx.gc_alloc()`)
- Type metadata access
- Complex `any`/`interface` handling
- Closure calling

```rust
use vo_ext::prelude::*;

#[vo_extern_ctx("mylib", "Process")]
fn process(ctx: &mut ExternCallContext) -> ExternResult {
    // `slots` module is auto-injected with parameter/return slot constants
    // Hover over slots::ARG_X in IDE to see type info
    
    let input = ctx.arg_ref(slots::ARG_INPUT);
    let count = ctx.arg_i64(slots::ARG_COUNT);
    
    // Do work...
    let result = ctx.alloc_str("result");
    
    ctx.ret_ref(slots::RET_0, result);
    ExternResult::Ok
}
```

## Type Mapping

### Vo Type → Rust Type → Slot Count

| Vo Type | Rust Type | Slots |
|---------|-----------|-------|
| `int`, `int64` | `i64` | 1 |
| `int32` | `i32` | 1 |
| `uint`, `uint64` | `u64` | 1 |
| `float64` | `f64` | 1 |
| `bool` | `bool` | 1 |
| `string` | `&str` / `String` | 1 |
| `[]byte` | `&[u8]` / `Vec<u8>` | 1 |
| `*T`, `[]T`, `map[K]V` | `GcRef` | 1 |
| `any`, `interface{}` | `AnySlot` | 2 |
| `error` | `ErrorSlot` | 2 |
| `type Alias any` | `AnySlot` | 2 |

### Working with `any` Types

The `any` type occupies 2 slots:
- slot0: metadata (`[itab_id:32 | rttid:24 | value_kind:8]`)
- slot1: data (immediate value or GcRef)

**Reading `any` arguments:**

```rust
// Full access to metadata
let any_val = ctx.arg_any(slots::ARG_X);
let vk = any_val.value_kind();
let data = any_val.as_i64();

// Convenience methods (when you know the type)
let n = ctx.arg_any_as_i64(slots::ARG_X);
let f = ctx.arg_any_as_f64(slots::ARG_Y);
let r = ctx.arg_any_as_ref(slots::ARG_Z);
```

**Writing `any` return values:**

```rust
ctx.ret_any(slots::RET_0, AnySlot::from_i64(42));
ctx.ret_any(slots::RET_0, AnySlot::from_f64(3.14));
ctx.ret_any(slots::RET_0, AnySlot::from_ref(gc_ref, rttid, value_kind));
ctx.ret_any(slots::RET_0, AnySlot::nil());
```

## Auto-Generated `slots` Module

When using `#[vo_extern_ctx]`, a `mod slots` is automatically injected into your function body:

```rust
#[vo_extern_ctx("pkg", "Func")]
fn my_func(ctx: &mut ExternCallContext) -> ExternResult {
    // Auto-generated (invisible):
    // mod slots {
    //     /// Argument `x` (int) at slot 0, 1 slot(s)
    //     pub const ARG_X: u16 = 0;
    //     /// Argument `y` (any) at slot 1, 2 slot(s)
    //     pub const ARG_Y: u16 = 1;
    //     /// Return value 0 (string) at slot 0, 1 slot(s)
    //     pub const RET_0: u16 = 0;
    //     pub const TOTAL_ARG_SLOTS: u16 = 3;
    //     pub const TOTAL_RET_SLOTS: u16 = 1;
    // }
    
    // Use directly:
    let x = ctx.arg_i64(slots::ARG_X);
    let y = ctx.arg_any_as_i64(slots::ARG_Y);
    // ...
}
```

**IDE Support**: Hover over `slots::ARG_X` to see the parameter name, Vo type, slot position, and slot count.

## Error Handling

```rust
// Return panic (unrecoverable)
return ExternResult::Panic("error message".to_string());

// Return nil error
ctx.ret_nil_error(slots::RET_ERROR);

// Return error value
ctx.ret_error(slots::RET_ERROR, ErrorSlot::from_ref(err_ref, rttid, vk));
```

## ExternCallContext API Reference

### Reading Arguments

| Method | Vo Type | Returns |
|--------|---------|---------|
| `arg_i64(slot)` | `int` | `i64` |
| `arg_u64(slot)` | `uint` | `u64` |
| `arg_f64(slot)` | `float64` | `f64` |
| `arg_bool(slot)` | `bool` | `bool` |
| `arg_ref(slot)` | `*T`, `[]T`, etc. | `GcRef` |
| `arg_str(slot)` | `string` | `&str` |
| `arg_bytes(slot)` | `[]byte` | `&[u8]` |
| `arg_any(slot)` | `any` | `AnySlot` |
| `arg_any_as_i64(slot)` | `any` (int inside) | `i64` |
| `arg_any_as_f64(slot)` | `any` (float inside) | `f64` |
| `arg_any_as_ref(slot)` | `any` (ref inside) | `GcRef` |

### Writing Return Values

| Method | Vo Type |
|--------|---------|
| `ret_i64(slot, val)` | `int` |
| `ret_u64(slot, val)` | `uint` |
| `ret_f64(slot, val)` | `float64` |
| `ret_bool(slot, val)` | `bool` |
| `ret_ref(slot, val)` | `*T`, `[]T`, etc. |
| `ret_str(slot, &str)` | `string` |
| `ret_bytes(slot, &[u8])` | `[]byte` |
| `ret_any(slot, AnySlot)` | `any` |
| `ret_error(slot, ErrorSlot)` | `error` |
| `ret_nil_error(slot)` | `error` (nil) |

### GC Allocation

| Method | Description |
|--------|-------------|
| `alloc_str(&str)` | Allocate string, returns `GcRef` |
| `alloc_bytes(&[u8])` | Allocate byte slice, returns `GcRef` |
| `gc_alloc(slots, types)` | Allocate struct, returns `GcRef` |

## Complete Example

**Vo declaration** (`mylib/api.vo`):
```go
package mylib

type Handle any

func Create(name string) Handle

func Process(h Handle, data []byte) ([]byte, error)
```

**Rust implementation**:
```rust
use vo_ext::prelude::*;
use vo_runtime::objects::string;

static HANDLES: Mutex<Vec<MyData>> = Mutex::new(Vec::new());

#[vo_extern_ctx("mylib", "Create")]
fn create(ctx: &mut ExternCallContext) -> ExternResult {
    let name = ctx.arg_str(slots::ARG_NAME);
    
    let mut handles = HANDLES.lock().unwrap();
    let id = handles.len();
    handles.push(MyData::new(name));
    
    ctx.ret_any(slots::RET_0, AnySlot::from_i64(id as i64));
    ExternResult::Ok
}

#[vo_extern_ctx("mylib", "Process")]
fn process(ctx: &mut ExternCallContext) -> ExternResult {
    let handle_id = ctx.arg_any_as_i64(slots::ARG_H) as usize;
    let data = ctx.arg_bytes(slots::ARG_DATA);
    
    // Process...
    let result = process_data(data);
    
    ctx.ret_bytes(slots::RET_0, &result);
    ctx.ret_nil_error(slots::RET_1);
    ExternResult::Ok
}

vo_ext::export_extensions!();
```

## Struct Accessors with `vo_struct!`

Access Vo struct fields type-safely from Rust:

```rust
use vo_ext::prelude::*;

// Generate accessors for a Vo struct
vo_struct!("mylib", "Config");

#[vo_extern_ctx("mylib", "Run")]
fn run(ctx: &mut ExternCallContext) -> ExternResult {
    // Stack-passed struct (Accessor)
    let config = Config::at(slots::ARG_CONFIG);
    let title = config.title(ctx);           // getter
    config.set_width(ctx, 1920);             // setter
    
    // Heap struct via pointer (Ref)
    let obj = Config::from_ref(ctx.arg_ref(slots::ARG_PTR));
    let name = obj.name(ctx);
    obj.set_count(ctx, 42);
    
    // Build new struct
    let data = Config::builder()
        .width(1920)
        .height(1080)
        .title(ctx.alloc_str("Hello"))
        .data();
    
    ExternResult::Ok
}
```

Generated types:
- `Config::at(slot) -> Accessor` - for stack-passed structs
- `Config::from_ref(ptr) -> Ref` - for heap objects
- `Config::builder() -> Builder` - for constructing new structs

## Container Accessors

Type-safe wrappers for Vo container types:

### VoSlice

```rust
let s = VoSlice::<i64>::from_ref(ctx.arg_ref(slots::ARG_DATA));
let len = s.len();
let val = s.get(0);
s.set(0, val * 2);

// Cursor iteration (allows modification)
let mut cur = s.cursor();
while let Some((idx, val)) = cur.next() {
    if val > 100 { s.set(idx, val * 2); }
}
```

### VoMap

```rust
let m = VoMap::<String, i64>::from_ref(ctx.arg_ref(slots::ARG_MAP));
if let Some(v) = m.get(ctx, "key") {  // ctx needed for string key alloc
    m.set(ctx, "key", v + 1);
}
m.delete(ctx, "old");

// Cursor iteration
let mut cur = m.cursor();
while let Some((k, v)) = cur.next() {
    println!("{}: {}", k, v);
}
```

### VoString / VoBytes

```rust
// Zero-copy string access
let s = VoString::from_ref(ctx.arg_ref(slots::ARG_NAME));
let name: &str = s.as_str();

// Byte slice
let b = VoBytes::from_ref(ctx.arg_ref(slots::ARG_DATA));
let data: &[u8] = b.as_slice();
```

## Enhanced Type Accessors

### AnySlot

Full control over `any` / `interface{}` values:

```rust
let any = ctx.arg_any(slots::ARG_VALUE);

// Type checking
if any.is_nil() { /* ... */ }
if any.is_int() { let v = any.as_i64(); }
if any.is_string() { let s = any.as_str(); }
if any.is_ref_type() { let r = any.as_ref(); }

// Creation
let a = AnySlot::nil();
let a = AnySlot::from_i64(42);
let a = AnySlot::from_f64(3.14);
let a = AnySlot::from_ref(ptr, rttid, ValueKind::Slice);

// Write to return slot
ctx.ret_any(slots::RET_0, a);
```

### ErrorSlot

```rust
let err = ctx.arg_any(slots::ARG_ERR);  // ErrorSlot = AnySlot
if err.is_ok() { /* no error */ }
if err.is_err() {
    let msg = err.message();
}

// Create errors
let ok = ErrorSlot::ok();
ctx.ret_any(slots::RET_ERROR, ok);
```

### VoClosure

Call Vo closures from Rust:

```rust
let cb = VoClosure::from_ref(ctx.arg_ref(slots::ARG_CALLBACK));

cb.call0(ctx);                              // no args, no return
let r = cb.call0_ret_i64(ctx);              // no args, return i64
cb.call1_i64(ctx, 42);                      // one i64 arg
let r = cb.call1_i64_ret_i64(ctx, 42);      // one i64 arg, return i64

// Generic call with raw slots
let args = [1u64, 2u64];
let mut ret = [0u64];
cb.call_raw(ctx, &args, &mut ret);
```

### VoPtr

```rust
let p = VoPtr::<MyStruct>::from_ref(ctx.arg_ref(slots::ARG_PTR));
if !p.is_null() {
    let r: GcRef = p.as_ref();
}
```
