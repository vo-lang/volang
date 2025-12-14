# GoX FFI Design

## Overview

FFI (Foreign Function Interface) allows GoX code to call native (Rust) functions.

## FFI Types

```rust
pub struct GoxString { ptr: *const u8, len: usize }
pub struct GoxSlice { ptr: *mut u8, len: usize, cap: usize }
pub struct GoxBytes { ptr: *mut u8, len: usize, cap: usize }
```

## Runtime Context

All backends implement the `RuntimeContext` trait:

```rust
pub trait RuntimeContext {
    fn alloc(&mut self, layout: Layout) -> *mut u8;
    fn dealloc(&mut self, ptr: *mut u8, layout: Layout);
    fn gc_root(&mut self, ptr: *mut u8);
    fn gc_unroot(&mut self, ptr: *mut u8);
}
```

## Native Function Registration

Native functions are implemented in `gox-runtime-*/natives/`:

```rust
#[gox_native(name = "println")]
pub fn println(args: &[FfiValue]) {
    // implementation
}
```
