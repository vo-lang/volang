# Vo Native FFI Design

## Overview

This document describes the Native FFI (Foreign Function Interface) mechanism for the Vo language, allowing users to implement Vo functions in Rust.

## Design Goals

1. **Simple user interface**: Write ordinary Rust functions, macros handle conversion automatically
2. **Compile-time safety**: Signature mismatches cause compile errors, not runtime errors
3. **Zero runtime overhead**: Macro expands to direct slot access, no extra checks
4. **Consistent with Vo module system**: Import path resolution reuses vo-module logic

## Architecture

### Crate Structure

```
crates/
├── vo-module/        # Module resolution (existing)
│   └── resolver.rs   # Import path resolution logic
│
├── vo-runtime-core/  # Shared runtime core (VM and JIT both use)
│   └── src/ffi.rs    # ExternCall, ExternResult type definitions
│
├── vo-vm/            # VM interpreter
│   └── depends on vo-runtime-core
│
├── vo-jit/           # JIT compiler
│   └── depends on vo-runtime-core
│
└── vo-ffi-macro/     # proc macro (new)
    └── src/lib.rs    # #[vo_extern] implementation
```

### Dependency Graph

```
Compile-time:
  user crate ──[proc-macro]──> vo-ffi-macro ──> vo-module
                                                (path resolution, not propagated to runtime)
Runtime:
  user crate ──> vo-runtime-core (ExternCall and other types)
                      ▲
                      │
              vo-vm ──┴── vo-jit (both depend on runtime-core)
```

## Type Definitions

### ExternCall (vo-runtime-core/src/ffi.rs)

```rust
/// External function call context
pub struct ExternCall<'a> {
    stack: &'a mut [u64],
    bp: usize,
    arg: u16,
    ret: u16,
}

impl ExternCall<'_> {
    // Argument reading
    pub fn i64_at(&self, slot: u16) -> i64;
    pub fn f64_at(&self, slot: u16) -> f64;
    pub fn bool_at(&self, slot: u16) -> bool;
    
    // Return value writing
    pub fn set_i64(&mut self, slot: u16, val: i64);
    pub fn set_f64(&mut self, slot: u16, val: f64);
    pub fn set_bool(&mut self, slot: u16, val: bool);
}

/// Context with GC access (for strings, slices, etc.)
pub struct ExternCallWithGc<'a> {
    call: ExternCall<'a>,
    gc: &'a mut Gc,
}

impl ExternCallWithGc<'_> {
    pub fn arg_str(&self, slot: u16) -> &str;
    pub fn ret_str(&mut self, slot: u16, s: &str);
    // ...
}

/// Execution result
pub enum ExternResult {
    Ok,
    Yield,
    Panic(String),
}
```

## Proc Macro Design

### User Interface

```rust
use vo_ffi_macro::vo_extern;

#[vo_extern("fmt", "Println")]
fn println(args: VarArgs) -> i64 {
    // implementation
}

#[vo_extern("game/engine", "RenderSprite")]
fn render_sprite(x: i64, y: i64, sprite: &str) {
    // implementation
}

#[vo_extern("math", "Divmod")]
fn divmod(a: i64, b: i64) -> (i64, i64) {
    (a / b, a % b)
}
```

### Macro Expansion

```rust
// #[vo_extern("math", "Divmod")]
// fn divmod(a: i64, b: i64) -> (i64, i64) { (a / b, a % b) }

// Expands to:
fn divmod(a: i64, b: i64) -> (i64, i64) { (a / b, a % b) }

fn __vo_extern_divmod(call: &mut vo_runtime_core::ffi::ExternCall) -> vo_runtime_core::ffi::ExternResult {
    let a = call.i64_at(0);
    let b = call.i64_at(1);
    let (__r0, __r1) = divmod(a, b);
    call.set_i64(0, __r0);
    call.set_i64(1, __r1);
    vo_runtime_core::ffi::ExternResult::Ok
}
```

### Compile-time Validation

When the proc macro executes:

1. **Resolve import path**: Reuse `vo-module::ModuleResolver::resolve_import`
2. **Read .vo files**: Find the corresponding package directory
3. **Extract function signature**: Find function declarations without bodies
4. **Validate signature match**: Rust parameter/return types correspond to Vo signature

### Error Examples

```rust
#[vo_extern("fmt", "Println")]
fn println(x: i64) { }  // signature mismatch
```

```
error: signature mismatch with fmt::Println
  --> src/lib.rs:3:1
   |
   | Vo:   func Println(a ...any) int
   | Rust: fn(i64)
   |       ~~~~~~~~
   = note: Vo expects variadic `any` and returns `int`
```

## Type Mapping

| Rust Type | Vo Type | Slots |
|-----------|---------|-------|
| `i64` | `int` | 1 |
| `i32` | `int32` | 1 |
| `f64` | `float64` | 1 |
| `bool` | `bool` | 1 |
| `&str` | `string` (borrowed) | 1 |
| `String` | `string` (return new) | 1 |
| `GcRef` | any reference type | 1 |
| `(T, U)` | multiple return values | N |
| `&mut Gc` | special parameter | 0 |

## User Workflow

### 1. Vo side: Declare functions (no body)

```go
// game/engine.vo
package engine

func RenderSprite(x, y int, sprite string)
func PlaySound(name string, volume float64)
func GetInput() (int, int, bool)
```

### 2. Rust side: Create native crate

```toml
# native/Cargo.toml
[package]
name = "mygame-native"

[lib]
crate-type = ["cdylib"]

[dependencies]
vo-runtime-core = "0.1"
vo-ffi-macro = "0.1"
```

```rust
// native/src/lib.rs
use vo_ffi_macro::vo_extern;

#[vo_extern("game/engine", "RenderSprite")]
fn render_sprite(x: i64, y: i64, sprite: &str) {
    my_engine::render(x as i32, y as i32, sprite);
}

#[vo_extern("game/engine", "GetInput")]
fn get_input() -> (i64, i64, bool) {
    let (x, y, pressed) = my_engine::input::poll();
    (x as i64, y as i64, pressed)
}
```

### 3. Build native library

```bash
cd native && cargo build --release

# Copy to .vonative directory
mkdir -p ../.vonative
cp target/release/libmygame_native.dylib ../.vonative/libgame_engine.dylib
```

### 4. Run

```bash
# VM automatically loads native libraries from .vonative/
vo run main.vo
```

## Native Library Directory

Native libraries are stored in `.vonative/` at project root:

```
project/
├── vo.mod
├── main.vo
├── game/
│   └── engine.vo           # declares: func RenderSprite(x, y int, s string)
└── .vonative/              # native library directory
    └── libgame_engine.dylib
```

### Library Naming Convention

| Package Path | Library Name |
|-------------|--------------|
| `game/engine` | `libgame_engine.{dylib,so,dll}` |
| `fmt` | `libfmt.{dylib,so,dll}` |
| `encoding/json` | `libencoding_json.{dylib,so,dll}` |

### Symbol Naming Convention

The macro generates exported symbols with pattern `__vo_<pkg>_<func>`:

```rust
#[vo_extern("game/engine", "RenderSprite")]
fn render_sprite(...) { ... }

// Generates:
#[no_mangle]
pub extern "C" fn __vo_game_engine_RenderSprite(...) { ... }
```

The VM loads `.vonative/libgame_engine.dylib` and looks up symbol `__vo_game_engine_RenderSprite`.

### 5. vo.mod configuration (optional)

```
module mygame

native "game/engine" = "./native"
```

## Implementation Details

### vo-ffi-macro Core Logic

```rust
#[proc_macro_attribute]
pub fn vo_extern(attr: TokenStream, item: TokenStream) -> TokenStream {
    // 1. Parse attributes: ("pkg/path", "FuncName")
    let (pkg_path, func_name) = parse_attr(attr);
    
    // 2. Find project root (search upward for vo.mod)
    let project_root = find_vo_mod_root();
    
    // 3. Reuse vo-module to resolve import path
    let resolver = ModuleResolver::new(&project_root);
    let mod_file = ModFile::parse_file(project_root.join("vo.mod"))?;
    let closure = resolver.compute_closure(&mod_file)?;
    let pkg = resolver.resolve_import(&pkg_path, &closure)?;
    
    // 4. Parse .vo files, extract function signatures without bodies
    let vo_sig = find_extern_func(&pkg.dir, &func_name)?;
    
    // 5. Parse Rust function
    let rust_func = parse_macro_input!(item as ItemFn);
    
    // 6. Validate signature match
    validate_signature(&rust_func.sig, &vo_sig)?;
    
    // 7. Generate wrapper code
    generate_wrapper(&rust_func, &vo_sig)
}
```

## Advantages

1. **Type safety**: Compile-time validation, signature mismatches fail immediately
2. **Zero overhead**: Macro expands to direct slot access
3. **Path consistency**: Uses the same resolution logic as Vo imports
4. **User friendly**: Write ordinary Rust functions, no manual slot handling
