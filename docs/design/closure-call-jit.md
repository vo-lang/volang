# GoX JIT ClosureCall Design

## Overview

This document describes the implementation of closure calls (ClosureCall) in the GoX JIT compiler. The design is inspired by Go 1.1+ and uses a function pointer table for indirect calls.

## Background

### Problem

`ClosureCall` needs to call functions dynamically based on closure objects at runtime:

```
Opcode::ClosureCall
  a = closure_reg   // closure object
  b = arg_start     // argument start register
  c = arg_count     // number of arguments
  flags = ret_count // number of return values
```

The closure stores `func_id`, but Cranelift's `call_indirect` requires a function pointer.

### Go 1.1+ Approach

Go uses a two-word structure for func values:

```
func_value → [code_ptr | context_data...]
```

The `code_ptr` is obtained by dereferencing, and context is passed via a register (e.g., R0).

## GoX Design

### Data Structures

```
┌─────────────────────────────────────────────────────────────┐
│                  Closure Object (GC heap)                    │
├─────────────────────────────────────────────────────────────┤
│  func_id (u64)  │  upval_count  │  upval[0]  │  upval[1]   │
└────────┬────────┴───────────────┴────────────┴─────────────┘
         │ slot 0
         │ inline load
         ▼
┌─────────────────────────────────────────────────────────────┐
│           Function Pointer Table GOX_FUNC_TABLE (global)     │
├─────────────────────────────────────────────────────────────┤
│  [0] ptr_0  │  [1] ptr_1  │  [2] ptr_2  │  ...  │ [N-1]    │
└─────────────┴─────────────┴─────────────┴───────┴──────────┘
         │
         │ call_indirect
         ▼
┌─────────────────────────────────────────────────────────────┐
│                    JIT Compiled Function Code                │
└─────────────────────────────────────────────────────────────┘
```

### Comparison with Go

| Aspect | Go 1.1+ | GoX |
|--------|---------|-----|
| func value structure | `*{code_ptr, data...}` | `GcRef{func_id, upvals...}` |
| code pointer storage | distributed in func values | centralized in global table |
| lookup overhead | 1 dereference | 2 inline loads |
| context passing | register (R0) | first function argument |
| VM compatibility | N/A | ✅ shared closure structure |

### Memory Overhead

- **Function table**: N × 8 bytes (N = number of functions)
- **Per closure**: 4 bytes less than Go (u32 func_id vs u64 code_ptr)

Total memory is comparable to Go, potentially better in some scenarios.

## Implementation Details

### 1. Function Pointer Table (gc_global.rs)

```rust
// Global function pointer table
static mut FUNC_TABLE: *mut *const u8 = std::ptr::null_mut();
static mut FUNC_TABLE_LEN: usize = 0;

/// Initialize function table
pub fn init_func_table(size: usize) {
    unsafe {
        let table = vec![std::ptr::null(); size].into_boxed_slice();
        FUNC_TABLE_LEN = size;
        FUNC_TABLE = Box::leak(table).as_mut_ptr();
    }
}

/// Set function pointer
pub fn set_func_ptr(func_id: u32, ptr: *const u8) {
    unsafe {
        debug_assert!((func_id as usize) < FUNC_TABLE_LEN);
        *FUNC_TABLE.add(func_id as usize) = ptr;
    }
}

/// Get function table pointer (for JIT symbol registration)
#[no_mangle]
pub extern "C" fn gox_func_table_ptr() -> *const *const u8 {
    unsafe { FUNC_TABLE }
}
```

### 2. JIT Initialization (jit.rs)

```rust
pub fn compile_module(&mut self, bytecode: &BytecodeModule) -> Result<()> {
    // Initialize function table
    gox_runtime_native::init_func_table(bytecode.functions.len());
    
    // ... compile all functions ...
    
    // Populate function table
    self.module.finalize_definitions()?;
    for (idx, _func_def) in bytecode.functions.iter().enumerate() {
        let func_id = compile_ctx.get_gox_func(idx as u32).unwrap();
        let code_ptr = self.module.get_finalized_function(func_id);
        gox_runtime_native::set_func_ptr(idx as u32, code_ptr);
    }
    
    Ok(())
}
```

### 3. ClosureCall Translation (translate.rs)

```rust
Opcode::ClosureCall => {
    let closure = builder.use_var(self.variables[inst.a as usize]);
    let arg_start = inst.b;
    let arg_count = inst.c as usize;
    let ret_count = inst.flags as usize;
    
    // 1. Get func_id (inline load from closure slot 0, ~3 cycles)
    //    Closure layout: [func_id | upval_count | upval[0] | ...]
    let func_id = builder.ins().load(I64, MemFlags::trusted(), closure, 0);
    
    // 2. Get code pointer from function table (inline load, ~5 cycles)
    let table_id = ctx.get_or_declare_func_table(module)?;
    let gv = module.declare_data_in_func(table_id, builder.func);
    let table_ptr = builder.ins().global_value(I64, gv);
    let offset = builder.ins().imul_imm(func_id, 8);
    let addr = builder.ins().iadd(table_ptr, offset);
    let func_ptr = builder.ins().load(I64, MemFlags::trusted(), addr, 0);
    
    // 3. Prepare arguments (closure as implicit first argument)
    let mut args = Vec::with_capacity(arg_count + 1);
    args.push(closure);  // context
    for i in 0..arg_count {
        args.push(builder.use_var(self.variables[(arg_start as usize) + i]));
    }
    
    // 4. Create signature and indirect call
    let sig = self.create_closure_sig(arg_count + 1, ret_count);
    let sig_ref = builder.import_signature(sig);
    let call = builder.ins().call_indirect(sig_ref, func_ptr, &args);
    
    // 5. Store return values
    let results: Vec<_> = builder.inst_results(call).to_vec();
    for (i, result) in results.into_iter().enumerate() {
        builder.def_var(self.variables[(arg_start as usize) + i], result);
    }
}
```

### 4. Accessing Upvalues in Called Function

Closure functions receive `closure` as the first argument to access upvalues:

```rust
// Inside closure function
Opcode::ClosureGet => {
    // closure is the first parameter
    let closure = builder.use_var(self.variables[0]);
    let idx = inst.b;
    let func_ref = self.get_runtime_func_ref(builder, module, ctx, RuntimeFunc::ClosureGetUpvalue)?;
    let idx_val = builder.ins().iconst(I64, idx as i64);
    let call = builder.ins().call(func_ref, &[closure, idx_val]);
    let result = builder.inst_results(call)[0];
    builder.def_var(self.variables[inst.a as usize], result);
}
```

## Call Flow

```
ClosureCall(closure, args...)
    │
    ├─1─► load closure[0] → func_id        (inline, ~3 cycles)
    │
    ├─2─► load FUNC_TABLE[func_id] → code_ptr  (inline, ~5 cycles)
    │
    ├─3─► call_indirect(code_ptr, closure, args...)  (~5 cycles)
    │
    └─4─► callee accesses upvalues via closure argument
```

## Performance Analysis

| Operation | Overhead |
|-----------|----------|
| Get func_id | ~3 cycles (inline load) |
| Table lookup | ~5 cycles (inline load) |
| Indirect call | ~5 cycles |
| **Total** | **~13 cycles** |

Compared to Go's ~10 cycles, the extra ~3 cycles come from the additional table lookup. This is acceptable overhead.

## TODO

- [ ] Implement `init_func_table` and `set_func_ptr` in gc_global.rs
- [ ] Register `GOX_FUNC_TABLE` symbol in JIT builder
- [ ] Populate function table after JIT compilation
- [ ] Implement ClosureCall translation with `call_indirect`
- [ ] Modify closure function compilation to receive closure as first arg
- [ ] Add test cases
