# JIT Manager Refactor Plan

## Overview

Refactor JIT code to create a unified `JitManager` that manages all JIT compilation states, replacing the current scattered implementation across `vm.rs`.

## Current Problems

### 1. Code Scattered Across vm.rs (~600 lines)

| Location | Content |
|----------|---------|
| 114-118 | JIT imports |
| 121-220 | trampolines (itab, extern, vm_call) |
| 348-354 | Vm fields (jit, jit_func_table) |
| 389-412 | init_jit, has_jit |
| 416-598 | try_jit_call, try_osr |
| 602-647 | precompile_callees |
| 665-668 | func_table init in load() |
| 1042-1156 | Jump/JumpIf/JumpIfNot/Call dual versions |
| 1630+ | exec_inst_inline |

### 2. Duplicated Code

- `Jump`, `JumpIf`, `JumpIfNot`, `Call` each have two versions (`#[cfg(feature = "jit")]` and `#[cfg(not(feature = "jit"))]`)

### 3. Mixed Concerns

- Hot counting logic mixed with execution
- Compilation decisions scattered
- Version management ad-hoc

---

## Design: JIT State Machine

### Core Principle

Each function has multiple possible execution versions:

```
Priority: full_jit_entry > osr_entries > vm_interpreter
```

When a higher-priority version exists, lower-priority versions are not used.

### Function States

```rust
pub enum CompileState {
    /// Not compiled, use VM interpreter
    Interpreted,
    /// Has OSR version(s), but no full function version
    PartialCompiled,
    /// Has full function JIT version
    FullyCompiled,
    /// Cannot JIT (unsupported features), never retry
    Unsupported,
}
```

### State Transitions

```
                    ┌──────────────────────────────────────────────┐
                    │                                              │
                    ▼                                              │
            ┌──────────────┐                                       │
            │ Interpreted  │                                       │
            └──────────────┘                                       │
                    │                                              │
        ┌───────────┴───────────┐                                  │
        │ loop hot              │ call hot                         │
        ▼                       ▼                                  │
┌──────────────────┐    ┌──────────────┐                           │
│ PartialCompiled  │    │              │                           │
│ (has OSR)        │────│FullyCompiled │◄──────────────────────────┘
└──────────────────┘    │ (has full)   │     OSR complete triggers
        │               └──────────────┘     full compilation
        │ call hot / OSR complete
        └────────────────────────┘

            ┌──────────────┐
            │ Unsupported  │  (can_jit failed, terminal state)
            └──────────────┘
```

### Key Insight: OSR Completion Triggers Full Compilation

When OSR version executes successfully and returns, it proves the function is hot. We should immediately compile the full function version so next call uses JIT from the start.

---

## Data Structures

### FunctionJitInfo

```rust
pub struct FunctionJitInfo {
    /// Compilation state
    pub state: CompileState,
    
    /// Full function JIT entry (entry_pc=0)
    pub full_entry: Option<JitFuncPtr>,
    
    /// OSR entries: loop_header_pc -> function pointer
    pub osr_entries: HashMap<usize, JitFuncPtr>,
    
    /// Call count (for triggering full compilation)
    pub call_count: u32,
    
    /// Backedge counts: backedge_pc -> count (for triggering OSR)
    pub backedge_counts: HashMap<usize, u32>,
}
```

### JitManager

```rust
pub struct JitManager {
    /// Per-function JIT info
    funcs: Vec<FunctionJitInfo>,
    
    /// Fast dispatch table: func_id -> full_entry pointer (null = use VM)
    func_table: Vec<*const u8>,
    
    /// Cranelift compiler
    compiler: JitCompiler,
    
    /// Configuration
    config: JitConfig,
}

pub struct JitConfig {
    /// Call count threshold for full compilation
    pub call_threshold: u32,  // default: 1000
    /// Backedge count threshold for OSR
    pub loop_threshold: u32,  // default: 100
}
```

---

## API Design

### JitManager Public API

```rust
impl JitManager {
    /// Initialize (called on module load)
    pub fn init(&mut self, func_count: usize);
    
    /// Get function entry for dispatch (O(1))
    #[inline]
    pub fn get_entry(&self, func_id: u32) -> Option<JitFuncPtr>;
    
    /// Get OSR entry (only called during VM execution)
    pub fn get_osr_entry(&self, func_id: u32, loop_header_pc: usize) -> Option<JitFuncPtr>;
    
    /// Record function call, returns true if should compile full
    pub fn record_call(&mut self, func_id: u32) -> bool;
    
    /// Record backedge, returns Some(loop_header_pc) if should compile OSR
    pub fn record_backedge(&mut self, func_id: u32, backedge_pc: usize, loop_header_pc: usize) -> Option<usize>;
    
    /// Compile full function
    pub fn compile_full(&mut self, func_id: u32, func_def: &FunctionDef, module: &Module) -> Result<(), JitError>;
    
    /// Compile OSR version
    pub fn compile_osr(&mut self, func_id: u32, loop_header_pc: usize, func_def: &FunctionDef, module: &Module) -> Result<JitFuncPtr, JitError>;
    
    /// Called when OSR execution completes, triggers full compilation
    pub fn on_osr_complete(&mut self, func_id: u32, func_def: &FunctionDef, module: &Module);
    
    /// Get raw func_table pointer for JIT code
    pub fn func_table_ptr(&self) -> *const *const u8;
}
```

---

## VM Integration

### Simplified VM Code

```rust
impl Vm {
    /// Function call dispatch
    fn dispatch_call(&mut self, func_id: u32, args: &[u64]) -> ExecResult {
        // 1. Try JIT
        if let Some(ptr) = self.jit_mgr.get_entry(func_id) {
            return self.call_jit(ptr, args);
        }
        
        // 2. Record call, check if should compile
        if self.jit_mgr.record_call(func_id) {
            let module = self.module.as_ref().unwrap();
            let func_def = &module.functions[func_id as usize];
            let _ = self.jit_mgr.compile_full(func_id, func_def, module);
            
            // If compiled, use JIT
            if let Some(ptr) = self.jit_mgr.get_entry(func_id) {
                return self.call_jit(ptr, args);
            }
        }
        
        // 3. VM interpret
        self.interpret(func_id, args)
    }
    
    /// Backedge handling (during VM loop execution)
    fn on_backedge(&mut self, func_id: u32, backedge_pc: usize, loop_header_pc: usize, bp: usize) -> Option<ExecResult> {
        // 1. Check existing OSR
        if let Some(ptr) = self.jit_mgr.get_osr_entry(func_id, loop_header_pc) {
            return self.execute_osr(func_id, ptr, bp);
        }
        
        // 2. Record backedge, check if should compile OSR
        if let Some(entry_pc) = self.jit_mgr.record_backedge(func_id, backedge_pc, loop_header_pc) {
            let module = self.module.as_ref().unwrap();
            let func_def = &module.functions[func_id as usize];
            
            if let Ok(ptr) = self.jit_mgr.compile_osr(func_id, entry_pc, func_def, module) {
                return self.execute_osr(func_id, ptr, bp);
            }
        }
        
        // 3. Continue VM
        None
    }
    
    /// Execute OSR version
    fn execute_osr(&mut self, func_id: u32, ptr: JitFuncPtr, bp: usize) -> Option<ExecResult> {
        let result = self.call_osr_jit(ptr, bp);
        
        // On success, trigger full compilation
        if result.is_ok() {
            let module = self.module.as_ref().unwrap();
            let func_def = &module.functions[func_id as usize];
            self.jit_mgr.on_osr_complete(func_id, func_def, module);
        }
        
        Some(result)
    }
}
```

---

## File Structure After Refactor

```
vo-jit/src/
├── lib.rs              # Exports
├── manager.rs          # JitManager (NEW - core state machine)
├── compiler.rs         # JitCompiler (existing, minor changes)
├── translate.rs        # IR translation (existing)
└── ...

vo-vm/src/
├── vm.rs               # VM main loop, uses JitManager
├── hot_counter.rs      # DELETE (merged into JitManager)
└── ...
```

---

## Refactor Steps

### Phase 1: Create JitManager

1. Create `vo-jit/src/manager.rs` with `JitManager`, `FunctionJitInfo`, `JitConfig`
2. Implement all public APIs
3. Move `hot_counter` logic into `JitManager`
4. Export from `vo-jit/src/lib.rs`

### Phase 2: Integrate with VM

1. Replace `Vm.jit` + `Vm.jit_func_table` + `Vm.hot_counter` with `Vm.jit_mgr: Option<JitManager>`
2. Update `Vm::init_jit()` to create `JitManager`
3. Update `Vm::load()` to call `jit_mgr.init()`
4. Simplify `dispatch_call` and `on_backedge`

### Phase 3: Simplify VM Opcodes

1. Remove duplicate `#[cfg(feature = "jit")]` blocks for Jump/JumpIf/JumpIfNot/Call
2. Keep single version that calls `on_backedge()` for backward jumps
3. Keep single version of Call that uses dispatch logic

### Phase 4: Cleanup

1. Delete `vo-vm/src/hot_counter.rs`
2. Delete `precompile_callees` (not needed)
3. Move trampolines to appropriate location
4. Update tests

---

## Design Decisions

### 1. No Callee Pre-compilation

**Reason**: Each function compiles when it becomes hot. Pre-compiling callees adds complexity and may compile cold functions.

### 2. OSR Complete → Full Compile

**Reason**: OSR success proves function is hot. Next call should use JIT from start, not re-enter VM and re-trigger OSR.

### 3. Independent Backedge Counting

**Reason**: A function may have multiple loops. Each loop's backedge is counted independently to trigger OSR at the right loop.

### 4. Full Version Invalidates OSR

**Reason**: Once full version exists, all calls use JIT from start. OSR paths are never reached.

### 5. Synchronous Compilation

**Reason**: Keep simple. Async compilation can be added later if needed.

---

## Default Configuration

```rust
JitConfig {
    call_threshold: 1000,   // 1000 calls → compile full
    loop_threshold: 100,    // 100 backedges → compile OSR
}
```

Note: `call_threshold > loop_threshold` so hot loops get OSR first, then full compilation happens via OSR completion or enough calls.

---

## Testing Plan

1. Run full JIT test suite after each phase
2. Verify OSR triggers correctly
3. Verify full compilation after OSR complete
4. Verify benchmarks performance maintained or improved
5. Verify `#[cfg(not(feature = "jit"))]` still builds and works
