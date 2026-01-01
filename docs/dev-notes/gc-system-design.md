# Vo GC System Design

## Overview

Two-phase implementation based on Lua 5.4 GC concepts:
- **Phase 1**: Incremental GC (tri-color mark-sweep with debt-based triggering)
- **Phase 2**: Generational GC (minor/major collection on top of Phase 1)

Phase 1 data structures fully support Phase 2 - no breaking changes needed.

---

## Data Structures

### GcHeader (8 bytes) ✅ IMPLEMENTED

```rust
/// Layout: [marked:8 | reserved:8 | slots:16 | ValueMeta:32]
///
/// marked field bit layout (Lua style):
///   bit 0-2: age (for generational GC)
///   bit 3: WHITE0
///   bit 4: WHITE1  
///   bit 5: BLACK
///   bit 6-7: reserved
#[repr(C)]
pub struct GcHeader {
    pub marked: u8,
    pub _reserved: u8,
    pub slots: u16,
    pub value_meta: ValueMeta,
}

// Color bits
const WHITE0: u8 = 1 << 3;
const WHITE1: u8 = 1 << 4;
const BLACK: u8 = 1 << 5;
const WHITEBITS: u8 = WHITE0 | WHITE1;

// Age values (Phase 2)
const AGE_MASK: u8 = 0x07;
const G_YOUNG: u8 = 0;
const G_SURVIVAL: u8 = 1;
const G_OLD: u8 = 2;
const G_TOUCHED: u8 = 3;
```

**Why dual-white?** Prevents newly allocated objects from being swept during incremental collection. Objects allocated during sweep get the "other" white, safe from current sweep.

### GcState

```rust
#[repr(u8)]
pub enum GcState {
    Pause = 0,       // Idle, waiting for trigger
    Propagate = 1,   // Incremental marking (interruptible)
    Atomic = 2,      // Atomic marking (not interruptible)
    Sweep = 3,       // Sweeping
}
```

### GcMode

```rust
#[repr(u8)]
pub enum GcMode {
    Incremental = 0,   // Phase 1: full heap each cycle
    Generational = 1,  // Phase 2: minor + major
}
```

### Gc Structure

```rust
pub struct Gc {
    // ========== Object Storage ==========
    all_objects: Vec<GcObjectEntry>,
    
    // ========== Mark Queues ==========
    gray: Vec<GcRef>,        // To be scanned
    grayagain: Vec<GcRef>,   // Re-scan (barrier triggered)
    
    // ========== State ==========
    state: GcState,
    mode: GcMode,
    current_white: u8,       // Current white (WHITE0 or WHITE1)
    
    // ========== Memory Stats ==========
    total_bytes: usize,
    estimate: usize,         // Estimated live memory
    debt: isize,             // Debt (trigger mechanism)
    
    // ========== Parameters ==========
    pause: u16,              // Trigger threshold %, default 200
    stepmul: u16,            // Work per step, default 100
    stepsize: usize,         // Bytes per step, default 8KB
    
    // ========== Phase 2 Extensions (defined but unused in Phase 1) ==========
    minor_mul: u16,          // Minor GC multiplier
    major_mul: u16,          // Major GC multiplier
    survival: Vec<GcRef>,    // Survived one cycle
    old: Vec<GcRef>,         // Old objects
    touched: Vec<GcRef>,     // Modified old objects
}
```

---

## Phase 1: Incremental GC

### State Machine

```
                    ┌─────────────────────────────────────┐
                    │              GC Cycle               │
                    └─────────────────────────────────────┘
                                     │
     ┌───────────────────────────────┼───────────────────────────────┐
     ▼                               ▼                               ▼
┌─────────┐    debt > 0         ┌─────────┐    gray empty       ┌─────────┐
│  Pause  │ ──────────────────> │Propagate│ ──────────────────> │ Atomic  │
└─────────┘                     └─────────┘                     └─────────┘
     ▲                               │                               │
     │                        (incremental:                          │
     │                         do one step, return)                  ▼
     │                               │                          ┌─────────┐
     └───────────────────────── sweep done <─────────────────── │  Sweep  │
                                                                └─────────┘
```

### Debt-Based Triggering

```rust
fn check_gc(&mut self) {
    self.debt += self.stepsize as isize;
    if self.debt > 0 {
        self.step();
    }
}
```

- Allocation increases debt
- GC work decreases debt
- debt > 0 triggers a step

### Write Barrier

```rust
fn barrier(&mut self, parent: GcRef, child: GcRef) {
    // Backward barrier: black parent writes white child → parent turns gray
    if self.state == GcState::Propagate 
       && self.is_black(parent) 
       && self.is_white(child) {
        self.barrier_back(parent);
    }
}

fn barrier_back(&mut self, obj: GcRef) {
    // Turn black → white, add to grayagain
    Self::header_mut(obj).marked = 
        (Self::header(obj).marked & !BLACK) | self.current_white;
    self.grayagain.push(obj);
}
```

### Cycle Transition

```rust
fn new_cycle(&mut self) {
    // Flip white color
    self.current_white ^= WHITEBITS;
    self.state = GcState::Propagate;
}
```

---

## Phase 2: Generational GC

### Additional Logic

```rust
fn step_generational(&mut self) {
    if self.need_major_gc() {
        self.major_gc();  // Full heap scan
    } else {
        self.minor_gc();  // Young + touched only
    }
}

fn minor_gc(&mut self) {
    // Only mark young + touched
    self.scan_roots_minor();
    
    while let Some(obj) = self.gray.pop() {
        let age = Self::header(obj).age & AGE_MASK;
        if age == G_YOUNG || age == G_TOUCHED {
            self.propagate_mark(obj);
        }
    }
    
    self.sweep_young();
    self.age_objects();  // Promote survivors
}

fn age_objects(&mut self) {
    // young survived → survival
    // survival survived → old
}

fn barrier_generational(&mut self, parent: GcRef, child: GcRef) {
    let p_age = Self::header(parent).age & AGE_MASK;
    let c_age = Self::header(child).age & AGE_MASK;
    
    // old → young: mark old as touched
    if p_age >= G_OLD && c_age == G_YOUNG {
        Self::header_mut(parent).age = G_TOUCHED;
        self.touched.push(parent);
    }
}
```

---

## GC Roots

All sources that may hold GcRef:

| Source | Location | Status |
|--------|----------|--------|
| globals[] | VmState | ✅ Implemented |
| fiber.stack[] | Fiber | ✅ Implemented |
| fiber.defer_stack[] | Fiber | ✅ Implemented |
| fiber.defer_state.pending[] | Fiber | ✅ Implemented |
| fiber.defer_state.ret_vals[] | Fiber | ❌ Missing |
| fiber.panic_value | Fiber | ✅ Implemented |
| scheduler.trampoline_fibers[] | Scheduler | ❌ Missing |

### Fix Required

```rust
// gc_roots.rs
impl Vm {
    pub fn scan_roots(&mut self) {
        let module = self.module.as_ref().unwrap();
        scan_globals(&mut self.state.gc, &self.state.globals, &module.globals);
        scan_fibers(&mut self.state.gc, &self.scheduler.fibers, &module.functions);
        scan_fibers(&mut self.state.gc, &self.scheduler.trampoline_fibers, &module.functions);  // ADD
    }
}
```

---

## VM/JIT Integration

### Trigger Points

```rust
// jit_api.rs
#[no_mangle]
pub extern "C" fn vo_gc_alloc(ctx: *mut JitContext, meta: u32, slots: u32) -> u64;

#[no_mangle]
pub extern "C" fn vo_gc_safepoint(ctx: *mut JitContext);

#[no_mangle]
pub extern "C" fn vo_gc_write_barrier(ctx: *mut JitContext, parent: u64, child: u64);
```

### VM Wrapper

```rust
impl Vm {
    fn gc_alloc(&mut self, meta: ValueMeta, slots: u16) -> GcRef {
        self.gc_step_if_needed();
        self.state.gc.alloc(meta, slots)
    }
    
    fn gc_step_if_needed(&mut self) {
        if self.state.gc.debt > 0 {
            self.scan_roots();
            self.state.gc.step();
        }
    }
}
```

---

## File Structure

```
vo-runtime/src/
├── gc.rs              # Gc struct + core algorithm
├── gc_types.rs        # scan_object, finalize_object
└── jit_api.rs         # vo_gc_* FFI

vo-vm/src/
├── gc_roots.rs        # scan_roots, scan_fibers
└── vm/mod.rs          # gc_alloc, gc_check wrappers
```

---

## Implementation Plan

### Phase 1 Tasks

| # | Task | File | Status |
|---|------|------|--------|
| 1 | Modify GcHeader (marked bit layout) | gc.rs | ✅ |
| 2 | Add GcState | gc.rs | ✅ |
| 3 | Add new Gc fields (debt, current_white, grayagain, etc.) | gc.rs | ✅ |
| 4 | Implement step() state machine | gc.rs | ✅ |
| 5 | Implement barrier() (backward barrier) | gc.rs | ✅ |
| 6 | Fix scan_roots (add trampoline_fibers) | gc_roots.rs | ✅ |
| 7 | Implement jit_api functions (vo_gc_write_barrier) | jit_api.rs | ✅ |
| 8 | VM integration (gc_alloc, gc_check) | vm/mod.rs | Pending |

### Phase 2 Tasks

| # | Task | File |
|---|------|------|
| 1 | Enable age field | gc.rs |
| 2 | Implement minor_gc, major_gc | gc.rs |
| 3 | Implement age_objects | gc.rs |
| 4 | Extend barrier for generational | gc.rs |
| 5 | Add scan_roots_minor | gc_roots.rs |

---

---

## Existing Code: Required Changes

### 1. gc_roots.rs - Missing Root Scans

**File**: `vo-vm/src/gc_roots.rs`

```rust
// CURRENT (line 18):
scan_fibers(&mut self.state.gc, &self.scheduler.fibers, ...);

// MISSING:
scan_fibers(&mut self.state.gc, &self.scheduler.trampoline_fibers, ...);
```

**Also missing**: `defer_state.ret_vals` scanning (may contain GcRef during defer execution).

### 2. gc.rs - Structure Changes

**File**: `vo-runtime/src/gc.rs`

| Current | New | Change |
|---------|-----|--------|
| `mark: u8` (simple enum) | `marked: u8` (bit field) | Rename + reinterpret |
| `gen: u8` | `age: u8` | Rename (same purpose) |
| `gray_queue` | `gray` + `grayagain` | Split into two |
| `threshold` | `debt` + `estimate` | Replace trigger mechanism |
| - | `state: GcState` | Add |
| - | `mode: GcMode` | Add |
| - | `current_white: u8` | Add |
| - | `pause, stepmul, stepsize` | Add parameters |

### 3. jit_api.rs - Implement Empty Functions

**File**: `vo-runtime/src/jit_api.rs`

```rust
// Line 175-179: EMPTY - needs implementation
pub extern "C" fn vo_gc_write_barrier(_gc: *mut Gc, _obj: u64, _offset: u32, _val: u64) {
    // TODO: Implement
}

// Line 192-198: EMPTY - needs implementation  
pub extern "C" fn vo_gc_safepoint(_ctx: *mut JitContext) {
    // TODO: Implement
}
```

### 4. VM Integration - Add GC Trigger

**File**: `vo-vm/src/vm/mod.rs`

Need to add:
```rust
impl Vm {
    /// Check and run GC step if needed
    fn gc_step_if_needed(&mut self) {
        if self.state.gc.debt > 0 {
            self.scan_roots();
            self.state.gc.step();
        }
    }
    
    /// Allocation wrapper with GC check
    pub fn gc_alloc(&mut self, meta: ValueMeta, slots: u16) -> GcRef {
        self.gc_step_if_needed();
        self.state.gc.alloc(meta, slots)
    }
}
```

### 5. Write Barrier Call Sites

Need to add `barrier()` calls at all heap write locations:

| Instruction | File | Action |
|-------------|------|--------|
| `PtrSet` | vm/mod.rs, jit translate.rs | Add barrier call |
| `ArraySet` | exec/array.rs, jit translate.rs | Add barrier if elem is GcRef |
| `MapSet` | exec/map.rs, jit_api.rs | Add barrier |
| `SliceSet` | exec/slice.rs | Add barrier if elem is GcRef |
| `ClosureCapture` | exec/closure.rs | Add barrier |

### 6. JitContext Changes

**File**: `vo-runtime/src/jit_api.rs`

```rust
// Current JitContext
pub struct JitContext {
    pub gc: *mut Gc,
    pub globals: *mut u64,
    pub safepoint_flag: *mut bool,
    // ...
}

// May need to add:
pub struct JitContext {
    // ...
    pub vm: *mut Vm,  // For calling scan_roots during GC
}
```

---

## Migration Strategy

### Step 1: Fix Root Scanning (No GC logic change)
1. Add `trampoline_fibers` to `scan_roots`
2. Add `defer_state.ret_vals` scanning
3. Test: existing tests should pass

### Step 2: Refactor Gc Structure
1. Rename fields (`mark` → `marked`, `gen` → `age`)
2. Add new fields (`state`, `debt`, `current_white`, etc.)
3. Keep old `collect()` working temporarily
4. Test: existing tests should pass

### Step 3: Implement Incremental Logic
1. Implement `step()` state machine
2. Implement `barrier()`
3. Replace `should_collect()` + `collect()` with debt-based triggering
4. Test: new GC tests

### Step 4: Integrate with VM/JIT
1. Implement `vo_gc_safepoint`
2. Implement `vo_gc_write_barrier`
3. Add barrier calls to VM instructions
4. Add barrier calls to JIT translate.rs
5. Test: full test suite

---

## References

- Lua 5.4 GC: https://www.lua.org/source/5.4/lgc.h.html
- Lua GC talk: https://www.lua.org/wshop18/Ierusalimschy.pdf
