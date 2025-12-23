# MM Phase 3: Runtime Changes

**Parent**: [2025-12-22-mm-memory-model-plan.md](2025-12-22-mm-memory-model-plan.md)  
**Status**: Not Started  
**Est. Lines**: ~300  
**Depends On**: P1 (Escape Analysis)

## Overview

Update runtime to support:
1. Boxed primitives (BoxedInt, BoxedFloat, BoxedBool)
2. Updated GC scanning for stack/heap distinction

## Modifications

### 3.1 vo-common-core/src/types.rs

Add new ValueKind variants:

```rust
#[repr(u8)]
pub enum ValueKind {
    // ... existing variants ...
    
    // Boxed primitives (escaped)
    BoxedInt = 20,
    BoxedFloat = 21,
    BoxedBool = 22,
}
```

Update helper functions:

```rust
pub fn is_heap_type(value_kind: ValueKind) -> bool {
    matches!(value_kind,
        ValueKind::String | ValueKind::Slice | ValueKind::Map |
        ValueKind::Array | ValueKind::Channel | ValueKind::Closure |
        ValueKind::Struct | ValueKind::BoxedInt | ValueKind::BoxedFloat |
        ValueKind::BoxedBool)
}
```

### 3.2 vo-runtime-core/src/gc.rs

GcHeader already has `slots` field in design. Verify implementation:

```rust
#[repr(C)]
pub struct GcHeader {
    pub mark: u8,
    pub gen: u8,
    pub value_kind: u8,
    pub flags: u8,
    pub type_id: u16,
    pub slots: u16,  // Ensure this exists
}
```

### 3.3 vo-runtime-core/src/gc_types.rs

Update `scan_object` to handle boxed types:

```rust
pub fn scan_object(gc: &mut Gc, obj: GcRef) {
    let value_kind = ValueKind::from_u8(header.value_kind);
    
    match value_kind {
        // ... existing cases ...
        
        // Boxed primitives: no internal references
        ValueKind::BoxedInt | ValueKind::BoxedFloat | ValueKind::BoxedBool => {
            // Nothing to scan
        }
    }
}
```

### 3.4 vo-vm/src/vm.rs

May need updates for boxed value instructions:

```rust
Opcode::Alloc => {
    // Handle BoxedInt/Float/Bool allocation
    // Same as existing struct allocation, just different value_kind
}
```

## Boxed Value Layout

All boxed primitives have the same simple layout:

```
┌────────────┬────────────┐
│  GcHeader  │   value    │
│  8 bytes   │  8 bytes   │
└────────────┴────────────┘
```

- `slots = 1`
- `type_id = 0` (not used for boxed primitives)
- GC scanning: skip (no internal references)

## Tasks Checklist

### vo-common-core/src/types.rs
- [ ] Add `ValueKind::BoxedInt`
- [ ] Add `ValueKind::BoxedFloat`
- [ ] Add `ValueKind::BoxedBool`
- [ ] Add `is_heap_type()` function
- [ ] Update `needs_gc()` if needed

### vo-runtime-core/src/gc.rs
- [ ] Verify GcHeader has `slots` field
- [ ] Update `alloc()` signature if needed

### vo-runtime-core/src/gc_types.rs
- [ ] Add boxed types to `scan_object`

### vo-vm/src/vm.rs
- [ ] Handle boxed value allocation (if different from struct)

## Testing

```vo
func test_boxed_int() {
    x := 42
    f := func() int { return x }  // x escapes → BoxedInt
    assert(f() == 42)
}

func test_boxed_float() {
    x := 3.14
    f := func() float64 { return x }  // x escapes → BoxedFloat
    assert(f() == 3.14)
}
```
