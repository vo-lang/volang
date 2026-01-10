# Dynamic Access Protocol Redesign

## Status: In Progress

## Summary

Redesign the `~>` operator to support **protocol-first dispatch** with language-level protocol methods (`DynAttr`, `DynSetAttr`, etc.). This enables concrete types (especially those using embedding) to naturally support dynamic access without requiring explicit `any` conversion.

## Goals

1. Allow `~>` on concrete types that implement protocol methods
2. Protocol methods take priority over reflection-based access
3. Compile-time signature checking for protocol method names
4. Clean, unified implementation without scattered special cases

## Protocol Methods

| Method | Signature | Triggered By |
|--------|-----------|--------------|
| `DynAttr` | `(name string) (any, error)` | `a~>field` |
| `DynSetAttr` | `(name string, value any) error` | `a~>field = v` |
| `DynIndex` | `(key any) (any, error)` | `a~>[key]` |
| `DynSetIndex` | `(key any, value any) error` | `a~>[key] = v` |
| `DynCall` | `(args ...any) (any, error)` | `a~>(args)` |

## Design Decisions

### Why `DynXxx` naming (not `__xxx`)?

- `DynXxx` is valid Go-style exported name
- No need for `exported()` hack in Analysis
- Clear naming convention with `Dyn` prefix

### Why compile-time signature check?

- Protocol names are reserved; wrong signature = user error
- Early error detection is better
- Matches Go's static typing philosophy

### Why no new opcodes?

- Protocol method calls require invoking user-defined functions
- Extern functions can already do this via `lookup_method` + function call
- Adding opcodes would duplicate function call logic

### Dispatch priority

1. Protocol method (if exists with correct signature)
2. Map key access (if `map[string]V`)
3. Struct field access
4. Method as closure (fallback)
5. Error

## Implementation Plan

### Phase 1: Analysis - Relax type restriction

**File**: `crates/vo-analysis/src/check/expr.rs`

Modify `is_dyn_access_base_type` to:
- Allow all types except `int`, `bool`, `float` (primitive non-indexable types)
- `string` is allowed (index access to characters)

```rust
pub(crate) fn is_dyn_access_base_type(&self, type_key: TypeKey) -> bool {
    let typ = self.otype(type_key);
    if let Some(basic) = typ.try_as_basic() {
        // Only allow string among basic types
        return basic.kind() == BasicKind::String;
    }
    true  // All other types allowed
}
```

### Phase 2: Analysis - Protocol signature check

**File**: `crates/vo-analysis/src/check/decl.rs` (or similar)

When checking method declarations, if name matches `Dyn*` protocol:
- Verify signature matches expected
- Emit compile error if mismatch

### Phase 3: Codegen - Remove fast-path

**File**: `crates/vo-codegen/src/stmt.rs`

Delete:
- Lines 590-638: `SetAttrObject` fast-path with `IfaceAssert`/`CallIface`
- Lines 652-700: `SetIndexObject` fast-path
- Lines 70-80: `try_lookup_pkg_type` function

### Phase 4: Codegen - Box non-interface to any

**File**: `crates/vo-codegen/src/stmt.rs`

When base is not interface type:
1. Use `compile_iface_assign` to box to `any`
2. Then call `dyn_set_attr` extern as usual

### Phase 5-7: Runtime - Protocol-first dispatch

**File**: `crates/vo-runtime/src/stdlib/dynamic.rs`

Modify `dyn_set_attr`:
```rust
fn dyn_set_attr(call: &mut ExternCallContext) -> ExternResult {
    // ... get base_rttid, field_name, value ...
    
    // Protocol-first: check for DynSetAttr
    if let Some((func_id, is_ptr_recv, _)) = call.lookup_method(base_rttid, "DynSetAttr") {
        return invoke_protocol_method(call, func_id, is_ptr_recv, ...);
    }
    
    // Fallback: existing reflection logic
    // ...
}
```

Similarly for `dyn_get_attr` (check `DynAttr`), `dyn_get_index` (check `DynIndex`), `dyn_set_index` (check `DynSetIndex`).

### Phase 8: stdlib/dyn - Rename methods

**File**: `stdlib/dyn/dyn.vo`

Rename all `__xxx` methods to `DynXxx`:
- `__attr` → `DynAttr`
- `__setattr` → `DynSetAttr`
- `__index` → `DynIndex`
- `__setindex` → `DynSetIndex`
- `__call` → `DynCall`

Update interface definitions accordingly.

### Phase 9-10: Tests

Create test files:
- `test_data/dyn_protocol_embed.vo`: Test embed inheriting protocol methods
- `test_data/dyn_protocol_bad_sig.vo`: Test compile error on wrong signature

## Files to Modify

| File | Change |
|------|--------|
| `vo-analysis/src/check/expr.rs` | Relax `is_dyn_access_base_type` |
| `vo-analysis/src/check/decl.rs` | Add protocol signature check |
| `vo-codegen/src/stmt.rs` | Remove fast-path, add boxing |
| `vo-runtime/src/stdlib/dynamic.rs` | Add protocol method lookup |
| `stdlib/dyn/dyn.vo` | Rename `__xxx` to `DynXxx` |

## Files to Delete/Clean

| File | Lines | Content |
|------|-------|---------|
| `vo-codegen/src/stmt.rs` | 590-638 | `SetAttrObject` fast-path |
| `vo-codegen/src/stmt.rs` | 652-700 | `SetIndexObject` fast-path |
| `vo-codegen/src/stmt.rs` | 70-80 | `try_lookup_pkg_type` |

## Testing

```bash
# Full test
./d.py test

# Specific test
cargo run --bin vo -- run test_data/dyn_protocol_embed.vo
```

## Risks

- **Promoted method lookup**: Ensure `lookup_method` finds promoted methods from embedded types
- **Receiver adjustment**: Protocol methods on embedded fields need correct receiver passing

## Timeline

Implementation complete. All features working including embed types.
