# Runtime Type System Design

## 1. Overview

Runtime type information consists of two components:
- **ValueKind** (u8): Distinguishes all types (int, string, struct, interface, slice, etc.)
- **RuntimeTypeId** (u16): Index into meta tables, only meaningful for struct/interface

Only **struct** and **interface** have RuntimeTypeId. Each has its own independent ID space starting from 0, indexing into separate meta tables.

## 2. Type Information Layout

```
┌─────────────────────────────────────────────────────────────┐
│  Runtime Type Info = ValueKind (u8) + RuntimeTypeId (u16)   │
└─────────────────────────────────────────────────────────────┘

ValueKind: Nil, Bool, Int, String, Slice, Map, Struct, Interface, ...
RuntimeTypeId (u16): 
  - For Struct: index into struct_metas[]
  - For Interface: index into interface_metas[]
  - For other types: unused (0)
```

## 3. Meta Tables

Two separate tables, each indexed by RuntimeTypeId starting from 0:

```
struct_metas[0]    → TypeMeta for first struct
struct_metas[1]    → TypeMeta for second struct
...
struct_metas[N-1]  → TypeMeta for Nth struct

interface_metas[0] → TypeMeta for first interface
interface_metas[1] → TypeMeta for second interface
...
interface_metas[M-1] → TypeMeta for Mth interface
```

## 4. Storage Locations

### GcHeader (heap object header)

```rust
#[repr(C)]
pub struct GcHeader {
    pub mark: u8,
    pub gen: u8,
    pub value_kind: u8,    // ValueKind
    pub _pad: u8,
    pub type_id: u16,      // RuntimeTypeId (only for struct, 0 otherwise)
    pub _pad2: u16,
}
```

### GlobalDef (bytecode metadata)

```rust
pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    pub value_kind: u8,    // ValueKind
    pub type_id: u16,      // RuntimeTypeId (for struct/interface)
}
```

### Interface Value (2 slots)

```
slot[0]: Packed type info (64 bits)
┌────────────────────────────────────────────────────────────────┐
│ iface_type_id (16) │ value_type_id (16) │ value_kind (8) │ ... │
└────────────────────────────────────────────────────────────────┘

slot[1]: Data (value or GcRef, depends on value_kind)
```

- **iface_type_id**: The interface's own RuntimeTypeId (indexes interface_metas)
- **value_kind**: The held value's ValueKind
- **value_type_id**: The held value's RuntimeTypeId (only if value_kind is Struct/Interface)

## 5. Codegen Flow

```
Pass 1: Register Types (in CodegenContext)
├── Traverse all types via query.iter_types()
├── Type::Struct → ctx.register_struct_type(type_key) → returns 0, 1, 2, ...
├── Type::Interface → ctx.register_interface_type(type_key) → returns 0, 1, 2, ...
└── Store in struct_type_ids / interface_type_ids HashMaps

Pass 2: Generate Code
├── For struct allocation: Opcode::Alloc with struct's type_id
├── For interface assignment: pack (iface_type_id, value_kind, value_type_id)
└── For globals: store (value_kind, type_id) in GlobalDef
```

## 6. Implementation

**CodegenContext** (`gox-codegen-vm/src/context.rs`):
```rust
pub struct CodegenContext {
    struct_type_ids: HashMap<TypeKey, u16>,
    interface_type_ids: HashMap<TypeKey, u16>,
    next_struct_id: u16,      // starts at 0
    next_interface_id: u16,   // starts at 0
}
```

**TypeMeta** (`gox-vm/src/types.rs`):
```rust
pub struct TypeMeta {
    pub size_slots: usize,
    pub size_bytes: usize,
    pub slot_types: Vec<SlotType>,  // For GC scanning
    pub name: String,
    pub field_layouts: Vec<FieldLayout>,
}
```

**Module** (`gox-vm/src/bytecode.rs`):
```rust
pub struct Module {
    pub struct_metas: Vec<TypeMeta>,
    pub interface_metas: Vec<TypeMeta>,
    pub globals: Vec<GlobalDef>,
    // ...
}
```

## 7. GC Scanning

**Determining if a slot needs GC scanning**:
```rust
fn needs_gc(value_kind: ValueKind) -> bool {
    matches!(value_kind, 
        ValueKind::String | ValueKind::Slice | ValueKind::Map |
        ValueKind::Array | ValueKind::Channel | ValueKind::Closure |
        ValueKind::Struct | ValueKind::Pointer)
}
```

**Global Variables**:
- Check `GlobalDef.value_kind` to determine if slot is GcRef
- Interface: 2 slots, dynamically check `value_kind` in slot[0]

**Heap Objects**:
- Use `GcHeader.value_kind` + `type_id` to lookup `struct_metas[type_id].slot_types`
- Scan fields according to `slot_types`

## 8. Benefits

- **Compact**: RuntimeTypeId is u16 (max 65535 types per category)
- **Independent**: Struct and interface IDs don't interfere
- **Direct indexing**: type_id directly indexes into meta arrays (no offset subtraction)
- **Clear semantics**: ValueKind tells you what the type is, RuntimeTypeId tells you which one
