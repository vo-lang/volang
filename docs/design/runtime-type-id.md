# RuntimeTypeId Design

## 1. ID Range Allocation

```
┌─────────────────────────────────────────────────────┐
│ 0-13      │ Basic value types (Nil, Bool, Int...)   │
│ 14-22     │ Built-in ref types (String, Slice...)   │
│ 100+      │ User-defined structs (FirstStruct)      │
│ 2^31+     │ User-defined interfaces (FirstInterface)│
└─────────────────────────────────────────────────────┘
```

## 2. Struct Type ID Allocation

- **Each struct definition** gets a unique ID (no deduplication)
- Named struct (`type MyStruct struct{...}`) → `FirstStruct + idx`
- Anonymous struct (`struct{x int}`) → `FirstStruct + idx`
- Each definition registers a `TypeMeta` (including `slot_types` for GC scanning)

## 3. Interface Type ID Allocation

- **Deduplicated by method set**
- Interfaces with identical method sets share the same ID
- Named interface → `FirstInterface + idx`
- Anonymous interface → lookup existing method set or allocate new ID

## 4. Codegen Flow

```
Pass 1: Collect Types
├── Traverse all Type::Struct in AST → allocate FirstStruct + struct_idx
├── Traverse all Type::Interface in AST → dedupe by method set, allocate FirstInterface + iface_idx
└── Register to module.types

Pass 2: Generate Code
├── Opcode::Alloc uses the allocated type_id
└── Interface variable slot[0] stores the actual value's type ID
```

## 5. Data Structures

**GlobalDef**:
```rust
pub struct GlobalDef {
    pub name: String,
    pub slots: u16,
    pub type_id: u32,  // RuntimeTypeId value
}
```

**TypeMeta**:
```rust
pub struct TypeMeta {
    pub type_id: u32,
    pub size_slots: usize,
    pub slot_types: Vec<SlotType>,  // For GC scanning
    // ...
}
```

## 6. GC Scanning

**Global Variables**:
- Interface → 2 slots, dynamically check slot[1]
- Other GC types → mark slot[0]

**Heap Objects**:
- Use object header's `type_id` to lookup `slot_types`
- Precisely scan fields according to `slot_types`
