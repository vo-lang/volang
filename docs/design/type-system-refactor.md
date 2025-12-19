# Analysis & Codegen Type System Refactoring Design

## 1. Problem Analysis

### 1.1 Current Architecture Issues

#### Issue 1: TypeCheckResult Data Structure Not Optimized for Fast Lookups

Current `TypeCheckResult` structure:

```rust
pub struct TypeCheckResult {
    pub scope: Scope,                           // Symbol table
    pub named_types: Vec<NamedTypeInfo>,        // Stored sequentially
    pub expr_types: HashMap<(u32, u32), Type>,  // Indexed by span
}
```

**Problems**:
- `named_types` is a `Vec`, looking up by Symbol requires O(n) linear scan
- `expr_types` uses span as key, which is unstable and cumbersome to query

```rust
// Every lookup is O(n)
pub fn get_named_type_info(&self, sym: Symbol) -> Option<&NamedTypeInfo> {
    for named in &self.result.named_types {
        if named.name == sym {
            return Some(named);
        }
    }
    None
}
```

#### Issue 2: String Concatenation + HashMap Lookup

`method_table` uses `HashMap<String, u32>` with key `"TypeName.MethodName"`:

```rust
// Every method call requires format! + HashMap lookup
let method_key = format!("{}.{}", type_name, method);
if let Some(&func_idx) = ctx.method_table.get(&method_key) { ... }
```

Even worse is the O(n) scan during interface dynamic dispatch:

```rust
// Iterates entire method_table + string ends_with
for (key, &func_idx) in ctx.method_table.iter() {
    if key.ends_with(&format!(".{}", method)) { ... }
}
```

#### Issue 3: Redundant Type Inference

`infer_type_from_type_expr_with_interner()` re-parses AST `TypeExpr`, but analysis phase already did this work.

#### Issue 4: Cannot Quickly Determine Type Equality

Current Type is a full struct; comparing two types requires deep comparison of the entire structure.

---

## 2. Design

### 2.1 Core ID System

Introduce three core ID types:

```rust
// ===== gox-common-core =====

/// Expression unique ID (assigned by Parser)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(pub u32);

/// Type unique ID (assigned by TypeInterner)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

// Symbol already exists for identifiers
```

### 2.2 AST Modification

Add unique ID to `Expr`:

```rust
// ===== gox-syntax/src/ast.rs =====

pub struct Expr {
    pub id: ExprId,        // New: unique ID
    pub kind: ExprKind,
    pub span: Span,        // Keep: for error reporting
}
```

Parser assigns ExprId during parsing:

```rust
impl Parser {
    next_expr_id: u32,
    
    fn alloc_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.next_expr_id);
        self.next_expr_id += 1;
        id
    }
}
```

### 2.3 TypeInterner - Type Deduplication and Mapping

```rust
// ===== gox-analysis/src/types.rs =====

/// Type Interner - type deduplication + fast lookup
pub struct TypeInterner {
    /// TypeId → Type (all unique types)
    types: Vec<Type>,
    
    /// Type → TypeId (for deduplication)
    type_to_id: HashMap<Type, TypeId>,
    
    /// Symbol → TypeId (var name/param name/type name → type)
    symbol_types: HashMap<Symbol, TypeId>,
    
    /// ExprId → TypeId (expression → type)
    expr_types: HashMap<ExprId, TypeId>,
}

impl TypeInterner {
    /// Insert type, return TypeId (with deduplication)
    pub fn intern(&mut self, ty: Type) -> TypeId {
        // Named types: each declaration is independent
        if let Type::Named(decl_id) = &ty {
            return TypeId(decl_id.0);
        }
        
        // Other types: structurally equal = share TypeId
        if let Some(&id) = self.type_to_id.get(&ty) {
            return id;
        }
        let id = TypeId(self.types.len() as u32);
        self.types.push(ty.clone());
        self.type_to_id.insert(ty, id);
        id
    }
    
    /// TypeId → &Type
    pub fn resolve(&self, id: TypeId) -> &Type {
        &self.types[id.0 as usize]
    }
    
    /// Symbol → TypeId
    pub fn get_symbol_type(&self, sym: Symbol) -> Option<TypeId> {
        self.symbol_types.get(&sym).copied()
    }
    
    /// ExprId → TypeId
    pub fn get_expr_type(&self, expr_id: ExprId) -> Option<TypeId> {
        self.expr_types.get(&expr_id).copied()
    }
    
    /// Bind Symbol → TypeId
    pub fn bind_symbol(&mut self, sym: Symbol, type_id: TypeId) {
        self.symbol_types.insert(sym, type_id);
    }
    
    /// Bind ExprId → TypeId
    pub fn bind_expr(&mut self, expr_id: ExprId, type_id: TypeId) {
        self.expr_types.insert(expr_id, type_id);
    }
    
    /// Type comparison - O(1)
    pub fn same_type(&self, a: TypeId, b: TypeId) -> bool {
        a == b
    }
}
```

### 2.4 Type Equality Rules

| Type | Equality Rule | Example |
|------|---------------|---------|
| **Basic** | Same name = same type | `int` == `int` |
| **Named** | Each declaration is independent, even if underlying is same | `type A int` ≠ `type B int` |
| **Slice** | Element type is same | `[]int` == `[]int` |
| **Array** | Length + element type are both same | `[3]int` == `[3]int` |
| **Map** | Key + value types are both same | `map[string]int` == `map[string]int` |
| **Pointer** | Pointed type is same | `*Foo` == `*Foo` |
| **Chan** | Direction + element type are same | `chan int` ≠ `<-chan int` |
| **Func** | Param types + return types are same | `func(int) string` == `func(int) string` |
| **Struct literal** | Field names + types + order are all same | `struct{x int}` == `struct{x int}` |
| **Interface** | Method sets are same | `interface{Read() int}` == `interface{Read() int}` |

**Key**: Named types are independent per declaration; other types share TypeId if structurally equal.

### 2.5 New TypeCheckResult

```rust
pub struct TypeCheckResult {
    /// Type system (dedup + Symbol/Expr → Type mapping)
    pub types: TypeInterner,
    
    /// Named types definition info
    pub named_types: Vec<NamedTypeInfo>,
    
    /// Named type fast index: Symbol → index
    pub named_type_index: HashMap<Symbol, u32>,
}
```

### 2.6 Method Table Refactoring

Change from `HashMap<String, u32>` to `HashMap<(Symbol, Symbol), u32>`:

```rust
// Before
pub method_table: HashMap<String, u32>,  // "TypeName.MethodName" → func_idx

// After
pub method_table: HashMap<(Symbol, Symbol), u32>,  // (type_sym, method_sym) → func_idx
```

---

## 3. Data Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Parser Phase                                 │
├─────────────────────────────────────────────────────────────────────┤
│  - Assign ExprId to each Expr                                       │
│  - Output: AST (with ExprId)                                        │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                        Analysis Phase                                │
├─────────────────────────────────────────────────────────────────────┤
│  1. Collect: Gather type declarations, assign TypeId to Named types │
│  2. Resolve: Resolve type references                                │
│  3. Check:                                                          │
│     - Infer type for each expression                                │
│     - intern(type) → TypeId                                         │
│     - bind_expr(expr.id, type_id)                                   │
│     - bind_symbol(var_sym, type_id)                                 │
│                                                                      │
│  Output: TypeCheckResult { types: TypeInterner, ... }               │
└───────────────────────────────┬─────────────────────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────────┐
│                        Codegen Phase                                 │
├─────────────────────────────────────────────────────────────────────┤
│  // Query expression type - O(1)                                    │
│  let type_id = ctx.types.get_expr_type(expr.id)?;                   │
│  let ty = ctx.types.resolve(type_id);                               │
│                                                                      │
│  // Query variable type - O(1)                                      │
│  let type_id = ctx.types.get_symbol_type(var_sym)?;                 │
│                                                                      │
│  // Type comparison - O(1)                                          │
│  if type_id_a == type_id_b { /* same type */ }                      │
│                                                                      │
│  // Method lookup - O(1)                                            │
│  ctx.method_table.get(&(type_sym, method_sym))                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 3.1 Codegen Access Interface Details

```rust
// ===== CodegenContext holds reference to TypeInterner =====

pub struct CodegenContext<'a> {
    pub types: &'a TypeInterner,  // From TypeCheckResult
    // ... other fields
}

// ===== Typical Usage Scenarios =====

// 1. Get type when compiling expression
fn compile_expr(ctx: &CodegenContext, expr: &Expr) -> Result<...> {
    let type_id = ctx.types.get_expr_type(expr.id)?;
    let ty = ctx.types.resolve(type_id);
    
    match ty {
        Type::Slice(_) => { /* generate slice operations */ }
        Type::Map(_) => { /* generate map operations */ }
        Type::Struct(s) => { /* access fields via s.fields */ }
        _ => { ... }
    }
}

// 2. Check if expression is a specific type
fn is_float_expr(ctx: &CodegenContext, expr: &Expr) -> bool {
    ctx.types.get_expr_type(expr.id)
        .map(|id| matches!(ctx.types.resolve(id), Type::Basic(BasicType::Float64)))
        .unwrap_or(false)
}

// 3. Get variable type (global variables)
fn get_var_type(ctx: &CodegenContext, var_sym: Symbol) -> Option<&Type> {
    ctx.types.get_symbol_type(var_sym)
        .map(|id| ctx.types.resolve(id))
}

// 4. Compare if two expressions have same type
fn same_type(ctx: &CodegenContext, e1: &Expr, e2: &Expr) -> bool {
    match (ctx.types.get_expr_type(e1.id), ctx.types.get_expr_type(e2.id)) {
        (Some(t1), Some(t2)) => t1 == t2,  // Direct TypeId comparison
        _ => false,
    }
}

// 5. Get struct field information
fn get_field_offset(ctx: &CodegenContext, expr: &Expr, field_sym: Symbol) -> Option<u16> {
    let type_id = ctx.types.get_expr_type(expr.id)?;
    let ty = ctx.types.resolve(type_id);
    
    if let Type::Struct(s) = ty {
        for (i, field) in s.fields.iter().enumerate() {
            if field.name == Some(field_sym) {
                return Some(i as u16);
            }
        }
    }
    None
}
```

---

## 4. Performance Improvements

| Operation | Before | After |
|-----------|--------|-------|
| Named type lookup | O(n) linear scan | O(1) HashMap |
| Expression type query | O(1) but requires span key construction | O(1) with ExprId |
| Variable type query | Traverse scope | O(1) HashMap |
| Method call lookup | format! + string hash | Pure integer tuple hash |
| Interface dispatch | O(n) traverse + ends_with | O(m) only matching methods |
| Type comparison | Deep structure comparison | `a == b` integer comparison |

---

## 5. Implementation Steps

| Phase | Content | Affected Crate |
|-------|---------|----------------|
| **Phase 1** | Add `ExprId`, `TypeId` to gox-common-core | `gox-common-core` |
| **Phase 2** | Add `id` field to `Expr`, Parser assigns ID | `gox-syntax` |
| **Phase 3** | Implement `TypeInterner`, refactor `TypeCheckResult` | `gox-analysis` |
| **Phase 4** | Modify type checking to use intern/bind | `gox-analysis` |
| **Phase 5** | Codegen uses new interfaces, remove string operations | `gox-codegen-vm` |

---

## 6. Appendix: Type Hash Implementation

```rust
impl Hash for Type {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Type::Basic(b) => { 0u8.hash(state); b.hash(state); }
            Type::Named(id) => { 1u8.hash(state); id.0.hash(state); }
            Type::Slice(s) => { 2u8.hash(state); s.elem.hash(state); }
            Type::Array(a) => { 
                3u8.hash(state); 
                a.len.hash(state); 
                a.elem.hash(state); 
            }
            Type::Map(m) => { 
                4u8.hash(state); 
                m.key.hash(state); 
                m.value.hash(state); 
            }
            Type::Pointer(p) => { 5u8.hash(state); p.hash(state); }
            Type::Chan(c) => { 
                6u8.hash(state); 
                c.dir.hash(state); 
                c.elem.hash(state); 
            }
            Type::Func(f) => { 
                7u8.hash(state); 
                f.params.hash(state); 
                f.results.hash(state);
                f.variadic.hash(state);
            }
            Type::Struct(s) => {
                8u8.hash(state);
                for field in &s.fields {
                    field.name.hash(state);
                    field.ty.hash(state);
                }
            }
            Type::Interface(i) => {
                9u8.hash(state);
                for method in &i.methods {
                    method.name.hash(state);
                    method.sig.hash(state);
                }
            }
            Type::Tuple(ts) => { 10u8.hash(state); ts.hash(state); }
            Type::Untyped(k) => { 11u8.hash(state); k.hash(state); }
            Type::Nil => { 12u8.hash(state); }
            Type::Invalid => { 13u8.hash(state); }
        }
    }
}
```
