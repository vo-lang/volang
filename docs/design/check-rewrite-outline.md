# Check Module Rewrite Outline

## Current Structure

```
vo-analysis/src/check/
├── mod.rs      (568 lines)  - TypeChecker struct, scope management, entry points
├── expr.rs     (1167 lines) - Expression checking, type predicates
├── stmt.rs     (687 lines)  - Statement checking
├── builtin.rs  (248 lines)  - Built-in function checking
└── tests.rs    (26275 lines) - Tests
```

## Key Changes Overview

| Component | Before | After |
|-----------|--------|-------|
| Type storage | `expr_types: HashMap<(u32,u32), Type>` | `types: &mut TypeInterner` |
| Expr key | `(span.start, span.end)` | `expr.id: ExprId` |
| Return type | `fn check_expr() -> Type` | `fn check_expr() -> TypeId` |
| Type compare | Deep struct comparison | `TypeId == TypeId` (O(1)) |

---

## Part 1: mod.rs Rewrite

### 1.1 TypeChecker Struct

**Before:**
```rust
pub struct TypeChecker<'a> {
    pub(crate) interner: &'a SymbolInterner,
    pub(crate) diagnostics: &'a mut DiagnosticSink,
    pub(crate) package_scope: &'a Scope,
    pub(crate) local_scope: Option<Scope>,
    pub(crate) named_types: &'a [NamedTypeInfo],
    pub(crate) registry: TypeRegistry<'a>,
    pub(crate) return_types: Vec<Type>,
    // ...
    pub expr_types: HashMap<(u32, u32), Type>,  // ← DELETE
}
```

**After:**
```rust
pub struct TypeChecker<'a> {
    pub(crate) interner: &'a SymbolInterner,
    pub(crate) diagnostics: &'a mut DiagnosticSink,
    pub(crate) package_scope: &'a Scope,
    pub(crate) local_scope: Option<Scope>,
    pub(crate) named_types: &'a [NamedTypeInfo],
    pub(crate) registry: TypeRegistry<'a>,
    pub(crate) return_type_ids: Vec<TypeId>,  // ← TypeId instead of Type
    // ...
    pub types: &'a mut TypeInterner,  // ← NEW: type interner reference
}
```

### 1.2 Constructor

**Before:**
```rust
pub fn new(resolve_result: &'a ResolveResult, ...) -> Self {
    Self {
        // ...
        expr_types: HashMap::new(),
    }
}
```

**After:**
```rust
pub fn new(
    resolve_result: &'a ResolveResult,
    types: &'a mut TypeInterner,  // ← Pass in
    ...
) -> Self {
    Self {
        // ...
        types,
    }
}
```

### 1.3 record_expr_type

**Before:**
```rust
pub(crate) fn record_expr_type(&mut self, expr: &Expr, ty: Type) {
    self.expr_types.insert((expr.span.start.0, expr.span.end.0), ty);
}
```

**After:**
```rust
pub(crate) fn record_expr_type(&mut self, expr: &Expr, type_id: TypeId) {
    self.types.bind_expr(expr.id, type_id);
}
```

### 1.4 resolve_type_expr

**Before:**
```rust
pub(crate) fn resolve_type_expr(&self, ty_expr: &TypeExpr) -> Type
```

**After:**
```rust
pub(crate) fn resolve_type_expr(&mut self, ty_expr: &TypeExpr) -> TypeId {
    let ty = match &ty_expr.kind {
        TypeExprKind::Ident(name) => { /* same logic */ }
        // ...
    };
    self.types.intern(ty)  // ← intern and return TypeId
}
```

### 1.5 define_var

**Before:**
```rust
pub(crate) fn define_var(&mut self, symbol: Symbol, ty: Type, span: Span) {
    scope.insert(symbol, Entity::Var(VarEntity { ty, ... }));
}
```

**After:**
```rust
pub(crate) fn define_var(&mut self, symbol: Symbol, type_id: TypeId, span: Span) {
    // Also bind symbol → type in interner
    self.types.bind_symbol(symbol, type_id);
    // VarEntity still stores Type for now (or change to TypeId)
    let ty = self.types.resolve(type_id).clone();
    scope.insert(symbol, Entity::Var(VarEntity { ty, ... }));
}
```

### 1.6 check_func_body

**Before:**
```rust
self.return_types = func.sig.results.iter()
    .map(|r| self.resolve_type_expr(&r.ty))
    .collect();
```

**After:**
```rust
self.return_type_ids = func.sig.results.iter()
    .map(|r| self.resolve_type_expr(&r.ty))  // Now returns TypeId
    .collect();
```

---

## Part 2: expr.rs Rewrite

### 2.1 check_expr Main Entry

**Before:**
```rust
pub fn check_expr(&mut self, expr: &Expr) -> Type {
    let ty = match &expr.kind {
        ExprKind::IntLit(lit) => self.check_int_lit(lit),
        // ...
    };
    self.record_expr_type(expr, ty.clone());
    ty
}
```

**After:**
```rust
pub fn check_expr(&mut self, expr: &Expr) -> TypeId {
    let type_id = match &expr.kind {
        ExprKind::IntLit(_) => self.types.intern(Type::Untyped(UntypedKind::Int)),
        ExprKind::FloatLit(_) => self.types.intern(Type::Untyped(UntypedKind::Float)),
        ExprKind::StringLit(_) => self.types.intern(Type::Untyped(UntypedKind::String)),
        ExprKind::RuneLit(_) => self.types.intern(Type::Untyped(UntypedKind::Rune)),
        ExprKind::Ident(ident) => self.check_ident(ident),
        ExprKind::Binary(bin) => self.check_binary(bin, expr.span),
        // ...
    };
    self.record_expr_type(expr, type_id);
    type_id
}
```

### 2.2 check_ident

**Before:**
```rust
fn check_ident(&mut self, ident: &Ident) -> Type {
    match self.lookup(ident.symbol) {
        Some(Entity::Var(v)) => v.ty.clone(),
        // ...
    }
}
```

**After:**
```rust
fn check_ident(&mut self, ident: &Ident) -> TypeId {
    match self.lookup(ident.symbol) {
        Some(Entity::Var(v)) => self.types.intern(v.ty.clone()),
        Some(Entity::Func(f)) => self.types.intern(Type::Func(f.sig.clone())),
        // ...
    }
}
```

### 2.3 check_binary

**Before:**
```rust
fn check_binary(&mut self, bin: &BinaryExpr, span: Span) -> Type {
    let left_ty = self.check_expr(&bin.left);
    let right_ty = self.check_expr(&bin.right);
    // ... compare Types
}
```

**After:**
```rust
fn check_binary(&mut self, bin: &BinaryExpr, span: Span) -> TypeId {
    let left_id = self.check_expr(&bin.left);
    let right_id = self.check_expr(&bin.right);
    
    // Resolve for comparison if needed
    let left_ty = self.types.resolve(left_id);
    let right_ty = self.types.resolve(right_id);
    
    // ... same logic, but intern result
    let result_ty = /* ... */;
    self.types.intern(result_ty)
}
```

### 2.4 Type Predicates

**Before:**
```rust
pub(crate) fn is_numeric_type(&self, ty: &Type) -> bool {
    match self.underlying_type(ty) { ... }
}
```

**After - Two Options:**

**Option A: Keep Type-based (simpler migration):**
```rust
pub(crate) fn is_numeric_type(&self, type_id: TypeId) -> bool {
    let ty = self.types.resolve(type_id);
    match self.underlying_type(ty) { ... }
}
```

**Option B: TypeId-based with caching (better perf):**
```rust
// Pre-compute well-known TypeIds
pub(crate) fn is_numeric_type(&self, type_id: TypeId) -> bool {
    // Fast path: check against cached numeric TypeIds
    if self.types.is_numeric_cached(type_id) {
        return true;
    }
    // Slow path: resolve and check
    let ty = self.types.resolve(type_id);
    match self.underlying_type(ty) { ... }
}
```

**Recommendation: Start with Option A, optimize later.**

### 2.5 underlying_type

**Before:**
```rust
pub(crate) fn underlying_type(&self, ty: &Type) -> Type {
    match ty {
        Type::Named(id) => {
            if let Some(info) = self.named_types.get(id.0 as usize) {
                self.underlying_type(&info.underlying)
            } else { Type::Invalid }
        }
        _ => ty.clone(),
    }
}
```

**After:**
```rust
pub(crate) fn underlying_type_id(&mut self, type_id: TypeId) -> TypeId {
    let ty = self.types.resolve(type_id);
    match ty {
        Type::Named(id) => {
            if let Some(info) = self.named_types.get(id.0 as usize) {
                let underlying = info.underlying.clone();
                let underlying_id = self.types.intern(underlying);
                self.underlying_type_id(underlying_id)
            } else {
                self.types.intern(Type::Invalid)
            }
        }
        _ => type_id,  // Already underlying
    }
}
```

### 2.6 is_assignable

**Before:**
```rust
pub(crate) fn is_assignable(&self, from: &Type, to: &Type) -> bool {
    if from == to { return true; }
    // ... complex logic
}
```

**After:**
```rust
pub(crate) fn is_assignable(&self, from_id: TypeId, to_id: TypeId) -> bool {
    // Fast path: same TypeId = same type
    if from_id == to_id { return true; }
    
    // Slow path: resolve and compare
    let from = self.types.resolve(from_id);
    let to = self.types.resolve(to_id);
    // ... same logic
}
```

---

## Part 3: stmt.rs Rewrite

### 3.1 check_assign

**Before:**
```rust
fn check_assign(&mut self, assign: &AssignStmt) {
    let lhs_types: Vec<Type> = assign.lhs.iter().map(|e| self.check_expr(e)).collect();
    let rhs_types: Vec<Type> = assign.rhs.iter().map(|e| self.check_expr(e)).collect();
    // ...
    if !self.is_assignable(rhs, lhs_ty) { ... }
}
```

**After:**
```rust
fn check_assign(&mut self, assign: &AssignStmt) {
    let lhs_ids: Vec<TypeId> = assign.lhs.iter().map(|e| self.check_expr(e)).collect();
    let rhs_ids: Vec<TypeId> = assign.rhs.iter().map(|e| self.check_expr(e)).collect();
    // ...
    if !self.is_assignable(rhs_id, lhs_id) { ... }
}
```

### 3.2 check_short_var

**Before:**
```rust
let var_ty = self.default_type(ty);
self.define_var(name.symbol, var_ty, name.span);
```

**After:**
```rust
let var_type_id = self.default_type_id(type_id);
self.define_var(name.symbol, var_type_id, name.span);
```

### 3.3 check_return

**Before:**
```rust
fn check_return(&mut self, ret: &ReturnStmt) {
    let actual_types: Vec<Type> = ret.values.iter().map(|e| self.check_expr(e)).collect();
    let return_types = self.return_types.clone();
    // compare actual_types with return_types
}
```

**After:**
```rust
fn check_return(&mut self, ret: &ReturnStmt) {
    let actual_ids: Vec<TypeId> = ret.values.iter().map(|e| self.check_expr(e)).collect();
    // compare actual_ids with self.return_type_ids
}
```

---

## Part 4: builtin.rs Rewrite

Same pattern: all `check_*` methods return `TypeId` instead of `Type`.

**Before:**
```rust
fn check_len(&mut self, args: &[Expr], span: Span) -> Type {
    let arg_ty = self.check_expr(&args[0]);
    match self.underlying_type(&arg_ty) {
        Type::Array(_) | ... => Type::Basic(BasicType::Int),
        _ => Type::Invalid,
    }
}
```

**After:**
```rust
fn check_len(&mut self, args: &[Expr], span: Span) -> TypeId {
    let arg_id = self.check_expr(&args[0]);
    let arg_ty = self.types.resolve(arg_id);
    match self.underlying_type(arg_ty) {
        Type::Array(_) | ... => self.types.intern(Type::Basic(BasicType::Int)),
        _ => self.types.intern(Type::Invalid),
    }
}
```

---

## Part 5: Helper Types to Add

### 5.1 Well-Known TypeIds (cached)

```rust
// In TypeInterner or TypeChecker
pub struct WellKnownTypeIds {
    pub invalid: TypeId,
    pub bool_: TypeId,
    pub int: TypeId,
    pub int64: TypeId,
    pub float64: TypeId,
    pub string: TypeId,
    pub untyped_int: TypeId,
    pub untyped_float: TypeId,
    pub untyped_string: TypeId,
    pub untyped_bool: TypeId,
    pub empty_tuple: TypeId,
}

impl TypeInterner {
    pub fn init_well_known(&mut self) -> WellKnownTypeIds {
        WellKnownTypeIds {
            invalid: self.intern(Type::Invalid),
            bool_: self.intern(Type::Basic(BasicType::Bool)),
            int: self.intern(Type::Basic(BasicType::Int)),
            // ...
        }
    }
}
```

### 5.2 Convenience Methods on TypeInterner

```rust
impl TypeInterner {
    /// Intern and return TypeId
    pub fn intern(&mut self, ty: Type) -> TypeId;
    
    /// Get Type from TypeId
    pub fn resolve(&self, id: TypeId) -> &Type;
    
    /// Bind ExprId → TypeId
    pub fn bind_expr(&mut self, expr_id: ExprId, type_id: TypeId);
    
    /// Bind Symbol → TypeId  
    pub fn bind_symbol(&mut self, sym: Symbol, type_id: TypeId);
    
    /// Get TypeId for ExprId
    pub fn get_expr_type(&self, expr_id: ExprId) -> Option<TypeId>;
    
    /// Get TypeId for Symbol
    pub fn get_symbol_type(&self, sym: Symbol) -> Option<TypeId>;
}
```

---

## Part 6: Rewrite Strategy (Break Everything, Fix Later)

### Phase 1: vo-common-core
- Add `ExprId`, `TypeId` to `types.rs`

### Phase 2: vo-syntax  
- Add `id: ExprId` to `Expr`
- Parser assigns ID when creating expressions

### Phase 3: vo-analysis - TypeInterner
- Create `type_interner.rs`

### Phase 4: vo-analysis - Rewrite check/ (BREAK)
- Rewrite `mod.rs`: TypeChecker uses `&mut TypeInterner`
- Rewrite `expr.rs`: All `check_*` return `TypeId`
- Rewrite `stmt.rs`: Use `TypeId` throughout
- Rewrite `builtin.rs`: Return `TypeId`
- **Will not compile until complete**

### Phase 5: vo-analysis - Update lib.rs
- `TypeCheckResult` contains `TypeInterner`
- Remove old `expr_types: HashMap<(u32,u32), Type>`

### Phase 6: vo-codegen-vm Rewrite
- Rewrite type access to use `TypeInterner`
- Remove string-based `method_table`
- See Part 6.5 below

### Phase 7: Fix Compilation Errors
- Run tests, fix failures

---

## Part 6.5: Codegen Rewrite

### 6.5.1 CodegenContext Changes

**Before:**
```rust
pub struct CodegenContext<'a> {
    pub result: &'a TypeCheckResult,  // Contains expr_types HashMap
    pub method_table: HashMap<String, u32>,  // "Type.Method" → func_idx
    // ...
}

pub fn lookup_expr_type(&self, expr: &Expr) -> Option<Type> {
    self.result.expr_types.get(&(expr.span.start.0, expr.span.end.0)).cloned()
}

pub fn get_named_type_info(&self, sym: Symbol) -> Option<&NamedTypeInfo> {
    for named in &self.result.named_types {
        if named.name == sym { return Some(named); }
    }
    None
}
```

**After:**
```rust
pub struct CodegenContext<'a> {
    pub types: &'a TypeInterner,  // From TypeCheckResult
    pub named_types: &'a [NamedTypeInfo],
    pub named_type_index: &'a HashMap<Symbol, u32>,  // O(1) lookup
    pub method_table: HashMap<(Symbol, Symbol), u32>,  // (type_sym, method_sym)
    // ...
}

pub fn get_expr_type(&self, expr: &Expr) -> Option<TypeId> {
    self.types.get_expr_type(expr.id)
}

pub fn resolve_type(&self, type_id: TypeId) -> &Type {
    self.types.resolve(type_id)
}

pub fn get_named_type_info(&self, sym: Symbol) -> Option<&NamedTypeInfo> {
    let idx = self.named_type_index.get(&sym)?;
    self.named_types.get(*idx as usize)
}
```

### 6.5.2 Delete These Functions

| Function | File | Reason |
|----------|------|--------|
| `lookup_expr_type()` | context.rs | Replace with `types.get_expr_type(expr.id)` |
| `infer_type_from_type_expr()` | context.rs | Use `expr.id` directly |
| `infer_type_from_type_expr_with_interner()` | context.rs | Delete entirely (~70 lines) |
| `get_named_type_info()` (loop version) | context.rs | Replace with indexed lookup |

### 6.5.3 method_table Rewrite

**Before:**
```rust
// Building method table
let method_key = format!("{}.{}", type_name, method_name);
method_table.insert(method_key, func_idx);

// Looking up
let method_key = format!("{}.{}", type_name, method);
if let Some(&func_idx) = ctx.method_table.get(&method_key) { ... }

// Interface dispatch (O(n) scan!)
for (key, &func_idx) in ctx.method_table.iter() {
    if key.ends_with(&format!(".{}", method)) { ... }
}
```

**After:**
```rust
// Building method table
method_table.insert((type_sym, method_sym), func_idx);

// Looking up - O(1)
if let Some(&func_idx) = ctx.method_table.get(&(type_sym, method_sym)) { ... }

// Interface dispatch - use method_by_name index
let methods = ctx.methods_by_name.get(&method_sym)?;  // Vec<(TypeSymbol, FuncIdx)>
```

### 6.5.4 expr.rs Changes

**Before:**
```rust
// expr.rs - many places like this:
let ty = ctx.lookup_expr_type(expr)?;
match ty {
    Type::Slice(_) => { ... }
}
```

**After:**
```rust
let type_id = ctx.types.get_expr_type(expr.id)?;
let ty = ctx.types.resolve(type_id);
match ty {
    Type::Slice(_) => { ... }
}
```

### 6.5.5 Files to Modify in codegen-vm

| File | Lines | Changes |
|------|-------|---------|
| `context.rs` | ~1400 | Major rewrite: CodegenContext, delete old functions |
| `expr.rs` | ~2800 | Replace all `lookup_expr_type` calls |
| `stmt.rs` | ~1100 | Replace type lookups |
| `lib.rs` | ~800 | Update method_table building |

### 6.5.6 Key Patterns to Replace

```rust
// Pattern 1: Expression type lookup
// Before:
ctx.lookup_expr_type(expr).map_or(default, |ty| ...)
// After:
ctx.types.get_expr_type(expr.id).map(|id| ctx.types.resolve(id)).map_or(default, |ty| ...)

// Pattern 2: Named type lookup
// Before:
for named in &ctx.result.named_types { if named.name == sym { ... } }
// After:
if let Some(&idx) = ctx.named_type_index.get(&sym) { let named = &ctx.named_types[idx]; ... }

// Pattern 3: Method lookup
// Before:
let key = format!("{}.{}", type_name, method);
ctx.method_table.get(&key)
// After:
ctx.method_table.get(&(type_sym, method_sym))

// Pattern 4: String resolution for method key
// Before:
let type_name = ctx.interner.resolve(type_sym).unwrap_or("");
let method_key = format!("{}.{}", type_name, method);
// After:
// Direct symbol tuple, no string needed
```

---

## Part 7: Files to Create/Modify

| File | Action | Changes |
|------|--------|---------|
| `vo-common-core/src/types.rs` | Modify | Add `ExprId`, `TypeId` |
| `vo-syntax/src/ast.rs` | Modify | Add `id: ExprId` to `Expr` |
| `vo-syntax/src/parser/expr.rs` | Modify | Assign `ExprId` when parsing |
| `vo-analysis/src/type_interner.rs` | **Create** | `TypeInterner` implementation |
| `vo-analysis/src/check/mod.rs` | Modify | See Part 1 |
| `vo-analysis/src/check/expr.rs` | Modify | See Part 2 |
| `vo-analysis/src/check/stmt.rs` | Modify | See Part 3 |
| `vo-analysis/src/check/builtin.rs` | Modify | See Part 4 |
| `vo-analysis/src/lib.rs` | Modify | Update `TypeCheckResult` |
| `vo-codegen-vm/src/context.rs` | **Rewrite** | See Part 6.5.1, delete old functions |
| `vo-codegen-vm/src/expr.rs` | Modify | Replace `lookup_expr_type` calls |
| `vo-codegen-vm/src/stmt.rs` | Modify | Replace type lookups |
| `vo-codegen-vm/src/lib.rs` | Modify | Update method_table building |

---

## Part 8: Estimated Line Changes

### vo-analysis

| File | Lines Before | Est. Lines After | Change Type |
|------|--------------|------------------|-------------|
| `type_interner.rs` | 0 | ~150 | New |
| `check/mod.rs` | 568 | ~550 | Refactor |
| `check/expr.rs` | 1167 | ~1100 | Refactor |
| `check/stmt.rs` | 687 | ~650 | Refactor |
| `check/builtin.rs` | 248 | ~250 | Minor |

### vo-codegen-vm

| File | Lines Before | Est. Lines After | Change Type |
|------|--------------|------------------|-------------|
| `context.rs` | 1472 | ~1300 | Rewrite (delete old code) |
| `expr.rs` | 2799 | ~2700 | Refactor |
| `stmt.rs` | 1100 | ~1050 | Refactor |
| `lib.rs` | 764 | ~750 | Refactor |

### Summary

| Crate | Before | After | Net |
|-------|--------|-------|-----|
| vo-analysis | ~2670 | ~2700 | +30 (add TypeInterner) |
| vo-codegen-vm | ~6135 | ~5800 | -335 (remove string ops) |
