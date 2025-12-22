# MM Phase 1: Escape Analysis

**Parent**: [2025-12-22-mm-memory-model-plan.md](2025-12-22-mm-memory-model-plan.md)  
**Status**: Not Started  
**Est. Lines**: ~500

## Overview

Implement static escape analysis in `gox-analysis` to determine which variables need heap allocation.

## Escape Rules

### Primitives (int, float, bool)
- Escape when: captured by closure

### struct
- Escape when:
  1. Address taken `&s`
  2. Captured by closure
  3. Assigned to interface

### array
- Escape when:
  1. Captured by closure
  2. Assigned to interface
  3. Sliced `arr[:]` or `arr[i:j]`

### Size Threshold
- struct/array > 256 slots → always escape (handled in codegen, not escape analysis)

### Nested Escape Propagation

If a struct or **any of its nested fields** triggers escape, the entire root struct escapes.

```gox
type Inner struct { x int }
type Outer struct { inner Inner }

var o Outer
p := &o.inner      // o escapes (not just inner)
p2 := &o.inner.x   // o escapes (not just x or inner)
```

**Rationale**: Fields are stored inline within the parent struct. Changing this to per-field heap allocation would require two different memory layouts for the same type, significantly increasing codegen complexity. We choose simplicity: always mark the root variable as escaped.

## Implementation

### Independent Pass (Recommended)

Run escape analysis as a separate pass **after** type checking completes. This is cleaner than integrating into the checker because:

1. No modification to existing checker logic
2. All data (TypeInfo) is available
3. Easier to test and debug
4. Clear separation of concerns

### Data Structure

```rust
// type_info.rs
pub struct TypeInfo {
    // ... existing fields
    
    /// Variables that escape to heap (set by escape analysis pass)
    pub escaped_vars: HashSet<ObjKey>,
}

impl TypeInfo {
    pub fn is_escaped(&self, obj: ObjKey) -> bool {
        self.escaped_vars.contains(&obj)
    }
}
```

### New File: `check/escape.rs`

```rust
pub fn analyze(
    files: &[File],
    type_info: &TypeInfo,
    tc_objs: &TCObjects,
) -> HashSet<ObjKey> {
    let mut analyzer = EscapeAnalyzer::new(type_info, tc_objs);
    for file in files {
        analyzer.visit_file(file, None);
    }
    analyzer.escaped
}

struct EscapeAnalyzer<'a> {
    type_info: &'a TypeInfo,
    tc_objs: &'a TCObjects,
    escaped: HashSet<ObjKey>,
}

impl EscapeAnalyzer<'_> {
    fn visit_expr(&mut self, expr: &Expr, func_scope: Option<ScopeKey>) {
        match &expr.kind {
            // 1. Address taken → root variable escapes
            ExprKind::Unary(UnaryOp::Addr, operand) => {
                if let Some(root) = self.find_root_var(operand) {
                    self.escaped.insert(root);
                }
            }
            
            // 2. Slice operation → array escapes
            ExprKind::Slice(sl) => {
                if let Some(root) = self.find_root_var(&sl.expr) {
                    if self.is_array_type(root) {
                        self.escaped.insert(root);
                    }
                }
            }
            
            // 3. FuncLit → enter new func_scope
            ExprKind::FuncLit(func) => {
                let closure_scope = self.type_info.scopes.get(&func.sig.span).copied();
                self.visit_block(&func.body, closure_scope);
            }
            
            // 4. Variable reference → check if captured
            ExprKind::Ident(ident) => {
                if let Some(func_scope) = func_scope {
                    if let Some(&obj) = self.type_info.uses.get(ident) {
                        if self.is_captured(obj, func_scope) {
                            self.escaped.insert(obj);
                        }
                    }
                }
            }
            
            _ => { /* visit children */ }
        }
    }
    
    /// Check if variable is captured by closure
    fn is_captured(&self, obj: ObjKey, func_scope: ScopeKey) -> bool {
        let var_scope = match self.tc_objs.lobjs[obj].parent() {
            Some(s) => s,
            None => return false,  // package-level var, not captured
        };
        // var_scope not within func_scope → captured
        !self.scope_contains(func_scope, var_scope)
    }
    
    /// Traverse Selector chain to find root variable
    fn find_root_var(&self, expr: &Expr) -> Option<ObjKey> {
        match &expr.kind {
            ExprKind::Ident(ident) => self.type_info.uses.get(ident).copied(),
            ExprKind::Selector(sel) => self.find_root_var(&sel.expr),
            ExprKind::Paren(inner) => self.find_root_var(inner),
            _ => None,
        }
    }
}
```

### Interface Assignment

Handle in `visit_stmt` for assignment statements:

```rust
StmtKind::Assign(lhs, rhs) => {
    for (l, r) in lhs.iter().zip(rhs.iter()) {
        if let Some(lhs_tv) = self.type_info.types.get(&l.id) {
            if is_interface(lhs_tv.typ, self.tc_objs) {
                if let Some(root) = self.find_root_var(r) {
                    self.escaped.insert(root);
                }
            }
        }
    }
}
```

### Call Flow

```rust
// checker.rs
pub fn check(...) -> TypeInfo {
    // 1. Existing type checking
    let mut type_info = self.do_check(files);
    
    // 2. Escape analysis pass (NEW)
    type_info.escaped_vars = escape::analyze(files, &type_info, &self.tc_objs);
    
    type_info
}
```

## Tasks

- [ ] Add `escaped_vars: HashSet<ObjKey>` field to `TypeInfo`
- [ ] Add `is_escaped(ObjKey)` method to `TypeInfo`
- [ ] Create `check/escape.rs` with `EscapeAnalyzer`
- [ ] Implement `find_root_var()` - traverse Selector chain
- [ ] Implement `is_captured()` - check scope containment
- [ ] Handle address-taken (`&expr`)
- [ ] Handle closure capture (variable reference in FuncLit)
- [ ] Handle interface assignment
- [ ] Handle slice operation on array
- [ ] Call `escape::analyze()` after type checking
- [ ] Write unit tests

## Testing

```gox
func test_no_escape() {
    var s Point  // should NOT escape
    s.x = 1
}

func test_address_escape() {
    var s Point
    p := &s      // s should escape
}

func test_nested_field_escape() {
    var o Outer
    p := &o.inner  // o should escape (not just inner)
}

func test_closure_escape() {
    x := 42
    f := func() { println(x) }  // x should escape
    f()
}

func test_closure_param_escape() {
    foo := func(x int) {
        f := func() { println(x) }  // x (param) should escape
        f()
    }
    foo(1)
}

func test_nested_closure_escape() {
    x := 1
    f := func() {
        g := func() { println(x) }  // x should escape
        g()
    }
    f()
}

func test_interface_escape() {
    var s Point
    var i interface{} = s  // s should escape
}

func test_interface_param_escape(i interface{}) {
    var s Point
    i = s  // s should escape
}

func test_slice_escape() {
    var arr [5]int
    s := arr[:]  // arr should escape
}

func test_package_var_no_capture() {
    f := func() { println(globalX) }  // globalX is package-level, NOT captured
    f()
}
```

## Deliverables

1. `check/escape.rs` - independent escape analysis pass
2. `TypeInfo.escaped_vars` field
3. `TypeInfo.is_escaped()` query
4. Unit tests for all escape scenarios
