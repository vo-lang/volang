//! Type information produced by type checking.


use crate::obj;
use crate::objects::{ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::operand::OperandMode;
use crate::selection::Selection;
use vo_syntax::ast::{Ident, IdentId, ExprId, TypeExprId};
use vo_common::Span;
use vo_syntax::ast::Expr;
use std::collections::{HashMap, HashSet};

/// TypeAndValue reports the type and value (for constants) of an expression.
#[derive(Debug, Clone)]
pub struct TypeAndValue {
    pub mode: OperandMode,
    pub typ: TypeKey,
}

/// Resolved dynamic access method for static dispatch.
/// 
/// When a `~>` operation is used on a concrete type (not any/interface),
/// the checker resolves the protocol method at compile time.
#[derive(Debug, Clone)]
pub struct DynAccessResolve {
    /// Method object key for static call.
    pub method: ObjKey,
    /// Index path for embedded types (empty if method is on the type itself).
    pub indices: Vec<usize>,
    /// Whether receiver needs indirection (pointer dereference).
    pub indirect: bool,
}

impl TypeAndValue {
    pub(crate) fn new(mode: OperandMode, typ: TypeKey) -> Self {
        TypeAndValue { mode, typ }
    }
}

/// An Initializer describes a package-level variable initialization.
#[derive(Debug, Clone)]
pub struct Initializer {
    pub lhs: Vec<ObjKey>,
    pub rhs: Expr,
}

impl Initializer {
    pub(crate) fn new(lhs: Vec<ObjKey>, rhs: Expr) -> Self {
        Initializer { lhs, rhs }
    }
}

/// TypeInfo holds the results of type checking.
#[derive(Debug, Default)]
pub struct TypeInfo {
    /// Maps expressions to their types (and values for constants).
    pub types: HashMap<ExprId, TypeAndValue>,

    /// Maps type expressions to their resolved types.
    pub type_exprs: HashMap<TypeExprId, TypeKey>,

    /// Maps identifier IDs to the objects they define.
    pub defs: HashMap<IdentId, Option<ObjKey>>,

    /// Maps identifier IDs to the objects they denote (use).
    pub uses: HashMap<IdentId, ObjKey>,

    /// Maps AST node spans to their implicitly declared objects.
    pub implicits: HashMap<Span, ObjKey>,

    /// Maps selector expression IDs to their selections.
    pub selections: HashMap<ExprId, Selection>,

    /// Maps AST node spans to the scopes they define.
    pub scopes: HashMap<Span, ScopeKey>,

    /// Package-level initializers in execution order.
    pub init_order: Vec<Initializer>,

    /// Variables that escape to heap (set by escape analysis pass).
    pub escaped_vars: HashSet<ObjKey>,

    /// Closure captures: FuncLit ExprId -> captured variables (set by escape analysis pass).
    pub closure_captures: HashMap<ExprId, Vec<ObjKey>>,

    /// Dynamic access method resolution for static dispatch.
    /// Key: DynAccess expression ID
    /// Value: Some = static call info, None = dynamic dispatch (any/interface base)
    pub dyn_access_methods: HashMap<ExprId, Option<DynAccessResolve>>,
}

impl TypeInfo {
    pub(crate) fn new() -> TypeInfo {
        TypeInfo::default()
    }

    /// Records the type and mode for an expression.
    pub(crate) fn record_type(&mut self, expr_id: ExprId, mode: OperandMode, typ: TypeKey) {
        self.types.insert(expr_id, TypeAndValue::new(mode, typ));
    }

    /// Records a type and value for an expression (alias for record_type).
    pub(crate) fn record_type_and_value(&mut self, expr_id: ExprId, mode: OperandMode, typ: TypeKey) {
        self.record_type(expr_id, mode, typ);
    }

    /// Records the resolved type for a type expression.
    pub(crate) fn record_type_expr(&mut self, type_expr_id: TypeExprId, typ: TypeKey) {
        self.type_exprs.insert(type_expr_id, typ);
    }

    /// Records a definition.
    pub(crate) fn record_def(&mut self, ident: Ident, obj: Option<ObjKey>) {
        self.defs.insert(ident.id, obj);
    }

    /// Records a use.
    pub(crate) fn record_use(&mut self, ident: Ident, obj: ObjKey) {
        self.uses.insert(ident.id, obj);
    }

    /// Records an implicit object.
    pub(crate) fn record_implicit(&mut self, span: Span, obj: ObjKey) {
        self.implicits.insert(span, obj);
    }

    /// Records a selection.
    pub(crate) fn record_selection(&mut self, expr_id: ExprId, sel: Selection) {
        self.selections.insert(expr_id, sel);
    }

    /// Records a scope for an AST node.
    pub(crate) fn record_scope(&mut self, span: Span, scope: ScopeKey) {
        self.scopes.insert(span, scope);
    }

    /// Records init order.
    pub(crate) fn record_init_order(&mut self, init_order: Vec<Initializer>) {
        self.init_order = init_order;
    }

    /// Records dynamic access method resolution.
    /// `resolve` is Some for static dispatch (concrete type), None for dynamic dispatch (any/interface).
    pub(crate) fn record_dyn_access(&mut self, expr_id: ExprId, resolve: Option<DynAccessResolve>) {
        self.dyn_access_methods.insert(expr_id, resolve);
    }

    /// Records builtin type signature for a builtin function expression.
    /// The expression must be a (possibly parenthesized) identifier denoting a built-in.
    pub(crate) fn record_builtin_type(
        &mut self,
        mode: &OperandMode,
        expr: &vo_syntax::ast::Expr,
        sig: TypeKey,
    ) {
        use vo_syntax::ast::ExprKind;
        
        let mut e = expr;
        loop {
            self.record_type_and_value(e.id, mode.clone(), sig);
            match &e.kind {
                ExprKind::Ident(_) => break,
                ExprKind::Paren(inner) => e = inner,
                _ => break, // Should not happen for builtin calls
            }
        }
    }

    /// Records comma-ok types for expressions like map index, type assertion, channel receive.
    /// Aligned with goscript/types/src/check/check.rs::record_comma_ok_types
    pub(crate) fn record_comma_ok_types(
        &mut self,
        expr: &vo_syntax::ast::Expr,
        t: &[TypeKey; 2],
        tc_objs: &mut TCObjects,
        pkg: PackageKey,
    ) {
        use vo_syntax::ast::ExprKind;
        
        let span = expr.span;
        let mut e = expr;
        loop {
            let tv = self.types.get_mut(&e.id).unwrap();
            let vars = vec![
                tc_objs.lobjs.insert(obj::LangObj::new_var(span, Some(pkg), String::new(), Some(t[0]))),
                tc_objs.lobjs.insert(obj::LangObj::new_var(span, Some(pkg), String::new(), Some(t[1]))),
            ];
            tv.typ = tc_objs.new_t_tuple(vars);
            match &e.kind {
                ExprKind::Paren(inner) => e = inner,
                _ => break,
            }
        }
    }

    /// Looks up the type of an expression.
    pub fn expr_type(&self, expr_id: ExprId) -> Option<TypeKey> {
        self.types.get(&expr_id).map(|tv| tv.typ)
    }

    /// Looks up the mode of an expression.
    pub fn expr_mode(&self, expr_id: ExprId) -> Option<&OperandMode> {
        self.types.get(&expr_id).map(|tv| &tv.mode)
    }

    /// Looks up the object for a definition.
    pub fn get_def(&self, ident: &Ident) -> Option<ObjKey> {
        self.defs.get(&ident.id).and_then(|o| *o)
    }

    /// Looks up the object for a use.
    pub fn get_use(&self, ident: &Ident) -> Option<ObjKey> {
        self.uses.get(&ident.id).copied()
    }

    /// Returns true if the identifier is a definition.
    pub fn is_def(&self, ident: &Ident) -> bool {
        self.defs.contains_key(&ident.id)
    }

    /// Returns true if the variable escapes to heap.
    pub fn is_escaped(&self, obj: ObjKey) -> bool {
        self.escaped_vars.contains(&obj)
    }
}

// === Type Layout Functions ===
// These functions compute type layout information (slot counts, slot types, etc.)
// They only depend on TCObjects and can be used by both codegen and other consumers.

use crate::typ::{self, Type, BasicType};
use vo_runtime::{SlotType, ValueKind};

/// Compute the number of slots a type occupies.
pub fn type_slot_count(type_key: TypeKey, tc_objs: &TCObjects) -> u16 {
    let underlying = typ::underlying_type(type_key, tc_objs);
    match &tc_objs.types[underlying] {
        Type::Basic(_) => 1,
        Type::Pointer(_) => 1,
        Type::Slice(_) => 1,
        Type::Map(_) => 1,
        Type::Chan(_) => 1,
        Type::Signature(_) => 1, // closure is GcRef
        Type::Interface(_) => 2,
        Type::Struct(s) => {
            let mut total = 0u16;
            for &field_obj in s.fields() {
                if let Some(field_type) = tc_objs.lobjs[field_obj].typ() {
                    total += type_slot_count(field_type, tc_objs);
                }
            }
            // Empty struct still needs 1 slot (zero-size types not supported)
            total.max(1)
        }
        Type::Array(a) => {
            let elem_slots = type_slot_count(a.elem(), tc_objs);
            let len = a.len().unwrap_or(0) as u16;
            elem_slots * len
        }
        Type::Named(n) => type_slot_count(n.underlying(), tc_objs),
        Type::Tuple(t) => {
            let mut total = 0u16;
            for &var in t.vars() {
                if let Some(var_type) = tc_objs.lobjs[var].typ() {
                    total += type_slot_count(var_type, tc_objs);
                }
            }
            total
        }
    }
}

/// Compute the slot types for a type.
pub fn type_slot_types(type_key: TypeKey, tc_objs: &TCObjects) -> Vec<SlotType> {
    let underlying = typ::underlying_type(type_key, tc_objs);
    match &tc_objs.types[underlying] {
        Type::Basic(_) => vec![SlotType::Value],
        Type::Pointer(_) => vec![SlotType::GcRef],
        Type::Slice(_) => vec![SlotType::GcRef],
        Type::Map(_) => vec![SlotType::GcRef],
        Type::Chan(_) => vec![SlotType::GcRef],
        Type::Signature(_) => vec![SlotType::GcRef],
        Type::Interface(_) => vec![SlotType::Interface0, SlotType::Interface1],
        Type::Struct(s) => {
            let mut types = Vec::new();
            for &field_obj in s.fields() {
                if let Some(field_type) = tc_objs.lobjs[field_obj].typ() {
                    types.extend(type_slot_types(field_type, tc_objs));
                }
            }
            // Empty struct still needs 1 slot
            if types.is_empty() {
                types.push(SlotType::Value);
            }
            types
        }
        Type::Array(a) => {
            let elem_types = type_slot_types(a.elem(), tc_objs);
            let mut types = Vec::new();
            let len = a.len().unwrap_or(0) as usize;
            for _ in 0..len {
                types.extend(elem_types.iter().cloned());
            }
            types
        }
        Type::Named(n) => type_slot_types(n.underlying(), tc_objs),
        Type::Tuple(t) => {
            let mut types = Vec::new();
            for &var in t.vars() {
                if let Some(var_type) = tc_objs.lobjs[var].typ() {
                    types.extend(type_slot_types(var_type, tc_objs));
                }
            }
            types
        }
    }
}

/// Calculate the byte size for heap array/slice elements.
/// Packed types (bool, int8-32, float32) use actual byte size.
/// Other types use slot-based storage (slots * 8).
pub fn elem_bytes_for_heap(elem_type: TypeKey, tc_objs: &TCObjects) -> usize {
    let vk = type_value_kind(elem_type, tc_objs);
    match vk {
        // Packed: primitive types with size < 8 bytes
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        // Slot-based: all other types
        _ => type_slot_count(elem_type, tc_objs) as usize * 8,
    }
}

/// Convert a type to its ValueKind.
pub fn type_value_kind(type_key: TypeKey, tc_objs: &TCObjects) -> ValueKind {
    let underlying = typ::underlying_type(type_key, tc_objs);
    match &tc_objs.types[underlying] {
        Type::Basic(b) => {
            match b.typ() {
                BasicType::Bool | BasicType::UntypedBool => ValueKind::Bool,
                BasicType::Int | BasicType::UntypedInt => ValueKind::Int,
                BasicType::Int8 => ValueKind::Int8,
                BasicType::Int16 => ValueKind::Int16,
                BasicType::Int32 | BasicType::UntypedRune | BasicType::Rune => ValueKind::Int32,
                BasicType::Int64 => ValueKind::Int64,
                BasicType::Uint => ValueKind::Uint,
                BasicType::Uint8 | BasicType::Byte => ValueKind::Uint8,
                BasicType::Uint16 => ValueKind::Uint16,
                BasicType::Uint32 => ValueKind::Uint32,
                BasicType::Uint64 | BasicType::Uintptr => ValueKind::Uint64,
                BasicType::Float32 => ValueKind::Float32,
                BasicType::Float64 | BasicType::UntypedFloat => ValueKind::Float64,
                BasicType::Str | BasicType::UntypedString => ValueKind::String,
                BasicType::UntypedNil => ValueKind::Void,
                other => panic!("type_value_kind: unhandled BasicType {:?}", other),
            }
        }
        Type::Pointer(_) => ValueKind::Pointer,
        Type::Array(_) => ValueKind::Array,
        Type::Slice(_) => ValueKind::Slice,
        Type::Map(_) => ValueKind::Map,
        Type::Struct(_) => ValueKind::Struct,
        Type::Interface(_) => ValueKind::Interface,
        Type::Chan(_) => ValueKind::Channel,
        Type::Signature(_) => ValueKind::Closure,
        Type::Named(n) => type_value_kind(n.underlying(), tc_objs),
        other => panic!("type_value_kind: unhandled type {:?}", other),
    }
}

// === Type Query Functions ===

/// Check if type is an interface.
pub fn is_interface(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    tc_objs.types[underlying].try_as_interface().is_some()
}

/// Check if type is a pointer.
pub fn is_pointer(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    tc_objs.types[underlying].try_as_pointer().is_some()
}

/// Check if type is a struct.
pub fn is_struct(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    tc_objs.types[underlying].try_as_struct().is_some()
}

/// Check if type is an array.
pub fn is_array(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    tc_objs.types[underlying].try_as_array().is_some()
}

/// Check if type is a slice.
pub fn is_slice(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    tc_objs.types[underlying].try_as_slice().is_some()
}

/// Check if type is a map.
pub fn is_map(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    tc_objs.types[underlying].try_as_map().is_some()
}

/// Check if type is a channel.
pub fn is_chan(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    tc_objs.types[underlying].try_as_chan().is_some()
}

/// Check if type has value semantics (struct or array).
pub fn is_value_type(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    is_struct(type_key, tc_objs) || is_array(type_key, tc_objs)
}

/// Check if type is a named type.
pub fn is_named_type(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    tc_objs.types[type_key].try_as_named().is_some()
}

/// Check if type is an integer type.
pub fn is_int(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    use crate::typ::BasicInfo;
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Basic(b) = &tc_objs.types[underlying] {
        b.info() == BasicInfo::IsInteger
    } else {
        false
    }
}

/// Check if type is a float type.
pub fn is_float(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    use crate::typ::BasicType;
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Basic(b) = &tc_objs.types[underlying] {
        matches!(b.typ(), BasicType::Float32 | BasicType::Float64 | BasicType::UntypedFloat)
    } else {
        false
    }
}

/// Check if type is an unsigned integer type.
pub fn is_unsigned(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Basic(b) = &tc_objs.types[underlying] {
        b.typ().is_unsigned()
    } else {
        false
    }
}

/// Check if type is a string type.
pub fn is_string(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    use crate::typ::BasicType;
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Basic(b) = &tc_objs.types[underlying] {
        matches!(b.typ(), BasicType::Str | BasicType::UntypedString)
    } else {
        false
    }
}

/// Get integer bit size. Panics if type is not an integer type.
pub fn int_bits(type_key: TypeKey, tc_objs: &TCObjects) -> u8 {
    use crate::typ::BasicType;
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Basic(b) = &tc_objs.types[underlying] {
        match b.typ() {
            BasicType::Int8 | BasicType::Uint8 | BasicType::Byte => 8,
            BasicType::Int16 | BasicType::Uint16 => 16,
            BasicType::Int32 | BasicType::Uint32 | BasicType::Rune | BasicType::UntypedRune => 32,
            BasicType::Int64 | BasicType::Uint64 => 64,
            BasicType::Int | BasicType::Uint | BasicType::Uintptr | BasicType::UntypedInt => 64,
            other => panic!("int_bits: not an integer type {:?}", other),
        }
    } else {
        panic!("int_bits: not a Basic type")
    }
}

// === Struct Layout Functions ===

/// Get struct field offset and slot count by field name.
pub fn struct_field_offset(type_key: TypeKey, field_name: &str, tc_objs: &TCObjects) -> (u16, u16) {
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Struct(s) = &tc_objs.types[underlying] {
        let mut offset = 0u16;
        for &field_obj in s.fields() {
            let obj = &tc_objs.lobjs[field_obj];
            if let Some(field_type) = obj.typ() {
                let field_slots = type_slot_count(field_type, tc_objs);
                if obj.name() == field_name {
                    return (offset, field_slots);
                }
                offset += field_slots;
            }
        }
    }
    panic!("struct field {} not found", field_name)
}

/// Get struct field offset and slot count by field index.
pub fn struct_field_offset_by_index(type_key: TypeKey, field_index: usize, tc_objs: &TCObjects) -> (u16, u16) {
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Struct(s) = &tc_objs.types[underlying] {
        let mut offset = 0u16;
        for (i, &field_obj) in s.fields().iter().enumerate() {
            if let Some(field_type) = tc_objs.lobjs[field_obj].typ() {
                let field_slots = type_slot_count(field_type, tc_objs);
                if i == field_index {
                    return (offset, field_slots);
                }
                offset += field_slots;
            }
        }
    }
    panic!("struct field index {} not found", field_index)
}

/// Get struct field type by index.
pub fn struct_field_type_by_index(type_key: TypeKey, field_index: usize, tc_objs: &TCObjects) -> TypeKey {
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Struct(s) = &tc_objs.types[underlying] {
        if let Some(&field_obj) = s.fields().get(field_index) {
            if let Some(field_type) = tc_objs.lobjs[field_obj].typ() {
                return field_type;
            }
        }
    }
    panic!("struct field index {} not found", field_index)
}

/// Get struct field type by name.
pub fn struct_field_type(type_key: TypeKey, field_name: &str, tc_objs: &TCObjects) -> TypeKey {
    let underlying = typ::underlying_type(type_key, tc_objs);
    if let Type::Struct(s) = &tc_objs.types[underlying] {
        for &field_obj in s.fields() {
            let obj = &tc_objs.lobjs[field_obj];
            if obj.name() == field_name {
                if let Some(field_type) = obj.typ() {
                    return field_type;
                }
            }
        }
    }
    panic!("struct field {} not found", field_name)
}

/// Compute field offset using selection indices.
pub fn compute_field_offset_from_indices(base_type: TypeKey, indices: &[usize], tc_objs: &TCObjects) -> (u16, u16) {
    if indices.is_empty() {
        panic!("compute_field_offset_from_indices: empty indices");
    }
    
    let mut offset = 0u16;
    let mut current_type = base_type;
    let mut final_slots = 1u16;
    
    for (i, &idx) in indices.iter().enumerate() {
        let (field_offset, field_slots) = struct_field_offset_by_index(current_type, idx, tc_objs);
        offset += field_offset;
        
        if i == indices.len() - 1 {
            final_slots = field_slots;
        } else {
            current_type = struct_field_type_by_index(current_type, idx, tc_objs);
        }
    }
    
    (offset, final_slots)
}
