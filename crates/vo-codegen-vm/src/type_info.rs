//! Type info wrapper - provides slot layout calculation and type queries.

use vo_analysis::objects::{ObjKey, TCObjects, TypeKey};
use vo_analysis::typ::{self, Type};
use vo_analysis::Project;
use vo_syntax::ast::Ident;
use vo_syntax::ast::ExprId;
use vo_common_core::types::SlotType;

/// Wrapper around Project for codegen queries.
pub struct TypeInfoWrapper<'a> {
    pub project: &'a Project,
}

impl<'a> TypeInfoWrapper<'a> {
    pub fn new(project: &'a Project) -> Self {
        Self { project }
    }

    fn tc_objs(&self) -> &TCObjects {
        &self.project.tc_objs
    }

    fn type_info(&self) -> &vo_analysis::check::TypeInfo {
        &self.project.type_info
    }

    // === Expression type queries ===

    pub fn expr_type(&self, expr_id: ExprId) -> TypeKey {
        self.project.type_info.types.get(&expr_id)
            .map(|tv| tv.typ)
            .expect("expression must have type during codegen")
    }

    pub fn expr_slots(&self, expr_id: ExprId) -> u16 {
        self.type_slot_count(self.expr_type(expr_id))
    }

    pub fn type_expr_type(&self, type_expr_id: vo_syntax::ast::TypeExprId) -> TypeKey {
        self.project.type_info.type_exprs.get(&type_expr_id)
            .copied()
            .expect("type expression must have type during codegen")
    }

    // === Definition/Use queries ===

    pub fn get_def(&self, ident: &Ident) -> ObjKey {
        self.type_info().get_def(ident)
            .unwrap_or_else(|| panic!("identifier {:?} (id={:?}) must have definition during codegen", ident.symbol, ident.id))
    }

    pub fn get_use(&self, ident: &Ident) -> ObjKey {
        self.type_info().get_use(ident)
            .expect("identifier must have use during codegen")
    }

    /// Check if object is a package name
    pub fn obj_is_pkg(&self, obj: ObjKey) -> bool {
        self.tc_objs().lobjs[obj].entity_type().is_pkg_name()
    }

    /// Get the package path for a package identifier
    pub fn package_path(&self, ident: &Ident) -> Option<String> {
        let obj = self.get_use(ident);
        if self.obj_is_pkg(obj) {
            let pkg_key = self.tc_objs().lobjs[obj].pkg_name_imported();
            Some(self.tc_objs().pkgs[pkg_key].path().to_string())
        } else {
            None
        }
    }

    // === Escape queries ===

    pub fn is_escaped(&self, obj: ObjKey) -> bool {
        self.type_info().is_escaped(obj)
    }

    /// Get pointer element type
    pub fn pointer_elem(&self, type_key: TypeKey) -> TypeKey {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Pointer(p) = &self.tc_objs().types[underlying] {
            return p.base();
        }
        panic!("pointer_elem: not a pointer type")
    }

    // === Closure captures ===

    pub fn get_closure_captures(&self, func_lit_id: ExprId) -> Option<&Vec<ObjKey>> {
        self.type_info().closure_captures.get(&func_lit_id)
    }

    // === Selection queries ===

    pub fn get_selection(&self, expr_id: ExprId) -> Option<&vo_analysis::selection::Selection> {
        self.type_info().selections.get(&expr_id)
    }

    // === Slot layout calculation ===

    pub fn type_slot_count(&self, type_key: TypeKey) -> u16 {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        match &self.tc_objs().types[underlying] {
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
                    let field_type = self.obj_type(field_obj, "struct field must have type");
                    total += self.type_slot_count(field_type);
                }
                total
            }
            Type::Array(a) => {
                let elem_slots = self.type_slot_count(a.elem());
                let len = a.len().expect("array must have length") as u16;
                elem_slots * len
            }
            Type::Named(n) => {
                // Named type - recurse with underlying
                self.type_slot_count(n.underlying())
            }
            Type::Tuple(t) => {
                let mut total = 0u16;
                for &var in t.vars() {
                    let var_type = self.obj_type(var, "tuple element must have type");
                    total += self.type_slot_count(var_type);
                }
                total
            }
            other => panic!("type_slot_count: unhandled type {:?}", other),
        }
    }

    pub fn type_slot_types(&self, type_key: TypeKey) -> Vec<SlotType> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        match &self.tc_objs().types[underlying] {
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
                    let field_type = self.obj_type(field_obj, "struct field must have type");
                    types.extend(self.type_slot_types(field_type));
                }
                types
            }
            Type::Array(a) => {
                let elem_types = self.type_slot_types(a.elem());
                let mut types = Vec::new();
                let len = a.len().expect("array must have length") as usize;
                for _ in 0..len {
                    types.extend(elem_types.iter().cloned());
                }
                types
            }
            Type::Named(n) => self.type_slot_types(n.underlying()),
            other => panic!("type_slot_types: unhandled type {:?}", other),
        }
    }

    /// Get slots and slot_types for a type expression (used for params/results).
    pub fn type_expr_layout(&self, type_expr_id: vo_syntax::ast::TypeExprId) -> (u16, Vec<SlotType>) {
        let type_key = self.type_expr_type(type_expr_id);
        let slots = self.type_slot_count(type_key);
        let slot_types = self.type_slot_types(type_key);
        (slots, slot_types)
    }

    // === Struct layout ===

    pub fn struct_field_offset(
        &self,
        type_key: TypeKey,
        field_name: &str,
    ) -> (u16, u16) {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Struct(s) = &self.tc_objs().types[underlying] {
            let mut offset = 0u16;
            for &field_obj in s.fields() {
                let obj = &self.tc_objs().lobjs[field_obj];
                let field_type = obj.typ().expect("struct field must have type");
                let field_slots = self.type_slot_count(field_type);
                if obj.name() == field_name {
                    return (offset, field_slots);
                }
                offset += field_slots;
            }
        }
        panic!("struct field {} not found during codegen", field_name)
    }

    /// Get struct field offset by index (for positional struct literals)
    pub fn struct_field_offset_by_index(
        &self,
        type_key: TypeKey,
        field_index: usize,
    ) -> (u16, u16) {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Struct(s) = &self.tc_objs().types[underlying] {
            let mut offset = 0u16;
            for (i, &field_obj) in s.fields().iter().enumerate() {
                let obj = &self.tc_objs().lobjs[field_obj];
                let field_type = obj.typ().expect("struct field must have type");
                let field_slots = self.type_slot_count(field_type);
                if i == field_index {
                    return (offset, field_slots);
                }
                offset += field_slots;
            }
        }
        panic!("struct_field_offset_by_index: field {} not found", field_index)
    }
    
    /// Get struct field type by index (for method promotion)
    pub fn struct_field_type_by_index(
        &self,
        type_key: TypeKey,
        field_index: usize,
    ) -> TypeKey {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Struct(s) = &self.tc_objs().types[underlying] {
            if let Some(&field_obj) = s.fields().get(field_index) {
                return self.obj_type(field_obj, "struct field must have type");
            }
        }
        panic!("struct_field_type_by_index: field {} not found", field_index)
    }

    /// Compute field offset using selection indices (unified approach for all field access)
    /// This handles both direct fields (indices.len() == 1) and promoted fields (indices.len() > 1)
    pub fn compute_field_offset_from_indices(
        &self,
        base_type: TypeKey,
        indices: &[usize],
    ) -> (u16, u16) {
        if indices.is_empty() {
            panic!("compute_field_offset_from_indices: empty indices");
        }
        
        let mut offset = 0u16;
        let mut current_type = base_type;
        let mut final_slots = 1u16;
        
        for (i, &idx) in indices.iter().enumerate() {
            let (field_offset, field_slots) = self.struct_field_offset_by_index(current_type, idx);
            offset += field_offset;
            
            if i == indices.len() - 1 {
                final_slots = field_slots;
            } else {
                current_type = self.struct_field_type_by_index(current_type, idx);
            }
        }
        
        (offset, final_slots)
    }

    // === Type queries ===

    pub fn is_interface(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_interface().is_some()
    }

    pub fn is_pointer(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_pointer().is_some()
    }

    pub fn is_struct(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_struct().is_some()
    }

    pub fn is_array(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_array().is_some()
    }

    pub fn is_slice(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_slice().is_some()
    }

    pub fn is_map(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_map().is_some()
    }

    pub fn is_string(&self, type_key: TypeKey) -> bool {
        typ::is_string(type_key, self.tc_objs())
    }

    pub fn is_chan(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_chan().is_some()
    }

    /// Check if type has value semantics (struct or array - copied by value, not reference)
    pub fn is_value_type(&self, type_key: TypeKey) -> bool {
        self.is_struct(type_key) || self.is_array(type_key)
    }

    /// Check if type is a named type (Type::Named)
    pub fn is_named_type(&self, type_key: TypeKey) -> bool {
        self.tc_objs().types[type_key].try_as_named().is_some()
    }

    /// Get map key and value slot counts
    pub fn map_key_val_slots(&self, type_key: TypeKey) -> (u16, u16) {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let map_type = self.tc_objs().types[underlying].try_as_map()
            .expect("map_key_val_slots: not a map type");
        let key_slots = self.type_slot_count(map_type.key());
        let val_slots = self.type_slot_count(map_type.elem());
        (key_slots, val_slots)
    }

    /// Get map key slot types
    pub fn map_key_slot_types(&self, type_key: TypeKey) -> Vec<vo_common_core::types::SlotType> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let map_type = self.tc_objs().types[underlying].try_as_map()
            .expect("map_key_slot_types: not a map type");
        self.type_slot_types(map_type.key())
    }

    /// Get map value slot types
    pub fn map_val_slot_types(&self, type_key: TypeKey) -> Vec<vo_common_core::types::SlotType> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let map_type = self.tc_objs().types[underlying].try_as_map()
            .expect("map_val_slot_types: not a map type");
        self.type_slot_types(map_type.elem())
    }

    /// Get map key ValueKind
    pub fn map_key_value_kind(&self, type_key: TypeKey) -> vo_common_core::types::ValueKind {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let map_type = self.tc_objs().types[underlying].try_as_map()
            .expect("map_key_value_kind: not a map type");
        self.type_value_kind(map_type.key())
    }

    /// Get map value ValueKind
    pub fn map_val_value_kind(&self, type_key: TypeKey) -> vo_common_core::types::ValueKind {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let map_type = self.tc_objs().types[underlying].try_as_map()
            .expect("map_val_value_kind: not a map type");
        self.type_value_kind(map_type.elem())
    }

    /// Get ValueKind for a type
    pub fn type_value_kind(&self, type_key: TypeKey) -> vo_common_core::types::ValueKind {
        use vo_common_core::types::ValueKind;
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        match &self.tc_objs().types[underlying] {
            Type::Basic(b) => {
                use vo_analysis::typ::BasicType;
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
            Type::Named(n) => self.type_value_kind(n.underlying()),
            other => panic!("type_value_kind: unhandled type {:?}", other),
        }
    }

    /// Get object's type. Panics with the given message if object has no type.
    pub fn obj_type(&self, obj: ObjKey, msg: &str) -> TypeKey {
        self.tc_objs().lobjs[obj].typ().expect(msg)
    }

    /// Get object's name
    pub fn obj_name(&self, obj: ObjKey) -> &str {
        self.tc_objs().lobjs[obj].name()
    }

    /// Get struct field offset from pointer type
    pub fn struct_field_offset_from_ptr(
        &self,
        ptr_type: TypeKey,
        field_name: &str,
    ) -> (u16, u16) {
        let underlying = typ::underlying_type(ptr_type, self.tc_objs());
        if let Type::Pointer(p) = &self.tc_objs().types[underlying] {
            self.struct_field_offset(p.base(), field_name)
        } else {
            panic!("struct_field_offset_from_ptr: not a pointer type")
        }
    }

    /// Get array element slot count
    pub fn array_elem_slots(&self, type_key: TypeKey) -> u16 {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Array(a) = &self.tc_objs().types[underlying] {
            self.type_slot_count(a.elem())
        } else {
            panic!("array_elem_slots: not an array type")
        }
    }

    /// Get array element slot types
    pub fn array_elem_slot_types(&self, type_key: TypeKey) -> Vec<vo_common_core::types::SlotType> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Array(a) = &self.tc_objs().types[underlying] {
            self.type_slot_types(a.elem())
        } else {
            panic!("array_elem_slot_types: not an array type")
        }
    }

    /// Get slice element slot count
    pub fn slice_elem_slots(&self, type_key: TypeKey) -> u16 {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Slice(s) = &self.tc_objs().types[underlying] {
            self.type_slot_count(s.elem())
        } else {
            panic!("slice_elem_slots: not a slice type")
        }
    }

    /// Get slice element slot types
    pub fn slice_elem_slot_types(&self, type_key: TypeKey) -> Vec<vo_common_core::types::SlotType> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Slice(s) = &self.tc_objs().types[underlying] {
            self.type_slot_types(s.elem())
        } else {
            panic!("slice_elem_slot_types: not a slice type")
        }
    }

    /// Get array length
    pub fn array_len(&self, type_key: TypeKey) -> u64 {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Array(a) = &self.tc_objs().types[underlying] {
            a.len().expect("array must have length")
        } else {
            panic!("array_len: not an array type")
        }
    }

    /// Get array element type
    pub fn array_elem_type(&self, type_key: TypeKey) -> TypeKey {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Array(a) = &self.tc_objs().types[underlying] {
            a.elem()
        } else {
            panic!("array_elem_type: not an array type")
        }
    }

    /// Get pointer element slot count
    pub fn pointer_elem_slots(&self, type_key: TypeKey) -> u16 {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Pointer(p) = &self.tc_objs().types[underlying] {
            self.type_slot_count(p.base())
        } else {
            panic!("pointer_elem_slots: not a pointer type")
        }
    }

    /// Get method index in interface
    pub fn get_interface_method_index(&self, iface_type: TypeKey, method_name: &str) -> u16 {
        let underlying = typ::underlying_type(iface_type, self.tc_objs());
        if let Type::Interface(iface) = &self.tc_objs().types[underlying] {
            for (idx, method) in iface.methods().iter().enumerate() {
                if self.obj_name(*method) == method_name {
                    return idx as u16;
                }
            }
        }
        panic!("get_interface_method_index: method {} not found", method_name)
    }

    /// Get channel element slot count
    pub fn chan_elem_slots(&self, type_key: TypeKey) -> u16 {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Chan(c) = &self.tc_objs().types[underlying] {
            self.type_slot_count(c.elem())
        } else {
            panic!("chan_elem_slots: not a channel type")
        }
    }

    /// Get signature details for a function type
    pub fn as_signature(&self, type_key: TypeKey) -> &typ::SignatureDetail {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_signature()
            .expect("as_signature: not a signature type")
    }

    /// Check if function signature is variadic
    pub fn is_variadic(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Signature(sig) = &self.tc_objs().types[underlying] {
            sig.variadic()
        } else {
            false
        }
    }

    /// Get parameter types for a function signature
    pub fn func_param_types(&self, type_key: TypeKey) -> Vec<TypeKey> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Signature(sig) = &self.tc_objs().types[underlying] {
            let params_key = sig.params();
            if let Type::Tuple(tuple) = &self.tc_objs().types[params_key] {
                tuple.vars().iter()
                    .filter_map(|&var_key| self.tc_objs().lobjs[var_key].typ())
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        }
    }

    /// Check if type is an integer type
    pub fn is_int(&self, type_key: TypeKey) -> bool {
        use vo_analysis::typ::BasicType;
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Basic(b) = &self.tc_objs().types[underlying] {
            matches!(b.typ(),
                BasicType::Int | BasicType::Int8 | BasicType::Int16 | BasicType::Int32 | BasicType::Int64 |
                BasicType::Uint | BasicType::Uint8 | BasicType::Uint16 | BasicType::Uint32 | BasicType::Uint64 |
                BasicType::UntypedInt | BasicType::UntypedRune)
        } else {
            false
        }
    }

    /// Check if type is a float type
    pub fn is_float(&self, type_key: TypeKey) -> bool {
        use vo_analysis::typ::BasicType;
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Basic(b) = &self.tc_objs().types[underlying] {
            matches!(b.typ(), BasicType::Float32 | BasicType::Float64 | BasicType::UntypedFloat)
        } else {
            false
        }
    }

    /// Get integer bit size. Panics if type is not an integer type.
    pub fn int_bits(&self, type_key: TypeKey) -> u8 {
        use vo_analysis::typ::BasicType;
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Basic(b) = &self.tc_objs().types[underlying] {
            match b.typ() {
                BasicType::Int8 | BasicType::Uint8 => 8,
                BasicType::Int16 | BasicType::Uint16 => 16,
                BasicType::Int32 | BasicType::Uint32 | BasicType::UntypedRune => 32,
                BasicType::Int64 | BasicType::Uint64 => 64,
                BasicType::Int | BasicType::Uint | BasicType::UntypedInt => 64, // assume 64-bit platform
                other => panic!("int_bits: not an integer type {:?}", other),
            }
        } else {
            panic!("int_bits: not a Basic type")
        }
    }

    /// Get base type from pointer type
    pub fn pointer_base(&self, ptr_type: TypeKey) -> TypeKey {
        let underlying = typ::underlying_type(ptr_type, self.tc_objs());
        if let Type::Pointer(p) = &self.tc_objs().types[underlying] {
            p.base()
        } else {
            panic!("pointer_base: not a pointer type")
        }
    }
}

/// Encode i32 as two u16 values (low, high)
pub fn encode_i32(val: i32) -> (u16, u16) {
    let bits = val as u32;
    ((bits & 0xFFFF) as u16, ((bits >> 16) & 0xFFFF) as u16)
}

// === Meta encoding helpers ===

/// Encode MapSet meta: (key_slots << 8) | val_slots
#[inline]
pub fn encode_map_set_meta(key_slots: u16, val_slots: u16) -> u32 {
    ((key_slots as u32) << 8) | (val_slots as u32)
}

/// Encode MapGet meta: (key_slots << 16) | (val_slots << 1) | has_ok
#[inline]
pub fn encode_map_get_meta(key_slots: u16, val_slots: u16, has_ok: bool) -> u32 {
    ((key_slots as u32) << 16) | ((val_slots as u32) << 1) | (has_ok as u32)
}

/// Encode MapNew slots: (key_slots << 8) | val_slots
#[inline]
pub fn encode_map_new_slots(key_slots: u16, val_slots: u16) -> u16 {
    ((key_slots as u16) << 8) | (val_slots as u16)
}

/// Encode Call args: (arg_slots << 8) | ret_slots
#[inline]
pub fn encode_call_args(arg_slots: u16, ret_slots: u16) -> u16 {
    (arg_slots << 8) | ret_slots
}

/// Encode func_id for Call instruction
#[inline]
pub fn encode_func_id(func_idx: u32) -> (u16, u8) {
    let low = (func_idx & 0xFFFF) as u16;
    let high = ((func_idx >> 16) & 0xFF) as u8;
    (low, high)
}

/// Encode iterator meta: (key_slots << 8) | val_slots
#[inline]
pub fn encode_iter_meta(key_slots: u16, val_slots: u16) -> u64 {
    ((key_slots as u64) << 8) | (val_slots as u64)
}
