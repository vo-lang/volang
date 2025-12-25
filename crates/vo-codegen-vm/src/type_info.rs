//! Type info wrapper - provides slot layout calculation and type queries.

use vo_analysis::objects::{ObjKey, TCObjects, TypeKey};
use vo_analysis::typ::{self, Type};
use vo_analysis::Project;
use vo_common::symbol::Ident;
use vo_common_core::ExprId;
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

    pub fn expr_type(&self, expr_id: ExprId) -> Option<TypeKey> {
        self.project.type_info.types.get(&expr_id).map(|tv| tv.typ)
    }

    pub fn type_expr_type(&self, type_expr_id: vo_common_core::TypeExprId) -> Option<TypeKey> {
        self.project.type_info.type_exprs.get(&type_expr_id).copied()
    }

    pub fn expr_mode(&self, expr_id: ExprId) -> Option<&vo_analysis::operand::OperandMode> {
        self.type_info().expr_mode(expr_id)
    }

    // === Definition/Use queries ===

    pub fn get_def(&self, ident: &Ident) -> Option<ObjKey> {
        self.type_info().get_def(ident)
    }

    pub fn get_use(&self, ident: &Ident) -> Option<ObjKey> {
        self.type_info().get_use(ident)
    }

    /// Check if an identifier refers to a package
    pub fn is_package(&self, ident: &Ident) -> bool {
        if let Some(obj) = self.get_use(ident) {
            self.tc_objs().lobjs[obj].entity_type().is_pkg_name()
        } else if let Some(obj) = self.get_def(ident) {
            self.tc_objs().lobjs[obj].entity_type().is_pkg_name()
        } else {
            false
        }
    }

    /// Get the package path for a package identifier
    pub fn package_path(&self, ident: &Ident) -> Option<String> {
        let obj = self.get_use(ident).or_else(|| self.get_def(ident))?;
        let lobj = &self.tc_objs().lobjs[obj];
        if lobj.entity_type().is_pkg_name() {
            let pkg_key = lobj.pkg_name_imported();
            Some(self.tc_objs().pkgs[pkg_key].path().to_string())
        } else {
            None
        }
    }

    // === Escape queries ===

    pub fn is_escaped(&self, obj: ObjKey) -> bool {
        self.type_info().is_escaped(obj)
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
                    if let Some(field_type) = self.tc_objs().lobjs[field_obj].typ() {
                        total += self.type_slot_count(field_type);
                    }
                }
                total
            }
            Type::Array(a) => {
                let elem_slots = self.type_slot_count(a.elem());
                let len = a.len().unwrap_or(0) as u16;
                elem_slots * len
            }
            _ => 1,
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
                    if let Some(field_type) = self.tc_objs().lobjs[field_obj].typ() {
                        types.extend(self.type_slot_types(field_type));
                    }
                }
                types
            }
            Type::Array(a) => {
                let elem_types = self.type_slot_types(a.elem());
                let mut types = Vec::new();
                let len = a.len().unwrap_or(0) as usize;
                for _ in 0..len {
                    types.extend(elem_types.iter().cloned());
                }
                types
            }
            _ => vec![SlotType::Value],
        }
    }

    /// Get slots and slot_types for a type expression (used for params/results).
    /// Returns (slots, slot_types) with default fallback if type is unknown.
    pub fn type_expr_layout(&self, type_expr_id: vo_common_core::TypeExprId) -> (u16, Vec<SlotType>) {
        let type_key = self.project.type_info.type_exprs.get(&type_expr_id).copied();
        let slots = type_key.map(|t| self.type_slot_count(t)).unwrap_or(1);
        let slot_types = type_key
            .map(|t| self.type_slot_types(t))
            .unwrap_or_else(|| vec![SlotType::Value]);
        (slots, slot_types)
    }

    // === Struct layout ===

    pub fn struct_field_offset(
        &self,
        type_key: TypeKey,
        field_name: &str,
    ) -> Option<(u16, u16)> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Struct(s) = &self.tc_objs().types[underlying] {
            let mut offset = 0u16;
            for &field_obj in s.fields() {
                let obj = &self.tc_objs().lobjs[field_obj];
                let field_type = obj.typ()?;
                let field_slots = self.type_slot_count(field_type);
                if obj.name() == field_name {
                    return Some((offset, field_slots));
                }
                offset += field_slots;
            }
        }
        None
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

    /// Get map key and value slot counts
    pub fn map_key_val_slots(&self, type_key: TypeKey) -> Option<(u16, u16)> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let map_type = self.tc_objs().types[underlying].try_as_map()?;
        let key_slots = self.type_slot_count(map_type.key());
        let val_slots = self.type_slot_count(map_type.elem());
        Some((key_slots, val_slots))
    }

    /// Get object's type
    pub fn obj_type(&self, obj: ObjKey) -> Option<TypeKey> {
        self.tc_objs().lobjs[obj].typ()
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
    ) -> Option<(u16, u16)> {
        let underlying = typ::underlying_type(ptr_type, self.tc_objs());
        if let Type::Pointer(p) = &self.tc_objs().types[underlying] {
            self.struct_field_offset(p.base(), field_name)
        } else {
            None
        }
    }

    /// Get array element slot count
    pub fn array_elem_slots(&self, type_key: TypeKey) -> Option<u16> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Array(a) = &self.tc_objs().types[underlying] {
            Some(self.type_slot_count(a.elem()))
        } else {
            None
        }
    }

    /// Get array element slot types
    pub fn array_elem_slot_types(&self, type_key: TypeKey) -> Option<Vec<vo_common_core::types::SlotType>> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Array(a) = &self.tc_objs().types[underlying] {
            Some(self.type_slot_types(a.elem()))
        } else {
            None
        }
    }

    /// Get slice element slot count
    pub fn slice_elem_slots(&self, type_key: TypeKey) -> Option<u16> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Slice(s) = &self.tc_objs().types[underlying] {
            Some(self.type_slot_count(s.elem()))
        } else {
            None
        }
    }

    /// Get array length
    pub fn array_len(&self, type_key: TypeKey) -> Option<u64> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Array(a) = &self.tc_objs().types[underlying] {
            a.len()
        } else {
            None
        }
    }

    /// Get array element type
    pub fn array_elem_type(&self, type_key: TypeKey) -> Option<TypeKey> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Array(a) = &self.tc_objs().types[underlying] {
            Some(a.elem())
        } else {
            None
        }
    }

    /// Get pointer element slot count
    pub fn pointer_elem_slots(&self, type_key: TypeKey) -> Option<u16> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Pointer(p) = &self.tc_objs().types[underlying] {
            Some(self.type_slot_count(p.base()))
        } else {
            None
        }
    }

    /// Get interface meta ID (for IfaceAssign)
    /// Note: This needs the CodegenContext to lookup registered metas
    /// For now, return None - the actual lookup happens in context.rs
    pub fn get_interface_meta_id(&self, _type_key: TypeKey) -> Option<u16> {
        // Interface meta ID lookup is done via CodegenContext::get_interface_meta_id
        // This method is kept for API compatibility but actual lookup should use ctx
        None
    }

    /// Get value kind for a type (returns ValueKind enum value)
    pub fn value_kind(&self, type_key: TypeKey) -> u8 {
        use vo_analysis::typ::BasicType;
        use vo_common_core::types::ValueKind;
        
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let vk = match &self.tc_objs().types[underlying] {
            Type::Basic(b) => match b.typ() {
                BasicType::Bool | BasicType::UntypedBool => ValueKind::Bool,
                BasicType::Int | BasicType::UntypedInt => ValueKind::Int,
                BasicType::Int8 => ValueKind::Int8,
                BasicType::Int16 => ValueKind::Int16,
                BasicType::Int32 | BasicType::Rune | BasicType::UntypedRune => ValueKind::Int32,
                BasicType::Int64 => ValueKind::Int64,
                BasicType::Uint => ValueKind::Uint,
                BasicType::Uint8 | BasicType::Byte => ValueKind::Uint8,
                BasicType::Uint16 => ValueKind::Uint16,
                BasicType::Uint32 => ValueKind::Uint32,
                BasicType::Uint64 | BasicType::Uintptr => ValueKind::Uint64,
                BasicType::Float32 => ValueKind::Float32,
                BasicType::Float64 | BasicType::UntypedFloat => ValueKind::Float64,
                BasicType::Str | BasicType::UntypedString => ValueKind::String,
                _ => ValueKind::Void,
            },
            Type::Pointer(_) => ValueKind::Pointer,
            Type::Struct(_) => ValueKind::Struct,
            Type::Array(_) => ValueKind::Array,
            Type::Slice(_) => ValueKind::Slice,
            Type::Map(_) => ValueKind::Map,
            Type::Chan(_) => ValueKind::Channel,
            Type::Interface(_) => ValueKind::Interface,
            Type::Signature(_) => ValueKind::Closure,
            _ => ValueKind::Void,
        };
        vk as u8
    }

    /// Get method index in interface
    pub fn get_interface_method_index(&self, iface_type: TypeKey, method_name: &str) -> Option<u16> {
        let underlying = typ::underlying_type(iface_type, self.tc_objs());
        if let Type::Interface(iface) = &self.tc_objs().types[underlying] {
            for (idx, method) in iface.methods().iter().enumerate() {
                if self.tc_objs().lobjs[*method].name() == method_name {
                    return Some(idx as u16);
                }
            }
        }
        None
    }

    /// Get channel element slot count
    pub fn chan_elem_slots(&self, type_key: TypeKey) -> Option<u16> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Chan(c) = &self.tc_objs().types[underlying] {
            Some(self.type_slot_count(c.elem()))
        } else {
            None
        }
    }

    /// Get channel element type
    pub fn chan_elem_type(&self, type_key: TypeKey) -> Option<TypeKey> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Chan(c) = &self.tc_objs().types[underlying] {
            Some(c.elem())
        } else {
            None
        }
    }

    /// Check if type is a function/signature type
    pub fn is_func(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_signature().is_some()
    }

    /// Get function signature return slot count
    pub fn func_ret_slots(&self, type_key: TypeKey) -> Option<u16> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Signature(sig) = &self.tc_objs().types[underlying] {
            let results = sig.results();
            if let Some(tuple) = self.tc_objs().types[results].try_as_tuple() {
                let mut total = 0u16;
                for &var in tuple.vars() {
                    if let Some(t) = self.tc_objs().lobjs[var].typ() {
                        total += self.type_slot_count(t);
                    }
                }
                return Some(total);
            }
        }
        None
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

    /// Get variadic element type (the element type of the ...T parameter)
    pub fn variadic_elem_type(&self, type_key: TypeKey) -> Option<TypeKey> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Signature(sig) = &self.tc_objs().types[underlying] {
            if sig.variadic() {
                let params = sig.params();
                if let Some(tuple) = self.tc_objs().types[params].try_as_tuple() {
                    let vars = tuple.vars();
                    if let Some(&last_var) = vars.last() {
                        if let Some(param_type) = self.tc_objs().lobjs[last_var].typ() {
                            // The last param is []T, get element type
                            let underlying_param = typ::underlying_type(param_type, self.tc_objs());
                            if let Type::Slice(s) = &self.tc_objs().types[underlying_param] {
                                return Some(s.elem());
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Get the number of fixed (non-variadic) parameters
    pub fn fixed_param_count(&self, type_key: TypeKey) -> Option<usize> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Signature(sig) = &self.tc_objs().types[underlying] {
            let params = sig.params();
            if let Some(tuple) = self.tc_objs().types[params].try_as_tuple() {
                let count = tuple.vars().len();
                if sig.variadic() {
                    return Some(count.saturating_sub(1));
                } else {
                    return Some(count);
                }
            }
        }
        None
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

    /// Get integer bit size
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
                _ => 64,
            }
        } else {
            64
        }
    }

    /// Check if type is a bool type
    pub fn is_bool(&self, type_key: TypeKey) -> bool {
        use vo_analysis::typ::BasicType;
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Basic(b) = &self.tc_objs().types[underlying] {
            matches!(b.typ(), BasicType::Bool | BasicType::UntypedBool)
        } else {
            false
        }
    }

    /// Get pointer type for a given base type (for method lookup)
    pub fn pointer_to(&self, base_type: TypeKey) -> Option<TypeKey> {
        // Search for a pointer type that points to base_type
        for (key, typ) in self.tc_objs().types.iter() {
            if let Type::Pointer(p) = typ {
                if p.base() == base_type {
                    return Some(key);
                }
            }
        }
        None
    }

    /// Get base type from pointer type
    pub fn pointer_base(&self, ptr_type: TypeKey) -> Option<TypeKey> {
        let underlying = typ::underlying_type(ptr_type, self.tc_objs());
        if let Type::Pointer(p) = &self.tc_objs().types[underlying] {
            Some(p.base())
        } else {
            None
        }
    }
}

/// Encode i32 as two u16 values (low, high)
pub fn encode_i32(val: i32) -> (u16, u16) {
    let bits = val as u32;
    ((bits & 0xFFFF) as u16, ((bits >> 16) & 0xFFFF) as u16)
}
