//! Type info wrapper - provides slot layout calculation and type queries.
//! 
//! This module provides a convenient wrapper around Project for codegen queries.
//! Core type layout functions are in vo_analysis::check::type_info.

use vo_analysis::objects::{ObjKey, TCObjects, TypeKey};
use vo_analysis::typ::{self, Type};
use vo_analysis::Project;
use vo_analysis::check::type_info as type_layout;
use vo_syntax::ast::Ident;
use vo_syntax::ast::ExprId;
use vo_runtime::SlotType;

/// Describes how call arguments should be compiled.
/// If `tuple_expand` is Some, the single argument is a tuple that needs expansion.
pub struct CallArgInfo {
    /// Expanded argument types (after tuple expansion if applicable)
    pub arg_types: Vec<TypeKey>,
    /// If Some, the single AST argument is a tuple and should be expanded.
    /// The value is the tuple type to expand.
    pub tuple_expand: Option<TypeKey>,
}

/// Wrapper around Project for codegen queries.
/// 
/// Each package has its own TypeInfo (because ExprId/IdentId are local to each package).
/// When compiling a package, use the TypeInfoWrapper for that package.
pub struct TypeInfoWrapper<'a> {
    pub project: &'a Project,
    /// The type_info for the package being compiled.
    type_info: &'a vo_analysis::check::TypeInfo,
}

impl<'a> TypeInfoWrapper<'a> {
    /// Create a TypeInfoWrapper for the main package.
    pub fn for_main_package(project: &'a Project) -> Self {
        Self { project, type_info: &project.type_info }
    }
    
    /// Create a TypeInfoWrapper for an imported package.
    pub fn for_package(project: &'a Project, type_info: &'a vo_analysis::check::TypeInfo) -> Self {
        Self { project, type_info }
    }

    pub fn tc_objs(&self) -> &TCObjects {
        &self.project.tc_objs
    }

    fn type_info(&self) -> &vo_analysis::check::TypeInfo {
        self.type_info
    }

    // === Expression type queries ===

    pub fn expr_type(&self, expr_id: ExprId) -> TypeKey {
        self.type_info().types.get(&expr_id)
            .map(|tv| tv.typ)
            .unwrap_or_else(|| panic!("expression {:?} must have type during codegen", expr_id))
    }

    /// Get the i-th element type from a tuple type (for comma-ok expressions)
    pub fn tuple_elem_type(&self, type_key: TypeKey, i: usize) -> TypeKey {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Tuple(tuple) = &self.tc_objs().types[underlying] {
            let vars = tuple.vars();
            if i < vars.len() {
                return self.tc_objs().lobjs[vars[i]].typ().expect("tuple element must have type");
            }
        }
        panic!("tuple_elem_type: not a tuple or index out of bounds")
    }

    /// Check if type is a tuple (comma-ok result)
    pub fn is_tuple(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        matches!(&self.tc_objs().types[underlying], Type::Tuple(_))
    }

    /// Get the number of elements in a tuple type
    pub fn tuple_len(&self, type_key: TypeKey) -> usize {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Tuple(tuple) = &self.tc_objs().types[underlying] {
            tuple.vars().len()
        } else {
            panic!("tuple_len: not a tuple type")
        }
    }

    /// Check if type is (any, error) tuple - used for dynamic access short-circuiting
    pub fn is_tuple_any_error(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let Type::Tuple(tuple) = &self.tc_objs().types[underlying] else { return false };
        let vars = tuple.vars();
        if vars.len() != 2 { return false }
        
        let Some(first_type) = self.tc_objs().lobjs[vars[0]].typ() else { return false };
        let first_underlying = typ::underlying_type(first_type, self.tc_objs());
        let is_interface = matches!(&self.tc_objs().types[first_underlying], Type::Interface(_));
        
        let Some(second_type) = self.tc_objs().lobjs[vars[1]].typ() else { return false };
        let Some(ref universe) = self.project.tc_objs.universe else { return false };
        let is_error = typ::identical(second_type, universe.error_type(), self.tc_objs());
        
        is_interface && is_error
    }
    
    /// Get expected return count for dynamic access expression.
    /// Expression type is a tuple: (any, any, ..., error) where last element is error.
    /// Returns the number of return values (tuple length - 1 for error).
    pub fn get_dyn_access_ret_count(&self, type_key: TypeKey) -> u16 {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let Type::Tuple(tuple) = &self.tc_objs().types[underlying] else { return 1 };
        let vars = tuple.vars();
        if vars.is_empty() { return 1 }
        // Last element is error, so ret_count = len - 1
        (vars.len() - 1) as u16
    }
    
    /// Get return types for dynamic access expression (excluding error).
    /// Returns a list of TypeKeys for each return value.
    pub fn get_dyn_access_ret_types(&self, type_key: TypeKey) -> Vec<TypeKey> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let Type::Tuple(tuple) = &self.tc_objs().types[underlying] else { 
            return vec![]; 
        };
        let vars = tuple.vars();
        if vars.is_empty() { return vec![]; }
        // All elements except last (which is error)
        vars[..vars.len() - 1]
            .iter()
            .filter_map(|&var| self.tc_objs().lobjs[var].typ())
            .collect()
    }
    
    /// Calculate total dst slots for dynamic access return types.
    /// Each any type takes 2 slots, each typed value takes its actual slot count.
    pub fn dyn_access_dst_slots(&self, ret_types: &[TypeKey]) -> u16 {
        ret_types.iter()
            .map(|&t| if self.is_any_type(t) { 2 } else { self.type_slot_count(t) })
            .sum()
    }
    
    /// Check if a type is the empty interface (any).
    pub fn is_any_type(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Interface(iface) = &self.tc_objs().types[underlying] {
            iface.methods().is_empty() && iface.embeddeds().is_empty()
        } else {
            false
        }
    }
    
    /// Convert TypeKey to RuntimeType.
    /// This is the unified entry point for type conversion in codegen.
    pub fn type_to_runtime_type(&self, type_key: TypeKey, ctx: &mut crate::context::CodegenContext) -> vo_runtime::RuntimeType {
        use vo_runtime::{RuntimeType, ValueKind};
        
        // Check if it's a Named type - use ObjKey (the true identity) for lookup
        let tc_objs = self.tc_objs();
        if let Type::Named(named) = &tc_objs.types[type_key] {
            if let Some(obj_key) = named.obj() {
                if let Some(id) = ctx.get_named_type_id(*obj_key) {
                    let struct_meta_id = ctx.get_struct_meta_id(type_key);
                    return RuntimeType::Named { id, struct_meta_id };
                }
            }
            // Named type not registered - recurse on underlying type
            return self.type_to_runtime_type(named.underlying(), ctx);
        }
        
        match self.type_value_kind(type_key) {
            vk @ (ValueKind::Int | ValueKind::Int8 | ValueKind::Int16 | ValueKind::Int32 | ValueKind::Int64 |
                  ValueKind::Uint | ValueKind::Uint8 | ValueKind::Uint16 | ValueKind::Uint32 | ValueKind::Uint64 |
                  ValueKind::Float32 | ValueKind::Float64 | ValueKind::Bool | ValueKind::String) => {
                RuntimeType::Basic(vk)
            }
            ValueKind::Struct | ValueKind::Array | ValueKind::Interface => {
                let rttid = ctx.intern_type_key(type_key, self);
                ctx.runtime_type(rttid).clone()
            }
            ValueKind::Pointer => RuntimeType::Pointer(self.intern_value_rttid(self.pointer_elem(type_key), ctx)),
            ValueKind::Slice => RuntimeType::Slice(self.intern_value_rttid(self.slice_elem_type(type_key), ctx)),
            ValueKind::Map => {
                let (key_type, val_type) = self.map_key_val_types(type_key);
                RuntimeType::Map {
                    key: self.intern_value_rttid(key_type, ctx),
                    val: self.intern_value_rttid(val_type, ctx),
                }
            }
            ValueKind::Channel => {
                RuntimeType::Chan {
                    dir: self.chan_dir(type_key),
                    elem: self.intern_value_rttid(self.chan_elem_type(type_key), ctx),
                }
            }
            ValueKind::Closure => {
                let underlying = typ::underlying_type(type_key, tc_objs);
                if let Type::Signature(sig) = &tc_objs.types[underlying] {
                    RuntimeType::Func {
                        params: self.tuple_to_value_rttids(sig.params(), ctx),
                        results: self.tuple_to_value_rttids(sig.results(), ctx),
                        variadic: sig.variadic(),
                    }
                } else {
                    RuntimeType::Func { params: Vec::new(), results: Vec::new(), variadic: false }
                }
            }
            _ => RuntimeType::Basic(ValueKind::Void),
        }
    }
    
    /// Intern a type and return its ValueRttid.
    #[inline]
    fn intern_value_rttid(&self, type_key: TypeKey, ctx: &mut crate::context::CodegenContext) -> vo_runtime::ValueRttid {
        vo_runtime::ValueRttid::new(ctx.intern_type_key(type_key, self), self.type_value_kind(type_key))
    }
    
    /// Convert tuple type to Vec<ValueRttid>
    fn tuple_to_value_rttids(&self, tuple_key: TypeKey, ctx: &mut crate::context::CodegenContext) -> Vec<vo_runtime::ValueRttid> {
        use vo_runtime::ValueRttid;
        let tc_objs = self.tc_objs();
        if let Type::Tuple(tuple) = &tc_objs.types[tuple_key] {
            tuple.vars().iter()
                .filter_map(|&v| {
                    let obj = &tc_objs.lobjs[v];
                    obj.typ().map(|t| {
                        let rttid = ctx.intern_type_key(t, self);
                        let vk = self.type_value_kind(t);
                        ValueRttid::new(rttid, vk)
                    })
                })
                .collect()
        } else {
            Vec::new()
        }
    }
    
    /// Get or create interface meta ID (simplified API - no need to pass tc_objs/interner).
    pub fn get_or_create_interface_meta_id(&self, type_key: TypeKey, ctx: &mut crate::context::CodegenContext) -> u32 {
        ctx.get_or_create_interface_meta_id(type_key, &self.project.tc_objs, &self.project.interner)
    }
    
    /// Get method index in InterfaceMeta (for CallIface). Uses registered meta order.
    pub fn get_iface_meta_method_index(&self, type_key: TypeKey, method_name: &str, ctx: &mut crate::context::CodegenContext) -> u32 {
        ctx.get_interface_method_index(type_key, method_name, &self.project.tc_objs, &self.project.interner)
    }

    /// Lookup a type by package path and name.
    /// Returns None if the package is not imported or type doesn't exist.
    pub fn lookup_pkg_type(&self, pkg_path: &str, type_name: &str) -> Option<TypeKey> {
        let tc_objs = &self.project.tc_objs;
        let pkg = tc_objs.find_package_by_path(pkg_path)?;
        let scope_key = *tc_objs.pkgs[pkg].scope();
        let scope = &tc_objs.scopes[scope_key];
        let obj = scope.lookup(type_name)?;
        tc_objs.lobjs[obj].typ()
    }

    /// Get the empty interface type (any) from Universe.
    pub fn any_type(&self) -> TypeKey {
        self.tc_objs().universe().any_type()
    }

    pub fn expr_type_raw(&self, expr_id: ExprId) -> TypeKey {
        self.type_info().types.get(&expr_id)
            .map(|tv| tv.typ)
            .expect("expression must have type during codegen")
    }

    pub fn expr_slots(&self, expr_id: ExprId) -> u16 {
        self.type_slot_count(self.expr_type(expr_id))
    }

    pub fn type_expr_type(&self, type_expr_id: vo_syntax::ast::TypeExprId) -> TypeKey {
        self.type_info().type_exprs.get(&type_expr_id)
            .copied()
            .expect("type expression must have type during codegen")
    }
    
    /// Get constant value for an expression (if it's a constant)
    pub fn const_value(&self, expr_id: ExprId) -> Option<&vo_analysis::ConstValue> {
        let tv = self.type_info().types.get(&expr_id)?;
        if let vo_analysis::operand::OperandMode::Constant(val) = &tv.mode {
            Some(val)
        } else {
            None
        }
    }
    
    /// Try to get constant integer value from an expression
    pub fn try_const_int(&self, expr: &vo_syntax::ast::Expr) -> Option<i64> {
        let val = self.const_value(expr.id)?;
        match val {
            vo_analysis::ConstValue::Int64(i) => Some(*i),
            _ => None,
        }
    }
    
    /// Get the simple name of a type (for embedded field names).
    /// For Named types, returns the type name. For pointer to Named, returns the base type name.
    pub fn get_type_name(&self, type_key: TypeKey) -> String {
        let tc_objs = self.tc_objs();
        let mut tk = type_key;
        
        // Strip pointer if present
        if let Type::Pointer(p) = &tc_objs.types[tk] {
            tk = p.base();
        }
        
        // Get name from Named type
        if let Type::Named(named) = &tc_objs.types[tk] {
            if let Some(obj_key) = named.obj() {
                return tc_objs.lobjs[*obj_key].name().to_string();
            }
        }
        
        // Fallback for anonymous types
        "?".to_string()
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

    /// Check if a function object has a body (is not extern).
    pub fn func_has_body(&self, obj: ObjKey) -> bool {
        self.tc_objs().lobjs[obj].entity_type().func_has_body()
    }

    /// Check if identifier is a definition (vs re-assignment)
    pub fn is_def(&self, ident: &Ident) -> bool {
        self.type_info().defs.contains_key(&ident.id)
    }

    /// Get closure captures for a function literal expression
    pub fn closure_captures(&self, expr_id: ExprId) -> Vec<vo_analysis::objects::ObjKey> {
        self.type_info().closure_captures.get(&expr_id)
            .cloned()
            .unwrap_or_default()
    }

    /// Get global variable initialization order (only valid for main package)
    pub fn init_order(&self) -> &[vo_analysis::check::Initializer] {
        &self.type_info().init_order
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

    /// Check if a variable is captured by any closure
    pub fn is_captured_by_closure(&self, obj: ObjKey) -> bool {
        self.type_info().closure_captures.values()
            .any(|captures| captures.contains(&obj))
    }

    /// Determine if a variable needs boxing based on type and escape analysis.
    /// 
    /// Boxing rules:
    /// - Reference types: only box when captured by closure (to share storage location)
    /// - Non-reference types: box when escaped for any reason
    pub fn needs_boxing(&self, obj: ObjKey, type_key: TypeKey) -> bool {
        if self.is_reference_type(type_key) {
            self.is_captured_by_closure(obj)
        } else {
            self.is_escaped(obj)
        }
    }

    // === Selection queries ===

    pub fn get_selection(&self, expr_id: ExprId) -> Option<&vo_analysis::selection::Selection> {
        self.type_info().selections.get(&expr_id)
    }

    // === Dynamic access queries ===

    /// Get resolved protocol method for a dynamic access expression.
    /// Returns Some(Some(resolve)) for static dispatch, Some(None) for dynamic dispatch, None if not recorded.
    pub fn get_dyn_access_resolve(&self, expr_id: ExprId) -> Option<&Option<vo_analysis::check::type_info::DynAccessResolve>> {
        self.type_info().dyn_access_methods.get(&expr_id)
    }

    // === Slot layout calculation (delegates to vo_analysis::check::type_info) ===

    pub fn type_slot_count(&self, type_key: TypeKey) -> u16 {
        type_layout::type_slot_count(type_key, self.tc_objs())
    }

    pub fn type_slot_types(&self, type_key: TypeKey) -> Vec<SlotType> {
        type_layout::type_slot_types(type_key, self.tc_objs())
    }

    /// Get ValueKind for each slot in a composite type (struct/array).
    /// Used for comparison to distinguish string GcRefs from others.
    pub fn type_slot_value_kinds(&self, type_key: TypeKey) -> Vec<vo_runtime::ValueKind> {
        use vo_analysis::typ::{Type, underlying_type};
        
        let underlying = underlying_type(type_key, self.tc_objs());
        match &self.tc_objs().types[underlying] {
            Type::Struct(st) => {
                let mut result = Vec::new();
                for field_obj in st.fields().iter() {
                    if let Some(field_type) = self.tc_objs().lobjs[*field_obj].typ() {
                        let field_vk = self.type_value_kind(field_type);
                        let field_slots = self.type_slot_count(field_type);
                        for _ in 0..field_slots {
                            result.push(field_vk);
                        }
                    }
                }
                result
            }
            Type::Array(arr) => {
                let elem_vk = self.type_value_kind(arr.elem());
                let elem_slots = self.type_slot_count(arr.elem());
                let len = arr.len().unwrap_or(0) as usize;
                let mut result = Vec::with_capacity(len * elem_slots as usize);
                for _ in 0..len {
                    for _ in 0..elem_slots {
                        result.push(elem_vk);
                    }
                }
                result
            }
            _ => vec![self.type_value_kind(type_key)],
        }
    }

    /// Get slots and slot_types for a type expression (used for params/results).
    pub fn type_expr_layout(&self, type_expr_id: vo_syntax::ast::TypeExprId) -> (u16, Vec<SlotType>) {
        let type_key = self.type_expr_type(type_expr_id);
        let slots = self.type_slot_count(type_key);
        let slot_types = self.type_slot_types(type_key);
        (slots, slot_types)
    }

    // === Struct layout (delegates to vo_analysis::check::type_info) ===

    pub fn struct_field_offset(&self, type_key: TypeKey, field_name: &str) -> (u16, u16) {
        type_layout::struct_field_offset(type_key, field_name, self.tc_objs())
    }

    pub fn struct_field_offset_by_index(&self, type_key: TypeKey, field_index: usize) -> (u16, u16) {
        type_layout::struct_field_offset_by_index(type_key, field_index, self.tc_objs())
    }
    
    pub fn struct_field_type_by_index(&self, type_key: TypeKey, field_index: usize) -> TypeKey {
        type_layout::struct_field_type_by_index(type_key, field_index, self.tc_objs())
    }
    
    /// Get struct field offset, slots, and type by field name
    pub fn struct_field_offset_with_type(&self, type_key: TypeKey, field_name: &str) -> (u16, u16, TypeKey) {
        let (offset, slots) = type_layout::struct_field_offset(type_key, field_name, self.tc_objs());
        let field_type = type_layout::struct_field_type(type_key, field_name, self.tc_objs());
        (offset, slots, field_type)
    }
    
    /// Get struct field offset, slots, and type by field index
    pub fn struct_field_offset_by_index_with_type(&self, type_key: TypeKey, field_index: usize) -> (u16, u16, TypeKey) {
        let (offset, slots) = type_layout::struct_field_offset_by_index(type_key, field_index, self.tc_objs());
        let field_type = type_layout::struct_field_type_by_index(type_key, field_index, self.tc_objs());
        (offset, slots, field_type)
    }

    pub fn compute_field_offset_from_indices(&self, base_type: TypeKey, indices: &[usize]) -> (u16, u16) {
        type_layout::compute_field_offset_from_indices(base_type, indices, self.tc_objs())
    }

    /// Get field offset for a selector expression.
    /// Uses selection indices if available (handles promoted fields), otherwise falls back to field name.
    pub fn selector_field_offset(
        &self,
        expr_id: vo_syntax::ast::ExprId,
        base_type: TypeKey,
        field_name: &str,
    ) -> (u16, u16) {
        self.get_selection(expr_id)
            .map(|sel_info| self.compute_field_offset_from_indices(base_type, sel_info.indices()))
            .unwrap_or_else(|| self.struct_field_offset(base_type, field_name))
    }

    // === Type queries (delegates to vo_analysis::check::type_info) ===

    pub fn is_interface(&self, type_key: TypeKey) -> bool {
        type_layout::is_interface(type_key, self.tc_objs())
    }

    /// Check if type is empty interface (any/interface{})
    pub fn is_empty_interface(&self, type_key: TypeKey) -> bool {
        let underlying = vo_analysis::typ::underlying_type(type_key, self.tc_objs());
        if let vo_analysis::typ::Type::Interface(iface) = &self.tc_objs().types[underlying] {
            iface.methods().is_empty() && iface.all_methods().as_ref().map_or(true, |m| m.is_empty())
        } else {
            false
        }
    }

    pub fn is_pointer(&self, type_key: TypeKey) -> bool {
        type_layout::is_pointer(type_key, self.tc_objs())
    }

    pub fn is_struct(&self, type_key: TypeKey) -> bool {
        type_layout::is_struct(type_key, self.tc_objs())
    }

    pub fn is_array(&self, type_key: TypeKey) -> bool {
        type_layout::is_array(type_key, self.tc_objs())
    }

    pub fn is_slice(&self, type_key: TypeKey) -> bool {
        type_layout::is_slice(type_key, self.tc_objs())
    }

    pub fn is_map(&self, type_key: TypeKey) -> bool {
        type_layout::is_map(type_key, self.tc_objs())
    }

    pub fn is_string(&self, type_key: TypeKey) -> bool {
        typ::is_string(type_key, self.tc_objs())
    }

    pub fn is_func_type(&self, type_key: TypeKey) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        self.tc_objs().types[underlying].try_as_signature().is_some()
    }

    pub fn is_chan(&self, type_key: TypeKey) -> bool {
        type_layout::is_chan(type_key, self.tc_objs())
    }

    pub fn is_value_type(&self, type_key: TypeKey) -> bool {
        type_layout::is_value_type(type_key, self.tc_objs())
    }

    /// Reference types (pointer, slice, map, channel, closure, string) are already GcRefs.
    /// They only need boxing when captured by closure (to share storage location).
    /// Other escape reasons (e.g. assigned to interface) don't require boxing.
    pub fn is_reference_type(&self, type_key: TypeKey) -> bool {
        use vo_runtime::ValueKind;
        let vk = self.type_value_kind(type_key);
        matches!(vk, 
            ValueKind::Pointer 
            | ValueKind::Slice 
            | ValueKind::Map 
            | ValueKind::Channel 
            | ValueKind::Closure 
            | ValueKind::String)
    }

    pub fn is_named_type(&self, type_key: TypeKey) -> bool {
        type_layout::is_named_type(type_key, self.tc_objs())
    }

    /// Get map type details (key_type, val_type) from a map type key.
    fn get_map_types(&self, type_key: TypeKey) -> (TypeKey, TypeKey) {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let map_type = self.tc_objs().types[underlying].try_as_map()
            .expect("get_map_types: not a map type");
        (map_type.key(), map_type.elem())
    }

    /// Get map key and value slot counts
    pub fn map_key_val_slots(&self, type_key: TypeKey) -> (u16, u16) {
        let (key_type, val_type) = self.get_map_types(type_key);
        (self.type_slot_count(key_type), self.type_slot_count(val_type))
    }

    /// Get map key slot types
    pub fn map_key_slot_types(&self, type_key: TypeKey) -> Vec<vo_runtime::SlotType> {
        let (key_type, _) = self.get_map_types(type_key);
        self.type_slot_types(key_type)
    }

    /// Get map value slot types
    pub fn map_val_slot_types(&self, type_key: TypeKey) -> Vec<vo_runtime::SlotType> {
        let (_, val_type) = self.get_map_types(type_key);
        self.type_slot_types(val_type)
    }

    /// Get map key ValueKind
    pub fn map_key_value_kind(&self, type_key: TypeKey) -> vo_runtime::ValueKind {
        let (key_type, _) = self.get_map_types(type_key);
        self.type_value_kind(key_type)
    }

    /// Get map value ValueKind
    pub fn map_val_value_kind(&self, type_key: TypeKey) -> vo_runtime::ValueKind {
        let (_, val_type) = self.get_map_types(type_key);
        self.type_value_kind(val_type)
    }

    /// Get ValueKind for a type
    pub fn type_value_kind(&self, type_key: TypeKey) -> vo_runtime::ValueKind {
        type_layout::type_value_kind(type_key, self.tc_objs())
    }

    /// Get object's type. Panics with the given message if object has no type.
    pub fn obj_type(&self, obj: ObjKey, msg: &str) -> TypeKey {
        self.tc_objs().lobjs[obj].typ().expect(msg)
    }
    
    /// Try to get object's type. Returns None if object has no type.
    pub fn try_obj_type(&self, obj: ObjKey) -> Option<TypeKey> {
        self.tc_objs().lobjs[obj].typ()
    }

    /// Get method receiver's base type from function declaration.
    /// For `func (r T) Method()` returns type of T.
    /// For `func (r *T) Method()` returns type of T (not *T).
    pub fn method_receiver_base_type(&self, func_decl: &vo_syntax::ast::FuncDecl) -> Option<TypeKey> {
        let func_obj = self.get_def(&func_decl.name);
        let func_type = self.tc_objs().lobjs[func_obj].typ()?;
        let sig = self.as_signature(func_type);
        let recv_obj = (*sig.recv())?;
        let recv_type = self.tc_objs().lobjs[recv_obj].typ()?;
        // If pointer receiver, get the base type
        let underlying = typ::underlying_type(recv_type, self.tc_objs());
        if let Type::Pointer(p) = &self.tc_objs().types[underlying] {
            Some(p.base())
        } else {
            Some(recv_type)
        }
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
    pub fn array_elem_slot_types(&self, type_key: TypeKey) -> Vec<vo_runtime::SlotType> {
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
    pub fn slice_elem_slot_types(&self, type_key: TypeKey) -> Vec<vo_runtime::SlotType> {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Slice(s) = &self.tc_objs().types[underlying] {
            self.type_slot_types(s.elem())
        } else {
            panic!("slice_elem_slot_types: not a slice type")
        }
    }

    /// Get slice element type
    pub fn slice_elem_type(&self, type_key: TypeKey) -> TypeKey {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Slice(s) = &self.tc_objs().types[underlying] {
            s.elem()
        } else {
            panic!("slice_elem_type: not a slice type")
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

    /// Get array element heap bytes (for packed array storage)
    pub fn array_elem_bytes(&self, type_key: TypeKey) -> usize {
        let elem_type = self.array_elem_type(type_key);
        vo_analysis::check::type_info::elem_bytes_for_heap(elem_type, self.tc_objs())
    }

    /// Get slice element heap bytes (for packed array storage)
    pub fn slice_elem_bytes(&self, type_key: TypeKey) -> usize {
        let elem_type = self.slice_elem_type(type_key);
        vo_analysis::check::type_info::elem_bytes_for_heap(elem_type, self.tc_objs())
    }

    /// Get map key and value types
    pub fn map_key_val_types(&self, type_key: TypeKey) -> (TypeKey, TypeKey) {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        let map_type = self.tc_objs().types[underlying].try_as_map()
            .expect("map_key_val_types: not a map type");
        (map_type.key(), map_type.elem())
    }

    /// Get channel element type
    pub fn chan_elem_type(&self, type_key: TypeKey) -> TypeKey {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Chan(c) = &self.tc_objs().types[underlying] {
            c.elem()
        } else {
            panic!("chan_elem_type: not a channel type")
        }
    }

    /// Get channel direction
    pub fn chan_dir(&self, type_key: TypeKey) -> vo_runtime::ChanDir {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Chan(c) = &self.tc_objs().types[underlying] {
            match c.dir() {
                vo_analysis::typ::ChanDir::SendRecv => vo_runtime::ChanDir::Both,
                vo_analysis::typ::ChanDir::SendOnly => vo_runtime::ChanDir::Send,
                vo_analysis::typ::ChanDir::RecvOnly => vo_runtime::ChanDir::Recv,
            }
        } else {
            panic!("chan_dir: not a channel type")
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

    /// Get parameter types and variadic flag for an interface method
    pub fn get_interface_method_signature(&self, iface_type: TypeKey, method_name: &str) -> (Vec<TypeKey>, bool) {
        let underlying = typ::underlying_type(iface_type, self.tc_objs());
        if let Type::Interface(iface) = &self.tc_objs().types[underlying] {
            let all_methods = iface.all_methods();
            let methods = all_methods.as_ref().map(|v| v.as_slice()).unwrap_or(iface.methods());
            for &method in methods {
                if self.obj_name(method) == method_name {
                    let method_type = self.tc_objs().lobjs[method].typ()
                        .expect("interface method must have type");
                    let param_types = self.func_param_types(method_type);
                    let is_variadic = self.is_variadic(method_type);
                    return (param_types, is_variadic);
                }
            }
        }
        panic!("get_interface_method_signature: method {} not found", method_name)
    }
    
    /// Get interface method's param_slots and ret_slots for method value wrapper generation.
    /// Returns (param_slots, ret_slots) where param_slots includes receiver (2 slots for interface).
    pub fn get_interface_method_slots(&self, iface_type: TypeKey, method_name: &str) -> Option<(u16, u16)> {
        let underlying = typ::underlying_type(iface_type, self.tc_objs());
        if let Type::Interface(iface) = &self.tc_objs().types[underlying] {
            let all_methods = iface.all_methods();
            let methods = all_methods.as_ref().map(|v| v.as_slice()).unwrap_or(iface.methods());
            for &method in methods {
                if self.obj_name(method) == method_name {
                    let method_type = self.tc_objs().lobjs[method].typ()
                        .expect("interface method must have type");
                    let sig = self.as_signature(method_type);
                    
                    // Calculate param slots (excluding receiver - interface methods don't have receiver in signature)
                    let mut param_slots = 0u16;
                    let params_key = sig.params();
                    if let Type::Tuple(tuple) = &self.tc_objs().types[params_key] {
                        for &p in tuple.vars() {
                            let param_type = self.tc_objs().lobjs[p].typ().unwrap();
                            param_slots += self.type_slot_count(param_type);
                        }
                    }
                    
                    // Calculate return slots
                    let mut ret_slots = 0u16;
                    let results_key = sig.results();
                    if let Type::Tuple(tuple) = &self.tc_objs().types[results_key] {
                        for &r in tuple.vars() {
                            let ret_type = self.tc_objs().lobjs[r].typ().unwrap();
                            ret_slots += self.type_slot_count(ret_type);
                        }
                    }
                    
                    return Some((param_slots, ret_slots));
                }
            }
        }
        None
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

    /// Get call argument info for a function call.
    /// Handles the `f(g())` pattern where g() returns multiple values.
    pub fn get_call_arg_info(&self, args: &[vo_syntax::ast::Expr], param_types: &[TypeKey]) -> CallArgInfo {
        // Check for multi-value expansion: single arg that is a tuple matching multiple params
        if args.len() == 1 && param_types.len() > 1 {
            let arg_type = self.expr_type(args[0].id);
            if self.is_tuple(arg_type) && self.tuple_len(arg_type) == param_types.len() {
                return CallArgInfo {
                    arg_types: (0..param_types.len())
                        .map(|i| self.tuple_elem_type(arg_type, i))
                        .collect(),
                    tuple_expand: Some(arg_type),
                };
            }
        }
        // Normal case: arg types from expressions
        CallArgInfo {
            arg_types: args.iter().map(|a| self.expr_type(a.id)).collect(),
            tuple_expand: None,
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

    /// Get variadic element type from the last parameter of a variadic function.
    /// The last parameter is a slice type; returns the element type.
    pub fn variadic_elem_type(&self, func_type: TypeKey) -> TypeKey {
        let param_types = self.func_param_types(func_type);
        let last_param = param_types.last().expect("variadic function must have at least one param");
        self.slice_elem_type(*last_param)
    }

    /// Check if type is an integer type
    pub fn is_int(&self, type_key: TypeKey) -> bool {
        type_layout::is_int(type_key, self.tc_objs())
    }

    /// Check if type is a float type
    pub fn is_float(&self, type_key: TypeKey) -> bool {
        type_layout::is_float(type_key, self.tc_objs())
    }

    /// Check if type is float32
    pub fn is_float32(&self, type_key: TypeKey) -> bool {
        matches!(
            &self.tc_objs().types[type_key],
            Type::Basic(b) if b.typ() == typ::BasicType::Float32
        )
    }

    /// Check if type is an unsigned integer type
    pub fn is_unsigned(&self, type_key: TypeKey) -> bool {
        type_layout::is_unsigned(type_key, self.tc_objs())
    }

    /// Check if type is UntypedNil (the nil literal)
    pub fn is_nil(&self, type_key: TypeKey) -> bool {
        matches!(
            &self.tc_objs().types[type_key],
            Type::Basic(b) if b.typ() == typ::BasicType::UntypedNil
        )
    }

    /// Get integer bit size. Panics if type is not an integer type.
    pub fn int_bits(&self, type_key: TypeKey) -> u8 {
        type_layout::int_bits(type_key, self.tc_objs())
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

    /// Check if type is []byte (slice of uint8/byte)
    pub fn is_byte_slice(&self, type_key: TypeKey) -> bool {
        self.is_slice_of(type_key, |b| b == typ::BasicType::Byte || b == typ::BasicType::Uint8)
    }

    /// Check if type is []rune (slice of int32/rune)
    pub fn is_rune_slice(&self, type_key: TypeKey) -> bool {
        self.is_slice_of(type_key, |b| b == typ::BasicType::Rune || b == typ::BasicType::Int32)
    }

    /// Check if type is a slice of basic type matching predicate
    fn is_slice_of(&self, type_key: TypeKey, pred: impl Fn(typ::BasicType) -> bool) -> bool {
        let underlying = typ::underlying_type(type_key, self.tc_objs());
        if let Type::Slice(s) = &self.tc_objs().types[underlying] {
            let elem_underlying = typ::underlying_type(s.elem(), self.tc_objs());
            matches!(
                &self.tc_objs().types[elem_underlying],
                Type::Basic(b) if pred(b.typ())
            )
        } else {
            false
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
