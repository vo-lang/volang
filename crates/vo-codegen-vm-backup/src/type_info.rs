//! Type information wrapper for code generation.
//!
//! This module wraps vo_analysis::TypeQuery and adds expression type tracking.

use vo_analysis::{Builtin, ConstValue, Selection, Type, TypeAndValue, TypeKey, TypeQuery};
use vo_analysis::operand::OperandMode;
use vo_common::Symbol;
use vo_common_core::SlotType;
use vo_syntax::ast::{Expr, TypeExpr};
use std::collections::HashMap;

use vo_common_core::{ExprId, TypeExprId};

/// Layout calculation helpers for struct types.
/// Separated from TypeInfo for single responsibility.
pub struct LayoutCalculator;

impl LayoutCalculator {
    /// Get the StructDetail from a type (handles Named types).
    pub fn get_struct_detail<'a>(query: &'a TypeQuery, ty: &'a Type) -> Option<&'a vo_analysis::StructDetail> {
        match ty {
            Type::Struct(s) => Some(s),
            Type::Named(named) => {
                if let Some(Type::Struct(s)) = query.named_underlying(named) {
                    Some(s)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Check if a type is a struct (including named structs).
    pub fn is_struct_type(query: &TypeQuery, ty: &Type) -> bool {
        Self::get_struct_detail(query, ty).is_some()
    }

    /// Get the size in slots for a struct type.
    pub fn struct_size_slots(query: &TypeQuery, ty: &Type) -> u16 {
        match ty {
            Type::Struct(s) => {
                s.fields()
                    .iter()
                    .map(|&okey| {
                        query.get_obj_type(okey)
                            .map(|field_ty| {
                                if Self::is_struct_type(query, field_ty) {
                                    1
                                } else {
                                    query.type_slots(field_ty)
                                }
                            })
                            .unwrap_or(1)
                    })
                    .sum()
            }
            Type::Named(named) => {
                if let Some(underlying) = query.named_underlying(named) {
                    Self::struct_size_slots(query, underlying)
                } else {
                    1
                }
            }
            _ => 1,
        }
    }

    /// Get slot types for struct fields (for TypeMeta generation).
    pub fn struct_field_slot_types(query: &TypeQuery, ty: &Type) -> Vec<SlotType> {
        let Some(s) = Self::get_struct_detail(query, ty) else {
            return Vec::new();
        };
        
        let mut result = Vec::new();
        for &okey in s.fields() {
            if let Some(field_ty) = query.get_obj_type(okey) {
                if Self::is_struct_type(query, field_ty) {
                    result.push(SlotType::GcRef);
                } else {
                    result.extend(query.type_slot_types(field_ty));
                }
            } else {
                result.push(SlotType::Value);
            }
        }
        result
    }

    /// Get the byte offset and slot count of a struct field.
    pub fn struct_field_info(query: &TypeQuery, struct_type: &Type, field: Symbol) -> Option<(u16, usize)> {
        let struct_detail = Self::get_struct_detail(query, struct_type)?;
        
        let fields = query.struct_fields(struct_detail);
        let field_name = query.symbol_str(field);
        
        let mut byte_offset: u16 = 0;
        for f in &fields {
            if f.name == field_name {
                if let Some(field_type) = f.typ {
                    let slots = if Self::is_struct_type(query, field_type) {
                        1
                    } else {
                        query.type_slots(field_type) as usize
                    };
                    return Some((byte_offset, slots));
                }
            }
            if let Some(field_type) = f.typ {
                let field_slots = if Self::is_struct_type(query, field_type) {
                    1
                } else {
                    query.type_slots(field_type) as usize
                };
                byte_offset += (field_slots * 8) as u16;
            } else {
                byte_offset += 8;
            }
        }
        None
    }

    /// Get nested struct fields that need recursive deep copy.
    pub fn struct_nested_fields<'a>(query: &TypeQuery<'a>, ty: &'a Type) -> Vec<(u16, &'a Type)> {
        let Some(s) = Self::get_struct_detail(query, ty) else {
            return Vec::new();
        };
        
        let mut result = Vec::new();
        let mut byte_offset: u16 = 0;
        
        for &okey in s.fields() {
            if let Some(field_ty) = query.get_obj_type(okey) {
                let field_slots = if Self::is_struct_type(query, field_ty) {
                    result.push((byte_offset, field_ty));
                    1
                } else {
                    query.type_slots(field_ty) as usize
                };
                byte_offset += (field_slots * 8) as u16;
            } else {
                byte_offset += 8;
            }
        }
        
        result
    }
}

/// Type information for code generation.
///
/// Wraps TypeQuery from vo-analysis and adds expression type tracking.
pub struct TypeInfo<'a> {
    /// Type query interface from analysis.
    pub query: TypeQuery<'a>,
    /// Expression types and values recorded during type checking.
    pub expr_types: &'a HashMap<ExprId, TypeAndValue>,
    /// Type expression types recorded during type checking.
    pub type_expr_types: &'a HashMap<TypeExprId, TypeKey>,
    /// Selector expression selections (for field promotion).
    pub selections: &'a HashMap<ExprId, Selection>,
}

impl<'a> TypeInfo<'a> {
    pub fn new(
        query: TypeQuery<'a>,
        expr_types: &'a HashMap<ExprId, TypeAndValue>,
        type_expr_types: &'a HashMap<TypeExprId, TypeKey>,
        selections: &'a HashMap<ExprId, Selection>,
    ) -> Self {
        Self { query, expr_types, type_expr_types, selections }
    }
    
    /// Get the selection for a selector expression (field access).
    pub fn expr_selection(&self, expr: &Expr) -> Option<&'a Selection> {
        self.selections.get(&expr.id)
    }

    // === Expression type queries ===

    pub fn expr_type(&self, expr: &Expr) -> Option<&'a Type> {
        self.expr_types
            .get(&expr.id)
            .map(|tv| self.query.get_type(tv.typ))
    }

    pub fn expr_type_key(&self, expr: &Expr) -> Option<TypeKey> {
        self.expr_types.get(&expr.id).map(|tv| tv.typ)
    }

    /// Get constant value for an expression (if it's a constant).
    pub fn expr_const_value(&self, expr: &Expr) -> Option<&ConstValue> {
        self.expr_types.get(&expr.id).and_then(|tv| {
            match &tv.mode {
                OperandMode::Constant(v) => Some(v),
                _ => None,
            }
        })
    }

    /// Check if an expression is a type expression (for type conversions).
    pub fn is_type_expr(&self, expr: &Expr) -> bool {
        self.expr_types
            .get(&expr.id)
            .map(|tv| matches!(tv.mode, OperandMode::TypeExpr))
            .unwrap_or(false)
    }

    // === Symbol queries (delegate to TypeQuery) ===

    pub fn symbol_str(&self, sym: Symbol) -> &str {
        self.query.symbol_str(sym)
    }

    pub fn is_builtin(&self, sym: Symbol) -> Option<Builtin> {
        self.query.is_builtin(sym)
    }

    // === Type property queries (delegate to TypeQuery) ===

    pub fn value_kind(&self, ty: &Type) -> vo_common_core::ValueKind {
        self.query.value_kind(ty)
    }

    pub fn type_slots(&self, ty: &Type) -> u16 {
        self.query.type_slots(ty)
    }

    pub fn type_slot_types(&self, ty: &Type) -> Vec<SlotType> {
        self.query.type_slot_types(ty)
    }

    /// Get slot types for a type, with a default of [Value] if None.
    pub fn slot_types_or_default(&self, ty: Option<&Type>) -> Vec<SlotType> {
        ty.map(|t| self.type_slot_types(t)).unwrap_or_else(|| vec![SlotType::Value])
    }

    pub fn is_ref_type(&self, ty: &Type) -> bool {
        self.query.is_ref_type(ty)
    }

    pub fn is_interface(&self, ty: &Type) -> bool {
        self.query.is_interface(ty)
    }

    /// Lookup a symbol and get its type.
    pub fn lookup_symbol_type(&self, sym: Symbol) -> Option<&'a Type> {
        use vo_analysis::query::EntityRef;
        match self.query.lookup_symbol(sym)? {
            EntityRef::Var { typ, .. } => typ,
            _ => None,
        }
    }

    /// Lookup a type name symbol and return its TypeKey.
    pub fn lookup_type_key(&self, sym: Symbol) -> Option<TypeKey> {
        self.query.lookup_type_key(sym)
    }

    /// Resolve a TypeExpr to its Type using analysis results.
    /// Returns the type recorded during type checking.
    pub fn resolve_type_expr(&self, ty: &TypeExpr) -> Option<&'a Type> {
        let type_key = self.type_expr_types.get(&ty.id)?;
        Some(self.query.get_type(*type_key))
    }
    
    /// Get TypeKey for a TypeExpr from analysis results.
    pub fn type_expr_type_key(&self, ty: &TypeExpr) -> Option<TypeKey> {
        self.type_expr_types.get(&ty.id).copied()
    }
    
    /// Get slot types for a TypeExpr using analysis results.
    pub fn type_expr_slot_types(&self, ty: &TypeExpr) -> Vec<SlotType> {
        if let Some(t) = self.resolve_type_expr(ty) {
            self.type_slot_types(t)
        } else {
            // Fallback for unresolved types
            vec![SlotType::Value]
        }
    }

    /// Delegate to LayoutCalculator for struct field info.
    pub fn struct_field_info(&self, struct_type: &Type, field: Symbol) -> Option<(u16, usize)> {
        LayoutCalculator::struct_field_info(&self.query, struct_type, field)
    }
    
    /// Delegate to LayoutCalculator.
    pub fn is_struct_type(&self, ty: &Type) -> bool {
        LayoutCalculator::is_struct_type(&self.query, ty)
    }
    
    /// Delegate to LayoutCalculator.
    pub fn struct_size_slots(&self, ty: &Type) -> u16 {
        LayoutCalculator::struct_size_slots(&self.query, ty)
    }

    /// Delegate to LayoutCalculator.
    pub fn struct_field_slot_types(&self, ty: &Type) -> Vec<SlotType> {
        LayoutCalculator::struct_field_slot_types(&self.query, ty)
    }

    /// Delegate to LayoutCalculator.
    pub fn struct_nested_fields(&self, ty: &'a Type) -> Vec<(u16, &'a Type)> {
        LayoutCalculator::struct_nested_fields(&self.query, ty)
    }

    /// Get the base type key for method dispatch.
    /// For pointer types (*T), returns the TypeKey of T.
    /// For named types, returns their TypeKey.
    pub fn method_receiver_type_key(&self, expr: &Expr) -> Option<TypeKey> {
        let type_key = self.expr_type_key(expr)?;
        let ty = self.query.get_type(type_key);
        match ty {
            Type::Pointer(ptr_detail) => Some(ptr_detail.base()),
            Type::Named { .. } => Some(type_key),
            _ => None,
        }
    }

    /// Get the receiver type for a function symbol (from its signature).
    /// Returns None if no receiver or not a function.
    pub fn func_recv_type(&self, func_sym: Symbol) -> Option<&'a Type> {
        use vo_analysis::query::EntityRef;
        match self.query.lookup_symbol(func_sym)? {
            EntityRef::Func { sig, .. } => {
                let sig_detail = sig?;
                let recv_obj_key = sig_detail.recv().as_ref()?;
                self.query.get_obj_type(*recv_obj_key)
            }
            _ => None,
        }
    }

    /// Get the interface method index for an interface method call.
    /// Returns (interface_type_key, method_index) if the receiver is an interface.
    pub fn interface_method_info(&self, receiver_expr: &Expr, method_selection: &Selection) -> Option<(TypeKey, usize)> {
        let recv_type_key = self.expr_type_key(receiver_expr)?;
        let recv_type = self.query.get_type(recv_type_key);
        
        if !self.query.is_interface(recv_type) {
            return None;
        }
        
        let iface_detail = self.query.get_interface_detail_by_key(recv_type_key)?;
        let method_obj = method_selection.obj();
        let method_idx = self.query.interface_method_index(iface_detail, method_obj)?;
        
        Some((recv_type_key, method_idx))
    }

    /// Lookup a symbol and return its ObjKey.
    pub fn lookup_symbol_objkey(&self, sym: Symbol) -> Option<vo_analysis::ObjKey> {
        self.query.lookup_symbol_objkey(sym)
    }

    /// Build method dispatch mapping for a concrete type implementing an interface.
    /// Returns Vec of (method_name, concrete_method_obj_key) for each interface method.
    pub fn build_iface_method_mapping(
        &self,
        concrete_type_key: TypeKey,
        iface_type_key: TypeKey,
    ) -> Option<Vec<(String, vo_analysis::ObjKey)>> {
        let iface_detail = self.query.get_interface_detail_by_key(iface_type_key)?;
        let iface_methods = self.query.interface_all_methods(iface_detail);
        
        let mut result = Vec::with_capacity(iface_methods.len());
        for iface_method_obj in iface_methods {
            let method_name = self.query.obj_name(iface_method_obj);
            let concrete_method = self.query.lookup_concrete_method(concrete_type_key, method_name)?;
            result.push((method_name.to_string(), concrete_method));
        }
        Some(result)
    }
}
