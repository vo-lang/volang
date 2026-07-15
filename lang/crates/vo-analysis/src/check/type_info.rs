//! Type information produced by type checking.

use crate::obj;
use crate::objects::{ObjKey, PackageKey, ScopeKey, TCObjects, TypeKey};
use crate::operand::OperandMode;
use crate::selection::Selection;
use std::collections::{HashMap, HashSet};
use vo_common::Span;
use vo_syntax::ast::Expr;
use vo_syntax::ast::{ExprId, Ident, IdentId, TypeExprId};

/// TypeAndValue reports the type and value (for constants) of an expression.
#[derive(Debug, Clone)]
pub struct TypeAndValue {
    pub mode: OperandMode,
    pub typ: TypeKey,
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
    pub rhs: Vec<Expr>,
}

impl Initializer {
    pub(crate) fn new(lhs: Vec<ObjKey>, rhs: Vec<Expr>) -> Self {
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

    /// Variables defined inside loops (Go 1.22 per-iteration semantics, set by escape analysis pass).
    pub loop_defined_vars: HashSet<ObjKey>,
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
    pub(crate) fn record_type_and_value(
        &mut self,
        expr_id: ExprId,
        mode: OperandMode,
        typ: TypeKey,
    ) {
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
                tc_objs.lobjs.insert(obj::LangObj::new_var(
                    span,
                    Some(pkg),
                    String::new(),
                    Some(t[0]),
                )),
                tc_objs.lobjs.insert(obj::LangObj::new_var(
                    span,
                    Some(pkg),
                    String::new(),
                    Some(t[1]),
                )),
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

    /// Returns true if the variable is defined inside a loop (Go 1.22 per-iteration semantics).
    pub fn is_loop_var(&self, obj: ObjKey) -> bool {
        self.loop_defined_vars.contains(&obj)
    }
}

// === Type Layout Functions ===
// These functions compute type layout information (slot counts, slot types, etc.)
// They only depend on TCObjects and can be used by both codegen and other consumers.

use crate::typ::{self, BasicType, Type};
use std::fmt;
use vo_common_core::types::{SlotType, ValueKind};

/// A malformed or unrepresentable physical type layout.
///
/// Type checking normally prevents these states. Keeping layout construction
/// fallible is still important: code generation consumes arena metadata and
/// must turn hostile depth, accidental cycles, and width overflow into a
/// deterministic compiler error instead of recursing or allocating from an
/// unchecked user-controlled size.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeLayoutError {
    InvalidTypeKey(TypeKey),
    InvalidObjectKey(ObjKey),
    MissingNamedUnderlying(TypeKey),
    MissingArrayLength(TypeKey),
    MissingObjectType(ObjKey),
    ByValueCycle(TypeKey),
    SlotCountOverflow(TypeKey),
    SlotCountTooWide { slots: usize, max: usize },
    AllocationFailed { slots: usize },
    MaterializedWidthMismatch { expected: usize, actual: usize },
    InvalidBasicType(BasicType),
}

impl fmt::Display for TypeLayoutError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidTypeKey(key) => write!(f, "invalid type key in layout metadata: {key:?}"),
            Self::InvalidObjectKey(key) => {
                write!(f, "invalid object key in layout metadata: {key:?}")
            }
            Self::MissingNamedUnderlying(key) => {
                write!(f, "named type has no underlying type: {key:?}")
            }
            Self::MissingArrayLength(key) => {
                write!(f, "array type has no resolved length: {key:?}")
            }
            Self::MissingObjectType(key) => {
                write!(f, "layout object has no resolved type: {key:?}")
            }
            Self::ByValueCycle(key) => {
                write!(f, "cyclic by-value type layout through {key:?}")
            }
            Self::SlotCountOverflow(key) => {
                write!(f, "type slot count overflow for type {key:?}")
            }
            Self::SlotCountTooWide { slots, max } if *max == usize::from(u16::MAX) => {
                write!(f, "type slot count exceeds u16::MAX: {slots} slots")
            }
            Self::SlotCountTooWide { slots, max } => {
                write!(f, "type slot count {slots} exceeds layout limit {max}")
            }
            Self::AllocationFailed { slots } => {
                write!(f, "could not allocate type slot layout for {slots} slots")
            }
            Self::MaterializedWidthMismatch { expected, actual } => write!(
                f,
                "materialized type layout width mismatch: expected {expected}, got {actual}"
            ),
            Self::InvalidBasicType(kind) => {
                write!(f, "invalid basic type in runtime layout: {kind:?}")
            }
        }
    }
}

impl std::error::Error for TypeLayoutError {}

/// Validated physical-width facts for one complete type arena.
///
/// The map stays private so downstream code cannot pair a materializer with
/// counts from another arena or with unchecked, incomplete facts.
/// Widths through `u16::MAX` are exact. Wider arena-only layouts use
/// `u16::MAX + 1` as an over-wide sentinel, which keeps whole-project
/// validation independent of the compiler host's pointer width.
#[derive(Debug)]
pub struct TypeLayoutFacts {
    counts: HashMap<TypeKey, usize>,
}

impl TypeLayoutFacts {
    pub fn slot_count(&self, type_key: TypeKey) -> Result<usize, TypeLayoutError> {
        self.counts
            .get(&type_key)
            .copied()
            .ok_or(TypeLayoutError::InvalidTypeKey(type_key))
    }

    pub fn iter(&self) -> impl Iterator<Item = (TypeKey, usize)> + '_ {
        self.counts.iter().map(|(&key, &count)| (key, count))
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum VisitState {
    Visiting,
    Complete,
}

enum CountTask {
    Enter(TypeKey),
    Exit(TypeKey),
}

/// Compute all by-value subtree widths reachable from `root`.
///
/// Reference-bearing type constructors intentionally have no children here:
/// their pointed-to/element metadata has an independent layout and does not
/// contribute slots to the reference value itself.
fn compute_slot_counts_for_roots(
    roots: impl IntoIterator<Item = TypeKey>,
    tc_objs: &TCObjects,
    saturation: Option<usize>,
) -> Result<HashMap<TypeKey, usize>, TypeLayoutError> {
    let mut states = HashMap::<TypeKey, VisitState>::new();
    let mut counts = HashMap::<TypeKey, usize>::new();
    let mut tasks: Vec<_> = roots.into_iter().map(CountTask::Enter).collect();

    while let Some(task) = tasks.pop() {
        match task {
            CountTask::Enter(key) => {
                if counts.contains_key(&key) {
                    continue;
                }
                match states.get(&key) {
                    Some(VisitState::Visiting) => {
                        return Err(TypeLayoutError::ByValueCycle(key));
                    }
                    Some(VisitState::Complete) => continue,
                    None => {}
                }

                let typ = tc_objs
                    .types
                    .get(key)
                    .ok_or(TypeLayoutError::InvalidTypeKey(key))?;
                states.insert(key, VisitState::Visiting);
                tasks.push(CountTask::Exit(key));

                match typ {
                    Type::Named(named) => {
                        let underlying = named
                            .try_underlying()
                            .ok_or(TypeLayoutError::MissingNamedUnderlying(key))?;
                        tasks.push(CountTask::Enter(underlying));
                    }
                    Type::Struct(detail) => {
                        for &field in detail.fields().iter().rev() {
                            let object = tc_objs
                                .lobjs
                                .get(field)
                                .ok_or(TypeLayoutError::InvalidObjectKey(field))?;
                            let field_type = object
                                .typ()
                                .ok_or(TypeLayoutError::MissingObjectType(field))?;
                            tasks.push(CountTask::Enter(field_type));
                        }
                    }
                    Type::Array(detail) => {
                        if detail.len().is_none() {
                            return Err(TypeLayoutError::MissingArrayLength(key));
                        }
                        tasks.push(CountTask::Enter(detail.elem()));
                    }
                    Type::Tuple(detail) => {
                        for &variable in detail.vars().iter().rev() {
                            let object = tc_objs
                                .lobjs
                                .get(variable)
                                .ok_or(TypeLayoutError::InvalidObjectKey(variable))?;
                            let variable_type = object
                                .typ()
                                .ok_or(TypeLayoutError::MissingObjectType(variable))?;
                            tasks.push(CountTask::Enter(variable_type));
                        }
                    }
                    Type::Basic(_)
                    | Type::Pointer(_)
                    | Type::Slice(_)
                    | Type::Map(_)
                    | Type::Chan(_)
                    | Type::Port(_)
                    | Type::Signature(_)
                    | Type::Interface(_)
                    | Type::Island => {}
                }
            }
            CountTask::Exit(key) => {
                let typ = tc_objs
                    .types
                    .get(key)
                    .ok_or(TypeLayoutError::InvalidTypeKey(key))?;
                let count = match typ {
                    Type::Basic(_)
                    | Type::Pointer(_)
                    | Type::Slice(_)
                    | Type::Map(_)
                    | Type::Chan(_)
                    | Type::Port(_)
                    | Type::Signature(_)
                    | Type::Island => 1,
                    Type::Interface(_) => 2,
                    Type::Named(named) => {
                        let underlying = named
                            .try_underlying()
                            .ok_or(TypeLayoutError::MissingNamedUnderlying(key))?;
                        *counts
                            .get(&underlying)
                            .ok_or(TypeLayoutError::ByValueCycle(key))?
                    }
                    Type::Struct(detail) => {
                        let mut total = 0usize;
                        for &field in detail.fields() {
                            let object = tc_objs
                                .lobjs
                                .get(field)
                                .ok_or(TypeLayoutError::InvalidObjectKey(field))?;
                            let field_type = object
                                .typ()
                                .ok_or(TypeLayoutError::MissingObjectType(field))?;
                            let field_slots = *counts
                                .get(&field_type)
                                .ok_or(TypeLayoutError::ByValueCycle(key))?;
                            total = if let Some(limit) = saturation {
                                total.saturating_add(field_slots).min(limit)
                            } else {
                                total
                                    .checked_add(field_slots)
                                    .ok_or(TypeLayoutError::SlotCountOverflow(key))?
                            };
                        }
                        total.max(1)
                    }
                    Type::Array(detail) => {
                        let elem_slots = *counts
                            .get(&detail.elem())
                            .ok_or(TypeLayoutError::ByValueCycle(key))?;
                        // Do not convert or multiply a logical giant length
                        // when each element contributes no physical slots.
                        if elem_slots == 0 {
                            0
                        } else if let Some(limit) = saturation {
                            // `limit` is the first unencodable VM width. Once
                            // reached, exact host-sized arithmetic is neither
                            // useful nor portable (notably for wasm32).
                            let len = detail
                                .len()
                                .ok_or(TypeLayoutError::MissingArrayLength(key))?;
                            if len == 0 {
                                0
                            } else if elem_slots >= limit
                                || len > u64::try_from(limit / elem_slots).unwrap_or(u64::MAX)
                            {
                                limit
                            } else {
                                elem_slots
                                    * usize::try_from(len)
                                        .map_err(|_| TypeLayoutError::SlotCountOverflow(key))?
                            }
                        } else {
                            let len = detail
                                .len()
                                .ok_or(TypeLayoutError::MissingArrayLength(key))?;
                            let len = usize::try_from(len)
                                .map_err(|_| TypeLayoutError::SlotCountOverflow(key))?;
                            elem_slots
                                .checked_mul(len)
                                .ok_or(TypeLayoutError::SlotCountOverflow(key))?
                        }
                    }
                    Type::Tuple(detail) => {
                        let mut total = 0usize;
                        for &variable in detail.vars() {
                            let object = tc_objs
                                .lobjs
                                .get(variable)
                                .ok_or(TypeLayoutError::InvalidObjectKey(variable))?;
                            let variable_type = object
                                .typ()
                                .ok_or(TypeLayoutError::MissingObjectType(variable))?;
                            let variable_slots = *counts
                                .get(&variable_type)
                                .ok_or(TypeLayoutError::ByValueCycle(key))?;
                            total = if let Some(limit) = saturation {
                                total.saturating_add(variable_slots).min(limit)
                            } else {
                                total
                                    .checked_add(variable_slots)
                                    .ok_or(TypeLayoutError::SlotCountOverflow(key))?
                            };
                        }
                        total
                    }
                };
                counts.insert(key, count);
                states.insert(key, VisitState::Complete);
            }
        }
    }

    Ok(counts)
}

fn compute_slot_counts(
    root: TypeKey,
    tc_objs: &TCObjects,
) -> Result<HashMap<TypeKey, usize>, TypeLayoutError> {
    compute_slot_counts_for_roots(std::iter::once(root), tc_objs, None)
}

/// Validate every arena type and return its physical slot width.
///
/// The shared traversal makes whole-project codegen preflight O(V+E), even
/// when many declared names point through the same deep composite subtree.
pub fn try_all_type_slot_counts(tc_objs: &TCObjects) -> Result<TypeLayoutFacts, TypeLayoutError> {
    const OVERWIDE_SLOT_COUNT: usize = u16::MAX as usize + 1;
    Ok(TypeLayoutFacts {
        counts: compute_slot_counts_for_roots(
            tc_objs.types.iter().map(|(key, _)| key),
            tc_objs,
            Some(OVERWIDE_SLOT_COUNT),
        )?,
    })
}

/// Fallible slot-count fact source used at compiler trust boundaries.
pub fn try_type_slot_count_usize(
    type_key: TypeKey,
    tc_objs: &TCObjects,
) -> Result<usize, TypeLayoutError> {
    let counts = compute_slot_counts(type_key, tc_objs)?;
    counts
        .get(&type_key)
        .copied()
        .ok_or(TypeLayoutError::InvalidTypeKey(type_key))
}

/// Compute the number of slots a type occupies without narrowing to VM operand width.
pub fn type_slot_count_usize(type_key: TypeKey, tc_objs: &TCObjects) -> Option<usize> {
    try_type_slot_count_usize(type_key, tc_objs).ok()
}

/// Compute a slot count that is directly representable by VM metadata.
pub fn try_type_slot_count(type_key: TypeKey, tc_objs: &TCObjects) -> Result<u16, TypeLayoutError> {
    let slots = try_type_slot_count_usize(type_key, tc_objs)?;
    u16::try_from(slots).map_err(|_| TypeLayoutError::SlotCountTooWide {
        slots,
        max: usize::from(u16::MAX),
    })
}

/// Compute the VM slot count for a type.
///
/// VM frame and metadata widths are u16. Heap sequence byte sizing can use the
/// wider `type_slot_count_usize` fact source directly.
pub fn type_slot_count(type_key: TypeKey, tc_objs: &TCObjects) -> u16 {
    try_type_slot_count(type_key, tc_objs).unwrap_or_else(|error| panic!("{error}"))
}

enum LayoutTask {
    Type(TypeKey),
    Repeat { type_key: TypeKey, remaining: usize },
}

/// Materialize a VM slot layout after checking its u16 metadata width.
///
/// The width check happens before `Vec::try_reserve_exact`, so a giant source
/// array cannot become a giant allocator request during compilation.
pub fn try_type_slot_types(
    type_key: TypeKey,
    tc_objs: &TCObjects,
) -> Result<Vec<SlotType>, TypeLayoutError> {
    let facts = TypeLayoutFacts {
        counts: compute_slot_counts(type_key, tc_objs)?,
    };
    try_type_slot_types_with_facts(type_key, tc_objs, &facts)
}

/// Materialize slot types from a previously validated whole-project count map.
pub fn try_type_slot_types_with_facts(
    type_key: TypeKey,
    tc_objs: &TCObjects,
    facts: &TypeLayoutFacts,
) -> Result<Vec<SlotType>, TypeLayoutError> {
    let counts = &facts.counts;
    let slot_count = *counts
        .get(&type_key)
        .ok_or(TypeLayoutError::InvalidTypeKey(type_key))?;
    if slot_count > usize::from(u16::MAX) {
        return Err(TypeLayoutError::SlotCountTooWide {
            slots: slot_count,
            max: usize::from(u16::MAX),
        });
    }

    let mut result = Vec::new();
    result
        .try_reserve_exact(slot_count)
        .map_err(|_| TypeLayoutError::AllocationFailed { slots: slot_count })?;
    let mut tasks = vec![LayoutTask::Type(type_key)];

    while let Some(task) = tasks.pop() {
        let key = match task {
            LayoutTask::Type(key) => key,
            LayoutTask::Repeat {
                type_key,
                remaining,
            } => {
                if remaining > 1 {
                    tasks.push(LayoutTask::Repeat {
                        type_key,
                        remaining: remaining - 1,
                    });
                }
                if remaining != 0 {
                    tasks.push(LayoutTask::Type(type_key));
                }
                continue;
            }
        };
        let typ = tc_objs
            .types
            .get(key)
            .ok_or(TypeLayoutError::InvalidTypeKey(key))?;
        match typ {
            Type::Basic(detail) => {
                let slot_type = if matches!(detail.typ(), BasicType::Str | BasicType::UntypedString)
                {
                    SlotType::GcRef
                } else if matches!(
                    detail.typ(),
                    BasicType::Float64 | BasicType::Float32 | BasicType::UntypedFloat
                ) {
                    SlotType::Float
                } else {
                    SlotType::Value
                };
                result.push(slot_type);
            }
            Type::Pointer(_)
            | Type::Slice(_)
            | Type::Map(_)
            | Type::Chan(_)
            | Type::Port(_)
            | Type::Signature(_)
            | Type::Island => result.push(SlotType::GcRef),
            Type::Interface(_) => {
                result.push(SlotType::Interface0);
                result.push(SlotType::Interface1);
            }
            Type::Named(named) => {
                let underlying = named
                    .try_underlying()
                    .ok_or(TypeLayoutError::MissingNamedUnderlying(key))?;
                tasks.push(LayoutTask::Type(underlying));
            }
            Type::Struct(detail) => {
                let mut physical_fields = 0usize;
                for &field in detail.fields() {
                    let object = tc_objs
                        .lobjs
                        .get(field)
                        .ok_or(TypeLayoutError::InvalidObjectKey(field))?;
                    let field_type = object
                        .typ()
                        .ok_or(TypeLayoutError::MissingObjectType(field))?;
                    physical_fields = physical_fields
                        .checked_add(
                            *counts
                                .get(&field_type)
                                .ok_or(TypeLayoutError::ByValueCycle(key))?,
                        )
                        .ok_or(TypeLayoutError::SlotCountOverflow(key))?;
                }
                if physical_fields == 0 {
                    result.push(SlotType::Value);
                } else {
                    for &field in detail.fields().iter().rev() {
                        let object = tc_objs
                            .lobjs
                            .get(field)
                            .ok_or(TypeLayoutError::InvalidObjectKey(field))?;
                        let field_type = object
                            .typ()
                            .ok_or(TypeLayoutError::MissingObjectType(field))?;
                        tasks.push(LayoutTask::Type(field_type));
                    }
                }
            }
            Type::Array(detail) => {
                let elem_slots = *counts
                    .get(&detail.elem())
                    .ok_or(TypeLayoutError::ByValueCycle(key))?;
                if elem_slots == 0 {
                    continue;
                }
                let len = detail
                    .len()
                    .ok_or(TypeLayoutError::MissingArrayLength(key))?;
                let len =
                    usize::try_from(len).map_err(|_| TypeLayoutError::SlotCountOverflow(key))?;
                if len != 0 {
                    tasks.push(LayoutTask::Repeat {
                        type_key: detail.elem(),
                        remaining: len,
                    });
                }
            }
            Type::Tuple(detail) => {
                for &variable in detail.vars().iter().rev() {
                    let object = tc_objs
                        .lobjs
                        .get(variable)
                        .ok_or(TypeLayoutError::InvalidObjectKey(variable))?;
                    let variable_type = object
                        .typ()
                        .ok_or(TypeLayoutError::MissingObjectType(variable))?;
                    tasks.push(LayoutTask::Type(variable_type));
                }
            }
        }
    }

    if result.len() != slot_count {
        return Err(TypeLayoutError::MaterializedWidthMismatch {
            expected: slot_count,
            actual: result.len(),
        });
    }
    Ok(result)
}

/// Compute the slot types for a VM-representable type.
pub fn type_slot_types(type_key: TypeKey, tc_objs: &TCObjects) -> Vec<SlotType> {
    try_type_slot_types(type_key, tc_objs).unwrap_or_else(|error| panic!("{error}"))
}

enum ValueKindTask {
    Type(TypeKey),
    Repeat { type_key: TypeKey, remaining: usize },
}

/// Flatten the equality/value kind associated with each physical slot.
///
/// This mirrors `try_type_slot_types` and shares its pre-allocation width
/// check. Composite comparison lowering can therefore handle deep types
/// without a second recursive layout walker.
pub fn try_type_slot_value_kinds(
    type_key: TypeKey,
    tc_objs: &TCObjects,
) -> Result<Vec<ValueKind>, TypeLayoutError> {
    let facts = TypeLayoutFacts {
        counts: compute_slot_counts(type_key, tc_objs)?,
    };
    try_type_slot_value_kinds_with_facts(type_key, tc_objs, &facts)
}

/// Materialize per-slot value kinds from validated whole-project counts.
pub fn try_type_slot_value_kinds_with_facts(
    type_key: TypeKey,
    tc_objs: &TCObjects,
    facts: &TypeLayoutFacts,
) -> Result<Vec<ValueKind>, TypeLayoutError> {
    let counts = &facts.counts;
    let slot_count = *counts
        .get(&type_key)
        .ok_or(TypeLayoutError::InvalidTypeKey(type_key))?;
    if slot_count > usize::from(u16::MAX) {
        return Err(TypeLayoutError::SlotCountTooWide {
            slots: slot_count,
            max: usize::from(u16::MAX),
        });
    }

    let mut result = Vec::new();
    result
        .try_reserve_exact(slot_count)
        .map_err(|_| TypeLayoutError::AllocationFailed { slots: slot_count })?;
    let mut tasks = vec![ValueKindTask::Type(type_key)];

    while let Some(task) = tasks.pop() {
        let key = match task {
            ValueKindTask::Type(key) => key,
            ValueKindTask::Repeat {
                type_key,
                remaining,
            } => {
                if remaining > 1 {
                    tasks.push(ValueKindTask::Repeat {
                        type_key,
                        remaining: remaining - 1,
                    });
                }
                if remaining != 0 {
                    tasks.push(ValueKindTask::Type(type_key));
                }
                continue;
            }
        };

        let typ = tc_objs
            .types
            .get(key)
            .ok_or(TypeLayoutError::InvalidTypeKey(key))?;
        match typ {
            Type::Named(named) => {
                let underlying = named
                    .try_underlying()
                    .ok_or(TypeLayoutError::MissingNamedUnderlying(key))?;
                tasks.push(ValueKindTask::Type(underlying));
            }
            Type::Struct(detail) => {
                let mut physical_fields = 0usize;
                for &field in detail.fields() {
                    let object = tc_objs
                        .lobjs
                        .get(field)
                        .ok_or(TypeLayoutError::InvalidObjectKey(field))?;
                    let field_type = object
                        .typ()
                        .ok_or(TypeLayoutError::MissingObjectType(field))?;
                    physical_fields = physical_fields
                        .checked_add(
                            *counts
                                .get(&field_type)
                                .ok_or(TypeLayoutError::ByValueCycle(key))?,
                        )
                        .ok_or(TypeLayoutError::SlotCountOverflow(key))?;
                }
                if physical_fields == 0 {
                    result.push(ValueKind::Struct);
                } else {
                    for &field in detail.fields().iter().rev() {
                        let object = tc_objs
                            .lobjs
                            .get(field)
                            .ok_or(TypeLayoutError::InvalidObjectKey(field))?;
                        let field_type = object
                            .typ()
                            .ok_or(TypeLayoutError::MissingObjectType(field))?;
                        tasks.push(ValueKindTask::Type(field_type));
                    }
                }
            }
            Type::Array(detail) => {
                let elem_slots = *counts
                    .get(&detail.elem())
                    .ok_or(TypeLayoutError::ByValueCycle(key))?;
                if elem_slots == 0 {
                    continue;
                }
                let len = detail
                    .len()
                    .ok_or(TypeLayoutError::MissingArrayLength(key))?;
                let len =
                    usize::try_from(len).map_err(|_| TypeLayoutError::SlotCountOverflow(key))?;
                if len != 0 {
                    tasks.push(ValueKindTask::Repeat {
                        type_key: detail.elem(),
                        remaining: len,
                    });
                }
            }
            Type::Tuple(detail) => {
                for &variable in detail.vars().iter().rev() {
                    let object = tc_objs
                        .lobjs
                        .get(variable)
                        .ok_or(TypeLayoutError::InvalidObjectKey(variable))?;
                    let variable_type = object
                        .typ()
                        .ok_or(TypeLayoutError::MissingObjectType(variable))?;
                    tasks.push(ValueKindTask::Type(variable_type));
                }
            }
            _ => {
                let count = *counts
                    .get(&key)
                    .ok_or(TypeLayoutError::InvalidTypeKey(key))?;
                let kind = try_type_value_kind(key, tc_objs)?;
                result.extend(std::iter::repeat_n(kind, count));
            }
        }
    }

    if result.len() != slot_count {
        return Err(TypeLayoutError::MaterializedWidthMismatch {
            expected: slot_count,
            actual: result.len(),
        });
    }
    Ok(result)
}

/// Calculate the byte size for heap array/slice elements.
/// Packed types (bool, int8-32, float32) use actual byte size.
/// Other types use slot-based storage (slots * 8).
/// Empty struct (struct{}) returns 0 bytes.
pub fn try_elem_bytes_for_heap(
    elem_type: TypeKey,
    tc_objs: &TCObjects,
) -> Result<usize, TypeLayoutError> {
    let facts = TypeLayoutFacts {
        counts: compute_slot_counts(elem_type, tc_objs)?,
    };
    try_elem_bytes_for_heap_with_facts(elem_type, tc_objs, &facts)
}

/// Compute heap element bytes from validated whole-project slot counts.
pub fn try_elem_bytes_for_heap_with_facts(
    elem_type: TypeKey,
    tc_objs: &TCObjects,
    facts: &TypeLayoutFacts,
) -> Result<usize, TypeLayoutError> {
    let counts = &facts.counts;
    let vk = try_type_value_kind(elem_type, tc_objs)?;
    Ok(match vk {
        // Packed: primitive types with size < 8 bytes
        ValueKind::Bool | ValueKind::Int8 | ValueKind::Uint8 => 1,
        ValueKind::Int16 | ValueKind::Uint16 => 2,
        ValueKind::Int32 | ValueKind::Uint32 | ValueKind::Float32 => 4,
        // Struct: compute actual field bytes (empty struct = 0, not 1 slot)
        ValueKind::Struct => try_struct_actual_bytes_with_counts(elem_type, tc_objs, counts)?,
        // Slot-based: all other types
        _ => counts
            .get(&elem_type)
            .copied()
            .ok_or(TypeLayoutError::InvalidTypeKey(elem_type))?
            .checked_mul(8)
            .ok_or(TypeLayoutError::SlotCountOverflow(elem_type))?,
    })
}

pub fn elem_bytes_for_heap(elem_type: TypeKey, tc_objs: &TCObjects) -> usize {
    try_elem_bytes_for_heap(elem_type, tc_objs).unwrap_or_else(|error| panic!("{error}"))
}

fn try_struct_actual_bytes_with_counts(
    type_key: TypeKey,
    tc_objs: &TCObjects,
    counts: &HashMap<TypeKey, usize>,
) -> Result<usize, TypeLayoutError> {
    let underlying = try_deep_underlying_for_layout(type_key, tc_objs)?;
    if let Type::Struct(s) = tc_objs
        .types
        .get(underlying)
        .ok_or(TypeLayoutError::InvalidTypeKey(underlying))?
    {
        let mut total = 0usize;
        for &field_obj in s.fields() {
            let object = tc_objs
                .lobjs
                .get(field_obj)
                .ok_or(TypeLayoutError::InvalidObjectKey(field_obj))?;
            let field_type = object
                .typ()
                .ok_or(TypeLayoutError::MissingObjectType(field_obj))?;
            let field_bytes = counts
                .get(&field_type)
                .copied()
                .ok_or(TypeLayoutError::InvalidTypeKey(field_type))?
                .checked_mul(8)
                .ok_or(TypeLayoutError::SlotCountOverflow(field_type))?;
            total = total
                .checked_add(field_bytes)
                .ok_or(TypeLayoutError::SlotCountOverflow(type_key))?;
        }
        Ok(total)
    } else {
        counts
            .get(&type_key)
            .copied()
            .ok_or(TypeLayoutError::InvalidTypeKey(type_key))?
            .checked_mul(8)
            .ok_or(TypeLayoutError::SlotCountOverflow(type_key))
    }
}

fn try_deep_underlying_for_layout(
    type_key: TypeKey,
    tc_objs: &TCObjects,
) -> Result<TypeKey, TypeLayoutError> {
    let mut current = type_key;
    let mut visited = HashSet::new();
    loop {
        if !visited.insert(current) {
            return Err(TypeLayoutError::ByValueCycle(current));
        }
        let typ = tc_objs
            .types
            .get(current)
            .ok_or(TypeLayoutError::InvalidTypeKey(current))?;
        let Type::Named(named) = typ else {
            return Ok(current);
        };
        current = named
            .try_underlying()
            .ok_or(TypeLayoutError::MissingNamedUnderlying(current))?;
    }
}

/// Fallibly convert a type to its runtime value kind without host recursion.
pub fn try_type_value_kind(
    type_key: TypeKey,
    tc_objs: &TCObjects,
) -> Result<ValueKind, TypeLayoutError> {
    let underlying = try_deep_underlying_for_layout(type_key, tc_objs)?;
    Ok(
        match tc_objs
            .types
            .get(underlying)
            .ok_or(TypeLayoutError::InvalidTypeKey(underlying))?
        {
            Type::Basic(b) => match b.typ() {
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
                BasicType::Uint64 => ValueKind::Uint64,
                BasicType::Float32 => ValueKind::Float32,
                BasicType::Float64 | BasicType::UntypedFloat => ValueKind::Float64,
                BasicType::Str | BasicType::UntypedString => ValueKind::String,
                BasicType::UntypedNil => ValueKind::Void,
                other => return Err(TypeLayoutError::InvalidBasicType(other)),
            },
            Type::Pointer(_) => ValueKind::Pointer,
            Type::Array(_) => ValueKind::Array,
            Type::Slice(_) => ValueKind::Slice,
            Type::Map(_) => ValueKind::Map,
            Type::Struct(_) => ValueKind::Struct,
            Type::Interface(_) => ValueKind::Interface,
            Type::Chan(_) => ValueKind::Channel,
            Type::Port(_) => ValueKind::Port,
            Type::Island => ValueKind::Island,
            Type::Signature(_) => ValueKind::Closure,
            Type::Named(_) => unreachable!("deep underlying resolver returned a named type"),
            Type::Tuple(_) => ValueKind::Void,
        },
    )
}

/// Convert a checked type to its runtime value kind.
pub fn type_value_kind(type_key: TypeKey, tc_objs: &TCObjects) -> ValueKind {
    try_type_value_kind(type_key, tc_objs).unwrap_or_else(|error| panic!("{error}"))
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

pub fn is_port(type_key: TypeKey, tc_objs: &TCObjects) -> bool {
    let underlying = typ::underlying_type(type_key, tc_objs);
    tc_objs.types[underlying].try_as_port().is_some()
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
        matches!(
            b.typ(),
            BasicType::Float32 | BasicType::Float64 | BasicType::UntypedFloat
        )
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
            BasicType::Int | BasicType::Uint | BasicType::UntypedInt => 64,
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
pub fn struct_field_offset_by_index(
    type_key: TypeKey,
    field_index: usize,
    tc_objs: &TCObjects,
) -> (u16, u16) {
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
pub fn struct_field_type_by_index(
    type_key: TypeKey,
    field_index: usize,
    tc_objs: &TCObjects,
) -> TypeKey {
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
pub fn compute_field_offset_from_indices(
    base_type: TypeKey,
    indices: &[usize],
    tc_objs: &TCObjects,
) -> (u16, u16) {
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

#[cfg(test)]
mod layout_tests {
    use super::*;
    use crate::arena::ArenaKey;
    use crate::objects::TCObjects;
    use vo_common::Span;

    fn int_type(objects: &TCObjects) -> TypeKey {
        objects
            .universe()
            .lookup_type(BasicType::Int)
            .expect("int must be registered in the universe")
    }

    #[test]
    fn layout_handles_deep_named_and_struct_chains_iteratively() {
        let mut objects = TCObjects::new();
        let mut current = int_type(&objects);
        for _ in 0..8_192 {
            current = objects.new_t_named(None, Some(current), Vec::new());
        }

        assert_eq!(try_type_slot_count(current, &objects), Ok(1));
        assert_eq!(
            try_type_slot_types(current, &objects),
            Ok(vec![SlotType::Value])
        );
        assert_eq!(
            try_type_slot_value_kinds(current, &objects),
            Ok(vec![ValueKind::Int])
        );
        assert_eq!(try_type_value_kind(current, &objects), Ok(ValueKind::Int));

        for depth in 0..4_096 {
            let field = objects.new_field(
                Span::dummy(),
                None,
                format!("field_{depth}"),
                Some(current),
                false,
            );
            current = objects.new_t_struct(vec![field], None);
        }

        assert_eq!(try_type_slot_count(current, &objects), Ok(1));
        assert_eq!(
            try_type_slot_types(current, &objects),
            Ok(vec![SlotType::Value])
        );
        assert_eq!(
            try_type_slot_value_kinds(current, &objects),
            Ok(vec![ValueKind::Int])
        );
        assert_eq!(
            try_type_value_kind(current, &objects),
            Ok(ValueKind::Struct)
        );
    }

    #[test]
    fn layout_rejects_named_and_composite_cycles() {
        let mut objects = TCObjects::new();
        let first = objects.new_t_named(None, None, Vec::new());
        let second = objects.new_t_named(None, Some(first), Vec::new());
        objects.types[first]
            .try_as_named_mut()
            .expect("test type is named")
            .set_underlying(second);
        assert!(matches!(
            try_type_slot_count_usize(first, &objects),
            Err(TypeLayoutError::ByValueCycle(_))
        ));
        assert!(matches!(
            try_type_value_kind(first, &objects),
            Err(TypeLayoutError::ByValueCycle(_))
        ));

        let recursive = objects.new_t_named(None, None, Vec::new());
        let array = objects.new_t_array(recursive, Some(1));
        objects.types[recursive]
            .try_as_named_mut()
            .expect("test type is named")
            .set_underlying(array);
        assert!(matches!(
            try_type_slot_types(recursive, &objects),
            Err(TypeLayoutError::ByValueCycle(_))
        ));
    }

    #[test]
    fn layout_rejects_incomplete_and_invalid_metadata() {
        let mut objects = TCObjects::new();
        let incomplete_named = objects.new_t_named(None, None, Vec::new());
        assert_eq!(
            try_type_slot_count_usize(incomplete_named, &objects),
            Err(TypeLayoutError::MissingNamedUnderlying(incomplete_named))
        );

        let unresolved_array = objects.new_t_array(int_type(&objects), None);
        assert_eq!(
            try_type_slot_count_usize(unresolved_array, &objects),
            Err(TypeLayoutError::MissingArrayLength(unresolved_array))
        );

        let invalid = TypeKey::null();
        assert_eq!(
            try_type_slot_count_usize(invalid, &objects),
            Err(TypeLayoutError::InvalidTypeKey(invalid))
        );
    }

    #[test]
    fn giant_zero_slot_array_preserves_logical_validity_without_materialization() {
        let mut objects = TCObjects::new();
        let empty = objects.new_t_array(int_type(&objects), Some(0));
        let giant = objects.new_t_array(empty, Some(u64::MAX));

        assert_eq!(try_type_slot_count_usize(giant, &objects), Ok(0));
        assert_eq!(try_type_slot_types(giant, &objects), Ok(Vec::new()));
        assert_eq!(try_elem_bytes_for_heap(empty, &objects), Ok(0));
    }

    #[test]
    fn oversized_layout_fails_before_slot_vector_allocation() {
        let mut objects = TCObjects::new();
        let oversized = objects.new_t_array(int_type(&objects), Some(u64::from(u16::MAX) + 1));

        assert_eq!(
            try_type_slot_count_usize(oversized, &objects),
            Ok(usize::from(u16::MAX) + 1)
        );
        assert_eq!(
            try_type_slot_types(oversized, &objects),
            Err(TypeLayoutError::SlotCountTooWide {
                slots: usize::from(u16::MAX) + 1,
                max: usize::from(u16::MAX),
            })
        );
    }

    #[test]
    fn whole_arena_facts_preserve_vm_boundary_and_saturate_wider_layouts() {
        let mut objects = TCObjects::new();
        let int = int_type(&objects);
        let max = objects.new_t_array(int, Some(u64::from(u16::MAX)));
        let first_overwide = objects.new_t_array(int, Some(u64::from(u16::MAX) + 1));
        let beyond_wasm_usize = objects.new_t_array(int, Some(u64::from(u32::MAX) + 1));
        let nested_overwide = objects.new_t_array(beyond_wasm_usize, Some(2));
        let zero_len_overwide = objects.new_t_array(beyond_wasm_usize, Some(0));

        let facts = try_all_type_slot_counts(&objects).expect("whole-arena layout facts");
        assert_eq!(facts.slot_count(max), Ok(usize::from(u16::MAX)));
        for typ in [first_overwide, beyond_wasm_usize, nested_overwide] {
            assert_eq!(
                facts.slot_count(typ),
                Ok(usize::from(u16::MAX) + 1),
                "{typ:?} must retain the over-wide sentinel"
            );
        }
        assert_eq!(facts.slot_count(zero_len_overwide), Ok(0));
    }

    #[test]
    fn slot_count_arithmetic_overflow_is_fallible() {
        let mut objects = TCObjects::new();
        let huge = objects.new_t_array(int_type(&objects), Some(u64::MAX));
        let overflow = objects.new_t_array(huge, Some(2));

        if usize::BITS == 64 {
            assert_eq!(try_type_slot_count_usize(huge, &objects), Ok(usize::MAX));
        }
        assert_eq!(
            try_type_slot_count_usize(overflow, &objects),
            Err(TypeLayoutError::SlotCountOverflow(overflow))
        );
    }

    #[test]
    fn bounded_repeat_materializes_maximum_vm_layout() {
        let mut objects = TCObjects::new();
        let max = objects.new_t_array(int_type(&objects), Some(u64::from(u16::MAX)));
        let layout = try_type_slot_types(max, &objects).expect("u16::MAX slots are representable");
        assert_eq!(layout.len(), usize::from(u16::MAX));
        assert!(layout.iter().all(|slot| *slot == SlotType::Value));
    }
}
