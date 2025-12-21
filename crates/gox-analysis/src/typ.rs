//! Type representations for GoX.
//!
//! This module defines all types in the GoX type system.
//! Adapted from goscript with GoX-specific modifications:
//! - No complex types (complex64/128)
//! - Pointer only valid for struct types

#![allow(dead_code)]

use crate::objects::{ObjKey, ScopeKey, TCObjects, TypeKey};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashSet;
use std::mem::size_of as std_size_of;
use std::rc::Rc;

/// The main type enum representing all GoX types.
#[derive(Debug)]
pub enum Type {
    Basic(BasicDetail),
    Array(ArrayDetail),
    Slice(SliceDetail),
    Struct(StructDetail),
    Pointer(PointerDetail),
    Tuple(TupleDetail),
    Signature(SignatureDetail),
    Interface(InterfaceDetail),
    Map(MapDetail),
    Chan(ChanDetail),
    Named(NamedDetail),
}

impl Type {
    pub fn try_as_basic(&self) -> Option<&BasicDetail> {
        match self {
            Type::Basic(b) => Some(b),
            _ => None,
        }
    }

    pub fn try_as_array(&self) -> Option<&ArrayDetail> {
        match self {
            Type::Array(a) => Some(a),
            _ => None,
        }
    }

    pub fn try_as_array_mut(&mut self) -> Option<&mut ArrayDetail> {
        match self {
            Type::Array(a) => Some(a),
            _ => None,
        }
    }

    pub fn try_as_slice(&self) -> Option<&SliceDetail> {
        match self {
            Type::Slice(s) => Some(s),
            _ => None,
        }
    }

    pub fn try_as_struct(&self) -> Option<&StructDetail> {
        match self {
            Type::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn try_as_pointer(&self) -> Option<&PointerDetail> {
        match self {
            Type::Pointer(p) => Some(p),
            _ => None,
        }
    }

    pub fn try_as_tuple(&self) -> Option<&TupleDetail> {
        match self {
            Type::Tuple(t) => Some(t),
            _ => None,
        }
    }

    pub fn try_as_tuple_mut(&mut self) -> Option<&mut TupleDetail> {
        match self {
            Type::Tuple(t) => Some(t),
            _ => None,
        }
    }

    pub fn try_as_signature(&self) -> Option<&SignatureDetail> {
        match self {
            Type::Signature(s) => Some(s),
            _ => None,
        }
    }

    pub fn try_as_signature_mut(&mut self) -> Option<&mut SignatureDetail> {
        match self {
            Type::Signature(s) => Some(s),
            _ => None,
        }
    }

    pub fn try_as_interface(&self) -> Option<&InterfaceDetail> {
        match self {
            Type::Interface(i) => Some(i),
            _ => None,
        }
    }

    pub fn try_as_interface_mut(&mut self) -> Option<&mut InterfaceDetail> {
        match self {
            Type::Interface(i) => Some(i),
            _ => None,
        }
    }

    pub fn try_as_map(&self) -> Option<&MapDetail> {
        match self {
            Type::Map(m) => Some(m),
            _ => None,
        }
    }

    pub fn try_as_chan(&self) -> Option<&ChanDetail> {
        match self {
            Type::Chan(c) => Some(c),
            _ => None,
        }
    }

    pub fn try_as_chan_mut(&mut self) -> Option<&mut ChanDetail> {
        match self {
            Type::Chan(c) => Some(c),
            _ => None,
        }
    }

    pub fn try_as_named(&self) -> Option<&NamedDetail> {
        match self {
            Type::Named(n) => Some(n),
            _ => None,
        }
    }

    pub fn try_as_named_mut(&mut self) -> Option<&mut NamedDetail> {
        match self {
            Type::Named(n) => Some(n),
            _ => None,
        }
    }

    pub fn underlying(&self) -> Option<TypeKey> {
        match self {
            Type::Named(detail) => detail.underlying,
            _ => None,
        }
    }

    pub fn underlying_val<'a>(&'a self, objs: &'a TCObjects) -> &'a Type {
        if let Some(k) = self.underlying() {
            &objs.types[k]
        } else {
            self
        }
    }

    pub fn is_named(&self) -> bool {
        matches!(self, Type::Basic(_) | Type::Named(_))
    }

    pub fn is_invalid(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.info() == BasicInfo::IsInvalid,
            _ => false,
        }
    }

    pub fn is_boolean(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.info() == BasicInfo::IsBoolean,
            _ => false,
        }
    }

    pub fn is_integer(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.info() == BasicInfo::IsInteger,
            _ => false,
        }
    }

    pub fn is_unsigned(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.typ().is_unsigned(),
            _ => false,
        }
    }

    pub fn is_float(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.info() == BasicInfo::IsFloat,
            _ => false,
        }
    }

    pub fn is_numeric(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.info().is_numeric(),
            _ => false,
        }
    }

    pub fn is_string(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.info() == BasicInfo::IsString,
            _ => false,
        }
    }

    pub fn is_typed(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => !b.typ().is_untyped(),
            _ => true,
        }
    }

    pub fn is_untyped(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.typ().is_untyped(),
            _ => false,
        }
    }

    pub fn is_ordered(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.info().is_ordered(),
            _ => false,
        }
    }

    pub fn is_const_type(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.info().is_const_type(),
            _ => false,
        }
    }

    pub fn is_interface(&self, objs: &TCObjects) -> bool {
        matches!(self.underlying_val(objs), Type::Interface(_))
    }

    /// Reports whether a type includes the nil value.
    pub fn has_nil(&self, objs: &TCObjects) -> bool {
        matches!(
            self.underlying_val(objs),
            Type::Slice(_)
                | Type::Pointer(_)
                | Type::Signature(_)
                | Type::Interface(_)
                | Type::Map(_)
                | Type::Chan(_)
        )
    }

    /// Reports whether values of this type are comparable.
    pub fn comparable(&self, objs: &TCObjects) -> bool {
        match self.underlying_val(objs) {
            Type::Basic(b) => b.typ() != BasicType::UntypedNil,
            Type::Pointer(_) | Type::Interface(_) | Type::Chan(_) => true,
            Type::Struct(s) => s
                .fields()
                .iter()
                .all(|f| comparable(objs.lobjs[*f].typ().unwrap(), objs)),
            Type::Array(a) => comparable(a.elem(), objs),
            _ => false,
        }
    }
}

/// Basic type kinds (GoX version - no complex types).
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BasicType {
    Invalid,
    // predeclared types
    Bool,
    Int,
    Int8,
    Int16,
    Int32,
    Int64,
    Uint,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Uintptr,
    Float32,
    Float64,
    Str,
    // types for untyped values
    UntypedBool,
    UntypedInt,
    UntypedRune,
    UntypedFloat,
    UntypedString,
    UntypedNil,
    // aliases
    Byte, // = Uint8
    Rune, // = Int32
}

impl BasicType {
    pub fn is_unsigned(&self) -> bool {
        matches!(
            self,
            BasicType::Uint
                | BasicType::Uint8
                | BasicType::Uint16
                | BasicType::Uint32
                | BasicType::Uint64
                | BasicType::Byte
                | BasicType::Uintptr
        )
    }

    pub fn is_untyped(&self) -> bool {
        matches!(
            self,
            BasicType::UntypedBool
                | BasicType::UntypedInt
                | BasicType::UntypedRune
                | BasicType::UntypedFloat
                | BasicType::UntypedString
                | BasicType::UntypedNil
        )
    }

    pub fn real_type(&self) -> BasicType {
        match self {
            BasicType::Byte => BasicType::Uint8,
            BasicType::Rune => BasicType::Int32,
            _ => *self,
        }
    }
}

/// Classification of basic types.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BasicInfo {
    IsInvalid,
    IsBoolean,
    IsInteger,
    IsFloat,
    IsString,
}

impl BasicInfo {
    pub fn is_ordered(&self) -> bool {
        matches!(
            self,
            BasicInfo::IsInteger | BasicInfo::IsFloat | BasicInfo::IsString
        )
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, BasicInfo::IsInteger | BasicInfo::IsFloat)
    }

    pub fn is_const_type(&self) -> bool {
        matches!(
            self,
            BasicInfo::IsBoolean | BasicInfo::IsInteger | BasicInfo::IsFloat | BasicInfo::IsString
        )
    }
}

/// A BasicDetail represents a basic type.
#[derive(Copy, Clone, Debug)]
pub struct BasicDetail {
    typ: BasicType,
    info: BasicInfo,
    name: &'static str,
}

impl BasicDetail {
    pub fn new(typ: BasicType, info: BasicInfo, name: &'static str) -> BasicDetail {
        BasicDetail { typ, info, name }
    }

    pub fn typ(&self) -> BasicType {
        self.typ
    }

    pub fn info(&self) -> BasicInfo {
        self.info
    }

    pub fn name(&self) -> &str {
        self.name
    }

    pub fn size_of(&self) -> usize {
        match self.typ {
            BasicType::Bool | BasicType::Byte | BasicType::Uint8 | BasicType::Int8 => 1,
            BasicType::Int16 | BasicType::Uint16 => 2,
            BasicType::Int32 | BasicType::Uint32 | BasicType::Rune | BasicType::Float32 => 4,
            BasicType::Int64 | BasicType::Uint64 | BasicType::Float64 => 8,
            BasicType::Int | BasicType::Uint | BasicType::Uintptr => std_size_of::<usize>(),
            BasicType::Str => std_size_of::<usize>() * 2, // ptr + len
            _ => 0,
        }
    }
}

/// An ArrayDetail represents an array type.
#[derive(Debug)]
pub struct ArrayDetail {
    len: Option<u64>,
    elem: TypeKey,
}

impl ArrayDetail {
    pub fn new(elem: TypeKey, len: Option<u64>) -> ArrayDetail {
        ArrayDetail { len, elem }
    }

    pub fn len(&self) -> Option<u64> {
        self.len
    }

    pub fn set_len(&mut self, len: u64) {
        self.len = Some(len);
    }

    pub fn elem(&self) -> TypeKey {
        self.elem
    }
}

/// A SliceDetail represents a slice type.
#[derive(Debug)]
pub struct SliceDetail {
    elem: TypeKey,
}

impl SliceDetail {
    pub fn new(elem: TypeKey) -> SliceDetail {
        SliceDetail { elem }
    }

    pub fn elem(&self) -> TypeKey {
        self.elem
    }
}

/// A StructDetail represents a struct type.
#[derive(Debug)]
pub struct StructDetail {
    fields: Vec<ObjKey>,
    tags: Option<Vec<Option<String>>>,
}

impl StructDetail {
    pub fn new(fields: Vec<ObjKey>, tags: Option<Vec<Option<String>>>) -> StructDetail {
        StructDetail { fields, tags }
    }

    pub fn fields(&self) -> &Vec<ObjKey> {
        &self.fields
    }

    pub fn tags(&self) -> &Option<Vec<Option<String>>> {
        &self.tags
    }

    pub fn tag(&self, i: usize) -> Option<&String> {
        self.tags
            .as_ref()
            .and_then(|t| if i < t.len() { t[i].as_ref() } else { None })
    }
}

/// A PointerDetail represents a pointer type.
/// In GoX, pointers are only valid for struct types.
#[derive(Debug)]
pub struct PointerDetail {
    base: TypeKey,
}

impl PointerDetail {
    pub fn new(base: TypeKey) -> PointerDetail {
        PointerDetail { base }
    }

    pub fn base(&self) -> TypeKey {
        self.base
    }
}

/// A TupleDetail represents an ordered list of variables.
/// Tuples are used as components of signatures and to represent multiple assignments.
#[derive(Debug)]
pub struct TupleDetail {
    vars: Vec<ObjKey>,
}

impl TupleDetail {
    pub fn new(vars: Vec<ObjKey>) -> TupleDetail {
        TupleDetail { vars }
    }

    pub fn vars(&self) -> &Vec<ObjKey> {
        &self.vars
    }

    pub fn vars_mut(&mut self) -> &mut Vec<ObjKey> {
        &mut self.vars
    }
}

/// A SignatureDetail represents a function or method type.
#[derive(Copy, Clone, Debug)]
pub struct SignatureDetail {
    scope: Option<ScopeKey>,
    recv: Option<ObjKey>,
    params: TypeKey,
    results: TypeKey,
    variadic: bool,
}

impl SignatureDetail {
    pub fn new(
        scope: Option<ScopeKey>,
        recv: Option<ObjKey>,
        params: TypeKey,
        results: TypeKey,
        variadic: bool,
    ) -> SignatureDetail {
        SignatureDetail {
            scope,
            recv,
            params,
            results,
            variadic,
        }
    }

    pub fn scope(&self) -> Option<ScopeKey> {
        self.scope
    }

    pub fn recv(&self) -> &Option<ObjKey> {
        &self.recv
    }

    pub fn set_recv(&mut self, r: Option<ObjKey>) {
        self.recv = r;
    }

    pub fn params(&self) -> TypeKey {
        self.params
    }

    pub fn set_params(&mut self, p: TypeKey) {
        self.params = p;
    }

    pub fn results(&self) -> TypeKey {
        self.results
    }

    pub fn variadic(&self) -> bool {
        self.variadic
    }

    pub fn params_count(&self, objs: &TCObjects) -> usize {
        let l = objs.types[self.params]
            .try_as_tuple()
            .unwrap()
            .vars()
            .len();
        if self.variadic {
            l - 1
        } else {
            l
        }
    }

    pub fn results_count(&self, objs: &TCObjects) -> usize {
        objs.types[self.results]
            .try_as_tuple()
            .unwrap()
            .vars()
            .len()
    }
}

/// An InterfaceDetail represents an interface type.
#[derive(Debug)]
pub struct InterfaceDetail {
    methods: Vec<ObjKey>,
    embeddeds: Vec<TypeKey>,
    all_methods: Rc<RefCell<Option<Vec<ObjKey>>>>,
}

impl InterfaceDetail {
    pub fn new(methods: Vec<ObjKey>, embeddeds: Vec<TypeKey>) -> InterfaceDetail {
        InterfaceDetail {
            methods,
            embeddeds,
            all_methods: Rc::new(RefCell::new(None)),
        }
    }

    pub fn new_empty() -> InterfaceDetail {
        InterfaceDetail {
            methods: Vec::new(),
            embeddeds: Vec::new(),
            all_methods: Rc::new(RefCell::new(Some(Vec::new()))),
        }
    }

    pub fn methods(&self) -> &Vec<ObjKey> {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<ObjKey> {
        &mut self.methods
    }

    pub fn embeddeds(&self) -> &Vec<TypeKey> {
        &self.embeddeds
    }

    pub fn embeddeds_mut(&mut self) -> &mut Vec<TypeKey> {
        &mut self.embeddeds
    }

    pub fn all_methods(&self) -> Ref<'_, Option<Vec<ObjKey>>> {
        self.all_methods.borrow()
    }

    pub fn all_methods_mut(&self) -> RefMut<'_, Option<Vec<ObjKey>>> {
        self.all_methods.borrow_mut()
    }

    pub fn is_empty(&self) -> bool {
        self.all_methods()
            .as_ref()
            .map_or(true, |m| m.is_empty())
    }

    pub fn is_complete(&self) -> bool {
        self.all_methods.borrow().is_some()
    }

    pub fn set_complete(&self, methods: Vec<ObjKey>) {
        *self.all_methods.borrow_mut() = Some(methods);
    }

    /// Push a method to all_methods. Creates the vec if None.
    pub fn all_methods_push(&self, m: ObjKey) {
        let mut all = self.all_methods.borrow_mut();
        if all.is_none() {
            *all = Some(Vec::new());
        }
        all.as_mut().unwrap().push(m);
    }

    /// Set all_methods to empty vec (for empty interfaces).
    pub fn set_empty_complete(&self) {
        *self.all_methods.borrow_mut() = Some(Vec::new());
    }

    /// Completes the interface by collecting all methods including from embedded interfaces.
    /// This must be called after all embedded interfaces are themselves complete.
    pub fn complete(&self, objs: &TCObjects) {
        if self.all_methods.borrow().is_some() {
            return;
        }
        let mut all = self.methods.clone();
        for &tkey in &self.embeddeds {
            // Get underlying type for Named types (e.g., type Reader interface{...})
            let underlying_key = underlying_type(tkey, objs);
            if let Some(embedded) = objs.types[underlying_key].try_as_interface() {
                embedded.complete(objs);
                if let Some(ref embedded_methods) = *embedded.all_methods() {
                    all.extend(embedded_methods.iter().cloned());
                }
            }
        }
        *self.all_methods.borrow_mut() = Some(all);
    }
}

/// A MapDetail represents a map type.
#[derive(Debug)]
pub struct MapDetail {
    key: TypeKey,
    elem: TypeKey,
}

impl MapDetail {
    pub fn new(key: TypeKey, elem: TypeKey) -> MapDetail {
        MapDetail { key, elem }
    }

    pub fn key(&self) -> TypeKey {
        self.key
    }

    pub fn elem(&self) -> TypeKey {
        self.elem
    }
}

/// Channel direction.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ChanDir {
    SendRecv,
    SendOnly,
    RecvOnly,
}

/// A ChanDetail represents a channel type.
#[derive(Debug)]
pub struct ChanDetail {
    dir: ChanDir,
    elem: TypeKey,
}

impl ChanDetail {
    pub fn new(dir: ChanDir, elem: TypeKey) -> ChanDetail {
        ChanDetail { dir, elem }
    }

    pub fn dir(&self) -> ChanDir {
        self.dir
    }

    pub fn elem(&self) -> TypeKey {
        self.elem
    }
}

/// A NamedDetail represents a named (defined) type.
#[derive(Debug)]
pub struct NamedDetail {
    obj: Option<ObjKey>,
    underlying: Option<TypeKey>,
    methods: Vec<ObjKey>,
}

impl NamedDetail {
    pub fn new(
        obj: Option<ObjKey>,
        underlying: Option<TypeKey>,
        methods: Vec<ObjKey>,
    ) -> NamedDetail {
        NamedDetail {
            obj,
            underlying,
            methods,
        }
    }

    pub fn obj(&self) -> &Option<ObjKey> {
        &self.obj
    }

    pub fn set_obj(&mut self, obj: ObjKey) {
        self.obj = Some(obj);
    }

    pub fn methods(&self) -> &Vec<ObjKey> {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<ObjKey> {
        &mut self.methods
    }

    pub fn underlying(&self) -> TypeKey {
        self.underlying.unwrap()
    }

    pub fn try_underlying(&self) -> Option<TypeKey> {
        self.underlying
    }

    pub fn set_underlying(&mut self, t: TypeKey) {
        self.underlying = Some(t);
    }
}

// ----------------------------------------------------------------------------
// Utility functions

/// Returns the underlying type of type 't'.
pub fn underlying_type(t: TypeKey, objs: &TCObjects) -> TypeKey {
    objs.types[t].underlying().unwrap_or(t)
}

/// Returns the 'deep' underlying type following chains.
pub fn deep_underlying_type(t: TypeKey, objs: &TCObjects) -> TypeKey {
    let mut typ = &objs.types[t];
    let mut ret = t;
    loop {
        match typ.underlying() {
            Some(ut) => {
                typ = &objs.types[ut];
                ret = ut;
            }
            None => return ret,
        }
    }
}

pub fn is_named(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_named()
}

pub fn is_boolean(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_boolean(objs)
}

pub fn is_integer(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_integer(objs)
}

pub fn is_unsigned(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_unsigned(objs)
}

pub fn is_float(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_float(objs)
}

pub fn is_numeric(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_numeric(objs)
}

pub fn is_string(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_string(objs)
}

pub fn is_typed(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_typed(objs)
}

pub fn is_untyped(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_untyped(objs)
}

pub fn is_ordered(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_ordered(objs)
}

pub fn is_const_type(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_const_type(objs)
}

pub fn is_interface(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_interface(objs)
}

pub fn has_nil(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].has_nil(objs)
}

pub fn comparable(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].comparable(objs)
}

/// Returns the default "typed" type for an "untyped" type;
/// returns the incoming type for all other types.
/// The default type for untyped nil is untyped nil.
pub fn untyped_default_type(t: TypeKey, objs: &TCObjects) -> TypeKey {
    objs.types[t].try_as_basic().map_or(t, |bt| {
        let univ = objs.universe();
        match bt.typ() {
            BasicType::UntypedBool => univ.types()[&BasicType::Bool],
            BasicType::UntypedInt => univ.types()[&BasicType::Int],
            BasicType::UntypedRune => univ.rune(),
            BasicType::UntypedFloat => univ.types()[&BasicType::Float64],
            BasicType::UntypedString => univ.types()[&BasicType::Str],
            _ => t,
        }
    })
}

/// Reports whether x and y are identical types.
pub fn identical(x: TypeKey, y: TypeKey, objs: &TCObjects) -> bool {
    identical_impl(x, y, true, &mut HashSet::new(), objs)
}

/// Reports whether x and y are identical types (for Option<TypeKey>).
pub fn identical_o(x: Option<TypeKey>, y: Option<TypeKey>, objs: &TCObjects) -> bool {
    match (x, y) {
        (Some(a), Some(b)) => identical(a, b, objs),
        (None, None) => true,
        _ => false,
    }
}

/// Reports whether x and y are identical types, ignoring struct tags.
pub fn identical_ignore_tags(x: TypeKey, y: TypeKey, objs: &TCObjects) -> bool {
    identical_impl(x, y, false, &mut HashSet::new(), objs)
}

/// Reports whether x and y are identical types, ignoring struct tags (for Option<TypeKey>).
pub fn identical_ignore_tags_o(x: Option<TypeKey>, y: Option<TypeKey>, objs: &TCObjects) -> bool {
    match (x, y) {
        (Some(a), Some(b)) => identical_ignore_tags(a, b, objs),
        (None, None) => true,
        _ => false,
    }
}

fn identical_impl(
    x: TypeKey,
    y: TypeKey,
    cmp_tags: bool,
    dup: &mut HashSet<(TypeKey, TypeKey)>,
    objs: &TCObjects,
) -> bool {
    if x == y {
        return true;
    }

    let tx = &objs.types[x];
    let ty = &objs.types[y];

    match (tx, ty) {
        (Type::Basic(bx), Type::Basic(by)) => bx.typ().real_type() == by.typ().real_type(),
        (Type::Array(ax), Type::Array(ay)) => {
            ax.len() == ay.len() && identical_impl(ax.elem(), ay.elem(), cmp_tags, dup, objs)
        }
        (Type::Slice(sx), Type::Slice(sy)) => {
            identical_impl(sx.elem(), sy.elem(), cmp_tags, dup, objs)
        }
        (Type::Struct(sx), Type::Struct(sy)) => {
            if sx.fields().len() != sy.fields().len() {
                return false;
            }
            sx.fields().iter().enumerate().all(|(i, f)| {
                let of = &objs.lobjs[*f];
                let og = &objs.lobjs[sy.fields()[i]];
                of.var_embedded() == og.var_embedded()
                    && (!cmp_tags || sx.tag(i) == sy.tag(i))
                    && of.same_id(og.pkg(), og.name(), objs)
                    && identical_impl_o(of.typ(), og.typ(), cmp_tags, dup, objs)
            })
        }
        (Type::Pointer(px), Type::Pointer(py)) => {
            identical_impl(px.base(), py.base(), cmp_tags, dup, objs)
        }
        (Type::Tuple(tx), Type::Tuple(ty)) => {
            if tx.vars().len() != ty.vars().len() {
                return false;
            }
            tx.vars().iter().enumerate().all(|(i, v)| {
                let ov = &objs.lobjs[*v];
                let ow = &objs.lobjs[ty.vars()[i]];
                identical_impl_o(ov.typ(), ow.typ(), cmp_tags, dup, objs)
            })
        }
        (Type::Signature(sx), Type::Signature(sy)) => {
            sx.variadic() == sy.variadic()
                && identical_impl(sx.params(), sy.params(), cmp_tags, dup, objs)
                && identical_impl(sx.results(), sy.results(), cmp_tags, dup, objs)
        }
        (Type::Interface(ix), Type::Interface(iy)) => {
            let ax = ix.all_methods();
            let ay = iy.all_methods();
            match (ax.as_ref(), ay.as_ref()) {
                (Some(a), Some(b)) if a.len() == b.len() => {
                    let pair = (x, y);
                    if dup.contains(&pair) {
                        return true;
                    }
                    dup.insert(pair);
                    a.iter().enumerate().all(|(i, k)| {
                        let ox = &objs.lobjs[*k];
                        let oy = &objs.lobjs[b[i]];
                        ox.id(objs) == oy.id(objs)
                            && identical_impl_o(ox.typ(), oy.typ(), cmp_tags, dup, objs)
                    })
                }
                (None, None) => true,
                _ => false,
            }
        }
        (Type::Map(mx), Type::Map(my)) => {
            identical_impl(mx.key(), my.key(), cmp_tags, dup, objs)
                && identical_impl(mx.elem(), my.elem(), cmp_tags, dup, objs)
        }
        (Type::Chan(cx), Type::Chan(cy)) => {
            cx.dir() == cy.dir() && identical_impl(cx.elem(), cy.elem(), cmp_tags, dup, objs)
        }
        (Type::Named(nx), Type::Named(ny)) => nx.obj() == ny.obj(),
        _ => false,
    }
}

fn identical_impl_o(
    x: Option<TypeKey>,
    y: Option<TypeKey>,
    cmp_tags: bool,
    dup: &mut HashSet<(TypeKey, TypeKey)>,
    objs: &TCObjects,
) -> bool {
    match (x, y) {
        (Some(a), Some(b)) => identical_impl(a, b, cmp_tags, dup, objs),
        (None, None) => true,
        _ => false,
    }
}

// ----------------------------------------------------------------------------
// Formatting

use std::fmt::{self, Write};

pub fn fmt_type(t: Option<TypeKey>, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
    fmt_type_impl(t, f, &mut HashSet::new(), objs)
}

pub fn fmt_signature(t: TypeKey, f: &mut fmt::Formatter<'_>, objs: &TCObjects) -> fmt::Result {
    fmt_signature_impl(t, f, &mut HashSet::new(), objs)
}

fn fmt_type_impl(
    t: Option<TypeKey>,
    f: &mut fmt::Formatter<'_>,
    visited: &mut HashSet<TypeKey>,
    objs: &TCObjects,
) -> fmt::Result {
    if t.is_none() {
        return f.write_str("<nil>");
    }
    let tkey = t.unwrap();
    if visited.contains(&tkey) {
        return write!(f, "type#{:?}", tkey);
    }
    visited.insert(tkey);
    let typ = &objs.types[tkey];
    match typ {
        Type::Basic(detail) => {
            write!(f, "{}", detail.name())?;
        }
        Type::Array(detail) => {
            match detail.len() {
                Some(i) => write!(f, "[{}]", i)?,
                None => f.write_str("[unknown]")?,
            };
            fmt_type_impl(Some(detail.elem()), f, visited, objs)?;
        }
        Type::Slice(detail) => {
            f.write_str("[]")?;
            fmt_type_impl(Some(detail.elem()), f, visited, objs)?;
        }
        Type::Struct(detail) => {
            f.write_str("struct{")?;
            for (i, key) in detail.fields().iter().enumerate() {
                if i > 0 {
                    f.write_str("; ")?;
                }
                let field = &objs.lobjs[*key];
                if !field.var_embedded() {
                    write!(f, "{} ", field.name())?;
                }
                fmt_type_impl(field.typ(), f, visited, objs)?;
                if let Some(tag) = detail.tag(i) {
                    write!(f, " {}", tag)?;
                }
            }
            f.write_str("}")?;
        }
        Type::Pointer(detail) => {
            f.write_char('*')?;
            fmt_type_impl(Some(detail.base()), f, visited, objs)?;
        }
        Type::Tuple(_) => {
            fmt_tuple(tkey, false, f, visited, objs)?;
        }
        Type::Signature(_) => {
            f.write_str("func")?;
            fmt_signature_impl(tkey, f, visited, objs)?;
        }
        Type::Interface(detail) => {
            f.write_str("interface{")?;
            for (i, k) in detail.methods().iter().enumerate() {
                if i > 0 {
                    f.write_str("; ")?;
                }
                let mobj = &objs.lobjs[*k];
                f.write_str(mobj.name())?;
                fmt_signature_impl(mobj.typ().unwrap(), f, visited, objs)?;
            }
            for (i, k) in detail.embeddeds().iter().enumerate() {
                if i > 0 || !detail.methods().is_empty() {
                    f.write_str("; ")?;
                }
                fmt_type_impl(Some(*k), f, visited, objs)?;
            }
            if detail.all_methods().is_none() {
                f.write_str(" /* incomplete */")?;
            }
            f.write_char('}')?;
        }
        Type::Map(detail) => {
            f.write_str("map[")?;
            fmt_type_impl(Some(detail.key()), f, visited, objs)?;
            f.write_char(']')?;
            fmt_type_impl(Some(detail.elem()), f, visited, objs)?;
        }
        Type::Chan(detail) => {
            let (s, paren) = match detail.dir() {
                ChanDir::SendRecv => ("chan ", {
                    let elm = &objs.types[detail.elem()];
                    if let Some(c) = elm.try_as_chan() {
                        c.dir() == ChanDir::RecvOnly
                    } else {
                        false
                    }
                }),
                ChanDir::SendOnly => ("chan<- ", false),
                ChanDir::RecvOnly => ("<-chan ", false),
            };
            f.write_str(s)?;
            if paren {
                f.write_char('(')?;
            }
            fmt_type_impl(Some(detail.elem()), f, visited, objs)?;
            if paren {
                f.write_char(')')?;
            }
        }
        Type::Named(detail) => {
            if let Some(okey) = detail.obj() {
                let o = &objs.lobjs[*okey];
                if let Some(pkg) = o.pkg() {
                    objs.pkgs[pkg].fmt_with_qualifier(f, Some(&*objs.fmt_qualifier))?;
                }
                f.write_str(o.name())?;
            } else {
                f.write_str("<Named w/o object>")?;
            }
        }
    }
    Ok(())
}

fn fmt_signature_impl(
    t: TypeKey,
    f: &mut fmt::Formatter<'_>,
    visited: &mut HashSet<TypeKey>,
    objs: &TCObjects,
) -> fmt::Result {
    let sig = objs.types[t].try_as_signature().unwrap();
    fmt_tuple(sig.params(), sig.variadic(), f, visited, objs)?;
    f.write_char(' ')?;
    let results = objs.types[sig.results()].try_as_tuple().unwrap();
    if results.vars().len() == 1 {
        let obj = &objs.lobjs[results.vars()[0]];
        if obj.name().is_empty() {
            return fmt_type_impl(obj.typ(), f, visited, objs);
        }
    }
    fmt_tuple(sig.results(), false, f, visited, objs)
}

fn fmt_tuple(
    tkey: TypeKey,
    variadic: bool,
    f: &mut fmt::Formatter<'_>,
    visited: &mut HashSet<TypeKey>,
    objs: &TCObjects,
) -> fmt::Result {
    f.write_char('(')?;
    let tuple = objs.types[tkey].try_as_tuple().unwrap();
    for (i, v) in tuple.vars().iter().enumerate() {
        if i > 0 {
            f.write_str(", ")?;
        }
        let obj = &objs.lobjs[*v];
        if !obj.name().is_empty() {
            write!(f, "{} ", obj.name())?;
        }
        let var_typ = obj.typ();
        if variadic && i == tuple.vars().len() - 1 {
            let utype = underlying_type(var_typ.unwrap(), objs);
            let typ = &objs.types[utype];
            match typ {
                Type::Slice(detail) => {
                    f.write_str("...")?;
                    fmt_type_impl(Some(detail.elem()), f, visited, objs)?;
                }
                Type::Basic(detail) => {
                    assert!(detail.typ() == BasicType::Str);
                    fmt_type_impl(var_typ, f, visited, objs)?;
                    f.write_str("...")?;
                }
                _ => unreachable!(),
            }
        } else {
            fmt_type_impl(var_typ, f, visited, objs)?;
        }
    }
    f.write_char(')')?;
    Ok(())
}
