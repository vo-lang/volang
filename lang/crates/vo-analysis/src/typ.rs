//! Type representations for Vo.
//!
//! This module defines all types in the Vo type system.
//! Adapted from goscript with Vo-specific modifications:
//! - No complex types (complex64/128)
//! - Pointer only valid for struct types

use crate::objects::{ObjKey, ScopeKey, TCObjects, TypeKey};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::mem::size_of as std_size_of;
use std::rc::Rc;

/// The main type enum representing all Vo types.
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
    Port(PortDetail),
    Island,
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

    pub fn try_as_port(&self) -> Option<&PortDetail> {
        match self {
            Type::Port(p) => Some(p),
            _ => None,
        }
    }

    pub fn try_as_port_mut(&mut self) -> Option<&mut PortDetail> {
        match self {
            Type::Port(p) => Some(p),
            _ => None,
        }
    }

    pub fn queue_dir_elem(&self) -> Option<(ChanDir, TypeKey)> {
        match self {
            Type::Chan(chan) => Some((chan.dir(), chan.elem())),
            Type::Port(port) => Some((port.dir(), port.elem())),
            _ => None,
        }
    }

    pub fn is_chan(&self) -> bool {
        matches!(self, Type::Chan(_))
    }

    pub fn is_port(&self) -> bool {
        matches!(self, Type::Port(_))
    }

    pub fn is_island(&self) -> bool {
        matches!(self, Type::Island)
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
                | Type::Port(_)
                | Type::Island
        )
    }

    /// Reports whether values of this type are comparable.
    pub fn comparable(&self, objs: &TCObjects) -> bool {
        enum ComparableTask<'a> {
            Root(&'a Type),
            Key(TypeKey),
        }

        let mut tasks = vec![ComparableTask::Root(self)];
        let mut visited = HashSet::new();
        while let Some(task) = tasks.pop() {
            let typ = match task {
                ComparableTask::Root(typ) => typ,
                ComparableTask::Key(key) => {
                    if !visited.insert(key) {
                        continue;
                    }
                    let typ = &objs.types[key];
                    if let Type::Named(named) = typ {
                        let Some(underlying) = named
                            .try_underlying()
                            .and_then(|key| try_deep_underlying_type(key, objs))
                        else {
                            return false;
                        };
                        &objs.types[underlying]
                    } else {
                        typ
                    }
                }
            };

            match typ {
                Type::Basic(b) if b.typ() != BasicType::UntypedNil => {}
                Type::Pointer(_)
                | Type::Interface(_)
                | Type::Chan(_)
                | Type::Port(_)
                | Type::Island => {}
                Type::Struct(s) => {
                    for field in s.fields().iter().rev() {
                        let Some(field_type) = objs.lobjs[*field].typ() else {
                            return false;
                        };
                        tasks.push(ComparableTask::Key(field_type));
                    }
                }
                Type::Array(a) => tasks.push(ComparableTask::Key(a.elem())),
                Type::Named(named) => {
                    let Some(underlying) = named
                        .try_underlying()
                        .and_then(|key| try_deep_underlying_type(key, objs))
                    else {
                        return false;
                    };
                    tasks.push(ComparableTask::Key(underlying));
                }
                _ => return false,
            }
        }
        true
    }
}

/// Basic type kinds (Vo version - no complex types).
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
            BasicType::Int
            | BasicType::Uint
            | BasicType::Int64
            | BasicType::Uint64
            | BasicType::Float64 => 8,
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

    pub fn is_empty(&self) -> bool {
        self.len.is_some_and(|n| n == 0)
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
/// In Vo, pointers are only valid for struct types.
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
        let l = objs.types[self.params].try_as_tuple().unwrap().vars().len();
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

    pub fn new_complete(methods: Vec<ObjKey>, embeddeds: Vec<TypeKey>) -> InterfaceDetail {
        let all = methods.clone();
        InterfaceDetail {
            methods,
            embeddeds,
            all_methods: Rc::new(RefCell::new(Some(all))),
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
        self.all_methods().as_ref().is_none_or(|m| m.is_empty())
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

    /// Completes this interface and every incomplete embedded interface.
    ///
    /// Completion is transactional: malformed embedding metadata or an embedding cycle leaves
    /// every previously incomplete interface discovered by this call unchanged. This keeps
    /// callers from observing a partially populated method cache after an internal type-checker
    /// invariant is violated.
    pub fn complete(&self, objs: &TCObjects) {
        let Ok(root_methods) = self.all_methods.try_borrow() else {
            return;
        };
        if root_methods.is_some() {
            return;
        }
        drop(root_methods);

        #[derive(Clone, Copy)]
        enum Visit<'a> {
            Enter(&'a InterfaceDetail),
            Exit(&'a InterfaceDetail),
        }

        fn cache_id(interface: &InterfaceDetail) -> usize {
            Rc::as_ptr(&interface.all_methods) as usize
        }

        // First build a post-order of all incomplete interfaces. No cache is changed during this
        // pass, so every early return below preserves the original graph state.
        let mut work = vec![Visit::Enter(self)];
        let mut visiting = HashSet::new();
        let mut settled = HashSet::new();
        let mut interfaces = HashMap::new();
        let mut postorder = Vec::new();

        while let Some(visit) = work.pop() {
            match visit {
                Visit::Enter(interface) => {
                    let id = cache_id(interface);
                    if settled.contains(&id) {
                        continue;
                    }

                    let Ok(methods) = interface.all_methods.try_borrow() else {
                        return;
                    };
                    if methods.is_some() {
                        settled.insert(id);
                        continue;
                    }
                    drop(methods);

                    if !visiting.insert(id) {
                        return;
                    }
                    interfaces.insert(id, interface);
                    work.push(Visit::Exit(interface));

                    // Reverse the push order so the explicit stack visits embeddings in source
                    // order, matching the former recursive traversal.
                    for &embedded_key in interface.embeddeds.iter().rev() {
                        let Some(underlying_key) = try_deep_underlying_type(embedded_key, objs)
                        else {
                            return;
                        };
                        let Some(embedded) = objs.types[underlying_key].try_as_interface() else {
                            return;
                        };
                        let embedded_id = cache_id(embedded);
                        if visiting.contains(&embedded_id) {
                            return;
                        }
                        if !settled.contains(&embedded_id) {
                            work.push(Visit::Enter(embedded));
                        }
                    }
                }
                Visit::Exit(interface) => {
                    let id = cache_id(interface);
                    if !visiting.remove(&id) {
                        return;
                    }
                    settled.insert(id);
                    postorder.push(id);
                }
            }
        }

        // Compute every cache value in temporary storage. Embedded interfaces precede their
        // parents in post-order, while already complete interfaces are trusted as graph leaves.
        let mut computed: HashMap<usize, Vec<ObjKey>> = HashMap::with_capacity(postorder.len());
        for &id in &postorder {
            let Some(&interface) = interfaces.get(&id) else {
                return;
            };
            let mut all = interface.methods.clone();
            for &embedded_key in &interface.embeddeds {
                let Some(underlying_key) = try_deep_underlying_type(embedded_key, objs) else {
                    return;
                };
                let Some(embedded) = objs.types[underlying_key].try_as_interface() else {
                    return;
                };
                let embedded_id = cache_id(embedded);
                let Ok(cached) = embedded.all_methods.try_borrow() else {
                    return;
                };
                if let Some(methods) = cached.as_ref() {
                    all.extend(methods.iter().copied());
                } else if let Some(methods) = computed.get(&embedded_id) {
                    all.extend(methods.iter().copied());
                } else {
                    return;
                }
            }
            computed.insert(id, all);
        }

        // Acquire every mutable cache borrow before changing any cache. This also makes the
        // transaction robust when a public Ref/RefMut guard is alive at the call site.
        let mut updates = Vec::with_capacity(postorder.len());
        for &id in &postorder {
            let Some(&interface) = interfaces.get(&id) else {
                return;
            };
            let Some(methods) = computed.remove(&id) else {
                return;
            };
            let Ok(slot) = interface.all_methods.try_borrow_mut() else {
                return;
            };
            if slot.is_some() {
                return;
            }
            updates.push((slot, methods));
        }
        for (mut slot, methods) in updates {
            *slot = Some(methods);
        }
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

#[derive(Debug)]
pub struct PortDetail {
    dir: ChanDir,
    elem: TypeKey,
}

impl PortDetail {
    pub fn new(dir: ChanDir, elem: TypeKey) -> PortDetail {
        PortDetail { dir, elem }
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

/// Returns the 'deep' underlying type following chains, or `None` for invalid/cyclic metadata.
pub fn try_deep_underlying_type(t: TypeKey, objs: &TCObjects) -> Option<TypeKey> {
    let mut visited = HashSet::new();
    let mut ret = t;
    loop {
        if !visited.insert(ret) {
            return None;
        }
        let Some(underlying) = objs.types.get(ret)?.underlying() else {
            return Some(ret);
        };
        ret = underlying;
    }
}

/// Returns the 'deep' underlying type following chains.
///
/// Invalid or cyclic named-type metadata is an internal type-checker invariant violation.
pub fn deep_underlying_type(t: TypeKey, objs: &TCObjects) -> TypeKey {
    try_deep_underlying_type(t, objs).expect("invalid or cyclic named type metadata")
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

pub fn is_chan(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_chan()
}

pub fn is_island(t: TypeKey, objs: &TCObjects) -> bool {
    objs.types[t].is_island()
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
    seen: &mut HashSet<(TypeKey, TypeKey)>,
    objs: &TCObjects,
) -> bool {
    let mut tasks = vec![(x, y)];
    while let Some((x, y)) = tasks.pop() {
        if x == y || !seen.insert((x, y)) {
            continue;
        }

        match (&objs.types[x], &objs.types[y]) {
            (Type::Basic(bx), Type::Basic(by)) => {
                if bx.typ().real_type() != by.typ().real_type() {
                    return false;
                }
            }
            (Type::Array(ax), Type::Array(ay)) => {
                if ax.len() != ay.len() {
                    return false;
                }
                tasks.push((ax.elem(), ay.elem()));
            }
            (Type::Slice(sx), Type::Slice(sy)) => tasks.push((sx.elem(), sy.elem())),
            (Type::Struct(sx), Type::Struct(sy)) => {
                if sx.fields().len() != sy.fields().len() {
                    return false;
                }
                for (i, (fx, fy)) in sx.fields().iter().zip(sy.fields()).enumerate().rev() {
                    let ox = &objs.lobjs[*fx];
                    let oy = &objs.lobjs[*fy];
                    if ox.var_embedded() != oy.var_embedded()
                        || (cmp_tags && sx.tag(i) != sy.tag(i))
                        || !ox.same_id(oy.pkg(), oy.name(), objs)
                    {
                        return false;
                    }
                    match (ox.typ(), oy.typ()) {
                        (Some(tx), Some(ty)) => tasks.push((tx, ty)),
                        (None, None) => {}
                        _ => return false,
                    }
                }
            }
            (Type::Pointer(px), Type::Pointer(py)) => tasks.push((px.base(), py.base())),
            (Type::Tuple(tx), Type::Tuple(ty)) => {
                if tx.vars().len() != ty.vars().len() {
                    return false;
                }
                for (vx, vy) in tx.vars().iter().zip(ty.vars()).rev() {
                    match (objs.lobjs[*vx].typ(), objs.lobjs[*vy].typ()) {
                        (Some(tx), Some(ty)) => tasks.push((tx, ty)),
                        (None, None) => {}
                        _ => return false,
                    }
                }
            }
            (Type::Signature(sx), Type::Signature(sy)) => {
                if sx.variadic() != sy.variadic() {
                    return false;
                }
                tasks.push((sx.results(), sy.results()));
                tasks.push((sx.params(), sy.params()));
            }
            (Type::Interface(ix), Type::Interface(iy)) => {
                let ax = ix.all_methods();
                let ay = iy.all_methods();
                match (ax.as_ref(), ay.as_ref()) {
                    (Some(mx), Some(my)) if mx.len() == my.len() => {
                        for (method_x, method_y) in mx.iter().zip(my).rev() {
                            let ox = &objs.lobjs[*method_x];
                            let oy = &objs.lobjs[*method_y];
                            if ox.id(objs) != oy.id(objs) {
                                return false;
                            }
                            match (ox.typ(), oy.typ()) {
                                (Some(tx), Some(ty)) => tasks.push((tx, ty)),
                                (None, None) => {}
                                _ => return false,
                            }
                        }
                    }
                    (None, None) => {}
                    _ => return false,
                }
            }
            (Type::Map(mx), Type::Map(my)) => {
                tasks.push((mx.elem(), my.elem()));
                tasks.push((mx.key(), my.key()));
            }
            (Type::Chan(cx), Type::Chan(cy)) => {
                if cx.dir() != cy.dir() {
                    return false;
                }
                tasks.push((cx.elem(), cy.elem()));
            }
            (Type::Port(px), Type::Port(py)) => {
                if px.dir() != py.dir() {
                    return false;
                }
                tasks.push((px.elem(), py.elem()));
            }
            (Type::Island, Type::Island) => {}
            (Type::Named(nx), Type::Named(ny)) if nx.obj() == ny.obj() => {}
            _ => return false,
        }
    }
    true
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

enum FmtTask {
    Type(Option<TypeKey>),
    LeaveType(TypeKey),
    Signature(TypeKey),
    Tuple(TypeKey, bool),
    Static(&'static str),
    Owned(String),
    Char(char),
}

fn fmt_type_impl(
    t: Option<TypeKey>,
    f: &mut fmt::Formatter<'_>,
    visited: &mut HashSet<TypeKey>,
    objs: &TCObjects,
) -> fmt::Result {
    run_fmt_tasks(vec![FmtTask::Type(t)], f, visited, objs)
}

fn fmt_signature_impl(
    t: TypeKey,
    f: &mut fmt::Formatter<'_>,
    visited: &mut HashSet<TypeKey>,
    objs: &TCObjects,
) -> fmt::Result {
    run_fmt_tasks(vec![FmtTask::Signature(t)], f, visited, objs)
}

fn run_fmt_tasks(
    mut tasks: Vec<FmtTask>,
    f: &mut fmt::Formatter<'_>,
    visited: &mut HashSet<TypeKey>,
    objs: &TCObjects,
) -> fmt::Result {
    while let Some(task) = tasks.pop() {
        match task {
            FmtTask::Static(text) => f.write_str(text)?,
            FmtTask::Owned(text) => f.write_str(&text)?,
            FmtTask::Char(ch) => f.write_char(ch)?,
            FmtTask::Type(None) => f.write_str("<nil>")?,
            FmtTask::Type(Some(tkey)) => {
                if !visited.insert(tkey) {
                    write!(f, "type#{:?}", tkey)?;
                    continue;
                }
                tasks.push(FmtTask::LeaveType(tkey));
                match &objs.types[tkey] {
                    Type::Basic(detail) => f.write_str(detail.name())?,
                    Type::Array(detail) => {
                        match detail.len() {
                            Some(len) => write!(f, "[{}]", len)?,
                            None => f.write_str("[unknown]")?,
                        }
                        tasks.push(FmtTask::Type(Some(detail.elem())));
                    }
                    Type::Slice(detail) => {
                        f.write_str("[]")?;
                        tasks.push(FmtTask::Type(Some(detail.elem())));
                    }
                    Type::Struct(detail) => {
                        f.write_str("struct{")?;
                        let mut sequence = Vec::new();
                        for (index, key) in detail.fields().iter().enumerate() {
                            if index > 0 {
                                sequence.push(FmtTask::Static("; "));
                            }
                            let field = &objs.lobjs[*key];
                            if !field.var_embedded() {
                                sequence.push(FmtTask::Owned(format!("{} ", field.name())));
                            }
                            sequence.push(FmtTask::Type(field.typ()));
                            if let Some(tag) = detail.tag(index) {
                                sequence.push(FmtTask::Owned(format!(" {}", tag)));
                            }
                        }
                        sequence.push(FmtTask::Char('}'));
                        tasks.extend(sequence.into_iter().rev());
                    }
                    Type::Pointer(detail) => {
                        f.write_char('*')?;
                        tasks.push(FmtTask::Type(Some(detail.base())));
                    }
                    Type::Tuple(_) => tasks.push(FmtTask::Tuple(tkey, false)),
                    Type::Signature(_) => {
                        f.write_str("func")?;
                        tasks.push(FmtTask::Signature(tkey));
                    }
                    Type::Interface(detail) => {
                        f.write_str("interface{")?;
                        let mut sequence = Vec::new();
                        for (index, key) in detail.methods().iter().enumerate() {
                            if index > 0 {
                                sequence.push(FmtTask::Static("; "));
                            }
                            let method = &objs.lobjs[*key];
                            sequence.push(FmtTask::Owned(method.name().to_string()));
                            sequence.push(FmtTask::Signature(method.typ().unwrap()));
                        }
                        for (index, embedded) in detail.embeddeds().iter().enumerate() {
                            if index > 0 || !detail.methods().is_empty() {
                                sequence.push(FmtTask::Static("; "));
                            }
                            sequence.push(FmtTask::Type(Some(*embedded)));
                        }
                        if detail.all_methods().is_none() {
                            sequence.push(FmtTask::Static(" /* incomplete */"));
                        }
                        sequence.push(FmtTask::Char('}'));
                        tasks.extend(sequence.into_iter().rev());
                    }
                    Type::Map(detail) => {
                        f.write_str("map[")?;
                        tasks.push(FmtTask::Type(Some(detail.elem())));
                        tasks.push(FmtTask::Char(']'));
                        tasks.push(FmtTask::Type(Some(detail.key())));
                    }
                    Type::Chan(detail) => {
                        let (prefix, paren) = match detail.dir() {
                            ChanDir::SendRecv => (
                                "chan ",
                                objs.types[detail.elem()]
                                    .try_as_chan()
                                    .is_some_and(|chan| chan.dir() == ChanDir::RecvOnly),
                            ),
                            ChanDir::SendOnly => ("chan<- ", false),
                            ChanDir::RecvOnly => ("<-chan ", false),
                        };
                        f.write_str(prefix)?;
                        if paren {
                            f.write_char('(')?;
                            tasks.push(FmtTask::Char(')'));
                        }
                        tasks.push(FmtTask::Type(Some(detail.elem())));
                    }
                    Type::Port(detail) => {
                        let (prefix, paren) = match detail.dir() {
                            ChanDir::SendRecv => (
                                "port ",
                                objs.types[detail.elem()]
                                    .try_as_port()
                                    .is_some_and(|port| port.dir() == ChanDir::RecvOnly),
                            ),
                            ChanDir::SendOnly => ("port<- ", false),
                            ChanDir::RecvOnly => ("<-port ", false),
                        };
                        f.write_str(prefix)?;
                        if paren {
                            f.write_char('(')?;
                            tasks.push(FmtTask::Char(')'));
                        }
                        tasks.push(FmtTask::Type(Some(detail.elem())));
                    }
                    Type::Island => f.write_str("island")?,
                    Type::Named(detail) => {
                        if let Some(okey) = detail.obj() {
                            let obj = &objs.lobjs[*okey];
                            if let Some(pkg) = obj.pkg() {
                                objs.pkgs[pkg].fmt_with_qualifier(f, Some(&*objs.fmt_qualifier))?;
                            }
                            f.write_str(obj.name())?;
                        } else {
                            f.write_str("<Named w/o object>")?;
                        }
                    }
                }
            }
            FmtTask::LeaveType(tkey) => {
                visited.remove(&tkey);
            }
            FmtTask::Signature(tkey) => {
                let signature = objs.types[tkey].try_as_signature().unwrap();
                let mut sequence = vec![
                    FmtTask::Tuple(signature.params(), signature.variadic()),
                    FmtTask::Char(' '),
                ];
                let results = objs.types[signature.results()].try_as_tuple().unwrap();
                if results.vars().len() == 1 {
                    let result = &objs.lobjs[results.vars()[0]];
                    if result.name().is_empty() {
                        sequence.push(FmtTask::Type(result.typ()));
                    } else {
                        sequence.push(FmtTask::Tuple(signature.results(), false));
                    }
                } else {
                    sequence.push(FmtTask::Tuple(signature.results(), false));
                }
                tasks.extend(sequence.into_iter().rev());
            }
            FmtTask::Tuple(tkey, variadic) => {
                let tuple = objs.types[tkey].try_as_tuple().unwrap();
                let mut sequence = vec![FmtTask::Char('(')];
                for (index, key) in tuple.vars().iter().enumerate() {
                    if index > 0 {
                        sequence.push(FmtTask::Static(", "));
                    }
                    let object = &objs.lobjs[*key];
                    if !object.name().is_empty() {
                        sequence.push(FmtTask::Owned(format!("{} ", object.name())));
                    }
                    let variable_type = object.typ();
                    if variadic && index == tuple.vars().len() - 1 {
                        let underlying = underlying_type(variable_type.unwrap(), objs);
                        match &objs.types[underlying] {
                            Type::Slice(detail) => {
                                sequence.push(FmtTask::Static("..."));
                                sequence.push(FmtTask::Type(Some(detail.elem())));
                            }
                            Type::Basic(detail) => {
                                assert!(detail.typ() == BasicType::Str);
                                sequence.push(FmtTask::Type(variable_type));
                                sequence.push(FmtTask::Static("..."));
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        sequence.push(FmtTask::Type(variable_type));
                    }
                }
                sequence.push(FmtTask::Char(')'));
                tasks.extend(sequence.into_iter().rev());
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::type_string;

    const DEEP_TYPE_NESTING: usize = 8_192;

    fn basic_type(objs: &TCObjects, basic: BasicType) -> TypeKey {
        objs.universe()
            .lookup_type(basic)
            .expect("predeclared basic type")
    }

    #[test]
    fn comparable_and_identical_handle_deep_types_without_host_recursion() {
        let mut objs = TCObjects::new();
        let int_type = basic_type(&objs, BasicType::Int);
        let bool_type = basic_type(&objs, BasicType::Bool);
        let mut left = int_type;
        let mut right = int_type;
        let mut mismatch = bool_type;
        for _ in 0..DEEP_TYPE_NESTING {
            left = objs.new_t_array(left, Some(1));
            right = objs.new_t_array(right, Some(1));
            mismatch = objs.new_t_array(mismatch, Some(1));
        }

        assert!(objs.types[left].comparable(&objs));
        assert!(identical(left, right, &objs));
        assert!(!identical(left, mismatch, &objs));

        let mut non_comparable = objs.new_t_slice(int_type);
        for _ in 0..DEEP_TYPE_NESTING {
            non_comparable = objs.new_t_array(non_comparable, Some(1));
        }
        assert!(!objs.types[non_comparable].comparable(&objs));
    }

    #[test]
    fn deep_type_formatting_uses_an_explicit_task_stack() {
        let mut objs = TCObjects::new();
        let mut typ = basic_type(&objs, BasicType::Int);
        for _ in 0..DEEP_TYPE_NESTING {
            typ = objs.new_t_array(typ, Some(1));
        }

        let rendered = type_string(typ, &objs);
        assert_eq!(rendered.len(), DEEP_TYPE_NESTING * 3 + "int".len());
        assert!(rendered.starts_with("[1][1][1]"));
        assert!(rendered.ends_with("int"));
    }

    #[test]
    fn cyclic_named_metadata_is_detected() {
        let mut objs = TCObjects::new();
        let first = objs.new_t_named(None, None, Vec::new());
        let second = objs.new_t_named(None, Some(first), Vec::new());
        objs.types[first]
            .try_as_named_mut()
            .expect("named type")
            .set_underlying(second);

        assert_eq!(try_deep_underlying_type(first, &objs), None);
        assert!(!objs.types[first].comparable(&objs));
    }

    #[test]
    fn interface_completion_handles_deep_embedding_graphs_iteratively() {
        let mut objs = TCObjects::new();
        let mut current = objs.new_t_interface(Vec::new(), Vec::new());
        let mut interfaces = vec![current];
        for _ in 1..DEEP_TYPE_NESTING {
            current = objs.new_t_interface(Vec::new(), vec![current]);
            interfaces.push(current);
        }

        objs.types[current]
            .try_as_interface()
            .expect("interface type")
            .complete(&objs);

        for key in interfaces {
            let interface = objs.types[key].try_as_interface().expect("interface type");
            assert!(interface.is_complete());
            assert!(interface.all_methods().as_ref().is_some_and(Vec::is_empty));
        }
    }

    #[test]
    fn interface_completion_is_transactional_when_an_embedding_cycle_exists() {
        let mut objs = TCObjects::new();
        let acyclic_leaf = objs.new_t_interface(Vec::new(), Vec::new());
        let cycle_a = objs.new_t_interface(Vec::new(), Vec::new());
        let cycle_b = objs.new_t_interface(Vec::new(), vec![cycle_a]);
        objs.types[cycle_a]
            .try_as_interface_mut()
            .expect("interface type")
            .embeddeds_mut()
            .push(cycle_b);
        let root = objs.new_t_interface(Vec::new(), vec![acyclic_leaf, cycle_a]);

        objs.types[root]
            .try_as_interface()
            .expect("interface type")
            .complete(&objs);

        for key in [root, acyclic_leaf, cycle_a, cycle_b] {
            assert!(
                !objs.types[key]
                    .try_as_interface()
                    .expect("interface type")
                    .is_complete(),
                "interface {key:?} was partially completed"
            );
        }
    }

    #[test]
    fn interface_completion_rejects_invalid_embedding_metadata() {
        let mut objs = TCObjects::new();
        let int_type = basic_type(&objs, BasicType::Int);
        let invalid_type = <TypeKey as crate::arena::ArenaKey>::null();
        assert_eq!(try_deep_underlying_type(invalid_type, &objs), None);
        let invalid_key_root = objs.new_t_interface(Vec::new(), vec![invalid_type]);
        objs.types[invalid_key_root]
            .try_as_interface()
            .expect("interface type")
            .complete(&objs);
        assert!(!objs.types[invalid_key_root]
            .try_as_interface()
            .expect("interface type")
            .is_complete());

        let non_interface_root = objs.new_t_interface(Vec::new(), vec![int_type]);
        objs.types[non_interface_root]
            .try_as_interface()
            .expect("interface type")
            .complete(&objs);
        assert!(!objs.types[non_interface_root]
            .try_as_interface()
            .expect("interface type")
            .is_complete());

        let named_a = objs.new_t_named(None, None, Vec::new());
        let named_b = objs.new_t_named(None, Some(named_a), Vec::new());
        objs.types[named_a]
            .try_as_named_mut()
            .expect("named type")
            .set_underlying(named_b);
        let named_cycle_root = objs.new_t_interface(Vec::new(), vec![named_a]);
        objs.types[named_cycle_root]
            .try_as_interface()
            .expect("interface type")
            .complete(&objs);
        assert!(!objs.types[named_cycle_root]
            .try_as_interface()
            .expect("interface type")
            .is_complete());
    }

    #[test]
    fn interface_completion_preserves_method_order_across_embeddings() {
        let mut objs = TCObjects::new();
        let int_type = basic_type(&objs, BasicType::Int);
        let first = objs.new_var(
            vo_common::span::Span::default(),
            None,
            "first".to_string(),
            Some(int_type),
        );
        let second = objs.new_var(
            vo_common::span::Span::default(),
            None,
            "second".to_string(),
            Some(int_type),
        );
        let third = objs.new_var(
            vo_common::span::Span::default(),
            None,
            "third".to_string(),
            Some(int_type),
        );
        let embedded = objs.new_t_interface(vec![second, third], Vec::new());
        let root = objs.new_t_interface(vec![first], vec![embedded]);

        let root_interface = objs.types[root].try_as_interface().expect("interface type");
        root_interface.complete(&objs);

        assert_eq!(
            root_interface.all_methods().as_deref(),
            Some([first, second, third].as_slice())
        );
    }

    #[test]
    fn iterative_formatter_preserves_composite_type_syntax() {
        let mut objs = TCObjects::new();
        let int_type = basic_type(&objs, BasicType::Int);
        let string_type = basic_type(&objs, BasicType::Str);
        let values = objs.new_t_slice(string_type);
        let mapping = objs.new_t_map(int_type, values);

        assert_eq!(type_string(mapping, &objs), "map[int][]string");
    }

    #[test]
    fn iterative_formatter_distinguishes_shared_types_from_cycles() {
        let mut objs = TCObjects::new();
        let int_type = basic_type(&objs, BasicType::Int);
        let first = objs.new_field(
            vo_common::span::Span::default(),
            None,
            "first".to_string(),
            Some(int_type),
            false,
        );
        let second = objs.new_field(
            vo_common::span::Span::default(),
            None,
            "second".to_string(),
            Some(int_type),
            false,
        );
        let shared = objs.new_t_struct(vec![first, second], None);

        assert_eq!(type_string(shared, &objs), "struct{first int; second int}");

        let cyclic = objs.new_t_pointer(int_type);
        let Type::Pointer(pointer) = &mut objs.types[cyclic] else {
            panic!("pointer type");
        };
        pointer.base = cyclic;
        assert!(type_string(cyclic, &objs).contains("type#TypeKey"));
    }
}
