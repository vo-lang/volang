//! FFI (Foreign Function Interface) for Vo native extensions.
//!
//! This module provides the interface for implementing Vo functions in Rust.
//! Both VM interpreter and JIT compiler use these types.
//!
//! # Example
//!
//! ```ignore
//! use vo_runtime::ffi::{ExternCall, ExternResult};
//!
//! fn my_add(call: &mut ExternCall) -> ExternResult {
//!     let a = call.arg_i64(0);
//!     let b = call.arg_i64(1);
//!     call.ret_i64(0, a + b);
//!     ExternResult::Ok
//! }
//! ```

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use linkme::distributed_slice;

use crate::gc::{Gc, GcRef};
use crate::objects::{string, slice};
use vo_common_core::bytecode::{DynErrorCodes, InterfaceMeta, NamedTypeMeta, StructMeta, WellKnownTypes};
use vo_common_core::runtime_type::RuntimeType;
use vo_common_core::types::{ValueKind, ValueMeta, ValueRttid};
use crate::itab::ItabCache;

/// Extern function execution result.
#[derive(Debug, Clone)]
pub enum ExternResult {
    /// Success.
    Ok,
    /// Yield to scheduler (for async operations).
    Yield,
    /// Panic with error message.
    Panic(String),
}

/// Extern function signature.
pub type ExternFn = fn(&mut ExternCall) -> ExternResult;

/// Extern function with full context (GC + type metadata).
pub type ExternFnWithContext = fn(&mut ExternCallContext) -> ExternResult;

// ==================== Auto-registration via linkme ====================

/// Entry for auto-registered extern functions.
pub struct ExternEntry {
    /// Function name in format "pkg_FuncName" (e.g., "fmt_Println").
    pub name: &'static str,
    /// The extern function.
    pub func: ExternFn,
}

/// Entry for auto-registered extern functions with full context.
pub struct ExternEntryWithContext {
    /// Function name in format "pkg_FuncName" (e.g., "fmt_Sprint").
    pub name: &'static str,
    /// The extern function with full context.
    pub func: ExternFnWithContext,
}

/// Distributed slice for auto-registered extern functions.
#[distributed_slice]
pub static EXTERN_TABLE: [ExternEntry] = [..];

/// Distributed slice for auto-registered extern functions with full context.
#[distributed_slice]
pub static EXTERN_TABLE_WITH_CONTEXT: [ExternEntryWithContext] = [..];

/// Lookup an extern function by name.
pub fn lookup_extern(name: &str) -> Option<ExternFn> {
    for entry in EXTERN_TABLE {
        if entry.name == name {
            return Some(entry.func);
        }
    }
    None
}

/// Lookup an extern function with full context by name.
pub fn lookup_extern_with_context(name: &str) -> Option<ExternFnWithContext> {
    for entry in EXTERN_TABLE_WITH_CONTEXT {
        if entry.name == name {
            return Some(entry.func);
        }
    }
    None
}

/// External function call context - provides type-safe stack access.
///
/// This is the main interface for extern functions that don't need GC allocation.
pub struct ExternCall<'a> {
    /// Stack slots.
    stack: &'a mut [u64],
    /// Base pointer (frame start).
    bp: usize,
    /// Argument start slot (relative to bp).
    arg_start: u16,
    /// Argument count (number of slots).
    arg_count: u16,
    /// Return value start slot (relative to bp).
    ret_start: u16,
}

impl<'a> ExternCall<'a> {
    /// Create a new extern call context.
    #[inline]
    pub fn new(stack: &'a mut [u64], bp: usize, arg_start: u16, arg_count: u16, ret_start: u16) -> Self {
        Self { stack, bp, arg_start, arg_count, ret_start }
    }

    // ==================== Raw Slot Access ====================

    /// Get the number of available slots from bp to end of stack.
    #[inline]
    pub fn available_slots(&self) -> usize {
        self.stack.len().saturating_sub(self.bp)
    }

    /// Get the number of argument slots passed to this call.
    #[inline]
    pub fn arg_count(&self) -> u16 {
        self.arg_count
    }

    /// Read a raw slot value.
    #[inline]
    pub fn slot(&self, offset: u16) -> u64 {
        self.stack[self.bp + offset as usize]
    }

    /// Write a raw slot value.
    #[inline]
    pub fn set_slot(&mut self, offset: u16, val: u64) {
        self.stack[self.bp + offset as usize] = val;
    }

    // ==================== Argument Reading ====================

    /// Read argument as i64.
    #[inline]
    pub fn arg_i64(&self, n: u16) -> i64 {
        self.slot(self.arg_start + n) as i64
    }

    /// Read argument as u64.
    #[inline]
    pub fn arg_u64(&self, n: u16) -> u64 {
        self.slot(self.arg_start + n)
    }

    /// Read argument as f64.
    #[inline]
    pub fn arg_f64(&self, n: u16) -> f64 {
        f64::from_bits(self.slot(self.arg_start + n))
    }

    /// Read argument as bool.
    #[inline]
    pub fn arg_bool(&self, n: u16) -> bool {
        self.slot(self.arg_start + n) != 0
    }

    /// Read argument as GcRef.
    #[inline]
    pub fn arg_ref(&self, n: u16) -> GcRef {
        self.slot(self.arg_start + n) as GcRef
    }

    // ==================== Return Value Writing ====================

    /// Write return value as i64.
    #[inline]
    pub fn ret_i64(&mut self, n: u16, val: i64) {
        self.set_slot(self.ret_start + n, val as u64);
    }

    /// Write return value as u64.
    #[inline]
    pub fn ret_u64(&mut self, n: u16, val: u64) {
        self.set_slot(self.ret_start + n, val);
    }

    /// Write return value as f64.
    #[inline]
    pub fn ret_f64(&mut self, n: u16, val: f64) {
        self.set_slot(self.ret_start + n, val.to_bits());
    }

    /// Write return value as bool.
    #[inline]
    pub fn ret_bool(&mut self, n: u16, val: bool) {
        self.set_slot(self.ret_start + n, val as u64);
    }

    /// Write return value as GcRef.
    #[inline]
    pub fn ret_ref(&mut self, n: u16, val: GcRef) {
        self.set_slot(self.ret_start + n, val as u64);
    }

    /// Write nil return value.
    #[inline]
    pub fn ret_nil(&mut self, n: u16) {
        self.set_slot(self.ret_start + n, 0);
    }
}

/// External function call context with full runtime access.
///
/// Provides GC allocation and type metadata access for extern functions.
pub struct ExternCallContext<'a> {
    /// Base call context.
    call: ExternCall<'a>,
    /// GC for allocations.
    gc: &'a mut Gc,
    /// Struct metadata for reflection.
    struct_metas: &'a [StructMeta],
    /// Named type metadata for reflection.
    named_type_metas: &'a [NamedTypeMeta],
    interface_metas: &'a [InterfaceMeta],
    /// Runtime types for rttid resolution.
    runtime_types: &'a [RuntimeType],
    /// Pre-computed IDs for well-known types.
    well_known: &'a WellKnownTypes,
    itab_cache: &'a mut ItabCache,
}

impl<'a> ExternCallContext<'a> {
    /// Create a new extern call context.
    #[inline]
    pub fn new(
        stack: &'a mut [u64],
        bp: usize,
        arg_start: u16,
        arg_count: u16,
        ret_start: u16,
        gc: &'a mut Gc,
        struct_metas: &'a [StructMeta],
        named_type_metas: &'a [NamedTypeMeta],
        interface_metas: &'a [InterfaceMeta],
        runtime_types: &'a [RuntimeType],
        well_known: &'a WellKnownTypes,
        itab_cache: &'a mut ItabCache,
    ) -> Self {
        Self {
            call: ExternCall::new(stack, bp, arg_start, arg_count, ret_start),
            gc,
            struct_metas,
            named_type_metas,
            interface_metas,
            runtime_types,
            well_known,
            itab_cache,
        }
    }

    /// Get struct metadata by index.
    #[inline]
    pub fn struct_meta(&self, idx: usize) -> Option<&StructMeta> {
        self.struct_metas.get(idx)
    }

    /// Get named type metadata by index.
    #[inline]
    pub fn named_type_meta(&self, idx: usize) -> Option<&NamedTypeMeta> {
        self.named_type_metas.get(idx)
    }

    #[inline]
    pub fn named_type_metas(&self) -> &'a [NamedTypeMeta] {
        self.named_type_metas
    }

    #[inline]
    pub fn interface_meta(&self, idx: usize) -> Option<&InterfaceMeta> {
        self.interface_metas.get(idx)
    }

    #[inline]
    pub fn interface_metas(&self) -> &'a [InterfaceMeta] {
        self.interface_metas
    }

    #[inline]
    pub fn runtime_types(&self) -> &'a [RuntimeType] {
        self.runtime_types
    }

    #[inline]
    pub fn well_known(&self) -> &'a WellKnownTypes {
        self.well_known
    }

    #[inline]
    pub fn dyn_err(&self) -> &'a DynErrorCodes {
        &self.well_known.dyn_error_codes
    }

    #[inline]
    pub fn get_or_create_itab(&mut self, named_type_id: u32, iface_meta_id: u32) -> u32 {
        self.itab_cache.get_or_create(
            named_type_id,
            iface_meta_id,
            self.named_type_metas,
            self.interface_metas,
        )
    }

    /// Try to get or create itab. Returns None if named type doesn't implement the interface.
    /// Use this for dynamic access where type mismatch should return error, not panic.
    #[inline]
    pub fn try_get_or_create_itab(&mut self, named_type_id: u32, iface_meta_id: u32) -> Option<u32> {
        self.itab_cache.try_get_or_create(
            named_type_id,
            iface_meta_id,
            self.named_type_metas,
            self.interface_metas,
        )
    }

    /// Get struct_meta_id from rttid using RuntimeType's embedded meta_id.
    /// O(1) lookup via RuntimeType.struct_meta_id().
    pub fn get_struct_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        self.runtime_types.get(rttid as usize)
            .and_then(|rt| rt.struct_meta_id())
    }

    /// Get interface_meta_id from rttid.
    /// Handles both direct Interface types and Named interface types.
    pub fn get_interface_meta_id_from_rttid(&self, rttid: u32) -> Option<u32> {
        use vo_common_core::runtime_type::RuntimeType;
        let rt = self.runtime_types.get(rttid as usize)?;
        match rt {
            RuntimeType::Interface { meta_id, .. } => Some(*meta_id),
            RuntimeType::Named { id, .. } => {
                // For named interface types, get meta_id from underlying_meta
                let named_meta = self.named_type_metas.get(*id as usize)?;
                if named_meta.underlying_meta.value_kind() == ValueKind::Interface {
                    Some(named_meta.underlying_meta.meta_id())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Get named_type_id from rttid.
    /// If `follow_pointer` is true, dereferences Pointer types to find the base Named type.
    /// Returns Some(named_type_id) if rttid refers to a Named type.
    pub fn get_named_type_id_from_rttid(&self, rttid: u32, follow_pointer: bool) -> Option<u32> {
        use vo_common_core::runtime_type::RuntimeType;
        let rt = self.runtime_types.get(rttid as usize)?;
        match rt {
            RuntimeType::Named { id, .. } => Some(*id),
            RuntimeType::Pointer(elem) if follow_pointer => {
                match self.runtime_types.get(elem.rttid() as usize)? {
                    RuntimeType::Named { id, .. } => Some(*id),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// Lookup a method by name on a named type.
    /// Returns (func_id, is_pointer_receiver, signature_rttid) if found.
    /// For Pointer types, dereferences to find the base named type.
    pub fn lookup_method(&self, rttid: u32, method_name: &str) -> Option<(u32, bool, u32)> {
        let named_id = self.get_named_type_id_from_rttid(rttid, true)?;
        let named_meta = self.named_type_metas.get(named_id as usize)?;
        let method_info = named_meta.methods.get(method_name)?;
        Some((method_info.func_id, method_info.is_pointer_receiver, method_info.signature_rttid))
    }

    /// Get the base call context.
    #[inline]
    pub fn call(&self) -> &ExternCall<'a> {
        &self.call
    }

    /// Get mutable access to the base call context.
    #[inline]
    pub fn call_mut(&mut self) -> &mut ExternCall<'a> {
        &mut self.call
    }

    /// Get mutable GC reference.
    #[inline]
    pub fn gc(&mut self) -> &mut Gc {
        self.gc
    }

    // ==================== Slot info (delegated) ====================

    #[inline]
    pub fn available_slots(&self) -> usize { self.call.available_slots() }
    #[inline]
    pub fn arg_count(&self) -> u16 { self.call.arg_count() }
    #[inline]
    pub fn arg_start(&self) -> u16 { self.call.arg_start }
    #[inline]
    pub fn ret_start(&self) -> u16 { self.call.ret_start }

    // ==================== Argument Reading (delegated) ====================

    #[inline]
    pub fn arg_i64(&self, n: u16) -> i64 { self.call.arg_i64(n) }
    #[inline]
    pub fn arg_u64(&self, n: u16) -> u64 { self.call.arg_u64(n) }
    #[inline]
    pub fn arg_f64(&self, n: u16) -> f64 { self.call.arg_f64(n) }
    #[inline]
    pub fn arg_bool(&self, n: u16) -> bool { self.call.arg_bool(n) }
    #[inline]
    pub fn arg_ref(&self, n: u16) -> GcRef { self.call.arg_ref(n) }

    /// Read argument as string (zero-copy borrow).
    #[inline]
    pub fn arg_str(&self, n: u16) -> &str {
        let ptr = self.call.arg_ref(n);
        if ptr.is_null() {
            ""
        } else {
            string::as_str(ptr)
        }
    }

    /// Read argument as byte slice (zero-copy borrow).
    #[inline]
    pub fn arg_bytes(&self, n: u16) -> &[u8] {
        let ptr = self.call.arg_ref(n);
        if ptr.is_null() {
            &[]
        } else {
            let data_ptr = slice::data_ptr(ptr);
            let len = slice::len(ptr);
            unsafe { core::slice::from_raw_parts(data_ptr, len) }
        }
    }

    // ==================== Return Value Writing (delegated) ====================

    #[inline]
    pub fn ret_i64(&mut self, n: u16, val: i64) { self.call.ret_i64(n, val); }
    #[inline]
    pub fn ret_u64(&mut self, n: u16, val: u64) { self.call.ret_u64(n, val); }
    #[inline]
    pub fn ret_f64(&mut self, n: u16, val: f64) { self.call.ret_f64(n, val); }
    #[inline]
    pub fn ret_bool(&mut self, n: u16, val: bool) { self.call.ret_bool(n, val); }
    #[inline]
    pub fn ret_ref(&mut self, n: u16, val: GcRef) { self.call.ret_ref(n, val); }
    #[inline]
    pub fn ret_nil(&mut self, n: u16) { self.call.ret_nil(n); }

    /// Allocate and return a new string.
    #[inline]
    pub fn ret_str(&mut self, n: u16, s: &str) {
        let ptr = string::from_rust_str(self.gc, s);
        self.call.ret_ref(n, ptr);
    }

    /// Allocate a new string.
    #[inline]
    pub fn alloc_str(&mut self, s: &str) -> GcRef {
        string::from_rust_str(self.gc, s)
    }

    /// Allocate a struct on the heap.
    #[inline]
    pub fn gc_alloc(&mut self, slots: u16, _slot_types: &[crate::SlotType]) -> GcRef {
        // Use generic struct meta for dynamic field access
        let value_meta = crate::ValueMeta::new(0, crate::ValueKind::Struct);
        self.gc.alloc(value_meta, slots)
    }

    /// Box a value into interface format (slot0, slot1).
    ///
    /// This is the canonical way to convert any value to interface representation.
    ///
    /// # Design Decision: Boxing in Runtime, Unboxing in Codegen
    ///
    /// Boxing and unboxing are intentionally asymmetric:
    /// - **Boxing (here, runtime)**: For dynamic access (`a~>field`, `a~>[k]`, `a~>Method()`),
    ///   the source type is unknown at compile time. Only runtime knows the actual field/element type.
    /// - **Unboxing (codegen)**: The target type (LHS) is always known at compile time.
    ///   Codegen generates optimal instructions (Copy/PtrGet) directly, avoiding runtime overhead.
    ///
    /// This asymmetry is dictated by the problem structure:
    /// - Boxing: "known value → any" (runtime knows source type)
    /// - Unboxing: "any → known target" (codegen knows target type)
    ///
    /// Putting unboxing in runtime would add unnecessary indirection: runtime would return
    /// `Vec<u64>` that codegen must copy to stack, whereas codegen can directly emit PtrGet.
    ///
    /// # Arguments
    /// * `rttid` - Runtime type ID
    /// * `vk` - Value kind
    /// * `raw_slots` - Raw slot values to box
    ///
    /// # Returns
    /// `(slot0, slot1)` in interface format
    ///
    /// # Boxing Rules
    /// - **Struct/Array**: Allocate GcRef, copy all slots, return `(pack_slot0(rttid, vk), GcRef)`
    /// - **Interface**: Return as-is to preserve itab_id
    /// - **Others**: Return `(pack_slot0(rttid, vk), raw_slots[0])`
    pub fn box_to_interface(&mut self, rttid: u32, vk: ValueKind, raw_slots: &[u64]) -> (u64, u64) {
        use crate::objects::interface;

        match vk {
            ValueKind::Struct | ValueKind::Array => {
                let new_ref = self.alloc_and_copy_slots(raw_slots);
                let slot0 = interface::pack_slot0(0, rttid, vk);
                (slot0, new_ref as u64)
            }
            ValueKind::Interface => {
                // Preserve itab_id: return as-is
                (raw_slots[0], raw_slots.get(1).copied().unwrap_or(0))
            }
            _ => {
                let slot0 = interface::pack_slot0(0, rttid, vk);
                (slot0, raw_slots.get(0).copied().unwrap_or(0))
            }
        }
    }

    /// Allocate a GcRef and copy raw slots into it.
    /// Used for boxing large structs/arrays to heap.
    pub fn alloc_and_copy_slots(&mut self, raw_slots: &[u64]) -> GcRef {
        let slot_count = raw_slots.len();
        let new_ref = self.gc_alloc(slot_count as u16, &[]);
        for (i, &val) in raw_slots.iter().enumerate() {
            unsafe { Gc::write_slot(new_ref, i, val) };
        }
        new_ref
    }

    /// Get return ValueRttids for all return values from a Func RuntimeType.
    pub fn get_func_results(&self, func_rttid: u32) -> Vec<ValueRttid> {
        use crate::RuntimeType;
        
        if let Some(RuntimeType::Func { results, .. }) = self.runtime_types.get(func_rttid as usize) {
            return results.clone();
        }
        Vec::new()
    }
    
    /// Check if two function signatures are compatible for dynamic call.
    /// Returns Ok(()) if compatible, Err(message) if not.
    ///
    /// # Design: LHS determines expected signature
    ///
    /// The `expected_sig_rttid` is built from LHS types at compile time.
    /// This function enforces that:
    /// - Parameter count must match exactly
    /// - Return count must match exactly (LHS count == closure return count)
    /// - Each expected param type must be assignable from actual param type
    /// - Each actual return type must be assignable to expected return type (any accepts all)
    ///
    /// If return count mismatches, this returns an error before the call happens.
    pub fn check_func_signature_compatible(
        &self,
        closure_sig_rttid: u32,
        expected_sig_rttid: u32,
    ) -> Result<(), String> {
        use crate::RuntimeType;
        
        let get_func_sig = |rttid: u32| -> Option<(&Vec<crate::ValueRttid>, &Vec<crate::ValueRttid>)> {
            match self.runtime_types.get(rttid as usize)? {
                RuntimeType::Func { params, results, .. } => Some((params, results)),
                _ => None,
            }
        };
        
        let (closure_params, closure_results) = get_func_sig(closure_sig_rttid)
            .ok_or("closure is not a function type")?;
        let (expected_params, expected_results) = get_func_sig(expected_sig_rttid)
            .ok_or("expected signature is not a function type")?;
        
        if closure_params.len() != expected_params.len() {
            return Err(format!("parameter count mismatch: expected {}, got {}", 
                expected_params.len(), closure_params.len()));
        }
        
        if closure_results.len() != expected_results.len() {
            return Err(format!("return count mismatch: expected {}, got {}", 
                expected_results.len(), closure_results.len()));
        }
        
        for (i, (expected, closure)) in expected_params.iter().zip(closure_params).enumerate() {
            if !self.value_rttids_compatible(*expected, *closure) {
                return Err(format!("parameter {} type mismatch", i + 1));
            }
        }
        
        for (i, (closure, expected)) in closure_results.iter().zip(expected_results).enumerate() {
            if !self.value_rttids_compatible(*closure, *expected) {
                return Err(format!("return {} type mismatch", i + 1));
            }
        }
        
        Ok(())
    }
    
    /// Check if source ValueRttid is compatible with target ValueRttid.
    fn value_rttids_compatible(&self, source: crate::ValueRttid, target: crate::ValueRttid) -> bool {
        use crate::RuntimeType;
        
        if source == target {
            return true;
        }
        
        // Check if target is any (empty interface)
        let target_rttid = target.rttid();
        if let Some(RuntimeType::Interface { methods, .. }) = self.runtime_types.get(target_rttid as usize) {
            if methods.is_empty() {
                return true;
            }
        }
        
        false
    }

    /// Get element ValueRttid from a Slice/Map/Chan RuntimeType.
    /// Now that RuntimeType stores ValueRttid directly, this is O(1).
    /// Returns elem ValueRttid for slice/chan, val ValueRttid for map.
    /// Panics if base_rttid is invalid - this indicates a codegen bug.
    pub fn get_elem_value_rttid_from_base(&self, base_rttid: u32) -> crate::ValueRttid {
        use crate::RuntimeType;
        
        let rt = self.runtime_types.get(base_rttid as usize)
            .expect("dyn_get_index: base_rttid not found in runtime_types");
        
        match rt {
            RuntimeType::Slice(elem_rttid) | RuntimeType::Chan { elem: elem_rttid, .. } => {
                *elem_rttid
            }
            RuntimeType::Pointer(elem_rttid) => {
                *elem_rttid
            }
            RuntimeType::Map { val, .. } => {
                *val
            }
            // String indexing returns uint8 - basic type
            RuntimeType::Basic(crate::ValueKind::String) => {
                crate::ValueRttid::new(crate::ValueKind::Uint8 as u32, crate::ValueKind::Uint8)
            }
            // Named type: recurse on underlying type
            RuntimeType::Named { id, .. } => {
                let meta = &self.named_type_metas[*id as usize];
                let underlying_rttid = meta.underlying_meta.meta_id();
                self.get_elem_value_rttid_from_base(underlying_rttid)
            }
            _ => panic!("get_elem_value_rttid_from_base: unexpected type {:?}", rt),
        }
    }

    /// Get the slot count for a type based on its rttid.
    /// Uses runtime_types to resolve the actual type and compute slot count.
    pub fn get_type_slot_count(&self, rttid: u32) -> u16 {
        use crate::RuntimeType;
        
        // Get the RuntimeType for this rttid
        let rt = self.runtime_types.get(rttid as usize)
            .expect("get_type_slot_count: rttid not found in runtime_types");
        
        match rt {
            // Named type: get underlying type info from named_type_meta
            RuntimeType::Named { id: named_id, .. } => {
                if let Some(named_meta) = self.named_type_metas.get(*named_id as usize) {
                    let underlying_vk = named_meta.underlying_meta.value_kind();
                    let underlying_meta_id = named_meta.underlying_meta.meta_id();
                    match underlying_vk {
                        crate::ValueKind::Struct => {
                            if let Some(meta) = self.struct_meta(underlying_meta_id as usize) {
                                return meta.slot_count();
                            }
                        }
                        crate::ValueKind::Interface => return 2,
                        _ => return 1,
                    }
                }
                1
            }
            // Anonymous struct: use embedded meta_id
            RuntimeType::Struct { meta_id, .. } => {
                if let Some(meta) = self.struct_meta(*meta_id as usize) {
                    return meta.slot_count();
                }
                2
            }
            // Interface is always 2 slots
            RuntimeType::Interface { .. } => 2,
            // Array: compute total slots from element slots * length
            RuntimeType::Array { len, elem } => {
                let elem_slots = self.get_type_slot_count(elem.rttid());
                elem_slots * (*len as u16)
            }
            // All other types are 1 slot (reference types)
            _ => 1,
        }
    }

    /// Allocate and return a new byte slice.
    #[inline]
    pub fn ret_bytes(&mut self, n: u16, data: &[u8]) {
        let ptr = self.alloc_bytes(data);
        self.call.ret_ref(n, ptr);
    }

    /// Allocate a new byte slice.
    #[inline]
    pub fn alloc_bytes(&mut self, data: &[u8]) -> GcRef {
        let len = data.len();
        let elem_meta = ValueMeta::new(0, ValueKind::Uint8);
        let s = slice::create(self.gc, elem_meta, 1, len, len);
        let dst = slice::data_ptr(s);
        unsafe { core::ptr::copy_nonoverlapping(data.as_ptr(), dst, len) };
        s
    }

    /// Allocate and return a new string slice ([]string).
    #[inline]
    pub fn ret_string_slice(&mut self, n: u16, strings: &[String]) {
        let ptr = self.alloc_string_slice(strings);
        self.call.ret_ref(n, ptr);
    }

    /// Allocate a new string slice ([]string).
    #[inline]
    pub fn alloc_string_slice(&mut self, strings: &[String]) -> GcRef {
        let len = strings.len();
        // String is a reference type, takes 1 slot (8 bytes)
        let elem_meta = ValueMeta::new(0, ValueKind::String);
        let s = slice::create(self.gc, elem_meta, 8, len, len);
        
        // Write each string to the slice
        for (i, rust_str) in strings.iter().enumerate() {
            let str_ref = string::from_rust_str(self.gc, rust_str);
            // String is a GcRef (8 bytes), store as u64
            slice::set(s, i, str_ref as u64, 8);
        }
        s
    }

}

// ==================== Extern Registry ====================

/// Registry for extern functions.
#[derive(Default)]
pub struct ExternRegistry {
    funcs: Vec<Option<ExternFnEntry>>,
}

enum ExternFnEntry {
    Simple(ExternFn),
    WithContext(ExternFnWithContext),
}

impl ExternRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self { funcs: Vec::new() }
    }

    /// Register a simple extern function (no GC access).
    pub fn register(&mut self, id: u32, func: ExternFn) {
        let idx = id as usize;
        if idx >= self.funcs.len() {
            self.funcs.resize_with(idx + 1, || None);
        }
        self.funcs[idx] = Some(ExternFnEntry::Simple(func));
    }

    /// Register an extern function with full context.
    pub fn register_with_context(&mut self, id: u32, func: ExternFnWithContext) {
        let idx = id as usize;
        if idx >= self.funcs.len() {
            self.funcs.resize_with(idx + 1, || None);
        }
        self.funcs[idx] = Some(ExternFnEntry::WithContext(func));
    }

    /// Call an extern function.
    pub fn call(
        &self,
        id: u32,
        stack: &mut [u64],
        bp: usize,
        arg_start: u16,
        arg_count: u16,
        ret_start: u16,
        gc: &mut Gc,
        struct_metas: &[StructMeta],
        interface_metas: &[InterfaceMeta],
        named_type_metas: &[NamedTypeMeta],
        runtime_types: &[RuntimeType],
        well_known: &WellKnownTypes,
        itab_cache: &mut ItabCache,
    ) -> ExternResult {
        match self.funcs.get(id as usize) {
            Some(Some(ExternFnEntry::Simple(f))) => {
                let mut call = ExternCall::new(stack, bp, arg_start, arg_count, ret_start);
                f(&mut call)
            }
            Some(Some(ExternFnEntry::WithContext(f))) => {
                let mut call = ExternCallContext::new(
                    stack,
                    bp,
                    arg_start,
                    arg_count,
                    ret_start,
                    gc,
                    struct_metas,
                    named_type_metas,
                    interface_metas,
                    runtime_types,
                    well_known,
                    itab_cache,
                );
                f(&mut call)
            }
            _ => ExternResult::Panic(format!("extern function {} not found", id)),
        }
    }

    /// Check if a function is registered.
    pub fn has(&self, id: u32) -> bool {
        matches!(self.funcs.get(id as usize), Some(Some(_)))
    }

    /// Get the number of registered functions.
    pub fn len(&self) -> usize {
        self.funcs.iter().filter(|f| f.is_some()).count()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
