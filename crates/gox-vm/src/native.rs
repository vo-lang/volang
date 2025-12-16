//! Zero-copy native function interface.
//!
//! This module provides a high-performance FFI for native functions that:
//! - Directly accesses VM registers (no Vec allocation)
//! - Supports zero-copy string borrowing
//! - Converts types only when needed
//!
//! # Example
//!
//! ```ignore
//! fn native_println(ctx: &mut NativeCtx) -> NativeResult {
//!     let output = ctx.format_all();
//!     println!("{}", output);
//!     ctx.ret_i64(0, output.len() as i64);
//!     NativeResult::Ok(1)
//! }
//! ```

use std::collections::HashMap;

use crate::gc::{Gc, GcRef};
use crate::objects::string;
use crate::types::builtin;

// Re-export TypeTag for native function implementations
pub use gox_common::ValueKind as TypeTag;

/// Native function signature - direct register access.
pub type NativeFn = fn(ctx: &mut NativeCtx) -> NativeResult;

/// Native function execution result.
#[derive(Debug)]
pub enum NativeResult {
    /// Success with number of return values written.
    Ok(u8),
    /// Panic with error message.
    Panic(String),
}

/// Native function context - provides type-safe register access.
///
/// Arguments are laid out as pairs: [type0, val0, type1, val1, ...]
/// Return values are written starting at ret_base.
pub struct NativeCtx<'a> {
    /// GC for allocations
    gc: &'a mut Gc,
    /// Register file (borrowed from fiber)
    regs: &'a mut [u64],
    /// Argument base index in regs
    arg_base: usize,
    /// Number of arguments
    arg_count: usize,
    /// Return value base index in regs
    ret_base: usize,
}

impl<'a> NativeCtx<'a> {
    /// Create a new native context.
    #[inline]
    pub fn new(
        gc: &'a mut Gc,
        regs: &'a mut [u64],
        arg_base: usize,
        arg_count: usize,
        ret_base: usize,
    ) -> Self {
        Self {
            gc,
            regs,
            arg_base,
            arg_count,
            ret_base,
        }
    }

    // ==================== Argument Reading (Zero-Copy) ====================

    /// Get the number of arguments.
    #[inline]
    pub fn argc(&self) -> usize {
        self.arg_count
    }

    /// Get the type tag of an argument.
    #[inline]
    pub fn arg_type(&self, idx: usize) -> TypeTag {
        debug_assert!(idx < self.arg_count, "arg index out of bounds");
        TypeTag::from_u8(self.regs[self.arg_base + idx * 2] as u8)
    }

    /// Read raw u64 value (fastest, use when type is known).
    #[inline]
    pub fn arg_raw(&self, idx: usize) -> u64 {
        debug_assert!(idx < self.arg_count, "arg index out of bounds");
        self.regs[self.arg_base + idx * 2 + 1]
    }

    /// Read as i64.
    #[inline]
    pub fn arg_i64(&self, idx: usize) -> i64 {
        self.arg_raw(idx) as i64
    }

    /// Read as f64.
    #[inline]
    pub fn arg_f64(&self, idx: usize) -> f64 {
        f64::from_bits(self.arg_raw(idx))
    }

    /// Read as bool.
    #[inline]
    pub fn arg_bool(&self, idx: usize) -> bool {
        self.arg_raw(idx) != 0
    }

    /// Read as string (zero-copy borrow!).
    #[inline]
    pub fn arg_str(&self, idx: usize) -> &str {
        let ptr = self.arg_raw(idx) as GcRef;
        if ptr.is_null() {
            ""
        } else {
            string::as_str(ptr)
        }
    }

    /// Read as GcRef (for slice, map, struct, etc.).
    #[inline]
    pub fn arg_ref(&self, idx: usize) -> GcRef {
        self.arg_raw(idx) as GcRef
    }

    // ==================== Return Value Writing (Zero-Copy) ====================

    /// Write raw u64 to return slot.
    #[inline]
    pub fn ret_raw(&mut self, idx: usize, val: u64) {
        self.regs[self.ret_base + idx] = val;
    }

    /// Return i64.
    #[inline]
    pub fn ret_i64(&mut self, idx: usize, val: i64) {
        self.ret_raw(idx, val as u64);
    }

    /// Return f64.
    #[inline]
    pub fn ret_f64(&mut self, idx: usize, val: f64) {
        self.ret_raw(idx, val.to_bits());
    }

    /// Return bool.
    #[inline]
    pub fn ret_bool(&mut self, idx: usize, val: bool) {
        self.ret_raw(idx, if val { 1 } else { 0 });
    }

    /// Return a new string (requires GC allocation).
    #[inline]
    pub fn ret_string(&mut self, idx: usize, s: &str) {
        let str_ref = string::from_rust_str(self.gc, builtin::STRING, s);
        self.ret_raw(idx, str_ref as u64);
    }

    /// Return GcRef.
    #[inline]
    pub fn ret_ref(&mut self, idx: usize, ptr: GcRef) {
        self.ret_raw(idx, ptr as u64);
    }

    /// Return nil.
    #[inline]
    pub fn ret_nil(&mut self, idx: usize) {
        self.ret_raw(idx, 0);
    }

    // ==================== GC Operations ====================

    /// Get mutable reference to GC.
    #[inline]
    pub fn gc(&mut self) -> &mut Gc {
        self.gc
    }

    /// Allocate a new string.
    #[inline]
    pub fn new_string(&mut self, s: &str) -> GcRef {
        string::from_rust_str(self.gc, builtin::STRING, s)
    }

    /// Get string content from GcRef.
    #[inline]
    pub fn get_string(&self, ptr: GcRef) -> &str {
        if ptr.is_null() {
            ""
        } else {
            string::as_str(ptr)
        }
    }

    // ==================== High-Level API (On-Demand Conversion) ====================

    /// Format a single argument as string.
    pub fn format_arg(&self, idx: usize) -> String {
        match self.arg_type(idx) {
            TypeTag::Nil => "nil".into(),
            TypeTag::Bool => {
                if self.arg_bool(idx) {
                    "true"
                } else {
                    "false"
                }
                .into()
            }
            TypeTag::Int
            | TypeTag::Int8
            | TypeTag::Int16
            | TypeTag::Int32
            | TypeTag::Int64
            | TypeTag::Uint
            | TypeTag::Uint8
            | TypeTag::Uint16
            | TypeTag::Uint32
            | TypeTag::Uint64 => self.arg_i64(idx).to_string(),
            TypeTag::Float32 => (f32::from_bits(self.arg_raw(idx) as u32)).to_string(),
            TypeTag::Float64 => self.arg_f64(idx).to_string(),
            TypeTag::String => self.arg_str(idx).to_string(),
            TypeTag::Slice => "[...]".into(),
            TypeTag::Array => "[...]".into(),
            TypeTag::Map => "map[...]".into(),
            TypeTag::Struct => "{...}".into(),
            TypeTag::Obx => "object{...}".into(),
            TypeTag::Interface => "<interface>".into(),
            TypeTag::Channel => "<chan>".into(),
            TypeTag::Closure => "<closure>".into(),
        }
    }

    /// Format all arguments with spaces between them.
    pub fn format_all(&self) -> String {
        if self.arg_count == 0 {
            return String::new();
        }
        
        let mut output = self.format_arg(0);
        for i in 1..self.arg_count {
            output.push(' ');
            output.push_str(&self.format_arg(i));
        }
        output
    }

    /// Check if argument at idx is nil.
    #[inline]
    pub fn is_nil(&self, idx: usize) -> bool {
        self.arg_type(idx) == TypeTag::Nil || self.arg_raw(idx) == 0
    }
}

/// Native function registry.
#[derive(Default)]
pub struct NativeRegistry {
    funcs: HashMap<String, NativeFn>,
}

impl NativeRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
        }
    }

    /// Register a native function.
    pub fn register(&mut self, name: &str, func: NativeFn) {
        self.funcs.insert(name.to_string(), func);
    }

    /// Get a native function by name.
    pub fn get(&self, name: &str) -> Option<NativeFn> {
        self.funcs.get(name).copied()
    }

    /// Get number of registered functions.
    pub fn len(&self) -> usize {
        self.funcs.len()
    }

    /// Check if registry is empty.
    pub fn is_empty(&self) -> bool {
        self.funcs.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_tag_conversion() {
        assert_eq!(TypeTag::from_u8(0), TypeTag::Nil);
        assert_eq!(TypeTag::from_u8(1), TypeTag::Bool);
        assert_eq!(TypeTag::from_u8(14), TypeTag::String);
    }

    #[test]
    fn test_registry() {
        fn dummy(_ctx: &mut NativeCtx) -> NativeResult {
            NativeResult::Ok(0)
        }

        let mut registry = NativeRegistry::new();
        registry.register("test.Dummy", dummy);
        
        assert!(registry.get("test.Dummy").is_some());
        assert!(registry.get("nonexistent").is_none());
        assert_eq!(registry.len(), 1);
    }
}

