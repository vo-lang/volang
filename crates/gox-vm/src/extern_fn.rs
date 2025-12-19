//! Zero-copy extern function interface.
//!
//! This module provides a high-performance FFI for extern functions that:
//! - Directly accesses VM registers (no Vec allocation)
//! - Supports zero-copy string borrowing
//! - Converts types only when needed
//!
//! "Extern" means any function called from GoX to outside (native runtime).
//!
//! # Example
//!
//! ```ignore
//! fn extern_println(ctx: &mut ExternCtx) -> ExternResult {
//!     let output = ctx.format_all();
//!     println!("{}", output);
//!     ctx.ret_i64(0, output.len() as i64);
//!     ExternResult::Ok(1)
//! }
//! ```

use alloc::string::{String, ToString};
use hashbrown::HashMap;

use crate::gc::{Gc, GcRef};
use crate::objects::string;
use crate::types::TypeId;
pub use gox_common_core::ValueKind;

/// Extern function signature - direct register access.
pub type ExternFn = fn(ctx: &mut ExternCtx) -> ExternResult;

/// Extern function execution result.
#[derive(Debug)]
pub enum ExternResult {
    /// Success with number of return values written.
    Ok(u8),
    /// Panic with error message.
    Panic(String),
}

/// Extern function context - provides type-safe register access.
///
/// Arguments are laid out as pairs: [type0, val0, type1, val1, ...]
/// Return values are written starting at ret_base.
pub struct ExternCtx<'a> {
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

impl<'a> ExternCtx<'a> {
    /// Create a new extern context.
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
    pub fn arg_type(&self, idx: usize) -> ValueKind {
        debug_assert!(idx < self.arg_count, "arg index out of bounds");
        ValueKind::from_u8(self.regs[self.arg_base + idx * 2] as u8)
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
        let str_ref = string::from_rust_str(self.gc, ValueKind::String as TypeId, s);
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
        string::from_rust_str(self.gc, ValueKind::String as TypeId, s)
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

    /// Return a string slice ([]string).
    /// Allocates an array of strings and wraps it in a slice.
    pub fn ret_string_slice(&mut self, idx: usize, strings: &[String]) {
        use crate::objects::{array, slice};
        
        let len = strings.len();
        if len == 0 {
            self.ret_nil(idx);
            return;
        }
        
        // Create array to hold string pointers (8 bytes per pointer)
        let arr = array::create(self.gc, ValueKind::Array as TypeId, ValueKind::String as TypeId, 8, len);
        
        // Allocate each string and store in array
        for (i, s) in strings.iter().enumerate() {
            let str_ref = string::from_rust_str(self.gc, ValueKind::String as TypeId, s);
            array::set(arr, i, str_ref as u64);
        }
        
        // Create slice wrapping the array
        let sl = slice::create(self.gc, ValueKind::Slice as TypeId, arr, 0, len, len);
        self.ret_raw(idx, sl as u64);
    }

    // ==================== Slice Operations ====================

    /// Get slice length.
    pub fn slice_len(&self, slice_ref: GcRef) -> usize {
        use crate::objects::slice;
        slice::len(slice_ref)
    }

    /// Get i64 element from slice.
    pub fn slice_get_i64(&self, slice_ref: GcRef, idx: usize) -> i64 {
        use crate::objects::slice;
        slice::get(slice_ref, idx) as i64
    }

    /// Set i64 element in slice.
    pub fn slice_set_i64(&mut self, slice_ref: GcRef, idx: usize, val: i64) {
        use crate::objects::slice;
        slice::set(slice_ref, idx, val as u64);
    }

    /// Get f64 element from slice.
    pub fn slice_get_f64(&self, slice_ref: GcRef, idx: usize) -> f64 {
        use crate::objects::slice;
        f64::from_bits(slice::get(slice_ref, idx))
    }

    /// Set f64 element in slice.
    pub fn slice_set_f64(&mut self, slice_ref: GcRef, idx: usize, val: f64) {
        use crate::objects::slice;
        slice::set(slice_ref, idx, val.to_bits());
    }

    /// Get byte slice as Vec<u8> from a []byte argument.
    /// Each byte is stored as a u64 slot in GoX.
    pub fn arg_bytes(&self, idx: usize) -> Vec<u8> {
        use crate::objects::slice;
        let slice_ref = self.arg_ref(idx);
        if slice_ref.is_null() {
            return Vec::new();
        }
        let len = slice::len(slice_ref);
        if len == 0 {
            return Vec::new();
        }
        // Each element is stored as u64, extract as bytes
        let mut result = Vec::with_capacity(len);
        for i in 0..len {
            result.push(slice::get(slice_ref, i) as u8);
        }
        result
    }

    /// Return a byte slice ([]byte).
    /// Each byte is stored as a u64 slot in GoX.
    pub fn ret_byte_slice(&mut self, idx: usize, bytes: &[u8]) {
        use crate::objects::{array, slice};
        
        let len = bytes.len();
        if len == 0 {
            self.ret_nil(idx);
            return;
        }
        
        // Create packed byte array (1 byte per element)
        let arr = array::create(self.gc, ValueKind::Array as TypeId, ValueKind::Uint8 as TypeId, 1, len);
        
        // Direct memory copy for packed bytes
        unsafe {
            let dest = array::as_bytes_mut(arr);
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), dest, len);
        }
        
        let sl = slice::create(self.gc, ValueKind::Slice as TypeId, arr, 0, len, len);
        self.ret_raw(idx, sl as u64);
    }

    // ==================== High-Level API (On-Demand Conversion) ====================

    /// Format a single argument as string.
    pub fn format_arg(&self, idx: usize) -> String {
        match self.arg_type(idx) {
            ValueKind::Nil => "nil".into(),
            ValueKind::Bool => if self.arg_bool(idx) { "true" } else { "false" }.into(),
            ValueKind::Int
            | ValueKind::Int8
            | ValueKind::Int16
            | ValueKind::Int32
            | ValueKind::Int64
            | ValueKind::Uint
            | ValueKind::Uint8
            | ValueKind::Uint16
            | ValueKind::Uint32
            | ValueKind::Uint64 => self.arg_i64(idx).to_string(),
            ValueKind::Float32 => (f32::from_bits(self.arg_raw(idx) as u32)).to_string(),
            ValueKind::Float64 => self.arg_f64(idx).to_string(),
            ValueKind::String => self.arg_str(idx).to_string(),
            ValueKind::Slice => "[...]".into(),
            ValueKind::Array => "[...]".into(),
            ValueKind::Map => "map[...]".into(),
            ValueKind::Struct => "{...}".into(),
            ValueKind::Pointer => "*struct{...}".into(),
            ValueKind::Interface => "<interface>".into(),
            ValueKind::Channel => "<chan>".into(),
            ValueKind::Closure => "<closure>".into(),
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
        self.arg_type(idx) == ValueKind::Nil || self.arg_raw(idx) == 0
    }
}

/// Extern function registry.
#[derive(Default)]
pub struct ExternRegistry {
    funcs: HashMap<String, ExternFn>,
}

impl ExternRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            funcs: HashMap::new(),
        }
    }

    /// Register an extern function.
    pub fn register(&mut self, name: &str, func: ExternFn) {
        self.funcs.insert(name.to_string(), func);
    }

    /// Get an extern function by name.
    pub fn get(&self, name: &str) -> Option<ExternFn> {
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
        assert_eq!(ValueKind::from_u8(0), ValueKind::Nil);
        assert_eq!(ValueKind::from_u8(1), ValueKind::Bool);
        assert_eq!(ValueKind::from_u8(14), ValueKind::String);
    }

    #[test]
    fn test_registry() {
        fn dummy(_ctx: &mut ExternCtx) -> ExternResult {
            ExternResult::Ok(0)
        }

        let mut registry = ExternRegistry::new();
        registry.register("test.Dummy", dummy);

        assert!(registry.get("test.Dummy").is_some());
        assert!(registry.get("nonexistent").is_none());
        assert_eq!(registry.len(), 1);
    }
}
