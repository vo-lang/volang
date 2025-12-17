//! Runtime symbol table for JIT compilation.
//!
//! Provides function pointers that can be registered with a JIT compiler.

use gox_runtime_core::ffi;

/// A runtime symbol with its name and function pointer.
#[derive(Clone, Copy)]
pub struct RuntimeSymbol {
    pub name: &'static str,
    pub ptr: *const u8,
}

/// Collection of all runtime symbols for JIT registration.
pub struct RuntimeSymbols {
    symbols: Vec<RuntimeSymbol>,
}

impl RuntimeSymbols {
    /// Create a new symbol table with all runtime functions.
    pub fn new() -> Self {
        let symbols = vec![
            // GC functions
            RuntimeSymbol { name: "gox_gc_alloc", ptr: ffi::gox_gc_alloc as *const u8 },
            RuntimeSymbol { name: "gox_gc_read_slot", ptr: ffi::gox_gc_read_slot as *const u8 },
            RuntimeSymbol { name: "gox_gc_write_slot", ptr: ffi::gox_gc_write_slot as *const u8 },
            RuntimeSymbol { name: "gox_gc_write_barrier", ptr: ffi::gox_gc_write_barrier as *const u8 },
            RuntimeSymbol { name: "gox_gc_mark_gray", ptr: ffi::gox_gc_mark_gray as *const u8 },
            
            // String functions
            RuntimeSymbol { name: "gox_string_len", ptr: ffi::gox_string_len as *const u8 },
            RuntimeSymbol { name: "gox_string_index", ptr: ffi::gox_string_index as *const u8 },
            RuntimeSymbol { name: "gox_string_concat", ptr: ffi::gox_string_concat as *const u8 },
            RuntimeSymbol { name: "gox_string_eq", ptr: ffi::gox_string_eq as *const u8 },
            RuntimeSymbol { name: "gox_string_ne", ptr: ffi::gox_string_ne as *const u8 },
            
            // Array functions
            RuntimeSymbol { name: "gox_array_create", ptr: ffi::gox_array_create as *const u8 },
            RuntimeSymbol { name: "gox_array_len", ptr: ffi::gox_array_len as *const u8 },
            RuntimeSymbol { name: "gox_array_get", ptr: ffi::gox_array_get as *const u8 },
            RuntimeSymbol { name: "gox_array_set", ptr: ffi::gox_array_set as *const u8 },
            
            // Slice functions
            RuntimeSymbol { name: "gox_slice_create", ptr: ffi::gox_slice_create as *const u8 },
            RuntimeSymbol { name: "gox_slice_len", ptr: ffi::gox_slice_len as *const u8 },
            RuntimeSymbol { name: "gox_slice_cap", ptr: ffi::gox_slice_cap as *const u8 },
            RuntimeSymbol { name: "gox_slice_get", ptr: ffi::gox_slice_get as *const u8 },
            RuntimeSymbol { name: "gox_slice_set", ptr: ffi::gox_slice_set as *const u8 },
            RuntimeSymbol { name: "gox_slice_append", ptr: ffi::gox_slice_append as *const u8 },
            RuntimeSymbol { name: "gox_slice_slice", ptr: ffi::gox_slice_slice as *const u8 },
            
            // Closure functions
            RuntimeSymbol { name: "gox_closure_create", ptr: ffi::gox_closure_create as *const u8 },
            RuntimeSymbol { name: "gox_closure_func_id", ptr: ffi::gox_closure_func_id as *const u8 },
            RuntimeSymbol { name: "gox_closure_upvalue_count", ptr: ffi::gox_closure_upvalue_count as *const u8 },
            RuntimeSymbol { name: "gox_closure_get_upvalue", ptr: ffi::gox_closure_get_upvalue as *const u8 },
            RuntimeSymbol { name: "gox_closure_set_upvalue", ptr: ffi::gox_closure_set_upvalue as *const u8 },
            
            // Interface functions
            RuntimeSymbol { name: "gox_interface_unbox_type", ptr: ffi::gox_interface_unbox_type as *const u8 },
            RuntimeSymbol { name: "gox_interface_unbox_data", ptr: ffi::gox_interface_unbox_data as *const u8 },
            RuntimeSymbol { name: "gox_interface_is_nil", ptr: ffi::gox_interface_is_nil as *const u8 },
        ];
        
        Self { symbols }
    }
    
    /// Iterate over all symbols.
    pub fn iter(&self) -> impl Iterator<Item = &RuntimeSymbol> {
        self.symbols.iter()
    }
    
    /// Get a symbol by name.
    pub fn get(&self, name: &str) -> Option<&RuntimeSymbol> {
        self.symbols.iter().find(|s| s.name == name)
    }
    
    /// Get the number of symbols.
    pub fn len(&self) -> usize {
        self.symbols.len()
    }
    
    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }
}

impl Default for RuntimeSymbols {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_symbols_not_empty() {
        let symbols = RuntimeSymbols::new();
        assert!(!symbols.is_empty());
        assert!(symbols.len() > 20);
    }
    
    #[test]
    fn test_get_symbol() {
        let symbols = RuntimeSymbols::new();
        let gc_alloc = symbols.get("gox_gc_alloc");
        assert!(gc_alloc.is_some());
        assert!(!gc_alloc.unwrap().ptr.is_null());
    }
}
