//! Runtime symbol table for JIT compilation.
//!
//! Provides function pointers that can be registered with a JIT compiler.

use vo_runtime_core::ffi;

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
            // GC functions (all from gc_global)
            RuntimeSymbol { name: "vo_rt_alloc", ptr: crate::gc_global::vo_rt_alloc as *const u8 },
            RuntimeSymbol { name: "vo_gc_read_slot", ptr: crate::gc_global::vo_gc_read_slot as *const u8 },
            RuntimeSymbol { name: "vo_gc_write_slot", ptr: crate::gc_global::vo_gc_write_slot as *const u8 },
            RuntimeSymbol { name: "vo_rt_write_barrier", ptr: crate::gc_global::vo_rt_write_barrier as *const u8 },
            RuntimeSymbol { name: "vo_rt_mark_gray", ptr: crate::gc_global::vo_rt_mark_gray as *const u8 },
            
            // Global variable functions
            RuntimeSymbol { name: "vo_rt_get_global", ptr: crate::gc_global::vo_rt_get_global as *const u8 },
            RuntimeSymbol { name: "vo_rt_set_global", ptr: crate::gc_global::vo_rt_set_global as *const u8 },
            
            // String functions (some using global GC wrappers)
            RuntimeSymbol { name: "vo_string_len", ptr: ffi::vo_string_len as *const u8 },
            RuntimeSymbol { name: "vo_string_index", ptr: ffi::vo_string_index as *const u8 },
            RuntimeSymbol { name: "vo_rt_string_concat", ptr: crate::gc_global::vo_rt_string_concat as *const u8 },
            RuntimeSymbol { name: "vo_string_eq", ptr: ffi::vo_string_eq as *const u8 },
            RuntimeSymbol { name: "vo_string_ne", ptr: ffi::vo_string_ne as *const u8 },
            RuntimeSymbol { name: "vo_string_lt", ptr: ffi::vo_string_lt as *const u8 },
            RuntimeSymbol { name: "vo_string_le", ptr: ffi::vo_string_le as *const u8 },
            RuntimeSymbol { name: "vo_string_gt", ptr: ffi::vo_string_gt as *const u8 },
            RuntimeSymbol { name: "vo_string_ge", ptr: ffi::vo_string_ge as *const u8 },
            RuntimeSymbol { name: "vo_rt_string_from_ptr", ptr: crate::gc_global::vo_rt_string_from_ptr as *const u8 },
            RuntimeSymbol { name: "vo_rt_string_slice", ptr: crate::gc_global::vo_rt_string_slice as *const u8 },
            
            // Array functions (using global GC wrappers)
            RuntimeSymbol { name: "vo_rt_array_create", ptr: crate::gc_global::vo_rt_array_create as *const u8 },
            RuntimeSymbol { name: "vo_array_len", ptr: ffi::vo_array_len as *const u8 },
            RuntimeSymbol { name: "vo_array_get", ptr: ffi::vo_array_get as *const u8 },
            RuntimeSymbol { name: "vo_array_set", ptr: ffi::vo_array_set as *const u8 },
            
            // Slice functions (using global GC wrappers where needed)
            RuntimeSymbol { name: "vo_rt_slice_create", ptr: crate::gc_global::vo_rt_slice_create as *const u8 },
            RuntimeSymbol { name: "vo_slice_len", ptr: ffi::vo_slice_len as *const u8 },
            RuntimeSymbol { name: "vo_slice_cap", ptr: ffi::vo_slice_cap as *const u8 },
            RuntimeSymbol { name: "vo_slice_get", ptr: ffi::vo_slice_get as *const u8 },
            RuntimeSymbol { name: "vo_slice_set", ptr: ffi::vo_slice_set as *const u8 },
            RuntimeSymbol { name: "vo_rt_slice_append", ptr: crate::gc_global::vo_rt_slice_append as *const u8 },
            RuntimeSymbol { name: "vo_rt_slice_slice", ptr: crate::gc_global::vo_rt_slice_slice as *const u8 },
            
            // Closure functions (some using global GC wrappers)
            RuntimeSymbol { name: "vo_rt_closure_create", ptr: crate::gc_global::vo_rt_closure_create as *const u8 },
            RuntimeSymbol { name: "vo_closure_func_id", ptr: ffi::vo_closure_func_id as *const u8 },
            RuntimeSymbol { name: "vo_closure_upvalue_count", ptr: ffi::vo_closure_upvalue_count as *const u8 },
            RuntimeSymbol { name: "vo_closure_get_upvalue", ptr: ffi::vo_closure_get_upvalue as *const u8 },
            RuntimeSymbol { name: "vo_closure_set_upvalue", ptr: ffi::vo_closure_set_upvalue as *const u8 },
            RuntimeSymbol { name: "vo_rt_upval_box_create", ptr: crate::gc_global::vo_rt_upval_box_create as *const u8 },
            RuntimeSymbol { name: "vo_upval_box_get", ptr: ffi::vo_upval_box_get as *const u8 },
            RuntimeSymbol { name: "vo_upval_box_set", ptr: ffi::vo_upval_box_set as *const u8 },
            
            // Interface functions
            RuntimeSymbol { name: "vo_interface_unbox_value_kind", ptr: ffi::vo_interface_unbox_value_kind as *const u8 },
            RuntimeSymbol { name: "vo_interface_unbox_value_type_id", ptr: ffi::vo_interface_unbox_value_type_id as *const u8 },
            RuntimeSymbol { name: "vo_interface_unbox_data", ptr: ffi::vo_interface_unbox_data as *const u8 },
            RuntimeSymbol { name: "vo_interface_is_nil", ptr: ffi::vo_interface_is_nil as *const u8 },
            
            // Function table pointer (for indirect closure calls)
            RuntimeSymbol { name: "vo_func_table_ptr", ptr: crate::gc_global::vo_func_table_ptr as *const u8 },
            
            // Extern dispatch (for CallExtern opcode)
            RuntimeSymbol { name: "vo_extern_call", ptr: crate::extern_dispatch::vo_extern_call as *const u8 },
            
            // Goroutine functions (from vo-runtime-native, not core)
            RuntimeSymbol { name: "vo_go_spawn", ptr: crate::goroutine::vo_go_spawn as *const u8 },
            RuntimeSymbol { name: "vo_yield", ptr: crate::goroutine::vo_yield as *const u8 },
            RuntimeSymbol { name: "vo_chan_new", ptr: crate::goroutine::vo_chan_new as *const u8 },
            RuntimeSymbol { name: "vo_chan_send", ptr: crate::goroutine::vo_chan_send as *const u8 },
            RuntimeSymbol { name: "vo_chan_recv", ptr: crate::goroutine::vo_chan_recv as *const u8 },
            RuntimeSymbol { name: "vo_chan_close", ptr: crate::goroutine::vo_chan_close as *const u8 },
            
            // Defer/Panic/Recover functions
            RuntimeSymbol { name: "vo_defer_push", ptr: crate::goroutine::vo_defer_push as *const u8 },
            RuntimeSymbol { name: "vo_defer_pop", ptr: crate::goroutine::vo_defer_pop as *const u8 },
            RuntimeSymbol { name: "vo_panic", ptr: crate::goroutine::vo_panic as *const u8 },
            RuntimeSymbol { name: "vo_recover", ptr: crate::goroutine::vo_recover as *const u8 },
            
            // Select functions
            RuntimeSymbol { name: "vo_select_start", ptr: crate::goroutine::vo_select_start as *const u8 },
            RuntimeSymbol { name: "vo_select_add_send", ptr: crate::goroutine::vo_select_add_send as *const u8 },
            RuntimeSymbol { name: "vo_select_add_recv", ptr: crate::goroutine::vo_select_add_recv as *const u8 },
            RuntimeSymbol { name: "vo_select_exec", ptr: crate::goroutine::vo_select_exec as *const u8 },
            
            // Iterator functions
            RuntimeSymbol { name: "vo_iter_begin", ptr: crate::goroutine::vo_iter_begin as *const u8 },
            RuntimeSymbol { name: "vo_iter_next", ptr: crate::goroutine::vo_iter_next as *const u8 },
            RuntimeSymbol { name: "vo_iter_end", ptr: crate::goroutine::vo_iter_end as *const u8 },
            
            // Debug/Assert functions
            RuntimeSymbol { name: "vo_debug_print", ptr: crate::debug::vo_debug_print as *const u8 },
            RuntimeSymbol { name: "vo_assert_begin", ptr: crate::debug::vo_assert_begin as *const u8 },
            RuntimeSymbol { name: "vo_assert_arg", ptr: crate::debug::vo_assert_arg as *const u8 },
            RuntimeSymbol { name: "vo_assert_end", ptr: crate::debug::vo_assert_end as *const u8 },
            
            // Note: Stdlib extern functions (math.Sqrt, strings.Index, etc.) are NOT registered here.
            // Both JIT and AOT use CallExtern opcode -> vo_extern_call -> extern_dispatch.
            // See extern_fns/mod.rs register_all() for the actual stdlib function registration.
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
        let gc_alloc = symbols.get("vo_rt_alloc");
        assert!(gc_alloc.is_some());
        assert!(!gc_alloc.unwrap().ptr.is_null());
    }
}
