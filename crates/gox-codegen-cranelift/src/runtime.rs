//! Runtime function declarations for Cranelift code generation.
//!
//! This module provides:
//! - RuntimeFunc enum for all runtime functions
//! - Cranelift signatures for each function
//! - RuntimeFuncs registry for declaration management

use cranelift_codegen::ir::types::{I64, I8, I32};
use cranelift_codegen::ir::{AbiParam, Signature};
use cranelift_codegen::isa::CallConv;
use cranelift_module::{FuncId, Linkage, Module};
use anyhow::Result;
use std::collections::HashMap;

/// Runtime function identifiers.
/// These correspond to extern "C" functions in gox-runtime-core/ffi.rs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeFunc {
    // === GC (5) ===
    GcAlloc,       // gox_rt_alloc (uses global GC)
    GcReadSlot,
    GcWriteSlot,
    GcWriteBarrier,
    GcMarkGray,

    // === Globals (2) ===
    GetGlobal,     // gox_rt_get_global
    SetGlobal,     // gox_rt_set_global

    // === String (6) ===
    StringLen,
    StringIndex,
    StringConcat,
    StringEq,
    StringNe,
    StringFromPtr,

    // === Array (4) ===
    ArrayCreate,
    ArrayLen,
    ArrayGet,
    ArraySet,

    // === Slice (7) ===
    SliceCreate,
    SliceLen,
    SliceCap,
    SliceGet,
    SliceSet,
    SliceAppend,
    SliceSlice,

    // === Struct (1) ===
    StructHash,

    // === Map (6) ===
    MapCreate,
    MapLen,
    MapGet,
    MapSet,
    MapDelete,
    MapContains,

    // === Closure (8) ===
    ClosureCreate,
    ClosureFuncId,
    ClosureUpvalueCount,
    ClosureGetUpvalue,
    ClosureSetUpvalue,
    UpvalBoxCreate,
    UpvalBoxGet,
    UpvalBoxSet,

    // === Interface (3) ===
    InterfaceUnboxType,
    InterfaceUnboxData,
    InterfaceIsNil,

    // === Builtin (2) ===
    BuiltinLen,
    BuiltinCap,

    // === Strings stdlib (5) ===
    StringsIndex,
    StringsCount,
    StringsToLower,
    StringsToUpper,
    StringsEqualFold,

    // === Fmt (2) ===
    FmtFormatValue,
    FmtPrintln,

    // === Goroutine (5) ===
    GoSpawn,
    GoYield,
    ChanSend,
    ChanRecv,
    ChanClose,

    // === Defer/Panic/Recover (4) ===
    DeferPush,
    DeferPop,
    Panic,
    Recover,

    // === Select (4) ===
    SelectStart,
    SelectAddSend,
    SelectAddRecv,
    SelectExec,

    // === Iterator (3) ===
    IterBegin,
    IterNext,
    IterEnd,

    // === Debug/Assert (4) ===
    DebugPrint,
    AssertBegin,
    AssertArg,
    AssertEnd,

    // === Function Table (1) ===
    FuncTablePtr,

    // === Extern Dispatch (1) ===
    ExternCall,
}

impl RuntimeFunc {
    /// Get the symbol name for this runtime function.
    pub fn symbol_name(&self) -> &'static str {
        match self {
            // GC
            RuntimeFunc::GcAlloc => "gox_rt_alloc",  // Uses global GC
            RuntimeFunc::GcReadSlot => "gox_gc_read_slot",
            RuntimeFunc::GcWriteSlot => "gox_gc_write_slot",
            RuntimeFunc::GcWriteBarrier => "gox_gc_write_barrier",
            RuntimeFunc::GcMarkGray => "gox_gc_mark_gray",
            // Globals
            RuntimeFunc::GetGlobal => "gox_rt_get_global",
            RuntimeFunc::SetGlobal => "gox_rt_set_global",
            // String
            RuntimeFunc::StringLen => "gox_string_len",
            RuntimeFunc::StringIndex => "gox_string_index",
            RuntimeFunc::StringConcat => "gox_rt_string_concat",  // Uses global GC
            RuntimeFunc::StringEq => "gox_string_eq",
            RuntimeFunc::StringNe => "gox_string_ne",
            RuntimeFunc::StringFromPtr => "gox_rt_string_from_ptr",  // Uses global GC
            // Array
            RuntimeFunc::ArrayCreate => "gox_rt_array_create",  // Uses global GC
            RuntimeFunc::ArrayLen => "gox_array_len",
            RuntimeFunc::ArrayGet => "gox_array_get",
            RuntimeFunc::ArraySet => "gox_array_set",
            // Slice
            RuntimeFunc::SliceCreate => "gox_rt_slice_create",  // Uses global GC
            RuntimeFunc::SliceLen => "gox_slice_len",
            RuntimeFunc::SliceCap => "gox_slice_cap",
            RuntimeFunc::SliceGet => "gox_slice_get",
            RuntimeFunc::SliceSet => "gox_slice_set",
            RuntimeFunc::SliceAppend => "gox_rt_slice_append",  // Uses global GC
            RuntimeFunc::SliceSlice => "gox_rt_slice_slice",  // Uses global GC
            // Struct
            RuntimeFunc::StructHash => "gox_struct_hash",
            // Map
            RuntimeFunc::MapCreate => "gox_map_create",
            RuntimeFunc::MapLen => "gox_map_len",
            RuntimeFunc::MapGet => "gox_map_get",
            RuntimeFunc::MapSet => "gox_map_set",
            RuntimeFunc::MapDelete => "gox_map_delete",
            RuntimeFunc::MapContains => "gox_map_contains",
            // Closure
            RuntimeFunc::ClosureCreate => "gox_rt_closure_create",  // Uses global GC
            RuntimeFunc::ClosureFuncId => "gox_closure_func_id",
            RuntimeFunc::ClosureUpvalueCount => "gox_closure_upvalue_count",
            RuntimeFunc::ClosureGetUpvalue => "gox_closure_get_upvalue",
            RuntimeFunc::ClosureSetUpvalue => "gox_closure_set_upvalue",
            RuntimeFunc::UpvalBoxCreate => "gox_rt_upval_box_create",  // Uses global GC
            RuntimeFunc::UpvalBoxGet => "gox_upval_box_get",
            RuntimeFunc::UpvalBoxSet => "gox_upval_box_set",
            // Interface
            RuntimeFunc::InterfaceUnboxType => "gox_interface_unbox_type",
            RuntimeFunc::InterfaceUnboxData => "gox_interface_unbox_data",
            RuntimeFunc::InterfaceIsNil => "gox_interface_is_nil",
            // Builtin
            RuntimeFunc::BuiltinLen => "gox_builtin_len",
            RuntimeFunc::BuiltinCap => "gox_builtin_cap",
            // Strings
            RuntimeFunc::StringsIndex => "gox_strings_index",
            RuntimeFunc::StringsCount => "gox_strings_count",
            RuntimeFunc::StringsToLower => "gox_strings_to_lower",
            RuntimeFunc::StringsToUpper => "gox_strings_to_upper",
            RuntimeFunc::StringsEqualFold => "gox_strings_equal_fold",
            // Fmt
            RuntimeFunc::FmtFormatValue => "gox_fmt_format_value",
            RuntimeFunc::FmtPrintln => "gox_fmt_println",
            // Goroutine
            RuntimeFunc::GoSpawn => "gox_go_spawn",
            RuntimeFunc::GoYield => "gox_yield",
            RuntimeFunc::ChanSend => "gox_chan_send",
            RuntimeFunc::ChanRecv => "gox_chan_recv",
            RuntimeFunc::ChanClose => "gox_chan_close",
            // Defer/Panic/Recover
            RuntimeFunc::DeferPush => "gox_defer_push",
            RuntimeFunc::DeferPop => "gox_defer_pop",
            RuntimeFunc::Panic => "gox_panic",
            RuntimeFunc::Recover => "gox_recover",
            // Select
            RuntimeFunc::SelectStart => "gox_select_start",
            RuntimeFunc::SelectAddSend => "gox_select_add_send",
            RuntimeFunc::SelectAddRecv => "gox_select_add_recv",
            RuntimeFunc::SelectExec => "gox_select_exec",
            // Iterator
            RuntimeFunc::IterBegin => "gox_iter_begin",
            RuntimeFunc::IterNext => "gox_iter_next",
            RuntimeFunc::IterEnd => "gox_iter_end",
            // Debug/Assert
            RuntimeFunc::DebugPrint => "gox_debug_print",
            RuntimeFunc::AssertBegin => "gox_assert_begin",
            RuntimeFunc::AssertArg => "gox_assert_arg",
            RuntimeFunc::AssertEnd => "gox_assert_end",
            // Function table
            RuntimeFunc::FuncTablePtr => "gox_func_table_ptr",
            // Extern dispatch
            RuntimeFunc::ExternCall => "gox_extern_call",
        }
    }

    /// Build the Cranelift signature for this runtime function.
    pub fn signature(&self, call_conv: CallConv) -> Signature {
        let mut sig = Signature::new(call_conv);
        
        match self {
            // === GC ===
            RuntimeFunc::GcAlloc => {
                // gox_rt_alloc uses global GC, no GC pointer needed
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I64));  // slots
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::GetGlobal => {
                sig.params.push(AbiParam::new(I64));  // idx
                sig.returns.push(AbiParam::new(I64)); // value
            }
            RuntimeFunc::SetGlobal => {
                sig.params.push(AbiParam::new(I64));  // idx
                sig.params.push(AbiParam::new(I64));  // value
            }
            RuntimeFunc::GcReadSlot => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::GcWriteSlot => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }
            RuntimeFunc::GcWriteBarrier => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }
            RuntimeFunc::GcMarkGray => {
                sig.params.push(AbiParam::new(I64));
            }

            // === String ===
            RuntimeFunc::StringLen => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::StringIndex => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I8));
            }
            RuntimeFunc::StringConcat => {
                // gox_rt_string_concat(type_id, a, b) uses global GC
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I64));  // a
                sig.params.push(AbiParam::new(I64));  // b
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::StringEq | RuntimeFunc::StringNe => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I8));
            }
            RuntimeFunc::StringFromPtr => {
                // gox_rt_string_from_ptr(ptr, len, type_id) uses global GC
                sig.params.push(AbiParam::new(I64));  // data ptr
                sig.params.push(AbiParam::new(I64));  // len
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }

            // === Array ===
            RuntimeFunc::ArrayCreate => {
                // gox_rt_array_create(type_id, elem_type, elem_size, len)
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I32));  // elem_type
                sig.params.push(AbiParam::new(I64));  // elem_size
                sig.params.push(AbiParam::new(I64));  // len
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::ArrayLen => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::ArrayGet => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::ArraySet => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }

            // === Slice ===
            RuntimeFunc::SliceCreate => {
                // gox_rt_slice_create(type_id, array, start, len, cap)
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I64));  // array
                sig.params.push(AbiParam::new(I64));  // start
                sig.params.push(AbiParam::new(I64));  // len
                sig.params.push(AbiParam::new(I64));  // cap
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::SliceLen | RuntimeFunc::SliceCap => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::SliceGet => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::SliceSet => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }
            RuntimeFunc::SliceAppend => {
                // gox_rt_slice_append(type_id, arr_type_id, slice, val)
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I32));  // arr_type_id
                sig.params.push(AbiParam::new(I64));  // slice
                sig.params.push(AbiParam::new(I64));  // val
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::SliceSlice => {
                // gox_rt_slice_slice(type_id, slice, start, end)
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I64));  // slice
                sig.params.push(AbiParam::new(I64));  // start
                sig.params.push(AbiParam::new(I64));  // end
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }

            // === Struct ===
            RuntimeFunc::StructHash => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }

            // === Map ===
            RuntimeFunc::MapCreate => {
                sig.params.push(AbiParam::new(I32));
                sig.params.push(AbiParam::new(I32));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::MapLen => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::MapGet => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I8));
            }
            RuntimeFunc::MapSet => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }
            RuntimeFunc::MapDelete => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }
            RuntimeFunc::MapContains => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I8));
            }

            // === Closure ===
            RuntimeFunc::ClosureCreate => {
                // gox_rt_closure_create(type_id, func_id, upvalue_count) - uses global GC
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I32));  // func_id
                sig.params.push(AbiParam::new(I64));  // upvalue_count
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::ClosureFuncId => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I32));
            }
            RuntimeFunc::ClosureUpvalueCount => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::ClosureGetUpvalue => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::ClosureSetUpvalue => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }
            RuntimeFunc::UpvalBoxCreate => {
                // gox_rt_upval_box_create(type_id) - uses global GC
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::UpvalBoxGet => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::UpvalBoxSet => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
            }

            // === Interface ===
            RuntimeFunc::InterfaceUnboxType => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I32));
            }
            RuntimeFunc::InterfaceUnboxData => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::InterfaceIsNil => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I8));
            }

            // === Builtin ===
            RuntimeFunc::BuiltinLen | RuntimeFunc::BuiltinCap => {
                sig.params.push(AbiParam::new(I8));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }

            // === Strings stdlib ===
            RuntimeFunc::StringsIndex | RuntimeFunc::StringsCount => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::StringsToLower | RuntimeFunc::StringsToUpper => {
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::StringsEqualFold => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I8));
            }

            // === Fmt ===
            RuntimeFunc::FmtFormatValue => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I8));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::FmtPrintln => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }

            // === Goroutine ===
            RuntimeFunc::GoSpawn => {
                sig.params.push(AbiParam::new(I64));  // func_ptr
                sig.params.push(AbiParam::new(I64));  // args_ptr (pointer to stack args)
                sig.params.push(AbiParam::new(I64));  // arg_count
            }
            RuntimeFunc::GoYield => {
                // No params, no return
            }
            RuntimeFunc::ChanSend => {
                sig.params.push(AbiParam::new(I64));  // chan_ref
                sig.params.push(AbiParam::new(I64));  // value
                sig.returns.push(AbiParam::new(I8));  // success
            }
            RuntimeFunc::ChanRecv => {
                sig.params.push(AbiParam::new(I64));  // chan_ref
                sig.params.push(AbiParam::new(I64));  // ok_ptr
                sig.returns.push(AbiParam::new(I64)); // value
            }
            RuntimeFunc::ChanClose => {
                sig.params.push(AbiParam::new(I64));  // chan_ref
            }

            // === Defer/Panic/Recover ===
            RuntimeFunc::DeferPush => {
                sig.params.push(AbiParam::new(I64));  // func_ptr
                sig.params.push(AbiParam::new(I64));  // args_ptr
                sig.params.push(AbiParam::new(I64));  // arg_count
            }
            RuntimeFunc::DeferPop => {
                // No params - pops and executes defers for current frame
            }
            RuntimeFunc::Panic => {
                sig.params.push(AbiParam::new(I64));  // panic_value
            }
            RuntimeFunc::Recover => {
                sig.returns.push(AbiParam::new(I64)); // recovered_value (0 if not panicking)
            }

            // === Select ===
            RuntimeFunc::SelectStart => {
                sig.params.push(AbiParam::new(I64));  // case_count
                sig.params.push(AbiParam::new(I64));  // has_default
            }
            RuntimeFunc::SelectAddSend => {
                sig.params.push(AbiParam::new(I64));  // chan_ref
                sig.params.push(AbiParam::new(I64));  // value
            }
            RuntimeFunc::SelectAddRecv => {
                sig.params.push(AbiParam::new(I64));  // chan_ref
                sig.returns.push(AbiParam::new(I64)); // value
                sig.returns.push(AbiParam::new(I64)); // ok
            }
            RuntimeFunc::SelectExec => {
                sig.returns.push(AbiParam::new(I64)); // chosen_case_index
            }

            // === Iterator ===
            RuntimeFunc::IterBegin => {
                sig.params.push(AbiParam::new(I64));  // container_ref
                sig.params.push(AbiParam::new(I64));  // iter_type (0=slice, 1=map, 2=string)
                sig.returns.push(AbiParam::new(I64)); // iterator_handle
            }
            RuntimeFunc::IterNext => {
                sig.params.push(AbiParam::new(I64));  // iterator_handle
                sig.params.push(AbiParam::new(I64));  // out: *mut u64 (writes done, key, value)
            }
            RuntimeFunc::IterEnd => {
                sig.params.push(AbiParam::new(I64));  // iterator_handle
            }

            // === Debug/Assert ===
            RuntimeFunc::DebugPrint => {
                sig.params.push(AbiParam::new(I64));  // value
                sig.params.push(AbiParam::new(I8));   // type_tag
            }
            RuntimeFunc::AssertBegin => {
                sig.params.push(AbiParam::new(I64));  // condition (0=failed)
                sig.params.push(AbiParam::new(I64));  // arg_count
                sig.params.push(AbiParam::new(I64));  // line number
                sig.returns.push(AbiParam::new(I8));  // should_continue (1=passed, skip args)
            }
            RuntimeFunc::AssertArg => {
                sig.params.push(AbiParam::new(I64));  // value
                sig.params.push(AbiParam::new(I8));   // type_tag
            }
            RuntimeFunc::AssertEnd => {
                // No params - checks if assert failed and terminates if so
            }
            
            // === Function Table ===
            RuntimeFunc::FuncTablePtr => {
                // () -> ptr
                sig.returns.push(AbiParam::new(I64));  // table pointer
            }
            
            // === Extern Dispatch ===
            RuntimeFunc::ExternCall => {
                // (name_ptr, name_len, args_ptr, arg_count, rets_ptr, ret_count) -> i32
                sig.params.push(AbiParam::new(I64));  // name_ptr
                sig.params.push(AbiParam::new(I64));  // name_len
                sig.params.push(AbiParam::new(I64));  // args_ptr
                sig.params.push(AbiParam::new(I64));  // arg_count
                sig.params.push(AbiParam::new(I64));  // rets_ptr
                sig.params.push(AbiParam::new(I64));  // ret_count
                sig.returns.push(AbiParam::new(I32)); // error code
            }
        }
        
        sig
    }
}

/// Registry of declared runtime functions.
pub struct RuntimeFuncs {
    funcs: HashMap<RuntimeFunc, FuncId>,
    call_conv: CallConv,
}

impl RuntimeFuncs {
    /// Create a new empty registry.
    pub fn new(call_conv: CallConv) -> Self {
        Self {
            funcs: HashMap::new(),
            call_conv,
        }
    }

    /// Declare a runtime function if not already declared.
    pub fn declare<M: Module>(&mut self, module: &mut M, func: RuntimeFunc) -> Result<FuncId> {
        if let Some(&id) = self.funcs.get(&func) {
            return Ok(id);
        }

        let sig = func.signature(self.call_conv);
        let id = module.declare_function(func.symbol_name(), Linkage::Import, &sig)?;
        self.funcs.insert(func, id);
        Ok(id)
    }

    /// Get a previously declared function.
    pub fn get(&self, func: RuntimeFunc) -> Option<FuncId> {
        self.funcs.get(&func).copied()
    }

    /// Declare all commonly used runtime functions.
    pub fn declare_common<M: Module>(&mut self, module: &mut M) -> Result<()> {
        self.declare(module, RuntimeFunc::GcAlloc)?;
        self.declare(module, RuntimeFunc::GcReadSlot)?;
        self.declare(module, RuntimeFunc::GcWriteSlot)?;
        self.declare(module, RuntimeFunc::StringLen)?;
        self.declare(module, RuntimeFunc::StringConcat)?;
        self.declare(module, RuntimeFunc::StringEq)?;
        self.declare(module, RuntimeFunc::SliceLen)?;
        self.declare(module, RuntimeFunc::SliceGet)?;
        self.declare(module, RuntimeFunc::SliceSet)?;
        self.declare(module, RuntimeFunc::SliceAppend)?;
        self.declare(module, RuntimeFunc::MapCreate)?;
        self.declare(module, RuntimeFunc::MapGet)?;
        self.declare(module, RuntimeFunc::MapSet)?;
        self.declare(module, RuntimeFunc::FmtPrintln)?;
        Ok(())
    }
}
