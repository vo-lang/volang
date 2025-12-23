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
/// These correspond to extern "C" functions in vo-runtime-core/ffi.rs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeFunc {
    // === GC (5) ===
    GcAlloc,       // vo_rt_alloc (uses global GC)
    GcReadSlot,
    GcWriteSlot,
    GcWriteBarrier,
    GcMarkGray,

    // === Globals (2) ===
    GetGlobal,     // vo_rt_get_global
    SetGlobal,     // vo_rt_set_global

    // === String (11) ===
    StringLen,
    StringIndex,
    StringConcat,
    StringEq,
    StringNe,
    StringLt,
    StringLe,
    StringGt,
    StringGe,
    StringFromPtr,
    StringSlice,

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

    // === Goroutine (6) ===
    GoSpawn,
    GoYield,
    ChanNew,
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
            RuntimeFunc::GcAlloc => "vo_rt_alloc",  // Uses global GC
            RuntimeFunc::GcReadSlot => "vo_gc_read_slot",
            RuntimeFunc::GcWriteSlot => "vo_gc_write_slot",
            RuntimeFunc::GcWriteBarrier => "vo_rt_write_barrier",  // Uses global GC
            RuntimeFunc::GcMarkGray => "vo_rt_mark_gray",  // Uses global GC
            // Globals
            RuntimeFunc::GetGlobal => "vo_rt_get_global",
            RuntimeFunc::SetGlobal => "vo_rt_set_global",
            // String
            RuntimeFunc::StringLen => "vo_string_len",
            RuntimeFunc::StringIndex => "vo_string_index",
            RuntimeFunc::StringConcat => "vo_rt_string_concat",  // Uses global GC
            RuntimeFunc::StringEq => "vo_string_eq",
            RuntimeFunc::StringNe => "vo_string_ne",
            RuntimeFunc::StringLt => "vo_string_lt",
            RuntimeFunc::StringLe => "vo_string_le",
            RuntimeFunc::StringGt => "vo_string_gt",
            RuntimeFunc::StringGe => "vo_string_ge",
            RuntimeFunc::StringFromPtr => "vo_rt_string_from_ptr",  // Uses global GC
            RuntimeFunc::StringSlice => "vo_rt_string_slice",
            // Array
            RuntimeFunc::ArrayCreate => "vo_rt_array_create",  // Uses global GC
            RuntimeFunc::ArrayLen => "vo_array_len",
            RuntimeFunc::ArrayGet => "vo_array_get",
            RuntimeFunc::ArraySet => "vo_array_set",
            // Slice
            RuntimeFunc::SliceCreate => "vo_rt_slice_create",  // Uses global GC
            RuntimeFunc::SliceLen => "vo_slice_len",
            RuntimeFunc::SliceCap => "vo_slice_cap",
            RuntimeFunc::SliceGet => "vo_slice_get",
            RuntimeFunc::SliceSet => "vo_slice_set",
            RuntimeFunc::SliceAppend => "vo_rt_slice_append",  // Uses global GC
            RuntimeFunc::SliceSlice => "vo_rt_slice_slice",  // Uses global GC
            // Struct
            RuntimeFunc::StructHash => "vo_struct_hash",
            // Map
            RuntimeFunc::MapCreate => "vo_map_create",
            RuntimeFunc::MapLen => "vo_map_len",
            RuntimeFunc::MapGet => "vo_map_get",
            RuntimeFunc::MapSet => "vo_map_set",
            RuntimeFunc::MapDelete => "vo_map_delete",
            RuntimeFunc::MapContains => "vo_map_contains",
            // Closure
            RuntimeFunc::ClosureCreate => "vo_rt_closure_create",  // Uses global GC
            RuntimeFunc::ClosureFuncId => "vo_closure_func_id",
            RuntimeFunc::ClosureUpvalueCount => "vo_closure_upvalue_count",
            RuntimeFunc::ClosureGetUpvalue => "vo_closure_get_upvalue",
            RuntimeFunc::ClosureSetUpvalue => "vo_closure_set_upvalue",
            RuntimeFunc::UpvalBoxCreate => "vo_rt_upval_box_create",  // Uses global GC
            RuntimeFunc::UpvalBoxGet => "vo_upval_box_get",
            RuntimeFunc::UpvalBoxSet => "vo_upval_box_set",
            // Interface
            RuntimeFunc::InterfaceUnboxType => "vo_interface_unbox_type",
            RuntimeFunc::InterfaceUnboxData => "vo_interface_unbox_data",
            RuntimeFunc::InterfaceIsNil => "vo_interface_is_nil",
            // Builtin
            RuntimeFunc::BuiltinLen => "vo_builtin_len",
            RuntimeFunc::BuiltinCap => "vo_builtin_cap",
            // Strings
            RuntimeFunc::StringsIndex => "vo_strings_index",
            RuntimeFunc::StringsCount => "vo_strings_count",
            RuntimeFunc::StringsToLower => "vo_strings_to_lower",
            RuntimeFunc::StringsToUpper => "vo_strings_to_upper",
            RuntimeFunc::StringsEqualFold => "vo_strings_equal_fold",
            // Fmt
            RuntimeFunc::FmtFormatValue => "vo_fmt_format_value",
            RuntimeFunc::FmtPrintln => "vo_fmt_println",
            // Goroutine
            RuntimeFunc::GoSpawn => "vo_go_spawn",
            RuntimeFunc::GoYield => "vo_yield",
            RuntimeFunc::ChanNew => "vo_chan_new",
            RuntimeFunc::ChanSend => "vo_chan_send",
            RuntimeFunc::ChanRecv => "vo_chan_recv",
            RuntimeFunc::ChanClose => "vo_chan_close",
            // Defer/Panic/Recover
            RuntimeFunc::DeferPush => "vo_defer_push",
            RuntimeFunc::DeferPop => "vo_defer_pop",
            RuntimeFunc::Panic => "vo_panic",
            RuntimeFunc::Recover => "vo_recover",
            // Select
            RuntimeFunc::SelectStart => "vo_select_start",
            RuntimeFunc::SelectAddSend => "vo_select_add_send",
            RuntimeFunc::SelectAddRecv => "vo_select_add_recv",
            RuntimeFunc::SelectExec => "vo_select_exec",
            // Iterator
            RuntimeFunc::IterBegin => "vo_iter_begin",
            RuntimeFunc::IterNext => "vo_iter_next",
            RuntimeFunc::IterEnd => "vo_iter_end",
            // Debug/Assert
            RuntimeFunc::DebugPrint => "vo_debug_print",
            RuntimeFunc::AssertBegin => "vo_assert_begin",
            RuntimeFunc::AssertArg => "vo_assert_arg",
            RuntimeFunc::AssertEnd => "vo_assert_end",
            // Function table
            RuntimeFunc::FuncTablePtr => "vo_func_table_ptr",
            // Extern dispatch
            RuntimeFunc::ExternCall => "vo_extern_call",
        }
    }

    /// Build the Cranelift signature for this runtime function.
    pub fn signature(&self, call_conv: CallConv) -> Signature {
        let mut sig = Signature::new(call_conv);
        
        match self {
            // === GC ===
            RuntimeFunc::GcAlloc => {
                // vo_rt_alloc uses global GC, no GC pointer needed
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
                // vo_rt_write_barrier uses global GC, no GC pointer needed
                sig.params.push(AbiParam::new(I64));  // parent
                sig.params.push(AbiParam::new(I64));  // child
            }
            RuntimeFunc::GcMarkGray => {
                // vo_rt_mark_gray uses global GC, no GC pointer needed
                sig.params.push(AbiParam::new(I64));  // obj
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
                // vo_rt_string_concat(type_id, a, b) uses global GC
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I64));  // a
                sig.params.push(AbiParam::new(I64));  // b
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::StringEq | RuntimeFunc::StringNe
            | RuntimeFunc::StringLt | RuntimeFunc::StringLe
            | RuntimeFunc::StringGt | RuntimeFunc::StringGe => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I8));
            }
            RuntimeFunc::StringSlice => {
                // vo_rt_string_slice(type_id, str, start, end)
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I64));  // str
                sig.params.push(AbiParam::new(I64));  // start
                sig.params.push(AbiParam::new(I64));  // end
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::StringFromPtr => {
                // vo_rt_string_from_ptr(ptr, len, type_id) uses global GC
                sig.params.push(AbiParam::new(I64));  // data ptr
                sig.params.push(AbiParam::new(I64));  // len
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }

            // === Array ===
            RuntimeFunc::ArrayCreate => {
                // vo_rt_array_create(type_id, elem_type, elem_size, len)
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
                // vo_rt_slice_create(type_id, array, start, len, cap)
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
                // vo_rt_slice_append(type_id, arr_type_id, slice, val)
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I32));  // arr_type_id
                sig.params.push(AbiParam::new(I64));  // slice
                sig.params.push(AbiParam::new(I64));  // val
                sig.returns.push(AbiParam::new(I64)); // GcRef
            }
            RuntimeFunc::SliceSlice => {
                // vo_rt_slice_slice(type_id, slice, start, end)
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
                // vo_rt_closure_create(type_id, func_id, upvalue_count) - uses global GC
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
                // vo_rt_upval_box_create(type_id) - uses global GC
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
            RuntimeFunc::ChanNew => {
                sig.params.push(AbiParam::new(I32));  // elem_type
                sig.params.push(AbiParam::new(I64));  // capacity
                sig.returns.push(AbiParam::new(I64)); // chan_ref
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
