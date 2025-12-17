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
    GcAlloc,
    GcReadSlot,
    GcWriteSlot,
    GcWriteBarrier,
    GcMarkGray,

    // === String (5) ===
    StringLen,
    StringIndex,
    StringConcat,
    StringEq,
    StringNe,

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
}

impl RuntimeFunc {
    /// Get the symbol name for this runtime function.
    pub fn symbol_name(&self) -> &'static str {
        match self {
            // GC
            RuntimeFunc::GcAlloc => "gox_gc_alloc",
            RuntimeFunc::GcReadSlot => "gox_gc_read_slot",
            RuntimeFunc::GcWriteSlot => "gox_gc_write_slot",
            RuntimeFunc::GcWriteBarrier => "gox_gc_write_barrier",
            RuntimeFunc::GcMarkGray => "gox_gc_mark_gray",
            // String
            RuntimeFunc::StringLen => "gox_string_len",
            RuntimeFunc::StringIndex => "gox_string_index",
            RuntimeFunc::StringConcat => "gox_string_concat",
            RuntimeFunc::StringEq => "gox_string_eq",
            RuntimeFunc::StringNe => "gox_string_ne",
            // Array
            RuntimeFunc::ArrayCreate => "gox_array_create",
            RuntimeFunc::ArrayLen => "gox_array_len",
            RuntimeFunc::ArrayGet => "gox_array_get",
            RuntimeFunc::ArraySet => "gox_array_set",
            // Slice
            RuntimeFunc::SliceCreate => "gox_slice_create",
            RuntimeFunc::SliceLen => "gox_slice_len",
            RuntimeFunc::SliceCap => "gox_slice_cap",
            RuntimeFunc::SliceGet => "gox_slice_get",
            RuntimeFunc::SliceSet => "gox_slice_set",
            RuntimeFunc::SliceAppend => "gox_slice_append",
            RuntimeFunc::SliceSlice => "gox_slice_slice",
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
            RuntimeFunc::ClosureCreate => "gox_closure_create",
            RuntimeFunc::ClosureFuncId => "gox_closure_func_id",
            RuntimeFunc::ClosureUpvalueCount => "gox_closure_upvalue_count",
            RuntimeFunc::ClosureGetUpvalue => "gox_closure_get_upvalue",
            RuntimeFunc::ClosureSetUpvalue => "gox_closure_set_upvalue",
            RuntimeFunc::UpvalBoxCreate => "gox_upval_box_create",
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
        }
    }

    /// Build the Cranelift signature for this runtime function.
    pub fn signature(&self, call_conv: CallConv) -> Signature {
        let mut sig = Signature::new(call_conv);
        
        match self {
            // === GC ===
            RuntimeFunc::GcAlloc => {
                sig.params.push(AbiParam::new(I32));  // type_id
                sig.params.push(AbiParam::new(I64));  // slots
                sig.returns.push(AbiParam::new(I64)); // GcRef
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
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::StringEq | RuntimeFunc::StringNe => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I8));
            }

            // === Array ===
            RuntimeFunc::ArrayCreate => {
                sig.params.push(AbiParam::new(I32));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
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
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
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
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
            }
            RuntimeFunc::SliceSlice => {
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
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
                sig.params.push(AbiParam::new(I32));
                sig.params.push(AbiParam::new(I64));
                sig.returns.push(AbiParam::new(I64));
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
                sig.returns.push(AbiParam::new(I64));
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
