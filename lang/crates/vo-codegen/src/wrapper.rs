//! Interface method wrapper generation.
//!
//! This module handles generating wrapper functions for interface method dispatch:
//! - `$iface` wrappers: For value receiver methods (unbox GcRef, call original)
//! - `$promoted` wrappers: For promoted methods through embedding (navigate path, call original)

use vo_analysis::objects::TypeKey;
use vo_runtime::SlotType;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

/// Define forwarded parameters in a wrapper function.
/// Returns the first parameter slot if any parameters were defined.
fn define_forwarded_params(builder: &mut FuncBuilder, param_slots: u16) -> Option<u16> {
    if param_slots > 0 {
        let first = builder.define_param(None, 1, &[SlotType::Value]);
        for _ in 1..param_slots {
            builder.define_param(None, 1, &[SlotType::Value]);
        }
        Some(first)
    } else {
        None
    }
}

/// Compute total slot count for a tuple type (params or results).
fn tuple_slot_count(tuple_key: vo_analysis::objects::TypeKey, tc_objs: &vo_analysis::objects::TCObjects) -> u16 {
    tc_objs.types[tuple_key].try_as_tuple()
        .map(|t| t.vars().iter()
            .map(|v| {
                let typ = tc_objs.lobjs[*v].typ().unwrap();
                vo_analysis::check::type_info::type_slot_count(typ, tc_objs)
            })
            .sum())
        .unwrap_or(0)
}

/// Generate a wrapper function for value receiver methods.
/// The wrapper accepts interface data slot (GcRef for struct/array, value for basic types),
/// and calls the original method with the receiver value.
pub fn generate_iface_wrapper(
    ctx: &mut CodegenContext,
    info: &TypeInfoWrapper,
    func_decl: &vo_syntax::ast::FuncDecl,
    original_func_id: u32,
    recv_type: TypeKey,
) -> Result<u32, CodegenError> {
    let name = info.project.interner.resolve(func_decl.name.symbol)
        .unwrap_or("unknown");
    let wrapper_name = format!("{}$iface", name);
    
    let mut builder = FuncBuilder::new(&wrapper_name);
    
    // Check if receiver needs unboxing (struct/array are boxed in interface slot1)
    let recv_vk = info.type_value_kind(recv_type);
    let needs_unbox = recv_vk.needs_boxing();
    
    // Wrapper receives interface data slot as first parameter (1 slot)
    builder.set_recv_slots(1);
    let slot_type = if needs_unbox { SlotType::GcRef } else { SlotType::Value };
    let data_slot = builder.define_param(None, 1, &[slot_type]);
    
    // Define other parameters (forwarded from original function)
    // Note: for variadic param, TypeExpr is element type T, but actual param is []T
    let mut wrapper_param_slots = Vec::new();
    let params = &func_decl.sig.params;
    for (i, param) in params.iter().enumerate() {
        let variadic_last = func_decl.sig.variadic && i == params.len() - 1;
        let (slots, slot_types) = if variadic_last { (1, vec![SlotType::GcRef]) } else { info.type_expr_layout(param.ty.id) };
        for name in &param.names {
            wrapper_param_slots.push((builder.define_param(Some(name.symbol), slots, &slot_types), slots));
        }
    }
    
    // Get receiver value slots
    let recv_slots = info.type_slot_count(recv_type);
    
    let ret_slots: u16 = func_decl.sig.results.iter()
        .map(|r| info.type_expr_layout(r.ty.id).0)
        .sum();
    
    // Allocate args area for call (ensure enough space for return values)
    let forwarded_param_slots: u16 = wrapper_param_slots.iter().map(|(_, s)| *s).sum();
    let total_arg_slots = recv_slots + forwarded_param_slots;
    let alloc_slots = total_arg_slots.max(ret_slots);
    let args_start = builder.alloc_temp_typed(&vec![vo_runtime::SlotType::Value; alloc_slots as usize]);
    
    if needs_unbox {
        // Struct/Array: dereference GcRef to get value
        builder.emit_ptr_get(args_start, data_slot, 0, recv_slots);
    } else {
        // Basic types: slot1 is the value directly, just copy
        builder.emit_copy(args_start, data_slot, recv_slots);
    }
    
    // Copy other parameters after receiver value
    let mut dest_offset = args_start + recv_slots;
    for (src_slot, slots) in &wrapper_param_slots {
        if *slots > 0 {
            builder.emit_copy(dest_offset, *src_slot, *slots);
            dest_offset += *slots;
        }
    }
    
    // Call and return
    emit_call_and_return(&mut builder, original_func_id, args_start, total_arg_slots, ret_slots);
    
    let func_def = builder.build();
    let wrapper_id = ctx.add_function(func_def);
    
    Ok(wrapper_id)
}

/// Generate a wrapper for method expression on promoted method.
/// 
/// Unlike `generate_promoted_wrapper` which receives GcRef (for interface dispatch),
/// this wrapper receives the outer type's value directly as parameter.
/// 
/// For `Derived.GetValue` where Derived embeds Base:
/// - Receives Derived (N slots) as parameter
/// - Extracts Base from embedded field
/// - Calls Base.GetValue
/// 
/// For `(*Derived).SetValue`:
/// - Receives *Derived (1 slot) as parameter
/// - Navigates to get *Base
/// - Calls (*Base).SetValue
pub fn generate_method_expr_promoted_wrapper(
    ctx: &mut CodegenContext,
    outer_type: TypeKey,
    embed_path: &crate::embed::EmbedPathInfo,
    original_func_id: u32,
    expects_ptr_recv: bool,
    method_name: &str,
    tc_objs: &vo_analysis::objects::TCObjects,
) -> u32 {
    let orig_func = &ctx.module().functions[original_func_id as usize];
    let orig_param_slots = orig_func.param_slots;
    let ret_slots = orig_func.ret_slots;
    let orig_recv_slots = orig_func.recv_slots;
    let forwarded_param_slots = orig_param_slots.saturating_sub(orig_recv_slots);
    
    let final_type = embed_path.final_type;
    let recv_slots_for_call = if expects_ptr_recv {
        1u16
    } else {
        vo_analysis::check::type_info::type_slot_count(final_type, tc_objs)
    };
    
    let wrapper_name = format!("{}$mexpr", method_name);
    let mut builder = FuncBuilder::new(&wrapper_name);
    
    // Determine if outer receiver is a pointer type
    // For (*T).M, outer is pointer (GcRef)
    // For T.M (even if path has pointer steps), outer is value type
    // NOTE: has_pointer_step does NOT mean outer is pointer - it just means the path
    // contains embedded pointer fields that need to be dereferenced
    let outer_is_pointer = expects_ptr_recv;
    
    let outer_recv_slots = if outer_is_pointer {
        1u16
    } else {
        vo_analysis::check::type_info::type_slot_count(outer_type, tc_objs)
    };
    
    builder.set_recv_slots(outer_recv_slots);
    
    // Define outer receiver parameter based on whether it's pointer or value
    let outer_recv = if outer_is_pointer {
        builder.define_param(None, 1, &[SlotType::GcRef])
    } else {
        builder.define_param(None, outer_recv_slots, &vec![SlotType::Value; outer_recv_slots as usize])
    };
    
    // Define forwarded params
    let first_param_slot = define_forwarded_params(&mut builder, forwarded_param_slots);
    
    // Allocate args area for call
    let total_arg_slots = recv_slots_for_call + forwarded_param_slots;
    let alloc_slots = total_arg_slots.max(ret_slots);
    let args_start = builder.alloc_temp_typed(&vec![vo_runtime::SlotType::Value; alloc_slots as usize]);
    
    // Emit receiver extraction based on path
    emit_receiver_through_path(
        &mut builder,
        outer_recv,
        outer_is_pointer,
        args_start,
        &embed_path.steps,
        expects_ptr_recv,
        recv_slots_for_call,
    );
    
    // Copy forwarded params
    if let Some(first_param) = first_param_slot {
        let params_dest = args_start + recv_slots_for_call;
        builder.emit_copy(params_dest, first_param, forwarded_param_slots);
    }
    
    // Call and return
    emit_call_and_return(&mut builder, original_func_id, args_start, total_arg_slots, ret_slots);
    
    let func_def = builder.build();
    ctx.add_function(func_def)
}

/// Emit receiver extraction through embed path.
/// 
/// Thin wrapper around `embed::emit_embed_path_traversal` for backwards compatibility.
fn emit_receiver_through_path(
    builder: &mut FuncBuilder,
    outer_recv: u16,
    outer_is_pointer: bool,
    args_start: u16,
    steps: &[crate::embed::EmbedStep],
    expects_ptr_recv: bool,
    recv_slots: u16,
) {
    let start = crate::embed::TraverseStart {
        reg: outer_recv,
        is_pointer: outer_is_pointer,
    };
    crate::embed::emit_embed_path_traversal(builder, start, steps, expects_ptr_recv, recv_slots, args_start);
}

/// Generate a wrapper for promoted method that navigates through embedded fields
/// and calls the original method directly (no double wrapper).
/// 
/// For example, if `Outer` embeds `*Inner` and `Inner` has method `Foo()`,
/// calling `outer.Foo()` through interface needs a wrapper that:
/// 1. Receives GcRef to Outer
/// 2. Reads the embedded *Inner pointer from Outer
/// 3. Dereferences *Inner to get Inner value (if value receiver)
/// 4. Calls Inner.Foo directly with the value
pub fn generate_promoted_wrapper(
    ctx: &mut CodegenContext,
    outer_type: TypeKey,
    embed_indices: &[usize],
    original_func_id: u32,
    iface_func_id: u32,
    method_name: &str,
    tc_objs: &vo_analysis::objects::TCObjects,
) -> u32 {
    // Get the original function's signature
    let orig_func = &ctx.module().functions[original_func_id as usize];
    let orig_param_slots = orig_func.param_slots;
    let ret_slots = orig_func.ret_slots;
    let orig_recv_slots = orig_func.recv_slots;
    let forwarded_param_slots = orig_param_slots.saturating_sub(orig_recv_slots);
    
    // Check if original method is pointer receiver
    let is_pointer_receiver = original_func_id == iface_func_id;
    
    // Use unified embed path analysis
    // Note: embed_indices here does NOT include method index (already stripped by caller)
    // So we pass indices as-is without adding a fake method index
    let path_info = crate::embed::analyze_embed_path_raw(outer_type, embed_indices, tc_objs);
    let current_type = path_info.final_type;
    
    let recv_slots_for_call = if is_pointer_receiver {
        1u16
    } else {
        vo_analysis::check::type_info::type_slot_count(current_type, tc_objs)
    };
    
    // Build wrapper using FuncBuilder
    let wrapper_name = format!("{}$promoted", method_name);
    let mut builder = FuncBuilder::new(&wrapper_name);
    
    // Define receiver parameter (GcRef to outer type)
    builder.set_recv_slots(1);
    let outer_gcref = builder.define_param(None, 1, &[SlotType::GcRef]);
    
    // Define forwarded params
    let first_param_slot = define_forwarded_params(&mut builder, forwarded_param_slots);
    
    // Allocate args area for call (ensure enough space for return values)
    let total_arg_slots = recv_slots_for_call + forwarded_param_slots;
    let alloc_slots = total_arg_slots.max(ret_slots);
    let args_start = builder.alloc_temp_typed(&vec![vo_runtime::SlotType::Value; alloc_slots as usize]);
    
    // Emit receiver loading based on embedding type
    // For promoted wrapper, outer is always GcRef (outer_is_pointer = true)
    emit_receiver_through_path(
        &mut builder,
        outer_gcref,
        true,  // outer_is_pointer
        args_start,
        &path_info.steps,
        is_pointer_receiver,
        recv_slots_for_call,
    );
    
    // Copy forwarded params
    if let Some(first_param) = first_param_slot {
        let params_dest = args_start + recv_slots_for_call;
        builder.emit_copy(params_dest, first_param, forwarded_param_slots);
    }
    
    // Call and return
    emit_call_and_return(&mut builder, original_func_id, args_start, total_arg_slots, ret_slots);
    
    let func_def = builder.build();
    ctx.add_function(func_def)
}

// =============================================================================
// Helper functions
// =============================================================================

/// Emit a function call instruction
fn emit_call(builder: &mut FuncBuilder, func_id: u32, args_start: u16, arg_slots: u16, ret_slots: u16) {
    let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_id);
    builder.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
}

/// Emit call and return - common wrapper epilogue
fn emit_call_and_return(builder: &mut FuncBuilder, func_id: u32, args_start: u16, arg_slots: u16, ret_slots: u16) {
    emit_call(builder, func_id, args_start, arg_slots, ret_slots);
    builder.set_ret_slots(ret_slots);
    builder.emit_op(Opcode::Return, args_start, ret_slots, 0);
}

/// Generate wrapper for methods coming from embedded interface fields.
/// 
/// When a struct embeds an interface:
/// ```
/// type Outer struct { Inner }  // Inner is interface type
/// ```
/// calling `outer.Foo()` through interface dispatch needs a wrapper that:
/// 1. Receives GcRef to Outer
/// 2. Reads the embedded interface (2 slots: itab, data)
/// 3. Calls the method on that interface via CallIface
pub fn generate_embedded_iface_wrapper(
    ctx: &mut CodegenContext,
    _outer_type: TypeKey,
    embed_offset: u16,
    iface_type: TypeKey,
    method_name: &str,
    method_obj: vo_analysis::objects::ObjKey,
    tc_objs: &vo_analysis::objects::TCObjects,
    interner: &vo_common::SymbolInterner,
) -> u32 {
    let offset = embed_offset;
    
    // Get interface meta to find method index
    let iface_meta_id = ctx.get_or_create_interface_meta_id(iface_type, tc_objs, interner);
    let iface_meta = &ctx.module().interface_metas[iface_meta_id as usize];
    let method_idx = iface_meta.method_names.iter()
        .position(|n| n == method_name)
        .expect("method must exist in embedded interface");
    
    // Get method signature from the interface method obj
    let method_type = tc_objs.lobjs[method_obj].typ()
        .expect("interface method must have type");
    let sig = tc_objs.types[method_type].try_as_signature()
        .expect("method type must be signature");
    
    // Compute param and return slots
    let param_slots = tuple_slot_count(sig.params(), tc_objs);
    let ret_slots = tuple_slot_count(sig.results(), tc_objs);
    
    // Build wrapper
    let wrapper_name = format!("{}$embed_iface", method_name);
    let mut builder = FuncBuilder::new(&wrapper_name);
    
    // Receiver: GcRef to outer struct
    builder.set_recv_slots(1);
    let outer_gcref = builder.define_param(None, 1, &[SlotType::GcRef]);
    
    // Forward parameters
    let first_param_slot = define_forwarded_params(&mut builder, param_slots);
    
    // Load embedded interface (2 slots) from outer struct
    let iface_slot = builder.alloc_temp_typed(&[vo_runtime::SlotType::Interface0, vo_runtime::SlotType::Interface1]);
    builder.emit_ptr_get(iface_slot, outer_gcref, offset, 2);
    
    // Allocate args for CallIface (params only, receiver is separate)
    let args_start = builder.alloc_temp_typed(&vec![vo_runtime::SlotType::Value; param_slots.max(ret_slots).max(1) as usize]);
    
    // Copy forwarded params
    if let Some(first_param) = first_param_slot {
        builder.emit_copy(args_start, first_param, param_slots);
    }
    
    // CallIface: a=iface_slot, b=args_start, c=(arg_slots<<8|ret_slots), flags=method_idx
    let c = crate::type_info::encode_call_args(param_slots, ret_slots);
    builder.emit_with_flags(Opcode::CallIface, method_idx as u8, iface_slot, args_start, c);
    
    // Return
    builder.set_ret_slots(ret_slots);
    builder.emit_op(Opcode::Return, args_start, ret_slots, 0);
    
    let func_def = builder.build();
    ctx.add_function(func_def)
}
