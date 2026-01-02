//! Interface method wrapper generation.
//!
//! This module handles generating wrapper functions for interface method dispatch:
//! - `$iface` wrappers: For value receiver methods (unbox GcRef, call original)
//! - `$promoted` wrappers: For promoted methods through embedding (navigate path, call original)

use vo_analysis::objects::TypeKey;
use vo_common::symbol::Symbol;
use vo_runtime::SlotType;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

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
    let data_slot = builder.define_param(Symbol::DUMMY, 1, &[slot_type]);
    
    // Define other parameters (forwarded from original function)
    let mut wrapper_param_slots = Vec::new();
    for param in &func_decl.sig.params {
        let (slots, slot_types) = info.type_expr_layout(param.ty.id);
        for name in &param.names {
            let slot = builder.define_param(name.symbol, slots, &slot_types);
            wrapper_param_slots.push((slot, slots));
        }
    }
    
    // Get receiver value slots
    let recv_slots = info.type_slot_count(recv_type);
    let recv_slot_types = info.type_slot_types(recv_type);
    
    let ret_slots: u16 = func_decl.sig.results.iter()
        .map(|r| info.type_expr_layout(r.ty.id).0)
        .sum();
    
    // Allocate args area for call: recv_value + params
    let args_start = builder.alloc_temp_typed(&recv_slot_types);
    
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
            builder.alloc_temp(*slots);
            builder.emit_copy(dest_offset, *src_slot, *slots);
            dest_offset += *slots;
        }
    }
    
    // Call original function
    let forwarded_param_slots: u16 = wrapper_param_slots.iter().map(|(_, s)| *s).sum();
    let total_arg_slots = recv_slots + forwarded_param_slots;
    emit_call(&mut builder, original_func_id, args_start, total_arg_slots, ret_slots);
    
    // Return
    builder.set_ret_slots(ret_slots);
    builder.emit_op(Opcode::Return, args_start, ret_slots, 0);
    
    let func_def = builder.build();
    let wrapper_id = ctx.add_function(func_def);
    
    Ok(wrapper_id)
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
    
    // Compute embedding path info and final receiver type
    let (offset, current_type, has_pointer_embed) = 
        compute_embed_path(outer_type, embed_indices, tc_objs);
    
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
    let outer_gcref = builder.define_param(Symbol::DUMMY, 1, &[SlotType::GcRef]);
    
    // Define forwarded params
    let first_param_slot = if forwarded_param_slots > 0 {
        let slot = builder.define_param(Symbol::DUMMY, 1, &[SlotType::Value]);
        for _ in 1..forwarded_param_slots {
            builder.define_param(Symbol::DUMMY, 1, &[SlotType::Value]);
        }
        Some(slot)
    } else {
        None
    };
    
    // Allocate args area for call
    let total_arg_slots = recv_slots_for_call + forwarded_param_slots;
    let args_start = builder.alloc_temp(total_arg_slots);
    
    // Emit receiver loading based on embedding type
    emit_promoted_receiver(
        &mut builder,
        outer_gcref,
        args_start,
        offset,
        has_pointer_embed,
        is_pointer_receiver,
        recv_slots_for_call,
    );
    
    // Copy forwarded params
    if let Some(first_param) = first_param_slot {
        let params_dest = args_start + recv_slots_for_call;
        builder.emit_copy(params_dest, first_param, forwarded_param_slots);
    }
    
    // Call original method
    emit_call(&mut builder, original_func_id, args_start, total_arg_slots, ret_slots);
    
    // Return
    builder.set_ret_slots(ret_slots);
    builder.emit_op(Opcode::Return, args_start, ret_slots, 0);
    
    let func_def = builder.build();
    ctx.add_function(func_def)
}

// =============================================================================
// Helper functions
// =============================================================================

/// Compute embedding path info: total offset, final type, and whether any embed is pointer
fn compute_embed_path(
    outer_type: TypeKey,
    embed_indices: &[usize],
    tc_objs: &vo_analysis::objects::TCObjects,
) -> (u16, TypeKey, bool) {
    let mut offset = 0u16;
    let mut current_type = outer_type;
    let mut has_pointer_embed = false;
    
    for &idx in embed_indices {
        let (field_offset, _) = vo_analysis::check::type_info::struct_field_offset_by_index(
            current_type, idx, tc_objs
        );
        let field_type = vo_analysis::check::type_info::struct_field_type_by_index(
            current_type, idx, tc_objs
        );
        
        offset += field_offset;
        
        if vo_analysis::check::type_info::is_pointer(field_type, tc_objs) {
            has_pointer_embed = true;
            let underlying = vo_analysis::typ::underlying_type(field_type, tc_objs);
            if let vo_analysis::typ::Type::Pointer(p) = &tc_objs.types[underlying] {
                current_type = p.base();
            }
        } else {
            current_type = field_type;
        }
    }
    
    (offset, current_type, has_pointer_embed)
}

/// Emit receiver loading for promoted method wrapper
fn emit_promoted_receiver(
    builder: &mut FuncBuilder,
    outer_gcref: u16,
    args_start: u16,
    offset: u16,
    has_pointer_embed: bool,
    is_pointer_receiver: bool,
    recv_slots: u16,
) {
    if has_pointer_embed {
        // Pointer embedding: read embedded pointer, then dereference if value receiver
        let temp_ptr = builder.alloc_temp(1);
        builder.emit_ptr_get(temp_ptr, outer_gcref, offset, 1);
        
        if is_pointer_receiver {
            builder.emit_copy(args_start, temp_ptr, 1);
        } else {
            builder.emit_ptr_get(args_start, temp_ptr, 0, recv_slots);
        }
    } else {
        // Value embedding
        if is_pointer_receiver {
            builder.emit_copy(args_start, outer_gcref, 1);
        } else {
            builder.emit_ptr_get(args_start, outer_gcref, offset, recv_slots);
        }
    }
}

/// Emit a function call instruction
fn emit_call(builder: &mut FuncBuilder, func_id: u32, args_start: u16, arg_slots: u16, ret_slots: u16) {
    let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_id);
    builder.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
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
) -> u32 {
    let offset = embed_offset;
    
    // Get interface meta to find method index
    let iface_meta_id = ctx.get_or_create_interface_meta_id(iface_type, tc_objs);
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
    let params_tuple = sig.params();
    let results_tuple = sig.results();
    let param_slots: u16 = tc_objs.types[params_tuple].try_as_tuple()
        .map(|t| t.vars().iter()
            .map(|v| {
                let typ = tc_objs.lobjs[*v].typ().unwrap();
                vo_analysis::check::type_info::type_slot_count(typ, tc_objs)
            })
            .sum())
        .unwrap_or(0);
    let ret_slots: u16 = tc_objs.types[results_tuple].try_as_tuple()
        .map(|t| t.vars().iter()
            .map(|v| {
                let typ = tc_objs.lobjs[*v].typ().unwrap();
                vo_analysis::check::type_info::type_slot_count(typ, tc_objs)
            })
            .sum())
        .unwrap_or(0);
    
    // Build wrapper
    let wrapper_name = format!("{}$embed_iface", method_name);
    let mut builder = FuncBuilder::new(&wrapper_name);
    
    // Receiver: GcRef to outer struct
    builder.set_recv_slots(1);
    let outer_gcref = builder.define_param(Symbol::DUMMY, 1, &[SlotType::GcRef]);
    
    // Forward parameters
    let first_param_slot = if param_slots > 0 {
        let slot = builder.define_param(Symbol::DUMMY, 1, &[SlotType::Value]);
        for _ in 1..param_slots {
            builder.define_param(Symbol::DUMMY, 1, &[SlotType::Value]);
        }
        Some(slot)
    } else {
        None
    };
    
    // Load embedded interface (2 slots) from outer struct
    let iface_slot = builder.alloc_temp(2);
    builder.emit_ptr_get(iface_slot, outer_gcref, offset, 2);
    
    // Allocate args for CallIface (params only, receiver is separate)
    let args_start = builder.alloc_temp(param_slots.max(ret_slots).max(1));
    
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
