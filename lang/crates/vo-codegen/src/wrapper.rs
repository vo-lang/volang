//! Wrapper function generation for defer/go and interface method dispatch.
//!
//! This module handles generating wrapper functions:
//! - Defer wrappers: For deferring extern calls and interface method calls
//! - `$iface` wrappers: For value receiver methods (unbox GcRef, call original)
//! - `$promoted` wrappers: For promoted methods through embedding (navigate path, call original)

use vo_analysis::objects::TypeKey;
use vo_runtime::SlotType;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::TypeInfoWrapper;

// =============================================================================
// Helper functions
// =============================================================================

/// Define forwarded parameters in a wrapper function.
/// Returns the first parameter slot if any parameters were defined.
fn define_forwarded_params(builder: &mut FuncBuilder, param_layouts: &[Vec<SlotType>]) -> Option<u16> {
    let mut first_param = None;
    for layout in param_layouts {
        let slot = builder.define_param(None, layout.len() as u16, layout);
        if first_param.is_none() {
            first_param = Some(slot);
        }
    }
    first_param
}

fn split_param_layouts(
    param_types: &[vo_vm::bytecode::TransferType],
    flat_slot_types: &[SlotType],
) -> Vec<Vec<SlotType>> {
    let mut layouts = Vec::with_capacity(param_types.len());
    let mut offset = 0usize;
    for transfer_type in param_types {
        let slots = transfer_type.slots as usize;
        let end = offset + slots;
        assert!(end <= flat_slot_types.len(), "param slot layout shorter than transfer layout");
        layouts.push(flat_slot_types[offset..end].to_vec());
        offset = end;
    }
    assert!(offset == flat_slot_types.len(), "param slot layout length mismatch");
    layouts
}

fn slot_layouts_from_type_keys(type_keys: &[TypeKey], info: &TypeInfoWrapper) -> Vec<Vec<SlotType>> {
    type_keys.iter().map(|&type_key| info.type_slot_types(type_key)).collect()
}

fn flatten_param_layouts(param_layouts: &[Vec<SlotType>]) -> Vec<SlotType> {
    let mut slot_types = Vec::new();
    for layout in param_layouts {
        slot_types.extend_from_slice(layout);
    }
    slot_types
}

/// Compute total slot count for a tuple type (params or results).
fn tuple_slot_count(tuple_key: TypeKey, tc_objs: &vo_analysis::objects::TCObjects) -> u16 {
    tc_objs.types[tuple_key].try_as_tuple()
        .map(|t| t.vars().iter()
            .map(|v| {
                let typ = tc_objs.lobjs[*v].typ().unwrap();
                vo_analysis::check::type_info::type_slot_count(typ, tc_objs)
            })
            .sum())
        .unwrap_or(0)
}

/// Emit a function call instruction.
fn emit_call(builder: &mut FuncBuilder, func_id: u32, args_start: u16, arg_slots: u16, ret_slots: u16) {
    let c = crate::type_info::encode_call_args(arg_slots, ret_slots);
    let (func_id_low, func_id_high) = crate::type_info::encode_func_id(func_id);
    builder.emit_with_flags(Opcode::Call, func_id_high, func_id_low, args_start, c);
}

/// Emit call and return - common wrapper epilogue.
/// With the new call buffer layout [Value×arg_slots | ret_slots], return values
/// live at args_start + arg_slots, not at args_start.
fn emit_call_and_return(builder: &mut FuncBuilder, func_id: u32, args_start: u16, arg_slots: u16, ret_slots: u16) {
    emit_call(builder, func_id, args_start, arg_slots, ret_slots);
    builder.set_ret_slots(ret_slots);
    let ret_start = args_start + arg_slots;
    builder.emit_op(Opcode::Return, ret_start, ret_slots, 0);
}

// =============================================================================
// Interface value receiver wrapper
// =============================================================================

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
    let mut first_param_slot = None;
    let mut forwarded_param_layouts = Vec::new();
    let params = &func_decl.sig.params;
    for (i, param) in params.iter().enumerate() {
        let variadic_last = func_decl.sig.variadic && i == params.len() - 1;
        let (slots, slot_types) = if variadic_last { (1, vec![SlotType::GcRef]) } else { info.type_expr_layout(param.ty.id) };
        if param.names.is_empty() {
            let slot = builder.define_param(None, slots, &slot_types);
            if first_param_slot.is_none() {
                first_param_slot = Some(slot);
            }
            forwarded_param_layouts.push(slot_types.clone());
            continue;
        }
        for name in &param.names {
            let slot = builder.define_param(Some(name.symbol), slots, &slot_types);
            if first_param_slot.is_none() {
                first_param_slot = Some(slot);
            }
            forwarded_param_layouts.push(slot_types.clone());
        }
    }
    
    // Get receiver value slots
    let recv_slots = info.type_slot_count(recv_type);
    let recv_slot_types = info.type_slot_types(recv_type);
    
    let mut ret_slots = 0u16;
    let mut ret_slot_types = Vec::new();
    for result in &func_decl.sig.results {
        let (slots, slot_types) = info.type_expr_layout(result.ty.id);
        ret_slots += slots;
        ret_slot_types.extend(slot_types);
    }
    
    // Allocate call buffer: [Value×arg_slots | Value×ret_slots]
    // New layout keeps arg and ret regions separate to prevent GC from scanning
    // arg values as GcRefs when a timeslice boundary occurs mid-call.
    let forwarded_param_slots = flatten_param_layouts(&forwarded_param_layouts).len() as u16;
    let total_arg_slots = recv_slots + forwarded_param_slots;
    let mut arg_slot_types = recv_slot_types;
    arg_slot_types.extend(flatten_param_layouts(&forwarded_param_layouts));
    let args_start = builder.alloc_call_buffer(&arg_slot_types, &ret_slot_types);
    
    if needs_unbox {
        // Struct/Array: dereference GcRef to get value
        builder.emit_ptr_get(args_start, data_slot, 0, recv_slots);
    } else {
        // Basic types: slot1 is the value directly, just copy
        builder.emit_copy(args_start, data_slot, recv_slots);
    }
    
    // Copy other parameters after receiver value
    if let Some(first_param) = first_param_slot {
        builder.emit_copy(args_start + recv_slots, first_param, forwarded_param_slots);
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
    recv_type: TypeKey,
    outer_type: TypeKey,
    embed_path: &crate::embed::EmbedPathInfo,
    method_obj: vo_analysis::objects::ObjKey,
    original_func_id: u32,
    expects_ptr_recv: bool,
    outer_is_pointer: bool,
    method_name: &str,
    info: &TypeInfoWrapper,
    tc_objs: &vo_analysis::objects::TCObjects,
) -> u32 {
    let orig_func = &ctx.module().functions[original_func_id as usize];
    let orig_param_slots = orig_func.param_slots;
    let orig_param_types = orig_func.param_types.clone();
    let ret_slots = orig_func.ret_slots;
    let orig_recv_slots = orig_func.recv_slots;
    let param_layouts = split_param_layouts(
        &orig_param_types,
        &orig_func.slot_types[orig_recv_slots as usize..orig_param_slots as usize],
    );
    let forwarded_slot_types = flatten_param_layouts(&param_layouts);
    let forwarded_param_slots = forwarded_slot_types.len() as u16;
    
    let final_type = embed_path.final_type;
    let recv_slots_for_call = if expects_ptr_recv {
        1u16
    } else {
        vo_analysis::check::type_info::type_slot_count(final_type, tc_objs)
    };
    
    let wrapper_name = format!("{}$mexpr", method_name);
    let mut builder = FuncBuilder::new(&wrapper_name);
    
    // outer_is_pointer is now passed as parameter
    // For (*T).M, outer is pointer (GcRef)
    // For T.M (even if path has pointer steps), outer is value type
    // NOTE: has_pointer_step does NOT mean outer is pointer - it just means the path
    // contains embedded pointer fields that need to be dereferenced
    
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
        builder.define_param(None, outer_recv_slots, &info.type_slot_types(recv_type))
    };
    builder.add_param_type_key(recv_type, ctx, info);
    
    // Define forwarded params
    let first_param_slot = define_forwarded_params(&mut builder, &param_layouts);
    builder.add_param_transfer_types(&orig_param_types);
    
    // Allocate call buffer: [Value×arg_slots | Value×ret_slots]
    let total_arg_slots = recv_slots_for_call + forwarded_param_slots;
    let mut arg_slot_types = if expects_ptr_recv {
        vec![SlotType::GcRef]
    } else {
        info.type_slot_types(final_type)
    };
    arg_slot_types.extend_from_slice(&forwarded_slot_types);
    let ret_slot_types = flatten_param_layouts(&slot_layouts_from_type_keys(
        &info.method_result_types(method_obj).expect("method result types missing"),
        info,
    ));
    let args_start = builder.alloc_call_buffer(&arg_slot_types, &ret_slot_types);
    
    // Emit receiver extraction based on path
    let start = crate::embed::TraverseStart::new(outer_recv, outer_is_pointer);
    crate::embed::emit_embed_path_traversal(&mut builder, start, &embed_path.steps, expects_ptr_recv, recv_slots_for_call, args_start);
    
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
    method_obj: vo_analysis::objects::ObjKey,
    original_func_id: u32,
    iface_func_id: u32,
    method_name: &str,
    info: &TypeInfoWrapper,
    tc_objs: &vo_analysis::objects::TCObjects,
) -> u32 {
    // Get the original function's signature
    let orig_func = &ctx.module().functions[original_func_id as usize];
    let orig_param_slots = orig_func.param_slots;
    let ret_slots = orig_func.ret_slots;
    let orig_recv_slots = orig_func.recv_slots;
    let param_layouts = split_param_layouts(
        &orig_func.param_types,
        &orig_func.slot_types[orig_recv_slots as usize..orig_param_slots as usize],
    );
    let forwarded_slot_types = flatten_param_layouts(&param_layouts);
    let forwarded_param_slots = forwarded_slot_types.len() as u16;
    
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
    let first_param_slot = define_forwarded_params(&mut builder, &param_layouts);
    builder.add_param_transfer_types(&orig_func.param_types);
    
    // Allocate call buffer: [Value×arg_slots | Value×ret_slots]
    let total_arg_slots = recv_slots_for_call + forwarded_param_slots;
    let mut arg_slot_types = if is_pointer_receiver {
        vec![SlotType::GcRef]
    } else {
        info.type_slot_types(current_type)
    };
    arg_slot_types.extend_from_slice(&forwarded_slot_types);
    let ret_slot_types = flatten_param_layouts(&slot_layouts_from_type_keys(
        &info.method_result_types(method_obj).expect("method result types missing"),
        info,
    ));
    let args_start = builder.alloc_call_buffer(&arg_slot_types, &ret_slot_types);
    
    // Emit receiver loading based on embedding type
    // For promoted wrapper, outer is always GcRef (outer_is_pointer = true)
    let start = crate::embed::TraverseStart::new(outer_gcref, true);
    crate::embed::emit_embed_path_traversal(&mut builder, start, &path_info.steps, is_pointer_receiver, recv_slots_for_call, args_start);
    
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
// Embedded interface wrappers
// =============================================================================

/// Receiver type for embedded interface wrapper generation.
pub enum EmbedIfaceRecvType {
    /// GcRef to outer struct (for interface dispatch)
    GcRef,
    /// Value or pointer based on outer_is_pointer flag (for method expression)
    ValueOrPointer { outer_type: TypeKey, outer_is_pointer: bool },
}

/// Generate wrapper for embedded interface method dispatch.
/// 
/// Unified function for both:
/// - Interface dispatch: receiver is always GcRef
/// - Method expression: receiver is value or pointer
/// 
/// The wrapper:
/// 1. Receives outer struct (GcRef, value, or pointer)
/// 2. Extracts embedded interface (2 slots) via path traversal
/// 3. Calls the method via CallIface
fn generate_embedded_iface_wrapper_impl(
    ctx: &mut CodegenContext,
    embed_path: &crate::embed::EmbedPathInfo,
    iface_type: TypeKey,
    method_name: &str,
    method_obj: vo_analysis::objects::ObjKey,
    recv_type: EmbedIfaceRecvType,
    wrapper_suffix: &str,
    info: &TypeInfoWrapper,
    method_expr_recv_type: Option<TypeKey>,
    tc_objs: &vo_analysis::objects::TCObjects,
    interner: &vo_common::SymbolInterner,
) -> u32 {
    // Get interface meta to find method index
    let iface_meta_id = ctx.get_or_create_interface_meta_id(iface_type, tc_objs, interner);
    let iface_meta = &ctx.module().interface_metas[iface_meta_id as usize];
    let method_idx = iface_meta.method_names.iter()
        .position(|n| n == method_name)
        .expect("method must exist in embedded interface");
    
    // Get method signature
    let method_type = tc_objs.lobjs[method_obj].typ().expect("interface method must have type");
    let param_type_keys = info.func_param_types(method_type);
    let param_layouts = slot_layouts_from_type_keys(&param_type_keys, info);
    let forwarded_slot_types = flatten_param_layouts(&param_layouts);
    let ret_slot_types = flatten_param_layouts(&slot_layouts_from_type_keys(
        &info.func_result_types(method_type),
        info,
    ));
    let sig = tc_objs.types[method_type].try_as_signature().expect("method type must be signature");
    let param_slots = tuple_slot_count(sig.params(), tc_objs);
    let ret_slots = tuple_slot_count(sig.results(), tc_objs);
    
    // Build wrapper
    let wrapper_name = format!("{}{}", method_name, wrapper_suffix);
    let mut builder = FuncBuilder::new(&wrapper_name);
    
    // Define receiver based on type
    let (outer_recv, outer_is_pointer) = match recv_type {
        EmbedIfaceRecvType::GcRef => {
            builder.set_recv_slots(1);
            (builder.define_param(None, 1, &[SlotType::GcRef]), true)
        }
        EmbedIfaceRecvType::ValueOrPointer { outer_type, outer_is_pointer } => {
            let slots = if outer_is_pointer { 1 } else {
                vo_analysis::check::type_info::type_slot_count(outer_type, tc_objs)
            };
            builder.set_recv_slots(slots);
            let recv = if outer_is_pointer {
                builder.define_param(None, 1, &[SlotType::GcRef])
            } else {
                builder.define_param(
                    None,
                    slots,
                    &info.type_slot_types(method_expr_recv_type.unwrap_or(outer_type)),
                )
            };
            (recv, outer_is_pointer)
        }
    };
    if let Some(recv_type_key) = method_expr_recv_type {
        builder.add_param_type_key(recv_type_key, ctx, info);
    }
    
    // Forward parameters
    let first_param_slot = define_forwarded_params(&mut builder, &param_layouts);
    for &param_type_key in &param_type_keys {
        builder.add_param_type_key(param_type_key, ctx, info);
    }
    
    // Load embedded interface (2 slots)
    let iface_slot = builder.alloc_slots(&[vo_runtime::SlotType::Interface0, vo_runtime::SlotType::Interface1]);
    let start = crate::embed::TraverseStart::new(outer_recv, outer_is_pointer);
    crate::embed::emit_embed_path_traversal(&mut builder, start, &embed_path.steps, false, 2, iface_slot);
    
    // Allocate call buffer: [Value×param_slots | Value×ret_slots]
    let args_start = builder.alloc_dynamic_call_buffer(&[SlotType::Value], &forwarded_slot_types, &ret_slot_types);
    if let Some(first_param) = first_param_slot {
        builder.emit_copy(args_start, first_param, param_slots);
    }
    
    // CallIface: result at args_start + param_slots (new call buffer layout)
    let c = crate::type_info::encode_call_args(param_slots, ret_slots);
    builder.emit_with_flags(Opcode::CallIface, method_idx as u8, iface_slot, args_start, c);
    let ret_start = args_start + param_slots;
    builder.set_ret_slots(ret_slots);
    builder.emit_op(Opcode::Return, ret_start, ret_slots, 0);
    
    ctx.add_function(builder.build())
}

/// Generate wrapper that takes interface as first param and calls via CallIface.
/// Shared implementation for method expression and defer wrappers.
fn generate_iface_call_wrapper(
    ctx: &mut CodegenContext,
    method_idx: u32,
    param_slot_types: Vec<Vec<SlotType>>,
    ret_slot_types: Vec<SlotType>,
    wrapper_name: &str,
    set_recv_slots: bool,
) -> u32 {
    if let Some(id) = ctx.get_wrapper(wrapper_name) {
        return id;
    }
    
    let mut builder = FuncBuilder::new(wrapper_name);
    
    // First parameter: interface value (2 slots)
    if set_recv_slots {
        builder.set_recv_slots(2);
    }
    let iface_slot = builder.define_param(None, 2, &[SlotType::Interface0, SlotType::Interface1]);
    
    // Forward other parameters
    let first_param_slot = define_forwarded_params(&mut builder, &param_slot_types);
    let param_slots = flatten_param_layouts(&param_slot_types).len() as u16;
    let ret_slots = ret_slot_types.len() as u16;
    
    // Allocate call buffer: [Value×param_slots | Value×ret_slots]
    let args_start = builder.alloc_dynamic_call_buffer(&[SlotType::Value], &flatten_param_layouts(&param_slot_types), &ret_slot_types);
    if let Some(first_param) = first_param_slot {
        builder.emit_copy(args_start, first_param, param_slots);
    }
    
    // CallIface
    let c = crate::type_info::encode_call_args(param_slots, ret_slots);
    builder.emit_with_flags(Opcode::CallIface, method_idx as u8, iface_slot, args_start, c);
    
    // Return: result at args_start + param_slots (new call buffer layout)
    let ret_start = args_start + param_slots;
    builder.set_ret_slots(ret_slots);
    builder.emit_op(Opcode::Return, ret_start, ret_slots, 0);
    
    ctx.register_wrapper_from_builder(wrapper_name, builder)
}

/// Generate wrapper for method expression on interface type (e.g., Reader.Read).
pub fn generate_method_expr_iface_wrapper(
    ctx: &mut CodegenContext,
    iface_type: TypeKey,
    method_idx: u32,
    param_slots: u16,
    ret_slots: u16,
    method_name: &str,
    info: &TypeInfoWrapper,
) -> u32 {
    let wrapper_name = format!(
        "{}$mexpr_iface_{}_t{}",
        method_name,
        method_idx,
        iface_type.raw(),
    );
    if let Some(id) = ctx.get_wrapper(&wrapper_name) {
        return id;
    }

    let (param_type_keys, _) = info.get_interface_method_signature(iface_type, method_name);
    let param_slot_types = slot_layouts_from_type_keys(&param_type_keys, info);
    let ret_slot_types = flatten_param_layouts(&slot_layouts_from_type_keys(
        &info.get_interface_method_result_types(iface_type, method_name),
        info,
    ));
    let mut builder = FuncBuilder::new(&wrapper_name);
    builder.set_recv_slots(2);
    let iface_slot = builder.define_param(None, 2, &[SlotType::Interface0, SlotType::Interface1]);
    builder.add_param_type_key(iface_type, ctx, info);

    let first_param_slot = define_forwarded_params(&mut builder, &param_slot_types);
    for &param_type_key in &param_type_keys {
        builder.add_param_type_key(param_type_key, ctx, info);
    }

    let computed_param_slots = flatten_param_layouts(&param_slot_types).len() as u16;
    let computed_ret_slots = ret_slot_types.len() as u16;
    debug_assert_eq!(param_slots, computed_param_slots);
    debug_assert_eq!(ret_slots, computed_ret_slots);
    let args_start = builder.alloc_dynamic_call_buffer(&[SlotType::Value], &flatten_param_layouts(&param_slot_types), &ret_slot_types);
    if let Some(first_param) = first_param_slot {
        builder.emit_copy(args_start, first_param, computed_param_slots);
    }

    let c = crate::type_info::encode_call_args(computed_param_slots, computed_ret_slots);
    builder.emit_with_flags(Opcode::CallIface, method_idx as u8, iface_slot, args_start, c);

    let ret_start = args_start + computed_param_slots;
    builder.set_ret_slots(computed_ret_slots);
    builder.emit_op(Opcode::Return, ret_start, computed_ret_slots, 0);

    ctx.register_wrapper_from_builder(&wrapper_name, builder)
}

/// Generate wrapper for method expression on embedded interface.
pub fn generate_method_expr_embedded_iface_wrapper(
    ctx: &mut CodegenContext,
    recv_type: TypeKey,
    outer_type: TypeKey,
    embed_path: &crate::embed::EmbedPathInfo,
    iface_type: TypeKey,
    method_name: &str,
    method_obj: vo_analysis::objects::ObjKey,
    outer_is_pointer: bool,
    info: &TypeInfoWrapper,
    tc_objs: &vo_analysis::objects::TCObjects,
    interner: &vo_common::SymbolInterner,
) -> u32 {
    generate_embedded_iface_wrapper_impl(
        ctx, embed_path, iface_type, method_name, method_obj,
        EmbedIfaceRecvType::ValueOrPointer { outer_type, outer_is_pointer },
        "$mexpr_iface", info, Some(recv_type), tc_objs, interner,
    )
}

/// Generate wrapper for embedded interface method through interface dispatch.
pub fn generate_embedded_iface_wrapper(
    ctx: &mut CodegenContext,
    embed_path: &crate::embed::EmbedPathInfo,
    iface_type: TypeKey,
    method_name: &str,
    method_obj: vo_analysis::objects::ObjKey,
    info: &TypeInfoWrapper,
    tc_objs: &vo_analysis::objects::TCObjects,
    interner: &vo_common::SymbolInterner,
) -> u32 {
    generate_embedded_iface_wrapper_impl(
        ctx, embed_path, iface_type, method_name, method_obj,
        EmbedIfaceRecvType::GcRef,
        "$embed_iface", info, None, tc_objs, interner,
    )
}

// === Defer Wrappers ===

/// Generate a wrapper for deferring extern calls (e.g., `defer fmt.Println(...)`).
///
/// The wrapper takes N interface values as parameters (each 2 slots),
/// then calls the extern function.
pub fn generate_defer_extern_wrapper(
    ctx: &mut CodegenContext,
    extern_name: &str,
    arg_count: usize,
    ret_slots: u16,
) -> u32 {
    let wrapper_name = format!("$defer_extern_{}", extern_name);
    
    if let Some(id) = ctx.get_wrapper(&wrapper_name) {
        return id;
    }
    
    let arg_slots = (arg_count * 2) as u16; // each arg is interface (2 slots)
    
    let mut builder = FuncBuilder::new(&wrapper_name);
    for _ in 0..arg_count {
        builder.define_param(None, 2, &[SlotType::Interface0, SlotType::Interface1]);
    }
    builder.set_ret_slots(0);
    
    let extern_id = ctx.get_or_register_extern_with_ret_slots(extern_name, ret_slots);
    // CallExtern: flags=arg_count*2, a=dst, b=extern_id, c=args_start
    builder.emit_with_flags(Opcode::CallExtern, arg_slots as u8, 0, extern_id as u16, 0);
    builder.emit_op(Opcode::Return, 0, 0, 0);
    
    ctx.register_wrapper_from_builder(&wrapper_name, builder)
}

/// Generate a wrapper for defer on interface method call.
/// Uses shared generate_iface_call_wrapper implementation.
pub fn generate_defer_iface_wrapper(
    ctx: &mut CodegenContext,
    method_name: &str,
    method_idx: usize,
    param_slot_types: Vec<Vec<SlotType>>,
) -> u32 {
    let wrapper_name = format!("{}$defer_iface_{}", method_name, method_idx);
    generate_iface_call_wrapper(ctx, method_idx as u32, param_slot_types, Vec::new(), &wrapper_name, false)
}
