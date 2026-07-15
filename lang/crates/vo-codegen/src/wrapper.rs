#![allow(clippy::too_many_arguments)]
//! Wrapper function generation for defer/go and interface method dispatch.
//!
//! This module handles generating wrapper functions:
//! - Defer wrappers: For deferring extern calls and interface method calls
//! - `$iface` wrappers: For value receiver methods (unbox GcRef, call original)
//! - `$promoted` wrappers: For promoted methods through embedding (navigate path, call original)

use vo_analysis::objects::TypeKey;
use vo_runtime::bytecode::ReturnShape;
use vo_runtime::instruction::Opcode;
use vo_runtime::{RuntimeType, SlotType, ValueKind, ValueRttid};

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::{ElemLayoutSpec, FuncBuilder};
use crate::type_info::TypeInfoWrapper;

// =============================================================================
// Helper functions
// =============================================================================

/// Define forwarded parameters in a wrapper function.
/// Returns the first parameter slot if any parameters were defined.
fn define_forwarded_params(
    ctx: &mut CodegenContext,
    builder: &mut FuncBuilder,
    param_layouts: &[Vec<SlotType>],
) -> Option<u16> {
    let mut first_param = None;
    for layout in param_layouts {
        let slots = ctx.slot_count_u16_or_record(layout.len());
        let slot = builder
            .try_define_param(None, slots, layout)
            .unwrap_or_else(|error| {
                ctx.record_layout_error(error);
                0
            });
        if first_param.is_none() {
            first_param = Some(slot);
        }
    }
    first_param
}

fn split_param_layouts(
    param_types: &[vo_runtime::bytecode::TransferType],
    flat_slot_types: &[SlotType],
) -> Vec<Vec<SlotType>> {
    let mut layouts = Vec::with_capacity(param_types.len());
    let mut offset = 0usize;
    for transfer_type in param_types {
        let slots = transfer_type.slots as usize;
        let end = offset + slots;
        assert!(
            end <= flat_slot_types.len(),
            "param slot layout shorter than transfer layout"
        );
        layouts.push(flat_slot_types[offset..end].to_vec());
        offset = end;
    }
    assert!(
        offset == flat_slot_types.len(),
        "param slot layout length mismatch"
    );
    layouts
}

fn slot_layouts_from_type_keys(
    type_keys: &[TypeKey],
    info: &TypeInfoWrapper,
) -> Vec<Vec<SlotType>> {
    type_keys
        .iter()
        .map(|&type_key| info.type_slot_types(type_key))
        .collect()
}

fn flatten_param_layouts(param_layouts: &[Vec<SlotType>]) -> Vec<SlotType> {
    let mut slot_types = Vec::new();
    for layout in param_layouts {
        slot_types.extend_from_slice(layout);
    }
    slot_types
}

fn slot_layout_key(slot_types: &[SlotType]) -> String {
    if slot_types.is_empty() {
        return "void".to_string();
    }
    slot_types
        .iter()
        .map(|slot_type| (*slot_type as u8).to_string())
        .collect::<Vec<_>>()
        .join("_")
}

fn return_shape_key(returns: &ReturnShape) -> String {
    let layout = slot_layout_key(&returns.slot_types);
    if returns.interface_metas.is_empty() {
        return layout;
    }
    let metas = returns
        .interface_metas
        .iter()
        .map(|meta| meta.map_or_else(|| "n".to_string(), |id| id.to_string()))
        .collect::<Vec<_>>()
        .join("_");
    format!("{layout}_im{metas}")
}

fn param_layout_key(param_slot_types: &[Vec<SlotType>]) -> String {
    if param_slot_types.is_empty() {
        return "void".to_string();
    }
    param_slot_types
        .iter()
        .map(|layout| slot_layout_key(layout))
        .collect::<Vec<_>>()
        .join("__")
}

fn iface_data_slot_type(value_kind: ValueKind) -> SlotType {
    match value_kind {
        ValueKind::Array
        | ValueKind::Struct
        | ValueKind::String
        | ValueKind::Slice
        | ValueKind::Map
        | ValueKind::Channel
        | ValueKind::Closure
        | ValueKind::Pointer
        | ValueKind::Port
        | ValueKind::Island => SlotType::GcRef,
        ValueKind::Float32 | ValueKind::Float64 => SlotType::Float,
        _ => SlotType::Value,
    }
}

/// Compute total slot count for a tuple type (params or results).
fn tuple_slot_count(tuple_key: TypeKey, tc_objs: &vo_analysis::objects::TCObjects) -> usize {
    let tuple = tc_objs.types[tuple_key]
        .try_as_tuple()
        .expect("signature params/results must be tuple types during wrapper generation");
    tuple
        .vars()
        .iter()
        .map(|v| {
            let typ = tc_objs.lobjs[*v].typ().unwrap();
            usize::from(vo_analysis::check::type_info::type_slot_count(typ, tc_objs))
        })
        .sum()
}

/// Emit call and return - common wrapper epilogue.
/// With the new call buffer layout [Value×arg_slots | ret_slots], return values
/// live at args_start + arg_slots, not at args_start.
fn emit_call_and_return(
    ctx: &mut CodegenContext,
    builder: &mut FuncBuilder,
    func_id: u32,
    args_start: u16,
    arg_slots: u16,
    ret_slot_types: &[SlotType],
) {
    let ret_slots = ctx.slot_count_u16_or_record(ret_slot_types.len());
    builder.emit_static_call(func_id, args_start, arg_slots, ret_slots);
    builder.set_ret_slot_types(ret_slot_types.to_vec());
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
    let name = info
        .project
        .interner
        .resolve(func_decl.name.symbol)
        .unwrap_or("unknown");
    let wrapper_name = format!("{}$iface", name);
    let orig_param_types = ctx.module().functions[original_func_id as usize]
        .param_types
        .clone();

    let mut builder = FuncBuilder::new(&wrapper_name);

    // Check if receiver needs unboxing (struct/array are boxed in interface slot1)
    let recv_vk = info.type_value_kind(recv_type);
    let needs_unbox = recv_vk.needs_boxing();

    // Wrapper receives interface data slot as first parameter (1 slot)
    builder.set_recv_slots(1);
    let data_slot = builder.define_param(None, 1, &[iface_data_slot_type(recv_vk)]);

    // Define other parameters (forwarded from original function)
    // Note: for variadic param, TypeExpr is element type T, but actual param is []T
    let mut first_param_slot = None;
    let mut forwarded_param_layouts = Vec::new();
    let params = &func_decl.sig.params;
    for (i, param) in params.iter().enumerate() {
        let variadic_last = func_decl.sig.variadic && i == params.len() - 1;
        let (slots, slot_types) = if variadic_last {
            (1, vec![SlotType::GcRef])
        } else {
            info.type_expr_layout(param.ty.id)
        };
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
    builder.add_param_transfer_types(&orig_param_types);

    // Get receiver value slots
    let recv_slots = info.type_slot_count(recv_type);
    let recv_slot_types = info.type_slot_types(recv_type);

    let mut ret_slot_types = Vec::new();
    for result in &func_decl.sig.results {
        let (_, slot_types) = info.type_expr_layout(result.ty.id);
        ret_slot_types.extend(slot_types);
    }

    // Allocate call buffer: [Value×arg_slots | Value×ret_slots]
    // New layout keeps arg and ret regions separate to prevent GC from scanning
    // arg values as GcRefs when a timeslice boundary occurs mid-call.
    let forwarded_slot_types = flatten_param_layouts(&forwarded_param_layouts);
    let forwarded_param_slots = ctx.slot_count_u16_or_record(forwarded_slot_types.len());
    let total_arg_slots =
        ctx.slot_count_u16_or_record(usize::from(recv_slots) + forwarded_slot_types.len());
    let mut arg_slot_types = recv_slot_types;
    arg_slot_types.extend(forwarded_slot_types);
    let args_start = builder.alloc_call_buffer(&arg_slot_types, &ret_slot_types);

    if recv_vk == ValueKind::Array {
        // Interface arrays use the canonical ArrayHeader-backed object layout.
        // Reconstruct the value receiver's flattened slots element by element.
        let elem_type = info.array_elem_type(recv_type);
        let elem_slots = info.type_slot_count(elem_type);
        let elem_bytes = info.array_elem_bytes(recv_type);
        let elem_vk = info.type_value_kind(elem_type);
        let elem_slot_types = info.type_slot_types(elem_type);
        if elem_slots != 0 {
            let index_reg = builder.alloc_slots(&[SlotType::Value]);
            for index in 0..info.array_len(recv_type) {
                let index_constant = ctx.const_int(index as i64);
                builder.emit_op(Opcode::LoadConst, index_reg, index_constant, 0);
                let offset = u16::try_from(index)
                    .ok()
                    .and_then(|index| index.checked_mul(elem_slots))
                    .ok_or_else(|| {
                        CodegenError::Internal(format!(
                            "interface array receiver offset exceeds u16: index={index}, elem_slots={elem_slots}"
                        ))
                    })?;
                let elem_dst = args_start.checked_add(offset).ok_or_else(|| {
                    CodegenError::Internal(
                        "interface array receiver destination exceeds u16".to_string(),
                    )
                })?;
                builder.emit_array_get(
                    elem_dst,
                    data_slot,
                    index_reg,
                    ElemLayoutSpec::new(elem_bytes, elem_vk, &elem_slot_types),
                    ctx,
                );
            }
        }
    } else if needs_unbox {
        // Struct: dereference GcRef to get value
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
    emit_call_and_return(
        ctx,
        &mut builder,
        original_func_id,
        args_start,
        total_arg_slots,
        &ret_slot_types,
    );

    let wrapper_id = ctx.add_function_from_builder(builder);

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
    let orig_recv_slots = orig_func.recv_slots;
    let param_layouts = split_param_layouts(
        &orig_param_types,
        &orig_func.slot_types[orig_recv_slots as usize..orig_param_slots as usize],
    );
    let forwarded_slot_types = flatten_param_layouts(&param_layouts);
    let forwarded_param_slots = ctx.slot_count_u16_or_record(forwarded_slot_types.len());

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
    let first_param_slot = define_forwarded_params(ctx, &mut builder, &param_layouts);
    builder.add_param_transfer_types(&orig_param_types);

    // Allocate call buffer: [Value×arg_slots | Value×ret_slots]
    let total_arg_slots =
        ctx.slot_count_u16_or_record(usize::from(recv_slots_for_call) + forwarded_slot_types.len());
    let mut arg_slot_types = if expects_ptr_recv {
        vec![SlotType::GcRef]
    } else {
        info.type_slot_types(final_type)
    };
    arg_slot_types.extend_from_slice(&forwarded_slot_types);
    let ret_slot_types = flatten_param_layouts(&slot_layouts_from_type_keys(
        &info
            .method_result_types(method_obj)
            .expect("method result types missing"),
        info,
    ));
    let args_start = builder.alloc_call_buffer(&arg_slot_types, &ret_slot_types);

    // Emit receiver extraction based on path
    let start = crate::embed::TraverseStart::new(outer_recv, outer_is_pointer);
    crate::embed::emit_embed_path_traversal(
        &mut builder,
        start,
        &embed_path.steps,
        expects_ptr_recv,
        recv_slots_for_call,
        args_start,
    );

    // Copy forwarded params
    if let Some(first_param) = first_param_slot {
        let params_dest = args_start + recv_slots_for_call;
        builder.emit_copy(params_dest, first_param, forwarded_param_slots);
    }

    // Call and return
    emit_call_and_return(
        ctx,
        &mut builder,
        original_func_id,
        args_start,
        total_arg_slots,
        &ret_slot_types,
    );

    ctx.add_function_from_builder(builder)
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
    let orig_recv_slots = orig_func.recv_slots;
    let orig_param_types = orig_func.param_types.clone();
    let orig_slot_types = orig_func.slot_types.clone();
    let param_layouts = split_param_layouts(
        &orig_param_types,
        &orig_slot_types[orig_recv_slots as usize..orig_param_slots as usize],
    );
    let forwarded_slot_types = flatten_param_layouts(&param_layouts);
    let forwarded_param_slots = ctx.slot_count_u16_or_record(forwarded_slot_types.len());

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
    let outer_rttid = ctx.intern_type_key(outer_type, info);
    let outer_kind = vo_analysis::check::type_info::type_value_kind(outer_type, tc_objs);
    ctx.intern_rttid(RuntimeType::Pointer(ValueRttid::new(
        outer_rttid,
        outer_kind,
    )));

    // Define forwarded params
    let first_param_slot = define_forwarded_params(ctx, &mut builder, &param_layouts);
    builder.add_param_transfer_types(&orig_param_types);

    // Allocate call buffer: [Value×arg_slots | Value×ret_slots]
    let total_arg_slots =
        ctx.slot_count_u16_or_record(usize::from(recv_slots_for_call) + forwarded_slot_types.len());
    let mut arg_slot_types = if is_pointer_receiver {
        vec![SlotType::GcRef]
    } else {
        info.type_slot_types(current_type)
    };
    arg_slot_types.extend_from_slice(&forwarded_slot_types);
    let ret_slot_types = flatten_param_layouts(&slot_layouts_from_type_keys(
        &info
            .method_result_types(method_obj)
            .expect("method result types missing"),
        info,
    ));
    let args_start = builder.alloc_call_buffer(&arg_slot_types, &ret_slot_types);

    // Emit receiver loading based on embedding type
    // For promoted wrapper, outer is always GcRef (outer_is_pointer = true)
    let start = crate::embed::TraverseStart::new(outer_gcref, true);
    crate::embed::emit_embed_path_traversal(
        &mut builder,
        start,
        &path_info.steps,
        is_pointer_receiver,
        recv_slots_for_call,
        args_start,
    );

    // Copy forwarded params
    if let Some(first_param) = first_param_slot {
        let params_dest = args_start + recv_slots_for_call;
        builder.emit_copy(params_dest, first_param, forwarded_param_slots);
    }

    // Call and return
    emit_call_and_return(
        ctx,
        &mut builder,
        original_func_id,
        args_start,
        total_arg_slots,
        &ret_slot_types,
    );

    ctx.add_function_from_builder(builder)
}

// =============================================================================
// Embedded interface wrappers
// =============================================================================

/// Receiver type for embedded interface wrapper generation.
pub enum EmbedIfaceRecvType {
    /// GcRef to outer struct (for interface dispatch)
    GcRef,
    /// Value or pointer based on outer_is_pointer flag (for method expression)
    ValueOrPointer {
        outer_type: TypeKey,
        outer_is_pointer: bool,
    },
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
    let method_identity = tc_objs.lobjs[method_obj].id(tc_objs);
    let method_idx = iface_meta
        .method_names
        .iter()
        .position(|n| n == method_identity.as_ref())
        .expect("method must exist in embedded interface");
    let method_idx = ctx.call_iface_method_index_or_record(method_idx);

    // Get method signature
    let method_type = tc_objs.lobjs[method_obj]
        .typ()
        .expect("interface method must have type");
    let param_type_keys = info.func_param_types(method_type);
    let param_layouts = slot_layouts_from_type_keys(&param_type_keys, info);
    let forwarded_slot_types = flatten_param_layouts(&param_layouts);
    let ret_slot_types = flatten_param_layouts(&slot_layouts_from_type_keys(
        &info.func_result_types(method_type),
        info,
    ));
    let sig = tc_objs.types[method_type]
        .try_as_signature()
        .expect("method type must be signature");
    let param_slots_usize = tuple_slot_count(sig.params(), tc_objs);
    let ret_slots_usize = tuple_slot_count(sig.results(), tc_objs);
    let param_slots = ctx.slot_count_u16_or_record(param_slots_usize);
    let ret_slots = ctx.slot_count_u16_or_record(ret_slots_usize);

    // Build wrapper
    let wrapper_name = format!("{}{}", method_name, wrapper_suffix);
    let mut builder = FuncBuilder::new(&wrapper_name);

    // Define receiver based on type
    let (outer_recv, outer_is_pointer) = match recv_type {
        EmbedIfaceRecvType::GcRef => {
            builder.set_recv_slots(1);
            (builder.define_param(None, 1, &[SlotType::GcRef]), true)
        }
        EmbedIfaceRecvType::ValueOrPointer {
            outer_type,
            outer_is_pointer,
        } => {
            let slots = if outer_is_pointer {
                1
            } else {
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
    let first_param_slot = define_forwarded_params(ctx, &mut builder, &param_layouts);
    for &param_type_key in &param_type_keys {
        builder.add_param_type_key(param_type_key, ctx, info);
    }

    // Load embedded interface (2 slots)
    let iface_slot = builder.alloc_slots(&[
        vo_runtime::SlotType::Interface0,
        vo_runtime::SlotType::Interface1,
    ]);
    let start = crate::embed::TraverseStart::new(outer_recv, outer_is_pointer);
    crate::embed::emit_embed_path_traversal(
        &mut builder,
        start,
        &embed_path.steps,
        false,
        2,
        iface_slot,
    );

    // Allocate call buffer: [Value×param_slots | Value×ret_slots]
    let args_start = builder.alloc_dynamic_call_buffer(
        &[SlotType::Value],
        &forwarded_slot_types,
        &ret_slot_types,
    );
    if let Some(first_param) = first_param_slot {
        builder.emit_copy(args_start, first_param, param_slots);
    }

    // CallIface: result at args_start + param_slots (new call buffer layout)
    let c = ctx.dynamic_call_shape_or_record(forwarded_slot_types.len(), ret_slot_types.len());
    builder.emit_call_iface(
        iface_meta_id,
        method_idx,
        iface_slot,
        args_start,
        c,
        &forwarded_slot_types,
        &ret_slot_types,
    );
    let ret_start = args_start + param_slots;
    builder.set_ret_slot_types(ret_slot_types);
    builder.emit_op(Opcode::Return, ret_start, ret_slots, 0);

    ctx.add_function_from_builder(builder)
}

/// Generate wrapper that takes interface as first param and calls via CallIface.
/// Shared implementation for method expression and defer wrappers.
fn generate_iface_call_wrapper(
    ctx: &mut CodegenContext,
    iface_meta_id: u32,
    method_idx: usize,
    param_slot_types: Vec<Vec<SlotType>>,
    call_ret_slot_types: Vec<SlotType>,
    wrapper_name: &str,
    set_recv_slots: bool,
    return_call_results: bool,
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
    let first_param_slot = define_forwarded_params(ctx, &mut builder, &param_slot_types);
    let forwarded_slot_types = flatten_param_layouts(&param_slot_types);
    let param_slots = ctx.slot_count_u16_or_record(forwarded_slot_types.len());
    let call_ret_slots = ctx.slot_count_u16_or_record(call_ret_slot_types.len());

    // Allocate call buffer: [Value×param_slots | Value×ret_slots]
    let args_start = builder.alloc_dynamic_call_buffer(
        &[SlotType::Value],
        &forwarded_slot_types,
        &call_ret_slot_types,
    );
    if let Some(first_param) = first_param_slot {
        builder.emit_copy(args_start, first_param, param_slots);
    }

    // CallIface
    let c = ctx.dynamic_call_shape_or_record(forwarded_slot_types.len(), call_ret_slot_types.len());
    let method_idx = ctx.call_iface_method_index_or_record(method_idx);
    builder.emit_call_iface(
        iface_meta_id,
        method_idx,
        iface_slot,
        args_start,
        c,
        &forwarded_slot_types,
        &call_ret_slot_types,
    );

    if return_call_results {
        // Return: result at args_start + param_slots (new call buffer layout)
        let ret_start = args_start + param_slots;
        builder.set_ret_slot_types(call_ret_slot_types);
        builder.emit_op(Opcode::Return, ret_start, call_ret_slots, 0);
    } else {
        builder.set_ret_slots(0);
        builder.emit_op(Opcode::Return, 0, 0, 0);
    }

    ctx.register_wrapper_from_builder(wrapper_name, builder)
}

/// Generate wrapper for method expression on interface type (e.g., Reader.Read).
pub fn generate_method_expr_iface_wrapper(
    ctx: &mut CodegenContext,
    iface_type: TypeKey,
    method_idx: u32,
    param_slots: usize,
    ret_slots: usize,
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

    let first_param_slot = define_forwarded_params(ctx, &mut builder, &param_slot_types);
    for &param_type_key in &param_type_keys {
        builder.add_param_type_key(param_type_key, ctx, info);
    }

    let forwarded_slot_types = flatten_param_layouts(&param_slot_types);
    let computed_param_slots = ctx.slot_count_u16_or_record(forwarded_slot_types.len());
    let computed_ret_slots = ctx.slot_count_u16_or_record(ret_slot_types.len());
    assert_eq!(param_slots, forwarded_slot_types.len());
    assert_eq!(ret_slots, ret_slot_types.len());
    let args_start = builder.alloc_dynamic_call_buffer(
        &[SlotType::Value],
        &forwarded_slot_types,
        &ret_slot_types,
    );
    if let Some(first_param) = first_param_slot {
        builder.emit_copy(args_start, first_param, computed_param_slots);
    }

    let c = ctx.dynamic_call_shape_or_record(forwarded_slot_types.len(), ret_slot_types.len());
    let iface_meta_id = ctx.get_or_create_interface_meta_id(
        iface_type,
        &info.project.tc_objs,
        &info.project.interner,
    );
    let method_idx = ctx.call_iface_method_index_or_record(method_idx as usize);
    builder.emit_call_iface(
        iface_meta_id,
        method_idx,
        iface_slot,
        args_start,
        c,
        &forwarded_slot_types,
        &ret_slot_types,
    );

    let ret_start = args_start + computed_param_slots;
    builder.set_ret_slot_types(ret_slot_types);
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
        ctx,
        embed_path,
        iface_type,
        method_name,
        method_obj,
        EmbedIfaceRecvType::ValueOrPointer {
            outer_type,
            outer_is_pointer,
        },
        "$mexpr_iface",
        info,
        Some(recv_type),
        tc_objs,
        interner,
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
        ctx,
        embed_path,
        iface_type,
        method_name,
        method_obj,
        EmbedIfaceRecvType::GcRef,
        "$embed_iface",
        info,
        None,
        tc_objs,
        interner,
    )
}

// === Defer Wrappers ===

/// Generate a wrapper for deferring extern calls (e.g., `defer fmt.Println(...)`).
///
/// The wrapper takes the same argument slot layout as the deferred call site,
/// then calls the extern function with that exact ABI.
pub fn generate_defer_extern_wrapper(
    ctx: &mut CodegenContext,
    extern_name: &str,
    arg_slot_types: Vec<SlotType>,
    returns: ReturnShape,
) -> u32 {
    let arg_layout_key = slot_layout_key(&arg_slot_types);
    let ret_layout_key = return_shape_key(&returns);
    let wrapper_name = format!(
        "$defer_extern_{}_a{}_r{}",
        extern_name, arg_layout_key, ret_layout_key
    );

    if let Some(id) = ctx.get_wrapper(&wrapper_name) {
        return id;
    }

    let mut param_layout_error = false;
    let arg_slots = ctx.slot_count_u16_or_record(arg_slot_types.len());

    let mut builder = FuncBuilder::new(&wrapper_name);
    if !arg_slot_types.is_empty() {
        builder
            .try_define_param(None, arg_slots, &arg_slot_types)
            .unwrap_or_else(|error| {
                param_layout_error = true;
                ctx.record_layout_error(error);
                0
            });
    }
    builder.set_ret_slots(0);

    if param_layout_error {
        builder.emit_op(Opcode::Return, 0, 0, 0);
        return ctx.register_wrapper_from_builder(&wrapper_name, builder);
    }

    let ret_slot_types = returns.slot_types.clone();
    let ret_dst = if ret_slot_types.is_empty() {
        0
    } else {
        builder.alloc_slots(&ret_slot_types)
    };
    let param_kinds = crate::context::ext_slot_kinds_for_slot_types(&arg_slot_types);
    let extern_id =
        ctx.get_or_register_declared_extern_with_return_shape(extern_name, returns, param_kinds);
    builder.emit_call_extern(ret_dst, extern_id, 0, arg_slot_types.len(), &ret_slot_types);
    builder.emit_op(Opcode::Return, 0, 0, 0);

    ctx.register_wrapper_from_builder(&wrapper_name, builder)
}

/// Generate a stable function-value trampoline for a declared extern.
///
/// Direct extern calls use `CallExtern` at the source call site. Once an extern
/// function is stored, passed, returned, or put in a container, dynamic calls
/// need an ordinary function ID behind the closure value. This wrapper retains
/// the full source signature (including variadic slice ABI, transfer metadata,
/// precise GC slot layouts, and all return slots) and forwards to `CallExtern`.
pub fn generate_extern_value_wrapper(
    ctx: &mut CodegenContext,
    extern_name: &str,
    func_type: TypeKey,
    info: &TypeInfoWrapper,
) -> Result<u32, CodegenError> {
    let wrapper_name = format!("$extern_value_{}_t{}", extern_name, func_type.raw());
    if let Some(id) = ctx.get_wrapper(&wrapper_name) {
        return Ok(id);
    }

    let param_type_keys = info.func_param_types(func_type);
    let result_type_keys = info.func_result_types(func_type);
    let param_slot_types = slot_layouts_from_type_keys(&param_type_keys, info);
    let flat_param_slot_types = flatten_param_layouts(&param_slot_types);
    let returns = crate::expr::call::return_shape_for_type_keys(&result_type_keys, ctx, info)?;
    let ret_slot_types = returns.slot_types.clone();

    let mut builder = FuncBuilder::new(&wrapper_name);
    let first_param_slot = define_forwarded_params(ctx, &mut builder, &param_slot_types);
    for &param_type_key in &param_type_keys {
        builder.add_param_type_key(param_type_key, ctx, info);
    }
    builder.set_return_types(result_type_keys.clone());
    builder.set_ret_slot_types(ret_slot_types.clone());
    if result_type_keys
        .last()
        .is_some_and(|&type_key| info.is_error_type(type_key))
    {
        let mut error_offset = 0u16;
        for &type_key in &result_type_keys[..result_type_keys.len() - 1] {
            error_offset = error_offset
                .checked_add(
                    info.try_type_slot_count(type_key)
                        .map_err(CodegenError::Internal)?,
                )
                .ok_or_else(|| {
                    CodegenError::Internal(
                        "extern function-value return layout exceeds u16::MAX".to_string(),
                    )
                })?;
        }
        builder.set_error_ret_slot(i32::from(error_offset));
    }

    let ret_dst = if ret_slot_types.is_empty() {
        0
    } else {
        builder.alloc_slots(&ret_slot_types)
    };
    let param_kinds = crate::context::ext_slot_kinds_for_slot_types(&flat_param_slot_types);
    let extern_id =
        ctx.get_or_register_declared_extern_with_return_shape(extern_name, returns, param_kinds);
    builder.emit_call_extern(
        ret_dst,
        extern_id,
        first_param_slot.unwrap_or(0),
        flat_param_slot_types.len(),
        &ret_slot_types,
    );
    let ret_slots = ctx.slot_count_u16_or_record(ret_slot_types.len());
    builder.emit_op(Opcode::Return, ret_dst, ret_slots, 0);

    Ok(ctx.register_wrapper_from_builder(&wrapper_name, builder))
}

/// Generate a wrapper for defer on interface method call.
/// Uses shared generate_iface_call_wrapper implementation.
pub fn generate_defer_iface_wrapper(
    ctx: &mut CodegenContext,
    iface_meta_id: u32,
    method_name: &str,
    method_idx: usize,
    param_slot_types: Vec<Vec<SlotType>>,
    ret_slot_types: Vec<SlotType>,
) -> u32 {
    let wrapper_name = format!(
        "{}$defer_iface_{}_i{}_p{}_r{}",
        method_name,
        method_idx,
        iface_meta_id,
        param_layout_key(&param_slot_types),
        slot_layout_key(&ret_slot_types)
    );
    generate_iface_call_wrapper(
        ctx,
        iface_meta_id,
        method_idx,
        param_slot_types,
        ret_slot_types,
        &wrapper_name,
        false,
        false,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{ExtSlotKind, JitInstructionMetadata, ParamShape};

    fn return_shape(slot_types: Vec<SlotType>) -> ReturnShape {
        ReturnShape::try_with_slot_types(slot_types).expect("test return shape should be valid")
    }

    fn interface_return_shape(iface_meta_id: u32) -> ReturnShape {
        ReturnShape::try_with_slot_types_and_interface_metas(
            vec![SlotType::Interface0, SlotType::Interface1],
            vec![Some(iface_meta_id), None],
        )
        .expect("test return shape should be valid")
    }

    #[test]
    fn add_function_from_builder_records_layout_errors() {
        let mut ctx = CodegenContext::new("wrapper-builder-layout-error");
        let mut builder = FuncBuilder::new("wide-wrapper");
        let wide_layout = vec![SlotType::Value; usize::from(u16::MAX) + 1];
        let _ = builder.alloc_slots(&wide_layout);

        let _ = ctx.add_function_from_builder(builder);

        let err = ctx
            .check_layout_errors()
            .expect_err("builder layout errors must be recorded at registration");
        assert_eq!(err, "type slot count exceeds u16::MAX: 65536 slots");
    }

    #[test]
    fn wrapper_builders_use_context_registration_owner() {
        let source = include_str!("wrapper.rs")
            .split("#[cfg(test)]")
            .next()
            .expect("wrapper source should contain tests section");

        assert!(
            !source.contains("ctx.add_function(builder.build())"),
            "wrapper builders must not bypass context-owned layout error recording"
        );
        assert!(
            !source.contains("let func_def = builder.build()"),
            "wrapper builders must not build before context-owned registration"
        );
    }

    #[test]
    fn defer_iface_wrapper_preserves_wide_method_index_in_metadata() {
        let mut ctx = CodegenContext::new("call-iface-method-idx-width");
        let _ = generate_defer_iface_wrapper(&mut ctx, 0, "wide", 256, Vec::new(), Vec::new());
        ctx.check_layout_errors()
            .expect("CallIface metadata owns the full method index");
        assert!(ctx.module().functions.iter().any(|function| {
            function
                .code
                .iter()
                .zip(&function.jit_metadata)
                .any(|(inst, metadata)| {
                    inst.opcode() == Opcode::CallIface
                        && inst.flags == 0
                        && matches!(
                            metadata,
                            JitInstructionMetadata::CallIfaceLayout {
                                method_idx: 256,
                                ..
                            }
                        )
                })
        }));
    }

    #[test]
    fn iface_wrapper_records_aggregate_param_slot_width() {
        let mut ctx = CodegenContext::new("dynamic-call-aggregate-width");
        let param_slot_types = vec![vec![SlotType::Value; 40_000], vec![SlotType::Value; 40_000]];
        let _ = generate_defer_iface_wrapper(&mut ctx, 0, "wide", 0, param_slot_types, Vec::new());
        let err = ctx
            .check_layout_errors()
            .expect_err("aggregate dynamic call params should be recorded");
        assert!(err.contains("type slot count exceeds u16::MAX"));
    }

    #[test]
    fn defer_extern_wrapper_cache_key_includes_arg_count() {
        let mut ctx = CodegenContext::new("defer-extern-arity");

        let one_arg = generate_defer_extern_wrapper(
            &mut ctx,
            "vo_println",
            vec![SlotType::Interface0, SlotType::Interface1],
            return_shape(Vec::new()),
        );
        let two_args = generate_defer_extern_wrapper(
            &mut ctx,
            "vo_println",
            vec![
                SlotType::Interface0,
                SlotType::Interface1,
                SlotType::Interface0,
                SlotType::Interface1,
            ],
            return_shape(Vec::new()),
        );

        assert_ne!(one_arg, two_args);
        assert_eq!(ctx.module().functions[one_arg as usize].param_slots, 2);
        assert_eq!(ctx.module().functions[two_args as usize].param_slots, 4);
        let extern_def = ctx
            .module()
            .externs
            .iter()
            .find(|def| def.name == "vo_println")
            .expect("wrapper should register vo_println");
        assert_eq!(extern_def.params, ParamShape::CallSiteVariadic);
        assert!(
            extern_def.param_kinds.is_empty(),
            "builtin variadic externs must not capture one wrapper arity as the global ABI"
        );
        ctx.check_layout_errors()
            .expect("distinct arity wrappers should not record layout errors");
    }

    #[test]
    fn defer_extern_wrapper_preserves_wide_call_extern_arg_layout() {
        let mut ctx = CodegenContext::new("defer-extern-call-width");

        let _ = generate_defer_extern_wrapper(
            &mut ctx,
            "vo_println",
            vec![SlotType::Value; 256],
            return_shape(Vec::new()),
        );

        ctx.check_layout_errors()
            .expect("CallExtern metadata owns the full argument layout");
        assert!(ctx.module().functions.iter().any(|function| {
            function
                .code
                .iter()
                .zip(&function.jit_metadata)
                .any(|(inst, metadata)| {
                    inst.opcode() == Opcode::CallExtern
                        && inst.flags == 0
                        && matches!(
                            metadata,
                            JitInstructionMetadata::CallExternLayout { arg_layout, .. }
                                if arg_layout.len() == 256
                        )
                })
        }));
    }

    #[test]
    fn vm_defer_extern_wrapper_param_width_023_records_layout_error() {
        let mut ctx = CodegenContext::new("defer-extern-param-width");

        let _ = generate_defer_extern_wrapper(
            &mut ctx,
            "vo_println",
            vec![SlotType::Value; usize::from(u16::MAX) + 1],
            return_shape(Vec::new()),
        );

        let err = ctx
            .check_layout_errors()
            .expect_err("wide deferred extern wrapper params should be recorded");
        assert!(err.contains("type slot count exceeds u16::MAX"));
    }

    #[test]
    fn defer_extern_wrapper_uses_declared_extern_effect_manifest() {
        let mut ctx = CodegenContext::new("defer-extern-effects");
        let extern_name = vo_runtime::vo_extern_name!("os", "blocking_fileRead");

        let _ = generate_defer_extern_wrapper(
            &mut ctx,
            extern_name,
            vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
            ReturnShape::try_with_slot_types_and_interface_metas(
                vec![SlotType::Value, SlotType::Interface0, SlotType::Interface1],
                vec![None, Some(0), None],
            )
            .expect("test return shape should be valid"),
        );

        let extern_def = ctx
            .module()
            .externs
            .iter()
            .find(|def| def.name == extern_name)
            .expect("defer wrapper should register extern");
        assert_eq!(
            extern_def.allowed_effects,
            vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY
        );
    }

    #[test]
    fn defer_extern_wrapper_preserves_call_extern_param_abi_050() {
        let mut ctx = CodegenContext::new("defer-extern-param-abi");
        let extern_name = vo_runtime::vo_extern_name!("pkg", "F");

        let wrapper_id = generate_defer_extern_wrapper(
            &mut ctx,
            extern_name,
            vec![SlotType::GcRef],
            return_shape(Vec::new()),
        );

        let extern_def = ctx
            .module()
            .externs
            .iter()
            .find(|def| def.name == extern_name)
            .expect("defer wrapper should register extern");
        assert_eq!(
            extern_def.params,
            vo_runtime::bytecode::ParamShape::Exact { slots: 1 }
        );
        assert_eq!(extern_def.param_kinds, vec![ExtSlotKind::Bytes]);

        let wrapper = &ctx.module().functions[wrapper_id as usize];
        assert_eq!(wrapper.param_slots, 1);
        assert_eq!(wrapper.slot_types.first().copied(), Some(SlotType::GcRef));
        assert!(
            wrapper.jit_metadata.iter().any(|metadata| matches!(
                metadata,
                vo_common_core::JitInstructionMetadata::CallExternLayout { arg_layout, .. }
                    if arg_layout == &vec![SlotType::GcRef]
            )),
            "defer extern wrapper must call extern with the same precise argument layout it exposes"
        );
    }

    #[test]
    fn defer_extern_wrapper_cache_key_does_not_bypass_return_interface_metadata_060() {
        let mut ctx = CodegenContext::new("defer-extern-interface-return-shape");
        let extern_name = vo_runtime::vo_extern_name!("pkg", "F");

        let _ = generate_defer_extern_wrapper(
            &mut ctx,
            extern_name,
            Vec::new(),
            interface_return_shape(1),
        );
        let _ = generate_defer_extern_wrapper(
            &mut ctx,
            extern_name,
            Vec::new(),
            interface_return_shape(2),
        );
        assert_eq!(
            ctx.check_layout_errors().unwrap_err(),
            format!(
                "extern '{extern_name}' registered with incompatible return interface metadata"
            )
        );
    }
}
