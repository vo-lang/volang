//! Builtin function compilation (len, cap, make, append, etc.).

use vo_runtime::SlotType;
use vo_vm::instruction::Opcode;

use crate::context::CodegenContext;
use crate::error::CodegenError;
use crate::func::FuncBuilder;
use crate::type_info::{encode_i32, TypeInfoWrapper};

use super::{compile_expr, compile_expr_to};

/// Compile arguments as (value, value_kind) pairs for print/println/assert.
/// Returns args_start register.
fn compile_args_with_value_kind(
    args: &[vo_syntax::ast::Expr],
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<u16, CodegenError> {
    // Print args are always boxed to interface (2 slots each)
    let args_start = func.alloc_temp_typed(&vec![SlotType::Interface0, SlotType::Interface1].repeat(args.len()));
    for (i, arg) in args.iter().enumerate() {
        let slot = args_start + (i * 2) as u16;
        compile_expr_to(arg, slot, ctx, func, info)?;
        let arg_type = info.expr_type(arg.id);
        let vk = info.type_value_kind(arg_type) as u8 as i32;
        let (b, c) = encode_i32(vk);
        func.emit_op(Opcode::LoadInt, slot + 1, b, c);
    }
    Ok(args_start)
}

pub fn is_builtin(name: &str) -> bool {
    matches!(name, "len" | "cap" | "make" | "new" | "append" | "copy" | "delete" | "panic" | "recover" | "print" | "println" | "close" | "assert")
}

pub fn compile_builtin_call(
    expr: &vo_syntax::ast::Expr,
    name: &str,
    call: &vo_syntax::ast::CallExpr,
    dst: u16,
    ctx: &mut CodegenContext,
    func: &mut FuncBuilder,
    info: &TypeInfoWrapper,
) -> Result<(), CodegenError> {
    match name {
        "len" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Internal("len expects 1 argument".to_string()));
            }
            let arg_reg = compile_expr(&call.args[0], ctx, func, info)?;
            let arg_type = info.expr_type(call.args[0].id);
            
            // Check type: string, array, slice, map, channel
            if info.is_array(arg_type) {
                // Array: len is known at compile time
                let len = info.array_len(arg_type);
                let (b, c) = encode_i32(len as i32);
                func.emit_op(Opcode::LoadInt, dst, b, c);
            } else if info.is_string(arg_type) {
                func.emit_op(Opcode::StrLen, dst, arg_reg, 0);
            } else if info.is_map(arg_type) {
                func.emit_op(Opcode::MapLen, dst, arg_reg, 0);
            } else if info.is_chan(arg_type) {
                func.emit_op(Opcode::ChanLen, dst, arg_reg, 0);
            } else if info.is_slice(arg_type) {
                func.emit_op(Opcode::SliceLen, dst, arg_reg, 0);
            } else {
                // Default to SliceLen
                func.emit_op(Opcode::SliceLen, dst, arg_reg, 0);
            }
        }
        "cap" => {
            if call.args.len() != 1 {
                return Err(CodegenError::Internal("cap expects 1 argument".to_string()));
            }
            let arg_reg = compile_expr(&call.args[0], ctx, func, info)?;
            let arg_type = info.expr_type(call.args[0].id);
            
            if info.is_chan(arg_type) {
                func.emit_op(Opcode::ChanCap, dst, arg_reg, 0);
            } else {
                func.emit_op(Opcode::SliceCap, dst, arg_reg, 0);
            }
        }
        "print" | "println" => {
            let extern_name = if name == "println" { "vo_println" } else { "vo_print" };
            let extern_id = ctx.get_or_register_extern(extern_name);
            let args_start = compile_args_with_value_kind(&call.args, ctx, func, info)?;
            func.emit_with_flags(Opcode::CallExtern, (call.args.len() * 2) as u8, dst, extern_id as u16, args_start);
        }
        "panic" => {
            // Compile panic message
            if !call.args.is_empty() {
                let msg_reg = compile_expr(&call.args[0], ctx, func, info)?;
                func.emit_op(Opcode::Panic, msg_reg, 0, 0);
            } else {
                func.emit_op(Opcode::Panic, 0, 0, 0);
            }
        }
        "make" => {
            // make([]T, len) or make([]T, len, cap) or make(map[K]V) or make(chan T)
            // Use the call expression's type, not the first arg (which is a type expr)
            let type_key = info.expr_type(expr.id);
            
            if info.is_slice(type_key) {
                    // make([]T, len) or make([]T, len, cap)
                    let elem_bytes = info.slice_elem_bytes(type_key);
                    let elem_type = info.slice_elem_type(type_key);
                    let elem_slot_types = info.slice_elem_slot_types(type_key);
                    let elem_vk = info.type_value_kind(elem_type);
                    let elem_rttid = ctx.intern_type_key(elem_type, info);
                    let elem_meta_idx = ctx.get_or_create_value_meta_with_rttid(elem_rttid, &elem_slot_types, Some(elem_vk));
                    
                    // Load elem_meta into register
                    let meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
                    func.emit_op(Opcode::LoadConst, meta_reg, elem_meta_idx, 0);
                    
                    let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
                    // When flags=0 (elem_bytes > 63), put elem_bytes in c+2
                    let num_regs = if flags == 0 { 3 } else { 2 };
                    let len_cap_reg = func.alloc_temp_typed(&vec![SlotType::Value; num_regs]);
                    
                    if call.args.len() > 1 {
                        compile_expr_to(&call.args[1], len_cap_reg, ctx, func, info)?;
                    } else {
                        func.emit_op(Opcode::LoadInt, len_cap_reg, 0, 0);
                    }
                    if call.args.len() > 2 {
                        compile_expr_to(&call.args[2], len_cap_reg + 1, ctx, func, info)?;
                    } else {
                        // cap = len
                        func.emit_op(Opcode::Copy, len_cap_reg + 1, len_cap_reg, 0);
                    }
                    if flags == 0 {
                        // Store elem_bytes in c+2 for dynamic case (use LoadConst so JIT can read from const table)
                        let elem_bytes_idx = ctx.const_int(elem_bytes as i64);
                        func.emit_op(Opcode::LoadConst, len_cap_reg + 2, elem_bytes_idx, 0);
                    }
                    
                    // SliceNew: a=dst, b=elem_meta, c=len_cap_start, flags=elem_flags
                    func.emit_with_flags(Opcode::SliceNew, flags, dst, meta_reg, len_cap_reg);
                } else if info.is_map(type_key) {
                    // make(map[K]V)
                    let (key_meta_idx, val_meta_idx, key_slots, val_slots) = ctx.get_or_create_map_metas(type_key, info);
                    
                    // Pack key_meta and val_meta: (key_meta << 32) | val_meta
                    let key_meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
                    let val_meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
                    func.emit_op(Opcode::LoadConst, key_meta_reg, key_meta_idx, 0);
                    func.emit_op(Opcode::LoadConst, val_meta_reg, val_meta_idx, 0);
                    
                    let packed_reg = func.alloc_temp_typed(&[SlotType::Value]);
                    let shift_reg = func.alloc_temp_typed(&[SlotType::Value]);
                    func.emit_op(Opcode::LoadInt, shift_reg, 32, 0);
                    func.emit_op(Opcode::Shl, packed_reg, key_meta_reg, shift_reg);
                    func.emit_op(Opcode::Or, packed_reg, packed_reg, val_meta_reg);
                    
                    let slots_arg = crate::type_info::encode_map_new_slots(key_slots, val_slots);
                    func.emit_op(Opcode::MapNew, dst, packed_reg, slots_arg);
                } else if info.is_chan(type_key) {
                    // make(chan T) or make(chan T, cap)
                    // ChanNew: a=dst, b=elem_meta, c=cap, flags=elem_slots
                    let elem_type_key = info.chan_elem_type(type_key);
                    let elem_slots = info.type_slot_count(elem_type_key);
                    let elem_slot_types = info.type_slot_types(elem_type_key);
                    let elem_vk = info.type_value_kind(elem_type_key);
                    let elem_rttid = ctx.intern_type_key(elem_type_key, info);
                    let elem_meta_idx = ctx.get_or_create_value_meta_with_rttid(elem_rttid, &elem_slot_types, Some(elem_vk));
                    
                    let elem_meta_reg = func.alloc_temp_typed(&[SlotType::Value]);
                    func.emit_op(Opcode::LoadConst, elem_meta_reg, elem_meta_idx, 0);
                    
                    let cap_reg = if call.args.len() > 1 {
                        compile_expr(&call.args[1], ctx, func, info)?
                    } else {
                        let tmp = func.alloc_temp_typed(&[SlotType::Value]);
                        func.emit_op(Opcode::LoadInt, tmp, 0, 0);
                        tmp
                    };
                    func.emit_with_flags(Opcode::ChanNew, elem_slots as u8, dst, elem_meta_reg, cap_reg);
            } else {
                return Err(CodegenError::UnsupportedExpr("make with unsupported type".to_string()));
            }
        }
        "new" => {
            // new(T) - allocate zero value of T on heap
            // Use the call expression's type (pointer to T), not the first arg
            let ptr_type_key = info.expr_type(expr.id);
            let type_key = info.pointer_elem(ptr_type_key);
            let slots = info.type_slot_count(type_key);
            // PtrNew: a=dst, b=0 (zero init), flags=slots
            func.emit_with_flags(Opcode::PtrNew, slots as u8, dst, 0, 0);
        }
        "append" => {
            // append(slice, elem...) - variadic, supports multiple elements
            // append(slice, other...) - spread: append all elements from other slice
            if call.args.len() < 2 {
                return Err(CodegenError::Internal("append requires at least 2 args".to_string()));
            }
            let slice_reg = compile_expr(&call.args[0], ctx, func, info)?;
            
            let slice_type = info.expr_type(call.args[0].id);
            let elem_bytes = info.slice_elem_bytes(slice_type);
            let elem_type = info.slice_elem_type(slice_type);
            let elem_slot_types = info.type_slot_types(elem_type);
            let elem_vk = info.type_value_kind(elem_type);
            
            // Get elem_meta as raw u32 value
            let elem_rttid = ctx.intern_type_key(elem_type, info);
            let elem_meta_idx = ctx.get_or_create_value_meta_with_rttid(elem_rttid, &elem_slot_types, Some(elem_vk));
            
            // Check for spread: append(a, b...)
            if call.spread && call.args.len() == 2 {
                // Spread append: append all elements from second slice
                let other_slice_reg = compile_expr(&call.args[1], ctx, func, info)?;
                let extern_id = ctx.get_or_register_extern("vo_slice_append_slice");
                let args_reg = func.alloc_temp_typed(&[SlotType::GcRef, SlotType::GcRef, SlotType::Value]);
                func.emit_op(Opcode::Copy, args_reg, slice_reg, 0);
                func.emit_op(Opcode::Copy, args_reg + 1, other_slice_reg, 0);
                func.emit_op(Opcode::LoadConst, args_reg + 2, elem_meta_idx, 0);
                func.emit_with_flags(Opcode::CallExtern, 3, dst, extern_id as u16, args_reg);
            } else {
                let flags = vo_common_core::elem_flags(elem_bytes, elem_vk);
                // SliceAppend: a=dst, b=slice, c=meta_and_elem, flags=elem_flags
                // When flags!=0: c=[elem_meta], c+1..=[elem]
                // When flags==0: c=[elem_meta], c+1=[elem_bytes], c+2..=[elem]
                let extra_slot = if flags == 0 { 1 } else { 0 };
                let mut meta_elem_slot_types = vec![SlotType::Value; 1 + extra_slot as usize];
                meta_elem_slot_types.extend(elem_slot_types.iter().cloned());
                let meta_and_elem_reg = func.alloc_temp_typed(&meta_elem_slot_types);
                
                // Current slice (updated after each append)
                let mut current_slice = slice_reg;
                
                // Append each element (args[1], args[2], ...)
                for (i, arg) in call.args.iter().skip(1).enumerate() {
                    let is_last = i == call.args.len() - 2;
                    let append_dst = if is_last { dst } else { func.alloc_temp_typed(&[SlotType::GcRef]) };
                    
                    func.emit_op(Opcode::LoadConst, meta_and_elem_reg, elem_meta_idx, 0);
                    if flags == 0 {
                        let elem_bytes_idx = ctx.const_int(elem_bytes as i64);
                        func.emit_op(Opcode::LoadConst, meta_and_elem_reg + 1, elem_bytes_idx, 0);
                        crate::stmt::compile_value_to(arg, meta_and_elem_reg + 2, elem_type, ctx, func, info)?;
                    } else {
                        crate::stmt::compile_value_to(arg, meta_and_elem_reg + 1, elem_type, ctx, func, info)?;
                    }
                    
                    func.emit_with_flags(Opcode::SliceAppend, flags, append_dst, current_slice, meta_and_elem_reg);
                    current_slice = append_dst;
                }
            }
        }
        "copy" => {
            // copy(dst, src) - use extern for now
            let extern_id = ctx.get_or_register_extern("vo_copy");
            let args_start = func.alloc_temp_typed(&[SlotType::GcRef, SlotType::GcRef]);
            compile_expr_to(&call.args[0], args_start, ctx, func, info)?;
            compile_expr_to(&call.args[1], args_start + 1, ctx, func, info)?;
            func.emit_with_flags(Opcode::CallExtern, 2, dst, extern_id as u16, args_start);
        }
        "delete" => {
            // delete(map, key)
            if call.args.len() != 2 {
                return Err(CodegenError::Internal("delete requires 2 args".to_string()));
            }
            let map_reg = compile_expr(&call.args[0], ctx, func, info)?;
            let key_reg = compile_expr(&call.args[1], ctx, func, info)?;
            
            // MapDelete expects: a=map, b=meta_and_key
            // meta = key_slots, key at b+1
            let map_type = info.expr_type(call.args[0].id);
            let (key_slots, _) = info.map_key_val_slots(map_type);
            
            let mut delete_slot_types = vec![SlotType::Value]; // meta
            let (key_type, _) = info.map_key_val_types(map_type);
            delete_slot_types.extend(info.type_slot_types(key_type)); // key
            let meta_and_key_reg = func.alloc_temp_typed(&delete_slot_types);
            let meta_idx = ctx.const_int(key_slots as i64);
            func.emit_op(Opcode::LoadConst, meta_and_key_reg, meta_idx, 0);
            func.emit_copy(meta_and_key_reg + 1, key_reg, key_slots);
            
            func.emit_op(Opcode::MapDelete, map_reg, meta_and_key_reg, 0);
        }
        "close" => {
            // close(chan)
            if call.args.len() != 1 {
                return Err(CodegenError::Internal("close requires 1 arg".to_string()));
            }
            let chan_reg = compile_expr(&call.args[0], ctx, func, info)?;
            func.emit_op(Opcode::ChanClose, chan_reg, 0, 0);
        }
        "recover" => {
            // recover() - returns interface{}
            // Recover: a=dst
            func.emit_op(Opcode::Recover, dst, 0, 0);
        }
        "assert" => {
            if call.args.is_empty() {
                return Err(CodegenError::Internal("assert requires at least 1 argument".to_string()));
            }
            let extern_id = ctx.get_or_register_extern("vo_assert");
            let args_start = compile_args_with_value_kind(&call.args, ctx, func, info)?;
            
            // Record debug info for assert (may cause panic)
            let pc = func.current_pc() as u32;
            ctx.record_debug_loc(pc, expr.span, &info.project.source_map);
            
            func.emit_with_flags(Opcode::CallExtern, (call.args.len() * 2) as u8, dst, extern_id as u16, args_start);
        }
        _ => {
            return Err(CodegenError::UnsupportedExpr(format!("builtin {}", name)));
        }
    }
    
    Ok(())
}
