#![allow(clippy::result_large_err, unused_imports)]
//! Shared instruction translation logic.

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    types, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, Value,
};

use vo_runtime::bytecode::Constant;
use vo_runtime::instruction::{Instruction, Opcode, QUEUE_KIND_PORT_FLAG};

use crate::translator::{
    emit_funcref_call, emit_funcref_call_with_effect, HelperCallEffect, IrEmitter, TranslateResult,
};
use crate::JitError;

mod collections;
mod conversions;
mod memory;
mod runtime_ops;
mod scalar;

use self::collections::*;
use self::conversions::*;
use self::memory::*;
use self::runtime_ops::*;
use self::scalar::*;

/// Translate a single instruction.
pub fn translate_inst<'a>(
    e: &mut impl IrEmitter<'a>,
    inst: &Instruction,
) -> Result<TranslateResult, JitError> {
    use Opcode::*;
    use TranslateResult::*;

    match inst.opcode() {
        Hint => Ok(Completed),
        LoadInt => {
            load_int(e, inst);
            Ok(Completed)
        }
        LoadConst => {
            load_const(e, inst);
            Ok(Completed)
        }
        Copy => {
            copy(e, inst);
            Ok(Completed)
        }
        CopyN => {
            copy_n(e, inst);
            Ok(Completed)
        }
        AddI => {
            add_i(e, inst);
            Ok(Completed)
        }
        SubI => {
            sub_i(e, inst);
            Ok(Completed)
        }
        MulI => {
            mul_i(e, inst);
            Ok(Completed)
        }
        DivI => {
            div_i(e, inst);
            Ok(Completed)
        }
        DivU => {
            div_u(e, inst);
            Ok(Completed)
        }
        ModI => {
            mod_i(e, inst);
            Ok(Completed)
        }
        ModU => {
            mod_u(e, inst);
            Ok(Completed)
        }
        NegI => {
            neg_i(e, inst);
            Ok(Completed)
        }
        AddF => {
            add_f(e, inst);
            Ok(Completed)
        }
        SubF => {
            sub_f(e, inst);
            Ok(Completed)
        }
        MulF => {
            mul_f(e, inst);
            Ok(Completed)
        }
        DivF => {
            div_f(e, inst);
            Ok(Completed)
        }
        NegF => {
            neg_f(e, inst);
            Ok(Completed)
        }
        EqI => {
            cmp_i(e, inst, IntCC::Equal);
            Ok(Completed)
        }
        NeI => {
            cmp_i(e, inst, IntCC::NotEqual);
            Ok(Completed)
        }
        LtI => {
            cmp_i(e, inst, IntCC::SignedLessThan);
            Ok(Completed)
        }
        LeI => {
            cmp_i(e, inst, IntCC::SignedLessThanOrEqual);
            Ok(Completed)
        }
        GtI => {
            cmp_i(e, inst, IntCC::SignedGreaterThan);
            Ok(Completed)
        }
        GeI => {
            cmp_i(e, inst, IntCC::SignedGreaterThanOrEqual);
            Ok(Completed)
        }
        LtU => {
            cmp_i(e, inst, IntCC::UnsignedLessThan);
            Ok(Completed)
        }
        LeU => {
            cmp_i(e, inst, IntCC::UnsignedLessThanOrEqual);
            Ok(Completed)
        }
        GtU => {
            cmp_i(e, inst, IntCC::UnsignedGreaterThan);
            Ok(Completed)
        }
        GeU => {
            cmp_i(e, inst, IntCC::UnsignedGreaterThanOrEqual);
            Ok(Completed)
        }
        EqF => {
            cmp_f(e, inst, FloatCC::Equal);
            Ok(Completed)
        }
        NeF => {
            cmp_f(e, inst, FloatCC::NotEqual);
            Ok(Completed)
        }
        LtF => {
            cmp_f(e, inst, FloatCC::LessThan);
            Ok(Completed)
        }
        LeF => {
            cmp_f(e, inst, FloatCC::LessThanOrEqual);
            Ok(Completed)
        }
        GtF => {
            cmp_f(e, inst, FloatCC::GreaterThan);
            Ok(Completed)
        }
        GeF => {
            cmp_f(e, inst, FloatCC::GreaterThanOrEqual);
            Ok(Completed)
        }
        Not => {
            bitwise_not(e, inst);
            Ok(Completed)
        }
        BoolNot => {
            bool_not(e, inst);
            Ok(Completed)
        }
        And => {
            and(e, inst);
            Ok(Completed)
        }
        Or => {
            or(e, inst);
            Ok(Completed)
        }
        Xor => {
            xor(e, inst);
            Ok(Completed)
        }
        AndNot => {
            and_not(e, inst);
            Ok(Completed)
        }
        Shl => {
            shl(e, inst);
            Ok(Completed)
        }
        ShrS => {
            shr_s(e, inst);
            Ok(Completed)
        }
        ShrU => {
            shr_u(e, inst);
            Ok(Completed)
        }
        GlobalGet => {
            global_get(e, inst);
            Ok(Completed)
        }
        GlobalSet => {
            global_set(e, inst);
            Ok(Completed)
        }
        GlobalGetN => {
            global_get_n(e, inst);
            Ok(Completed)
        }
        GlobalSetN => {
            global_set_n(e, inst);
            Ok(Completed)
        }
        PtrGet => {
            ptr_get(e, inst);
            Ok(Completed)
        }
        PtrSet => {
            ptr_set(e, inst);
            Ok(Completed)
        }
        PtrGetN => {
            ptr_get_n(e, inst);
            Ok(Completed)
        }
        PtrSetN => {
            ptr_set_n(e, inst);
            Ok(Completed)
        }
        PtrAdd => {
            ptr_add(e, inst);
            Ok(Completed)
        }
        SlotGet => {
            slot_get(e, inst);
            Ok(Completed)
        }
        SlotSet => {
            slot_set(e, inst);
            Ok(Completed)
        }
        SlotGetN => {
            slot_get_n(e, inst);
            Ok(Completed)
        }
        SlotSetN => {
            slot_set_n(e, inst);
            Ok(Completed)
        }
        ConvI2F => {
            conv_i2f(e, inst);
            Ok(Completed)
        }
        ConvF2I => {
            conv_f2i(e, inst);
            Ok(Completed)
        }
        ConvF64F32 => {
            conv_f64_f32(e, inst);
            Ok(Completed)
        }
        ConvF32F64 => {
            conv_f32_f64(e, inst);
            Ok(Completed)
        }
        Trunc => {
            trunc(e, inst);
            Ok(Completed)
        }
        IndexCheck => {
            index_check(e, inst);
            Ok(Completed)
        }
        // Slice operations
        SliceNew => {
            slice_new(e, inst);
            Ok(Completed)
        }
        SliceGet => {
            slice_get(e, inst)?;
            Ok(Completed)
        }
        SliceSet => {
            slice_set(e, inst)?;
            Ok(Completed)
        }
        SliceLen => {
            slice_len(e, inst);
            Ok(Completed)
        }
        SliceCap => {
            slice_cap(e, inst);
            Ok(Completed)
        }
        SliceSlice => {
            slice_slice(e, inst);
            Ok(Completed)
        }
        SliceAppend => {
            slice_append(e, inst);
            Ok(Completed)
        }
        SliceAddr => {
            slice_addr(e, inst);
            Ok(Completed)
        }
        // Array operations
        ArrayNew => {
            array_new(e, inst);
            Ok(Completed)
        }
        ArrayGet => {
            array_get(e, inst)?;
            Ok(Completed)
        }
        ArraySet => {
            array_set(e, inst)?;
            Ok(Completed)
        }
        ArrayAddr => {
            array_addr(e, inst);
            Ok(Completed)
        }
        // String operations
        StrLen => {
            str_len(e, inst);
            Ok(Completed)
        }
        StrIndex => {
            str_index(e, inst);
            Ok(Completed)
        }
        StrConcat => {
            str_concat(e, inst);
            Ok(Completed)
        }
        StrSlice => {
            str_slice(e, inst);
            Ok(Completed)
        }
        StrEq => {
            str_eq(e, inst);
            Ok(Completed)
        }
        StrNe => {
            str_ne(e, inst);
            Ok(Completed)
        }
        StrLt => {
            str_cmp(e, inst, IntCC::SignedLessThan);
            Ok(Completed)
        }
        StrLe => {
            str_cmp(e, inst, IntCC::SignedLessThanOrEqual);
            Ok(Completed)
        }
        StrGt => {
            str_cmp(e, inst, IntCC::SignedGreaterThan);
            Ok(Completed)
        }
        StrGe => {
            str_cmp(e, inst, IntCC::SignedGreaterThanOrEqual);
            Ok(Completed)
        }
        StrDecodeRune => {
            str_decode_rune(e, inst);
            Ok(Completed)
        }
        // Map operations
        MapNew => {
            map_new(e, inst);
            Ok(Completed)
        }
        MapLen => {
            map_len(e, inst);
            Ok(Completed)
        }
        MapGet => {
            map_get(e, inst)?;
            Ok(Completed)
        }
        MapSet => {
            map_set(e, inst)?;
            Ok(Completed)
        }
        MapDelete => {
            map_delete(e, inst)?;
            Ok(Completed)
        }
        MapIterInit => {
            map_iter_init(e, inst);
            Ok(Completed)
        }
        MapIterNext => {
            map_iter_next(e, inst);
            Ok(Completed)
        }
        // Closure operations
        ClosureNew => {
            closure_new(e, inst);
            Ok(Completed)
        }
        ClosureGet => {
            closure_get(e, inst);
            Ok(Completed)
        }
        // Allocation
        PtrNew => {
            ptr_new(e, inst);
            Ok(Completed)
        }
        QueueNew => {
            queue_new(e, inst);
            Ok(Completed)
        }
        QueueLen => {
            queue_len(e, inst);
            Ok(Completed)
        }
        QueueCap => {
            queue_cap(e, inst);
            Ok(Completed)
        }
        // Island/Channel
        IslandNew => {
            island_new(e, inst);
            Ok(Completed)
        }
        QueueClose => {
            queue_close(e, inst)?;
            Ok(Completed)
        }
        QueueSend => {
            queue_send(e, inst)?;
            Ok(Completed)
        }
        QueueRecv => {
            queue_recv(e, inst)?;
            Ok(Completed)
        }
        // Goroutine Start
        GoStart => {
            go_start(e, inst);
            Ok(Completed)
        }
        GoIsland => {
            go_island(e, inst);
            Ok(Completed)
        }
        // Defer/Recover
        DeferPush => {
            defer_push(e, inst, false);
            Ok(Completed)
        }
        ErrDeferPush => {
            defer_push(e, inst, true);
            Ok(Completed)
        }
        Recover => {
            recover(e, inst);
            Ok(Completed)
        }
        // Select Statement
        SelectBegin => {
            select_begin(e, inst)?;
            Ok(Completed)
        }
        SelectSend => {
            select_send(e, inst);
            Ok(Completed)
        }
        SelectRecv => {
            select_recv(e, inst);
            Ok(Completed)
        }
        SelectExec => {
            select_exec(e, inst)?;
            Ok(Completed)
        }
        // Interface
        IfaceAssert => {
            iface_assert(e, inst);
            Ok(Completed)
        }
        StrNew => {
            str_new(e, inst);
            Ok(Completed)
        }
        IfaceAssign => {
            iface_assign(e, inst);
            Ok(Completed)
        }
        IfaceEq => {
            iface_eq(e, inst);
            Ok(Completed)
        }
        // ForLoop - handled by loop compiler
        ForLoop => Ok(Unhandled),
        // Control flow - compiler specific
        Jump | JumpIf | JumpIfNot | Return | Panic => Ok(Unhandled),
        // Function calls - compiler specific
        Call | CallExtern | CallClosure | CallIface => Ok(Unhandled),
        // Unsupported
        _ => Ok(Unhandled),
    }
}

/// Emit runtime panic (nil pointer, bounds check, division by zero, etc).
/// Sets panic_flag=true but NOT is_user_panic - VM will create default error message.
fn emit_panic_if<'a>(e: &mut impl IrEmitter<'a>, condition: Value) {
    let panic_block = e.builder().create_block();
    let ok_block = e.builder().create_block();
    e.builder()
        .ins()
        .brif(condition, panic_block, &[], ok_block, &[]);

    e.builder().switch_to_block(panic_block);
    e.builder().seal_block(panic_block);

    // Runtime errors: just set panic_flag, don't call vo_panic
    // (vo_panic sets is_user_panic=true which would prevent default message creation)
    let ctx = e.ctx_param();
    let panic_flag_offset =
        std::mem::offset_of!(vo_runtime::jit_api::JitContext, panic_flag) as i32;
    let panic_flag_ptr =
        e.builder()
            .ins()
            .load(types::I64, MemFlags::trusted(), ctx, panic_flag_offset);
    let true_val = e.builder().ins().iconst(types::I8, 1);
    e.builder()
        .ins()
        .store(MemFlags::trusted(), true_val, panic_flag_ptr, 0);

    let panic_ret_val = e.panic_return_value();
    let panic_ret = e.builder().ins().iconst(types::I32, panic_ret_val as i64);
    e.builder().ins().return_(&[panic_ret]);

    e.builder().switch_to_block(ok_block);
    e.builder().seal_block(ok_block);
}

/// Emit nil check for pointer. Panics if ptr is nil.
fn emit_nil_ptr_check<'a>(e: &mut impl IrEmitter<'a>, ptr: Value) {
    let zero = e.builder().ins().iconst(types::I64, 0);
    let is_nil = e.builder().ins().icmp(IntCC::Equal, ptr, zero);
    emit_panic_if(e, is_nil);
}

/// Emit nil check for pointer with slot tracking.
/// Skips the check if the slot has already been verified non-nil.
fn emit_nil_ptr_check_for_slot<'a>(e: &mut impl IrEmitter<'a>, ptr_slot: u16, ptr: Value) {
    if e.is_checked_non_nil(ptr_slot) {
        return; // Already verified non-nil in this basic block
    }
    emit_nil_ptr_check(e, ptr);
    e.mark_checked_non_nil(ptr_slot);
}

// =============================================================================
// ForLoop Helpers
// =============================================================================

/// Emit ForLoop step: idx += step; return (next_idx, continue_condition)
///
/// Shared by FuncCompiler and LoopCompiler.
/// flags: bit0=unsigned, bit1=decrement, bit2=inclusive
pub fn emit_forloop_step(
    builder: &mut cranelift_frontend::FunctionBuilder,
    idx: Value,
    limit: Value,
    is_decrement: bool,
    is_unsigned: bool,
    is_inclusive: bool,
) -> (Value, Value) {
    let one = builder.ins().iconst(types::I64, 1);
    let next_idx = if is_decrement {
        builder.ins().isub(idx, one)
    } else {
        builder.ins().iadd(idx, one)
    };

    let cc = match (is_decrement, is_unsigned, is_inclusive) {
        // Increment exclusive: i < limit
        (false, false, false) => IntCC::SignedLessThan,
        (false, true, false) => IntCC::UnsignedLessThan,
        // Increment inclusive: i <= limit
        (false, false, true) => IntCC::SignedLessThanOrEqual,
        (false, true, true) => IntCC::UnsignedLessThanOrEqual,
        // Decrement exclusive: i > limit
        (true, false, false) => IntCC::SignedGreaterThan,
        (true, true, false) => IntCC::UnsignedGreaterThan,
        // Decrement inclusive: i >= limit
        (true, false, true) => IntCC::SignedGreaterThanOrEqual,
        (true, true, true) => IntCC::UnsignedGreaterThanOrEqual,
    };
    let continue_loop = builder.ins().icmp(cc, next_idx, limit);

    (next_idx, continue_loop)
}
