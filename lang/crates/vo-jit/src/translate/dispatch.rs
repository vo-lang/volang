use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use vo_runtime::instruction::{Instruction, Opcode};

#[cfg(test)]
use crate::semantics::LoweringOwner;
use crate::translator::{IrEmitter, TranslateResult};
use crate::JitError;

use super::collections::{
    array_addr, array_get, array_new, array_set, map_delete, map_get, map_iter_init, map_iter_next,
    map_len, map_new, map_set, slice_addr, slice_append, slice_cap, slice_get, slice_len,
    slice_new, slice_set, slice_slice, str_cmp, str_concat, str_decode_rune, str_eq, str_index,
    str_len, str_ne, str_slice,
};
use super::conversions::{conv_f2i, conv_f32_f64, conv_f64_f32, conv_i2f, index_check, trunc};
use super::memory::{
    global_get, global_get_n, global_set, global_set_n, ptr_add, ptr_get, ptr_get_n, ptr_set,
    ptr_set_n, slot_get, slot_get_n, slot_set, slot_set_n,
};
use super::runtime_ops::{
    closure_get, closure_new, defer_push, go_island, go_start, iface_assert, iface_assign,
    iface_eq, island_new, ptr_new, queue_cap, queue_close, queue_len, queue_new, queue_recv,
    queue_send, recover, select_begin, select_exec, select_recv, select_send, str_new,
};
use super::scalar::{
    add_f, add_i, and, and_not, bitwise_not, bool_not, cmp_f, cmp_i, copy, copy_n, div_f, div_i,
    div_u, load_const, load_int, mod_i, mod_u, mul_f, mul_i, neg_f, neg_i, or, shl, shr_s, shr_u,
    sub_f, sub_i, xor,
};

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
            load_const(e, inst)?;
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
            ptr_set(e, inst)?;
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
        SliceNew => {
            slice_new(e, inst)?;
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
            slice_slice(e, inst)?;
            Ok(Completed)
        }
        SliceAppend => {
            slice_append(e, inst)?;
            Ok(Completed)
        }
        SliceAddr => {
            slice_addr(e, inst)?;
            Ok(Completed)
        }
        ArrayNew => {
            array_new(e, inst)?;
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
            array_addr(e, inst)?;
            Ok(Completed)
        }
        StrLen => {
            str_len(e, inst);
            Ok(Completed)
        }
        StrIndex => {
            str_index(e, inst)?;
            Ok(Completed)
        }
        StrConcat => {
            str_concat(e, inst)?;
            Ok(Completed)
        }
        StrSlice => {
            str_slice(e, inst)?;
            Ok(Completed)
        }
        StrEq => {
            str_eq(e, inst)?;
            Ok(Completed)
        }
        StrNe => {
            str_ne(e, inst)?;
            Ok(Completed)
        }
        StrLt => {
            str_cmp(e, inst, IntCC::SignedLessThan)?;
            Ok(Completed)
        }
        StrLe => {
            str_cmp(e, inst, IntCC::SignedLessThanOrEqual)?;
            Ok(Completed)
        }
        StrGt => {
            str_cmp(e, inst, IntCC::SignedGreaterThan)?;
            Ok(Completed)
        }
        StrGe => {
            str_cmp(e, inst, IntCC::SignedGreaterThanOrEqual)?;
            Ok(Completed)
        }
        StrDecodeRune => {
            str_decode_rune(e, inst)?;
            Ok(Completed)
        }
        MapNew => {
            map_new(e, inst)?;
            Ok(Completed)
        }
        MapLen => {
            map_len(e, inst)?;
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
            map_iter_init(e, inst)?;
            Ok(Completed)
        }
        MapIterNext => {
            map_iter_next(e, inst)?;
            Ok(Completed)
        }
        ClosureNew => {
            closure_new(e, inst)?;
            Ok(Completed)
        }
        ClosureGet => {
            closure_get(e, inst);
            Ok(Completed)
        }
        PtrNew => {
            ptr_new(e, inst)?;
            Ok(Completed)
        }
        QueueNew => {
            queue_new(e, inst)?;
            Ok(Completed)
        }
        QueueLen => {
            queue_len(e, inst)?;
            Ok(Completed)
        }
        QueueCap => {
            queue_cap(e, inst)?;
            Ok(Completed)
        }
        IslandNew => {
            island_new(e, inst)?;
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
        GoStart => {
            go_start(e, inst)?;
            Ok(Completed)
        }
        GoIsland => {
            go_island(e, inst)?;
            Ok(Completed)
        }
        DeferPush => {
            defer_push(e, inst, false)?;
            Ok(Completed)
        }
        ErrDeferPush => {
            defer_push(e, inst, true)?;
            Ok(Completed)
        }
        Recover => {
            recover(e, inst)?;
            Ok(Completed)
        }
        SelectBegin => {
            select_begin(e, inst)?;
            Ok(Completed)
        }
        SelectSend => {
            select_send(e, inst)?;
            Ok(Completed)
        }
        SelectRecv => {
            select_recv(e, inst)?;
            Ok(Completed)
        }
        SelectExec => {
            select_exec(e, inst)?;
            Ok(Completed)
        }
        IfaceAssert => {
            iface_assert(e, inst)?;
            Ok(Completed)
        }
        StrNew => {
            str_new(e, inst)?;
            Ok(Completed)
        }
        IfaceAssign => {
            iface_assign(e, inst)?;
            Ok(Completed)
        }
        IfaceEq => {
            iface_eq(e, inst)?;
            Ok(Completed)
        }
        opcode if is_compiler_specific_opcode(opcode) => Ok(Unhandled),
        Invalid => Err(JitError::UnsupportedOpcode(Invalid)),
        opcode => Err(JitError::UnsupportedOpcode(opcode)),
    }
}

pub(crate) fn is_compiler_specific_opcode(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::ForLoop
            | Opcode::Jump
            | Opcode::JumpIf
            | Opcode::JumpIfNot
            | Opcode::Return
            | Opcode::Panic
            | Opcode::Call
            | Opcode::CallExtern
            | Opcode::CallClosure
            | Opcode::CallIface
    )
}

#[cfg(test)]
pub(crate) fn translate_dispatch_lowering_owner(opcode: Opcode) -> LoweringOwner {
    use LoweringOwner::*;
    use Opcode::*;

    match opcode {
        Hint | LoadInt | LoadConst | Copy | CopyN | AddI | SubI | MulI | DivI | DivU | ModI
        | ModU | NegI | AddF | SubF | MulF | DivF | NegF | EqI | NeI | LtI | LeI | GtI | GeI
        | LtU | LeU | GtU | GeU | EqF | NeF | LtF | LeF | GtF | GeF | Not | BoolNot | And | Or
        | Xor | AndNot | Shl | ShrS | ShrU => TranslateScalar,

        GlobalGet | GlobalSet | GlobalGetN | GlobalSetN | PtrGet | PtrSet | PtrGetN | PtrSetN
        | PtrAdd | SlotGet | SlotSet | SlotGetN | SlotSetN => TranslateMemory,

        ConvI2F | ConvF2I | ConvF64F32 | ConvF32F64 | Trunc | IndexCheck => TranslateConversions,

        SliceNew | SliceGet | SliceSet | SliceLen | SliceCap | SliceSlice | SliceAppend
        | SliceAddr | ArrayNew | ArrayGet | ArraySet | ArrayAddr | StrLen | StrIndex
        | StrConcat | StrSlice | StrEq | StrNe | StrLt | StrLe | StrGt | StrGe | StrDecodeRune
        | MapNew | MapLen | MapGet | MapSet | MapDelete | MapIterInit | MapIterNext => {
            TranslateCollections
        }

        ClosureNew | ClosureGet | PtrNew | QueueNew | QueueLen | QueueCap | IslandNew
        | QueueClose | QueueSend | QueueRecv | GoStart | GoIsland | DeferPush | ErrDeferPush
        | Recover | SelectBegin | SelectSend | SelectRecv | SelectExec | IfaceAssert | StrNew
        | IfaceAssign | IfaceEq => TranslateRuntimeOps,

        Jump | JumpIf | JumpIfNot | Return | Panic => FunctionCompiler,
        Call | CallExtern | CallClosure | CallIface => CallHelpers,
        ForLoop => LoopCompiler,
        Opcode::Invalid => LoweringOwner::Invalid,
    }
}
