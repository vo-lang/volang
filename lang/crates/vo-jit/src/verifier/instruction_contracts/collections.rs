mod array_slice;
mod map;
mod queue_select;
mod string;

use super::{verify_layout, VerifierCtx};
use crate::verifier::JitMetadataError;
use vo_runtime::instruction::Opcode;
use vo_runtime::SlotType;

pub(super) fn verify(ctx: VerifierCtx<'_>) -> Result<(), JitMetadataError> {
    let func = ctx.func;
    let vo_module = ctx._vo_module;
    let pc = ctx.pc;
    let inst = ctx.inst;
    let opcode = ctx.opcode;

    match opcode {
        Opcode::StrNew => string::verify_str_new_contract(func, vo_module, pc, inst),
        Opcode::ArrayNew => array_slice::verify_array_new_contract(func, pc, opcode, inst),
        Opcode::ArrayGet => array_slice::verify_array_get_contract(func, pc, opcode, inst),
        Opcode::ArrayAddr => array_slice::verify_array_addr_contract(func, pc, opcode, inst),
        Opcode::ArraySet => array_slice::verify_array_set_contract(func, pc, opcode, inst),
        Opcode::SliceNew => array_slice::verify_slice_new_contract(func, pc, opcode, inst),
        Opcode::SliceGet => array_slice::verify_slice_get_contract(func, pc, opcode, inst),
        Opcode::SliceAddr => array_slice::verify_slice_addr_contract(func, pc, opcode, inst),
        Opcode::SliceSet => array_slice::verify_slice_set_contract(func, pc, opcode, inst),
        Opcode::SliceAppend => array_slice::verify_slice_append_contract(func, pc, opcode, inst),
        Opcode::SliceLen | Opcode::SliceCap => {
            array_slice::verify_slice_len_or_cap_contract(func, pc, opcode, inst)
        }
        Opcode::SliceSlice => array_slice::verify_slice_slice_contract(func, pc, opcode, inst),
        Opcode::StrLen => string::verify_str_len_contract(func, pc, opcode, inst),
        Opcode::StrIndex | Opcode::StrDecodeRune => {
            string::verify_str_index_or_decode_contract(func, pc, opcode, inst)
        }
        Opcode::StrConcat => string::verify_str_concat_contract(func, pc, opcode, inst),
        Opcode::StrEq
        | Opcode::StrNe
        | Opcode::StrLt
        | Opcode::StrLe
        | Opcode::StrGt
        | Opcode::StrGe => string::verify_str_compare_contract(func, pc, opcode, inst),
        Opcode::StrSlice => string::verify_str_slice_contract(func, pc, opcode, inst),
        Opcode::MapNew => map::verify_map_new_contract(func, pc, opcode, inst),
        Opcode::MapGet => map::verify_map_get_contract(func, pc, inst),
        Opcode::MapSet => map::verify_map_set_contract(func, pc, inst),
        Opcode::MapDelete => map::verify_map_delete_contract(func, pc, inst),
        Opcode::MapLen => map::verify_map_len_contract(func, pc, opcode, inst),
        Opcode::MapIterInit => map::verify_map_iter_init_contract(func, pc, opcode, inst),
        Opcode::MapIterNext => map::verify_map_iter_next_contract(func, pc, opcode, inst),
        Opcode::QueueNew => queue_select::verify_queue_new_contract(func, pc, inst),
        Opcode::QueueSend => queue_select::verify_queue_send_contract(func, pc, inst),
        Opcode::QueueRecv => queue_select::verify_queue_recv_contract(func, pc, opcode, inst),
        Opcode::QueueLen | Opcode::QueueCap | Opcode::QueueClose => {
            queue_select::verify_queue_len_cap_close_contract(func, pc, opcode, inst)
        }
        Opcode::SelectBegin => Ok(()),
        Opcode::SelectSend => queue_select::verify_select_send_contract(func, pc, inst),
        Opcode::SelectRecv => queue_select::verify_select_recv_contract(func, pc, opcode, inst),
        Opcode::SelectExec => queue_select::verify_select_exec_contract(func, pc, opcode, inst),
        Opcode::IslandNew => verify_layout(
            func,
            pc,
            opcode,
            inst.a,
            &[SlotType::GcRef],
            "IslandNew destination",
        ),
        other => unreachable!("collections verifier received {other:?}"),
    }
}
