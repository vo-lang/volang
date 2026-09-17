use super::*;
use crate::bytecode::{ExternDef, ExternEffects, FunctionDef, ReturnShape};
use crate::instruction_effects::{
    instruction_frame_memory_effect, visit_instruction_register_reads,
    visit_instruction_register_writes, FrameMemoryEffect,
};
use crate::SlotType;

fn function() -> FunctionDef {
    FunctionDef {
        name: "callee".into(),
        param_count: 2,
        param_slots: 2,
        local_slots: 5,
        ret_slots: 3,
        ret_slot_types: vec![SlotType::Value; 3],
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: vec![],
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        code: vec![],
        instruction_metadata: vec![],
        slot_types: vec![SlotType::Value; 5],
        capture_types: vec![],
        capture_slot_types: vec![],
        param_types: vec![],
    }
}

fn metadata(opcode: Opcode) -> InstructionMetadata {
    use InstructionMetadata as M;
    let layout = || vec![SlotType::Value; 3];
    match opcode {
        Opcode::SlotGet | Opcode::SlotGetN | Opcode::SlotSet | Opcode::SlotSetN => M::SlotLayout {
            array_len: 7,
            elem_layout: layout(),
        },
        Opcode::PtrGetN | Opcode::PtrSetN => M::PtrLayout {
            value_layout: layout(),
        },
        Opcode::CallClosure
        | Opcode::GoStart
        | Opcode::DeferPush
        | Opcode::ErrDeferPush
        | Opcode::GoIsland => M::CallLayout {
            arg_layout: layout(),
            ret_layout: layout(),
        },
        Opcode::CallIface => M::CallIfaceLayout {
            iface_meta_id: 0,
            method_idx: 70000,
            arg_layout: layout(),
            ret_layout: layout(),
        },
        Opcode::CallExtern => M::CallExternLayout {
            arg_layout: layout(),
            ret_layout: layout(),
        },
        Opcode::ArrayGet
        | Opcode::ArraySet
        | Opcode::SliceGet
        | Opcode::SliceSet
        | Opcode::SliceAppend => M::ElemLayout {
            elem_bytes: 24,
            needs_sign_extend: false,
            slot_layout: layout(),
        },
        Opcode::MapGet => M::MapGet {
            key_layout: layout(),
            val_layout: layout(),
            has_ok: true,
        },
        Opcode::MapSet => M::MapSet {
            key_layout: layout(),
            val_layout: layout(),
        },
        Opcode::MapDelete => M::MapDelete {
            key_layout: layout(),
        },
        Opcode::MapIterNext => M::MapIterNext {
            key_layout: layout(),
            val_layout: layout(),
        },
        Opcode::QueueSend | Opcode::QueueRecv | Opcode::SelectSend => M::QueueLayout {
            elem_layout: layout(),
        },
        Opcode::SelectExec => M::SelectExecLayout {
            cases: vec![
                SelectCaseLayout::Send {
                    queue: 501,
                    value: 600,
                    elem_slots: 3,
                },
                SelectCaseLayout::Recv {
                    destination: 700,
                    queue: 801,
                    elem_slots: 3,
                    has_ok: true,
                },
            ],
        },
        Opcode::IfaceAssert => M::IfaceAssertLayout {
            assert_kind: 0,
            target_id: 90000,
            result_layout: layout(),
        },
        _ => M::None,
    }
}

#[test]
fn every_opcode_remaps_the_common_effects_without_changing_widths_or_identities() {
    let functions = vec![function(); 4];
    let externs = vec![
        ExternDef::exact(
            "external".into(),
            3,
            ReturnShape::slots(3),
            ExternEffects::NONE,
            vec![]
        );
        81
    ];
    for op in 0..Opcode::COUNT as u8 {
        let opcode = Opcode::from_u8(op);
        for flags in [0, 1, 6, 16] {
            // Static IDs use flags as high bits; the closure flag alone is
            // sufficient to cover the alternate call-shape operand encoding.
            if matches!(opcode, Opcode::Call) && flags != 0
                || matches!(
                    opcode,
                    Opcode::GoStart | Opcode::DeferPush | Opcode::ErrDeferPush
                ) && flags > 1
            {
                continue;
            }
            let before = Instruction::with_flags(opcode, flags, 3, 80, 3000);
            let before_meta = metadata(opcode);
            let mut after = before;
            let mut after_meta = before_meta.clone();
            try_map_instruction_registers(&mut after, &mut after_meta, |slot| {
                Ok::<_, ()>(slot + 11)
            })
            .unwrap();
            let reads = |inst: &Instruction, meta: &InstructionMetadata| {
                let mut out = Vec::new();
                visit_instruction_register_reads(inst, Some(meta), &functions, |start, count| {
                    out.push((start, count))
                })
                .unwrap();
                if let FrameMemoryEffect::AliasedRange { start, count } =
                    instruction_frame_memory_effect(inst, Some(meta)).unwrap()
                {
                    out.push((start, count));
                }
                out
            };
            let writes = |inst: &Instruction, meta: &InstructionMetadata| {
                let mut out = Vec::new();
                visit_instruction_register_writes(
                    inst,
                    Some(meta),
                    &externs,
                    &functions,
                    |start, count| out.push((start, count)),
                )
                .unwrap();
                out
            };
            let mut expected = reads(&before, &before_meta);
            if opcode != Opcode::ClosureGet {
                // Fixed ABI self slot remains 0.
                for (start, _) in &mut expected {
                    *start += 11;
                }
            }
            assert_eq!(
                reads(&after, &after_meta),
                expected,
                "{opcode:?}/{flags} reads"
            );
            let expected: Vec<_> = writes(&before, &before_meta)
                .into_iter()
                .map(|(start, count)| (start + 11, count))
                .collect();
            assert_eq!(
                writes(&after, &after_meta),
                expected,
                "{opcode:?}/{flags} writes"
            );
        }
    }
}

#[test]
fn empty_windows_and_select_descriptors_keep_their_encoded_anchors() {
    let mut call = Instruction::new(Opcode::CallClosure, 1, 50, 900);
    let mut meta = InstructionMetadata::CallLayout {
        arg_layout: vec![],
        ret_layout: vec![],
    };
    assert_eq!(instruction_call_frame_prefix(&call), Some(49));
    try_map_instruction_registers(&mut call, &mut meta, |slot| Ok::<_, ()>(slot / 2)).unwrap();
    assert_eq!((call.a, call.b, call.c), (0, 25, 900));
    assert_eq!(instruction_call_frame_prefix(&call), Some(24));
    let mut recv = Instruction::new(Opcode::SelectRecv, 50, 20, 0);
    let mut meta = InstructionMetadata::QueueLayout {
        elem_layout: vec![],
    };
    try_map_instruction_registers(&mut recv, &mut meta, |slot| Ok::<_, ()>(slot / 2)).unwrap();
    assert_eq!((recv.a, recv.b), (25, 10));
    let mut terminal = Instruction::new(Opcode::SelectExec, 2, 0, 0);
    let mut meta = InstructionMetadata::SelectExecLayout {
        cases: vec![SelectCaseLayout::Recv {
            destination: 50,
            queue: 20,
            elem_slots: 0,
            has_ok: false,
        }],
    };
    try_map_instruction_registers(&mut terminal, &mut meta, |slot| Ok::<_, ()>(slot / 2)).unwrap();
    assert_eq!(
        meta,
        InstructionMetadata::SelectExecLayout {
            cases: vec![SelectCaseLayout::Recv {
                destination: 25,
                queue: 10,
                elem_slots: 0,
                has_ok: false
            }]
        }
    );
}
