use super::*;
use vo_common_core::instruction::copy_n_mirror_flags;

#[test]
fn loop_analysis_test_fixtures_use_checked_packed_operands() {
    let src = include_str!("../loop_analysis.rs");
    let forbidden = ["n.min", "(u8::MAX as u16) as u8"].concat();
    assert!(
            !src.contains(&forbidden),
            "loop-analysis fixtures must use checked packed-operand encoders instead of saturating u8 flags"
        );
}

fn make_func(code: Vec<Instruction>) -> FunctionDef {
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&code);
    let slot_types = vec![];
    let gc_scan_slots = FunctionDef::compute_gc_scan_slots(&slot_types);
    let borrowed_scan_slots_prefix = FunctionDef::compute_borrowed_scan_slots_prefix(&slot_types);
    FunctionDef {
        name: "test".to_string(),
        param_count: 0,
        param_slots: 0,
        param_types: vec![],
        local_slots: 20,
        gc_scan_slots,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: vec![],
        is_closure: false,
        capture_types: vec![],
        capture_slot_types: vec![],
        error_ret_slot: -1,
        has_defer: false,
        has_calls,
        has_call_extern,
        jit_metadata: vec![Default::default(); code.len()],
        code,
        slot_types,
        borrowed_scan_slots_prefix,
    }
}

fn with_metadata(
    mut func: FunctionDef,
    pc: usize,
    metadata: JitInstructionMetadata,
) -> FunctionDef {
    func.jit_metadata[pc] = metadata;
    func
}

fn hint_loop(depth: u8, end_offset: u8, exit_pc: u32) -> Instruction {
    // New format: a = flags(4) | depth(4) | end_offset(8), bc = exit_pc
    let loop_info = ((end_offset as u16) << 8) | ((depth as u16) << 4);
    let b = (exit_pc & 0xFFFF) as u16;
    let c = ((exit_pc >> 16) & 0xFFFF) as u16;
    Instruction {
        op: Opcode::Hint as u8,
        flags: HINT_LOOP,
        a: loop_info,
        b,
        c,
    }
}

fn load_int(dst: u16, val: i32) -> Instruction {
    let b = (val as u32 & 0xFFFF) as u16;
    let c = ((val as u32 >> 16) & 0xFFFF) as u16;
    Instruction {
        op: Opcode::LoadInt as u8,
        flags: 0,
        a: dst,
        b,
        c,
    }
}

fn add_i(dst: u16, src1: u16, src2: u16) -> Instruction {
    Instruction {
        op: Opcode::AddI as u8,
        flags: 0,
        a: dst,
        b: src1,
        c: src2,
    }
}

fn jump(offset: i32) -> Instruction {
    let b = (offset as u32 & 0xFFFF) as u16;
    let c = ((offset as u32 >> 16) & 0xFFFF) as u16;
    Instruction {
        op: Opcode::Jump as u8,
        flags: 0,
        a: 0,
        b,
        c,
    }
}

fn for_loop(idx: u16, limit: u16, offset: i16) -> Instruction {
    Instruction {
        op: Opcode::ForLoop as u8,
        flags: 0,
        a: idx,
        b: limit,
        c: offset as u16,
    }
}

fn ret() -> Instruction {
    Instruction {
        op: Opcode::Return as u8,
        flags: 0,
        a: 0,
        b: 0,
        c: 0,
    }
}

#[test]
fn test_no_loops() {
    let func = make_func(vec![load_int(0, 42), ret()]);
    let loops = try_analyze_loops(&func).unwrap();
    assert!(
        loops.is_empty(),
        "Should detect no loops without Hint instructions"
    );
}

#[test]
fn test_simple_loop_with_hints() {
    // Simple loop with new Hint format (no HINT_LOOP_END):
    // 0: LoadInt r0, 0
    // 1: Hint LOOP_BEGIN depth=0, end_offset=3, exit=5
    // 2: LoadInt r1, 10       <- begin_pc (loop_start)
    // 3: AddI r0, r0, r1
    // 4: Jump -2              <- end_pc (back-edge)
    // 5: Return               <- exit_pc
    let func = make_func(vec![
        load_int(0, 0),     // 0
        hint_loop(0, 3, 5), // 1: HINT_LOOP, end_offset=3 -> end_pc=4
        load_int(1, 10),    // 2: begin_pc (loop_start)
        add_i(0, 0, 1),     // 3
        jump(-2),           // 4: back edge (end_pc)
        ret(),              // 5: exit_pc
    ]);

    let loops = try_analyze_loops(&func).unwrap();
    assert_eq!(loops.len(), 1, "Should detect 1 loop");

    let loop_info = &loops[0];
    assert_eq!(loop_info.begin_pc, 2, "begin_pc should be hint_pc + 1 = 2");
    assert_eq!(
        loop_info.end_pc, 4,
        "end_pc should be hint_pc + end_offset = 4"
    );
    assert_eq!(loop_info.exit_pc, 5, "exit_pc should be 5");
    assert_eq!(loop_info.depth, 0, "depth should be 0");
}

#[test]
fn test_nested_loops_with_hints() {
    // Nested loops with new format:
    // 0: Hint LOOP_BEGIN depth=0, end_offset=7, exit=9
    // 1: LoadInt r0, 0          <- outer begin_pc
    // 2: Hint LOOP_BEGIN depth=1, end_offset=2, exit=5
    // 3: AddI r0, r0, r1        <- inner begin_pc
    // 4: Jump -1                <- inner end_pc (back edge)
    // 5: LoadInt r0, 0          <- inner exit_pc
    // 6: LoadInt r0, 0
    // 7: Jump -6                <- outer end_pc (back edge)
    // 8: LoadInt r0, 0
    // 9: Return                 <- outer exit_pc
    let func = make_func(vec![
        hint_loop(0, 7, 9), // 0: outer HINT_LOOP
        load_int(0, 0),     // 1: outer begin_pc
        hint_loop(1, 2, 5), // 2: inner HINT_LOOP
        add_i(0, 0, 1),     // 3: inner begin_pc
        jump(-1),           // 4: inner back edge
        load_int(0, 0),     // 5: inner exit_pc
        load_int(0, 0),     // 6
        jump(-6),           // 7: outer back edge
        load_int(0, 0),     // 8
        ret(),              // 9: outer exit_pc
    ]);

    let loops = try_analyze_loops(&func).unwrap();
    assert_eq!(loops.len(), 2, "Should detect 2 nested loops");

    // Loops are in bytecode order (outer first, then inner)
    let outer = &loops[0];
    assert_eq!(outer.depth, 0);
    assert_eq!(outer.begin_pc, 1, "outer begin_pc = hint_pc + 1");
    assert_eq!(outer.end_pc, 7, "outer end_pc = hint_pc + end_offset");
    assert_eq!(outer.exit_pc, 9);

    let inner = &loops[1];
    assert_eq!(inner.depth, 1);
    assert_eq!(inner.begin_pc, 3, "inner begin_pc = hint_pc + 1");
    assert_eq!(inner.end_pc, 4, "inner end_pc = hint_pc + end_offset");
    assert_eq!(inner.exit_pc, 5);
}

#[test]
fn test_infinite_loop() {
    // Infinite loop: exit_pc = 0
    // 0: Hint LOOP_BEGIN depth=0, end_offset=2, exit=0 (infinite)
    // 1: AddI r0, r0, r1        <- begin_pc
    // 2: Jump -1                <- end_pc (back edge)
    let func = make_func(vec![
        hint_loop(0, 2, 0), // 0: HINT_LOOP with exit=0 (infinite)
        add_i(0, 0, 1),     // 1: begin_pc
        jump(-1),           // 2: back edge (end_pc)
    ]);

    let loops = try_analyze_loops(&func).unwrap();
    assert_eq!(loops.len(), 1);

    let loop_info = &loops[0];
    assert_eq!(loop_info.begin_pc, 1, "begin_pc = hint_pc + 1");
    assert_eq!(loop_info.end_pc, 2, "end_pc = hint_pc + end_offset");
    assert!(loop_info.is_infinite(), "Should be infinite loop");
}

#[test]
fn try_analyze_reports_effect_slot_range_overflow() {
    let func = make_func(vec![
        hint_loop(0, 2, 3),
        copy_n(0, u16::MAX, 2),
        jump(-1),
        ret(),
    ]);

    assert!(matches!(
        try_analyze_loops(&func),
        Err(LoopAnalysisError::SlotRangeOverflow { pc: 1, .. })
    ));
}

#[test]
fn try_analyze_requires_loop_end_metadata_when_offset_is_not_encoded() {
    let func = make_func(vec![hint_loop(0, 0, 2), load_int(0, 1), ret()]);

    assert!(matches!(
        try_analyze_loops(&func),
        Err(LoopAnalysisError::MissingLoopEndMetadata { hint_pc: 0 })
    ));
}

#[test]
fn try_analyze_reports_metadata_end_without_back_edge() {
    let func = with_metadata(
        make_func(vec![hint_loop(0, 0, 2), load_int(0, 1), ret()]),
        0,
        JitInstructionMetadata::LoopEnd { end_pc: 2 },
    );

    assert!(matches!(
        try_analyze_loops(&func),
        Err(LoopAnalysisError::MissingBackEdge {
            loop_start: 1,
            end_pc: 2
        })
    ));
}

#[test]
fn try_analyze_uses_loop_end_metadata_for_unencoded_jump_loop() {
    let func = with_metadata(
        make_func(vec![hint_loop(0, 0, 0), add_i(0, 0, 1), jump(-1)]),
        0,
        JitInstructionMetadata::LoopEnd { end_pc: 2 },
    );

    let loops = try_analyze_loops(&func).unwrap();
    assert_eq!(loops.len(), 1);
    assert_eq!(loops[0].begin_pc, 1);
    assert_eq!(loops[0].end_pc, 2);
}

#[test]
fn try_analyze_uses_loop_end_metadata_for_unencoded_for_loop() {
    let func = with_metadata(
        make_func(vec![
            hint_loop(0, 0, 3),
            add_i(0, 0, 1),
            for_loop(0, 1, -2),
            ret(),
        ]),
        0,
        JitInstructionMetadata::LoopEnd { end_pc: 2 },
    );

    let loops = try_analyze_loops(&func).unwrap();
    assert_eq!(loops.len(), 1);
    assert_eq!(loops[0].begin_pc, 1);
    assert_eq!(loops[0].end_pc, 2);
}

#[test]
fn try_analyze_rejects_inconsistent_loop_end_metadata() {
    let func = with_metadata(
        make_func(vec![hint_loop(0, 2, 0), add_i(0, 0, 1), jump(-1)]),
        0,
        JitInstructionMetadata::LoopEnd { end_pc: 1 },
    );

    assert!(matches!(
        try_analyze_loops(&func),
        Err(LoopAnalysisError::InconsistentLoopEndMetadata {
            hint_pc: 0,
            encoded_end_pc: 2,
            metadata_end_pc: 1
        })
    ));
}

// =========================================================================
// Tests for get_read_regs and get_write_regs_multi
// =========================================================================

fn slice_set(slice: u16, idx: u16, val: u16, elem_bytes: u8) -> Instruction {
    Instruction {
        op: Opcode::SliceSet as u8,
        flags: elem_bytes,
        a: slice,
        b: idx,
        c: val,
    }
}

fn slice_get(dst: u16, slice: u16, idx: u16, elem_bytes: u8) -> Instruction {
    Instruction {
        op: Opcode::SliceGet as u8,
        flags: elem_bytes,
        a: dst,
        b: slice,
        c: idx,
    }
}

fn copy_n(dst: u16, src: u16, n: u16) -> Instruction {
    Instruction {
        op: Opcode::CopyN as u8,
        flags: copy_n_mirror_flags(n),
        a: dst,
        b: src,
        c: n,
    }
}

fn iface_assign(dst: u16, src: u16, vk: u8) -> Instruction {
    Instruction {
        op: Opcode::IfaceAssign as u8,
        flags: vk,
        a: dst,
        b: src,
        c: 0,
    }
}

fn call_extern(dst: u16, extern_id: u16, arg_start: u16, arg_count: u8) -> Instruction {
    Instruction {
        op: Opcode::CallExtern as u8,
        flags: arg_count,
        a: dst,
        b: extern_id,
        c: arg_start,
    }
}

fn call_iface(
    iface_slot: u16,
    arg_start: u16,
    arg_slots: u8,
    ret_slots: u8,
    method_idx: u8,
) -> Instruction {
    let c = ((arg_slots as u16) << 8) | (ret_slots as u16);
    Instruction {
        op: Opcode::CallIface as u8,
        flags: method_idx,
        a: iface_slot,
        b: arg_start,
        c,
    }
}

fn queue_new(dst: u16, elem_type: u16, cap: u16, elem_slots: u8, is_port: bool) -> Instruction {
    Instruction {
        op: Opcode::QueueNew as u8,
        flags: elem_slots
            | if is_port {
                vo_runtime::instruction::QUEUE_KIND_PORT_FLAG
            } else {
                0
            },
        a: dst,
        b: elem_type,
        c: cap,
    }
}

fn queue_len(dst: u16, ch: u16) -> Instruction {
    Instruction {
        op: Opcode::QueueLen as u8,
        flags: 0,
        a: dst,
        b: ch,
        c: 0,
    }
}

fn queue_cap(dst: u16, ch: u16) -> Instruction {
    Instruction {
        op: Opcode::QueueCap as u8,
        flags: 0,
        a: dst,
        b: ch,
        c: 0,
    }
}

fn queue_close(ch: u16) -> Instruction {
    Instruction {
        op: Opcode::QueueClose as u8,
        flags: 0,
        a: ch,
        b: 0,
        c: 0,
    }
}

fn queue_send(ch: u16, val_start: u16, elem_slots: u8) -> Instruction {
    Instruction {
        op: Opcode::QueueSend as u8,
        flags: elem_slots,
        a: ch,
        b: val_start,
        c: 0,
    }
}

fn queue_recv(dst: u16, ch: u16, elem_slots: u8, has_ok: bool) -> Instruction {
    let flags = (elem_slots << 1) | if has_ok { 1 } else { 0 };
    Instruction {
        op: Opcode::QueueRecv as u8,
        flags,
        a: dst,
        b: ch,
        c: 0,
    }
}

fn select_send_inst(ch: u16, val_start: u16, elem_slots: u8) -> Instruction {
    Instruction {
        op: Opcode::SelectSend as u8,
        flags: elem_slots,
        a: ch,
        b: val_start,
        c: 0,
    }
}

fn select_recv_inst(dst: u16, ch: u16, elem_slots: u8, has_ok: bool) -> Instruction {
    let flags = (elem_slots << 1) | if has_ok { 1 } else { 0 };
    Instruction {
        op: Opcode::SelectRecv as u8,
        flags,
        a: dst,
        b: ch,
        c: 0,
    }
}

fn select_begin(case_count: u16, has_default: bool) -> Instruction {
    Instruction {
        op: Opcode::SelectBegin as u8,
        flags: if has_default { 1 } else { 0 },
        a: case_count,
        b: 0,
        c: 0,
    }
}

fn select_exec(result: u16) -> Instruction {
    Instruction {
        op: Opcode::SelectExec as u8,
        flags: 0,
        a: result,
        b: 0,
        c: 0,
    }
}

#[test]
fn test_get_read_regs_slice_set() {
    // SliceSet: a=slice, b=idx, c=val_start, flags=elem_bytes
    // Single slot element
    let inst = slice_set(10, 11, 12, 8);
    let regs = get_read_regs(&inst);
    assert_eq!(
        regs,
        vec![10, 11, 12],
        "SliceSet should read slice, idx, val"
    );

    // Multi-slot element (16 bytes = 2 slots)
    let inst = slice_set(10, 11, 12, 16);
    let regs = get_read_regs(&inst);
    assert_eq!(
        regs,
        vec![10, 11, 12, 13],
        "SliceSet with 16 bytes should read 2 value slots"
    );
}

#[test]
fn test_get_read_regs_copy_n() {
    let inst = copy_n(5, 10, 3);
    let regs = get_read_regs(&inst);
    assert_eq!(regs, vec![10, 11, 12], "CopyN should read n slots from src");
}

#[test]
fn test_get_read_regs_copy_n_canonical_count() {
    let inst = Instruction {
        op: Opcode::CopyN as u8,
        flags: 0,
        a: 5,
        b: 10,
        c: 3,
    };
    let regs = get_read_regs(&inst);
    assert_eq!(
        regs,
        vec![10, 11, 12],
        "CopyN should use canonical c count even when flags is zero"
    );
}

#[test]
fn test_get_read_regs_iface_assign() {
    // Concrete source (vk != 16)
    let inst = iface_assign(5, 10, 1);
    let regs = get_read_regs(&inst);
    assert_eq!(
        regs,
        vec![10],
        "IfaceAssign with concrete source reads 1 slot"
    );

    // Interface source (vk == 16)
    let inst = iface_assign(5, 10, 16);
    let regs = get_read_regs(&inst);
    assert_eq!(
        regs,
        vec![10, 11],
        "IfaceAssign with interface source reads 2 slots"
    );
}

#[test]
fn test_get_read_regs_call_extern() {
    // CallExtern: a=dst, b=extern_id, c=arg_start, flags=arg_count
    let inst = call_extern(0, 5, 10, 3);
    let regs = get_read_regs(&inst);
    assert_eq!(
        regs,
        vec![10, 11, 12],
        "CallExtern should read arg_count args from arg_start"
    );
}

#[test]
fn test_get_read_regs_call_iface() {
    // CallIface: a=iface_slot (2 slots), b=arg_start, c=(arg_slots<<8|ret_slots)
    let inst = call_iface(5, 10, 2, 1, 0);
    let regs = get_read_regs(&inst);
    assert_eq!(
        regs,
        vec![5, 6, 10, 11],
        "CallIface should read iface (2 slots) + args"
    );
}

#[test]
fn test_get_write_regs_multi_slice_get() {
    // Single slot element
    let inst = slice_get(10, 5, 6, 8);
    let regs = get_write_regs_multi(&inst);
    assert_eq!(regs, vec![10], "SliceGet with 8 bytes writes 1 slot");

    // Multi-slot element (16 bytes = 2 slots)
    let inst = slice_get(10, 5, 6, 16);
    let regs = get_write_regs_multi(&inst);
    assert_eq!(regs, vec![10, 11], "SliceGet with 16 bytes writes 2 slots");
}

#[test]
fn test_get_write_regs_multi_copy_n() {
    let inst = copy_n(10, 5, 3);
    let regs = get_write_regs_multi(&inst);
    assert_eq!(regs, vec![10, 11, 12], "CopyN should write n slots to dst");
}

#[test]
fn test_get_write_regs_multi_copy_n_large_canonical_count() {
    let inst = Instruction {
        op: Opcode::CopyN as u8,
        flags: 0,
        a: 10,
        b: 5,
        c: 300,
    };
    let regs = get_write_regs_multi(&inst);
    assert_eq!(regs.len(), 300, "CopyN should not truncate counts to flags");
    assert_eq!(regs[0], 10);
    assert_eq!(regs[299], 309);
}

#[test]
fn test_get_write_regs_multi_iface_assign() {
    let inst = iface_assign(10, 5, 1);
    let regs = get_write_regs_multi(&inst);
    assert_eq!(regs, vec![10, 11], "IfaceAssign always writes 2 slots");
}

#[test]
fn test_get_write_regs_multi_call_extern() {
    let inst = call_extern(10, 5, 20, 3);
    assert!(matches!(
        effects::try_multi_write_regs(&inst),
        Err(effects::EffectError::MissingExtern { extern_id: 5 })
    ));
}

#[test]
fn test_get_write_regs_multi_call_iface() {
    let inst = call_iface(5, 10, 2, 3, 0);
    let regs = get_write_regs_multi(&inst);
    assert_eq!(
        regs,
        vec![12, 13, 14],
        "CallIface should write ret_slots after arg slots"
    );
}

#[test]
fn test_get_read_regs_queue_and_select_ops() {
    assert_eq!(get_read_regs(&queue_new(1, 2, 3, 1, false)), vec![2, 3]);
    assert_eq!(get_read_regs(&queue_len(4, 5)), vec![5]);
    assert_eq!(get_read_regs(&queue_close(6)), vec![6]);
    assert_eq!(get_read_regs(&queue_send(7, 8, 2)), vec![7, 8, 9]);
    assert_eq!(get_read_regs(&queue_recv(10, 11, 2, true)), vec![11]);
    assert_eq!(get_read_regs(&select_send_inst(12, 13, 0)), vec![12]);
    assert_eq!(get_read_regs(&select_recv_inst(14, 15, 0, true)), vec![15]);
}

#[test]
fn test_get_write_reg_queue_and_select_ops() {
    assert_eq!(get_write_reg(&queue_new(1, 2, 3, 1, true)), Some(1));
    assert_eq!(get_write_reg(&queue_len(4, 5)), Some(4));
    assert_eq!(get_write_reg(&queue_cap(6, 7)), Some(6));
    assert_eq!(get_write_reg(&select_exec(8)), Some(8));
    assert_eq!(get_write_reg(&queue_close(9)), None);
}

#[test]
fn test_get_write_regs_multi_queue_and_select_ops() {
    assert_eq!(
        get_write_regs_multi(&queue_recv(10, 5, 2, true)),
        vec![10, 11, 12]
    );
    assert_eq!(
        get_write_regs_multi(&queue_recv(20, 6, 2, false)),
        vec![20, 21]
    );
    assert_eq!(
        get_write_regs_multi(&select_recv_inst(30, 7, 0, true)),
        vec![30, 31]
    );
}

#[test]
fn test_analyze_loop_liveness_port_queue_ops() {
    let func = make_func(vec![
        hint_loop(0, 3, 4),
        queue_send(0, 1, 2),
        queue_recv(3, 0, 2, true),
        jump(-2),
        ret(),
    ]);

    let loops = try_analyze_loops(&func).unwrap();
    assert_eq!(loops.len(), 1);
    assert_eq!(loops[0].live_in, vec![0, 1, 2]);
    assert_eq!(loops[0].live_out, vec![3, 4, 5]);
}

#[test]
fn test_analyze_loop_liveness_select_ops() {
    let func = make_func(vec![
        hint_loop(0, 5, 6),
        select_begin(2, false),
        select_send_inst(0, 1, 2),
        select_recv_inst(10, 4, 2, true),
        select_exec(13),
        jump(-4),
        ret(),
    ]);

    let loops = try_analyze_loops(&func).unwrap();
    assert_eq!(loops.len(), 1);
    assert_eq!(loops[0].live_in, vec![0, 1, 2, 4]);
    assert_eq!(loops[0].live_out, vec![10, 11, 12, 13]);
}
