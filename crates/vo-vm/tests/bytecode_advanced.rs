use vo_common_core::types::{SlotType, ValueKind, ValueMeta};
use vo_vm::bytecode::{Constant, ExternDef, FunctionDef, GlobalDef, InterfaceMeta, Module, StructMeta};
use vo_vm::exec::ExternCallResult;
use vo_vm::instruction::{Instruction, Opcode};
use vo_vm::vm::Vm;

fn imm32_parts(v: i32) -> (u16, u16) {
    let u = v as u32;
    (u as u16, (u >> 16) as u16)
}

fn build_module(
    globals: Vec<GlobalDef>,
    constants: Vec<Constant>,
    struct_metas: Vec<StructMeta>,
    interface_metas: Vec<InterfaceMeta>,
    externs: Vec<ExternDef>,
    functions: Vec<FunctionDef>,
    entry_func: u32,
) -> Module {
    let mut module = Module::new("test".into());
    module.globals = globals;
    module.constants = constants;
    module.struct_metas = struct_metas;
    module.interface_metas = interface_metas;
    module.externs = externs;
    module.functions = functions;
    module.entry_func = entry_func;
    module
}

fn run_module(mut vm: Vm, module: Module) -> Vm {
    vm.load(module);
    vm.run().unwrap();
    vm
}

#[test]
fn test_call_return_args_and_ret() {
    // add2(a,b) => a+b
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
    }];

    // --- callee: add2 ---
    let add2 = FunctionDef {
        name: "add2".into(),
        param_count: 2,
        param_slots: 2,
        local_slots: 3,
        ret_slots: 1,
        slot_types: vec![SlotType::Value; 3],
        code: vec![
            Instruction::new(Opcode::AddI, 2, 0, 1),
            Instruction::new(Opcode::Return, 2, 1, 0),
        ],
    };

    // --- main ---
    let (b_40, c_40) = imm32_parts(40);
    let (b_2, c_2) = imm32_parts(2);

    // inst.c = (arg_slots<<8) | ret_slots
    let call_c = ((2u16 as u16) << 8) | 1u16;

    let main = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 4,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 4],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_40, c_40),
            Instruction::new(Opcode::LoadInt, 1, b_2, c_2),
            Instruction::with_flags(Opcode::Call, 0, 1, 0, call_c),
            Instruction::new(Opcode::GlobalSet, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let module = build_module(globals, vec![], vec![], vec![], vec![], vec![main, add2], 0);
    let vm = run_module(Vm::new(), module);
    assert_eq!(vm.globals[0], 42);
}

#[test]
fn test_closure_capture_and_call_closure() {
    // closure captures x=5, call with arg=7 => return x+arg = 12
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
    }];

    // --- callee: closure_func(closure,arg) ---
    // reg0 = closure_ref, reg1 = arg
    let closure_func = FunctionDef {
        name: "closure_func".into(),
        param_count: 1,
        param_slots: 1,
        local_slots: 4,
        ret_slots: 1,
        slot_types: vec![SlotType::Value; 4],
        code: vec![
            Instruction::new(Opcode::ClosureGet, 2, 0, 0),
            Instruction::new(Opcode::AddI, 3, 2, 1),
            Instruction::new(Opcode::Return, 3, 1, 0),
        ],
    };

    let (b_5, c_5) = imm32_parts(5);
    let (b_7, c_7) = imm32_parts(7);
    let call_c = ((1u16 as u16) << 8) | 1u16;

    let main = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 6,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 6],
        code: vec![
            // r0 = closure(func_id=1, capture_count=1)
            Instruction::with_flags(Opcode::ClosureNew, 0, 0, 1, 1),
            Instruction::new(Opcode::LoadInt, 1, b_5, c_5),
            // set cap0 = r1
            Instruction::new(Opcode::ClosureSet, 0, 1, 0),
            Instruction::new(Opcode::LoadInt, 2, b_7, c_7),
            // call closure r0, args start at r2 (1 arg), ret_slots=1 -> returns into r2
            Instruction::with_flags(Opcode::CallClosure, 0, 0, 2, call_c),
            Instruction::new(Opcode::GlobalSet, 0, 2, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let module = build_module(globals, vec![], vec![], vec![], vec![], vec![main, closure_func], 0);
    let vm = run_module(Vm::new(), module);
    assert_eq!(vm.globals[0], 12);
}

#[test]
fn test_map_set_get_delete_len_with_ok() {
    // map[int64]int64: set two items, get one with ok, delete, len
    let globals = vec![
        GlobalDef {
            name: "g_val".into(),
            slots: 1,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
        },
        GlobalDef {
            name: "g_ok".into(),
            slots: 1,
            value_kind: ValueKind::Bool as u8,
            meta_id: 0,
        },
        GlobalDef {
            name: "g_len0".into(),
            slots: 1,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
        },
        GlobalDef {
            name: "g_len1".into(),
            slots: 1,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
        },
    ];

    let key_meta = ValueMeta::new(0, ValueKind::Int64).to_raw() as u64;
    let val_meta = ValueMeta::new(0, ValueKind::Int64).to_raw() as u64;
    let packed_meta = ((key_meta as u64) << 32) | (val_meta as u64);

    let constants = vec![Constant::Int(packed_meta as i64)];

    let (b_k1, c_k1) = imm32_parts(1);
    let (b_v10, c_v10) = imm32_parts(10);
    let (b_k2, c_k2) = imm32_parts(2);
    let (b_v20, c_v20) = imm32_parts(20);

    // map_set meta: [key_slots:8 | val_slots:8]
    let (b_set_meta, c_set_meta) = imm32_parts(0x0101);

    // map_get meta: [key_slots:16 | val_slots:15 | has_ok:1]
    let (b_get_meta, c_get_meta) = imm32_parts(0x0001_0003);

    // map_delete meta: key_slots (usize)
    let (b_del_meta, c_del_meta) = imm32_parts(1);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 16,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 16],
        code: vec![
            // r0 = packed key/val meta
            Instruction::new(Opcode::LoadConst, 0, 0, 0),
            // r1 = key_slots/val_slots for MapNew
            Instruction::with_flags(Opcode::MapNew, 0, 2, 0, 0x0101),

            // set(1->10)
            Instruction::new(Opcode::LoadInt, 3, b_set_meta, c_set_meta),
            Instruction::new(Opcode::LoadInt, 4, b_k1, c_k1),
            Instruction::new(Opcode::LoadInt, 5, b_v10, c_v10),
            Instruction::new(Opcode::MapSet, 2, 3, 5),

            // set(2->20)
            Instruction::new(Opcode::LoadInt, 4, b_k2, c_k2),
            Instruction::new(Opcode::LoadInt, 5, b_v20, c_v20),
            Instruction::new(Opcode::MapSet, 2, 3, 5),

            // len0 = len(m)
            Instruction::new(Opcode::MapLen, 6, 2, 0),
            Instruction::new(Opcode::GlobalSet, 2, 6, 0),

            // get(1)
            Instruction::new(Opcode::LoadInt, 7, b_get_meta, c_get_meta),
            Instruction::new(Opcode::LoadInt, 8, b_k1, c_k1),
            Instruction::with_flags(Opcode::MapGet, 0, 9, 2, 7),
            // dst: r9=val, r10=ok
            Instruction::new(Opcode::GlobalSet, 0, 9, 0),
            Instruction::new(Opcode::GlobalSet, 1, 10, 0),

            // delete(1)
            Instruction::new(Opcode::LoadInt, 11, b_del_meta, c_del_meta),
            Instruction::new(Opcode::LoadInt, 12, b_k1, c_k1),
            Instruction::new(Opcode::MapDelete, 2, 11, 0),

            // len1 = len(m)
            Instruction::new(Opcode::MapLen, 13, 2, 0),
            Instruction::new(Opcode::GlobalSet, 3, 13, 0),

            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let module = build_module(globals, constants, vec![], vec![], vec![], vec![func], 0);
    let vm = run_module(Vm::new(), module);

    assert_eq!(vm.globals[0], 10);
    assert_eq!(vm.globals[1], 1);
    assert_eq!(vm.globals[2], 2);
    assert_eq!(vm.globals[3], 1);
}

#[test]
fn test_slice_append_len_get() {
    let globals = vec![
        GlobalDef {
            name: "g_val".into(),
            slots: 1,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
        },
        GlobalDef {
            name: "g_len".into(),
            slots: 1,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
        },
    ];

    let elem_meta_raw = ValueMeta::new(0, ValueKind::Int64).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(elem_meta_raw);
    let (b_0, c_0) = imm32_parts(0);
    let (b_7, c_7) = imm32_parts(7);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 10,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 10],
        code: vec![
            Instruction::new(Opcode::LoadInt, 1, b_meta, c_meta),
            Instruction::new(Opcode::LoadInt, 2, b_0, c_0),
            Instruction::new(Opcode::LoadInt, 3, b_0, c_0),
            Instruction::with_flags(Opcode::SliceNew, 1, 0, 1, 2),

            Instruction::new(Opcode::LoadInt, 4, b_7, c_7),
            Instruction::with_flags(Opcode::SliceAppend, 1, 0, 0, 4),

            Instruction::new(Opcode::SliceLen, 5, 0, 0),
            Instruction::new(Opcode::LoadInt, 6, b_0, c_0),
            Instruction::with_flags(Opcode::SliceGet, 1, 7, 0, 6),

            Instruction::new(Opcode::GlobalSet, 0, 7, 0),
            Instruction::new(Opcode::GlobalSet, 1, 5, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let module = build_module(globals, vec![], vec![], vec![], vec![], vec![func], 0);
    let vm = run_module(Vm::new(), module);

    assert_eq!(vm.globals[0], 7);
    assert_eq!(vm.globals[1], 1);
}

fn extern_add(ret: &mut [u64], args: &[u64]) -> ExternCallResult {
    ret[0] = args[0].wrapping_add(args[1]);
    ExternCallResult::Ok
}

#[test]
fn test_go_chan_send_then_recv_after_yield() {
    // main:
    // 1) make buffered chan cap=1
    // 2) create closure(go_func) captures: ch, val
    // 3) GoCall + Yield (let go fiber run first)
    // 4) ChanRecv succeeds (no blocking)
    // 5) write (val, ok) to globals
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 2,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
    }];

    // --- go fiber function ---
    // reg0 = closure
    // cap0 = ch, cap1 = val
    let go_func = FunctionDef {
        name: "go_func".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 4,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 4],
        code: vec![
            Instruction::new(Opcode::ClosureGet, 1, 0, 0),
            Instruction::new(Opcode::ClosureGet, 2, 1, 0),
            Instruction::with_flags(Opcode::ChanSend, 1, 1, 2, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let elem_meta_raw = ValueMeta::new(0, ValueKind::Int64).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(elem_meta_raw);
    let (b_cap, c_cap) = imm32_parts(1);
    let (b_val, c_val) = imm32_parts(7);

    let main = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 10,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 10],
        code: vec![
            // ch = make(chan int64, 1)
            Instruction::new(Opcode::LoadInt, 0, b_meta, c_meta),
            Instruction::new(Opcode::LoadInt, 1, b_cap, c_cap),
            Instruction::with_flags(Opcode::ChanNew, 1, 2, 0, 1),

            // closure(go_func) with 2 captures
            Instruction::with_flags(Opcode::ClosureNew, 0, 3, 1, 2),
            Instruction::new(Opcode::LoadInt, 4, b_val, c_val),
            // cap0 = ch
            Instruction::new(Opcode::Copy, 5, 2, 0),
            Instruction::new(Opcode::Copy, 0, 3, 0),
            Instruction::new(Opcode::ClosureSet, 0, 5, 0),
            // cap1 = val
            Instruction::new(Opcode::ClosureSet, 1, 4, 0),

            // go closure
            Instruction::new(Opcode::GoCall, 0, 0, 0),
            // let go fiber run and send first
            Instruction::new(Opcode::Yield, 0, 0, 0),

            // recv
            Instruction::with_flags(Opcode::ChanRecv, 3, 6, 2, 0),
            Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 6, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let module = build_module(
        globals,
        vec![],
        vec![],
        vec![],
        vec![],
        vec![main, go_func],
        0,
    );

    let vm = run_module(Vm::new(), module);
    assert_eq!(vm.globals[0], 7);
    assert_eq!(vm.globals[1], 1);
}

#[test]
fn test_extern_call_add() {
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
    }];

    let externs = vec![ExternDef {
        name: "add".into(),
        param_slots: 2,
        ret_slots: 1,
    }];

    let (b_40, c_40) = imm32_parts(40);
    let (b_2, c_2) = imm32_parts(2);
    let call_c = ((2u16 as u16) << 8) | 1u16;

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 4,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 4],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_40, c_40),
            Instruction::new(Opcode::LoadInt, 1, b_2, c_2),
            Instruction::with_flags(Opcode::CallExtern, 0, 0, 0, call_c),
            Instruction::new(Opcode::GlobalSet, 0, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let module = build_module(globals, vec![], vec![], vec![], externs, vec![func], 0);

    let mut vm = Vm::new();
    vm.extern_registry.register(0, extern_add);

    let vm = run_module(vm, module);
    assert_eq!(vm.globals[0], 42);
}
