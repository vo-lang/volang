use vo_common_core::types::{SlotType, ValueKind, ValueMeta};
use vo_vm::bytecode::{
    Constant, ExternDef, FunctionDef, GlobalDef, InterfaceMeta, Module, StructMeta,
};
use vo_vm::instruction::{Instruction, Opcode};
use vo_vm::vm::Vm;

fn imm32_parts(v: i32) -> (u16, u16) {
    let u = v as u32;
    (u as u16, (u >> 16) as u16)
}

fn run_single_func_module(
    globals: Vec<GlobalDef>,
    constants: Vec<Constant>,
    struct_metas: Vec<StructMeta>,
    interface_metas: Vec<InterfaceMeta>,
    func: FunctionDef,
) -> Vm {
    let mut module = Module::new("test".into());
    module.globals = globals;
    module.constants = constants;
    module.struct_metas = struct_metas;
    module.interface_metas = interface_metas;
    module.functions.push(func);
    module.externs = Vec::<ExternDef>::new();
    module.entry_func = 0;

    let mut vm = Vm::new();
    vm.load(module);
    vm.run().unwrap();
    vm
}

#[test]
fn test_load_arith_global() {
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
    }];

    let (b1, c1) = imm32_parts(1);
    let (b2, c2) = imm32_parts(2);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 3,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 3],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b1, c1),
            Instruction::new(Opcode::LoadInt, 1, b2, c2),
            Instruction::new(Opcode::AddI, 2, 0, 1),
            Instruction::new(Opcode::GlobalSet, 0, 2, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let vm = run_single_func_module(globals, vec![], vec![], vec![], func);
    assert_eq!(vm.state.globals[0], 3);
}

#[test]
fn test_jump_if_else() {
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
    }];

    let (b_true, c_true) = imm32_parts(1);
    let (b_111, c_111) = imm32_parts(111);
    let (b_222, c_222) = imm32_parts(222);

    let (b_jifnot, c_jifnot) = imm32_parts(4);
    let (b_jend, c_jend) = imm32_parts(3);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 2,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 2],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_true, c_true),
            Instruction::new(Opcode::JumpIfNot, 0, b_jifnot, c_jifnot),
            Instruction::new(Opcode::LoadInt, 1, b_111, c_111),
            Instruction::new(Opcode::GlobalSet, 0, 1, 0),
            Instruction::new(Opcode::Jump, 0, b_jend, c_jend),
            Instruction::new(Opcode::LoadInt, 1, b_222, c_222),
            Instruction::new(Opcode::GlobalSet, 0, 1, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let vm = run_single_func_module(globals, vec![], vec![], vec![], func);
    assert_eq!(vm.state.globals[0], 111);
}

#[test]
fn test_ptr_new_set_get() {
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
    }];

    let meta_raw = ValueMeta::new(0, ValueKind::Pointer).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(meta_raw);
    let (b_99, c_99) = imm32_parts(99);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 4,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 4],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_meta, c_meta),
            Instruction::with_flags(Opcode::PtrNew, 1, 1, 0, 0),
            Instruction::new(Opcode::LoadInt, 2, b_99, c_99),
            Instruction::new(Opcode::PtrSet, 1, 0, 2),
            Instruction::new(Opcode::PtrGet, 3, 1, 0),
            Instruction::new(Opcode::GlobalSet, 0, 3, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let vm = run_single_func_module(globals, vec![], vec![], vec![], func);
    assert_eq!(vm.state.globals[0], 99);
}

#[test]
fn test_string_new_concat_len() {
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 1,
        value_kind: ValueKind::Int64 as u8,
        meta_id: 0,
    }];

    let constants = vec![
        Constant::String("hi".into()),
        Constant::String("there".into()),
    ];

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 4,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 4],
        code: vec![
            Instruction::new(Opcode::StrNew, 0, 0, 0),
            Instruction::new(Opcode::StrNew, 1, 1, 0),
            Instruction::new(Opcode::StrConcat, 2, 0, 1),
            Instruction::new(Opcode::StrLen, 3, 2, 0),
            Instruction::new(Opcode::GlobalSet, 0, 3, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let vm = run_single_func_module(globals, constants, vec![], vec![], func);
    assert_eq!(vm.state.globals[0], ("hi".len() + "there".len()) as u64);
}

#[test]
fn test_array_set_get_len() {
    let globals = vec![
        GlobalDef {
            name: "g0".into(),
            slots: 1,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
        },
        GlobalDef {
            name: "g1".into(),
            slots: 1,
            value_kind: ValueKind::Int64 as u8,
            meta_id: 0,
        },
    ];

    let elem_meta_raw = ValueMeta::new(0, ValueKind::Int64).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(elem_meta_raw);
    let (b_len, c_len) = imm32_parts(3);
    let (b_idx, c_idx) = imm32_parts(1);
    let (b_val, c_val) = imm32_parts(42);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 7,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 7],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_meta, c_meta),
            Instruction::new(Opcode::LoadInt, 1, b_len, c_len),
            Instruction::with_flags(Opcode::ArrayNew, 1, 2, 0, 1),
            Instruction::new(Opcode::LoadInt, 3, b_idx, c_idx),
            Instruction::new(Opcode::LoadInt, 4, b_val, c_val),
            Instruction::with_flags(Opcode::ArraySet, 1, 2, 3, 4),
            Instruction::with_flags(Opcode::ArrayGet, 1, 5, 2, 3),
            Instruction::new(Opcode::ArrayLen, 6, 2, 0),
            Instruction::new(Opcode::GlobalSet, 0, 5, 0),
            Instruction::new(Opcode::GlobalSet, 1, 6, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let vm = run_single_func_module(globals, vec![], vec![], vec![], func);
    assert_eq!(vm.state.globals[0], 42);
    assert_eq!(vm.state.globals[1], 3);
}

#[test]
fn test_channel_send_recv_buffered() {
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 2,
        value_kind: ValueKind::Channel as u8,
        meta_id: 0,
    }];

    let elem_meta_raw = ValueMeta::new(0, ValueKind::Int64).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(elem_meta_raw);
    let (b_cap, c_cap) = imm32_parts(1);
    let (b_val, c_val) = imm32_parts(7);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 6,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 6],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_meta, c_meta),
            Instruction::new(Opcode::LoadInt, 1, b_cap, c_cap),
            Instruction::with_flags(Opcode::ChanNew, 1, 2, 0, 1),
            Instruction::new(Opcode::LoadInt, 3, b_val, c_val),
            Instruction::with_flags(Opcode::ChanSend, 1, 2, 3, 0),
            Instruction::with_flags(Opcode::ChanRecv, 3, 4, 2, 0),
            Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 4, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let vm = run_single_func_module(globals, vec![], vec![], vec![], func);
    assert_eq!(vm.state.globals[0], 7);
    assert_eq!(vm.state.globals[1], 1);
}

#[test]
fn test_iface_assign_empty_from_int() {
    let globals = vec![GlobalDef {
        name: "g0".into(),
        slots: 2,
        value_kind: ValueKind::Interface as u8,
        meta_id: 0,
    }];

    let iface_metas = vec![InterfaceMeta {
        name: "empty".into(),
        method_names: vec![],
    }];

    // Constant for IfaceAssign: (named_type_id << 32) | itab_id
    // For primitive int: named_type_id = 0, itab_id = 0
    let constants = vec![Constant::Int(0)];

    let (b_val, c_val) = imm32_parts(123);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 3,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 3],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_val, c_val),
            // IfaceAssign: dst=1, src=0, const_idx=0, flags=value_kind
            Instruction::with_flags(Opcode::IfaceAssign, ValueKind::Int as u8, 1, 0, 0),
            Instruction::with_flags(Opcode::GlobalSetN, 2, 0, 1, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let vm = run_single_func_module(globals, constants, vec![], iface_metas, func);
    let slot0 = vm.state.globals[0];
    let slot1 = vm.state.globals[1];

    assert_eq!((slot0 >> 32) as u32, 0);
    assert_eq!((slot0 as u32) & 0xFF, ValueKind::Int as u32);
    assert_eq!(slot1, 123);
}
