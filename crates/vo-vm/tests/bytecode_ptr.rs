use vo_common_core::types::{SlotType, ValueKind, ValueMeta};
use vo_vm::bytecode::{Constant, ExternDef, FunctionDef, GlobalDef, InterfaceMeta, Module, StructMeta};
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
    functions: Vec<FunctionDef>,
) -> Module {
    let mut module = Module::new("test_ptr".into());
    module.globals = globals;
    module.constants = constants;
    module.struct_metas = struct_metas;
    module.functions = functions;
    module.entry_func = 0;
    module.externs = Vec::<ExternDef>::new();
    module.interface_metas = Vec::<InterfaceMeta>::new();
    module
}

fn run_module(module: Module) -> Vm {
    let mut vm = Vm::new();
    vm.load(module);
    vm.run().unwrap();
    vm
}

#[test]
fn test_ptr_struct_escape() {
    // Simulate:
    // type Point struct { x, y int64 }
    // p := new(Point) // escape
    // p.x = 10
    // p.y = 20
    // g_x = p.x
    // g_y = p.y

    let globals = vec![
        GlobalDef { name: "g_x".into(), slots: 1, value_kind: ValueKind::Int64 as u8, meta_id: 0 },
        GlobalDef { name: "g_y".into(), slots: 1, value_kind: ValueKind::Int64 as u8, meta_id: 0 },
    ];

    // Point struct meta (id 0)
    let struct_metas = vec![StructMeta {
        name: "Point".into(),
        slot_types: vec![SlotType::Value, SlotType::Value],
        field_names: vec!["x".into(), "y".into()],
        field_offsets: vec![0, 1],
        methods: Default::default(),
    }];

    // ValueMeta for Point struct (kind=Struct, meta_id=0)
    let point_meta = ValueMeta::new(0, ValueKind::Struct).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(point_meta);
    
    let (b_10, c_10) = imm32_parts(10);
    let (b_20, c_20) = imm32_parts(20);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 10,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 10],
        code: vec![
            // r0 = point_meta
            Instruction::new(Opcode::LoadInt, 0, b_meta, c_meta),
            // r1 = new(Point): with_flags(op, flags=2, a=1, b=0, c=0)
            Instruction::with_flags(Opcode::PtrNew, 2, 1, 0, 0),
            
            // r2 = 10, r3 = 20
            Instruction::new(Opcode::LoadInt, 2, b_10, c_10),
            Instruction::new(Opcode::LoadInt, 3, b_20, c_20),
            
            // p.x (offset 0) = 10
            Instruction::new(Opcode::PtrSet, 1, 0, 2),
            // p.y (offset 1) = 20
            Instruction::new(Opcode::PtrSet, 1, 1, 3),
            
            // r4 = p.x
            Instruction::new(Opcode::PtrGet, 4, 1, 0),
            // r5 = p.y
            Instruction::new(Opcode::PtrGet, 5, 1, 1),
            
            // global set
            Instruction::new(Opcode::GlobalSet, 0, 4, 0),
            Instruction::new(Opcode::GlobalSet, 1, 5, 0),
            
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };

    let module = build_module(globals, vec![], struct_metas, vec![func]);
    let vm = run_module(module);
    
    assert_eq!(vm.globals[0], 10);
    assert_eq!(vm.globals[1], 20);
}

#[test]
fn test_ptr_copy_reference_semantics() {
    // Test pointer reference semantics:
    // p1 := new(int64)
    // *p1 = 100
    // p2 := p1 // Copy pointer (reference semantics, NOT deep copy)
    // *p1 = 200
    // g_val = *p2 // should be 200 (same object)
    
    let globals = vec![
        GlobalDef { name: "g_val".into(), slots: 1, value_kind: ValueKind::Int64 as u8, meta_id: 0 },
    ];

    let int_meta = ValueMeta::new(0, ValueKind::Int64).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(int_meta);
    let (b_100, c_100) = imm32_parts(100);
    let (b_200, c_200) = imm32_parts(200);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 6,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 6],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_meta, c_meta),
            // r1 = new(int64), 1 slot
            Instruction::with_flags(Opcode::PtrNew, 1, 1, 0, 0),
            
            // *p1 = 100
            Instruction::new(Opcode::LoadInt, 2, b_100, c_100),
            Instruction::new(Opcode::PtrSet, 1, 0, 2),
            
            // r3 = r1 (Copy pointer, reference semantics)
            Instruction::new(Opcode::Copy, 3, 1, 0),
            
            // *p1 = 200
            Instruction::new(Opcode::LoadInt, 4, b_200, c_200),
            Instruction::new(Opcode::PtrSet, 1, 0, 4),
            
            // r5 = *p2 (should be 200 since p1 and p2 point to same object)
            Instruction::new(Opcode::PtrGet, 5, 3, 0),
            
            Instruction::new(Opcode::GlobalSet, 0, 5, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };
    
    let module = build_module(globals, vec![], vec![], vec![func]);
    let vm = run_module(module);
    
    assert_eq!(vm.globals[0], 200);
}

#[test]
fn test_ptr_clone_value_semantics() {
    // Test PtrClone deep copy (value semantics for escaped struct):
    // p1 := new(int64)
    // *p1 = 100
    // p2 := clone(p1) // Deep copy (value semantics)
    // *p1 = 200
    // g_val = *p2 // should be 100 (separate object)
    
    let globals = vec![
        GlobalDef { name: "g_val".into(), slots: 1, value_kind: ValueKind::Int64 as u8, meta_id: 0 },
    ];

    let int_meta = ValueMeta::new(0, ValueKind::Int64).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(int_meta);
    let (b_100, c_100) = imm32_parts(100);
    let (b_200, c_200) = imm32_parts(200);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 6,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 6],
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_meta, c_meta),
            // r1 = new(int64), 1 slot
            Instruction::with_flags(Opcode::PtrNew, 1, 1, 0, 0),
            
            // *p1 = 100
            Instruction::new(Opcode::LoadInt, 2, b_100, c_100),
            Instruction::new(Opcode::PtrSet, 1, 0, 2),
            
            // r3 = clone(r1) (PtrClone = deep copy)
            Instruction::new(Opcode::PtrClone, 3, 1, 0),
            
            // *p1 = 200
            Instruction::new(Opcode::LoadInt, 4, b_200, c_200),
            Instruction::new(Opcode::PtrSet, 1, 0, 4),
            
            // r5 = *p2 (should be 100 since p2 is a separate copy)
            Instruction::new(Opcode::PtrGet, 5, 3, 0),
            
            Instruction::new(Opcode::GlobalSet, 0, 5, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };
    
    let module = build_module(globals, vec![], vec![], vec![func]);
    let vm = run_module(module);
    
    assert_eq!(vm.globals[0], 100);
}

#[test]
fn test_ptr_nested_linked_list() {
    // Simulate linked list:
    // type Node struct { val int64; next *Node }
    // node1 := new(Node); node1.val = 1; node1.next = nil
    // node2 := new(Node); node2.val = 2; node2.next = node1
    // g_val = node2.next.val // should be 1
    
    let globals = vec![
        GlobalDef { name: "g_val".into(), slots: 1, value_kind: ValueKind::Int64 as u8, meta_id: 0 },
    ];

    // Node struct meta (id 0)
    // slot 0: val (Value)
    // slot 1: next (GcRef/Pointer)
    let struct_metas = vec![StructMeta {
        name: "Node".into(),
        slot_types: vec![SlotType::Value, SlotType::GcRef],
        field_names: vec!["val".into(), "next".into()],
        field_offsets: vec![0, 1],
        methods: Default::default(),
    }];
    
    let node_meta = ValueMeta::new(0, ValueKind::Struct).to_raw() as i32;
    let (b_meta, c_meta) = imm32_parts(node_meta);
    let (b_1, c_1) = imm32_parts(1);
    let (b_2, c_2) = imm32_parts(2);

    let func = FunctionDef {
        name: "main".into(),
        param_count: 0,
        param_slots: 0,
        local_slots: 10,
        ret_slots: 0,
        slot_types: vec![SlotType::Value; 10], // Note: registers hold refs as u64
        code: vec![
            Instruction::new(Opcode::LoadInt, 0, b_meta, c_meta),
            
            // --- node1 ---
            // r1 = new(Node), 2 slots
            Instruction::with_flags(Opcode::PtrNew, 2, 1, 0, 0),
            // node1.val = 1
            Instruction::new(Opcode::LoadInt, 2, b_1, c_1),
            Instruction::new(Opcode::PtrSet, 1, 0, 2),
            // node1.next = nil (implicitly 0, but let's be explicit with LoadNil)
            Instruction::new(Opcode::LoadNil, 3, 0, 0),
            Instruction::new(Opcode::PtrSet, 1, 1, 3),
            
            // --- node2 ---
            // r4 = new(Node), 2 slots
            Instruction::with_flags(Opcode::PtrNew, 2, 4, 0, 0),
            // node2.val = 2
            Instruction::new(Opcode::LoadInt, 5, b_2, c_2),
            Instruction::new(Opcode::PtrSet, 4, 0, 5),
            // node2.next = node1 (r1)
            Instruction::new(Opcode::PtrSet, 4, 1, 1),
            
            // --- access ---
            // r6 = node2.next (should be node1)
            Instruction::new(Opcode::PtrGet, 6, 4, 1),
            
            // check if r6 is nil (it shouldn't be)
            // Skip check for brevity, assume correct
            
            // r7 = r6.val (should be node1.val = 1)
            Instruction::new(Opcode::PtrGet, 7, 6, 0),
            
            Instruction::new(Opcode::GlobalSet, 0, 7, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
    };
    
    let module = build_module(globals, vec![], struct_metas, vec![func]);
    let vm = run_module(module);
    
    assert_eq!(vm.globals[0], 1);
}
