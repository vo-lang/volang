//! Bytecode tests for VM verification.
//!
//! Run with: `gox run-bytecode --test <name>`

use gox_vm::{Module, FunctionDef, Instruction, Opcode, Constant, VmResult};

/// Run a bytecode test by name.
pub fn run_test(name: &str) -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Running bytecode test: {} ===\n", name);
    
    match name {
        "arithmetic" => test_arithmetic(),
        "factorial" => test_factorial(),
        "ffi" => test_ffi(),
        "control" => test_control_flow(),
        "object" => test_object(),
        "slice" => test_slice(),
        "map" => test_map(),
        "channel" => test_channel(),
        "all" => {
            test_arithmetic()?;
            test_factorial()?;
            test_ffi()?;
            test_control_flow()?;
            test_object()?;
            test_slice()?;
            test_map()?;
            test_channel()?;
            Ok(())
        }
        _ => Err(format!("unknown test: {}. Available: arithmetic, factorial, ffi, control, object, slice, map, channel, all", name).into()),
    }
}

/// Test: 1 + 2 * 3 = 7
fn test_arithmetic() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Test: arithmetic (1 + 2 * 3 = 7) ---");
    
    let mut module = Module::new("test");
    
    // main function
    let mut main_fn = FunctionDef::new("main");
    main_fn.local_slots = 10;
    
    // r0 = 1
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 0, 1, 0));
    // r1 = 2
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 2, 0));
    // r2 = 3
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 3, 0));
    // r3 = r1 * r2 (2 * 3 = 6)
    main_fn.code.push(Instruction::new(Opcode::MulI64, 3, 1, 2));
    // r4 = r0 + r3 (1 + 6 = 7)
    main_fn.code.push(Instruction::new(Opcode::AddI64, 4, 0, 3));
    // debug print r4
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 4, 0, 0));
    // return
    main_fn.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    module.add_function(main_fn);
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => println!("✓ arithmetic test passed\n"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        _ => return Err("unexpected result".into()),
    }
    
    Ok(())
}

/// Test: factorial(5) = 120
fn test_factorial() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Test: factorial(5) = 120 ---");
    
    let mut module = Module::new("test");
    
    // factorial(n) function at index 0
    // if n <= 1: return 1
    // else: return n * factorial(n-1)
    let mut fact_fn = FunctionDef::new("factorial");
    fact_fn.param_count = 1;
    fact_fn.param_slots = 1;
    fact_fn.local_slots = 10;
    fact_fn.ret_slots = 1;
    
    // r0 = n (parameter)
    // r1 = 1
    fact_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 1, 0));
    // r2 = (n <= 1)
    fact_fn.code.push(Instruction::new(Opcode::LeI64, 2, 0, 1));
    // if r2: jump to return 1
    fact_fn.code.push(Instruction::new(Opcode::JumpIf, 2, 4, 0)); // jump +4
    // r3 = n - 1
    fact_fn.code.push(Instruction::new(Opcode::SubI64, 3, 0, 1));
    // r4 = factorial(r3) - call func 0, arg at r3, 1 arg, 1 ret
    fact_fn.code.push(Instruction::with_flags(Opcode::Call, 1, 0, 3, 1));
    // r5 = n * r4
    fact_fn.code.push(Instruction::new(Opcode::MulI64, 5, 0, 3));
    // return r5
    fact_fn.code.push(Instruction::new(Opcode::Return, 5, 1, 0));
    // return 1
    fact_fn.code.push(Instruction::new(Opcode::Return, 1, 1, 0));
    
    module.add_function(fact_fn);
    
    // main function at index 1
    let mut main_fn = FunctionDef::new("main");
    main_fn.local_slots = 10;
    
    // r0 = 5
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 0, 5, 0));
    // r1 = factorial(r0) - call func 0, arg at r0, 1 arg, 1 ret
    main_fn.code.push(Instruction::with_flags(Opcode::Call, 1, 0, 0, 1));
    // debug print r0 (result is stored back at arg position)
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 0, 0, 0));
    // return
    main_fn.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    module.add_function(main_fn);
    module.entry_func = 1; // main is at index 1
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => println!("✓ factorial test passed\n"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        _ => return Err("unexpected result".into()),
    }
    
    Ok(())
}

/// Test: call native fmt.Println
fn test_ffi() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Test: FFI (fmt.Println) ---");
    
    let mut module = Module::new("test");
    
    // Add string constant
    let hello_idx = module.add_constant(Constant::String("Hello from VM!".to_string()));
    
    // Add native function reference
    let println_id = module.add_extern("fmt.Println", 1, 1);
    
    // main function
    let mut main_fn = FunctionDef::new("main");
    main_fn.local_slots = 10;
    
    // r0 = "Hello from VM!"
    main_fn.code.push(Instruction::new(Opcode::LoadConst, 0, hello_idx, 0));
    // call fmt.Println(r0)
    main_fn.code.push(Instruction::with_flags(Opcode::CallExtern, 1, println_id as u16, 0, 1));
    // return
    main_fn.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    module.add_function(main_fn);
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => println!("✓ FFI test passed\n"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        _ => return Err("unexpected result".into()),
    }
    
    Ok(())
}

/// Test: control flow (if/else, loops)
fn test_control_flow() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Test: control flow (sum 1 to 10 = 55) ---");
    
    let mut module = Module::new("test");
    
    // main function: sum 1 to 10
    let mut main_fn = FunctionDef::new("main");
    main_fn.local_slots = 10;
    
    // r0 = sum = 0
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 0, 0, 0));
    // r1 = i = 1
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 1, 0));
    // r2 = 10
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 10, 0));
    // r3 = 1 (for increment)
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 3, 1, 0));
    
    // loop start (pc=4):
    // r4 = (i > 10)
    main_fn.code.push(Instruction::new(Opcode::GtI64, 4, 1, 2));
    // if r4: jump to end (+4)
    main_fn.code.push(Instruction::new(Opcode::JumpIf, 4, 4, 0));
    // sum = sum + i
    main_fn.code.push(Instruction::new(Opcode::AddI64, 0, 0, 1));
    // i = i + 1
    main_fn.code.push(Instruction::new(Opcode::AddI64, 1, 1, 3));
    // jump back to loop start (-5)
    main_fn.code.push(Instruction::new(Opcode::Jump, 0, (-5i32) as u16, ((-5i32) >> 16) as u16));
    
    // end:
    // debug print sum
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 0, 0, 0));
    // return
    main_fn.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    module.add_function(main_fn);
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => println!("✓ control flow test passed\n"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        _ => return Err("unexpected result".into()),
    }
    
    Ok(())
}

/// Test: object allocation and field access
fn test_object() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Test: object (alloc, get/set field) ---");
    
    let mut module = Module::new("test");
    
    // main function
    let mut main_fn = FunctionDef::new("main");
    main_fn.local_slots = 10;
    
    // r0 = alloc object with type 17 (MAP type, has slots), 2 extra slots
    main_fn.code.push(Instruction::new(Opcode::Alloc, 0, 17, 2));
    // r1 = 42
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 42, 0));
    // r2 = 100
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 100, 0));
    // obj.field[0] = 42
    main_fn.code.push(Instruction::new(Opcode::SetField, 0, 0, 1));
    // obj.field[1] = 100
    main_fn.code.push(Instruction::new(Opcode::SetField, 0, 1, 2));
    // r3 = obj.field[0]
    main_fn.code.push(Instruction::new(Opcode::GetField, 3, 0, 0));
    // r4 = obj.field[1]
    main_fn.code.push(Instruction::new(Opcode::GetField, 4, 0, 1));
    // r5 = r3 + r4 (should be 142)
    main_fn.code.push(Instruction::new(Opcode::AddI64, 5, 3, 4));
    // debug print r5
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 5, 0, 0));
    // return
    main_fn.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    module.add_function(main_fn);
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => println!("✓ object test passed\n"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        _ => return Err("unexpected result".into()),
    }
    
    Ok(())
}

/// Test: slice operations
fn test_slice() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Test: slice (create, append, access) ---");
    
    let mut module = Module::new("test");
    
    // main function
    let mut main_fn = FunctionDef::new("main");
    main_fn.local_slots = 20;
    
    // Create array with 3 elements
    // r0 = new array, elem_type=INT(2), len=3
    main_fn.code.push(Instruction::new(Opcode::ArrayNew, 0, 2, 3));
    // Set array[0] = 10
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 10, 0));
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 0, 0)); // index 0
    main_fn.code.push(Instruction::new(Opcode::ArraySet, 0, 2, 1));
    // Set array[1] = 20
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 20, 0));
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 1, 0)); // index 1
    main_fn.code.push(Instruction::new(Opcode::ArraySet, 0, 2, 1));
    // Set array[2] = 30
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 30, 0));
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 2, 0)); // index 2
    main_fn.code.push(Instruction::new(Opcode::ArraySet, 0, 2, 1));
    
    // r3 = array[1] (should be 20)
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 1, 0));
    main_fn.code.push(Instruction::new(Opcode::ArrayGet, 3, 0, 2));
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 3, 0, 0));
    
    // r4 = len(array) (should be 3)
    main_fn.code.push(Instruction::new(Opcode::ArrayLen, 4, 0, 0));
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 4, 0, 0));
    
    // return
    main_fn.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    module.add_function(main_fn);
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => println!("✓ slice test passed\n"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        _ => return Err("unexpected result".into()),
    }
    
    Ok(())
}

/// Test: map operations
fn test_map() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Test: map (create, set, get) ---");
    
    let mut module = Module::new("test");
    
    // main function
    let mut main_fn = FunctionDef::new("main");
    main_fn.local_slots = 20;
    
    // r0 = make(map[int]int)
    main_fn.code.push(Instruction::new(Opcode::MapNew, 0, 2, 2)); // key=INT, val=INT
    
    // map[1] = 100
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 1, 0)); // key
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 100, 0)); // value
    main_fn.code.push(Instruction::new(Opcode::MapSet, 0, 1, 2));
    
    // map[2] = 200
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 2, 0)); // key
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 2, 200, 0)); // value
    main_fn.code.push(Instruction::new(Opcode::MapSet, 0, 1, 2));
    
    // r3, r4 = map[1] (should be 100, ok=true)
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 1, 0)); // key
    main_fn.code.push(Instruction::new(Opcode::MapGet, 3, 0, 1)); // r3=val, r4=ok
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 3, 0, 0)); // print value
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 4, 0, 0)); // print ok
    
    // r5 = len(map) (should be 2)
    main_fn.code.push(Instruction::new(Opcode::MapLen, 5, 0, 0));
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 5, 0, 0));
    
    // return
    main_fn.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    module.add_function(main_fn);
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => println!("✓ map test passed\n"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        _ => return Err("unexpected result".into()),
    }
    
    Ok(())
}

/// Test: channel operations
fn test_channel() -> Result<(), Box<dyn std::error::Error>> {
    println!("--- Test: channel (buffered send/recv) ---");
    
    let mut module = Module::new("test");
    
    // main function
    let mut main_fn = FunctionDef::new("main");
    main_fn.local_slots = 20;
    
    // r0 = make(chan int, 2) - buffered channel with cap 2
    main_fn.code.push(Instruction::new(Opcode::ChanNew, 0, 2, 2)); // elem=INT, cap=2
    
    // send 42 to channel
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 42, 0));
    main_fn.code.push(Instruction::new(Opcode::ChanSend, 0, 1, 0));
    
    // send 100 to channel
    main_fn.code.push(Instruction::new(Opcode::LoadInt, 1, 100, 0));
    main_fn.code.push(Instruction::new(Opcode::ChanSend, 0, 1, 0));
    
    // r2, r3 = <-channel (should be 42, ok=true)
    main_fn.code.push(Instruction::new(Opcode::ChanRecv, 2, 0, 3));
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 2, 0, 0)); // print value
    
    // r4, r5 = <-channel (should be 100, ok=true)
    main_fn.code.push(Instruction::new(Opcode::ChanRecv, 4, 0, 5));
    main_fn.code.push(Instruction::new(Opcode::DebugPrint, 4, 0, 0)); // print value
    
    // return
    main_fn.code.push(Instruction::new(Opcode::Return, 0, 0, 0));
    
    module.add_function(main_fn);
    
    let mut vm = gox_runtime_vm::create_vm();
    vm.load_module(module);
    
    match vm.run() {
        VmResult::Done => println!("✓ channel test passed\n"),
        VmResult::Panic(msg) => return Err(format!("panic: {}", msg).into()),
        _ => return Err("unexpected result".into()),
    }
    
    Ok(())
}
