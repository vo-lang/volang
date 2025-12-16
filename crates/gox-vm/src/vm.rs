//! Virtual machine implementation.

use crate::bytecode::{Constant, Module};
use crate::fiber::{BlockReason, DeferEntry, FiberId, FiberStatus, IterState, Scheduler};
use crate::gc::{Gc, GcRef, NULL_REF};
use crate::instruction::{Instruction, Opcode};
use crate::native::{NativeCtx, NativeFn, NativeRegistry, NativeResult};
use crate::objects::{self, array, channel, closure, interface, map, slice, string};
use crate::types::{builtin, TypeId, TypeTable};

/// VM execution result.
#[derive(Debug)]
pub enum VmResult {
    Ok,
    Yield,
    Panic(String),
    Done,
}

/// The virtual machine.
pub struct Vm {
    pub gc: Gc,
    pub types: TypeTable,
    pub scheduler: Scheduler,
    pub natives: NativeRegistry,
    
    // Current module
    module: Option<Module>,
    
    // Preloaded string constants (GcRefs)
    string_constants: Vec<GcRef>,
    
    // Resolved native function pointers
    native_ptrs: Vec<Option<NativeFn>>,
    
    // Global variables storage
    globals: Vec<u64>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            types: TypeTable::new(),
            scheduler: Scheduler::new(),
            natives: NativeRegistry::new(),
            module: None,
            string_constants: Vec::new(),
            native_ptrs: Vec::new(),
            globals: Vec::new(),
        }
    }
    
    pub fn with_natives(natives: NativeRegistry) -> Self {
        Self {
            gc: Gc::new(),
            types: TypeTable::new(),
            scheduler: Scheduler::new(),
            natives,
            module: None,
            string_constants: Vec::new(),
            native_ptrs: Vec::new(),
            globals: Vec::new(),
        }
    }
    
    /// Load a module.
    pub fn load_module(&mut self, module: Module) {
        // Preload string constants
        self.string_constants.clear();
        for c in &module.constants {
            if let Constant::String(s) = c {
                let str_ref = string::from_rust_str(&mut self.gc, builtin::STRING, s);
                self.string_constants.push(str_ref);
            } else {
                self.string_constants.push(NULL_REF);
            }
        }
        
        // Resolve native functions
        self.native_ptrs.clear();
        for native in &module.natives {
            let ptr = self.natives.get(&native.name);
            self.native_ptrs.push(ptr);
        }
        
        // Allocate global variables
        self.globals.clear();
        let total_slots: usize = module.globals.iter().map(|g| g.slots as usize).sum();
        self.globals.resize(total_slots, 0);
        
        self.module = Some(module);
    }
    
    /// Run the loaded module.
    pub fn run(&mut self) -> VmResult {
        let module = self.module.as_ref().expect("no module loaded");
        let entry = module.entry_func;
        
        // Create main fiber
        let fiber_id = self.scheduler.spawn(1024);
        let fiber = self.scheduler.get_mut(fiber_id).unwrap();
        fiber.status = FiberStatus::Running;
        fiber.push_frame(entry, 0, module.functions[entry as usize].local_slots as usize, 0, 0);
        self.scheduler.current = Some(fiber_id);
        
        // Execute
        loop {
            match self.step() {
                VmResult::Ok => continue,
                VmResult::Yield => {
                    // Try to schedule another fiber
                    if self.scheduler.schedule().is_none() {
                        // All fibers blocked or done
                        if self.scheduler.all_done() {
                            return VmResult::Done;
                        }
                        // Deadlock
                        return VmResult::Panic("deadlock: all goroutines are asleep".to_string());
                    }
                }
                result => return result,
            }
        }
    }
    
    /// Execute one instruction.
    pub fn step(&mut self) -> VmResult {
        let fiber_id = match self.scheduler.current {
            Some(id) => id,
            None => return VmResult::Yield,
        };
        
        let module = self.module.as_ref().unwrap();
        
        // Get current instruction
        let (func_id, pc, instr) = {
            let fiber = self.scheduler.get(fiber_id).unwrap();
            let frame = match fiber.frame() {
                Some(f) => f,
                None => {
                    self.scheduler.kill(fiber_id);
                    return VmResult::Yield;
                }
            };
            
            let func = &module.functions[frame.func_id as usize];
            if frame.pc >= func.code.len() {
                // Implicit return
                return self.do_return(fiber_id, 0, 0);
            }
            
            (frame.func_id, frame.pc, func.code[frame.pc])
        };
        
        // Increment PC
        {
            let fiber = self.scheduler.get_mut(fiber_id).unwrap();
            fiber.frame_mut().unwrap().pc += 1;
        }
        
        // Execute instruction
        self.execute(fiber_id, func_id, pc, instr)
    }
    
    fn execute(&mut self, fiber_id: FiberId, _func_id: u32, _pc: usize, instr: Instruction) -> VmResult {
        let op = instr.opcode();
        let a = instr.a;
        let b = instr.b;
        let c = instr.c;
        let flags = instr.flags;
        
        match op {
            // ============ Load/Store ============
            Opcode::Nop => {}
            
            Opcode::LoadNil => {
                self.write_reg(fiber_id, a, 0);
            }
            
            Opcode::LoadTrue => {
                self.write_reg(fiber_id, a, 1);
            }
            
            Opcode::LoadFalse => {
                self.write_reg(fiber_id, a, 0);
            }
            
            Opcode::LoadInt => {
                let val = instr.imm32() as i64;
                self.write_reg(fiber_id, a, val as u64);
            }
            
            Opcode::LoadConst => {
                let module = self.module.as_ref().unwrap();
                let c = &module.constants[b as usize];
                let val = match c {
                    Constant::Nil => 0,
                    Constant::Bool(v) => if *v { 1 } else { 0 },
                    Constant::Int(v) => *v as u64,
                    Constant::Float(v) => v.to_bits(),
                    Constant::String(_) => self.string_constants[b as usize] as u64,
                };
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::Mov => {
                let val = self.read_reg(fiber_id, b);
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::MovN => {
                let vals: Vec<u64> = (0..c).map(|i| self.read_reg(fiber_id, b + i)).collect();
                for (i, v) in vals.into_iter().enumerate() {
                    self.write_reg(fiber_id, a + i as u16, v);
                }
            }
            
            // ============ Globals ============
            Opcode::GetGlobal => {
                let val = self.globals[b as usize];
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::SetGlobal => {
                let val = self.read_reg(fiber_id, b);
                self.globals[a as usize] = val;
            }
            
            // ============ Arithmetic (i64) ============
            Opcode::AddI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, vb.wrapping_add(vc) as u64);
            }
            
            Opcode::SubI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, vb.wrapping_sub(vc) as u64);
            }
            
            Opcode::MulI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, vb.wrapping_mul(vc) as u64);
            }
            
            Opcode::DivI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                if vc == 0 {
                    return VmResult::Panic("division by zero".to_string());
                }
                self.write_reg(fiber_id, a, vb.wrapping_div(vc) as u64);
            }
            
            Opcode::ModI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                if vc == 0 {
                    return VmResult::Panic("division by zero".to_string());
                }
                self.write_reg(fiber_id, a, vb.wrapping_rem(vc) as u64);
            }
            
            Opcode::NegI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                self.write_reg(fiber_id, a, (-vb) as u64);
            }
            
            // ============ Arithmetic (f64) ============
            Opcode::AddF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, (vb + vc).to_bits());
            }
            
            Opcode::SubF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, (vb - vc).to_bits());
            }
            
            Opcode::MulF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, (vb * vc).to_bits());
            }
            
            Opcode::DivF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, (vb / vc).to_bits());
            }
            
            Opcode::NegF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                self.write_reg(fiber_id, a, (-vb).to_bits());
            }
            
            // ============ Comparison (i64) ============
            Opcode::EqI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, if vb == vc { 1 } else { 0 });
            }
            
            Opcode::NeI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, if vb != vc { 1 } else { 0 });
            }
            
            Opcode::LtI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, if vb < vc { 1 } else { 0 });
            }
            
            Opcode::LeI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, if vb <= vc { 1 } else { 0 });
            }
            
            Opcode::GtI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, if vb > vc { 1 } else { 0 });
            }
            
            Opcode::GeI64 => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as i64;
                self.write_reg(fiber_id, a, if vb >= vc { 1 } else { 0 });
            }
            
            // ============ Comparison (f64) ============
            Opcode::EqF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, if vb == vc { 1 } else { 0 });
            }
            
            Opcode::NeF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, if vb != vc { 1 } else { 0 });
            }
            
            Opcode::LtF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, if vb < vc { 1 } else { 0 });
            }
            
            Opcode::LeF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, if vb <= vc { 1 } else { 0 });
            }
            
            Opcode::GtF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, if vb > vc { 1 } else { 0 });
            }
            
            Opcode::GeF64 => {
                let vb = f64::from_bits(self.read_reg(fiber_id, b));
                let vc = f64::from_bits(self.read_reg(fiber_id, c));
                self.write_reg(fiber_id, a, if vb >= vc { 1 } else { 0 });
            }
            
            // ============ Reference comparison ============
            Opcode::EqRef => {
                let vb = self.read_reg(fiber_id, b);
                let vc = self.read_reg(fiber_id, c);
                self.write_reg(fiber_id, a, if vb == vc { 1 } else { 0 });
            }
            
            Opcode::NeRef => {
                let vb = self.read_reg(fiber_id, b);
                let vc = self.read_reg(fiber_id, c);
                self.write_reg(fiber_id, a, if vb != vc { 1 } else { 0 });
            }
            
            Opcode::IsNil => {
                let vb = self.read_reg(fiber_id, b);
                self.write_reg(fiber_id, a, if vb == 0 { 1 } else { 0 });
            }
            
            // ============ Bitwise ============
            Opcode::Band => {
                let vb = self.read_reg(fiber_id, b);
                let vc = self.read_reg(fiber_id, c);
                self.write_reg(fiber_id, a, vb & vc);
            }
            
            Opcode::Bor => {
                let vb = self.read_reg(fiber_id, b);
                let vc = self.read_reg(fiber_id, c);
                self.write_reg(fiber_id, a, vb | vc);
            }
            
            Opcode::Bxor => {
                let vb = self.read_reg(fiber_id, b);
                let vc = self.read_reg(fiber_id, c);
                self.write_reg(fiber_id, a, vb ^ vc);
            }
            
            Opcode::Bnot => {
                let vb = self.read_reg(fiber_id, b);
                self.write_reg(fiber_id, a, !vb);
            }
            
            Opcode::Shl => {
                let vb = self.read_reg(fiber_id, b);
                let vc = self.read_reg(fiber_id, c) as u32;
                self.write_reg(fiber_id, a, vb.wrapping_shl(vc));
            }
            
            Opcode::Shr => {
                let vb = self.read_reg(fiber_id, b) as i64;
                let vc = self.read_reg(fiber_id, c) as u32;
                self.write_reg(fiber_id, a, vb.wrapping_shr(vc) as u64);
            }
            
            Opcode::Ushr => {
                let vb = self.read_reg(fiber_id, b);
                let vc = self.read_reg(fiber_id, c) as u32;
                self.write_reg(fiber_id, a, vb.wrapping_shr(vc));
            }
            
            // ============ Logical ============
            Opcode::Not => {
                let vb = self.read_reg(fiber_id, b);
                self.write_reg(fiber_id, a, if vb == 0 { 1 } else { 0 });
            }
            
            // ============ Control flow ============
            Opcode::Jump => {
                let offset = instr.imm32();
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                let frame = fiber.frame_mut().unwrap();
                frame.pc = (frame.pc as i32 + offset - 1) as usize;
            }
            
            Opcode::JumpIf => {
                let cond = self.read_reg(fiber_id, a);
                if cond != 0 {
                    let offset = instr.imm32();
                    let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                    let frame = fiber.frame_mut().unwrap();
                    frame.pc = (frame.pc as i32 + offset - 1) as usize;
                }
            }
            
            Opcode::JumpIfNot => {
                let cond = self.read_reg(fiber_id, a);
                if cond == 0 {
                    let offset = instr.imm32();
                    let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                    let frame = fiber.frame_mut().unwrap();
                    frame.pc = (frame.pc as i32 + offset - 1) as usize;
                }
            }
            
            // ============ Function call ============
            Opcode::Call => {
                // a=func_id, b=arg_start, c=arg_count, flags=ret_count
                let module = self.module.as_ref().unwrap();
                let func = &module.functions[a as usize];
                let ret_count = flags;
                
                // Get return register (caller's perspective)
                let ret_reg = b; // Return values go to arg_start
                
                // Push new frame
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                fiber.push_frame(a as u32, b, func.local_slots as usize, ret_reg, ret_count);
            }
            
            Opcode::Return => {
                // a=ret_start, b=ret_count
                return self.do_return(fiber_id, a, b as usize);
            }
            
            // ============ Native call (zero-copy) ============
            Opcode::CallNative => {
                // a=native_id, b=arg_start, c=pair_count (each arg is type,value pair)
                // Args layout: [type0, val0, type1, val1, ...]
                let native_fn = match self.native_ptrs.get(a as usize) {
                    Some(Some(f)) => *f,
                    _ => {
                        let module = self.module.as_ref().unwrap();
                        let name = &module.natives[a as usize].name;
                        return VmResult::Panic(format!("native function not found: {}", name));
                    }
                };
                
                
                // Pause GC during native call
                self.gc.pause_gc();
                
                // Get fiber stack info for zero-copy register access
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                let bp = fiber.bp();
                let stack_ptr = fiber.stack.as_mut_ptr();
                let stack_len = fiber.stack.len();
                
                // Create a slice view of registers (safe: no reallocation during native call)
                // SAFETY: We ensure fiber.stack is not reallocated during native call,
                // and bp is stable within this call.
                let regs = unsafe {
                    std::slice::from_raw_parts_mut(stack_ptr.add(bp), stack_len - bp)
                };
                
                // Create zero-copy native context
                let mut ctx = NativeCtx::new(
                    &mut self.gc,
                    regs,
                    b as usize,     // arg_base
                    c as usize,     // arg_count
                    b as usize,     // ret_base (return values overwrite args)
                );
                
                // Call native function
                let result = native_fn(&mut ctx);
                
                // Resume GC
                self.gc.resume_gc();
                
                // Handle result
                match result {
                    NativeResult::Ok(_) => {}
                    NativeResult::Panic(msg) => return VmResult::Panic(msg),
                }
            }
            
            // ============ Object operations ============
            Opcode::Alloc => {
                // a=dest, b=type_id, c=extra_slots
                let type_id = b as TypeId;
                let size = if type_id == 0 {
                    // Anonymous struct - use extra_slots directly as size
                    c as usize
                } else {
                    let type_meta = self.types.get_unchecked(type_id);
                    type_meta.size_slots + c as usize
                };
                let obj = self.gc.alloc(type_id, size);
                self.write_reg(fiber_id, a, obj as u64);
            }
            
            Opcode::GetField => {
                // a=dest, b=obj, c=field_idx
                let obj = self.read_reg(fiber_id, b) as GcRef;
                let val = Gc::read_slot(obj, c as usize);
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::SetField => {
                // a=obj, b=field_idx, c=value
                let obj = self.read_reg(fiber_id, a) as GcRef;
                let val = self.read_reg(fiber_id, c);
                Gc::write_slot(obj, b as usize, val);
            }
            
            Opcode::GetFieldN => {
                // a=dest_start, b=obj, c=count, flags=field_idx
                let obj = self.read_reg(fiber_id, b) as GcRef;
                for i in 0..c {
                    let val = Gc::read_slot(obj, flags as usize + i as usize);
                    self.write_reg(fiber_id, a + i, val);
                }
            }
            
            Opcode::SetFieldN => {
                // a=obj, b=src_start, c=count, flags=field_idx
                let obj = self.read_reg(fiber_id, a) as GcRef;
                for i in 0..c {
                    let val = self.read_reg(fiber_id, b + i);
                    Gc::write_slot(obj, (flags + i as u8) as usize, val);
                }
            }
            
            Opcode::StructHash => {
                // a=dest, b=struct, c=field_count
                let obj = self.read_reg(fiber_id, b) as GcRef;
                let hash = objects::struct_hash(obj, c as usize);
                self.write_reg(fiber_id, a, hash);
            }
            
            // ============ Array ============
            Opcode::ArrayNew => {
                // a=dest, b=elem_type, c=len
                let elem_type = b as TypeId;
                let elem_size = self.types.get_unchecked(elem_type).size_slots;
                let len = c as usize;
                let arr = array::create(&mut self.gc, builtin::ARRAY, elem_type, elem_size, len);
                self.write_reg(fiber_id, a, arr as u64);
            }
            
            Opcode::ArrayGet => {
                // a=dest, b=array, c=index
                let arr = self.read_reg(fiber_id, b) as GcRef;
                let idx = self.read_reg(fiber_id, c) as usize;
                if idx >= array::len(arr) {
                    return VmResult::Panic(format!("array index out of bounds: {} >= {}", idx, array::len(arr)));
                }
                let val = array::get(arr, idx);
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::ArraySet => {
                // a=array, b=index, c=value
                let arr = self.read_reg(fiber_id, a) as GcRef;
                let idx = self.read_reg(fiber_id, b) as usize;
                let val = self.read_reg(fiber_id, c);
                if idx >= array::len(arr) {
                    return VmResult::Panic(format!("array index out of bounds: {} >= {}", idx, array::len(arr)));
                }
                array::set(arr, idx, val);
            }
            
            Opcode::ArrayLen => {
                // a=dest, b=array
                let arr = self.read_reg(fiber_id, b) as GcRef;
                self.write_reg(fiber_id, a, array::len(arr) as u64);
            }
            
            // ============ Slice ============
            Opcode::SliceNew => {
                // a=dest, b=array, c=start, flags has end encoded
                let arr = self.read_reg(fiber_id, b) as GcRef;
                let start = c as usize;
                let end = flags as usize;
                let cap = array::len(arr) - start;
                let sl = slice::create(&mut self.gc, builtin::SLICE, arr, start, end - start, cap);
                self.write_reg(fiber_id, a, sl as u64);
            }
            
            Opcode::SliceGet => {
                // a=dest, b=slice, c=index
                let sl = self.read_reg(fiber_id, b) as GcRef;
                let idx = self.read_reg(fiber_id, c) as usize;
                if idx >= slice::len(sl) {
                    return VmResult::Panic(format!("slice index out of bounds: {} >= {}", idx, slice::len(sl)));
                }
                let val = slice::get(sl, idx);
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::SliceSet => {
                // a=slice, b=index, c=value
                let sl = self.read_reg(fiber_id, a) as GcRef;
                let idx = self.read_reg(fiber_id, b) as usize;
                let val = self.read_reg(fiber_id, c);
                if idx >= slice::len(sl) {
                    return VmResult::Panic(format!("slice index out of bounds: {} >= {}", idx, slice::len(sl)));
                }
                slice::set(sl, idx, val);
            }
            
            Opcode::SliceLen => {
                // a=dest, b=slice
                let sl = self.read_reg(fiber_id, b) as GcRef;
                self.write_reg(fiber_id, a, slice::len(sl) as u64);
            }
            
            Opcode::SliceCap => {
                // a=dest, b=slice
                let sl = self.read_reg(fiber_id, b) as GcRef;
                self.write_reg(fiber_id, a, slice::cap(sl) as u64);
            }
            
            Opcode::SliceSlice => {
                // a=dest, b=slice, c=start_reg, flags=end_reg
                let sl = self.read_reg(fiber_id, b) as GcRef;
                let start = self.read_reg(fiber_id, c) as usize;
                let end = self.read_reg(fiber_id, flags as u16) as usize;
                let new_sl = slice::slice_of(&mut self.gc, builtin::SLICE, sl, start, end);
                self.write_reg(fiber_id, a, new_sl as u64);
            }
            
            Opcode::SliceAppend => {
                // a=dest, b=slice, c=value
                let sl = self.read_reg(fiber_id, b) as GcRef;
                let val = self.read_reg(fiber_id, c);
                let new_sl = slice::append(&mut self.gc, builtin::SLICE, builtin::ARRAY, sl, val);
                self.write_reg(fiber_id, a, new_sl as u64);
            }
            
            // ============ String ============
            Opcode::StrNew => {
                // a=dest, b=const_idx
                let str_ref = self.string_constants[b as usize];
                self.write_reg(fiber_id, a, str_ref as u64);
            }
            
            Opcode::StrConcat => {
                // a=dest, b=str1, c=str2
                let s1 = self.read_reg(fiber_id, b) as GcRef;
                let s2 = self.read_reg(fiber_id, c) as GcRef;
                let result = string::concat(&mut self.gc, builtin::STRING, s1, s2);
                self.write_reg(fiber_id, a, result as u64);
            }
            
            Opcode::StrLen => {
                // a=dest, b=str
                let s = self.read_reg(fiber_id, b) as GcRef;
                self.write_reg(fiber_id, a, string::len(s) as u64);
            }
            
            Opcode::StrIndex => {
                // a=dest, b=str, c=index
                let s = self.read_reg(fiber_id, b) as GcRef;
                let idx = self.read_reg(fiber_id, c) as usize;
                if idx >= string::len(s) {
                    return VmResult::Panic(format!("string index out of bounds: {} >= {}", idx, string::len(s)));
                }
                self.write_reg(fiber_id, a, string::index(s, idx) as u64);
            }
            
            Opcode::StrEq => {
                // a=dest, b=str1, c=str2 (content comparison)
                let s1 = self.read_reg(fiber_id, b) as GcRef;
                let s2 = self.read_reg(fiber_id, c) as GcRef;
                self.write_reg(fiber_id, a, if string::eq(s1, s2) { 1 } else { 0 });
            }
            
            Opcode::StrNe => {
                // a=dest, b=str1, c=str2 (content comparison)
                let s1 = self.read_reg(fiber_id, b) as GcRef;
                let s2 = self.read_reg(fiber_id, c) as GcRef;
                self.write_reg(fiber_id, a, if string::ne(s1, s2) { 1 } else { 0 });
            }
            
            // ============ Map ============
            Opcode::MapNew => {
                // a=dest, b=key_type, c=val_type
                let m = map::create(&mut self.gc, builtin::MAP, b as TypeId, c as TypeId);
                self.write_reg(fiber_id, a, m as u64);
            }
            
            Opcode::MapGet => {
                // a=dest, b=map, c=key; ok stored at a+1
                let m = self.read_reg(fiber_id, b) as GcRef;
                let key = self.read_reg(fiber_id, c);
                match map::get(m, key) {
                    Some(val) => {
                        self.write_reg(fiber_id, a, val);
                        self.write_reg(fiber_id, a + 1, 1); // ok = true
                    }
                    None => {
                        self.write_reg(fiber_id, a, 0);
                        self.write_reg(fiber_id, a + 1, 0); // ok = false
                    }
                }
            }
            
            Opcode::MapSet => {
                // a=map, b=key, c=value
                let m = self.read_reg(fiber_id, a) as GcRef;
                let key = self.read_reg(fiber_id, b);
                let val = self.read_reg(fiber_id, c);
                map::set(m, key, val);
            }
            
            Opcode::MapDelete => {
                // a=map, b=key
                let m = self.read_reg(fiber_id, a) as GcRef;
                let key = self.read_reg(fiber_id, b);
                map::delete(m, key);
            }
            
            Opcode::MapLen => {
                // a=dest, b=map
                let m = self.read_reg(fiber_id, b) as GcRef;
                self.write_reg(fiber_id, a, map::len(m) as u64);
            }
            
            // ============ Channel ============
            Opcode::ChanNew => {
                // a=dest, b=elem_type, c=capacity
                let ch = channel::create(&mut self.gc, builtin::CHANNEL, b as TypeId, c as usize);
                self.write_reg(fiber_id, a, ch as u64);
            }
            
            Opcode::ChanSend => {
                // a=chan, b=value
                let ch = self.read_reg(fiber_id, a) as GcRef;
                if ch.is_null() {
                    // Send on nil channel blocks forever
                    self.scheduler.block_current(BlockReason::ChanSend(ch));
                    return VmResult::Yield;
                }
                let val = self.read_reg(fiber_id, b);
                
                match channel::try_send(ch, val) {
                    Ok(Some(receiver_id)) => {
                        // Directly sent to a waiting receiver
                        // Store value in receiver's register (they're blocked on ChanRecv)
                        // and unblock them
                        if let Some(recv_fiber) = self.scheduler.get_mut(receiver_id) {
                            // The receiver is waiting with their dest register info
                            // For simplicity, we'll use a different approach
                        }
                        self.scheduler.unblock(receiver_id);
                    }
                    Ok(None) => {
                        // Buffered successfully
                    }
                    Err(_) => {
                        // Would block - add to waiting senders
                        let state = channel::get_state(ch);
                        state.waiting_senders.push_back((fiber_id, val));
                        self.scheduler.block_current(BlockReason::ChanSend(ch));
                        return VmResult::Yield;
                    }
                }
            }
            
            Opcode::ChanRecv => {
                // a=dest, b=chan, c=ok_dest
                let ch = self.read_reg(fiber_id, b) as GcRef;
                if ch.is_null() {
                    // Receive on nil channel blocks forever
                    self.scheduler.block_current(BlockReason::ChanRecv(ch));
                    return VmResult::Yield;
                }
                
                match channel::try_recv(ch) {
                    Ok(Some(val)) => {
                        self.write_reg(fiber_id, a, val);
                        self.write_reg(fiber_id, c, 1); // ok = true
                        
                        // Unblock a waiting sender if any
                        let state = channel::get_state(ch);
                        if let Some((sender_id, _)) = state.waiting_senders.pop_front() {
                            self.scheduler.unblock(sender_id);
                        }
                    }
                    Ok(None) => {
                        // Channel closed
                        self.write_reg(fiber_id, a, 0);
                        self.write_reg(fiber_id, c, 0); // ok = false
                    }
                    Err(()) => {
                        // Would block - decrement PC so we retry this instruction when unblocked
                        if let Some(fiber) = self.scheduler.get_mut(fiber_id) {
                            if let Some(frame) = fiber.frame_mut() {
                                frame.pc -= 1;
                            }
                        }
                        let state = channel::get_state(ch);
                        state.waiting_receivers.push_back(fiber_id);
                        self.scheduler.block_current(BlockReason::ChanRecv(ch));
                        return VmResult::Yield;
                    }
                }
            }
            
            Opcode::ChanClose => {
                // a=chan
                let ch = self.read_reg(fiber_id, a) as GcRef;
                channel::close(ch);
                
                // Unblock all waiting receivers
                let state = channel::get_state(ch);
                let receivers: Vec<_> = state.waiting_receivers.drain(..).collect();
                for recv_id in receivers {
                    self.scheduler.unblock(recv_id);
                }
            }
            
            // ============ Iterator ============
            Opcode::IterBegin => {
                // a=container, b=iter_type (0=slice, 1=map, 2=string, 3=int_range)
                let container = self.read_reg(fiber_id, a) as GcRef;
                let iter = match b {
                    0 => { // slice
                        // Handle nil slice
                        if container.is_null() {
                            IterState::Slice {
                                slice_ref: container,
                                elem_size: 1,
                                index: 0,
                                len: 0,
                            }
                        } else {
                            let len = slice::len(container);
                            let elem_size = slice::elem_size(container);
                            IterState::Slice {
                                slice_ref: container,
                                elem_size,
                                index: 0,
                                len,
                            }
                        }
                    }
                    1 => { // map
                        IterState::Map {
                            map_ref: container,
                            index: 0,
                        }
                    }
                    2 => { // string
                        IterState::String {
                            str_ref: container,
                            byte_pos: 0,
                        }
                    }
                    _ => {
                        return VmResult::Panic(format!("unknown iterator type: {}", b));
                    }
                };
                
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                fiber.iter_stack.push(iter);
            }
            
            Opcode::IterNext => {
                // a=index_dest, b=value_dest, c=done_offset
                // First, get iteration result without holding mutable borrow
                let (done, key_val, elem_val) = {
                    let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                    let iter = fiber.iter_stack.last_mut();
                    
                    let iter = iter.unwrap();
                    
                    match iter {
                        IterState::Slice { slice_ref, index, len, .. } => {
                            if *index >= *len {
                                (true, 0, 0)
                            } else {
                                let idx = *index;
                                let val = slice::get(*slice_ref, idx);
                                *index += 1;
                                (false, idx as u64, val)
                            }
                        }
                        IterState::Map { map_ref, index } => {
                            match map::iter_at(*map_ref, *index) {
                                Some((k, v)) => {
                                    *index += 1;
                                    (false, k, v)
                                }
                                None => (true, 0, 0),
                            }
                        }
                        IterState::String { str_ref, byte_pos } => {
                            let bytes = string::as_bytes(*str_ref);
                            if *byte_pos >= bytes.len() {
                                (true, 0, 0)
                            } else {
                                let pos = *byte_pos;
                                let byte = bytes[pos];
                                *byte_pos += 1;
                                (false, pos as u64, byte as u64)
                            }
                        }
                        IterState::IntRange { current, end, step } => {
                            let is_done = if *step > 0 { *current >= *end } else { *current <= *end };
                            if is_done {
                                (true, 0, 0)
                            } else {
                                let val = *current;
                                *current += *step;
                                (false, val as u64, val as u64)
                            }
                        }
                    }
                };
                
                if done {
                    let offset = c as i32;
                    let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                    let frame = fiber.frame_mut().unwrap();
                    frame.pc = (frame.pc as i32 + offset - 1) as usize;
                } else {
                    self.write_reg(fiber_id, a, key_val);
                    self.write_reg(fiber_id, b, elem_val);
                }
            }
            
            Opcode::IterEnd => {
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                fiber.iter_stack.pop();
            }
            
            // ============ Closure ============
            Opcode::ClosureNew => {
                // a=dest, b=func_id, c=upvalue_count
                let cl = closure::create(&mut self.gc, builtin::CLOSURE, b as u32, c as usize);
                self.write_reg(fiber_id, a, cl as u64);
            }
            
            Opcode::ClosureGet => {
                // a=dest, b=closure, c=upval_idx
                let cl = self.read_reg(fiber_id, b) as GcRef;
                let val = closure::get_upvalue(cl, c as usize);
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::ClosureSet => {
                // a=closure, b=upval_idx, c=value
                let cl = self.read_reg(fiber_id, a) as GcRef;
                let val = self.read_reg(fiber_id, c);
                closure::set_upvalue(cl, b as usize, val);
            }
            
            Opcode::ClosureCall => {
                // a=closure_reg, b=arg_start, c=arg_count, flags=ret_count
                let cl = self.read_reg(fiber_id, a) as GcRef;
                let func_id = closure::func_id(cl);
                let module = self.module.as_ref().unwrap();
                let func = &module.functions[func_id as usize];
                
                // Copy arguments before pushing frame
                let args: Vec<u64> = (0..c).map(|i| self.read_reg(fiber_id, b + i)).collect();
                
                // Push frame: ret_reg = b (return to arg_start), ret_count = flags
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                fiber.push_frame(func_id, b, func.local_slots as usize, b, flags);
                
                // Write closure reference to register 0 for upvalue access
                fiber.write_reg(0, cl as u64);
                
                // Copy arguments to new frame (starting after closure ref at reg 1)
                for (i, arg) in args.into_iter().enumerate() {
                    fiber.write_reg((i + 1) as u16, arg);
                }
            }
            
            Opcode::UpvalNew => {
                // a=dest: create new upval_box for reference capture
                let uv = closure::create_upval_box(&mut self.gc, builtin::CLOSURE);
                self.write_reg(fiber_id, a, uv as u64);
            }
            
            Opcode::UpvalGet => {
                // a=dest, b=upval_box: read value from upval_box
                let uv = self.read_reg(fiber_id, b) as GcRef;
                let val = closure::get_upval_box(uv);
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::UpvalSet => {
                // a=upval_box, b=value: write value to upval_box
                let uv = self.read_reg(fiber_id, a) as GcRef;
                let val = self.read_reg(fiber_id, b);
                closure::set_upval_box(uv, val);
            }
            
            // ============ Goroutine ============
            Opcode::Go => {
                // a=func_id, b=arg_start, c=arg_count
                let module = self.module.as_ref().unwrap();
                let func = &module.functions[a as usize];
                
                // Create new fiber
                let new_fiber_id = self.scheduler.spawn(1024);
                
                // Copy arguments
                let args: Vec<u64> = (0..c).map(|i| self.read_reg(fiber_id, b + i)).collect();
                
                // Setup new fiber's frame
                let new_fiber = self.scheduler.get_mut(new_fiber_id).unwrap();
                new_fiber.push_frame(a as u32, 0, func.local_slots as usize, 0, 0);
                for (i, arg) in args.into_iter().enumerate() {
                    new_fiber.write_reg(i as u16, arg);
                }
            }
            
            Opcode::Yield => {
                self.scheduler.yield_current();
                return VmResult::Yield;
            }
            
            // ============ Defer/Panic/Recover ============
            Opcode::DeferPush => {
                // a=func_id, b=arg_start, c=arg_count
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                let frame_depth = fiber.frames.len();
                let mut entry = DeferEntry::new(frame_depth, a as u32);
                entry.arg_count = c as u8;
                for i in 0..c.min(8) {
                    entry.args[i as usize] = fiber.read_reg(b + i);
                }
                fiber.defer_stack.push(entry);
            }
            
            Opcode::DeferPop => {
                // Execute defers for current frame
                let defers = {
                    let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                    fiber.pop_frame_defers()
                };
                
                for defer in defers {
                    // Execute defer by calling the function
                    // For simplicity, we just call it inline
                    // In a full implementation, this would push a frame
                    let _func_id = defer.func_id;
                    // TODO: Execute defer function
                }
            }
            
            Opcode::Panic => {
                // a=value
                let val = self.read_reg(fiber_id, a);
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                fiber.panic_value = Some(val);
                
                // Unwind and execute defers
                while !fiber.frames.is_empty() {
                    let defers = fiber.pop_frame_defers();
                    for defer in defers {
                        if fiber.recovering {
                            // recover() was called
                            fiber.recovering = false;
                            fiber.panic_value = None;
                            // Continue execution
                            return VmResult::Ok;
                        }
                        // Execute defer
                        let _func_id = defer.func_id;
                        // TODO: Execute defer function
                    }
                    fiber.pop_frame();
                }
                
                // Panic not recovered
                let msg = format!("panic: {}", val);
                self.scheduler.kill(fiber_id);
                return VmResult::Panic(msg);
            }
            
            Opcode::Recover => {
                // a=dest
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                if let Some(val) = fiber.panic_value.take() {
                    fiber.recovering = true;
                    self.write_reg(fiber_id, a, val);
                } else {
                    self.write_reg(fiber_id, a, 0);
                }
            }
            
            // ============ Interface ============
            Opcode::BoxInterface => {
                // a=dest (2 slots), b=type_id, c=value
                let type_id = b as TypeId;
                let val = self.read_reg(fiber_id, c);
                let (slot0, slot1) = interface::box_value(type_id, val);
                self.write_reg(fiber_id, a, slot0);
                self.write_reg(fiber_id, a + 1, slot1);
            }
            
            Opcode::UnboxInterface => {
                // a=dest, b=interface (2 slots), c=type_reg
                let slot0 = self.read_reg(fiber_id, b);
                let slot1 = self.read_reg(fiber_id, b + 1);
                let type_id = interface::unbox_type(slot0);
                let data = interface::unbox_data(slot1);
                self.write_reg(fiber_id, a, data);
                self.write_reg(fiber_id, c, type_id as u64);
            }
            
            Opcode::TypeAssert => {
                // a=dest, b=interface (2 slots), c=expected_type, flags=ok_reg
                let slot0 = self.read_reg(fiber_id, b);
                let slot1 = self.read_reg(fiber_id, b + 1);
                let actual_type = interface::unbox_type(slot0);
                let expected_type = c as TypeId;
                
                if actual_type == expected_type {
                    self.write_reg(fiber_id, a, interface::unbox_data(slot1));
                    self.write_reg(fiber_id, flags as u16, 1);
                } else {
                    self.write_reg(fiber_id, a, 0);
                    self.write_reg(fiber_id, flags as u16, 0);
                }
            }
            
            // ============ Type conversion ============
            Opcode::I64ToF64 => {
                let v = self.read_reg(fiber_id, b) as i64;
                self.write_reg(fiber_id, a, (v as f64).to_bits());
            }
            
            Opcode::F64ToI64 => {
                let v = f64::from_bits(self.read_reg(fiber_id, b));
                self.write_reg(fiber_id, a, v as i64 as u64);
            }
            
            Opcode::I32ToI64 => {
                let v = self.read_reg(fiber_id, b) as i32;
                self.write_reg(fiber_id, a, v as i64 as u64);
            }
            
            Opcode::I64ToI32 => {
                let v = self.read_reg(fiber_id, b) as i64;
                self.write_reg(fiber_id, a, v as i32 as u64);
            }
            
            // ============ Debug ============
            Opcode::DebugPrint => {
                let type_tag = crate::ffi::TypeTag::from_u8(b as u8);
                
                let (val, inner_tag, is_iface) = if type_tag == crate::ffi::TypeTag::Interface {
                    let type_id = self.read_reg(fiber_id, a) as u32;
                    let data = self.read_reg(fiber_id, a + 1);
                    (data, type_id_to_tag(type_id), true)
                } else {
                    (self.read_reg(fiber_id, a), type_tag, false)
                };
                
                let s = format_value(val, inner_tag);
                if is_iface {
                    println!("(interface){}", s);
                } else {
                    println!("{}", s);
                }
            }
            
            Opcode::AssertBegin => {
                let cond = self.read_reg(fiber_id, a);
                let arg_count = b;
                let line = c;
                
                if cond == 0 {
                    // Assertion failed - set flag and print header
                    let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                    fiber.assert_failed = true;
                    fiber.assert_line = line;
                    if arg_count > 0 {
                        eprint!("assertion failed: ");
                    } else {
                        eprintln!("assertion failed");
                    }
                } else {
                    // Assertion passed - skip arg_count AssertArg instructions + AssertEnd
                    let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                    fiber.assert_failed = false;
                    if let Some(frame) = fiber.frame_mut() {
                        frame.pc += arg_count as usize + 1; // skip instructions (pc increments by 1 per instruction)
                    }
                }
            }
            
            Opcode::AssertArg => {
                let fiber = self.scheduler.get(fiber_id).unwrap();
                if fiber.assert_failed {
                    let type_tag = crate::ffi::TypeTag::from_u8(b as u8);
                    
                    let (val, inner_tag, is_iface) = if type_tag == crate::ffi::TypeTag::Interface {
                        let type_id = self.read_reg(fiber_id, a) as u32;
                        let data = self.read_reg(fiber_id, a + 1);
                        (data, type_id_to_tag(type_id), true)
                    } else {
                        (self.read_reg(fiber_id, a), type_tag, false)
                    };
                    
                    let s = format_value(val, inner_tag);
                    if is_iface {
                        eprint!("(interface){}", s);
                    } else {
                        eprint!("{}", s);
                    }
                }
            }
            
            Opcode::AssertEnd => {
                let fiber = self.scheduler.get(fiber_id).unwrap();
                if fiber.assert_failed {
                    let line = fiber.assert_line;
                    eprintln!();
                    eprintln!("  at line {}", line);
                    return VmResult::Panic("assertion failed".into());
                }
            }
            
            Opcode::Invalid => {
                return VmResult::Panic(format!("invalid opcode: {}", instr.op));
            }
        }
        
        VmResult::Ok
    }
    
    fn do_return(&mut self, fiber_id: FiberId, ret_start: u16, ret_count: usize) -> VmResult {
        // Execute defers first
        {
            let fiber = self.scheduler.get_mut(fiber_id).unwrap();
            let defers = fiber.pop_frame_defers();
            for _defer in defers {
                // TODO: Execute defer function
            }
        }
        
        // Get return info from current frame
        let (caller_ret_reg, caller_ret_count) = {
            let fiber = self.scheduler.get(fiber_id).unwrap();
            let frame = fiber.frame().unwrap();
            (frame.ret_reg, frame.ret_count as usize)
        };
        
        // Copy return values
        let ret_vals: Vec<u64> = (0..ret_count).map(|i| self.read_reg(fiber_id, ret_start + i as u16)).collect();
        
        // Pop frame
        {
            let fiber = self.scheduler.get_mut(fiber_id).unwrap();
            fiber.pop_frame();
        }
        
        // If no more frames, fiber is done
        let fiber = self.scheduler.get(fiber_id).unwrap();
        if fiber.frames.is_empty() {
            self.scheduler.kill(fiber_id);
            return VmResult::Yield;
        }
        
        // Store return values in caller's registers
        let actual_ret = ret_count.min(caller_ret_count);
        for i in 0..actual_ret {
            self.write_reg(fiber_id, caller_ret_reg + i as u16, ret_vals[i]);
        }
        
        VmResult::Ok
    }
    
    #[inline]
    fn read_reg(&self, fiber_id: FiberId, reg: u16) -> u64 {
        self.scheduler.get(fiber_id).unwrap().read_reg(reg)
    }
    
    #[inline]
    fn write_reg(&mut self, fiber_id: FiberId, reg: u16, val: u64) {
        self.scheduler.get_mut(fiber_id).unwrap().write_reg(reg, val);
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

/// Format a value for printing based on its type tag.
fn format_value(val: u64, type_tag: crate::ffi::TypeTag) -> String {
    match type_tag {
        crate::ffi::TypeTag::Float32 => {
            let f = f32::from_bits(val as u32);
            format!("{}", f)
        }
        crate::ffi::TypeTag::Float64 => {
            let f = f64::from_bits(val);
            if f.abs() >= 1e10 || (f != 0.0 && f.abs() < 1e-4) {
                format!("{:e}", f)
            } else {
                format!("{}", f)
            }
        }
        crate::ffi::TypeTag::Bool => {
            if val != 0 { "true".to_string() } else { "false".to_string() }
        }
        crate::ffi::TypeTag::String => {
            let ptr = val as crate::gc::GcRef;
            if ptr.is_null() {
                String::new()
            } else {
                crate::objects::string::as_str(ptr).to_string()
            }
        }
        _ => {
            format!("{}", val as i64)
        }
    }
}

/// Convert boxed type_id to TypeTag for formatting.
fn type_id_to_tag(type_id: u32) -> crate::ffi::TypeTag {
    use crate::types::builtin;
    match type_id {
        0 => crate::ffi::TypeTag::Nil,
        t if t == builtin::INT64 => crate::ffi::TypeTag::Int64,
        t if t == builtin::INT32 => crate::ffi::TypeTag::Int32,
        t if t == builtin::FLOAT64 => crate::ffi::TypeTag::Float64,
        t if t == builtin::FLOAT32 => crate::ffi::TypeTag::Float32,
        t if t == builtin::STRING => crate::ffi::TypeTag::String,
        t if t == builtin::BOOL => crate::ffi::TypeTag::Bool,
        _ => crate::ffi::TypeTag::Int64, // default
    }
}
