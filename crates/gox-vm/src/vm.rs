//! Virtual machine implementation.

use crate::bytecode::{Constant, Module};
use crate::fiber::{BlockReason, DeferEntry, FiberId, FiberStatus, IterState, Scheduler, SelectCase, SelectState};
use crate::gc::{Gc, GcRef, NULL_REF};
use crate::instruction::{Instruction, Opcode};
use crate::extern_fn::{ExternCtx, ExternFn, ExternRegistry, ExternResult};
use crate::objects::{self, array, channel, closure, interface, map, slice, string};
use crate::types::{TypeId, TypeTable};
use gox_common_core::{RuntimeTypeId, ValueKind};

use alloc::{format, string::{String, ToString}, vec::Vec};

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
    pub externs: ExternRegistry,
    
    // Current module
    module: Option<Module>,
    
    // Preloaded string constants (GcRefs)
    string_constants: Vec<GcRef>,
    
    // Resolved extern function pointers
    extern_ptrs: Vec<Option<ExternFn>>,
    
    // Global variables storage
    globals: Vec<u64>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            types: TypeTable::new(),
            scheduler: Scheduler::new(),
            externs: ExternRegistry::new(),
            module: None,
            string_constants: Vec::new(),
            extern_ptrs: Vec::new(),
            globals: Vec::new(),
        }
    }
    
    pub fn with_externs(externs: ExternRegistry) -> Self {
        Self {
            gc: Gc::new(),
            types: TypeTable::new(),
            scheduler: Scheduler::new(),
            externs,
            module: None,
            string_constants: Vec::new(),
            extern_ptrs: Vec::new(),
            globals: Vec::new(),
        }
    }
    
    /// Load a module.
    pub fn load_module(&mut self, module: Module) {
        // Load type metadata
        self.types.load_struct_types(&module.struct_types);
        self.types.load_interface_types(&module.interface_types);
        
        // Preload string constants
        self.string_constants.clear();
        for c in &module.constants {
            if let Constant::String(s) = c {
                let str_ref = string::from_rust_str(&mut self.gc, s);
                self.string_constants.push(str_ref);
            } else {
                self.string_constants.push(NULL_REF);
            }
        }
        
        // Resolve extern functions
        self.extern_ptrs.clear();
        for ext in &module.externs {
            let ptr = self.externs.get(&ext.name);
            self.extern_ptrs.push(ptr);
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
            
            Opcode::CallInterface => {
                // a=iface_reg, b=method_idx, c=args_start, flags=(ret_count<<4)|arg_count
                let arg_count = (flags & 0x0F) as u16;
                let ret_count = flags >> 4;
                
                // Read interface value (2 slots)
                let iface_slot0 = self.read_reg(fiber_id, a);
                let iface_slot1 = self.read_reg(fiber_id, a + 1);
                
                // Extract type info from slot0
                let iface_type_id = interface::unpack_iface_type_id(iface_slot0);
                let concrete_type_id = interface::unpack_value_type_id(iface_slot0);
                
                // Lookup method in dispatch table
                let module = self.module.as_ref().unwrap();
                let func_id = match module.lookup_iface_method(concrete_type_id, iface_type_id, b as usize) {
                    Some(id) => id,
                    None => {
                        return VmResult::Panic(format!(
                            "interface method not found: concrete_type={}, iface_type={}, method_idx={}",
                            concrete_type_id, iface_type_id, b
                        ));
                    }
                };
                
                let func = &module.functions[func_id as usize];
                
                // Copy receiver (from iface_slot1) + args before pushing frame
                let mut args_with_recv: Vec<u64> = Vec::with_capacity(1 + arg_count as usize);
                args_with_recv.push(iface_slot1); // receiver is first arg
                for i in 0..arg_count {
                    args_with_recv.push(self.read_reg(fiber_id, c + i));
                }
                
                // Push frame: return to c (args_start)
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                fiber.push_frame(func_id, c, func.local_slots as usize, c, ret_count);
                
                // Copy args into new frame
                for (i, &val) in args_with_recv.iter().enumerate() {
                    self.write_reg(fiber_id, i as u16, val);
                }
            }
            
            // ============ Extern call (zero-copy) ============
            Opcode::CallExtern => {
                // a=extern_id, b=arg_start, c=pair_count (each arg is type,value pair)
                // Args layout: [type0, val0, type1, val1, ...]
                let extern_fn = match self.extern_ptrs.get(a as usize) {
                    Some(Some(f)) => *f,
                    _ => {
                        let module = self.module.as_ref().unwrap();
                        let name = &module.externs[a as usize].name;
                        return VmResult::Panic(format!("extern function not found: {}", name));
                    }
                };
                
                
                // Pause GC during extern call
                self.gc.pause_gc();
                
                // Get fiber stack info for zero-copy register access
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                let bp = fiber.bp();
                let stack_ptr = fiber.stack.as_mut_ptr();
                let stack_len = fiber.stack.len();
                
                // Create a slice view of registers (safe: no reallocation during extern call)
                // SAFETY: We ensure fiber.stack is not reallocated during extern call,
                // and bp is stable within this call.
                let regs = unsafe {
                    core::slice::from_raw_parts_mut(stack_ptr.add(bp), stack_len - bp)
                };
                
                // Create zero-copy extern context
                let mut ctx = ExternCtx::new(
                    &mut self.gc,
                    regs,
                    b as usize,     // arg_base
                    c as usize,     // arg_count
                    b as usize,     // ret_base (return values overwrite args)
                );
                
                // Call extern function
                let result = extern_fn(&mut ctx);
                
                // Resume GC
                self.gc.resume_gc();
                
                // Handle result
                match result {
                    ExternResult::Ok(_) => {}
                    ExternResult::Panic(msg) => return VmResult::Panic(msg),
                }
            }
            
            // ============ Object operations ============
            Opcode::Alloc => {
                // a=dest, b=type_id_lo, c=type_id_hi, flags=field_count
                let type_id = (b as u32) | ((c as u32) << 16);
                let type_meta = self.types.get_struct_unchecked(type_id);
                let obj = self.gc.alloc(ValueKind::Struct as u8, type_id as u16, type_meta.size_slots);
                self.write_reg(fiber_id, a, obj as u64);
            }
            
            Opcode::GetField => {
                // a=dest, b=obj, c=byte_offset
                // flags[1:0]=size_code (0=1,1=2,2=4,3=8), flags[2]=signed
                // Branchless: read u64, mask and sign-extend
                let obj = self.read_reg(fiber_id, b) as GcRef;
                let byte_offset = c as usize;
                let size_code = flags & 0b11;
                let signed = (flags >> 2) & 1 != 0;
                
                let val = unsafe {
                    let data_ptr = Gc::get_data_ptr(obj) as *const u8;
                    let ptr = data_ptr.add(byte_offset);
                    // Read unaligned u64, then mask to size
                    let raw = core::ptr::read_unaligned(ptr as *const u64);
                    // Mask: (1 << (8 << size_code)) - 1, but 64-bit needs special handling
                    let bits = 8u32 << size_code; // 8, 16, 32, 64
                    let mask = if bits == 64 { u64::MAX } else { (1u64 << bits) - 1 };
                    let masked = raw & mask;
                    // Sign extension: shift left then arithmetic shift right
                    if signed && bits < 64 {
                        let shift = 64 - bits;
                        ((masked << shift) as i64 >> shift) as u64
                    } else {
                        masked
                    }
                };
                self.write_reg(fiber_id, a, val);
            }
            
            Opcode::SetField => {
                // a=obj, b=byte_offset, c=value
                // flags[1:0]=size_code (0=1,1=2,2=4,3=8)
                // Branchless: use copy_nonoverlapping with computed size
                let obj = self.read_reg(fiber_id, a) as GcRef;
                let byte_offset = b as usize;
                let val = self.read_reg(fiber_id, c);
                let size_code = flags & 0b11;
                
                unsafe {
                    let data_ptr = Gc::get_data_ptr(obj) as *mut u8;
                    let ptr = data_ptr.add(byte_offset);
                    // Size in bytes: 1 << size_code
                    let size = 1usize << size_code;
                    core::ptr::copy_nonoverlapping(&val as *const u64 as *const u8, ptr, size);
                }
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
            
            Opcode::StructClone => {
                // a=dest, b=src_struct, c=size_slots
                // Deep copy: allocate new struct with same type and copy all slots
                let src = self.read_reg(fiber_id, b) as GcRef;
                let size_slots = c as usize;
                let cloned = objects::struct_clone(&mut self.gc, src, size_slots);
                self.write_reg(fiber_id, a, cloned as u64);
            }
            
            // ============ Array ============
            Opcode::ArrayNew => {
                // a=dest, b=elem_type (ValueKind), c=len
                let elem_kind = b as u8;
                // Use elem_bytes for compact storage (1/2/4/8 bytes per element)
                let elem_bytes = elem_bytes_from_value_kind(elem_kind);
                let len = c as usize;
                let arr = array::create(&mut self.gc, elem_kind, 0, elem_bytes, len);
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
                let sl = slice::create(&mut self.gc, arr, start, end - start, cap);
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
                let len = if sl.is_null() { 0 } else { slice::len(sl) };
                self.write_reg(fiber_id, a, len as u64);
            }
            
            Opcode::SliceCap => {
                // a=dest, b=slice
                let sl = self.read_reg(fiber_id, b) as GcRef;
                let cap = if sl.is_null() { 0 } else { slice::cap(sl) };
                self.write_reg(fiber_id, a, cap as u64);
            }
            
            Opcode::SliceSlice => {
                // a=dest, b=slice, c=start_reg, flags=end_reg
                let sl = self.read_reg(fiber_id, b) as GcRef;
                let start = self.read_reg(fiber_id, c) as usize;
                let end = self.read_reg(fiber_id, flags as u16) as usize;
                let new_sl = slice::slice_of(&mut self.gc, sl, start, end);
                self.write_reg(fiber_id, a, new_sl as u64);
            }
            
            Opcode::SliceAppend => {
                // a=dest, b=slice, c=value
                let sl = self.read_reg(fiber_id, b) as GcRef;
                let val = self.read_reg(fiber_id, c);
                // For append, we need elem info from the slice (or use defaults for nil slice)
                let (elem_kind, elem_type_id, elem_bytes) = if sl.is_null() {
                    (ValueKind::Int as u8, 0u16, 8usize) // default: int elements
                } else {
                    (slice::elem_kind(sl) as u8, slice::elem_type_id(sl), array::elem_bytes(slice::array_ref(sl)))
                };
                let new_sl = slice::append(&mut self.gc, elem_kind, elem_type_id, elem_bytes, sl, val);
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
                let result = string::concat(&mut self.gc, s1, s2);
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
            
            Opcode::StrLt => {
                let s1 = self.read_reg(fiber_id, b) as GcRef;
                let s2 = self.read_reg(fiber_id, c) as GcRef;
                self.write_reg(fiber_id, a, if string::lt(s1, s2) { 1 } else { 0 });
            }
            
            Opcode::StrLe => {
                let s1 = self.read_reg(fiber_id, b) as GcRef;
                let s2 = self.read_reg(fiber_id, c) as GcRef;
                self.write_reg(fiber_id, a, if string::le(s1, s2) { 1 } else { 0 });
            }
            
            Opcode::StrGt => {
                let s1 = self.read_reg(fiber_id, b) as GcRef;
                let s2 = self.read_reg(fiber_id, c) as GcRef;
                self.write_reg(fiber_id, a, if string::gt(s1, s2) { 1 } else { 0 });
            }
            
            Opcode::StrGe => {
                let s1 = self.read_reg(fiber_id, b) as GcRef;
                let s2 = self.read_reg(fiber_id, c) as GcRef;
                self.write_reg(fiber_id, a, if string::ge(s1, s2) { 1 } else { 0 });
            }
            
            Opcode::StrSlice => {
                // a=dest, b=string, c=start_reg, flags=end_reg
                let str_ref = self.read_reg(fiber_id, b) as GcRef;
                let start = self.read_reg(fiber_id, c) as usize;
                let end = self.read_reg(fiber_id, flags as u16) as usize;
                let new_str = string::slice_of(&mut self.gc, str_ref, start, end);
                self.write_reg(fiber_id, a, new_str as u64);
            }
            
            // ============ Map ============
            Opcode::MapNew => {
                // a=dest, b=key_type, c=val_type
                let m = map::create(&mut self.gc, b as u8, c as u8);
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
                let ch = channel::create(&mut self.gc, b as u8, c as usize);
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
                let cap = channel::capacity(ch);
                let state = channel::get_state(ch);
                
                match state.try_send(val, cap) {
                    channel::SendResult::DirectSend(receiver_id) => {
                        // Directly sent to a waiting receiver
                        self.scheduler.unblock(receiver_id as FiberId);
                    }
                    channel::SendResult::Buffered => {
                        // Buffered successfully
                    }
                    channel::SendResult::WouldBlock => {
                        // Would block - add to waiting senders
                        state.register_sender(fiber_id as u64, val);
                        self.scheduler.block_current(BlockReason::ChanSend(ch));
                        return VmResult::Yield;
                    }
                    channel::SendResult::Closed => {
                        return VmResult::Panic("send on closed channel".to_string());
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
                
                let state = channel::get_state(ch);
                match state.try_recv() {
                    channel::RecvResult::Success(val, woke_sender) => {
                        self.write_reg(fiber_id, a, val);
                        self.write_reg(fiber_id, c, 1); // ok = true
                        
                        // Unblock the sender if any
                        if let Some(sender_id) = woke_sender {
                            self.scheduler.unblock(sender_id as FiberId);
                        }
                    }
                    channel::RecvResult::Closed => {
                        // Channel closed
                        self.write_reg(fiber_id, a, 0);
                        self.write_reg(fiber_id, c, 0); // ok = false
                    }
                    channel::RecvResult::WouldBlock => {
                        // Would block - decrement PC so we retry this instruction when unblocked
                        if let Some(fiber) = self.scheduler.get_mut(fiber_id) {
                            if let Some(frame) = fiber.frame_mut() {
                                frame.pc -= 1;
                            }
                        }
                        state.register_receiver(fiber_id as u64);
                        self.scheduler.block_current(BlockReason::ChanRecv(ch));
                        return VmResult::Yield;
                    }
                }
            }
            
            Opcode::ChanClose => {
                // a=chan
                let ch = self.read_reg(fiber_id, a) as GcRef;
                let state = channel::get_state(ch);
                state.close();
                
                // Unblock all waiting receivers
                let receivers = state.take_waiting_receivers();
                for recv_id in receivers {
                    self.scheduler.unblock(recv_id as FiberId);
                }
            }
            
            // ============ Select ============
            Opcode::SelectStart => {
                // a=case_count, b=has_default
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                fiber.select_state = Some(SelectState {
                    cases: Vec::with_capacity(a as usize),
                    has_default: b != 0,
                });
            }
            
            Opcode::SelectSend => {
                // a=chan, b=value; add send case
                let ch = self.read_reg(fiber_id, a) as GcRef;
                let val = self.read_reg(fiber_id, b);
                
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                if let Some(ref mut state) = fiber.select_state {
                    state.cases.push(SelectCase::Send { chan: ch, value: val });
                }
            }
            
            Opcode::SelectRecv => {
                // a=dest, b=chan, c=ok_dest; add recv case
                let ch = self.read_reg(fiber_id, b) as GcRef;
                
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                if let Some(ref mut state) = fiber.select_state {
                    state.cases.push(SelectCase::Recv { chan: ch, dest: a, ok_dest: c });
                }
            }
            
            Opcode::SelectEnd => {
                // a=dest (chosen case index); execute select
                let select_state = {
                    let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                    fiber.select_state.take()
                };
                
                if let Some(state) = select_state {
                    // Try each case to find a ready one
                    let mut ready_cases: Vec<usize> = Vec::new();
                    
                    for (i, case) in state.cases.iter().enumerate() {
                        match case {
                            SelectCase::Send { chan, .. } => {
                                if !chan.is_null() {
                                    let ch_state = channel::get_state(*chan);
                                    let cap = channel::capacity(*chan);
                                    // Ready if buffer not full or has waiting receiver
                                    if ch_state.buffer.len() < cap || !ch_state.waiting_receivers.is_empty() {
                                        ready_cases.push(i);
                                    }
                                }
                            }
                            SelectCase::Recv { chan, .. } => {
                                if !chan.is_null() {
                                    let ch_state = channel::get_state(*chan);
                                    // Ready if buffer not empty or has waiting sender or closed
                                    if !ch_state.buffer.is_empty() || !ch_state.waiting_senders.is_empty() || ch_state.closed {
                                        ready_cases.push(i);
                                    }
                                }
                            }
                        }
                    }
                    
                    let chosen = if !ready_cases.is_empty() {
                        // Pick a random ready case (for fairness, use simple selection)
                        let idx = ready_cases[0]; // TODO: use random selection
                        
                        // Execute the chosen case
                        match &state.cases[idx] {
                            SelectCase::Send { chan, value } => {
                                let ch_state = channel::get_state(*chan);
                                if let Some(recv_id) = ch_state.waiting_receivers.pop_front() {
                                    // Send directly to waiting receiver
                                    self.scheduler.unblock(recv_id as FiberId);
                                } else {
                                    // Buffer the value
                                    ch_state.buffer.push_back(*value);
                                }
                            }
                            SelectCase::Recv { chan, dest, ok_dest } => {
                                let ch_state = channel::get_state(*chan);
                                let (val, ok) = if let Some(v) = ch_state.buffer.pop_front() {
                                    // Wake a waiting sender if any
                                    if let Some((sender_id, send_val)) = ch_state.waiting_senders.pop_front() {
                                        ch_state.buffer.push_back(send_val);
                                        self.scheduler.unblock(sender_id as FiberId);
                                    }
                                    (v, 1u64)
                                } else if let Some((sender_id, v)) = ch_state.waiting_senders.pop_front() {
                                    self.scheduler.unblock(sender_id as FiberId);
                                    (v, 1u64)
                                } else if ch_state.closed {
                                    (0, 0u64)
                                } else {
                                    (0, 0u64)
                                };
                                
                                self.write_reg(fiber_id, *dest, val);
                                if *ok_dest != 0 {
                                    self.write_reg(fiber_id, *ok_dest, ok);
                                }
                            }
                        }
                        
                        idx as u64
                    } else if state.has_default {
                        // No ready case, execute default
                        state.cases.len() as u64 // default case index
                    } else {
                        // No ready case and no default - block
                        // For now, just return 0 (TODO: implement blocking select)
                        0
                    };
                    
                    self.write_reg(fiber_id, a, chosen);
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
                                // Decode UTF-8 to get rune and width
                                let (rune, width) = gox_common_core::utf8::decode_rune(&bytes[pos..]);
                                *byte_pos += width;
                                (false, pos as u64, rune as u64)
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
                let cl = closure::create(&mut self.gc, b as u32, c as usize);
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
            
            Opcode::CallClosure => {
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
                let uv = closure::create_upval_box(&mut self.gc);
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
            
            Opcode::ErrDeferPush => {
                // a=func_id, b=arg_start, c=arg_count
                // ErrDefer is like Defer but only runs when returning with error
                // For now, we implement it the same as DeferPush
                // The VM will need to track whether we're returning with error
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                let frame_depth = fiber.frames.len();
                let mut entry = DeferEntry::new(frame_depth, a as u32);
                entry.arg_count = c as u8;
                entry.is_errdefer = true; // Mark as errdefer
                for i in 0..c.min(8) {
                    entry.args[i as usize] = fiber.read_reg(b + i);
                }
                fiber.defer_stack.push(entry);
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
            Opcode::InitInterface => {
                // a=dest (2 slots), b=iface_type_id, c=unused
                // Initialize interface with iface_type_id, value_kind=Nil, value_type_id=0
                let iface_type_id = b as u16;
                let slot0 = interface::pack_slot0(iface_type_id, 0, ValueKind::Nil as u8);
                self.write_reg(fiber_id, a, slot0);
                self.write_reg(fiber_id, a + 1, 0);
            }
            
            Opcode::BoxInterface => {
                // a=dest (2 slots), b=value_type_id, c=value, flags=value_kind
                // Preserve iface_type_id, set value_kind, value_type_id, and data
                let old_slot0 = self.read_reg(fiber_id, a);
                let iface_type_id = interface::unpack_iface_type_id(old_slot0);
                let value_type_id = b;
                let value_kind = flags;
                let slot0 = interface::pack_slot0(iface_type_id, value_type_id, value_kind);
                let val = self.read_reg(fiber_id, c);
                self.write_reg(fiber_id, a, slot0);
                self.write_reg(fiber_id, a + 1, val);
            }
            
            Opcode::UnboxInterface => {
                // a=dest, b=interface (2 slots), c=value_kind_reg
                // Returns data in a, value_kind in c
                let slot0 = self.read_reg(fiber_id, b);
                let slot1 = self.read_reg(fiber_id, b + 1);
                let value_kind = interface::unpack_value_kind(slot0);
                let data = interface::unbox_data(slot1);
                self.write_reg(fiber_id, a, data);
                self.write_reg(fiber_id, c, value_kind as u8 as u64);
            }
            
            Opcode::TypeAssert => {
                // a=dest, b=interface (2 slots), c=expected_kind, flags=ok_reg
                let slot0 = self.read_reg(fiber_id, b);
                let slot1 = self.read_reg(fiber_id, b + 1);
                let actual_kind = interface::unpack_value_kind(slot0);
                let expected_kind = ValueKind::from_u8(c as u8);
                
                if actual_kind == expected_kind {
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
                // Zero-extend truncation (mask low 32 bits)
                let v = self.read_reg(fiber_id, b);
                let result = v & 0xFFFF_FFFF;
                self.write_reg(fiber_id, a, result);
            }
            
            // ============ Debug ============
            Opcode::DebugPrint => {
                let value_kind = ValueKind::from_u8(b as u8);
                
                let (val, inner_kind, is_iface) = if value_kind == ValueKind::Interface {
                    let slot0 = self.read_reg(fiber_id, a);
                    let inner = interface::unpack_value_kind(slot0);
                    let data = self.read_reg(fiber_id, a + 1);
                    (data, inner, true)
                } else {
                    (self.read_reg(fiber_id, a), value_kind, false)
                };
                
                let s = format_value(val, inner_kind as u32);
                #[cfg(feature = "std")]
                if is_iface {
                    println!("(interface){}", s);
                } else {
                    println!("{}", s);
                }
                #[cfg(not(feature = "std"))]
                let _ = (is_iface, s); // Suppress warnings
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
                    #[cfg(feature = "std")]
                    if arg_count > 0 {
                        eprint!("assertion failed: ");
                    } else {
                        eprintln!("assertion failed");
                    }
                    #[cfg(not(feature = "std"))]
                    let _ = arg_count;
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
                    let value_kind = ValueKind::from_u8(b as u8);
                    
                    let (val, inner_kind, is_iface) = if value_kind == ValueKind::Interface {
                        let slot0 = self.read_reg(fiber_id, a);
                        let inner = interface::unpack_value_kind(slot0);
                        let data = self.read_reg(fiber_id, a + 1);
                        (data, inner, true)
                    } else {
                        (self.read_reg(fiber_id, a), value_kind, false)
                    };
                    
                    let s = format_value(val, inner_kind as u32);
                    #[cfg(feature = "std")]
                    if is_iface {
                        eprint!("(interface){}", s);
                    } else {
                        eprint!("{}", s);
                    }
                    #[cfg(not(feature = "std"))]
                    let _ = (is_iface, s);
                }
            }
            
            Opcode::AssertEnd => {
                let fiber = self.scheduler.get(fiber_id).unwrap();
                if fiber.assert_failed {
                    let line = fiber.assert_line;
                    #[cfg(feature = "std")]
                    {
                        eprintln!();
                        eprintln!("  at line {}", line);
                    }
                    #[cfg(not(feature = "std"))]
                    let _ = line;
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
        // Get return info from current frame
        let (caller_ret_reg, caller_ret_count) = {
            let fiber = self.scheduler.get(fiber_id).unwrap();
            let frame = fiber.frame().unwrap();
            (frame.ret_reg, frame.ret_count as usize)
        };
        
        // Check if this is a defer function returning (ret_count=0, ret_reg=0, has defer_state)
        let is_defer_return = {
            let fiber = self.scheduler.get(fiber_id).unwrap();
            caller_ret_count == 0 && caller_ret_reg == 0 && fiber.defer_state.is_some()
        };
        
        if is_defer_return {
            // Pop the defer's frame and continue with next defer
            {
                let fiber = self.scheduler.get_mut(fiber_id).unwrap();
                fiber.pop_frame();
            }
            return self.execute_next_defer(fiber_id);
        }
        
        // Check if this is an error return (last return value is non-nil interface)
        let is_error_return = if ret_count >= 2 {
            let err_type_slot = self.read_reg(fiber_id, ret_start + (ret_count - 2) as u16);
            err_type_slot != 0
        } else {
            false
        };
        
        // Copy return values
        let ret_vals: Vec<u64> = (0..ret_count)
            .map(|i| self.read_reg(fiber_id, ret_start + i as u16))
            .collect();
        
        // Collect defers (LIFO order), filter errdefers based on is_error_return
        let defers: Vec<_> = {
            let fiber = self.scheduler.get_mut(fiber_id).unwrap();
            fiber.pop_frame_defers()
                .into_iter()
                .filter(|d| !d.is_errdefer || is_error_return)
                .collect()
        };
        
        // Pop the current frame
        {
            let fiber = self.scheduler.get_mut(fiber_id).unwrap();
            fiber.pop_frame();
        }
        
        // If there are defers to execute, save state and start first one
        if !defers.is_empty() {
            let fiber = self.scheduler.get_mut(fiber_id).unwrap();
            fiber.defer_state = Some(crate::fiber::DeferState::new(
                defers,
                ret_vals,
                caller_ret_reg,
                caller_ret_count,
                is_error_return,
            ));
            return self.execute_next_defer(fiber_id);
        }
        
        // No defers - complete return immediately
        self.complete_return(fiber_id, ret_vals, caller_ret_reg, caller_ret_count)
    }
    
    /// Execute the next pending defer, or complete the return if none left.
    fn execute_next_defer(&mut self, fiber_id: FiberId) -> VmResult {
        // Get next defer from state
        let defer = {
            let fiber = self.scheduler.get_mut(fiber_id).unwrap();
            if let Some(ref mut state) = fiber.defer_state {
                if state.pending.is_empty() {
                    // No more defers, complete the return
                    let ret_vals = state.ret_vals.clone();
                    let caller_ret_reg = state.caller_ret_reg;
                    let caller_ret_count = state.caller_ret_count;
                    fiber.defer_state = None;
                    return self.complete_return(fiber_id, ret_vals, caller_ret_reg, caller_ret_count);
                }
                state.pending.remove(0) // Take first defer (already LIFO ordered)
            } else {
                return VmResult::Ok; // No defer state, shouldn't happen
            }
        };
        
        // Get function info and set up call
        let func = self.module.as_ref().unwrap().get_function(defer.func_id);
        if let Some(func) = func {
            let local_slots = func.local_slots as usize;
            let fiber = self.scheduler.get_mut(fiber_id).unwrap();
            let arg_start = fiber.stack.len();
            fiber.stack.extend_from_slice(&defer.args[..defer.arg_count as usize]);
            
            fiber.frames.push(crate::fiber::CallFrame::new(
                defer.func_id,
                arg_start,
                0, // no return register for defer
                0, // no return values - marks this as defer call
            ));
            fiber.ensure_stack(local_slots + 64);
        }
        
        VmResult::Ok
    }
    
    /// Complete the return after all defers have executed.
    fn complete_return(
        &mut self,
        fiber_id: FiberId,
        ret_vals: Vec<u64>,
        caller_ret_reg: u16,
        caller_ret_count: usize,
    ) -> VmResult {
        // Check if fiber is done
        let fiber = self.scheduler.get(fiber_id).unwrap();
        if fiber.frames.is_empty() {
            self.scheduler.kill(fiber_id);
            return VmResult::Yield;
        }
        
        // Store return values in caller's registers
        let actual_ret = ret_vals.len().min(caller_ret_count);
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
    
    // ============ GC Support ============
    
    /// Check if GC should run and perform collection if needed.
    pub fn maybe_collect(&mut self) {
        if !self.gc.should_collect() {
            return;
        }
        self.collect_garbage();
    }
    
    /// Force garbage collection.
    pub fn collect_garbage(&mut self) {
        use crate::bytecode::SlotType;
        
        let module = match &self.module {
            Some(m) => m,
            None => return,
        };
        
        // Mark roots from all fiber stacks
        for fiber in self.scheduler.iter_fibers() {
            // Skip dead fibers or fibers with no frames
            if fiber.frames.is_empty() {
                continue;
            }
            
            // Scan each frame's registers
            for frame in &fiber.frames {
                // Bounds check for func_id
                if frame.func_id as usize >= module.functions.len() {
                    continue;
                }
                let func = &module.functions[frame.func_id as usize];
                let slot_types = &func.slot_types;
                
                // Scan registers based on type info
                for (reg_idx, slot_type) in slot_types.iter().enumerate() {
                    let slot_idx = frame.bp + reg_idx;
                    if slot_idx >= fiber.stack.len() {
                        break;
                    }
                    let val = fiber.stack[slot_idx];
                    
                    match slot_type {
                        SlotType::Value | SlotType::Interface0 => {
                            // Not a pointer, skip
                        }
                        SlotType::GcRef => {
                            if val != 0 {
                                self.gc.mark_gray(val as GcRef);
                            }
                        }
                        SlotType::Interface1 => {
                            // Dynamic check: value_kind in previous slot determines if this is a ref
                            if slot_idx > 0 {
                                let packed = fiber.stack[slot_idx - 1];
                                let value_kind = interface::unpack_value_kind(packed);
                                if value_kind.needs_gc() && val != 0 {
                                    self.gc.mark_gray(val as GcRef);
                                }
                            }
                        }
                    }
                }
                
                // Scan iterator stack (container refs)
                for iter in &fiber.iter_stack {
                    if let Some(ref_val) = iter.container_ref() {
                        if !ref_val.is_null() {
                            self.gc.mark_gray(ref_val);
                        }
                    }
                }
                
                // Scan defer stack args (conservatively treat as potential refs)
                for defer in &fiber.defer_stack {
                    for arg in &defer.args[..defer.arg_count as usize] {
                        if *arg != 0 {
                            // Conservative: mark any non-zero value as potential GcRef
                            // This may over-mark but is safe
                            self.gc.mark_gray(*arg as GcRef);
                        }
                    }
                }
            }
            
            // Scan block reason (channel refs)
            match fiber.block_reason {
                BlockReason::ChanSend(chan) | BlockReason::ChanRecv(chan) => {
                    if !chan.is_null() {
                        self.gc.mark_gray(chan);
                    }
                }
                BlockReason::None => {}
            }
        }
        
        // Mark string constants
        for str_ref in &self.string_constants {
            if !str_ref.is_null() {
                self.gc.mark_gray(*str_ref);
            }
        }
        
        // Mark globals using value_kind for GC scanning
        if let Some(ref module) = self.module {
            let mut slot_idx = 0usize;
            for g in &module.globals {
                let value_kind = ValueKind::from_u8(g.value_kind);
                if value_kind == ValueKind::Interface {
                    // Interface: 2 slots - first is packed info, second may be GcRef
                    let packed = self.globals.get(slot_idx).copied().unwrap_or(0);
                    let inner_kind = interface::unpack_value_kind(packed);
                    let data = self.globals.get(slot_idx + 1).copied().unwrap_or(0);
                    if inner_kind.needs_gc() && data != 0 {
                        self.gc.mark_gray(data as GcRef);
                    }
                } else if value_kind.needs_gc() {
                    // GC type: 1 slot
                    let val = self.globals.get(slot_idx).copied().unwrap_or(0);
                    if val != 0 {
                        self.gc.mark_gray(val as GcRef);
                    }
                }
                // else: Value type, skip
                slot_idx += g.slots as usize;
            }
        }
        
        // Perform collection with unified object scanner
        self.gc.collect(|gc, obj| {
            gox_runtime_core::gc_types::scan_object(gc, obj);
        });
    }
    
    /// Get the number of live GC objects.
    pub fn gc_object_count(&self) -> usize {
        self.gc.object_count()
    }
    
    /// Get the total bytes used by GC objects.
    pub fn gc_total_bytes(&self) -> usize {
        self.gc.total_bytes()
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

/// Format a value for printing based on its ValueKind.
fn format_value(val: u64, value_kind: u32) -> String {
    if value_kind == ValueKind::Float32 as u32 {
        let f = f32::from_bits(val as u32);
        format!("{}", f)
    } else if value_kind == ValueKind::Float64 as u32 {
        let f = f64::from_bits(val);
        if f.abs() >= 1e10 || (f != 0.0 && f.abs() < 1e-4) {
            format!("{:e}", f)
        } else {
            format!("{}", f)
        }
    } else if value_kind == ValueKind::Bool as u32 {
        if val != 0 { "true".to_string() } else { "false".to_string() }
    } else if value_kind == ValueKind::String as u32 {
        let ptr = val as crate::gc::GcRef;
        if ptr.is_null() {
            String::new()
        } else {
            crate::objects::string::as_str(ptr).to_string()
        }
    } else {
        format!("{}", val as i64)
    }
}

/// Get element byte size from ValueKind.
fn elem_bytes_from_value_kind(value_kind: u8) -> usize {
    match value_kind {
        t if t == ValueKind::Bool as u8 => 1,
        t if t == ValueKind::Int8 as u8 => 1,
        t if t == ValueKind::Uint8 as u8 => 1,
        t if t == ValueKind::Int16 as u8 => 2,
        t if t == ValueKind::Uint16 as u8 => 2,
        t if t == ValueKind::Int32 as u8 => 4,
        t if t == ValueKind::Uint32 as u8 => 4,
        t if t == ValueKind::Float32 as u8 => 4,
        _ => 8, // 64-bit types and all references
    }
}
