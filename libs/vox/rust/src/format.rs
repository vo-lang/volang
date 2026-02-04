//! Bytecode text format parser and formatter.

use vo_vm::bytecode::{Constant, FunctionDef, Module};
use vo_vm::instruction::{Instruction, Opcode};

/// Parse bytecode text format into a Module.
pub fn parse_text(_input: &str) -> Result<Module, String> {
    Err("bytecode text format parsing not yet implemented".into())
}

/// Format a Module as text.
pub fn format_text(module: &Module) -> String {
    let mut out = String::new();

    out.push_str(&format!("# Module: {}\n", module.name));
    out.push_str(&format!("# Entry: func_{}\n\n", module.entry_func));

    // Struct types
    if !module.struct_metas.is_empty() {
        out.push_str("## Struct Types\n");
        for (i, s) in module.struct_metas.iter().enumerate() {
            out.push_str(&format!(
                "# [{}] ({} slots)\n",
                i,
                s.slot_count()
            ));
            for field in &s.fields {
                out.push_str(&format!("#   {}: offset={}, slots={}\n", field.name, field.offset, field.slot_count));
            }
        }
        out.push('\n');
    }

    // Named types
    if !module.named_type_metas.is_empty() {
        out.push_str("## Named Types\n");
        for (i, nt) in module.named_type_metas.iter().enumerate() {
            out.push_str(&format!(
                "# [{}] {} (underlying: meta_id={}, vk={})\n",
                i,
                nt.name,
                nt.underlying_meta.meta_id(),
                nt.underlying_meta.value_kind() as u8
            ));
            if !nt.methods.is_empty() {
                out.push_str("#   methods:");
                for (name, info) in &nt.methods {
                    let ptr_str = if info.is_pointer_receiver { "*" } else { "" };
                    out.push_str(&format!(" {}{}=func_{}", ptr_str, name, info.func_id));
                }
                out.push('\n');
            }
        }
        out.push('\n');
    }

    // Interface types
    if !module.interface_metas.is_empty() {
        out.push_str("## Interface Types\n");
        for (i, iface) in module.interface_metas.iter().enumerate() {
            out.push_str(&format!("# [{}] {}\n", i, iface.name));
            for name in &iface.method_names {
                out.push_str(&format!("#   method {}\n", name));
            }
        }
        out.push('\n');
    }

    // Itabs
    if !module.itabs.is_empty() {
        out.push_str("## Itabs\n");
        for (i, itab) in module.itabs.iter().enumerate() {
            out.push_str(&format!("# [{}] methods: {:?}\n", i, itab.methods));
        }
        out.push('\n');
    }

    // Constants
    if !module.constants.is_empty() {
        out.push_str("## Constants\n");
        for (i, c) in module.constants.iter().enumerate() {
            out.push_str(&format!("# [{}] {}\n", i, format_constant(c)));
        }
        out.push('\n');
    }

    // Globals
    if !module.globals.is_empty() {
        out.push_str("## Globals\n");
        for (i, g) in module.globals.iter().enumerate() {
            out.push_str(&format!(
                "# [{}] {}: {} slot(s), vk={}, meta={}\n",
                i, g.name, g.slots, g.value_kind, g.meta_id
            ));
        }
        out.push('\n');
    }

    // Externs
    if !module.externs.is_empty() {
        out.push_str("## Externs\n");
        for (i, e) in module.externs.iter().enumerate() {
            out.push_str(&format!(
                "# [{}] {}({}) -> {}\n",
                i, e.name, e.param_slots, e.ret_slots
            ));
        }
        out.push('\n');
    }

    // Functions
    out.push_str("## Functions\n\n");
    for (i, f) in module.functions.iter().enumerate() {
        out.push_str(&format_function(i as u32, f));
        out.push('\n');
    }

    out
}

fn format_constant(c: &Constant) -> String {
    match c {
        Constant::Nil => "nil".to_string(),
        Constant::Bool(b) => format!("bool {}", b),
        Constant::Int(i) => format!("int {}", i),
        Constant::Float(f) => format!("float {}", f),
        Constant::String(s) => format!("string {:?}", s),
    }
}

fn format_function(func_id: u32, f: &FunctionDef) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "func_{} {}(params={}, param_slots={}, locals={}, ret={}):\n",
        func_id, f.name, f.param_count, f.param_slots, f.local_slots, f.ret_slots
    ));

    for (pc, instr) in f.code.iter().enumerate() {
        out.push_str(&format!("  {:04}: {}\n", pc, format_instruction(instr)));
    }

    out
}

fn format_instruction(instr: &Instruction) -> String {
    let op = instr.opcode();
    let a = instr.a;
    let b = instr.b;
    let c = instr.c;
    let flags = instr.flags;

    match op {
        // LOAD
        Opcode::Hint => format!("Hint          flags={}, a={}, bc={}", flags, a, instr.imm32_unsigned()),
        Opcode::LoadInt => format!("LoadInt       r{}, {}", a, instr.imm32()),
        Opcode::LoadConst => format!("LoadConst     r{}, const_{}", a, b),

        // COPY
        Opcode::Copy => format!("Copy          r{}, r{}", a, b),
        Opcode::CopyN => format!("CopyN         r{}, r{}, n={}", a, b, c),

        // SLOT
        Opcode::SlotGet => format!("SlotGet       r{}, r{}[r{}]", a, b, c),
        Opcode::SlotSet => format!("SlotSet       r{}[r{}], r{}", a, b, c),
        Opcode::SlotGetN => format!("SlotGetN      r{}, r{}[r{}], n={}", a, b, c, flags),
        Opcode::SlotSetN => format!("SlotSetN      r{}[r{}], r{}, n={}", a, b, c, flags),

        // GLOBAL
        Opcode::GlobalGet => format!("GlobalGet     r{}, global_{}", a, b),
        Opcode::GlobalGetN => format!("GlobalGetN    r{}, global_{}, n={}", a, b, flags),
        Opcode::GlobalSet => format!("GlobalSet     global_{}, r{}", a, b),
        Opcode::GlobalSetN => format!("GlobalSetN    global_{}, r{}, n={}", a, b, flags),

        // PTR
        Opcode::PtrNew => format!("PtrNew        r{}, meta=r{}, slots={}", a, b, flags),
        Opcode::PtrGet => format!("PtrGet        r{}, r{}[{}]", a, b, c),
        Opcode::PtrSet => format!("PtrSet        r{}[{}], r{}", a, b, c),
        Opcode::PtrGetN => format!("PtrGetN       r{}, r{}[{}], n={}", a, b, c, flags),
        Opcode::PtrSetN => format!("PtrSetN       r{}[{}], r{}, n={}", a, b, c, flags),
        Opcode::PtrAdd => format!("PtrAdd        r{}, r{}, r{}", a, b, c),

        // ARITH Integer
        Opcode::AddI => format!("AddI          r{}, r{}, r{}", a, b, c),
        Opcode::SubI => format!("SubI          r{}, r{}, r{}", a, b, c),
        Opcode::MulI => format!("MulI          r{}, r{}, r{}", a, b, c),
        Opcode::DivI => format!("DivI          r{}, r{}, r{}", a, b, c),
        Opcode::DivU => format!("DivU          r{}, r{}, r{}", a, b, c),
        Opcode::ModI => format!("ModI          r{}, r{}, r{}", a, b, c),
        Opcode::ModU => format!("ModU          r{}, r{}, r{}", a, b, c),
        Opcode::NegI => format!("NegI          r{}, r{}", a, b),

        // ARITH Float
        Opcode::AddF => format!("AddF          r{}, r{}, r{}", a, b, c),
        Opcode::SubF => format!("SubF          r{}, r{}, r{}", a, b, c),
        Opcode::MulF => format!("MulF          r{}, r{}, r{}", a, b, c),
        Opcode::DivF => format!("DivF          r{}, r{}, r{}", a, b, c),
        Opcode::NegF => format!("NegF          r{}, r{}", a, b),

        // CMP Integer (signed)
        Opcode::EqI => format!("EqI           r{}, r{}, r{}", a, b, c),
        Opcode::NeI => format!("NeI           r{}, r{}, r{}", a, b, c),
        Opcode::LtI => format!("LtI           r{}, r{}, r{}", a, b, c),
        Opcode::LeI => format!("LeI           r{}, r{}, r{}", a, b, c),
        Opcode::GtI => format!("GtI           r{}, r{}, r{}", a, b, c),
        Opcode::GeI => format!("GeI           r{}, r{}, r{}", a, b, c),
        
        // CMP Integer (unsigned)
        Opcode::LtU => format!("LtU           r{}, r{}, r{}", a, b, c),
        Opcode::LeU => format!("LeU           r{}, r{}, r{}", a, b, c),
        Opcode::GtU => format!("GtU           r{}, r{}, r{}", a, b, c),
        Opcode::GeU => format!("GeU           r{}, r{}, r{}", a, b, c),

        // CMP Float
        Opcode::EqF => format!("EqF           r{}, r{}, r{}", a, b, c),
        Opcode::NeF => format!("NeF           r{}, r{}, r{}", a, b, c),
        Opcode::LtF => format!("LtF           r{}, r{}, r{}", a, b, c),
        Opcode::LeF => format!("LeF           r{}, r{}, r{}", a, b, c),
        Opcode::GtF => format!("GtF           r{}, r{}, r{}", a, b, c),
        Opcode::GeF => format!("GeF           r{}, r{}, r{}", a, b, c),

        // BIT
        Opcode::And => format!("And           r{}, r{}, r{}", a, b, c),
        Opcode::Or => format!("Or            r{}, r{}, r{}", a, b, c),
        Opcode::Xor => format!("Xor           r{}, r{}, r{}", a, b, c),
        Opcode::AndNot => format!("AndNot        r{}, r{}, r{}", a, b, c),
        Opcode::Not => format!("Not           r{}, r{}", a, b),
        Opcode::Shl => format!("Shl           r{}, r{}, r{}", a, b, c),
        Opcode::ShrS => format!("ShrS          r{}, r{}, r{}", a, b, c),
        Opcode::ShrU => format!("ShrU          r{}, r{}, r{}", a, b, c),

        // LOGIC
        Opcode::BoolNot => format!("BoolNot       r{}, r{}", a, b),

        // JUMP
        Opcode::Jump => format!("Jump          pc_{}", instr.imm32()),
        Opcode::JumpIf => format!("JumpIf        r{}, pc_{}", a, instr.imm32()),
        Opcode::JumpIfNot => format!("JumpIfNot     r{}, pc_{}", a, instr.imm32()),

        // CALL
        // a=func_id_low, b=args_start, c=(arg_slots<<8|ret_slots), flags=func_id_high
        Opcode::Call => {
            let func_id = a as u32 | ((flags as u32) << 16);
            let arg_slots = c >> 8;
            let ret_slots = c & 0xFF;
            format!("Call          func_{}, args=r{}, arg_slots={}, ret_slots={}", func_id, b, arg_slots, ret_slots)
        }
        // CallExtern: a=result_start, b=extern_id, c=arg_start, flags=arg_count
        Opcode::CallExtern => format!("CallExtern    r{}, extern_{}, args={}, count={}", a, b, c, flags),
        // CallClosure: a=closure_reg, b=args_start, c=(arg_slots<<8|ret_slots)
        Opcode::CallClosure => {
            let arg_slots = c >> 8;
            let ret_slots = c & 0xFF;
            format!("CallClosure   r{}, r{}, arg_slots={}, ret_slots={}", a, b, arg_slots, ret_slots)
        }
        // CallIface: a=iface_slot, b=args_start, c=(arg_slots<<8|ret_slots), flags=method_idx
        Opcode::CallIface => {
            let arg_slots = c >> 8;
            let ret_slots = c & 0xFF;
            format!("CallIface     r{}, r{}, method={}, arg_slots={}, ret_slots={}", a, b, flags, arg_slots, ret_slots)
        }
        Opcode::Return => {
            if a == 0 && b == 0 {
                "Return".to_string()
            } else {
                format!("Return        r{}, count={}", a, b)
            }
        }

        // STR
        Opcode::StrNew => format!("StrNew        r{}, const_{}", a, b),
        Opcode::StrLen => format!("StrLen        r{}, r{}", a, b),
        Opcode::StrIndex => format!("StrIndex      r{}, r{}, r{}", a, b, c),
        Opcode::StrConcat => format!("StrConcat     r{}, r{}, r{}", a, b, c),
        Opcode::StrSlice => format!("StrSlice      r{}, r{}, r{}, r{}", a, b, c, flags),
        Opcode::StrEq => format!("StrEq         r{}, r{}, r{}", a, b, c),
        Opcode::StrNe => format!("StrNe         r{}, r{}, r{}", a, b, c),
        Opcode::StrLt => format!("StrLt         r{}, r{}, r{}", a, b, c),
        Opcode::StrLe => format!("StrLe         r{}, r{}, r{}", a, b, c),
        Opcode::StrGt => format!("StrGt         r{}, r{}, r{}", a, b, c),
        Opcode::StrGe => format!("StrGe         r{}, r{}, r{}", a, b, c),
        Opcode::StrDecodeRune => format!("StrDecodeRune r{}, r{}, r{}", a, b, c),

        // ARRAY
        // ArrayNew: a=dst, b=meta_reg, c=len_reg, flags=elem_bytes_encoding
        Opcode::ArrayNew => format!("ArrayNew      r{}, meta=r{}, len=r{}, flags={}", a, b, c, flags),
        // ArrayGet: a=dst, b=array, c=idx, flags=elem_bytes_encoding
        Opcode::ArrayGet => format!("ArrayGet      r{}, r{}[r{}], flags={}", a, b, c, flags),
        // ArraySet: a=array, b=idx, c=val, flags=elem_bytes_encoding
        Opcode::ArraySet => format!("ArraySet      r{}[r{}], r{}, flags={}", a, b, c, flags),
        Opcode::ArrayAddr => format!("ArrayAddr     r{}, r{}[r{}], elem_bytes={}", a, b, c, flags),

        // SLICE
        // SliceNew: a=dst, b=meta_reg, c=len_reg (len at c, cap at c+1), flags=elem_bytes_encoding
        Opcode::SliceNew => format!("SliceNew      r{}, meta=r{}, len=r{}, flags={}", a, b, c, flags),
        // SliceGet: a=dst, b=slice, c=idx, flags=elem_bytes_encoding
        Opcode::SliceGet => format!("SliceGet      r{}, r{}[r{}], elem_slots={}", a, b, c, flags),
        // SliceSet: a=slice, b=idx, c=val, flags=elem_bytes_encoding
        Opcode::SliceSet => format!("SliceSet      r{}[r{}], r{}, elem_slots={}", a, b, c, flags),
        Opcode::SliceLen => format!("SliceLen      r{}, r{}", a, b),
        Opcode::SliceCap => format!("SliceCap      r{}, r{}", a, b),
        // SliceSlice: a=dst, b=src, c=lo_reg (lo at c, hi at c+1), flags=mode
        Opcode::SliceSlice => {
            let is_array = (flags & 1) != 0;
            let has_max = (flags & 2) != 0;
            let src_type = if is_array { "array" } else { "slice" };
            let max_str = if has_max { ", has_max" } else { "" };
            format!("SliceSlice    r{}, r{}[r{}:], src={}{}", a, b, c, src_type, max_str)
        }
        // SliceAppend: a=dst, b=slice, c=meta_reg, flags=elem_bytes_encoding
        Opcode::SliceAppend => format!("SliceAppend   r{}, r{}, meta=r{}, flags={}", a, b, c, flags),
        Opcode::SliceAddr => format!("SliceAddr     r{}, r{}[r{}], elem_bytes={}", a, b, c, flags),

        // MAP
        Opcode::MapNew => format!("MapNew        r{}", a),
        Opcode::MapGet => format!("MapGet        r{}, r{}[r{}]", a, b, c),
        Opcode::MapSet => format!("MapSet        r{}[r{}], r{}", a, b, c),
        Opcode::MapDelete => format!("MapDelete     r{}[r{}]", a, b),
        Opcode::MapLen => format!("MapLen        r{}, r{}", a, b),
        Opcode::MapIterInit => format!("MapIterInit   r{}, r{}", a, b),
        Opcode::MapIterNext => {
            let key_slots = flags & 0x0F;
            let val_slots = (flags >> 4) & 0x0F;
            format!("MapIterNext   r{}, iter=r{}, ok=r{}, key_slots={}, val_slots={}", a, b, c, key_slots, val_slots)
        }

        // CHAN
        Opcode::ChanNew => format!("ChanNew       r{}, meta=r{}, cap=r{}, slots={}", a, b, c, flags),
        Opcode::ChanSend => format!("ChanSend      r{}, r{}, slots={}", a, b, flags),
        Opcode::ChanRecv => format!("ChanRecv      r{}, r{}, slots={}", a, b, (flags >> 1) & 0x7F),
        Opcode::ChanClose => format!("ChanClose     r{}", a),
        Opcode::ChanLen => format!("ChanLen       r{}, r{}", a, b),
        Opcode::ChanCap => format!("ChanCap       r{}, r{}", a, b),

        // SELECT
        Opcode::SelectBegin => format!("SelectBegin   r{}, cases={}", a, b),
        Opcode::SelectSend => format!("SelectSend    r{}, r{}", a, b),
        Opcode::SelectRecv => format!("SelectRecv    r{}, r{}", a, b),
        Opcode::SelectExec => format!("SelectExec    r{}", a),

        // CLOSURE
        // ClosureNew: a=dst, b=func_id_low, c=capture_count, flags=func_id_high
        Opcode::ClosureNew => {
            let func_id = b as u32 | ((flags as u32) << 16);
            format!("ClosureNew    r{}, func_{}, captures={}", a, func_id, c)
        }
        // ClosureGet: a=dst, b=capture_index (closure ref is always at r0)
        Opcode::ClosureGet => format!("ClosureGet    r{}, capture[{}]", a, b),

        // GO
        // a=func_id_low/closure_reg, b=args_start, c=arg_slots, flags bit0=is_closure
        Opcode::GoStart => {
            let is_closure = (flags & 1) != 0;
            if is_closure {
                format!("GoStart       closure=r{}, args=r{}, slots={}", a, b, c)
            } else {
                let func_id = a as u32 | (((flags >> 1) as u32) << 16);
                format!("GoStart       func_{}, args=r{}, slots={}", func_id, b, c)
            }
        }

        // DEFER
        // a=func_id_low/closure_reg, b=arg_start, c=arg_slots, flags bit0=is_closure
        Opcode::DeferPush => {
            let is_closure = (flags & 1) != 0;
            if is_closure {
                format!("DeferPush     closure=r{}, args=r{}, slots={}", a, b, c)
            } else {
                let func_id = a as u32 | (((flags >> 1) as u32) << 16);
                format!("DeferPush     func_{}, args=r{}, slots={}", func_id, b, c)
            }
        }
        Opcode::ErrDeferPush => {
            let is_closure = (flags & 1) != 0;
            if is_closure {
                format!("ErrDeferPush  closure=r{}, args=r{}, slots={}", a, b, c)
            } else {
                let func_id = a as u32 | (((flags >> 1) as u32) << 16);
                format!("ErrDeferPush  func_{}, args=r{}, slots={}", func_id, b, c)
            }
        }
        Opcode::Panic => format!("Panic         r{}", a),
        Opcode::Recover => format!("Recover       r{}", a),

        // IFACE
        // IfaceAssign: a=dst(2 slots), b=src, c=const_idx, flags=value_kind
        Opcode::IfaceAssign => format!("IfaceAssign   r{}, r{}, const={}, vk={}", a, b, c, flags),
        Opcode::IfaceAssert => format!("IfaceAssert   r{}, r{}, target_meta={}, flags={}", a, b, c, flags),
        Opcode::IfaceEq => format!("IfaceEq       r{}, r{}, r{}", a, b, c),

        // CONV
        Opcode::ConvI2F => format!("ConvI2F       r{}, r{}", a, b),
        Opcode::ConvF2I => format!("ConvF2I       r{}, r{}", a, b),
        Opcode::ConvF64F32 => format!("ConvF64F32    r{}, r{}", a, b),
        Opcode::ConvF32F64 => format!("ConvF32F64    r{}, r{}", a, b),
        Opcode::Trunc => {
            let signed = (flags & 0x80) != 0;
            let bytes = flags & 0x7F;
            let ty = match (bytes, signed) {
                (1, true) => "i8",
                (2, true) => "i16",
                (4, true) => "i32",
                (1, false) => "u8",
                (2, false) => "u16",
                (4, false) => "u32",
                _ => "?",
            };
            format!("Trunc         r{}, r{}, {}", a, b, ty)
        }

        Opcode::IndexCheck => format!("IndexCheck    r{}, r{}", a, b),

        // Island/Port operations
        Opcode::IslandNew => format!("IslandNew     r{}", a),
        Opcode::PortNew => format!("PortNew       r{}, meta={}, cap={}, elem_slots={}", a, b, c, flags),
        Opcode::PortSend => format!("PortSend      r{}, r{}, elem_slots={}", a, b, flags),
        Opcode::PortRecv => format!("PortRecv      r{}, r{}, elem_slots={}, has_ok={}", a, b, flags >> 1, flags & 1),
        Opcode::PortClose => format!("PortClose     r{}", a),
        Opcode::PortLen => format!("PortLen       r{}, r{}", a, b),
        Opcode::PortCap => format!("PortCap       r{}, r{}", a, b),
        Opcode::GoIsland => format!("GoIsland      r{}, r{}, capture_slots={}", a, b, flags),
        Opcode::ForLoop => format!("ForLoop       r{}, r{}, offset={}, flags={}", a, b, c as i16, flags),

        Opcode::Invalid => format!("Invalid       op={}, flags={}, a={}, b={}, c={}", instr.op, flags, a, b, c),
    }
}
