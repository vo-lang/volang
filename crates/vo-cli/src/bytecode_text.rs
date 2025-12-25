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
                "# [{}] {} ({} slots)\n",
                i,
                s.name,
                s.slot_count()
            ));
            for (j, name) in s.field_names.iter().enumerate() {
                let offset = s.field_offsets.get(j).copied().unwrap_or(0);
                out.push_str(&format!("#   {}: offset={}\n", name, offset));
            }
            if !s.methods.is_empty() {
                out.push_str("#   methods:");
                for (name, fid) in &s.methods {
                    out.push_str(&format!(" {}=func_{}", name, fid));
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
        Opcode::Nop => "Nop".to_string(),
        Opcode::LoadNil => format!("LoadNil       r{}", a),
        Opcode::LoadTrue => format!("LoadTrue      r{}", a),
        Opcode::LoadFalse => format!("LoadFalse     r{}", a),
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
        Opcode::PtrNew => format!("PtrNew        r{}, meta={}, slots={}", a, instr.imm32_unsigned() >> 16, instr.imm32_unsigned() & 0xFFFF),
        Opcode::PtrClone => format!("PtrClone      r{}, r{}", a, b),
        Opcode::PtrGet => format!("PtrGet        r{}, r{}[{}]", a, b, c),
        Opcode::PtrSet => format!("PtrSet        r{}[{}], r{}", a, b, c),
        Opcode::PtrGetN => format!("PtrGetN       r{}, r{}[{}], n={}", a, b, c, flags),
        Opcode::PtrSetN => format!("PtrSetN       r{}[{}], r{}, n={}", a, b, c, flags),

        // ARITH Integer
        Opcode::AddI => format!("AddI          r{}, r{}, r{}", a, b, c),
        Opcode::SubI => format!("SubI          r{}, r{}, r{}", a, b, c),
        Opcode::MulI => format!("MulI          r{}, r{}, r{}", a, b, c),
        Opcode::DivI => format!("DivI          r{}, r{}, r{}", a, b, c),
        Opcode::ModI => format!("ModI          r{}, r{}, r{}", a, b, c),
        Opcode::NegI => format!("NegI          r{}, r{}", a, b),

        // ARITH Float
        Opcode::AddF => format!("AddF          r{}, r{}, r{}", a, b, c),
        Opcode::SubF => format!("SubF          r{}, r{}, r{}", a, b, c),
        Opcode::MulF => format!("MulF          r{}, r{}, r{}", a, b, c),
        Opcode::DivF => format!("DivF          r{}, r{}, r{}", a, b, c),
        Opcode::NegF => format!("NegF          r{}, r{}", a, b),

        // CMP Integer
        Opcode::EqI => format!("EqI           r{}, r{}, r{}", a, b, c),
        Opcode::NeI => format!("NeI           r{}, r{}, r{}", a, b, c),
        Opcode::LtI => format!("LtI           r{}, r{}, r{}", a, b, c),
        Opcode::LeI => format!("LeI           r{}, r{}, r{}", a, b, c),
        Opcode::GtI => format!("GtI           r{}, r{}, r{}", a, b, c),
        Opcode::GeI => format!("GeI           r{}, r{}, r{}", a, b, c),

        // CMP Float
        Opcode::EqF => format!("EqF           r{}, r{}, r{}", a, b, c),
        Opcode::NeF => format!("NeF           r{}, r{}, r{}", a, b, c),
        Opcode::LtF => format!("LtF           r{}, r{}, r{}", a, b, c),
        Opcode::LeF => format!("LeF           r{}, r{}, r{}", a, b, c),
        Opcode::GtF => format!("GtF           r{}, r{}, r{}", a, b, c),
        Opcode::GeF => format!("GeF           r{}, r{}, r{}", a, b, c),

        // CMP Ref
        Opcode::EqRef => format!("EqRef         r{}, r{}, r{}", a, b, c),
        Opcode::NeRef => format!("NeRef         r{}, r{}, r{}", a, b, c),
        Opcode::IsNil => format!("IsNil         r{}, r{}", a, b),

        // BIT
        Opcode::And => format!("And           r{}, r{}, r{}", a, b, c),
        Opcode::Or => format!("Or            r{}, r{}, r{}", a, b, c),
        Opcode::Xor => format!("Xor           r{}, r{}, r{}", a, b, c),
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
        Opcode::Call => format!("Call          r{}, func_{}, args={}", a, b, c),
        Opcode::CallExtern => format!("CallExtern    r{}, extern_{}, args={}", a, b, c),
        Opcode::CallClosure => format!("CallClosure   r{}, r{}, args={}", a, b, c),
        Opcode::CallIface => format!("CallIface     r{}, r{}, method={}, args={}", a, b, c, flags),
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

        // ARRAY
        Opcode::ArrayNew => format!("ArrayNew      r{}, len=r{}, elem_slots={}", a, b, flags),
        Opcode::ArrayGet => format!("ArrayGet      r{}, r{}[r{}], elem_slots={}", a, b, c, flags),
        Opcode::ArraySet => format!("ArraySet      r{}[r{}], r{}, elem_slots={}", a, b, c, flags),
        Opcode::ArrayLen => format!("ArrayLen      r{}, r{}", a, b),

        // SLICE
        Opcode::SliceNew => format!("SliceNew      r{}, r{}, lo=r{}, hi=r{}", a, b, c, flags),
        Opcode::SliceGet => format!("SliceGet      r{}, r{}[r{}], elem_slots={}", a, b, c, flags),
        Opcode::SliceSet => format!("SliceSet      r{}[r{}], r{}, elem_slots={}", a, b, c, flags),
        Opcode::SliceLen => format!("SliceLen      r{}, r{}", a, b),
        Opcode::SliceCap => format!("SliceCap      r{}, r{}", a, b),
        Opcode::SliceSlice => format!("SliceSlice    r{}, r{}, lo=r{}, hi=r{}", a, b, c, flags),
        Opcode::SliceAppend => format!("SliceAppend   r{}, r{}, r{}, elem_slots={}", a, b, c, flags),

        // MAP
        Opcode::MapNew => format!("MapNew        r{}", a),
        Opcode::MapGet => format!("MapGet        r{}, r{}[r{}]", a, b, c),
        Opcode::MapSet => format!("MapSet        r{}[r{}], r{}", a, b, c),
        Opcode::MapDelete => format!("MapDelete     r{}[r{}]", a, b),
        Opcode::MapLen => format!("MapLen        r{}, r{}", a, b),

        // CHAN
        Opcode::ChanNew => format!("ChanNew       r{}, cap={}", a, b),
        Opcode::ChanSend => format!("ChanSend      r{}, r{}", a, b),
        Opcode::ChanRecv => format!("ChanRecv      r{}, r{}", a, b),
        Opcode::ChanClose => format!("ChanClose     r{}", a),

        // SELECT
        Opcode::SelectBegin => format!("SelectBegin   r{}, cases={}", a, b),
        Opcode::SelectSend => format!("SelectSend    r{}, r{}", a, b),
        Opcode::SelectRecv => format!("SelectRecv    r{}, r{}", a, b),
        Opcode::SelectExec => format!("SelectExec    r{}", a),

        // ITER
        Opcode::IterBegin => format!("IterBegin     r{}, r{}, kind={}", a, b, flags),
        Opcode::IterNext => format!("IterNext      r{}, r{}, pc_{}", a, b, instr.imm32()),
        Opcode::IterEnd => format!("IterEnd       r{}", a),

        // CLOSURE
        Opcode::ClosureNew => format!("ClosureNew    r{}, func_{}, captures={}", a, b, c),
        Opcode::ClosureGet => format!("ClosureGet    r{}, r{}[{}]", a, b, c),
        Opcode::ClosureSet => format!("ClosureSet    r{}[{}], r{}", a, b, c),

        // GO
        Opcode::GoCall => format!("GoCall        func_{}, args={}", a, b),
        Opcode::Yield => "Yield".to_string(),

        // DEFER
        Opcode::DeferPush => format!("DeferPush     func_{}, args={}", a, b),
        Opcode::ErrDeferPush => format!("ErrDeferPush  func_{}, args={}", a, b),
        Opcode::Panic => format!("Panic         r{}", a),
        Opcode::Recover => format!("Recover       r{}", a),

        // IFACE
        Opcode::IfaceAssign => format!("IfaceAssign   r{}, r{}, iface_meta={}, vk={}", a, b, c, flags),
        Opcode::IfaceAssert => format!("IfaceAssert   r{}, r{}, target_meta={}", a, b, c),

        // CONV
        Opcode::ConvI2F => format!("ConvI2F       r{}, r{}", a, b),
        Opcode::ConvF2I => format!("ConvF2I       r{}, r{}", a, b),
        Opcode::ConvI32I64 => format!("ConvI32I64    r{}, r{}", a, b),
        Opcode::ConvI64I32 => format!("ConvI64I32    r{}, r{}", a, b),

        Opcode::Invalid => format!("Invalid       op={}, flags={}, a={}, b={}, c={}", instr.op, flags, a, b, c),
    }
}
