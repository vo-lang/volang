//! Bytecode text format parser and formatter.
//!
//! Text format example:
//! ```text
//! module test
//! 
//! const 0 int 42
//! const 1 string "hello"
//! 
//! native 0 "fmt.Println" 1 0
//! 
//! func main
//!   locals 10
//!   0: LoadInt r0, 5
//!   1: CallNative 0, r0, 1, 1
//!   2: Return r0, 0
//! end
//! 
//! entry main
//! ```

use gox_vm::{Module, FunctionDef, Constant, Instruction, Opcode};
use std::collections::HashMap;

/// Parse bytecode text format into a Module.
pub fn parse_text(input: &str) -> Result<Module, String> {
    let mut parser = TextParser::new(input);
    parser.parse()
}

/// Format a Module as text.
pub fn format_text(module: &Module) -> String {
    let mut out = String::new();
    
    // Module name
    out.push_str(&format!("module {}\n\n", module.name));
    
    // Constants
    for (i, c) in module.constants.iter().enumerate() {
        out.push_str(&format!("const {} {}\n", i, format_constant(c)));
    }
    if !module.constants.is_empty() {
        out.push('\n');
    }
    
    // Natives
    for (i, n) in module.natives.iter().enumerate() {
        out.push_str(&format!("native {} \"{}\" {} {}\n", i, n.name, n.param_slots, n.ret_slots));
    }
    if !module.natives.is_empty() {
        out.push('\n');
    }
    
    // Functions
    for f in &module.functions {
        out.push_str(&format!("func {}\n", f.name));
        if f.param_count > 0 {
            out.push_str(&format!("  params {} {}\n", f.param_count, f.param_slots));
        }
        out.push_str(&format!("  locals {}\n", f.local_slots));
        if f.ret_slots > 0 {
            out.push_str(&format!("  returns {}\n", f.ret_slots));
        }
        for (i, instr) in f.code.iter().enumerate() {
            out.push_str(&format!("  {}: {}\n", i, format_instruction(instr)));
        }
        out.push_str("end\n\n");
    }
    
    // Entry
    if let Some(func) = module.functions.get(module.entry_func as usize) {
        out.push_str(&format!("entry {}\n", func.name));
    }
    
    out
}

fn format_constant(c: &Constant) -> String {
    match c {
        Constant::Nil => "nil".to_string(),
        Constant::Bool(v) => format!("bool {}", v),
        Constant::Int(v) => format!("int {}", v),
        Constant::Float(v) => format!("float {}", v),
        Constant::String(s) => format!("string {:?}", s),
    }
}

fn format_instruction(instr: &Instruction) -> String {
    let op = instr.opcode();
    let a = instr.a;
    let b = instr.b;
    let c = instr.c;
    let flags = instr.flags;
    
    match op {
        Opcode::Nop => "Nop".to_string(),
        Opcode::LoadNil => format!("LoadNil r{}", a),
        Opcode::LoadTrue => format!("LoadTrue r{}", a),
        Opcode::LoadFalse => format!("LoadFalse r{}", a),
        Opcode::LoadInt => format!("LoadInt r{}, {}", a, instr.imm32()),
        Opcode::LoadConst => format!("LoadConst r{}, ${}", a, b),
        Opcode::Mov => format!("Mov r{}, r{}", a, b),
        Opcode::MovN => format!("MovN r{}, r{}, {}", a, b, c),
        
        Opcode::GetGlobal => format!("GetGlobal r{}, g{}", a, b),
        Opcode::SetGlobal => format!("SetGlobal g{}, r{}", a, b),
        
        Opcode::AddI64 => format!("AddI64 r{}, r{}, r{}", a, b, c),
        Opcode::SubI64 => format!("SubI64 r{}, r{}, r{}", a, b, c),
        Opcode::MulI64 => format!("MulI64 r{}, r{}, r{}", a, b, c),
        Opcode::DivI64 => format!("DivI64 r{}, r{}, r{}", a, b, c),
        Opcode::ModI64 => format!("ModI64 r{}, r{}, r{}", a, b, c),
        Opcode::NegI64 => format!("NegI64 r{}, r{}", a, b),
        
        Opcode::AddF64 => format!("AddF64 r{}, r{}, r{}", a, b, c),
        Opcode::SubF64 => format!("SubF64 r{}, r{}, r{}", a, b, c),
        Opcode::MulF64 => format!("MulF64 r{}, r{}, r{}", a, b, c),
        Opcode::DivF64 => format!("DivF64 r{}, r{}, r{}", a, b, c),
        Opcode::NegF64 => format!("NegF64 r{}, r{}", a, b),
        
        Opcode::EqI64 => format!("EqI64 r{}, r{}, r{}", a, b, c),
        Opcode::NeI64 => format!("NeI64 r{}, r{}, r{}", a, b, c),
        Opcode::LtI64 => format!("LtI64 r{}, r{}, r{}", a, b, c),
        Opcode::LeI64 => format!("LeI64 r{}, r{}, r{}", a, b, c),
        Opcode::GtI64 => format!("GtI64 r{}, r{}, r{}", a, b, c),
        Opcode::GeI64 => format!("GeI64 r{}, r{}, r{}", a, b, c),
        
        Opcode::EqF64 => format!("EqF64 r{}, r{}, r{}", a, b, c),
        Opcode::NeF64 => format!("NeF64 r{}, r{}, r{}", a, b, c),
        Opcode::LtF64 => format!("LtF64 r{}, r{}, r{}", a, b, c),
        Opcode::LeF64 => format!("LeF64 r{}, r{}, r{}", a, b, c),
        Opcode::GtF64 => format!("GtF64 r{}, r{}, r{}", a, b, c),
        Opcode::GeF64 => format!("GeF64 r{}, r{}, r{}", a, b, c),
        
        Opcode::EqRef => format!("EqRef r{}, r{}, r{}", a, b, c),
        Opcode::NeRef => format!("NeRef r{}, r{}, r{}", a, b, c),
        Opcode::IsNil => format!("IsNil r{}, r{}", a, b),
        
        Opcode::Band => format!("Band r{}, r{}, r{}", a, b, c),
        Opcode::Bor => format!("Bor r{}, r{}, r{}", a, b, c),
        Opcode::Bxor => format!("Bxor r{}, r{}, r{}", a, b, c),
        Opcode::Bnot => format!("Bnot r{}, r{}", a, b),
        Opcode::Shl => format!("Shl r{}, r{}, r{}", a, b, c),
        Opcode::Shr => format!("Shr r{}, r{}, r{}", a, b, c),
        Opcode::Ushr => format!("Ushr r{}, r{}, r{}", a, b, c),
        
        Opcode::Not => format!("Not r{}, r{}", a, b),
        
        Opcode::Jump => format!("Jump {}", instr.imm32()),
        Opcode::JumpIf => format!("JumpIf r{}, {}", a, instr.imm32()),
        Opcode::JumpIfNot => format!("JumpIfNot r{}, {}", a, instr.imm32()),
        
        Opcode::Call => format!("Call {}, r{}, {}, {}", a, b, c, flags),
        Opcode::Return => format!("Return r{}, {}", a, b),
        
        Opcode::CallNative => format!("CallNative {}, r{}, {}, {}", a, b, c, flags),
        
        Opcode::Alloc => format!("Alloc r{}, {}, {}", a, b, c),
        Opcode::GetField => format!("GetField r{}, r{}, {}", a, b, c),
        Opcode::SetField => format!("SetField r{}, {}, r{}", a, b, c),
        Opcode::GetFieldN => format!("GetFieldN r{}, r{}, {}, {}", a, b, c, flags),
        Opcode::SetFieldN => format!("SetFieldN r{}, r{}, {}, {}", a, b, c, flags),
        Opcode::StructHash => format!("StructHash r{}, r{}, {}", a, b, c),
        
        Opcode::ArrayNew => format!("ArrayNew r{}, {}, {}", a, b, c),
        Opcode::ArrayGet => format!("ArrayGet r{}, r{}, r{}", a, b, c),
        Opcode::ArraySet => format!("ArraySet r{}, r{}, r{}", a, b, c),
        Opcode::ArrayLen => format!("ArrayLen r{}, r{}", a, b),
        
        Opcode::SliceNew => format!("SliceNew r{}, r{}, {}, {}", a, b, c, flags),
        Opcode::SliceGet => format!("SliceGet r{}, r{}, r{}", a, b, c),
        Opcode::SliceSet => format!("SliceSet r{}, r{}, r{}", a, b, c),
        Opcode::SliceLen => format!("SliceLen r{}, r{}", a, b),
        Opcode::SliceCap => format!("SliceCap r{}, r{}", a, b),
        Opcode::SliceSlice => format!("SliceSlice r{}, r{}, {}, {}", a, b, c, flags),
        Opcode::SliceAppend => format!("SliceAppend r{}, r{}, r{}", a, b, c),
        
        Opcode::StrNew => format!("StrNew r{}, ${}", a, b),
        Opcode::StrConcat => format!("StrConcat r{}, r{}, r{}", a, b, c),
        Opcode::StrLen => format!("StrLen r{}, r{}", a, b),
        Opcode::StrIndex => format!("StrIndex r{}, r{}, r{}", a, b, c),
        
        Opcode::MapNew => format!("MapNew r{}, {}, {}", a, b, c),
        Opcode::MapGet => format!("MapGet r{}, r{}, r{}", a, b, c),
        Opcode::MapSet => format!("MapSet r{}, r{}, r{}", a, b, c),
        Opcode::MapDelete => format!("MapDelete r{}, r{}", a, b),
        Opcode::MapLen => format!("MapLen r{}, r{}", a, b),
        
        Opcode::ChanNew => format!("ChanNew r{}, {}, {}", a, b, c),
        Opcode::ChanSend => format!("ChanSend r{}, r{}", a, b),
        Opcode::ChanRecv => format!("ChanRecv r{}, r{}, r{}", a, b, c),
        Opcode::ChanClose => format!("ChanClose r{}", a),
        
        Opcode::IterBegin => format!("IterBegin r{}, {}", a, b),
        Opcode::IterNext => format!("IterNext r{}, r{}, {}", a, b, c),
        Opcode::IterEnd => "IterEnd".to_string(),
        
        Opcode::ClosureNew => format!("ClosureNew r{}, {}, {}", a, b, c),
        Opcode::ClosureGet => format!("ClosureGet r{}, r{}, {}", a, b, c),
        Opcode::ClosureSet => format!("ClosureSet r{}, {}, r{}", a, b, c),
        Opcode::ClosureCall => format!("ClosureCall r{}, r{}, {} (ret={})", a, b, c, flags),
        Opcode::UpvalNew => format!("UpvalNew r{}", a),
        Opcode::UpvalGet => format!("UpvalGet r{}, r{}", a, b),
        Opcode::UpvalSet => format!("UpvalSet r{}, r{}", a, b),
        
        Opcode::Go => format!("Go {}, r{}, {}", a, b, c),
        Opcode::Yield => "Yield".to_string(),
        
        Opcode::DeferPush => format!("DeferPush {}, r{}, {}", a, b, c),
        Opcode::DeferPop => "DeferPop".to_string(),
        Opcode::Panic => format!("Panic r{}", a),
        Opcode::Recover => format!("Recover r{}", a),
        
        Opcode::BoxInterface => format!("BoxInterface r{}, {}, r{}", a, b, c),
        Opcode::UnboxInterface => format!("UnboxInterface r{}, r{}, r{}", a, b, c),
        Opcode::TypeAssert => format!("TypeAssert r{}, r{}, {}, {}", a, b, c, flags),
        
        Opcode::I64ToF64 => format!("I64ToF64 r{}, r{}", a, b),
        Opcode::F64ToI64 => format!("F64ToI64 r{}, r{}", a, b),
        Opcode::I32ToI64 => format!("I32ToI64 r{}, r{}", a, b),
        Opcode::I64ToI32 => format!("I64ToI32 r{}, r{}", a, b),
        
        Opcode::DebugPrint => format!("DebugPrint r{}", a),
        Opcode::AssertBegin => format!("AssertBegin r{}, {}, {}", a, b, c),
        Opcode::AssertArg => format!("AssertArg r{}, {}", a, b),
        Opcode::AssertEnd => "AssertEnd".to_string(),
        
        Opcode::Invalid => format!("Invalid {}, {}, {}", a, b, c),
    }
}

// === Parser ===

struct TextParser<'a> {
    lines: Vec<&'a str>,
    pos: usize,
    module: Module,
    func_names: HashMap<String, u32>,
}

impl<'a> TextParser<'a> {
    fn new(input: &'a str) -> Self {
        let lines: Vec<&str> = input.lines().collect();
        Self {
            lines,
            pos: 0,
            module: Module::new(""),
            func_names: HashMap::new(),
        }
    }
    
    fn parse(&mut self) -> Result<Module, String> {
        while self.pos < self.lines.len() {
            let line = self.lines[self.pos].trim();
            self.pos += 1;
            
            if line.is_empty() || line.starts_with('#') || line.starts_with("//") {
                continue;
            }
            
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.is_empty() {
                continue;
            }
            
            match parts[0] {
                "module" => {
                    if parts.len() < 2 {
                        return Err("module name required".to_string());
                    }
                    self.module.name = parts[1].to_string();
                }
                "const" => self.parse_const(&parts)?,
                "native" => self.parse_native(&parts)?,
                "func" => self.parse_func(&parts)?,
                "entry" => {
                    if parts.len() < 2 {
                        return Err("entry function name required".to_string());
                    }
                    let name = parts[1];
                    if let Some(&idx) = self.func_names.get(name) {
                        self.module.entry_func = idx;
                    } else {
                        return Err(format!("unknown function: {}", name));
                    }
                }
                _ => return Err(format!("unknown directive: {}", parts[0])),
            }
        }
        
        Ok(std::mem::take(&mut self.module))
    }
    
    fn parse_const(&mut self, parts: &[&str]) -> Result<(), String> {
        // const <idx> <type> <value>
        if parts.len() < 3 {
            return Err("const requires index and type".to_string());
        }
        
        let const_type = parts[2];
        let constant = match const_type {
            "nil" => Constant::Nil,
            "bool" => {
                if parts.len() < 4 {
                    return Err("bool constant requires value".to_string());
                }
                Constant::Bool(parts[3] == "true")
            }
            "int" => {
                if parts.len() < 4 {
                    return Err("int constant requires value".to_string());
                }
                let v: i64 = parts[3].parse().map_err(|_| "invalid int")?;
                Constant::Int(v)
            }
            "float" => {
                if parts.len() < 4 {
                    return Err("float constant requires value".to_string());
                }
                let v: f64 = parts[3].parse().map_err(|_| "invalid float")?;
                Constant::Float(v)
            }
            "string" => {
                // Rejoin the rest and parse as string literal
                let rest = parts[3..].join(" ");
                let s = parse_string_literal(&rest)?;
                Constant::String(s)
            }
            _ => return Err(format!("unknown constant type: {}", const_type)),
        };
        
        self.module.constants.push(constant);
        Ok(())
    }
    
    fn parse_native(&mut self, parts: &[&str]) -> Result<(), String> {
        // native <idx> "<name>" <param_slots> <ret_slots>
        if parts.len() < 5 {
            return Err("native requires idx, name, param_slots, ret_slots".to_string());
        }
        
        let name = parse_string_literal(parts[2])?;
        let param_slots: u16 = parts[3].parse().map_err(|_| "invalid param_slots")?;
        let ret_slots: u16 = parts[4].parse().map_err(|_| "invalid ret_slots")?;
        
        self.module.add_native(&name, param_slots, ret_slots);
        Ok(())
    }
    
    fn parse_func(&mut self, parts: &[&str]) -> Result<(), String> {
        // func <name>
        if parts.len() < 2 {
            return Err("func requires name".to_string());
        }
        
        let name = parts[1].to_string();
        let mut func = FunctionDef::new(&name);
        
        // Parse function body until 'end'
        while self.pos < self.lines.len() {
            let line = self.lines[self.pos].trim();
            self.pos += 1;
            
            if line.is_empty() || line.starts_with('#') || line.starts_with("//") {
                continue;
            }
            
            if line == "end" {
                break;
            }
            
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.is_empty() {
                continue;
            }
            
            match parts[0] {
                "params" => {
                    if parts.len() >= 3 {
                        func.param_count = parts[1].parse().map_err(|_| "invalid param_count")?;
                        func.param_slots = parts[2].parse().map_err(|_| "invalid param_slots")?;
                    }
                }
                "locals" => {
                    if parts.len() >= 2 {
                        func.local_slots = parts[1].parse().map_err(|_| "invalid local_slots")?;
                    }
                }
                "returns" => {
                    if parts.len() >= 2 {
                        func.ret_slots = parts[1].parse().map_err(|_| "invalid ret_slots")?;
                    }
                }
                _ => {
                    // Parse instruction: <idx>: <opcode> <args>
                    let instr = parse_instruction(line)?;
                    func.code.push(instr);
                }
            }
        }
        
        let idx = self.module.functions.len() as u32;
        self.func_names.insert(name, idx);
        self.module.functions.push(func);
        Ok(())
    }
}

fn parse_string_literal(s: &str) -> Result<String, String> {
    let s = s.trim();
    if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
        let inner = &s[1..s.len()-1];
        // Simple unescape (handle \n, \t, \\, \")
        let mut result = String::new();
        let mut chars = inner.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('n') => result.push('\n'),
                    Some('t') => result.push('\t'),
                    Some('\\') => result.push('\\'),
                    Some('"') => result.push('"'),
                    Some(c) => {
                        result.push('\\');
                        result.push(c);
                    }
                    None => result.push('\\'),
                }
            } else {
                result.push(c);
            }
        }
        Ok(result)
    } else {
        Err(format!("invalid string literal: {}", s))
    }
}

fn parse_instruction(line: &str) -> Result<Instruction, String> {
    // Format: <idx>: <opcode> <args>
    let line = line.trim();
    let colon_pos = line.find(':').ok_or("instruction must have :")?;
    let rest = line[colon_pos+1..].trim();
    
    // Split into opcode and args
    let mut parts: Vec<&str> = rest.split(|c| c == ' ' || c == ',').filter(|s| !s.is_empty()).collect();
    if parts.is_empty() {
        return Err("empty instruction".to_string());
    }
    
    let opcode_str = parts.remove(0);
    let args: Vec<i64> = parts.iter().map(|s| parse_arg(s)).collect::<Result<Vec<_>, _>>()?;
    
    // Match opcode and construct instruction
    let (op, a, b, c, flags) = match opcode_str {
        "Nop" => (Opcode::Nop, 0, 0, 0, 0),
        "LoadNil" => (Opcode::LoadNil, get_arg(&args, 0)?, 0, 0, 0),
        "LoadTrue" => (Opcode::LoadTrue, get_arg(&args, 0)?, 0, 0, 0),
        "LoadFalse" => (Opcode::LoadFalse, get_arg(&args, 0)?, 0, 0, 0),
        "LoadInt" => {
            let a = get_arg(&args, 0)?;
            let imm = get_arg(&args, 1)? as i32;
            let b = (imm as u32) as u16;
            let c = ((imm as u32) >> 16) as u16;
            (Opcode::LoadInt, a, b as i64, c as i64, 0)
        }
        "LoadConst" => (Opcode::LoadConst, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        "Mov" => (Opcode::Mov, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        "MovN" => (Opcode::MovN, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        
        "AddI64" => (Opcode::AddI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "SubI64" => (Opcode::SubI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "MulI64" => (Opcode::MulI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "DivI64" => (Opcode::DivI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "ModI64" => (Opcode::ModI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "NegI64" => (Opcode::NegI64, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        
        "AddF64" => (Opcode::AddF64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "SubF64" => (Opcode::SubF64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "MulF64" => (Opcode::MulF64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "DivF64" => (Opcode::DivF64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "NegF64" => (Opcode::NegF64, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        
        "EqI64" => (Opcode::EqI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "NeI64" => (Opcode::NeI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "LtI64" => (Opcode::LtI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "LeI64" => (Opcode::LeI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "GtI64" => (Opcode::GtI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "GeI64" => (Opcode::GeI64, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        
        "EqRef" => (Opcode::EqRef, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "NeRef" => (Opcode::NeRef, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "IsNil" => (Opcode::IsNil, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        
        "Not" => (Opcode::Not, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        
        "Band" => (Opcode::Band, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "Bor" => (Opcode::Bor, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "Bxor" => (Opcode::Bxor, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "Bnot" => (Opcode::Bnot, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        "Shl" => (Opcode::Shl, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "Shr" => (Opcode::Shr, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "Ushr" => (Opcode::Ushr, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        
        "Jump" => {
            let imm = get_arg(&args, 0)? as i32;
            let b = (imm as u32) as u16;
            let c = ((imm as u32) >> 16) as u16;
            (Opcode::Jump, 0, b as i64, c as i64, 0)
        }
        "JumpIf" => {
            let a = get_arg(&args, 0)?;
            let imm = get_arg(&args, 1)? as i32;
            let b = (imm as u32) as u16;
            let c = ((imm as u32) >> 16) as u16;
            (Opcode::JumpIf, a, b as i64, c as i64, 0)
        }
        "JumpIfNot" => {
            let a = get_arg(&args, 0)?;
            let imm = get_arg(&args, 1)? as i32;
            let b = (imm as u32) as u16;
            let c = ((imm as u32) >> 16) as u16;
            (Opcode::JumpIfNot, a, b as i64, c as i64, 0)
        }
        
        "Call" => (Opcode::Call, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, get_arg(&args, 3)? as i64),
        "Return" => (Opcode::Return, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        "CallNative" => (Opcode::CallNative, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, get_arg(&args, 3)? as i64),
        
        "Alloc" => (Opcode::Alloc, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "GetField" => (Opcode::GetField, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "SetField" => (Opcode::SetField, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        
        "ArrayNew" => (Opcode::ArrayNew, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "ArrayGet" => (Opcode::ArrayGet, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "ArraySet" => (Opcode::ArraySet, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "ArrayLen" => (Opcode::ArrayLen, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        
        "MapNew" => (Opcode::MapNew, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "MapGet" => (Opcode::MapGet, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "MapSet" => (Opcode::MapSet, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "MapLen" => (Opcode::MapLen, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        
        "ChanNew" => (Opcode::ChanNew, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "ChanSend" => (Opcode::ChanSend, get_arg(&args, 0)?, get_arg(&args, 1)?, 0, 0),
        "ChanRecv" => (Opcode::ChanRecv, get_arg(&args, 0)?, get_arg(&args, 1)?, get_arg(&args, 2)?, 0),
        "ChanClose" => (Opcode::ChanClose, get_arg(&args, 0)?, 0, 0, 0),
        
        "DebugPrint" => (Opcode::DebugPrint, get_arg(&args, 0)?, 0, 0, 0),
        "Yield" => (Opcode::Yield, 0, 0, 0, 0),
        
        _ => return Err(format!("unknown opcode: {}", opcode_str)),
    };
    
    Ok(Instruction::with_flags(op, flags as u8, a as u16, b as u16, c as u16))
}

fn parse_arg(s: &str) -> Result<i64, String> {
    let s = s.trim();
    if s.starts_with('r') {
        // Register: r0, r1, ...
        s[1..].parse().map_err(|_| format!("invalid register: {}", s))
    } else if s.starts_with('$') {
        // Constant index: $0, $1, ...
        s[1..].parse().map_err(|_| format!("invalid const index: {}", s))
    } else {
        // Plain number
        s.parse().map_err(|_| format!("invalid number: {}", s))
    }
}

fn get_arg(args: &[i64], idx: usize) -> Result<i64, String> {
    args.get(idx).copied().ok_or_else(|| format!("missing argument {}", idx))
}
