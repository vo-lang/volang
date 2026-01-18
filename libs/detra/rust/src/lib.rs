//! Detra language engine - compile, execute, and evaluate Detra programs.

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod executor;

use std::collections::HashMap;
use std::sync::Mutex;

use vo_ext::prelude::*;
use vo_runtime::gc::GcRef;

use crate::ast::Program;
use crate::executor::{Executor, State, ActionCall, RuntimeNode};

static PROGRAMS: Mutex<Vec<Program>> = Mutex::new(Vec::new());
static STATES: Mutex<Vec<State>> = Mutex::new(Vec::new());
static CURRENT_TREE: Mutex<Option<RuntimeNode>> = Mutex::new(None);
static CURRENT_COMMANDS: Mutex<Vec<executor::CommandCall>> = Mutex::new(Vec::new());

#[vo_extern_ctx("detra", "Compile")]
fn detra_compile(ctx: &mut ExternCallContext) -> ExternResult {
    let source = ctx.arg_str(slots::ARG_SOURCE);

    let mut lexer = lexer::Lexer::new(source);
    let tokens = match lexer.tokenize() {
        Ok(t) => t,
        Err(e) => return ExternResult::Panic(format!("Lexer error: {}", e)),
    };

    let mut parser = parser::Parser::new(tokens);
    let program = match parser.parse() {
        Ok(p) => p,
        Err(e) => return ExternResult::Panic(format!("Parser error: {}", e)),
    };

    let mut programs = PROGRAMS.lock().unwrap();
    let id = programs.len();
    programs.push(program);

    // Return as AnySlot (Program = any)
    ctx.ret_any(slots::RET_0, AnySlot::from_i64(id as i64));
    ExternResult::Ok
}

#[vo_extern_ctx("detra", "InitState")]
fn detra_init_state(ctx: &mut ExternCallContext) -> ExternResult {
    let program_id = ctx.arg_any_as_i64(slots::ARG_PROGRAM) as usize;

    let programs = PROGRAMS.lock().unwrap();
    let program = match programs.get(program_id) {
        Some(p) => p,
        None => return ExternResult::Panic(format!("Invalid program id: {}", program_id)),
    };

    let state = Executor::init_state(program);

    let mut states = STATES.lock().unwrap();
    let state_id = states.len();
    states.push(state);

    // Return as AnySlot (State = any)
    ctx.ret_any(slots::RET_0, AnySlot::from_i64(state_id as i64));
    ExternResult::Ok
}

#[vo_extern_ctx("detra", "Execute")]
fn detra_execute(ctx: &mut ExternCallContext) -> ExternResult {
    let program_id = ctx.arg_any_as_i64(slots::ARG_PROGRAM) as usize;
    let state_id = ctx.arg_any_as_i64(slots::ARG_STATE) as usize;
    let external_ref = ctx.arg_ref(slots::ARG_EXTERNAL);
    let action_name_ref = ctx.arg_ref(slots::ARG_ACTION_NAME);
    let action_args_ref = ctx.arg_ref(slots::ARG_ACTION_ARGS);


    let programs = PROGRAMS.lock().unwrap();
    let program = match programs.get(program_id) {
        Some(p) => p.clone(),
        None => return ExternResult::Panic(format!("Invalid program id: {}", program_id)),
    };
    drop(programs);

    let state = {
        let states = STATES.lock().unwrap();
        match states.get(state_id) {
            Some(s) => s.clone(),
            None => return ExternResult::Panic(format!("Invalid state id: {}", state_id)),
        }
    };

    let action = if action_name_ref.is_null() {
        None
    } else {
        let action_name = vo_runtime::objects::string::as_str(action_name_ref);
        if action_name.is_empty() {
            None
        } else {
            // Parse actionArgs map[string]string using VoMap cursor
            let mut args = HashMap::new();
            if !action_args_ref.is_null() {
                let map = VoMap::<String, String>::from_ref(action_args_ref);
                let mut cursor = map.cursor();
                while let Some((key, val)) = cursor.next() {
                    args.insert(key, value::Value::String(val));
                }
            }
            Some(ActionCall {
                name: action_name.to_string(),
                args,
            })
        }
    };

    // Parse external map[string]Value using VoMap cursor
    let mut external = HashMap::new();
    if !external_ref.is_null() {
        let map = VoMap::<String, String>::from_ref(external_ref);
        let mut cursor = map.cursor();
        while let Some((key, val)) = cursor.next() {
            external.insert(key, value::Value::String(val));
        }
    }

    let result = Executor::execute(&program, state, external, action);

    {
        let mut states = STATES.lock().unwrap();
        if state_id < states.len() {
            states[state_id] = result.state;
        }
    }

    // Set current tree and commands (renderer uses CURRENT_TREE directly)
    if result.error.is_none() {
        {
            let mut current = CURRENT_TREE.lock().unwrap();
            *current = Some(result.tree);
        }
        {
            let mut cmds = CURRENT_COMMANDS.lock().unwrap();
            *cmds = result.commands;
        }
    }

    // Return values: (State, string, string)
    ctx.ret_any(slots::RET_0, AnySlot::from_i64(state_id as i64));

    if let Some(ref err) = result.error {
        ctx.ret_str(slots::RET_1, &err.message);
        ctx.ret_str(slots::RET_2, &err.kind);
    } else {
        ctx.ret_str(slots::RET_1, "");
        ctx.ret_str(slots::RET_2, "");
    }

    ExternResult::Ok
}

#[vo_extern_ctx("detra", "CommandCount")]
fn detra_command_count(ctx: &mut ExternCallContext) -> ExternResult {
    let cmds = CURRENT_COMMANDS.lock().unwrap();
    ctx.ret_i64(slots::RET_0, cmds.len() as i64);
    ExternResult::Ok
}

#[vo_extern_ctx("detra", "CommandName")]
fn detra_command_name(ctx: &mut ExternCallContext) -> ExternResult {
    let index = ctx.arg_i64(slots::ARG_INDEX) as usize;
    let cmds = CURRENT_COMMANDS.lock().unwrap();
    if index < cmds.len() {
        ctx.ret_str(slots::RET_0, &cmds[index].name);
    } else {
        ctx.ret_str(slots::RET_0, "");
    }
    ExternResult::Ok
}

#[vo_extern_ctx("detra", "SetState")]
fn detra_set_state(ctx: &mut ExternCallContext) -> ExternResult {
    let state_id = ctx.arg_any_as_i64(slots::ARG_STATE) as usize;
    let key = ctx.arg_str(slots::ARG_KEY).to_string();
    let val = ctx.arg_str(slots::ARG_VALUE).to_string();
    
    let mut states = STATES.lock().unwrap();
    if let Some(state) = states.get_mut(state_id) {
        state.fields.insert(key, value::Value::String(val));
    }
    
    ExternResult::Ok
}

#[vo_extern_ctx("detra", "CommandArg")]
fn detra_command_arg(ctx: &mut ExternCallContext) -> ExternResult {
    let index = ctx.arg_i64(slots::ARG_INDEX) as usize;
    let key = ctx.arg_str(slots::ARG_KEY);
    
    let cmds = CURRENT_COMMANDS.lock().unwrap();
    if index < cmds.len() {
        if let Some(v) = cmds[index].args.get(key) {
            let val_str = match v {
                value::Value::String(s) => s.clone(),
                value::Value::Int(n) => n.to_string(),
                value::Value::Float(f) => f.to_string(),
                value::Value::Bool(b) => b.to_string(),
                _ => String::new(),
            };
            ctx.ret_str(slots::RET_0, &val_str);
        } else {
            ctx.ret_str(slots::RET_0, "");
        }
    } else {
        ctx.ret_str(slots::RET_0, "");
    }
    ExternResult::Ok
}

#[vo_extern_ctx("detra", "DebugPrintTree")]
fn detra_debug_print_tree(_ctx: &mut ExternCallContext) -> ExternResult {
    let current = CURRENT_TREE.lock().unwrap();
    if let Some(tree) = current.as_ref() {
        print_node(tree, 0);
    } else {
        println!("No current tree");
    }
    
    ExternResult::Ok
}

fn print_node(node: &RuntimeNode, indent: usize) {
    let prefix = "  ".repeat(indent);
    
    // Print node kind
    print!("{}{}", prefix, node.kind);
    
    // Print key props inline
    let mut inline_props = Vec::new();
    for (key, val) in &node.props {
        let val_str = match val {
            value::Value::String(s) => {
                if s.len() > 30 {
                    format!("\"{}...\"", &s[..27])
                } else {
                    format!("\"{}\"", s)
                }
            }
            value::Value::Int(n) => n.to_string(),
            value::Value::Float(f) => format!("{:.1}", f),
            value::Value::Bool(b) => b.to_string(),
            _ => "...".to_string(),
        };
        inline_props.push(format!("{}={}", key, val_str));
    }
    
    if !inline_props.is_empty() {
        print!("({})", inline_props.join(", "));
    }
    
    // Print events
    if !node.events.is_empty() {
        let events: Vec<_> = node.events.keys().collect();
        print!(" [{}]", events.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "));
    }
    
    println!();
    
    // Print children
    for child in &node.children {
        print_node(child, indent + 1);
    }
}

pub fn link_detra_externs() {
    // The vo_extern_ctx macros auto-generate the linkme statics,
    // but we keep this function for explicit linking if needed.
}

// Storage for RuntimeNode returned to renderer
static CURRENT_TREE_FOR_RENDERER: Mutex<Option<Box<detra_renderable::RuntimeNode>>> = Mutex::new(None);

/// Get current RuntimeNode for renderer.
/// Returns a pointer to detra_renderable::RuntimeNode.
#[no_mangle]
pub extern "C" fn detra_get_current_tree() -> *const detra_renderable::RuntimeNode {
    let current = CURRENT_TREE.lock().unwrap();
    let mut renderer_copy = CURRENT_TREE_FOR_RENDERER.lock().unwrap();
    
    if let Some(tree) = current.as_ref() {
        let renderable = convert_to_renderable(tree);
        let boxed = Box::new(renderable);
        let ptr = &*boxed as *const detra_renderable::RuntimeNode;
        *renderer_copy = Some(boxed);
        ptr
    } else {
        *renderer_copy = None;
        std::ptr::null()
    }
}

fn convert_to_renderable(node: &RuntimeNode) -> detra_renderable::RuntimeNode {
    detra_renderable::RuntimeNode {
        kind: node.kind.clone(),
        key: node.key.as_ref().map(|v| convert_value_to_renderable(v)),
        props: node.props.iter().map(|(k, v)| (k.clone(), convert_value_to_renderable(v))).collect(),
        events: node.events.iter().map(|(k, v)| (k.clone(), detra_renderable::ActionCall {
            name: v.name.clone(),
            args: v.args.iter().map(|(k, v)| (k.clone(), convert_value_to_renderable(v))).collect(),
        })).collect(),
        children: node.children.iter().map(convert_to_renderable).collect(),
    }
}

fn convert_value_to_renderable(v: &value::Value) -> detra_renderable::Value {
    match v {
        value::Value::Null => detra_renderable::Value::Null,
        value::Value::Bool(b) => detra_renderable::Value::Bool(*b),
        value::Value::Int(n) => detra_renderable::Value::Int(*n),
        value::Value::Float(f) => detra_renderable::Value::Float(*f),
        value::Value::String(s) => detra_renderable::Value::String(s.clone()),
        value::Value::Array(a) => detra_renderable::Value::Array(a.iter().map(convert_value_to_renderable).collect()),
        value::Value::Map(m) => detra_renderable::Value::Map(m.iter().map(|(k, v)| (k.clone(), convert_value_to_renderable(v))).collect()),
        value::Value::Struct(name, fields) => detra_renderable::Value::Struct(name.clone(), fields.iter().map(|(k, v)| (k.clone(), convert_value_to_renderable(v))).collect()),
    }
}

vo_ext::export_extensions!();
