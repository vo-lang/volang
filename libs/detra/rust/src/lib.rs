//! Detra language engine - compile, execute, and evaluate Detra programs.

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod value;
pub mod executor;

use std::collections::HashMap;
use std::sync::Mutex;

use linkme::distributed_slice;
use vo_runtime::ffi::{ExternCallContext, ExternEntryWithContext, ExternResult, EXTERN_TABLE_WITH_CONTEXT};
use vo_runtime::gc::GcRef;
use vo_runtime::objects::string;

use crate::ast::Program;
use crate::executor::{Executor, State, ActionCall, RuntimeNode};

static PROGRAMS: Mutex<Vec<Program>> = Mutex::new(Vec::new());
static STATES: Mutex<Vec<State>> = Mutex::new(Vec::new());
static CURRENT_TREE: Mutex<Option<RuntimeNode>> = Mutex::new(None);
static CURRENT_COMMANDS: Mutex<Vec<executor::CommandCall>> = Mutex::new(Vec::new());

fn detra_compile(ctx: &mut ExternCallContext) -> ExternResult {
    let source_ref = ctx.arg_ref(0);
    let source = string::as_str(source_ref);

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

    ctx.ret_i64(0, id as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_COMPILE: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_Compile",
    func: detra_compile,
};

fn detra_init_state(ctx: &mut ExternCallContext) -> ExternResult {
    let program_id = ctx.arg_i64(0) as usize;

    let programs = PROGRAMS.lock().unwrap();
    let program = match programs.get(program_id) {
        Some(p) => p,
        None => return ExternResult::Panic(format!("Invalid program id: {}", program_id)),
    };

    let state = Executor::init_state(program);

    let mut states = STATES.lock().unwrap();
    let state_id = states.len();
    states.push(state);

    ctx.ret_i64(0, state_id as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_INIT_STATE: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_InitState",
    func: detra_init_state,
};

fn detra_execute(ctx: &mut ExternCallContext) -> ExternResult {
    // Program (any = 2 slots): slot 0-1, we use slot 0 as the int id
    // State (any = 2 slots): slot 2-3, we use slot 2 as the int id
    // External (map = 1 slot): slot 4
    // actionName (string = 1 slot): slot 5
    // actionArgs (map = 1 slot): slot 6
    let program_id = ctx.arg_i64(0) as usize;
    let state_id = ctx.arg_i64(2) as usize;
    let external_ref = ctx.arg_ref(4);
    let action_name_ref = ctx.arg_ref(5);
    let action_args_ref = ctx.arg_ref(6);

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
        let action_name = string::as_str(action_name_ref);
        if action_name.is_empty() {
            None
        } else {
            // Parse actionArgs map[string]string
            let mut args = HashMap::new();
            if !action_args_ref.is_null() {
                use vo_runtime::objects::map;
                let mut iter = map::iter_init(action_args_ref);
                while let Some((k, v)) = map::iter_next(&mut iter) {
                    // k and v are slices - for string keys/values, slot 0 is the GcRef (as u64)
                    let key_ref = k[0] as GcRef;
                    let val_ref = v[0] as GcRef;
                    let key = string::as_str(key_ref).to_string();
                    let val = string::as_str(val_ref).to_string();
                    args.insert(key, value::Value::String(val));
                }
            }
            Some(ActionCall {
                name: action_name.to_string(),
                args,
            })
        }
    };

    // Parse external map[string]Value
    let mut external = HashMap::new();
    if !external_ref.is_null() {
        use vo_runtime::objects::map;
        let mut iter = map::iter_init(external_ref);
        while let Some((k, v)) = map::iter_next(&mut iter) {
            let key_ref = k[0] as GcRef;
            let val_ref = v[0] as GcRef;
            let key = string::as_str(key_ref).to_string();
            // External values are strings (map[string]Value where Value is string)
            let val = string::as_str(val_ref).to_string();
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

    ctx.ret_i64(0, state_id as i64);

    if let Some(ref err) = result.error {
        ctx.ret_str(1, &err.message);
        ctx.ret_str(2, &err.kind);
    } else {
        ctx.ret_str(1, "");
        ctx.ret_str(2, "");
    }

    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_EXECUTE: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_Execute",
    func: detra_execute,
};

fn detra_command_count(ctx: &mut ExternCallContext) -> ExternResult {
    let cmds = CURRENT_COMMANDS.lock().unwrap();
    ctx.ret_i64(0, cmds.len() as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_COMMAND_COUNT: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_CommandCount",
    func: detra_command_count,
};

fn detra_command_name(ctx: &mut ExternCallContext) -> ExternResult {
    let index = ctx.arg_i64(0) as usize;
    let cmds = CURRENT_COMMANDS.lock().unwrap();
    if index < cmds.len() {
        ctx.ret_str(0, &cmds[index].name);
    } else {
        ctx.ret_str(0, "");
    }
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_COMMAND_NAME: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_CommandName",
    func: detra_command_name,
};

fn detra_set_state(ctx: &mut ExternCallContext) -> ExternResult {
    // State is `any` type = 2 slots (slot 0-1), key is slot 2, value is slot 3
    let state_id = ctx.arg_i64(0) as usize;
    let key_ref = ctx.arg_ref(2);  // slot 2, not 1
    let value_ref = ctx.arg_ref(3);  // slot 3, not 2
    
    let key = string::as_str(key_ref).to_string();
    let val = string::as_str(value_ref).to_string();
    
    let mut states = STATES.lock().unwrap();
    if let Some(state) = states.get_mut(state_id) {
        state.fields.insert(key, value::Value::String(val));
    }
    
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_SET_STATE: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_SetState",
    func: detra_set_state,
};

fn detra_command_arg(ctx: &mut ExternCallContext) -> ExternResult {
    let index = ctx.arg_i64(0) as usize;
    let key_ref = ctx.arg_ref(1);
    let key = string::as_str(key_ref);
    
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
            ctx.ret_str(0, &val_str);
        } else {
            ctx.ret_str(0, "");
        }
    } else {
        ctx.ret_str(0, "");
    }
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_COMMAND_ARG: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_CommandArg",
    func: detra_command_arg,
};

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

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_DEBUG_PRINT_TREE: ExternEntryWithContext = ExternEntryWithContext {
    name: "detra_DebugPrintTree",
    func: detra_debug_print_tree,
};

pub fn link_detra_externs() {
    let _ = &__VO_DETRA_COMPILE;
    let _ = &__VO_DETRA_INIT_STATE;
    let _ = &__VO_DETRA_EXECUTE;
    let _ = &__VO_DETRA_SET_STATE;
    let _ = &__VO_DETRA_DEBUG_PRINT_TREE;
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
