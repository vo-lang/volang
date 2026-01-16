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
static TREES: Mutex<Vec<RuntimeNode>> = Mutex::new(Vec::new());
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
    name: ".._libs_detra_Compile",
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
    name: ".._libs_detra_InitState",
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
    let _external_ref = ctx.arg_ref(4);
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

    let external = HashMap::new();

    let result = Executor::execute(&program, state, external, action);

    {
        let mut states = STATES.lock().unwrap();
        if state_id < states.len() {
            states[state_id] = result.state;
        }
    }

    let tree_id = {
        let mut trees = TREES.lock().unwrap();
        let id = trees.len();
        trees.push(result.tree.clone());
        id
    };

    // Only set current tree and commands if no error
    if result.error.is_none() {
        // Set current tree for C ABI access
        {
            let mut current = CURRENT_TREE.lock().unwrap();
            *current = Some(result.tree);
        }

        // Store commands for GetCommands
        {
            let mut cmds = CURRENT_COMMANDS.lock().unwrap();
            *cmds = result.commands;
        }
    }

    ctx.ret_i64(0, state_id as i64);
    ctx.ret_i64(1, tree_id as i64);

    if let Some(ref err) = result.error {
        ctx.ret_str(2, &err.message);
        ctx.ret_str(3, &err.kind);
    } else {
        ctx.ret_str(2, "");
        ctx.ret_str(3, "");
    }

    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_EXECUTE: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_detra_Execute",
    func: detra_execute,
};

fn detra_command_count(ctx: &mut ExternCallContext) -> ExternResult {
    let cmds = CURRENT_COMMANDS.lock().unwrap();
    ctx.ret_i64(0, cmds.len() as i64);
    ExternResult::Ok
}

#[distributed_slice(EXTERN_TABLE_WITH_CONTEXT)]
static __VO_DETRA_COMMAND_COUNT: ExternEntryWithContext = ExternEntryWithContext {
    name: ".._libs_detra_CommandCount",
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
    name: ".._libs_detra_CommandName",
    func: detra_command_name,
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
    name: ".._libs_detra_CommandArg",
    func: detra_command_arg,
};

pub fn get_tree(tree_id: usize) -> Option<RuntimeNode> {
    TREES.lock().unwrap().get(tree_id).cloned()
}

// C ABI exports for renderer to call via dlsym
// CURRENT_TREE_COPY holds a clone that's safe to access from renderer
static CURRENT_TREE_COPY: Mutex<Option<Box<RuntimeNode>>> = Mutex::new(None);

#[no_mangle]
pub extern "C" fn detra_prepare_tree(_tree_id: usize) -> *const RuntimeNode {
    // Clone current tree into CURRENT_TREE_COPY for safe access
    let current = CURRENT_TREE.lock().unwrap();
    let mut copy = CURRENT_TREE_COPY.lock().unwrap();
    
    if let Some(tree) = current.as_ref() {
        let boxed = Box::new(tree.clone());
        let ptr = &*boxed as *const RuntimeNode;
        *copy = Some(boxed);
        ptr
    } else {
        *copy = None;
        std::ptr::null()
    }
}

#[no_mangle]
pub extern "C" fn detra_get_tree_ptr(_tree_id: usize) -> *const RuntimeNode {
    // Return pointer to the prepared copy
    let copy = CURRENT_TREE_COPY.lock().unwrap();
    match copy.as_ref() {
        Some(boxed) => &**boxed as *const RuntimeNode,
        None => std::ptr::null(),
    }
}

pub fn link_detra_externs() {
    let _ = &__VO_DETRA_COMPILE;
    let _ = &__VO_DETRA_INIT_STATE;
    let _ = &__VO_DETRA_EXECUTE;
}

vo_ext::export_extensions!();
