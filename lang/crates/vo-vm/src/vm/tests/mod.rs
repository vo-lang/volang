use super::*;
use crate::fiber::{
    DeferArgLayout, DeferEntry, Fiber, PanicState, QueueWaitState, ReturnValues, SelectState,
    UnwindingMode, UnwindingState,
};
use crate::test_support::queue as test_queue;
#[cfg(feature = "std")]
use std::sync::atomic::AtomicBool;
#[cfg(feature = "std")]
use std::sync::Arc;
use vo_runtime::bytecode::{
    Constant, ExternDef, FunctionDef, GlobalDef, InterfaceMeta, JitInstructionMetadata, MethodInfo,
    NamedTypeMeta, ParamShape, ReturnShape, StructMeta,
};
use vo_runtime::ffi::{ExternCallContext, ExternResult};
use vo_runtime::island::{EndpointResponseKind, IslandCommand};
use vo_runtime::objects::interface;
use vo_runtime::objects::queue_state::SelectWaitKind;
#[cfg(all(feature = "jit", feature = "std"))]
use vo_runtime::objects::queue_state::{QueueKind, QueueWaiter};
#[cfg(all(feature = "jit", feature = "std"))]
use vo_runtime::ValueRttid;
use vo_runtime::{RuntimeType, SlotType, ValueKind, ValueMeta};

fn extern_def_for_test(
    name: &str,
    params: ParamShape,
    returns: ReturnShape,
    effects: vo_runtime::bytecode::ExternEffects,
) -> ExternDef {
    ExternDef {
        name: name.to_string(),
        params,
        returns,
        allowed_effects: effects,
        param_kinds: Vec::new(),
    }
}

fn gc_test_module() -> Module {
    gc_test_module_with_root_slots(1)
}

fn add_named_string_receiver_metadata(module: &mut Module, func_id: u32) {
    module
        .runtime_types
        .push(RuntimeType::Basic(ValueKind::String));
    module.runtime_types.push(RuntimeType::Named {
        id: 0,
        struct_meta_id: None,
    });
    let mut named_meta = NamedTypeMeta {
        name: "NamedString".to_string(),
        underlying_meta: ValueMeta::new(0, ValueKind::String),
        underlying_rttid: vo_runtime::ValueRttid::new(0, ValueKind::String),
        methods: Default::default(),
    };
    named_meta.methods.insert(
        "M".to_string(),
        MethodInfo {
            func_id,
            is_pointer_receiver: false,
            receiver_is_iface_boxed: false,
            signature_rttid: 0,
        },
    );
    module.named_type_metas.push(named_meta);
}

fn gc_test_module_with_root_slots(root_slots: u16) -> Module {
    let mut module = Module::new("gc-test".to_string());
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.struct_metas.push(StructMeta {
        slot_types: Vec::new(),
        fields: Vec::new(),
        field_index: Default::default(),
    });
    module.functions.push(FunctionDef {
        name: "root_frame".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: root_slots,
        gc_scan_slots: root_slots,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        jit_metadata: Vec::new(),
        code: Vec::new(),
        slot_types: vec![SlotType::GcRef; root_slots as usize],
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&vec![
                SlotType::GcRef;
                root_slots as usize
            ]),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    module
}

fn gc_test_module_with_global() -> Module {
    let mut module = gc_test_module();
    module.globals.push(GlobalDef {
        name: "root_global".to_string(),
        slots: 1,
        value_kind: ValueKind::Struct as u8,
        meta_id: 1,
        slot_types: vec![SlotType::GcRef],
    });
    module
}

fn alloc_gc_test_object(vm: &mut Vm) -> GcRef {
    vm.state.gc.alloc(ValueMeta::new(1, ValueKind::Struct), 0)
}

fn assert_gc_roots_survive(vm: &mut Vm, roots: &[GcRef]) {
    run_gc_until_pause(vm);
    for &root in roots {
        assert_eq!(vm.state.gc.canonicalize_ref(root), Some(root));
    }
}

#[cfg(feature = "std")]
fn apply_gc_env_pairs(vm: &mut Vm, pairs: &[(&str, &str)]) {
    vm.apply_gc_environment_from(|name| {
        pairs
            .iter()
            .find_map(|(key, value)| (*key == name).then(|| (*value).to_string()))
    });
}

fn malformed_single_instruction_module(
    name: &str,
    code: Vec<Instruction>,
    constants: Vec<Constant>,
) -> Module {
    let mut module = Module::new(name.to_string());
    module.constants = constants;
    module.functions.push(FunctionDef {
        name: "main".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 4,
        gc_scan_slots: 0,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        jit_metadata: vec![vo_runtime::bytecode::JitInstructionMetadata::None; code.len()],
        code,
        slot_types: vec![SlotType::Value; 4],
        borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(&[
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
            SlotType::Value,
        ]),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    module
}

fn assert_vm_load_rejects(module: Module, expected: &[&str]) {
    let mut vm = Vm::new();
    let err = vm
        .load(module)
        .expect_err("invalid module must be rejected during VM load");
    match err {
        VmError::Jit(msg) => {
            assert!(msg.contains("invalid module metadata"), "{msg}");
            for needle in expected {
                assert!(msg.contains(needle), "missing `{needle}` in `{msg}`");
            }
        }
        other => panic!("invalid module load should return Jit error, got {other:?}"),
    }
}

fn refresh_vm_test_function_metadata(func: &mut FunctionDef) {
    func.gc_scan_slots = FunctionDef::compute_gc_scan_slots(&func.slot_types);
    func.borrowed_scan_slots_prefix =
        FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
    func.has_defer = func
        .code
        .iter()
        .any(|inst| matches!(inst.opcode(), Opcode::DeferPush | Opcode::ErrDeferPush));
    let (has_calls, has_call_extern) = FunctionDef::compute_call_flags(&func.code);
    func.has_calls = has_calls;
    func.has_call_extern = has_call_extern;
}

fn finish_load_and_resolve_externs_for_test(
    vm: &mut Vm,
    module: Module,
    registrations: &[(
        u32,
        vo_runtime::ffi::ExternFn,
        vo_runtime::bytecode::ExternEffects,
    )],
) {
    vm.finish_load(module);
    let externs = vm.module.as_ref().expect("loaded module").externs.clone();
    for (id, func, effects) in registrations {
        let name = externs
            .get(*id as usize)
            .unwrap_or_else(|| panic!("test extern id {id} missing from loaded module"))
            .name
            .clone();
        vm.state
            .extern_registry
            .register_test_named_with_effects(*id, name, *func, *effects);
    }
    vm.state.resolved_externs = vm
        .state
        .extern_registry
        .resolve_module_externs(&externs)
        .expect("resolve test externs");
}

#[cfg(feature = "jit")]
fn assert_goisland_validators_before(name: &str, source: &str, terminal: &str) {
    assert!(
            goisland_validators_before(source, terminal),
            "{name} GoIsland path must use shared frame_call object-kind validators before publishing island work"
        );
}

#[cfg(feature = "jit")]
fn goisland_validators_before_jit_publication(source: &str) -> bool {
    goisland_validators_before(source, "commit_go_spawn(")
        && goisland_validators_before(source, "commit_go_island_commands(")
}

#[cfg(feature = "jit")]
fn goisland_validators_before(source: &str, terminal: &str) -> bool {
    let (compact, _offsets) = vo_source_contract::compact_rust_source_for_contract(source);
    let compact = compact_source_without_non_dominating_blocks(&compact);
    let Some(island_pos) = compact_pattern_position(&compact, "validate_island_handle(") else {
        return false;
    };
    let Some(closure_pos) = compact_pattern_position(&compact, "validate_closure_target(") else {
        return false;
    };
    let Some(terminal_pos) = compact_pattern_position(&compact, terminal) else {
        return false;
    };
    island_pos < terminal_pos && closure_pos < terminal_pos
}

fn compact_source_without_non_dominating_blocks(compact: &[u8]) -> Vec<u8> {
    let mut filtered = Vec::with_capacity(compact.len());
    let mut idx = 0usize;
    let mut brace_depth = 0usize;
    while idx < compact.len() {
        let skipped_open = if compact[idx..].starts_with(b"iffalse{") {
            Some(idx + "iffalse".len())
        } else if compact[idx..].starts_with(b"ifcfg!(any()){") {
            Some(idx + "ifcfg!(any())".len())
        } else if compact[idx..].starts_with(b"matchfalse{") {
            Some(idx + "matchfalse".len())
        } else if compact[idx..].starts_with(b"async{") {
            Some(idx + "async".len())
        } else if compact[idx..].starts_with(b"asyncmove{") {
            Some(idx + "asyncmove".len())
        } else if compact[idx..].starts_with(b"macro_rules!") {
            compact[idx..]
                .iter()
                .position(|byte| *byte == b'{')
                .map(|offset| idx + offset)
        } else if compact[idx..].starts_with(b"move|") || compact[idx..].starts_with(b"|") {
            compact_closure_body_open(compact, idx)
        } else if compact[idx..].starts_with(b"||{") {
            Some(idx + "||".len())
        } else if brace_depth > 0 && compact_keyword_at(compact, idx, b"fn") {
            nested_function_body_open(compact, idx + 2)
        } else {
            None
        };
        if let Some(open_idx) = skipped_open {
            if let Some(close_idx) = compact_delimiter_close(compact, open_idx) {
                idx = close_idx + 1;
                continue;
            }
        }
        match compact[idx] {
            b'{' => brace_depth += 1,
            b'}' => brace_depth = brace_depth.saturating_sub(1),
            _ => {}
        }
        filtered.push(compact[idx]);
        idx += 1;
    }
    filtered
}

fn compact_closure_body_open(compact: &[u8], idx: usize) -> Option<usize> {
    let mut cursor = idx;
    if compact[cursor..].starts_with(b"move") {
        cursor += "move".len();
    }
    if compact.get(cursor) != Some(&b'|') {
        return None;
    }
    cursor += 1;
    while cursor < compact.len() && compact[cursor] != b'|' {
        cursor += 1;
    }
    if compact.get(cursor) != Some(&b'|') {
        return None;
    }
    cursor += 1;
    while cursor < compact.len() {
        match compact[cursor] {
            b'(' | b'[' => {
                cursor = compact_delimiter_close(compact, cursor)? + 1;
            }
            b'{' => return Some(cursor),
            b';' => return None,
            _ => cursor += 1,
        }
    }
    None
}

fn compact_keyword_at(compact: &[u8], idx: usize, keyword: &[u8]) -> bool {
    compact.get(idx..idx + keyword.len()) == Some(keyword)
        && (idx == 0 || !rust_ident_continue(compact[idx - 1]))
        && compact
            .get(idx + keyword.len())
            .is_some_and(|byte| rust_ident_start(*byte))
}

fn nested_function_body_open(compact: &[u8], mut idx: usize) -> Option<usize> {
    while idx < compact.len() {
        match compact[idx] {
            b'(' | b'[' => {
                idx = compact_delimiter_close(compact, idx)? + 1;
                continue;
            }
            b'{' => return Some(idx),
            b';' => return None,
            _ => idx += 1,
        }
    }
    None
}

fn rust_ident_start(byte: u8) -> bool {
    byte == b'_' || byte.is_ascii_alphabetic()
}

fn rust_ident_continue(byte: u8) -> bool {
    rust_ident_start(byte) || byte.is_ascii_digit()
}

fn compact_delimiter_close(compact: &[u8], open_idx: usize) -> Option<usize> {
    let (open, close) = match compact.get(open_idx).copied()? {
        b'(' => (b'(', b')'),
        b'[' => (b'[', b']'),
        b'{' => (b'{', b'}'),
        _ => return None,
    };
    let mut depth = 0usize;
    for (idx, byte) in compact.iter().copied().enumerate().skip(open_idx) {
        if byte == open {
            depth += 1;
        } else if byte == close {
            depth = depth.checked_sub(1)?;
            if depth == 0 {
                return Some(idx);
            }
        }
    }
    None
}

fn compact_pattern_position(compact: &[u8], pattern: &str) -> Option<usize> {
    let pattern = pattern.as_bytes();
    if pattern.is_empty() || pattern.len() > compact.len() {
        return None;
    }
    (0..=compact.len() - pattern.len()).find(|start| compact[*start..].starts_with(pattern))
}

fn compact_pattern_positions(compact: &[u8], pattern: &str) -> Vec<usize> {
    let pattern = pattern.as_bytes();
    if pattern.is_empty() || pattern.len() > compact.len() {
        return Vec::new();
    }
    (0..=compact.len() - pattern.len())
        .filter(|start| compact[*start..].starts_with(pattern))
        .collect()
}

fn compact_pattern_position_at_max_brace_depth(
    compact: &[u8],
    pattern: &str,
    max_depth: usize,
) -> Option<usize> {
    compact_pattern_positions_at_max_brace_depth(compact, pattern, max_depth)
        .into_iter()
        .next()
}

fn compact_pattern_positions_at_max_brace_depth(
    compact: &[u8],
    pattern: &str,
    max_depth: usize,
) -> Vec<usize> {
    let pattern = pattern.as_bytes();
    if pattern.is_empty() || pattern.len() > compact.len() {
        return Vec::new();
    }
    let mut positions = Vec::new();
    let mut depth = 0usize;
    for idx in 0..compact.len() {
        if compact[idx..].starts_with(pattern) && depth <= max_depth {
            positions.push(idx);
        }
        match compact[idx] {
            b'{' => depth += 1,
            b'}' => depth = depth.saturating_sub(1),
            _ => {}
        }
    }
    positions
}

fn compact_source_bytes(source: &str) -> Vec<u8> {
    vo_source_contract::compact_rust_source_for_contract(source).0
}

fn compact_contains(compact: &[u8], pattern: &str) -> bool {
    compact_pattern_position(compact, pattern).is_some()
}

fn compact_region_between_compact(
    compact: &[u8],
    marker: &str,
    terminator: &str,
) -> Option<Vec<u8>> {
    let marker_pos = compact_pattern_position(compact, marker)?;
    let start = marker_pos + marker.len();
    let end = compact_pattern_position(&compact[start..], terminator)
        .map(|offset| start + offset)
        .unwrap_or(compact.len());
    Some(compact[start..end].to_vec())
}

fn compact_region_between(source: &str, marker: &str, terminator: &str) -> Option<Vec<u8>> {
    let compact = compact_source_bytes(source);
    compact_region_between_compact(&compact, marker, terminator)
}

fn source_slice_between<'a>(source: &'a str, start: &str, end: &str) -> Option<&'a str> {
    let tail = source.split(start).nth(1)?;
    Some(tail.split(end).next().unwrap_or(tail))
}

fn run_gc_until_pause(vm: &mut Vm) {
    for _ in 0..10_000 {
        if !vm.state.gc.should_step() && vm.state.gc.state() == vo_runtime::gc::GcState::Pause {
            return;
        }
        vm.gc_step_after_fiber(None);
    }
    panic!(
        "GC did not reach pause state; state={:?} root_scan_pending={}",
        vm.state.gc.state(),
        vm.state.gc_root_scan.is_some(),
    );
}

fn run_until_atomic_root_scan_pending(vm: &mut Vm) {
    for _ in 0..10_000 {
        vm.gc_step_after_fiber(None);
        if vm.state.gc.state() == vo_runtime::gc::GcState::Atomic && vm.state.gc_root_scan.is_some()
        {
            return;
        }
    }
    panic!(
        "GC did not enter pending atomic root scan; state={:?} root_scan_pending={}",
        vm.state.gc.state(),
        vm.state.gc_root_scan.is_some(),
    );
}

fn queue_action_macro_source_062() -> &'static str {
    source_slice_between(
        include_str!("../mod.rs"),
        "macro_rules! handle_queue_action",
        "while fiber.execution_budget > 0",
    )
    .expect("handle_queue_action macro body")
}

mod extern_replay;
mod gc_roots;
mod go_island;
mod load_validation;
mod pending_transitions;
mod queue_boundary;
mod runtime_wake;
mod scheduler_and_frame;
mod spawn_and_host;
#[cfg(feature = "jit")]
mod strict_jit;
