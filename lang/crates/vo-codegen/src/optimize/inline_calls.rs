//! Compose bounded, total scalar bodies before portable callsite IDs freeze.
//! An expanded body cannot allocate, panic, suspend, access external state or
//! observe its own logical frame. Effectful and recursive calls remain intact.

use vo_common_core::debug_info::{
    DebugInfo, InlineFunctionSources, InlineSourceEntry, InlineSourceFrame,
    MAX_INLINE_SOURCE_RECORDS,
};
use vo_common_core::execution_effects::{opcode_effect_contract, EffectContract};
use vo_common_core::instruction::{Instruction, Opcode};
use vo_common_core::instruction_registers::try_map_instruction_registers;
use vo_common_core::{Constant, FunctionDef, InstructionMetadata, Module, SlotType};

mod sources;
use sources::Sources;

use super::pc_map::PcMap;
use super::{pure_destination, read_slots, validate_operand_ranges, write_slots};

const MAX_FUNCTIONS: usize = 4_096;
const MAX_DEPTH: usize = 8;
const MAX_BODY: usize = 64;
const MAX_TEMPLATE_SLOTS: u16 = 128;
const MAX_RETAINED_BODY: usize = 65_536;
const MAX_CALLER_GROWTH: usize = 256;
const MAX_MODULE_GROWTH: usize = 16_384;

struct Template {
    code: Vec<Instruction>,
    origins: Vec<u32>,
    slots: u16,
    params: u16,
    returns: u16,
    depth: usize,
}

/// Return changed function IDs so ordinary cleanup runs only where needed.
pub(super) fn compose(module: &mut Module) -> Result<Vec<usize>, String> {
    if module.functions.len() > MAX_FUNCTIONS {
        return Ok(Vec::new());
    }
    let mut sources = Sources::new(&module.debug_info.inline_sources)?;
    let mut templates: Vec<Option<Template>> = (0..module.functions.len()).map(|_| None).collect();
    let mut eligible = Vec::new();
    for (id, function) in module.functions.iter().enumerate() {
        if shape(module, function)? {
            eligible.push(id);
        }
    }
    let mut retained = 0;
    // Publish each complete wave together: declaration order cannot change
    // the admitted depth, and recursive components never gain a leaf proof.
    for _ in 0..MAX_DEPTH {
        let mut ready = Vec::new();
        for &id in &eligible {
            if templates[id].is_some() {
                continue;
            }
            let checkpoint = sources.checkpoint();
            if let Some(body) = template(
                id as u32,
                &module.functions[id],
                &module.debug_info,
                &templates,
                &mut sources,
            )? {
                if body.code.len() <= MAX_RETAINED_BODY - retained {
                    retained += body.code.len();
                    ready.push((id, body));
                    continue;
                }
            }
            sources.rollback(checkpoint);
        }
        if ready.is_empty() {
            break;
        }
        for (id, body) in ready {
            templates[id] = Some(body);
        }
    }
    let mut changed = Vec::new();
    let mut remaining = MAX_MODULE_GROWTH;
    let mut remaining_sources = MAX_INLINE_SOURCE_RECORDS
        .checked_sub(
            module
                .debug_info
                .inline_sources
                .functions
                .iter()
                .map(|f| f.entries.len())
                .sum(),
        )
        .ok_or("inline source entry budget exceeded")?;
    for id in 0..module.functions.len() {
        let checkpoint = sources.checkpoint();
        if rewrite(
            module,
            id,
            &templates,
            &mut remaining,
            &mut remaining_sources,
            &mut sources,
        )? {
            changed.push(id);
        } else {
            sources.rollback(checkpoint);
        }
    }
    sources.publish(&mut module.debug_info.inline_sources);
    Ok(changed)
}

pub(super) fn compact_sources(module: &mut Module) {
    sources::compact(&mut module.debug_info.inline_sources);
}

fn shape(module: &Module, function: &FunctionDef) -> Result<bool, String> {
    if function.is_closure
        || function.has_defer
        || function.has_call_extern
        || function.heap_ret_gcref_count != 0
        || function.error_ret_slot >= 0
        || function.param_slots > 8
        || function.ret_slots > 8
        || function.local_slots > 64
        || function.code.is_empty()
        || function.code.len() > MAX_BODY
        || function.code.len() != function.instruction_metadata.len()
        || function.slot_types.iter().any(|&ty| ty != SlotType::Value)
        || function
            .ret_slot_types
            .iter()
            .any(|&ty| ty != SlotType::Value)
        || function
            .instruction_metadata
            .iter()
            .any(|m| !matches!(m, InstructionMetadata::None))
    {
        return Ok(false);
    }
    validate_operand_ranges(module, function)?;
    let mut initialized = vec![false; usize::from(function.local_slots)];
    initialized[..usize::from(function.param_slots)].fill(true);
    for (pc, &inst) in function.code.iter().enumerate() {
        if pc + 1 == function.code.len() {
            if inst.opcode() != Opcode::Return || inst.flags != 0 || inst.b != function.ret_slots {
                return Ok(false);
            }
        } else if inst.opcode() != Opcode::Call && pure_destination(inst).is_none() {
            return Ok(false);
        }
        if inst.opcode() != Opcode::Call
            && opcode_effect_contract(inst.opcode()) != EffectContract::PURE
        {
            return Ok(false);
        }
        if inst.opcode() == Opcode::LoadConst
            && !matches!(
                module.constants.get(inst.b as usize),
                Some(Constant::Nil | Constant::Bool(_) | Constant::Int(_))
            )
        {
            return Ok(false);
        }
        let mut complete = true;
        read_slots(module, function, &inst, pc, |start, count| {
            complete &= initialized[usize::from(start)..usize::from(start) + usize::from(count)]
                .iter()
                .all(|&value| value);
        })?;
        if !complete {
            return Ok(false);
        }
        write_slots(module, function, &inst, pc, |start, count| {
            initialized[usize::from(start)..usize::from(start) + usize::from(count)].fill(true);
        })?;
    }
    Ok(true)
}

fn copy(
    code: &mut Vec<Instruction>,
    origins: &mut Vec<u32>,
    origin: u32,
    destination: u16,
    source: u16,
    count: u16,
) {
    if count == 1 {
        code.push(Instruction::new(Opcode::Copy, destination, source, 0));
        origins.push(origin);
    } else if count != 0 {
        code.push(Instruction::new(Opcode::CopyN, destination, source, count));
        origins.push(origin);
    }
}

fn expanded_len(body: &Template) -> usize {
    body.code.len() - 1 + usize::from(body.params != 0) + usize::from(body.returns != 0)
}

fn emit(
    code: &mut Vec<Instruction>,
    origins: &mut Vec<u32>,
    body: &Template,
    window: u16,
    temporary: u16,
    caller: u32,
    sources: &mut Sources,
) -> Result<bool, String> {
    copy(code, origins, caller, temporary, window, body.params);
    for (pc, &inst) in body.code[..body.code.len() - 1].iter().enumerate() {
        let Some(origin) = sources.prepend(caller, body.origins[pc]) else {
            return Ok(false);
        };
        let mut relocated = inst;
        try_map_instruction_registers(&mut relocated, &mut InstructionMetadata::None, |slot| {
            temporary
                .checked_add(slot)
                .ok_or_else(|| "inline temporary exceeds u16".to_string())
        })?;
        code.push(relocated);
        origins.push(origin);
    }
    let ret = body.code.last().unwrap();
    let Some(origin) = sources.prepend(caller, *body.origins.last().unwrap()) else {
        return Ok(false);
    };
    copy(
        code,
        origins,
        origin,
        window
            .checked_add(body.params)
            .ok_or("inline result window exceeds u16")?,
        temporary
            .checked_add(ret.a)
            .ok_or("inline result source exceeds u16")?,
        body.returns,
    );
    Ok(true)
}

fn template(
    id: u32,
    function: &FunctionDef,
    debug: &DebugInfo,
    templates: &[Option<Template>],
    sources: &mut Sources,
) -> Result<Option<Template>, String> {
    // Dependency waves can revisit a caller before its children are ready.
    // Do not allocate instruction/source recipes for those incomplete waves.
    if function.code.iter().any(|instruction| {
        instruction.opcode() == Opcode::Call
            && !matches!(
                templates.get(instruction.static_call_func_id() as usize),
                Some(Some(_))
            )
    }) {
        return Ok(None);
    }
    let mut code = Vec::new();
    let mut origins = Vec::new();
    let mut slots = function.local_slots;
    let mut depth = 0;
    for (pc, &inst) in function.code.iter().enumerate() {
        let Some(origin) = sources.at(debug, id, pc as u32) else {
            return Ok(None);
        };
        if inst.opcode() == Opcode::Call {
            let Some(Some(body)) = templates.get(inst.static_call_func_id() as usize) else {
                return Ok(None);
            };
            depth = depth.max(body.depth + 1);
            let Some(required) = function.local_slots.checked_add(body.slots) else {
                return Ok(None);
            };
            slots = slots.max(required);
            if depth >= MAX_DEPTH
                || slots > MAX_TEMPLATE_SLOTS
                || code.len() + expanded_len(body) >= MAX_BODY
            {
                return Ok(None);
            }
            if !emit(
                &mut code,
                &mut origins,
                body,
                inst.b,
                function.local_slots,
                origin,
                sources,
            )? {
                return Ok(None);
            }
        } else {
            code.push(inst);
            origins.push(origin);
        }
        if code.len() > MAX_BODY {
            return Ok(None);
        }
    }
    Ok(Some(Template {
        code,
        origins,
        slots,
        params: function.param_slots,
        returns: function.ret_slots,
        depth,
    }))
}

fn rewrite(
    module: &mut Module,
    id: usize,
    templates: &[Option<Template>],
    remaining: &mut usize,
    remaining_sources: &mut usize,
    sources: &mut Sources,
) -> Result<bool, String> {
    let function = &module.functions[id];
    if function.code.len() > 64 * 1024 || function.has_defer {
        return Ok(false);
    }
    let mut widths = vec![1; function.code.len()];
    let mut selected = vec![false; function.code.len()];
    let mut growth = 0;
    let mut source_entries = 0;
    let mut slots = function.local_slots;
    for (pc, &inst) in function.code.iter().enumerate() {
        if inst.opcode() != Opcode::Call {
            continue;
        }
        let Some(Some(body)) = templates.get(inst.static_call_func_id() as usize) else {
            continue;
        };
        let Some(required) = function.local_slots.checked_add(body.slots) else {
            continue;
        };
        let width = expanded_len(body);
        let added = width.saturating_sub(1);
        if added > MAX_CALLER_GROWTH - growth
            || added > *remaining - growth
            || width > *remaining_sources - source_entries
        {
            continue;
        }
        slots = slots.max(required);
        growth += added;
        source_entries += width;
        widths[pc] = width;
        selected[pc] = true;
    }
    if !selected.iter().any(|&value| value) {
        return Ok(false);
    }
    let map = PcMap::from_lengths(&widths)?;
    let mut code = Vec::with_capacity(widths.iter().sum());
    let mut origins = Vec::with_capacity(code.capacity());
    let mut metadata = Vec::with_capacity(code.capacity());
    for (pc, (&inst, original)) in function
        .code
        .iter()
        .zip(&function.instruction_metadata)
        .enumerate()
    {
        if selected[pc] {
            let body = templates[inst.static_call_func_id() as usize]
                .as_ref()
                .unwrap();
            let Some(origin) = sources.at(&module.debug_info, id as u32, pc as u32) else {
                return Ok(false);
            };
            if !emit(
                &mut code,
                &mut origins,
                body,
                inst.b,
                function.local_slots,
                origin,
                sources,
            )? {
                return Ok(false);
            }
            metadata.resize(code.len(), InstructionMetadata::None);
        } else {
            let mut item = original.clone();
            // Expansion can exceed the compact ForLoop branch domain. Preserve
            // the complete original function when relocation cannot represent it.
            let Ok(relocated) = map.relocate(pc, inst, &mut item) else {
                return Ok(false);
            };
            code.push(relocated);
            origins.push(
                module
                    .debug_info
                    .inline_sources
                    .frame_at(id as u32, pc as u32)
                    .unwrap_or(InlineSourceFrame::NO_PARENT),
            );
            metadata.push(item);
        }
    }
    let mut debug = module.debug_info.funcs.get(id).cloned();
    if let Some(debug) = &mut debug {
        map.relocate_debug(debug)?;
    }
    let entries = origins
        .into_iter()
        .enumerate()
        .filter_map(|(pc, frame)| {
            (frame != InlineSourceFrame::NO_PARENT && sources.is_inline(frame)).then_some(
                InlineSourceEntry {
                    pc: pc as u32,
                    frame,
                },
            )
        })
        .collect();
    let inline_function = InlineFunctionSources {
        function_id: id as u32,
        entries,
    };
    let inline_functions = &mut module.debug_info.inline_sources.functions;
    match inline_functions.binary_search_by_key(&(id as u32), |function| function.function_id) {
        Ok(index) => inline_functions[index] = inline_function,
        Err(index) => inline_functions.insert(index, inline_function),
    }
    let function = &mut module.functions[id];
    function.code = code;
    function.instruction_metadata = metadata;
    function.local_slots = slots;
    function
        .slot_types
        .resize(usize::from(slots), SlotType::Value);
    (function.has_calls, function.has_call_extern) =
        FunctionDef::compute_call_flags(&function.code);
    if let Some(debug) = debug {
        module.debug_info.funcs[id] = debug;
    }
    *remaining -= growth;
    *remaining_sources -= source_entries;
    validate_operand_ranges(module, &module.functions[id])?;
    Ok(true)
}

#[cfg(test)]
mod tests;
