//! Bounded cleanup of compiler-owned bytecode before call-site IDs freeze.
//!
//! Value numbering is local to basic blocks. The common register and frame
//! effects drive liveness; allocation, checks, effectful calls and root-bearing
//! stores remain observable. PC-bearing instructions and debug locations move through
//! a single relocation map. Backends keep their own optimizing representations.

mod frame;
mod inline_calls;
mod pc_map;

use std::collections::HashMap;
use vo_common_core::bytecode::{Constant, FunctionDef, Module};
use vo_common_core::instruction::{Instruction, Opcode, HINT_LOOP};
use vo_common_core::instruction_effects::{
    instruction_frame_memory_effect, visit_instruction_register_reads,
    visit_instruction_register_writes, FrameMemoryEffect,
};
use vo_common_core::{InstructionMetadata, SlotType};

use pc_map::{branch_target, set_imm32, PcMap};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Atom {
    Integer(u64),
    Float(u64),
    Definition { pc: usize, slot: u16 },
}

impl Atom {
    fn integer(self) -> Option<u64> {
        match self {
            Self::Integer(value) => Some(value),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Expression {
    opcode: u8,
    flags: u8,
    left: Atom,
    right: Option<Atom>,
}

#[derive(Default)]
struct Values {
    slots: HashMap<u16, Atom>,
    representatives: HashMap<Atom, u16>,
    expressions: HashMap<Expression, Atom>,
}

impl Values {
    fn get(&self, slot: u16) -> Atom {
        self.slots.get(&slot).copied().unwrap_or(Atom::Definition {
            pc: usize::MAX,
            slot,
        })
    }

    fn representative(&mut self, slot: u16, types: &[SlotType]) -> u16 {
        let value = self.get(slot);
        if let Some(&representative) = self.representatives.get(&value) {
            if self.get(representative) == value
                && types.get(representative as usize) == types.get(slot as usize)
            {
                return representative;
            }
        }
        self.representatives.insert(value, slot);
        slot
    }

    fn store(&mut self, slot: u16, value: Atom, types: &[SlotType]) {
        self.slots.insert(slot, value);
        self.representative(slot, types);
    }

    fn clear(&mut self) {
        self.slots.clear();
        self.representatives.clear();
        self.expressions.clear();
    }
}

fn scalar(slot: u16, function: &FunctionDef) -> bool {
    matches!(
        function.slot_types.get(slot as usize),
        Some(SlotType::Value | SlotType::Float)
    )
}

fn unary(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::NegI
            | Opcode::Not
            | Opcode::BoolNot
            | Opcode::Trunc
            | Opcode::NegF
            | Opcode::NegF32
            | Opcode::ConvI2F
            | Opcode::ConvF2I
            | Opcode::ConvF64F32
            | Opcode::ConvF32F64
    )
}

fn binary(opcode: Opcode) -> bool {
    matches!(
        opcode,
        Opcode::AddI
            | Opcode::SubI
            | Opcode::MulI
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::AndNot
            | Opcode::EqI
            | Opcode::NeI
            | Opcode::LtI
            | Opcode::LtU
            | Opcode::LeI
            | Opcode::LeU
            | Opcode::GtI
            | Opcode::GtU
            | Opcode::GeI
            | Opcode::GeU
            | Opcode::AddF
            | Opcode::AddF32
            | Opcode::SubF
            | Opcode::SubF32
            | Opcode::MulF
            | Opcode::MulF32
            | Opcode::DivF
            | Opcode::DivF32
            | Opcode::EqF
            | Opcode::EqF32
            | Opcode::NeF
            | Opcode::NeF32
            | Opcode::LtF
            | Opcode::LtF32
            | Opcode::LeF
            | Opcode::LeF32
            | Opcode::GtF
            | Opcode::GtF32
            | Opcode::GeF
            | Opcode::GeF32
    )
}

/// A closed set of total operations writing only their destination. Runtime
/// effects alone do not imply dead-store eligibility: GlobalSet and Return
/// also have a PURE execution contract but carry observable program effects.
fn pure_destination(instruction: Instruction) -> Option<(u16, u16)> {
    let opcode = instruction.opcode();
    if unary(opcode)
        || binary(opcode)
        || matches!(opcode, Opcode::Copy | Opcode::LoadInt | Opcode::LoadConst)
    {
        Some((instruction.a, 1))
    } else if opcode == Opcode::CopyN {
        Some((instruction.a, instruction.copy_n_count()))
    } else {
        None
    }
}

fn fold(expression: Expression) -> Option<u64> {
    let left = expression.left.integer()?;
    let right = expression.right.and_then(Atom::integer);
    Some(match Opcode::from_u8(expression.opcode) {
        Opcode::AddI => left.wrapping_add(right?),
        Opcode::SubI => left.wrapping_sub(right?),
        Opcode::MulI => left.wrapping_mul(right?),
        Opcode::And => left & right?,
        Opcode::Or => left | right?,
        Opcode::Xor => left ^ right?,
        Opcode::AndNot => left & !right?,
        Opcode::NegI => left.wrapping_neg(),
        Opcode::Not => !left,
        Opcode::BoolNot => u64::from(left == 0),
        Opcode::EqI => u64::from(left == right?),
        Opcode::NeI => u64::from(left != right?),
        Opcode::LtI => u64::from((left as i64) < right? as i64),
        Opcode::LeI => u64::from((left as i64) <= right? as i64),
        Opcode::GtI => u64::from((left as i64) > right? as i64),
        Opcode::GeI => u64::from((left as i64) >= right? as i64),
        Opcode::LtU => u64::from(left < right?),
        Opcode::LeU => u64::from(left <= right?),
        Opcode::GtU => u64::from(left > right?),
        Opcode::GeU => u64::from(left >= right?),
        Opcode::Trunc => match (expression.flags & 0x7f, expression.flags & 0x80 != 0) {
            (1, true) => left as i8 as i64 as u64,
            (2, true) => left as i16 as i64 as u64,
            (4, true) => left as i32 as i64 as u64,
            (1, false) => left as u8 as u64,
            (2, false) => left as u16 as u64,
            (4, false) => left as u32 as u64,
            _ => left,
        },
        _ => return None,
    })
}

fn successors(
    pc: usize,
    instruction: Instruction,
    code_len: usize,
) -> Result<[Option<usize>; 2], String> {
    let next = (pc + 1 < code_len).then_some(pc + 1);
    Ok(match instruction.opcode() {
        Opcode::Jump => [
            Some(branch_target(pc, instruction.imm32() as i64, code_len)?),
            None,
        ],
        Opcode::JumpIf | Opcode::JumpIfNot => [
            next,
            Some(branch_target(pc, instruction.imm32() as i64, code_len)?),
        ],
        Opcode::ForLoop => [
            next,
            Some(branch_target(
                pc,
                1 + i64::from(instruction.c as i16),
                code_len,
            )?),
        ],
        Opcode::Return | Opcode::Panic => [None, None],
        _ => [next, None],
    })
}

struct Block {
    start: usize,
    end: usize,
}

fn blocks(code: &[Instruction]) -> Result<(Vec<Block>, Vec<usize>), String> {
    let mut leaders = vec![false; code.len()];
    leaders[0] = true;
    for (pc, &instruction) in code.iter().enumerate() {
        if matches!(
            instruction.opcode(),
            Opcode::Jump
                | Opcode::JumpIf
                | Opcode::JumpIfNot
                | Opcode::ForLoop
                | Opcode::Return
                | Opcode::Panic
        ) {
            for target in successors(pc, instruction, code.len())?
                .into_iter()
                .flatten()
            {
                leaders[target] = true;
            }
            if pc + 1 < code.len() {
                leaders[pc + 1] = true;
            }
        }
    }
    let starts = leaders
        .iter()
        .enumerate()
        .filter_map(|(pc, &leader)| leader.then_some(pc))
        .collect::<Vec<_>>();
    let mut map = vec![0; code.len()];
    let blocks = starts
        .iter()
        .enumerate()
        .map(|(index, &start)| {
            let end = starts.get(index + 1).copied().unwrap_or(code.len());
            map[start..end].fill(index);
            Block { start, end }
        })
        .collect();
    Ok((blocks, map))
}

fn local_values(
    function: &FunctionDef,
    constants: &[Constant],
    code: &mut [Instruction],
    blocks: &[Block],
) {
    let mut values = Values::default();
    for block in blocks {
        values.clear();
        for (pc, instruction) in code
            .iter_mut()
            .enumerate()
            .take(block.end)
            .skip(block.start)
        {
            let opcode = instruction.opcode();
            if matches!(opcode, Opcode::JumpIf | Opcode::JumpIfNot) {
                if let Some(value) = values.get(instruction.a).integer() {
                    let taken = (value != 0) == (opcode == Opcode::JumpIf);
                    *instruction = if taken {
                        Instruction::new(Opcode::Jump, 0, instruction.b, instruction.c)
                    } else {
                        Instruction::new(Opcode::Hint, 0, 0, 0)
                    };
                } else if scalar(instruction.a, function) {
                    instruction.a = values.representative(instruction.a, &function.slot_types);
                }
                continue;
            }
            if opcode == Opcode::Return && instruction.b == 1 && scalar(instruction.a, function) {
                instruction.a = values.representative(instruction.a, &function.slot_types);
                continue;
            }
            if !scalar(instruction.a, function)
                || pure_destination(*instruction).is_none()
                || opcode == Opcode::CopyN
            {
                values.clear();
                continue;
            }
            let value = match opcode {
                Opcode::LoadInt => Atom::Integer(instruction.imm32() as i64 as u64),
                Opcode::LoadConst => match constants.get(instruction.b as usize) {
                    Some(Constant::Nil) => Atom::Integer(0),
                    Some(Constant::Bool(value)) => Atom::Integer(u64::from(*value)),
                    Some(Constant::Int(value)) => Atom::Integer(*value as u64),
                    Some(Constant::Float(value)) => Atom::Float(value.to_bits()),
                    _ => {
                        values.clear();
                        continue;
                    }
                },
                Opcode::Copy => {
                    instruction.b = values.representative(instruction.b, &function.slot_types);
                    values.get(instruction.b)
                }
                _ => {
                    instruction.b = values.representative(instruction.b, &function.slot_types);
                    if binary(opcode) {
                        instruction.c = values.representative(instruction.c, &function.slot_types);
                    }
                    let expression = Expression {
                        opcode: instruction.op,
                        flags: instruction.flags,
                        left: values.get(instruction.b),
                        right: binary(opcode).then(|| values.get(instruction.c)),
                    };
                    if let Some(value) = fold(expression) {
                        if let Ok(immediate) = i32::try_from(value as i64) {
                            *instruction = Instruction::new(Opcode::LoadInt, instruction.a, 0, 0);
                            set_imm32(instruction, immediate as u32);
                        }
                        Atom::Integer(value)
                    } else if let Some(&value) = values.expressions.get(&expression) {
                        if let Some(&slot) = values.representatives.get(&value) {
                            if values.get(slot) == value
                                && function.slot_types[slot as usize]
                                    == function.slot_types[instruction.a as usize]
                            {
                                *instruction =
                                    Instruction::new(Opcode::Copy, instruction.a, slot, 0);
                            }
                        }
                        value
                    } else {
                        let value = Atom::Definition {
                            pc,
                            slot: instruction.a,
                        };
                        values.expressions.insert(expression, value);
                        value
                    }
                }
            };
            values.store(instruction.a, value, &function.slot_types);
        }
    }
}

fn reachable(
    code: &[Instruction],
    metadata: &[InstructionMetadata],
) -> Result<(Vec<bool>, Vec<bool>), String> {
    let mut reachable = vec![false; code.len()];
    let mut anchors = vec![false; code.len()];
    let mut pending = vec![0];
    while let Some(pc) = pending.pop() {
        if reachable[pc] {
            continue;
        }
        reachable[pc] = true;
        pending.extend(successors(pc, code[pc], code.len())?.into_iter().flatten());
        if code[pc].opcode() == Opcode::Hint && code[pc].flags == HINT_LOOP {
            let InstructionMetadata::LoopEnd { end_pc } = metadata[pc] else {
                return Err("loop hint has no back edge".into());
            };
            let end = end_pc as usize;
            if end >= code.len() {
                return Err("loop back edge is outside its function".into());
            }
            anchors[end] = true;
            pending.push(end);
            let exit = code[pc].imm32_unsigned() as usize;
            if exit < code.len() {
                pending.push(exit);
            }
        }
    }
    Ok((reachable, anchors))
}

fn read_slots(
    module: &Module,
    function: &FunctionDef,
    instruction: &Instruction,
    pc: usize,
    mut visit: impl FnMut(u16, u16),
) -> Result<(), String> {
    visit_instruction_register_reads(
        instruction,
        function.instruction_metadata.get(pc),
        &module.functions,
        &mut visit,
    )
    .map_err(|error| format!("invalid optimizer read effect at {pc}: {error:?}"))?;
    if let FrameMemoryEffect::AliasedRange { start, count } =
        instruction_frame_memory_effect(instruction, function.instruction_metadata.get(pc))
            .map_err(|error| format!("invalid optimizer frame effect at {pc}: {error:?}"))?
    {
        visit(start, count);
    }
    Ok(())
}

fn write_slots(
    module: &Module,
    function: &FunctionDef,
    instruction: &Instruction,
    pc: usize,
    visit: impl FnMut(u16, u16),
) -> Result<(), String> {
    visit_instruction_register_writes(
        instruction,
        function.instruction_metadata.get(pc),
        &module.externs,
        &module.functions,
        visit,
    )
    .map_err(|error| format!("invalid optimizer write effect at {pc}: {error:?}"))
}

fn bit(bits: &[u64], slot: u16) -> bool {
    bits[slot as usize / 64] & (1_u64 << (slot % 64)) != 0
}
fn set(bits: &mut [u64], slot: u16) {
    bits[slot as usize / 64] |= 1_u64 << (slot % 64);
}
fn clear(bits: &mut [u64], slot: u16) {
    bits[slot as usize / 64] &= !(1_u64 << (slot % 64));
}
fn range(start: u16, count: u16) -> std::ops::Range<u16> {
    start..start + count
}

fn validate_operand_ranges(module: &Module, function: &FunctionDef) -> Result<(), String> {
    if function.slot_types.len() != usize::from(function.local_slots) {
        return Err(format!(
            "{} has an inconsistent frame layout",
            function.name
        ));
    }
    for (pc, instruction) in function.code.iter().enumerate() {
        if instruction.opcode() == Opcode::Invalid {
            return Err(format!("invalid opcode at {pc}"));
        }
        let mut invalid = false;
        let mut check = |start: u16, count: u16| {
            invalid |= count != 0
                && usize::from(start) + usize::from(count) > usize::from(function.local_slots);
        };
        read_slots(module, function, instruction, pc, &mut check)?;
        write_slots(module, function, instruction, pc, &mut check)?;
        if invalid {
            return Err(format!(
                "{} has an operand outside its frame at {pc}",
                function.name
            ));
        }
    }
    Ok(())
}

/// Returns without changing keep when the bounded data-flow budget is spent.
fn dead_stores(
    module: &Module,
    function: &FunctionDef,
    code: &[Instruction],
    blocks: &[Block],
    pc_to_block: &[usize],
    keep: &mut [bool],
) -> Result<(), String> {
    let words = (function.local_slots as usize).div_ceil(64);
    let Some(size) = words
        .checked_mul(blocks.len())
        .filter(|&size| size <= 2 * 1024 * 1024)
    else {
        return Ok(());
    };
    if words == 0 {
        return Ok(());
    }
    let mut uses = vec![0_u64; size];
    let mut definitions = vec![0_u64; size];
    let mut live = vec![0_u64; size];
    let mut outgoing = vec![0_u64; words];
    let mut edges = Vec::with_capacity(blocks.len());
    for (index, block) in blocks.iter().enumerate() {
        let uses = &mut uses[index * words..(index + 1) * words];
        let definitions = &mut definitions[index * words..(index + 1) * words];
        for pc in block.start..block.end {
            if !keep[pc] {
                continue;
            }
            read_slots(module, function, &code[pc], pc, |start, count| {
                for slot in range(start, count) {
                    if !bit(definitions, slot) {
                        set(uses, slot);
                    }
                }
            })?;
            write_slots(module, function, &code[pc], pc, |start, count| {
                for slot in range(start, count) {
                    set(definitions, slot);
                }
            })?;
        }
        edges.push(
            successors(block.end - 1, code[block.end - 1], code.len())?
                .map(|pc| pc.map(|pc| pc_to_block[pc])),
        );
    }
    let mut work = 0_usize;
    loop {
        let mut changed = false;
        for index in (0..blocks.len()).rev() {
            outgoing.fill(0);
            for successor in edges[index].into_iter().flatten() {
                for (out, source) in outgoing
                    .iter_mut()
                    .zip(&live[successor * words..(successor + 1) * words])
                {
                    *out |= source;
                }
            }
            for (word, &out) in outgoing.iter().enumerate() {
                let at = index * words + word;
                let value = uses[at] | (out & !definitions[at]);
                changed |= live[at] != value;
                live[at] = value;
            }
            work += words;
            if work > 16 * 1024 * 1024 {
                return Ok(());
            }
        }
        if !changed {
            break;
        }
    }
    for (index, block) in blocks.iter().enumerate() {
        outgoing.fill(0);
        for successor in edges[index].into_iter().flatten() {
            for (out, source) in outgoing
                .iter_mut()
                .zip(&live[successor * words..(successor + 1) * words])
            {
                *out |= source;
            }
        }
        for pc in (block.start..block.end).rev() {
            if !keep[pc] {
                continue;
            }
            if let Some((start, count)) = pure_destination(code[pc]) {
                if range(start, count).all(|slot| scalar(slot, function) && !bit(&outgoing, slot)) {
                    keep[pc] = false;
                    continue;
                }
            }
            write_slots(module, function, &code[pc], pc, |start, count| {
                for slot in range(start, count) {
                    clear(&mut outgoing, slot);
                }
            })?;
            read_slots(module, function, &code[pc], pc, |start, count| {
                for slot in range(start, count) {
                    set(&mut outgoing, slot);
                }
            })?;
        }
    }
    Ok(())
}

pub(crate) fn optimize_module(module: &mut Module) -> Result<(), String> {
    for function_id in 0..module.functions.len() {
        clean_function(module, function_id)?;
    }
    for function_id in inline_calls::compose(module)? {
        clean_function(module, function_id)?;
    }
    inline_calls::compact_sources(module);
    Ok(())
}

fn clean_function(module: &mut Module, function_id: usize) -> Result<(), String> {
    // Folding a branch or composing a call exposes larger blocks to value
    // numbering. Keep the same finite cleanup budget before and after inlining.
    for _ in 0..3 {
        if !optimize_function(module, function_id)? {
            break;
        }
    }
    frame::compact(module, function_id)?;
    Ok(())
}

fn optimize_function(module: &mut Module, function_id: usize) -> Result<bool, String> {
    let function = &module.functions[function_id];
    // Recovery can observe named returns through unwind metadata even
    // when ordinary control flow ends at Panic. Keep this path intact
    // until its implicit edges participate in the transformation model.
    // Cap value-number tables as well as liveness matrices. Very large
    // generated functions keep their original bytecode and compile path.
    if function.code.len() < 2 || function.code.len() > 64 * 1024 || function.has_defer {
        return Ok(false);
    }
    if function.code.len() != function.instruction_metadata.len() {
        return Err("optimizer metadata length mismatch".into());
    }
    validate_operand_ranges(module, function)?;
    let mut code = function.code.clone();
    let (blocks, pc_to_block) = blocks(&code)?;
    local_values(function, &module.constants, &mut code, &blocks);
    let (mut keep, anchors) = reachable(&code, &function.instruction_metadata)?;
    for (pc, instruction) in code.iter().enumerate() {
        if instruction.opcode() == Opcode::Hint && instruction.flags == 0 {
            keep[pc] = false;
        }
    }
    dead_stores(module, function, &code, &blocks, &pc_to_block, &mut keep)?;
    // Each round can expose another jump through an empty block. A fixed
    // budget avoids quadratic cleanup of adversarially nested branches;
    // remaining jumps are valid and can be handled by backend optimizers.
    for _ in 0..16 {
        let map = PcMap::new(&keep)?;
        let mut changed = false;
        for (pc, instruction) in code.iter().enumerate() {
            if keep[pc] && !anchors[pc] && instruction.opcode() == Opcode::Jump {
                let target = branch_target(pc, instruction.imm32() as i64, code.len())?;
                if map.boundary(target)? == map.boundary(pc)? + 1 {
                    keep[pc] = false;
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }
    if code == function.code && keep.iter().all(|&retained| retained) {
        return Ok(false);
    }
    let map = PcMap::new(&keep)?;
    let function = &mut module.functions[function_id];
    let old_metadata = std::mem::take(&mut function.instruction_metadata);
    function.code.clear();
    for (pc, mut metadata) in old_metadata.into_iter().enumerate() {
        if keep[pc] {
            function
                .code
                .push(map.relocate(pc, code[pc], &mut metadata)?);
            function.instruction_metadata.push(metadata);
        }
    }
    (function.has_calls, function.has_call_extern) =
        FunctionDef::compute_call_flags(&function.code);
    if let Some(debug) = module.debug_info.funcs.get_mut(function_id) {
        map.relocate_debug(debug)?;
    }
    if let Some(sources) = module
        .debug_info
        .inline_sources
        .function_mut(function_id as u32)
    {
        map.relocate_inline_sources(sources)?;
    }
    Ok(true)
}
