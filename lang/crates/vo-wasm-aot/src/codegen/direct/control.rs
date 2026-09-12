//! Bounded stackification for typed functions with lexical, single-entry loops.
//!
//! Instructions, block entries and fuel polls stay in bytecode order. Forward
//! labels and loop labels replace the per-block dispatcher only when every CFG
//! edge has a legal structured target. Other graphs retain the dispatcher.
use super::*;

const MAX_BLOCKS: usize = 4096;
const MAX_INSTRUCTIONS: usize = 65_536;
const MAX_LABEL_DEPTH: usize = 128;
const MAX_EDGE_CHECKS: usize = 1_000_000;

#[derive(Clone, Copy)]
pub(super) enum DirectControl<'a> {
    Dispatch { loop_depth: u32 },
    Structured { block: u32, labels: &'a [Label] },
}

impl DirectControl<'_> {
    pub(super) fn jump(self, body: &mut Function, locals: TypedFunctionLocals, target: u32) {
        match self {
            Self::Dispatch { loop_depth } => set_typed_block(body, locals, target, loop_depth),
            Self::Structured { block, labels } => {
                if target != block + 1 {
                    body.instruction(&W::Br(label_depth(labels, target).expect("validated edge")));
                }
            }
        }
    }

    /// Consumes an i32 predicate. The untaken edge is the next bytecode block.
    pub(super) fn conditional(
        self,
        body: &mut Function,
        locals: TypedFunctionLocals,
        target: u32,
        fallthrough: u32,
    ) {
        match self {
            Self::Dispatch { loop_depth } => {
                body.instruction(&W::If(BlockType::Empty))
                    .instruction(&W::I32Const(target as i32))
                    .instruction(&W::LocalSet(locals.block))
                    .instruction(&W::Else)
                    .instruction(&W::I32Const(fallthrough as i32))
                    .instruction(&W::LocalSet(locals.block))
                    .instruction(&W::End)
                    .instruction(&W::Br(loop_depth));
            }
            Self::Structured { block, labels } => {
                debug_assert_eq!(fallthrough, block + 1);
                if target == fallthrough {
                    body.instruction(&W::Drop);
                } else {
                    body.instruction(&W::BrIf(
                        label_depth(labels, target).expect("validated edge"),
                    ));
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Label {
    Forward(u32),
    Loop(u32),
}

impl Label {
    fn target(self) -> u32 {
        match self {
            Self::Forward(target) | Self::Loop(target) => target,
        }
    }
}

fn label_depth(labels: &[Label], target: u32) -> Option<u32> {
    labels
        .iter()
        .rev()
        .position(|label| label.target() == target)
        .map(|depth| depth as u32)
}

#[derive(Clone, Copy, Debug)]
enum Event {
    Open(Label),
    Close,
    Block(u32),
}

pub(super) struct StructuredControlPlan {
    events: Vec<Event>,
}

impl StructuredControlPlan {
    pub(super) fn for_function(
        function: &FunctionDef,
        blocks: &[BasicBlock],
        by_pc: &BTreeMap<usize, u32>,
    ) -> Option<Self> {
        if blocks.len() > MAX_BLOCKS || function.code.len() > MAX_INSTRUCTIONS {
            return None;
        }
        let mut edges = vec![Vec::new(); blocks.len()];
        for (index, block) in blocks.iter().enumerate() {
            let mut falls_through = true;
            for pc in block.start..block.end {
                let instruction = &function.code[pc];
                match instruction.opcode() {
                    Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot | Opcode::ForLoop => {
                        edges[index].push(*by_pc.get(&branch_target(pc, instruction))? as usize);
                        if instruction.opcode() != Opcode::Jump {
                            let next = *by_pc.get(&(pc + 1))? as usize;
                            if next != index + 1 {
                                return None;
                            }
                            edges[index].push(next);
                        }
                        falls_through = false;
                        break;
                    }
                    Opcode::Return | Opcode::Panic => {
                        falls_through = false;
                        break;
                    }
                    _ => {}
                }
            }
            if falls_through && index + 1 < blocks.len() {
                edges[index].push(index + 1);
            }
        }
        Self::for_edges(&edges)
    }

    fn for_edges(edges: &[Vec<usize>]) -> Option<Self> {
        let count = edges.len();
        if count == 0 || count > MAX_BLOCKS {
            return None;
        }
        let mut loop_ends = vec![None; count];
        let mut forward_targets = vec![false; count];
        for (source, successors) in edges.iter().enumerate() {
            if successors.len() > 2 {
                return None;
            }
            for &target in successors {
                if target >= count {
                    return None;
                }
                if target <= source {
                    loop_ends[target] = Some(loop_ends[target].unwrap_or(0).max(source + 1));
                } else if target != source + 1 {
                    forward_targets[target] = true;
                }
            }
        }
        let mut remaining_checks = MAX_EDGE_CHECKS;
        for (header, end) in loop_ends
            .iter()
            .enumerate()
            .filter_map(|(h, e)| e.map(|e| (h, e)))
        {
            for (source, successors) in edges.iter().enumerate() {
                for &target in successors {
                    remaining_checks = remaining_checks.checked_sub(1)?;
                    // All incoming edges must enter at the header. An edge to
                    // the interior would skip the opening Wasm loop label.
                    if (source < header || source >= end) && target > header && target < end {
                        return None;
                    }
                }
            }
        }
        let mut plan = Self {
            events: Vec::with_capacity(count * 5),
        };
        plan.region(0, count, None, &loop_ends, &forward_targets, 0)?;
        let mut labels = Vec::new();
        let mut next_block = 0;
        for event in &plan.events {
            match *event {
                Event::Open(label) => {
                    if labels.len() >= MAX_LABEL_DEPTH {
                        return None;
                    }
                    labels.push(label);
                }
                Event::Close => {
                    labels.pop()?;
                }
                Event::Block(block) => {
                    if block as usize != next_block {
                        return None;
                    }
                    next_block += 1;
                    for &target in &edges[block as usize] {
                        if target != next_block && label_depth(&labels, target as u32).is_none() {
                            return None;
                        }
                    }
                }
            }
        }
        (labels.is_empty() && next_block == count).then_some(plan)
    }

    fn region(
        &mut self,
        start: usize,
        end: usize,
        own_header: Option<usize>,
        loop_ends: &[Option<usize>],
        forward_targets: &[bool],
        depth: usize,
    ) -> Option<()> {
        if depth >= MAX_LABEL_DEPTH {
            return None;
        }
        let mut nodes = Vec::new();
        let mut cursor = start;
        while cursor < end {
            if Some(cursor) != own_header {
                if let Some(loop_end) = loop_ends[cursor] {
                    if loop_end > end {
                        return None;
                    } // Crossing loop intervals.
                    nodes.push((cursor, Some(loop_end)));
                    cursor = loop_end;
                    continue;
                }
            }
            nodes.push((cursor, None));
            cursor += 1;
        }
        for &(target, _) in nodes.iter().skip(1).rev() {
            if forward_targets[target] {
                self.events.push(Event::Open(Label::Forward(target as u32)));
            }
        }
        for (index, (target, loop_end)) in nodes.into_iter().enumerate() {
            if index > 0 && forward_targets[target] {
                self.events.push(Event::Close);
            }
            if let Some(loop_end) = loop_end {
                self.events.push(Event::Open(Label::Loop(target as u32)));
                self.region(
                    target,
                    loop_end,
                    Some(target),
                    loop_ends,
                    forward_targets,
                    depth + 1,
                )?;
                self.events.push(Event::Close);
            } else {
                self.events.push(Event::Block(target as u32));
            }
        }
        Some(())
    }

    pub(super) fn emit(
        &self,
        body: &mut Function,
        mut block: impl FnMut(&mut Function, usize, DirectControl<'_>) -> Result<(), WasmAotError>,
    ) -> Result<(), WasmAotError> {
        let mut labels = Vec::new();
        for event in &self.events {
            match *event {
                Event::Open(label) => {
                    body.instruction(&match label {
                        Label::Forward(_) => W::Block(BlockType::Empty),
                        Label::Loop(_) => W::Loop(BlockType::Empty),
                    });
                    labels.push(label);
                }
                Event::Close => {
                    body.instruction(&W::End);
                    labels.pop();
                }
                Event::Block(index) => block(
                    body,
                    index as usize,
                    DirectControl::Structured {
                        block: index,
                        labels: &labels,
                    },
                )?,
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests;
