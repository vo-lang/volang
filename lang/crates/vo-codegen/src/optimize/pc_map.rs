//! One relocation boundary for compiler-owned bytecode transformations.
//! Instruction identities and insertion boundaries have different meanings:
//! a removed instruction has no identity, while its boundary still names the
//! following retained instruction. Loop back edges require an exact identity.

use vo_common_core::debug_info::{FuncDebugInfo, InlineFunctionSources};
use vo_common_core::instruction::{Instruction, Opcode, HINT_LOOP};
use vo_common_core::InstructionMetadata;

pub(super) struct PcMap {
    boundaries: Vec<u32>,
}

impl PcMap {
    pub(super) fn new(keep: &[bool]) -> Result<Self, String> {
        Self::build(keep.iter().map(|&retained| usize::from(retained)))
    }

    /// A replacement exposes its first instruction as a boundary. Consumers
    /// requiring an exact original instruction still require emission width 1.
    pub(super) fn from_lengths(lengths: &[usize]) -> Result<Self, String> {
        Self::build(lengths.iter().copied())
    }

    fn build(lengths: impl ExactSizeIterator<Item = usize>) -> Result<Self, String> {
        let mut boundaries = Vec::with_capacity(lengths.len() + 1);
        let mut next = 0_u32;
        for length in lengths {
            boundaries.push(next);
            next = next
                .checked_add(u32::try_from(length).map_err(|_| "bytecode PC exceeds u32")?)
                .ok_or("bytecode PC exceeds u32")?;
        }
        boundaries.push(next);
        Ok(Self { boundaries })
    }

    pub(super) fn boundary(&self, pc: usize) -> Result<u32, String> {
        self.boundaries
            .get(pc)
            .copied()
            .ok_or_else(|| format!("source PC {pc} is outside the relocation map"))
    }

    fn instruction(&self, pc: usize) -> Result<u32, String> {
        let destination = self.boundary(pc)?;
        if self
            .boundaries
            .get(pc + 1)
            .and_then(|next| next.checked_sub(destination))
            != Some(1)
        {
            return Err(format!("required instruction at PC {pc} was removed"));
        }
        Ok(destination)
    }

    pub(super) fn relocate(
        &self,
        old_pc: usize,
        mut instruction: Instruction,
        metadata: &mut InstructionMetadata,
    ) -> Result<Instruction, String> {
        let new_pc = self.instruction(old_pc)?;
        match instruction.opcode() {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                let target = branch_target(
                    old_pc,
                    instruction.imm32() as i64,
                    self.boundaries.len() - 1,
                )?;
                let delta = i64::from(self.boundary(target)?) - i64::from(new_pc);
                set_imm32(
                    &mut instruction,
                    i32::try_from(delta).map_err(|_| "relocated branch exceeds i32")? as u32,
                );
            }
            Opcode::ForLoop => {
                let target = branch_target(
                    old_pc,
                    1 + i64::from(instruction.c as i16),
                    self.boundaries.len() - 1,
                )?;
                let delta = i64::from(self.boundary(target)?) - i64::from(new_pc) - 1;
                instruction.c =
                    i16::try_from(delta).map_err(|_| "relocated ForLoop exceeds i16")? as u16;
            }
            Opcode::Hint if instruction.flags == HINT_LOOP => {
                let exit = self.boundary(instruction.imm32_unsigned() as usize)?;
                set_imm32(&mut instruction, exit);
                let InstructionMetadata::LoopEnd { end_pc } = metadata else {
                    return Err("loop hint is missing its back-edge metadata".into());
                };
                *end_pc = self.instruction(*end_pc as usize)?;
            }
            _ => {}
        }
        Ok(instruction)
    }

    pub(super) fn relocate_debug(&self, debug: &mut FuncDebugInfo) -> Result<(), String> {
        for entry in &mut debug.entries {
            entry.pc = self.boundary(entry.pc as usize)?;
        }
        let end = *self
            .boundaries
            .last()
            .expect("relocation contains its end boundary");
        debug.entries.retain(|entry| entry.pc < end);
        // Source entries already follow old-PC order. Stable sorting preserves
        // the last, most specific location when removed ranges collapse.
        debug.sort();
        Ok(())
    }

    pub(super) fn relocate_inline_sources(
        &self,
        sources: &mut InlineFunctionSources,
    ) -> Result<(), String> {
        // Exact instruction attachments cannot be inherited by the successor
        // when a removed range collapses to one insertion boundary.
        for entry in &sources.entries {
            if entry.pc as usize >= self.boundaries.len() - 1 {
                return Err("inline source PC is outside the relocation map".into());
            }
        }
        sources.entries.retain_mut(|entry| {
            if let Ok(pc) = self.instruction(entry.pc as usize) {
                entry.pc = pc;
                true
            } else {
                false
            }
        });
        Ok(())
    }
}

pub(super) fn branch_target(pc: usize, delta: i64, code_len: usize) -> Result<usize, String> {
    let target = (pc as i64)
        .checked_add(delta)
        .and_then(|value| usize::try_from(value).ok())
        .filter(|&target| target < code_len)
        .ok_or_else(|| format!("branch at PC {pc} is outside its function"))?;
    Ok(target)
}

pub(super) fn set_imm32(instruction: &mut Instruction, value: u32) {
    instruction.b = value as u16;
    instruction.c = (value >> 16) as u16;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn relocates_relative_branches_absolute_hints_and_debug_together() {
        let map = PcMap::new(&[true, false, true, true, false, true]).unwrap();
        let mut hint = Instruction::with_flags(Opcode::Hint, HINT_LOOP, 0, 5, 0);
        let mut metadata = InstructionMetadata::LoopEnd { end_pc: 3 };
        hint = map.relocate(0, hint, &mut metadata).unwrap();
        assert_eq!(hint.imm32_unsigned(), 3);
        assert!(matches!(
            metadata,
            InstructionMetadata::LoopEnd { end_pc: 2 }
        ));
        let instruction = Instruction::new(Opcode::ForLoop, 0, 1, (-2_i16) as u16);
        assert_eq!(
            map.relocate(3, instruction, &mut InstructionMetadata::None)
                .unwrap()
                .c as i16,
            -2
        );
        let instruction = Instruction::new(Opcode::JumpIf, 0, 3, 0);
        assert_eq!(
            map.relocate(2, instruction, &mut InstructionMetadata::None)
                .unwrap()
                .imm32(),
            2
        );
        let mut debug = FuncDebugInfo::new();
        debug.add(1, 0, 10, 1, 1);
        debug.add(2, 0, 11, 1, 1);
        debug.add(5, 0, 12, 1, 1);
        map.relocate_debug(&mut debug).unwrap();
        assert_eq!(
            debug
                .entries
                .iter()
                .map(|entry| (entry.pc, entry.line))
                .collect::<Vec<_>>(),
            [(1, 10), (1, 11), (3, 12)]
        );
    }

    #[test]
    fn inserted_and_removed_ranges_preserve_branches_and_exact_back_edges() {
        let map = PcMap::from_lengths(&[1, 3, 0, 1]).unwrap();
        let branch = Instruction::new(Opcode::Jump, 0, 3, 0);
        assert_eq!(
            map.relocate(0, branch, &mut InstructionMetadata::None)
                .unwrap()
                .imm32(),
            4
        );
        let back = Instruction::new(Opcode::ForLoop, 0, 1, (-3_i16) as u16);
        assert_eq!(
            map.relocate(3, back, &mut InstructionMetadata::None)
                .unwrap()
                .c as i16,
            -4
        );
        assert!(map.instruction(1).is_err());
        assert!(map.instruction(2).is_err());
        let mut debug = FuncDebugInfo::new();
        debug.add(1, 0, 10, 1, 1);
        debug.add(3, 0, 20, 1, 1);
        map.relocate_debug(&mut debug).unwrap();
        assert_eq!(
            debug
                .entries
                .iter()
                .map(|e| (e.pc, e.line))
                .collect::<Vec<_>>(),
            [(1, 10), (4, 20)]
        );
    }

    #[test]
    fn removed_back_edges_and_out_of_range_targets_fail_closed() {
        let map = PcMap::new(&[true, false, true]).unwrap();
        let hint = Instruction::with_flags(Opcode::Hint, HINT_LOOP, 0, 2, 0);
        assert!(map
            .relocate(0, hint, &mut InstructionMetadata::LoopEnd { end_pc: 1 })
            .is_err());
        assert!(map
            .relocate(
                0,
                Instruction::new(Opcode::Jump, 0, 8, 0),
                &mut InstructionMetadata::None
            )
            .is_err());
    }

    #[test]
    fn removed_inline_origins_do_not_move_to_the_next_instruction() {
        use vo_common_core::debug_info::InlineSourceEntry;
        let map = PcMap::from_lengths(&[0, 1, 3, 1]).unwrap();
        let mut sources = InlineFunctionSources {
            function_id: 7,
            entries: (0..4)
                .map(|pc| InlineSourceEntry { pc, frame: pc + 10 })
                .collect(),
        };
        map.relocate_inline_sources(&mut sources).unwrap();
        assert_eq!(
            sources.entries,
            [
                InlineSourceEntry { pc: 0, frame: 11 },
                InlineSourceEntry { pc: 4, frame: 13 }
            ]
        );
    }
}
