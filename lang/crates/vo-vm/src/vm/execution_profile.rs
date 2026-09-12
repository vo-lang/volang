//! Opt-in work counts owned by one VM. These counters are absent from normal
//! builds and never aggregate independent child-Island owners implicitly.
use super::ExecResult;
use vo_common_core::instruction::Opcode;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionProfile {
    /// Interpreter opcode dispatches, excluding pre-dispatch GC-poll retries.
    /// Compiled native instructions are described by JitExecutionStats.
    pub opcode_counts: [u64; 256],
    pub interpreter_entries: u64,
    pub frame_refetch_attempts: u64,
    pub allocation_checks: u64,
    pub allocation_retries: u64,
    pub allocation_yields: u64,
    pub queue_continues: u64,
    pub queue_blocks: u64,
    pub queue_transitions: u64,
    pub execution_slices: u64,
    pub timeslice_expirations: u64,
    pub boundary_transitions: u64,
    pub completed_fibers: u64,
    pub other_slice_results: u64,
}

impl Default for ExecutionProfile {
    fn default() -> Self {
        Self {
            opcode_counts: [0; 256],
            interpreter_entries: 0,
            frame_refetch_attempts: 0,
            allocation_checks: 0,
            allocation_retries: 0,
            allocation_yields: 0,
            queue_continues: 0,
            queue_blocks: 0,
            queue_transitions: 0,
            execution_slices: 0,
            timeslice_expirations: 0,
            boundary_transitions: 0,
            completed_fibers: 0,
            other_slice_results: 0,
        }
    }
}

impl ExecutionProfile {
    pub fn instruction_count(&self) -> u64 {
        self.opcode_counts
            .iter()
            .fold(0_u64, |total, count| total.saturating_add(*count))
    }

    pub(super) fn instruction(&mut self, opcode: Opcode) {
        Self::increment(&mut self.opcode_counts[opcode as usize]);
    }

    pub(super) fn increment(counter: &mut u64) {
        *counter = counter.saturating_add(1);
    }

    pub(super) fn slice(&mut self, result: &ExecResult) {
        Self::increment(&mut self.execution_slices);
        let counter = match result {
            ExecResult::TimesliceExpired => &mut self.timeslice_expirations,
            ExecResult::Transition(_) => &mut self.boundary_transitions,
            ExecResult::Done => &mut self.completed_fibers,
            _ => &mut self.other_slice_results,
        };
        Self::increment(counter);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counters_saturate_and_do_not_wrap_into_smaller_work() {
        let mut profile = ExecutionProfile::default();
        profile.opcode_counts[Opcode::AddI as usize] = u64::MAX;
        profile.instruction(Opcode::AddI);
        profile.instruction(Opcode::Return);
        assert_eq!(profile.opcode_counts[Opcode::AddI as usize], u64::MAX);
        assert_eq!(profile.instruction_count(), u64::MAX);
        profile.slice(&ExecResult::Done);
        profile.slice(&ExecResult::TimesliceExpired);
        assert_eq!(profile.execution_slices, 2);
        assert_eq!(profile.completed_fibers, 1);
        assert_eq!(profile.timeslice_expirations, 1);
    }
}
