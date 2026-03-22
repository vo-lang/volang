use alloc::string::String;
use alloc::vec::Vec;

use vo_vm::vm::SchedulingOutcome;

use crate::PendingHostEvent;

/// Result of a single session step (run, dispatch event, island frame, etc.).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StepResult {
    pub outcome: SchedulingOutcome,
    pub render_output: Option<Vec<u8>>,
    pub stdout: Option<String>,
}

/// Internal aggregate of all side effects produced by one VM execution step.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct SessionEffects {
    pub(crate) replay_event_wait_token: Option<u64>,
    pub(crate) pending_host_events: Vec<PendingHostEvent>,
    pub(crate) outbound_island_frames: Vec<Vec<u8>>,
    pub(crate) render_output: Option<Vec<u8>>,
    pub(crate) stdout: Option<String>,
}

impl SessionEffects {
    pub(crate) fn collect(
        replay_event_wait_token: Option<u64>,
        pending_host_events: Vec<PendingHostEvent>,
        outbound_island_frames: Vec<Vec<u8>>,
        render_output: Option<Vec<u8>>,
        stdout: String,
    ) -> Self {
        Self {
            replay_event_wait_token,
            pending_host_events,
            outbound_island_frames,
            render_output,
            stdout: normalize_stdout(stdout),
        }
    }
}

fn normalize_stdout(stdout: String) -> Option<String> {
    if stdout.is_empty() {
        None
    } else {
        Some(stdout)
    }
}

#[cfg(test)]
mod tests {
    use alloc::string::String;

    use super::normalize_stdout;

    #[test]
    fn normalize_stdout_drops_empty_string() {
        assert_eq!(normalize_stdout(String::new()), None);
    }

    #[test]
    fn normalize_stdout_preserves_non_empty_output() {
        assert_eq!(normalize_stdout(" \n".into()), Some(" \n".into()));
    }
}
