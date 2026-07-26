use alloc::collections::{BTreeMap, VecDeque};
use alloc::vec;
use alloc::vec::Vec;

use vo_runtime::host_services_v2::CallerEndpointHandle;

use crate::{
    CertifiedEntryLaunch, EntryDescriptor, EntryFramework, RequestId, RequestOutcome,
    RuntimeArtifactRole,
};

const MAX_ENTRY_ERROR_BYTES: usize = 4096;

pub type EntryLaunchId = u64;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct EntryLaunchSupervisorConfig {
    pub max_live_launches: usize,
    pub max_pending_commands: usize,
    pub max_pending_bytes: usize,
    pub max_completions: usize,
}

impl Default for EntryLaunchSupervisorConfig {
    fn default() -> Self {
        Self {
            max_live_launches: 64,
            max_pending_commands: 32,
            max_pending_bytes: 32 * 1024 * 1024,
            max_completions: 64,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryLaunchState {
    Pending,
    Constructing,
    Running,
    Failed,
    Cancelled,
    Closed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct EntryLaunchRecord {
    pub launch_id: EntryLaunchId,
    pub caller: CallerEndpointHandle,
    pub request_id: RequestId,
    pub host_wait_key: u64,
    pub framework: EntryFramework,
    pub artifact_identity: [u8; 32],
    pub factory_id: u64,
    pub plan_identity: [u8; 32],
    pub plan_generation: u64,
    pub state: EntryLaunchState,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EntryIslandConstructCommand {
    pub launch_id: EntryLaunchId,
    pub framework: EntryFramework,
    pub function_id: u32,
    pub descriptor: EntryDescriptor,
    pub init: Vec<u8>,
    pub entry_artifact_digest: [u8; 32],
    pub plan_identity: [u8; 32],
    pub plan_generation: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EntryLaunchCompletion {
    pub launch_id: EntryLaunchId,
    pub caller: CallerEndpointHandle,
    pub request_id: RequestId,
    pub outcome: RequestOutcome,
    pub response: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EntryLaunchSupervisorError {
    InvalidConfig,
    Closed,
    Capacity,
    CommandCapacity,
    CompletionCapacity,
    IdentityExhausted,
    InvalidCertifiedLaunch,
    DuplicateRequest,
    UnknownLaunch,
    InvalidTransition,
    CallerMismatch,
}

pub struct EntryLaunchSupervisor {
    config: EntryLaunchSupervisorConfig,
    next_launch_id: EntryLaunchId,
    records: BTreeMap<EntryLaunchId, EntryLaunchRecord>,
    commands: VecDeque<EntryIslandConstructCommand>,
    pending_bytes: usize,
    completions: VecDeque<EntryLaunchCompletion>,
    closed: bool,
}

impl EntryLaunchSupervisor {
    pub fn new(config: EntryLaunchSupervisorConfig) -> Result<Self, EntryLaunchSupervisorError> {
        if config.max_live_launches == 0
            || config.max_pending_commands == 0
            || config.max_pending_bytes == 0
            || config.max_completions == 0
        {
            return Err(EntryLaunchSupervisorError::InvalidConfig);
        }
        Ok(Self {
            config,
            next_launch_id: 1,
            records: BTreeMap::new(),
            commands: VecDeque::new(),
            pending_bytes: 0,
            completions: VecDeque::new(),
            closed: false,
        })
    }

    pub fn enqueue(
        &mut self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
        host_wait_key: u64,
        certified: CertifiedEntryLaunch,
    ) -> Result<EntryLaunchId, EntryLaunchSupervisorError> {
        if self.closed {
            return Err(EntryLaunchSupervisorError::Closed);
        }
        if !caller.is_valid()
            || request_id == 0
            || host_wait_key == 0
            || certified.plan_generation == 0
            || certified.entry_artifact.role != RuntimeArtifactRole::EntryCode
            || certified.launch.descriptor.factory_id() == 0
            || certified.launch.descriptor.artifact_identity()
                != certified.entry_artifact.artifact_identity
        {
            return Err(EntryLaunchSupervisorError::InvalidCertifiedLaunch);
        }
        if self.records.len() == self.config.max_live_launches {
            return Err(EntryLaunchSupervisorError::Capacity);
        }
        if self.commands.len() == self.config.max_pending_commands {
            return Err(EntryLaunchSupervisorError::CommandCapacity);
        }
        let command_bytes = certified.launch.init.len();
        if command_bytes
            > self
                .config
                .max_pending_bytes
                .saturating_sub(self.pending_bytes)
        {
            return Err(EntryLaunchSupervisorError::CommandCapacity);
        }
        if self
            .records
            .values()
            .any(|record| record.caller == caller && record.request_id == request_id)
        {
            return Err(EntryLaunchSupervisorError::DuplicateRequest);
        }
        let launch_id = self.next_launch_id;
        self.next_launch_id = self
            .next_launch_id
            .checked_add(1)
            .ok_or(EntryLaunchSupervisorError::IdentityExhausted)?;
        let descriptor = certified.launch.descriptor;
        self.records.insert(
            launch_id,
            EntryLaunchRecord {
                launch_id,
                caller,
                request_id,
                host_wait_key,
                framework: descriptor.framework(),
                artifact_identity: descriptor.artifact_identity(),
                factory_id: descriptor.factory_id(),
                plan_identity: certified.plan_identity,
                plan_generation: certified.plan_generation,
                state: EntryLaunchState::Pending,
            },
        );
        self.pending_bytes += command_bytes;
        self.commands.push_back(EntryIslandConstructCommand {
            launch_id,
            framework: descriptor.framework(),
            function_id: certified.factory.function_id,
            descriptor,
            init: certified.launch.init,
            entry_artifact_digest: certified.entry_artifact.content_digest,
            plan_identity: certified.plan_identity,
            plan_generation: certified.plan_generation,
        });
        Ok(launch_id)
    }

    pub fn take_construct_command(
        &mut self,
    ) -> Result<Option<EntryIslandConstructCommand>, EntryLaunchSupervisorError> {
        let Some(command) = self.commands.pop_front() else {
            return Ok(None);
        };
        self.pending_bytes = self.pending_bytes.saturating_sub(command.init.len());
        let record = self
            .records
            .get_mut(&command.launch_id)
            .ok_or(EntryLaunchSupervisorError::UnknownLaunch)?;
        if record.state != EntryLaunchState::Pending {
            return Err(EntryLaunchSupervisorError::InvalidTransition);
        }
        record.state = EntryLaunchState::Constructing;
        Ok(Some(command))
    }

    pub fn mark_running(
        &mut self,
        launch_id: EntryLaunchId,
    ) -> Result<(), EntryLaunchSupervisorError> {
        let record = self
            .records
            .get_mut(&launch_id)
            .ok_or(EntryLaunchSupervisorError::UnknownLaunch)?;
        if record.state != EntryLaunchState::Constructing {
            return Err(EntryLaunchSupervisorError::InvalidTransition);
        }
        if self.completions.len() == self.config.max_completions {
            return Err(EntryLaunchSupervisorError::CompletionCapacity);
        }
        record.state = EntryLaunchState::Running;
        self.completions.push_back(EntryLaunchCompletion {
            launch_id,
            caller: record.caller,
            request_id: record.request_id,
            outcome: RequestOutcome::Success,
            response: vec![0],
        });
        Ok(())
    }

    pub fn fail(
        &mut self,
        launch_id: EntryLaunchId,
        message: &[u8],
    ) -> Result<(), EntryLaunchSupervisorError> {
        if self.completions.len() == self.config.max_completions {
            return Err(EntryLaunchSupervisorError::CompletionCapacity);
        }
        let record = self
            .records
            .get_mut(&launch_id)
            .ok_or(EntryLaunchSupervisorError::UnknownLaunch)?;
        if record.state != EntryLaunchState::Pending
            && record.state != EntryLaunchState::Constructing
        {
            return Err(EntryLaunchSupervisorError::InvalidTransition);
        }
        if record.state == EntryLaunchState::Pending {
            remove_pending_command(&mut self.commands, &mut self.pending_bytes, launch_id);
        }
        record.state = EntryLaunchState::Failed;
        self.completions.push_back(EntryLaunchCompletion {
            launch_id,
            caller: record.caller,
            request_id: record.request_id,
            outcome: RequestOutcome::ProviderError,
            response: error_response(message),
        });
        Ok(())
    }

    pub fn cancel_request(
        &mut self,
        caller: CallerEndpointHandle,
        request_id: RequestId,
    ) -> Result<Option<EntryLaunchId>, EntryLaunchSupervisorError> {
        let launch_id = self.records.values().find_map(|record| {
            (record.caller == caller && record.request_id == request_id).then_some(record.launch_id)
        });
        let Some(launch_id) = launch_id else {
            return Ok(None);
        };
        if self.completions.len() == self.config.max_completions {
            return Err(EntryLaunchSupervisorError::CompletionCapacity);
        }
        let record = self
            .records
            .get_mut(&launch_id)
            .expect("launch identity came from the same map");
        if record.state != EntryLaunchState::Pending
            && record.state != EntryLaunchState::Constructing
        {
            return Err(EntryLaunchSupervisorError::InvalidTransition);
        }
        if record.state == EntryLaunchState::Pending {
            remove_pending_command(&mut self.commands, &mut self.pending_bytes, launch_id);
        }
        record.state = EntryLaunchState::Cancelled;
        self.completions.push_back(EntryLaunchCompletion {
            launch_id,
            caller,
            request_id,
            outcome: RequestOutcome::Cancelled,
            response: error_response(b"entry launch cancelled"),
        });
        Ok(Some(launch_id))
    }

    pub fn close_launch(
        &mut self,
        launch_id: EntryLaunchId,
    ) -> Result<EntryLaunchRecord, EntryLaunchSupervisorError> {
        let mut record = self
            .records
            .remove(&launch_id)
            .ok_or(EntryLaunchSupervisorError::UnknownLaunch)?;
        if record.state == EntryLaunchState::Pending {
            remove_pending_command(&mut self.commands, &mut self.pending_bytes, launch_id);
        }
        record.state = EntryLaunchState::Closed;
        Ok(record)
    }

    pub fn close(&mut self) -> Result<(), EntryLaunchSupervisorError> {
        if self.closed {
            return Ok(());
        }
        let pending = self
            .records
            .values()
            .filter(|record| {
                record.state == EntryLaunchState::Pending
                    || record.state == EntryLaunchState::Constructing
            })
            .count();
        if pending
            > self
                .config
                .max_completions
                .saturating_sub(self.completions.len())
        {
            return Err(EntryLaunchSupervisorError::CompletionCapacity);
        }
        self.closed = true;
        self.commands.clear();
        self.pending_bytes = 0;
        for record in self.records.values_mut() {
            let needs_completion = record.state == EntryLaunchState::Pending
                || record.state == EntryLaunchState::Constructing;
            if needs_completion {
                record.state = EntryLaunchState::Closed;
                self.completions.push_back(EntryLaunchCompletion {
                    launch_id: record.launch_id,
                    caller: record.caller,
                    request_id: record.request_id,
                    outcome: RequestOutcome::SessionClosed,
                    response: error_response(b"entry launch supervisor closed"),
                });
            } else if record.state == EntryLaunchState::Running {
                record.state = EntryLaunchState::Closed;
            }
        }
        Ok(())
    }

    pub fn take_completion(&mut self) -> Option<EntryLaunchCompletion> {
        self.completions.pop_front()
    }

    pub fn record(&self, launch_id: EntryLaunchId) -> Option<EntryLaunchRecord> {
        self.records.get(&launch_id).copied()
    }

    pub fn pending_bytes(&self) -> usize {
        self.pending_bytes
    }
}

fn remove_pending_command(
    commands: &mut VecDeque<EntryIslandConstructCommand>,
    pending_bytes: &mut usize,
    launch_id: EntryLaunchId,
) {
    if let Some(index) = commands
        .iter()
        .position(|command| command.launch_id == launch_id)
    {
        if let Some(command) = commands.remove(index) {
            *pending_bytes = pending_bytes.saturating_sub(command.init.len());
        }
    }
}

fn error_response(message: &[u8]) -> Vec<u8> {
    let message = &message[..message.len().min(MAX_ENTRY_ERROR_BYTES)];
    let mut response = Vec::with_capacity(1 + message.len());
    response.push(1);
    response.extend_from_slice(message);
    response
}
