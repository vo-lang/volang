use alloc::vec::Vec;

use crate::{EntryLaunch, VoplayEngineCommand, VoplayPublicEngineDesc, VoplayPublicEngineRef};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct VoplayEngineControlConfig {
    pub max_engines: usize,
    pub max_pending_manual_ticks: u64,
}

impl Default for VoplayEngineControlConfig {
    fn default() -> Self {
        Self {
            max_engines: 64,
            max_pending_manual_ticks: 1_000_000,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VoplayPublicEngineState {
    Created,
    Configuring,
    Starting,
    Running,
    Suspended,
    Recovering,
    Stopping,
    Stopped,
    Failed,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VoplayEngineControlError {
    InvalidConfig,
    WrongSession,
    Capacity,
    StaleEngine,
    InvalidState,
    AlreadyInstalled,
    MissingEntry,
    TickCapacity,
    GenerationExhausted,
}

#[derive(Clone, Debug)]
struct EngineRecord {
    generation: u32,
    descriptor: VoplayPublicEngineDesc,
    entry: Option<EntryLaunch>,
    state: VoplayPublicEngineState,
    pending_manual_ticks: u64,
}

pub struct VoplayEngineControlStore {
    session_index: u32,
    session_generation: u32,
    session_epoch: u64,
    config: VoplayEngineControlConfig,
    records: Vec<Option<EngineRecord>>,
    generations: Vec<u32>,
    free: Vec<u32>,
}

impl VoplayEngineControlStore {
    pub fn new(
        session_index: u32,
        session_generation: u32,
        session_epoch: u64,
        config: VoplayEngineControlConfig,
    ) -> Result<Self, VoplayEngineControlError> {
        if session_index == u32::MAX
            || session_generation == 0
            || session_epoch == 0
            || config.max_engines == 0
            || config.max_engines > u32::MAX as usize
            || config.max_pending_manual_ticks == 0
        {
            return Err(VoplayEngineControlError::InvalidConfig);
        }
        Ok(Self {
            session_index,
            session_generation,
            session_epoch,
            config,
            records: Vec::new(),
            generations: Vec::new(),
            free: Vec::new(),
        })
    }

    pub fn apply(
        &mut self,
        command: VoplayEngineCommand,
    ) -> Result<Option<VoplayPublicEngineRef>, VoplayEngineControlError> {
        match command {
            VoplayEngineCommand::New {
                session_index,
                session_generation,
                session_epoch,
                descriptor,
            } => self
                .create(session_index, session_generation, session_epoch, descriptor)
                .map(Some),
            VoplayEngineCommand::Install { engine, entry } => {
                self.install(engine, entry)?;
                Ok(None)
            }
            VoplayEngineCommand::Start(engine) => {
                self.begin_start(engine)?;
                Ok(None)
            }
            VoplayEngineCommand::Step { engine, count } => {
                self.queue_manual_ticks(engine, count)?;
                Ok(None)
            }
            VoplayEngineCommand::Pause(engine) => {
                self.pause(engine)?;
                Ok(None)
            }
            VoplayEngineCommand::Resume(engine) => {
                self.resume(engine)?;
                Ok(None)
            }
            VoplayEngineCommand::Shutdown(engine) => {
                self.begin_shutdown(engine)?;
                Ok(None)
            }
        }
    }

    pub fn create(
        &mut self,
        session_index: u32,
        session_generation: u32,
        session_epoch: u64,
        descriptor: VoplayPublicEngineDesc,
    ) -> Result<VoplayPublicEngineRef, VoplayEngineControlError> {
        if (session_index, session_generation, session_epoch)
            != (
                self.session_index,
                self.session_generation,
                self.session_epoch,
            )
        {
            return Err(VoplayEngineControlError::WrongSession);
        }
        let (slot, generation) = if let Some(slot) = self.free.pop() {
            let generation = self.generations[slot as usize]
                .checked_add(1)
                .ok_or(VoplayEngineControlError::GenerationExhausted)?;
            self.generations[slot as usize] = generation;
            (slot, generation)
        } else {
            if self.records.len() == self.config.max_engines {
                return Err(VoplayEngineControlError::Capacity);
            }
            let slot = self.records.len() as u32;
            self.records.push(None);
            self.generations.push(1);
            (slot, 1)
        };
        self.records[slot as usize] = Some(EngineRecord {
            generation,
            descriptor,
            entry: None,
            state: VoplayPublicEngineState::Created,
            pending_manual_ticks: 0,
        });
        Ok(self.public_ref(slot, generation))
    }

    pub fn install(
        &mut self,
        engine: VoplayPublicEngineRef,
        entry: EntryLaunch,
    ) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if record.state != VoplayPublicEngineState::Created {
            return Err(VoplayEngineControlError::InvalidState);
        }
        if record.entry.is_some() {
            return Err(VoplayEngineControlError::AlreadyInstalled);
        }
        record.entry = Some(entry);
        record.state = VoplayPublicEngineState::Configuring;
        Ok(())
    }

    pub fn begin_start(
        &mut self,
        engine: VoplayPublicEngineRef,
    ) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if record.state != VoplayPublicEngineState::Configuring || record.entry.is_none() {
            return Err(VoplayEngineControlError::MissingEntry);
        }
        record.state = VoplayPublicEngineState::Starting;
        Ok(())
    }

    pub fn start_entry(
        &self,
        engine: VoplayPublicEngineRef,
    ) -> Result<&EntryLaunch, VoplayEngineControlError> {
        let record = self.record(engine)?;
        if record.state != VoplayPublicEngineState::Starting {
            return Err(VoplayEngineControlError::InvalidState);
        }
        record
            .entry
            .as_ref()
            .ok_or(VoplayEngineControlError::MissingEntry)
    }

    pub fn mark_running(
        &mut self,
        engine: VoplayPublicEngineRef,
    ) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if !matches!(
            record.state,
            VoplayPublicEngineState::Starting | VoplayPublicEngineState::Recovering
        ) {
            return Err(VoplayEngineControlError::InvalidState);
        }
        record.state = VoplayPublicEngineState::Running;
        Ok(())
    }

    pub fn pause(&mut self, engine: VoplayPublicEngineRef) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if record.state != VoplayPublicEngineState::Running {
            return Err(VoplayEngineControlError::InvalidState);
        }
        record.state = VoplayPublicEngineState::Suspended;
        Ok(())
    }

    pub fn resume(
        &mut self,
        engine: VoplayPublicEngineRef,
    ) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if record.state != VoplayPublicEngineState::Suspended {
            return Err(VoplayEngineControlError::InvalidState);
        }
        record.state = VoplayPublicEngineState::Running;
        Ok(())
    }

    pub fn queue_manual_ticks(
        &mut self,
        engine: VoplayPublicEngineRef,
        count: u64,
    ) -> Result<(), VoplayEngineControlError> {
        let limit = self.config.max_pending_manual_ticks;
        let record = self.record_mut(engine)?;
        if record.state != VoplayPublicEngineState::Running
            || !record.descriptor.headless
            || count == 0
        {
            return Err(VoplayEngineControlError::InvalidState);
        }
        record.pending_manual_ticks = record
            .pending_manual_ticks
            .checked_add(count)
            .filter(|ticks| *ticks <= limit)
            .ok_or(VoplayEngineControlError::TickCapacity)?;
        Ok(())
    }

    pub fn take_manual_ticks(
        &mut self,
        engine: VoplayPublicEngineRef,
        max: u64,
    ) -> Result<u64, VoplayEngineControlError> {
        if max == 0 {
            return Err(VoplayEngineControlError::InvalidState);
        }
        let record = self.record_mut(engine)?;
        if record.state != VoplayPublicEngineState::Running {
            return Err(VoplayEngineControlError::InvalidState);
        }
        let count = record.pending_manual_ticks.min(max);
        record.pending_manual_ticks -= count;
        Ok(count)
    }

    pub fn begin_recovery(
        &mut self,
        engine: VoplayPublicEngineRef,
    ) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if !matches!(
            record.state,
            VoplayPublicEngineState::Running | VoplayPublicEngineState::Suspended
        ) {
            return Err(VoplayEngineControlError::InvalidState);
        }
        record.state = VoplayPublicEngineState::Recovering;
        Ok(())
    }

    pub fn fail(&mut self, engine: VoplayPublicEngineRef) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if matches!(
            record.state,
            VoplayPublicEngineState::Stopped | VoplayPublicEngineState::Stopping
        ) {
            return Err(VoplayEngineControlError::InvalidState);
        }
        record.state = VoplayPublicEngineState::Failed;
        record.pending_manual_ticks = 0;
        Ok(())
    }

    pub fn begin_shutdown(
        &mut self,
        engine: VoplayPublicEngineRef,
    ) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if matches!(
            record.state,
            VoplayPublicEngineState::Stopping | VoplayPublicEngineState::Stopped
        ) {
            return Ok(());
        }
        record.state = VoplayPublicEngineState::Stopping;
        record.pending_manual_ticks = 0;
        Ok(())
    }

    pub fn mark_stopped(
        &mut self,
        engine: VoplayPublicEngineRef,
    ) -> Result<(), VoplayEngineControlError> {
        let record = self.record_mut(engine)?;
        if record.state != VoplayPublicEngineState::Stopping {
            return Err(VoplayEngineControlError::InvalidState);
        }
        record.state = VoplayPublicEngineState::Stopped;
        Ok(())
    }

    pub fn release(
        &mut self,
        engine: VoplayPublicEngineRef,
    ) -> Result<(), VoplayEngineControlError> {
        let slot = self.slot(engine)?;
        if self.records[slot]
            .as_ref()
            .is_none_or(|record| record.state != VoplayPublicEngineState::Stopped)
        {
            return Err(VoplayEngineControlError::InvalidState);
        }
        self.records[slot] = None;
        self.free.push(slot as u32);
        Ok(())
    }

    pub fn state(
        &self,
        engine: VoplayPublicEngineRef,
    ) -> Result<VoplayPublicEngineState, VoplayEngineControlError> {
        Ok(self.record(engine)?.state)
    }

    pub fn descriptor(
        &self,
        engine: VoplayPublicEngineRef,
    ) -> Result<&VoplayPublicEngineDesc, VoplayEngineControlError> {
        Ok(&self.record(engine)?.descriptor)
    }

    fn public_ref(&self, slot: u32, generation: u32) -> VoplayPublicEngineRef {
        VoplayPublicEngineRef {
            session_index: self.session_index,
            session_generation: self.session_generation,
            session_epoch: self.session_epoch,
            engine_index: slot,
            engine_generation: generation,
        }
    }

    fn slot(&self, engine: VoplayPublicEngineRef) -> Result<usize, VoplayEngineControlError> {
        if (
            engine.session_index,
            engine.session_generation,
            engine.session_epoch,
        ) != (
            self.session_index,
            self.session_generation,
            self.session_epoch,
        ) || engine.engine_index == u32::MAX
            || engine.engine_generation == 0
        {
            return Err(VoplayEngineControlError::WrongSession);
        }
        let slot = engine.engine_index as usize;
        let record = self
            .records
            .get(slot)
            .and_then(Option::as_ref)
            .ok_or(VoplayEngineControlError::StaleEngine)?;
        if record.generation != engine.engine_generation {
            return Err(VoplayEngineControlError::StaleEngine);
        }
        Ok(slot)
    }

    fn record(
        &self,
        engine: VoplayPublicEngineRef,
    ) -> Result<&EngineRecord, VoplayEngineControlError> {
        let slot = self.slot(engine)?;
        self.records[slot]
            .as_ref()
            .ok_or(VoplayEngineControlError::StaleEngine)
    }

    fn record_mut(
        &mut self,
        engine: VoplayPublicEngineRef,
    ) -> Result<&mut EngineRecord, VoplayEngineControlError> {
        let slot = self.slot(engine)?;
        self.records[slot]
            .as_mut()
            .ok_or(VoplayEngineControlError::StaleEngine)
    }
}
