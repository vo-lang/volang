use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    sync::{
        atomic::{AtomicU64, AtomicU8, Ordering},
        Arc, Mutex,
    },
};

use vo_app_protocol::{ViewHandle, WindowHandle};

const FAULT_NONE: u8 = 0;
const FAULT_RELIABLE_OVERFLOW: u8 = 1;
const FAULT_SEQUENCE_EXHAUSTED: u8 = 2;
const FAULT_POISONED: u8 = 3;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct NativeModifiers {
    pub shift: bool,
    pub control: bool,
    pub alt: bool,
    pub meta: bool,
    pub caps_lock: bool,
    pub function: bool,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum NativePointerButton {
    Primary,
    Secondary,
    Middle,
    Auxiliary(u16),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NativeScrollUnit {
    Pixel,
    Line,
    Page,
}

#[derive(Clone, Debug, PartialEq)]
pub enum NativeInputKind {
    PointerMoved {
        device: u64,
        x_milli: i32,
        y_milli: i32,
        delta_x_milli: i32,
        delta_y_milli: i32,
        pressure_milli: u16,
    },
    PointerButton {
        device: u64,
        button: NativePointerButton,
        pressed: bool,
        click_count: u16,
        x_milli: i32,
        y_milli: i32,
    },
    Wheel {
        device: u64,
        x_milli: i32,
        y_milli: i32,
        delta_x_milli: i32,
        delta_y_milli: i32,
        unit: NativeScrollUnit,
    },
    GamepadSnapshot {
        device: u64,
        connected: bool,
        standard_mapping: bool,
        axes_q15: Vec<i16>,
        buttons: Vec<vo_app_runtime::GamepadButton>,
    },
    DeviceDisconnected {
        device: u64,
        kind: vo_app_runtime::InputDeviceKind,
    },
    Key {
        device: u64,
        physical_key: u32,
        logical_key: String,
        pressed: bool,
        repeat: bool,
        modifiers: NativeModifiers,
    },
    ModifiersChanged(NativeModifiers),
    Text(String),
    ImeStarted,
    ImeUpdated {
        text: String,
        selection_start_utf16: u32,
        selection_len_utf16: u32,
    },
    ImeCommitted(String),
    ImeCancelled,
    FileDragEntered {
        x_milli: i32,
        y_milli: i32,
        paths: Vec<String>,
    },
    FileDragMoved {
        x_milli: i32,
        y_milli: i32,
    },
    FileDragLeft,
    FileDropped {
        x_milli: i32,
        y_milli: i32,
        paths: Vec<String>,
    },
    FocusChanged(bool),
    VisibilityChanged(bool),
    Resized {
        width_milli: u32,
        height_milli: u32,
        scale_milli: u32,
    },
    CloseRequested,
}

impl NativeInputKind {
    fn is_sampled(&self) -> bool {
        matches!(
            self,
            Self::PointerMoved { .. }
                | Self::Wheel { .. }
                | Self::GamepadSnapshot { .. }
                | Self::Resized { .. }
                | Self::FileDragMoved { .. }
        )
    }

    fn payload_bytes(&self) -> usize {
        match self {
            Self::Key { logical_key, .. }
            | Self::Text(logical_key)
            | Self::ImeCommitted(logical_key) => logical_key.len(),
            Self::ImeUpdated { text, .. } => text.len(),
            Self::FileDragEntered { paths, .. } | Self::FileDropped { paths, .. } => {
                paths.iter().map(String::len).sum::<usize>()
            }
            _ => 0,
        }
    }

    fn sample_key(&self) -> Option<(u8, u64)> {
        match self {
            Self::PointerMoved { device, .. } => Some((1, *device)),
            Self::Wheel { device, .. } => Some((2, *device)),
            Self::GamepadSnapshot { device, .. } => Some((3, *device)),
            Self::Resized { .. } => Some((4, 0)),
            Self::FileDragMoved { .. } => Some((5, 0)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NativeInputEvent {
    pub sequence: u64,
    pub timestamp_micros: u64,
    pub window: WindowHandle,
    pub view: ViewHandle,
    pub kind: NativeInputKind,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeInputChannelConfig {
    pub max_events: usize,
    pub max_text_bytes: usize,
}

impl Default for NativeInputChannelConfig {
    fn default() -> Self {
        Self {
            max_events: 4_096,
            max_text_bytes: 4 * 1024 * 1024,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NativeHostInputError {
    InvalidConfig,
    InvalidOwner,
    ReliableOverflow,
    SequenceExhausted,
    Poisoned,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct NativeInputStats {
    pub pending_events: usize,
    pub pending_text_bytes: usize,
    pub dropped_samples: u64,
    pub fault: Option<NativeHostInputError>,
}

struct NativeInputBuffer {
    config: NativeInputChannelConfig,
    events: VecDeque<NativeInputEvent>,
    text_bytes: usize,
    held: HeldInputState,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct HeldKeyId {
    window: WindowHandle,
    view: ViewHandle,
    device: u64,
    physical_key: u32,
}

#[derive(Clone, Debug)]
struct HeldKey {
    logical_key: String,
    modifiers: NativeModifiers,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct HeldButtonId {
    window: WindowHandle,
    view: ViewHandle,
    device: u64,
    button: NativePointerButton,
}

#[derive(Clone, Copy, Debug)]
struct HeldButton {
    x_milli: i32,
    y_milli: i32,
}

#[derive(Default)]
struct HeldInputState {
    keys: BTreeMap<HeldKeyId, HeldKey>,
    buttons: BTreeMap<HeldButtonId, HeldButton>,
    ime: BTreeSet<(WindowHandle, ViewHandle)>,
}

struct SharedInputChannel {
    buffer: Mutex<NativeInputBuffer>,
    next_sequence: AtomicU64,
    dropped_samples: AtomicU64,
    fault: AtomicU8,
}

#[derive(Clone)]
pub struct NativeInputChannel {
    shared: Arc<SharedInputChannel>,
}

pub struct NativeInputReceiver {
    shared: Arc<SharedInputChannel>,
}

impl NativeInputChannel {
    pub fn bounded(
        config: NativeInputChannelConfig,
    ) -> Result<(Self, NativeInputReceiver), NativeHostInputError> {
        if config.max_events == 0 || config.max_text_bytes == 0 {
            return Err(NativeHostInputError::InvalidConfig);
        }
        let shared = Arc::new(SharedInputChannel {
            buffer: Mutex::new(NativeInputBuffer {
                config,
                events: VecDeque::with_capacity(config.max_events),
                text_bytes: 0,
                held: HeldInputState::default(),
            }),
            next_sequence: AtomicU64::new(1),
            dropped_samples: AtomicU64::new(0),
            fault: AtomicU8::new(FAULT_NONE),
        });
        Ok((
            Self {
                shared: Arc::clone(&shared),
            },
            NativeInputReceiver { shared },
        ))
    }

    pub fn publish(
        &self,
        timestamp_micros: u64,
        window: WindowHandle,
        view: ViewHandle,
        kind: NativeInputKind,
    ) -> Result<u64, NativeHostInputError> {
        if !window.is_valid() || !view.is_valid() {
            return Err(NativeHostInputError::InvalidOwner);
        }
        self.require_healthy()?;
        if matches!(
            kind,
            NativeInputKind::FocusChanged(false)
                | NativeInputKind::VisibilityChanged(false)
                | NativeInputKind::CloseRequested
        ) {
            return self.publish_suspend(timestamp_micros, window, view, kind);
        }
        let sequence = self
            .shared
            .next_sequence
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |current| {
                current.checked_add(1)
            })
            .map_err(|_| {
                self.shared
                    .fault
                    .store(FAULT_SEQUENCE_EXHAUSTED, Ordering::Release);
                NativeHostInputError::SequenceExhausted
            })?;
        let event = NativeInputEvent {
            sequence,
            timestamp_micros,
            window,
            view,
            kind,
        };
        let sampled = event.kind.is_sampled();
        let Ok(mut buffer) = self.shared.buffer.try_lock() else {
            return self.record_contention(sampled, sequence);
        };
        let new_bytes = event.kind.payload_bytes();
        if buffer.events.len() < buffer.config.max_events
            && buffer
                .text_bytes
                .checked_add(new_bytes)
                .is_some_and(|bytes| bytes <= buffer.config.max_text_bytes)
        {
            buffer.text_bytes += new_bytes;
            buffer.held.observe(&event);
            buffer.events.push_back(event);
            return Ok(sequence);
        }
        if sampled {
            if let Some(sample_key) = event.kind.sample_key() {
                if let Some(index) = buffer.events.iter().rposition(|queued| {
                    queued.window == event.window
                        && queued.view == event.view
                        && queued.kind.sample_key() == Some(sample_key)
                }) {
                    let old_bytes = buffer.events[index].kind.payload_bytes();
                    let candidate_bytes = buffer.text_bytes - old_bytes + new_bytes;
                    if candidate_bytes <= buffer.config.max_text_bytes {
                        buffer.text_bytes = candidate_bytes;
                        buffer.events[index] = event;
                        self.shared.dropped_samples.fetch_add(1, Ordering::Relaxed);
                        return Ok(sequence);
                    }
                }
            }
            self.shared.dropped_samples.fetch_add(1, Ordering::Relaxed);
            return Ok(sequence);
        }
        self.shared
            .fault
            .store(FAULT_RELIABLE_OVERFLOW, Ordering::Release);
        Err(NativeHostInputError::ReliableOverflow)
    }

    fn publish_suspend(
        &self,
        timestamp_micros: u64,
        window: WindowHandle,
        view: ViewHandle,
        terminal: NativeInputKind,
    ) -> Result<u64, NativeHostInputError> {
        let Ok(mut buffer) = self.shared.buffer.try_lock() else {
            return self.record_contention(false, 0);
        };
        let mut kinds = buffer.held.release_kinds(window, view);
        kinds.push(terminal);
        let added_bytes = kinds
            .iter()
            .map(NativeInputKind::payload_bytes)
            .sum::<usize>();
        if buffer
            .events
            .len()
            .checked_add(kinds.len())
            .is_none_or(|count| count > buffer.config.max_events)
            || buffer
                .text_bytes
                .checked_add(added_bytes)
                .is_none_or(|bytes| bytes > buffer.config.max_text_bytes)
        {
            self.shared
                .fault
                .store(FAULT_RELIABLE_OVERFLOW, Ordering::Release);
            return Err(NativeHostInputError::ReliableOverflow);
        }
        let count =
            u64::try_from(kinds.len()).map_err(|_| NativeHostInputError::SequenceExhausted)?;
        let first = self
            .shared
            .next_sequence
            .fetch_update(Ordering::AcqRel, Ordering::Acquire, |current| {
                current.checked_add(count)
            })
            .map_err(|_| {
                self.shared
                    .fault
                    .store(FAULT_SEQUENCE_EXHAUSTED, Ordering::Release);
                NativeHostInputError::SequenceExhausted
            })?;
        let terminal_sequence = first + count - 1;
        for (offset, kind) in kinds.into_iter().enumerate() {
            let event = NativeInputEvent {
                sequence: first + offset as u64,
                timestamp_micros,
                window,
                view,
                kind,
            };
            buffer.text_bytes += event.kind.payload_bytes();
            buffer.events.push_back(event);
        }
        buffer.held.clear_owner(window, view);
        Ok(terminal_sequence)
    }

    fn record_contention(&self, sampled: bool, sequence: u64) -> Result<u64, NativeHostInputError> {
        if sampled {
            self.shared.dropped_samples.fetch_add(1, Ordering::Relaxed);
            Ok(sequence)
        } else {
            self.shared
                .fault
                .store(FAULT_RELIABLE_OVERFLOW, Ordering::Release);
            Err(NativeHostInputError::ReliableOverflow)
        }
    }

    fn require_healthy(&self) -> Result<(), NativeHostInputError> {
        match self.shared.fault.load(Ordering::Acquire) {
            FAULT_NONE => Ok(()),
            FAULT_RELIABLE_OVERFLOW => Err(NativeHostInputError::ReliableOverflow),
            FAULT_SEQUENCE_EXHAUSTED => Err(NativeHostInputError::SequenceExhausted),
            _ => Err(NativeHostInputError::Poisoned),
        }
    }
}

impl HeldInputState {
    fn observe(&mut self, event: &NativeInputEvent) {
        match &event.kind {
            NativeInputKind::Key {
                device,
                physical_key,
                logical_key,
                pressed,
                modifiers,
                ..
            } => {
                let id = HeldKeyId {
                    window: event.window,
                    view: event.view,
                    device: *device,
                    physical_key: *physical_key,
                };
                if *pressed {
                    self.keys.insert(
                        id,
                        HeldKey {
                            logical_key: logical_key.clone(),
                            modifiers: *modifiers,
                        },
                    );
                } else {
                    self.keys.remove(&id);
                }
            }
            NativeInputKind::PointerButton {
                device,
                button,
                pressed,
                x_milli,
                y_milli,
                ..
            } => {
                let id = HeldButtonId {
                    window: event.window,
                    view: event.view,
                    device: *device,
                    button: *button,
                };
                if *pressed {
                    self.buttons.insert(
                        id,
                        HeldButton {
                            x_milli: *x_milli,
                            y_milli: *y_milli,
                        },
                    );
                } else {
                    self.buttons.remove(&id);
                }
            }
            NativeInputKind::ImeStarted => {
                self.ime.insert((event.window, event.view));
            }
            NativeInputKind::ImeCommitted(_) | NativeInputKind::ImeCancelled => {
                self.ime.remove(&(event.window, event.view));
            }
            _ => {}
        }
    }

    fn release_kinds(&self, window: WindowHandle, view: ViewHandle) -> Vec<NativeInputKind> {
        let mut releases = Vec::new();
        for (id, held) in &self.keys {
            if id.window == window && id.view == view {
                releases.push(NativeInputKind::Key {
                    device: id.device,
                    physical_key: id.physical_key,
                    logical_key: held.logical_key.clone(),
                    pressed: false,
                    repeat: false,
                    modifiers: held.modifiers,
                });
            }
        }
        for (id, held) in &self.buttons {
            if id.window == window && id.view == view {
                releases.push(NativeInputKind::PointerButton {
                    device: id.device,
                    button: id.button,
                    pressed: false,
                    click_count: 0,
                    x_milli: held.x_milli,
                    y_milli: held.y_milli,
                });
            }
        }
        if self.ime.contains(&(window, view)) {
            releases.push(NativeInputKind::ImeCancelled);
        }
        releases
    }

    fn clear_owner(&mut self, window: WindowHandle, view: ViewHandle) {
        self.keys
            .retain(|id, _| id.window != window || id.view != view);
        self.buttons
            .retain(|id, _| id.window != window || id.view != view);
        self.ime.remove(&(window, view));
    }
}

impl NativeInputReceiver {
    pub fn drain(&self, max_events: usize) -> Result<Vec<NativeInputEvent>, NativeHostInputError> {
        let mut buffer = self
            .shared
            .buffer
            .lock()
            .map_err(|_| NativeHostInputError::Poisoned)?;
        let count = max_events.min(buffer.events.len());
        let mut drained = Vec::with_capacity(count);
        for _ in 0..count {
            let event = buffer
                .events
                .pop_front()
                .expect("drain count is bounded by queue length");
            buffer.text_bytes -= event.kind.payload_bytes();
            drained.push(event);
        }
        Ok(drained)
    }

    pub fn stats(&self) -> Result<NativeInputStats, NativeHostInputError> {
        let buffer = self
            .shared
            .buffer
            .lock()
            .map_err(|_| NativeHostInputError::Poisoned)?;
        Ok(NativeInputStats {
            pending_events: buffer.events.len(),
            pending_text_bytes: buffer.text_bytes,
            dropped_samples: self.shared.dropped_samples.load(Ordering::Relaxed),
            fault: current_fault(&self.shared),
        })
    }

    pub fn clear_fault_after_drain(&self) -> Result<(), NativeHostInputError> {
        let buffer = self
            .shared
            .buffer
            .lock()
            .map_err(|_| NativeHostInputError::Poisoned)?;
        if !buffer.events.is_empty() {
            return Err(NativeHostInputError::ReliableOverflow);
        }
        drop(buffer);
        self.shared.fault.store(FAULT_NONE, Ordering::Release);
        Ok(())
    }
}

fn current_fault(shared: &SharedInputChannel) -> Option<NativeHostInputError> {
    match shared.fault.load(Ordering::Acquire) {
        FAULT_NONE => None,
        FAULT_RELIABLE_OVERFLOW => Some(NativeHostInputError::ReliableOverflow),
        FAULT_SEQUENCE_EXHAUSTED => Some(NativeHostInputError::SequenceExhausted),
        FAULT_POISONED => Some(NativeHostInputError::Poisoned),
        _ => Some(NativeHostInputError::Poisoned),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_app_protocol::GenerationalHandle;

    fn window() -> WindowHandle {
        GenerationalHandle {
            index: 1,
            generation: 1,
        }
    }

    fn view() -> ViewHandle {
        GenerationalHandle {
            index: 2,
            generation: 1,
        }
    }

    #[test]
    fn file_drop_paths_are_charged_to_the_reliable_byte_budget() {
        let (channel, receiver) = NativeInputChannel::bounded(NativeInputChannelConfig {
            max_events: 4,
            max_text_bytes: 8,
        })
        .unwrap();
        channel
            .publish(
                1,
                window(),
                view(),
                NativeInputKind::FileDropped {
                    x_milli: 0,
                    y_milli: 0,
                    paths: vec![String::from("12345678")],
                },
            )
            .unwrap();
        assert_eq!(receiver.stats().unwrap().pending_text_bytes, 8);
        assert_eq!(
            channel.publish(
                2,
                window(),
                view(),
                NativeInputKind::FileDragEntered {
                    x_milli: 0,
                    y_milli: 0,
                    paths: vec![String::from("x")],
                },
            ),
            Err(NativeHostInputError::ReliableOverflow)
        );
    }

    #[test]
    fn file_drag_moves_coalesce_to_the_latest_sample() {
        let (channel, receiver) = NativeInputChannel::bounded(NativeInputChannelConfig {
            max_events: 1,
            max_text_bytes: 8,
        })
        .unwrap();
        for point in [1, 2] {
            channel
                .publish(
                    point,
                    window(),
                    view(),
                    NativeInputKind::FileDragMoved {
                        x_milli: point as i32,
                        y_milli: point as i32,
                    },
                )
                .unwrap();
        }
        let stats = receiver.stats().unwrap();
        assert_eq!(stats.pending_events, 1);
        assert_eq!(stats.dropped_samples, 1);
        let event = receiver.drain(1).unwrap().pop().unwrap();
        assert!(matches!(
            event.kind,
            NativeInputKind::FileDragMoved {
                x_milli: 2,
                y_milli: 2
            }
        ));
    }
}
