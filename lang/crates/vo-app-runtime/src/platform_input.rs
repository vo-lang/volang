use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use vo_app_protocol::{SurfaceHandle, ViewHandle, WindowHandle};

use crate::{
    ArbitrationEvent, ArbitrationResult, CompositionError, CompositionPointerId,
    CompositionRegistry, GraphicsSurfaceLease, SurfaceCloseReport, ViewInputReleaseReport,
};

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct InputDeviceId {
    pub value: u64,
    pub generation: u32,
}

impl InputDeviceId {
    pub const fn is_valid(self) -> bool {
        self.value != 0 && self.generation != 0
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InputDeviceKind {
    Mouse,
    Touch,
    Pen,
    Keyboard,
    Gamepad,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct InputModifiers {
    pub shift: bool,
    pub control: bool,
    pub alt: bool,
    pub meta: bool,
    pub caps_lock: bool,
    pub num_lock: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum KeyPhase {
    Down,
    Up,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PointerPhase {
    Down,
    Move,
    Up,
    Cancel,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WheelUnit {
    Pixel,
    Line,
    Page,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompositionPhase {
    Start,
    Update,
    End,
    Cancel,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum GamepadMapping {
    Standard,
    Raw,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct GamepadButton {
    pub value_q15: u16,
    pub pressed: bool,
    pub touched: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlatformInputHeader {
    pub sequence: u64,
    pub timestamp_micros: u64,
    pub metrics_revision: u64,
    pub window: WindowHandle,
    pub view: ViewHandle,
    pub device: InputDeviceId,
    pub device_kind: InputDeviceKind,
    pub modifiers: InputModifiers,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PlatformInputPayload {
    Pointer {
        contact: u32,
        phase: PointerPhase,
        x_milli: i32,
        y_milli: i32,
        delta_x_milli: i32,
        delta_y_milli: i32,
        pressure_q15: u16,
        tilt_x_degrees: i16,
        tilt_y_degrees: i16,
        buttons: u32,
        changed_button: Option<u8>,
    },
    Wheel {
        contact: u32,
        x_milli: i32,
        y_milli: i32,
        delta_x_milli: i32,
        delta_y_milli: i32,
        unit: WheelUnit,
    },
    Key {
        phase: KeyPhase,
        physical_key: u32,
        logical_key: String,
        repeat: bool,
    },
    Shortcut {
        class_mask: u64,
        system: bool,
    },
    Text {
        text: String,
    },
    Composition {
        phase: CompositionPhase,
        text: String,
        selection_start: u32,
        selection_end: u32,
    },
    GamepadSnapshot {
        connected: bool,
        mapping: GamepadMapping,
        axes_q15: Vec<i16>,
        buttons: Vec<GamepadButton>,
    },
    FocusChanged {
        focused: bool,
    },
    VisibilityChanged {
        visible: bool,
    },
    DeviceDisconnected,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlatformInputEvent {
    pub header: PlatformInputHeader,
    pub payload: PlatformInputPayload,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SynthesizedInputRelease {
    PointerCancel {
        pointer: CompositionPointerId,
        surface: SurfaceHandle,
    },
    KeyUp {
        device: InputDeviceId,
        physical_key: u32,
        surface: SurfaceHandle,
    },
    GamepadDisconnected {
        device: InputDeviceId,
        surface: SurfaceHandle,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlatformInputRoutingReport {
    pub event: PlatformInputEvent,
    pub arbitration: Option<ArbitrationResult>,
    pub synthesized_releases: Vec<SynthesizedInputRelease>,
    pub view_release: Option<ViewInputReleaseReport>,
    pub composition_revision: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SurfaceInputCloseReport {
    pub closed: SurfaceCloseReport,
    pub synthesized_releases: Vec<SynthesizedInputRelease>,
    pub released_graphics_surface: Option<GraphicsSurfaceLease>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PlatformInputError {
    InvalidDevice,
    InvalidSequence,
    InvalidTimestamp,
    InvalidPointer,
    InvalidPressure,
    InvalidTilt,
    InvalidTextSelection,
    InvalidShortcut,
    InvalidWindow,
    Capacity,
    Composition(CompositionError),
}

pub struct PlatformInputRouter {
    max_active_bindings: usize,
    last_timestamp_micros: u64,
    device_sequences: BTreeMap<InputDeviceId, u64>,
    active_pointers: BTreeMap<CompositionPointerId, SurfaceHandle>,
    active_keys: BTreeMap<(InputDeviceId, u32), SurfaceHandle>,
    active_gamepads: BTreeMap<InputDeviceId, SurfaceHandle>,
}

impl PlatformInputRouter {
    pub fn new(max_active_bindings: usize) -> Result<Self, PlatformInputError> {
        if max_active_bindings == 0 {
            return Err(PlatformInputError::Capacity);
        }
        Ok(Self {
            max_active_bindings,
            last_timestamp_micros: 0,
            device_sequences: BTreeMap::new(),
            active_pointers: BTreeMap::new(),
            active_keys: BTreeMap::new(),
            active_gamepads: BTreeMap::new(),
        })
    }

    pub fn route(
        &mut self,
        composition: &mut CompositionRegistry,
        event: PlatformInputEvent,
    ) -> Result<PlatformInputRoutingReport, PlatformInputError> {
        self.validate_event(composition, &event)?;
        self.ensure_capacity_for_event(&event)?;
        self.commit_order(&event.header)?;

        if matches!(&event.payload, PlatformInputPayload::DeviceDisconnected) {
            let synthesized_releases = self.release_device(composition, event.header.device)?;
            return Ok(PlatformInputRoutingReport {
                event,
                arbitration: None,
                synthesized_releases,
                view_release: None,
                composition_revision: composition.revision(),
            });
        }

        if matches!(
            &event.payload,
            PlatformInputPayload::FocusChanged { focused: false }
                | PlatformInputPayload::VisibilityChanged { visible: false }
        ) {
            let synthesized_releases = self.release_view(composition, event.header.view)?;
            let view_release = composition
                .suspend_view_input(event.header.view, composition.revision())
                .map_err(PlatformInputError::Composition)?;
            return Ok(PlatformInputRoutingReport {
                event,
                arbitration: None,
                synthesized_releases,
                composition_revision: view_release.revision,
                view_release: Some(view_release),
            });
        }

        let arbitration_event = self.arbitration_event(composition, &event)?;
        let arbitration = composition
            .arbitrate(event.header.view, arbitration_event)
            .map_err(PlatformInputError::Composition)?;
        self.track_active_binding(&event, &arbitration)?;
        Ok(PlatformInputRoutingReport {
            event,
            composition_revision: arbitration.revision,
            arbitration: Some(arbitration),
            synthesized_releases: Vec::new(),
            view_release: None,
        })
    }

    pub fn release_surface(&mut self, surface: SurfaceHandle) -> Vec<SynthesizedInputRelease> {
        let mut releases = Vec::new();
        let pointers = self
            .active_pointers
            .iter()
            .filter_map(|(pointer, owner)| (*owner == surface).then_some(*pointer))
            .collect::<Vec<_>>();
        for pointer in pointers {
            self.active_pointers.remove(&pointer);
            releases.push(SynthesizedInputRelease::PointerCancel { pointer, surface });
        }
        let keys = self
            .active_keys
            .iter()
            .filter_map(|(key, owner)| (*owner == surface).then_some(*key))
            .collect::<Vec<_>>();
        for (device, physical_key) in keys {
            self.active_keys.remove(&(device, physical_key));
            releases.push(SynthesizedInputRelease::KeyUp {
                device,
                physical_key,
                surface,
            });
        }
        let gamepads = self
            .active_gamepads
            .iter()
            .filter_map(|(device, owner)| (*owner == surface).then_some(*device))
            .collect::<Vec<_>>();
        for device in gamepads {
            self.active_gamepads.remove(&device);
            releases.push(SynthesizedInputRelease::GamepadDisconnected { device, surface });
        }
        releases
    }

    pub fn release_all(&mut self) -> Vec<SynthesizedInputRelease> {
        let mut surfaces = self
            .active_pointers
            .values()
            .chain(self.active_keys.values())
            .chain(self.active_gamepads.values())
            .copied()
            .collect::<Vec<_>>();
        surfaces.sort();
        surfaces.dedup();
        let mut releases = Vec::new();
        for surface in surfaces {
            releases.extend(self.release_surface(surface));
        }
        self.device_sequences.clear();
        self.last_timestamp_micros = 0;
        releases
    }

    pub fn active_binding_count(&self) -> usize {
        self.active_pointers.len() + self.active_keys.len() + self.active_gamepads.len()
    }

    pub fn clear(&mut self) {
        self.device_sequences.clear();
        self.active_pointers.clear();
        self.active_keys.clear();
        self.active_gamepads.clear();
        self.last_timestamp_micros = 0;
    }

    fn validate_event(
        &self,
        composition: &CompositionRegistry,
        event: &PlatformInputEvent,
    ) -> Result<(), PlatformInputError> {
        let header = &event.header;
        if !header.device.is_valid() {
            return Err(PlatformInputError::InvalidDevice);
        }
        let window = composition
            .view_window(header.view)
            .map_err(PlatformInputError::Composition)?;
        if window != header.window {
            return Err(PlatformInputError::InvalidWindow);
        }
        if composition
            .view_metrics(header.view)
            .map_err(PlatformInputError::Composition)?
            .revision
            != header.metrics_revision
        {
            return Err(PlatformInputError::Composition(
                CompositionError::RevisionConflict,
            ));
        }
        match &event.payload {
            PlatformInputPayload::Pointer {
                contact,
                pressure_q15,
                tilt_x_degrees,
                tilt_y_degrees,
                ..
            } => {
                if *contact == u32::MAX {
                    return Err(PlatformInputError::InvalidPointer);
                }
                if *pressure_q15 > 32_768 {
                    return Err(PlatformInputError::InvalidPressure);
                }
                if !(-90..=90).contains(tilt_x_degrees) || !(-90..=90).contains(tilt_y_degrees) {
                    return Err(PlatformInputError::InvalidTilt);
                }
            }
            PlatformInputPayload::Wheel { contact, .. } if *contact == u32::MAX => {
                return Err(PlatformInputError::InvalidPointer);
            }
            PlatformInputPayload::Composition {
                text,
                selection_start,
                selection_end,
                ..
            } if selection_start > selection_end
                || usize::try_from(*selection_end).map_or(true, |end| end > text.len()) =>
            {
                return Err(PlatformInputError::InvalidTextSelection);
            }
            PlatformInputPayload::Shortcut { class_mask: 0, .. } => {
                return Err(PlatformInputError::InvalidShortcut);
            }
            _ => {}
        }
        Ok(())
    }

    fn commit_order(&mut self, header: &PlatformInputHeader) -> Result<(), PlatformInputError> {
        if header.sequence == 0
            || self
                .device_sequences
                .get(&header.device)
                .is_some_and(|last| header.sequence <= *last)
        {
            return Err(PlatformInputError::InvalidSequence);
        }
        if header.timestamp_micros < self.last_timestamp_micros {
            return Err(PlatformInputError::InvalidTimestamp);
        }
        self.device_sequences.insert(header.device, header.sequence);
        self.last_timestamp_micros = header.timestamp_micros;
        Ok(())
    }

    fn ensure_capacity_for_event(
        &self,
        event: &PlatformInputEvent,
    ) -> Result<(), PlatformInputError> {
        let needs_binding = match &event.payload {
            PlatformInputPayload::Pointer {
                contact,
                phase: PointerPhase::Down,
                ..
            } => !self.active_pointers.contains_key(&CompositionPointerId {
                device: event.header.device.value,
                device_generation: event.header.device.generation,
                contact: *contact,
            }),
            PlatformInputPayload::Key {
                phase: KeyPhase::Down,
                physical_key,
                repeat: false,
                ..
            } => !self
                .active_keys
                .contains_key(&(event.header.device, *physical_key)),
            PlatformInputPayload::GamepadSnapshot {
                connected: true, ..
            } => !self.active_gamepads.contains_key(&event.header.device),
            _ => false,
        };
        if needs_binding {
            self.reserve_binding()?;
        }
        Ok(())
    }

    fn arbitration_event(
        &self,
        composition: &CompositionRegistry,
        event: &PlatformInputEvent,
    ) -> Result<ArbitrationEvent, PlatformInputError> {
        let pointer = |contact| CompositionPointerId {
            device: event.header.device.value,
            device_generation: event.header.device.generation,
            contact,
        };
        match &event.payload {
            PlatformInputPayload::Pointer {
                contact,
                phase: PointerPhase::Up | PointerPhase::Cancel,
                ..
            } => {
                let pointer = pointer(*contact);
                Ok(ArbitrationEvent::PointerFor {
                    pointer,
                    hit: self.active_pointers.get(&pointer).copied(),
                })
            }
            PlatformInputPayload::Pointer {
                contact,
                x_milli,
                y_milli,
                ..
            }
            | PlatformInputPayload::Wheel {
                contact,
                x_milli,
                y_milli,
                ..
            } => Ok(ArbitrationEvent::PointerStackFor {
                pointer: pointer(*contact),
                hits: composition
                    .hit_test_stack(event.header.view, *x_milli, *y_milli)
                    .map_err(PlatformInputError::Composition)?,
            }),
            PlatformInputPayload::Key {
                phase: KeyPhase::Up,
                physical_key,
                ..
            } => Ok(ArbitrationEvent::KeyboardFor {
                target: self
                    .active_keys
                    .get(&(event.header.device, *physical_key))
                    .copied(),
            }),
            PlatformInputPayload::Key { .. } => Ok(ArbitrationEvent::Keyboard),
            PlatformInputPayload::Shortcut {
                class_mask,
                system: true,
            } => Ok(ArbitrationEvent::SystemShortcut {
                class_mask: *class_mask,
            }),
            PlatformInputPayload::Shortcut { .. } => Ok(ArbitrationEvent::Shortcut),
            PlatformInputPayload::Text { .. } | PlatformInputPayload::Composition { .. } => {
                Ok(ArbitrationEvent::Text)
            }
            PlatformInputPayload::GamepadSnapshot {
                connected: false, ..
            } => Ok(ArbitrationEvent::GamepadFor {
                target: self.active_gamepads.get(&event.header.device).copied(),
            }),
            PlatformInputPayload::GamepadSnapshot { .. } => Ok(ArbitrationEvent::Gamepad),
            PlatformInputPayload::FocusChanged { .. }
            | PlatformInputPayload::VisibilityChanged { .. } => Ok(ArbitrationEvent::Keyboard),
            PlatformInputPayload::DeviceDisconnected => unreachable!(),
        }
    }

    fn track_active_binding(
        &mut self,
        event: &PlatformInputEvent,
        arbitration: &ArbitrationResult,
    ) -> Result<(), PlatformInputError> {
        let target = arbitration.consumed_by.or_else(|| {
            arbitration
                .deliveries
                .iter()
                .find(|delivery| !delivery.observed)
                .map(|delivery| delivery.surface)
        });
        match &event.payload {
            PlatformInputPayload::Pointer { contact, phase, .. } => {
                let pointer = CompositionPointerId {
                    device: event.header.device.value,
                    device_generation: event.header.device.generation,
                    contact: *contact,
                };
                match phase {
                    PointerPhase::Down => {
                        if let Some(target) = target {
                            if !self.active_pointers.contains_key(&pointer) {
                                self.reserve_binding()?;
                            }
                            self.active_pointers.insert(pointer, target);
                        }
                    }
                    PointerPhase::Up | PointerPhase::Cancel => {
                        self.active_pointers.remove(&pointer);
                    }
                    PointerPhase::Move => {}
                }
            }
            PlatformInputPayload::Key {
                phase,
                physical_key,
                repeat,
                ..
            } => match phase {
                KeyPhase::Down if !repeat => {
                    if let Some(target) = target {
                        let key = (event.header.device, *physical_key);
                        if !self.active_keys.contains_key(&key) {
                            self.reserve_binding()?;
                        }
                        self.active_keys.insert(key, target);
                    }
                }
                KeyPhase::Up => {
                    self.active_keys
                        .remove(&(event.header.device, *physical_key));
                }
                KeyPhase::Down => {}
            },
            PlatformInputPayload::GamepadSnapshot { connected, .. } => {
                if *connected {
                    if let Some(target) = target {
                        if !self.active_gamepads.contains_key(&event.header.device) {
                            self.reserve_binding()?;
                        }
                        self.active_gamepads.insert(event.header.device, target);
                    }
                } else {
                    self.active_gamepads.remove(&event.header.device);
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn reserve_binding(&self) -> Result<(), PlatformInputError> {
        if self.active_binding_count() >= self.max_active_bindings {
            Err(PlatformInputError::Capacity)
        } else {
            Ok(())
        }
    }

    fn release_device(
        &mut self,
        composition: &mut CompositionRegistry,
        device: InputDeviceId,
    ) -> Result<Vec<SynthesizedInputRelease>, PlatformInputError> {
        let mut releases = Vec::new();
        let pointers = self
            .active_pointers
            .iter()
            .filter_map(|(pointer, surface)| {
                (pointer.device == device.value && pointer.device_generation == device.generation)
                    .then_some((*pointer, *surface))
            })
            .collect::<Vec<_>>();
        for (pointer, surface) in pointers {
            self.active_pointers.remove(&pointer);
            releases.push(SynthesizedInputRelease::PointerCancel { pointer, surface });
        }
        let keys = self
            .active_keys
            .iter()
            .filter_map(|((candidate, physical_key), surface)| {
                (*candidate == device).then_some((*physical_key, *surface))
            })
            .collect::<Vec<_>>();
        for (physical_key, surface) in keys {
            self.active_keys.remove(&(device, physical_key));
            releases.push(SynthesizedInputRelease::KeyUp {
                device,
                physical_key,
                surface,
            });
        }
        if let Some(surface) = self.active_gamepads.remove(&device) {
            releases.push(SynthesizedInputRelease::GamepadDisconnected { device, surface });
        }
        for (pointer, surface) in composition
            .release_device_pointer_captures(device.value, device.generation)
            .map_err(PlatformInputError::Composition)?
        {
            let already_present = releases.iter().any(|release| {
                matches!(
                    release,
                    SynthesizedInputRelease::PointerCancel {
                        pointer: candidate,
                        surface: owner,
                    } if *candidate == pointer && *owner == surface
                )
            });
            if !already_present {
                releases.push(SynthesizedInputRelease::PointerCancel { pointer, surface });
            }
        }
        self.device_sequences.remove(&device);
        Ok(releases)
    }

    fn release_view(
        &mut self,
        composition: &CompositionRegistry,
        view: ViewHandle,
    ) -> Result<Vec<SynthesizedInputRelease>, PlatformInputError> {
        let belongs_to_view = |surface| {
            composition
                .surface_descriptor(surface)
                .map(|descriptor| descriptor.view == view)
                .map_err(PlatformInputError::Composition)
        };
        let mut releases = Vec::new();
        let pointers = self
            .active_pointers
            .iter()
            .map(|(pointer, surface)| Ok((*pointer, *surface, belongs_to_view(*surface)?)))
            .collect::<Result<Vec<_>, PlatformInputError>>()?;
        for (pointer, surface, belongs) in pointers {
            if belongs {
                self.active_pointers.remove(&pointer);
                releases.push(SynthesizedInputRelease::PointerCancel { pointer, surface });
            }
        }
        let keys = self
            .active_keys
            .iter()
            .map(|(key, surface)| Ok((*key, *surface, belongs_to_view(*surface)?)))
            .collect::<Result<Vec<_>, PlatformInputError>>()?;
        for ((device, physical_key), surface, belongs) in keys {
            if belongs {
                self.active_keys.remove(&(device, physical_key));
                releases.push(SynthesizedInputRelease::KeyUp {
                    device,
                    physical_key,
                    surface,
                });
            }
        }
        let gamepads = self
            .active_gamepads
            .iter()
            .map(|(device, surface)| Ok((*device, *surface, belongs_to_view(*surface)?)))
            .collect::<Result<Vec<_>, PlatformInputError>>()?;
        for (device, surface, belongs) in gamepads {
            if belongs {
                self.active_gamepads.remove(&device);
                releases.push(SynthesizedInputRelease::GamepadDisconnected { device, surface });
            }
        }
        for (pointer, surface) in composition
            .view_pointer_captures(view)
            .map_err(PlatformInputError::Composition)?
        {
            let already_present = releases.iter().any(|release| {
                matches!(
                    release,
                    SynthesizedInputRelease::PointerCancel {
                        pointer: candidate,
                        surface: owner,
                    } if *candidate == pointer && *owner == surface
                )
            });
            if !already_present {
                releases.push(SynthesizedInputRelease::PointerCancel { pointer, surface });
            }
        }
        Ok(releases)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        CompositionLimits, SurfaceDescriptor, SurfaceGeometry, SurfaceInputPolicy, SurfaceKind,
    };
    use alloc::{string::String, vec};
    use vo_app_protocol::SessionHandle;

    struct Fixture {
        composition: CompositionRegistry,
        router: PlatformInputRouter,
        window: WindowHandle,
        view: ViewHandle,
        surface: SurfaceHandle,
    }

    impl Fixture {
        fn new(max_active_bindings: usize) -> Self {
            let mut composition = CompositionRegistry::new(
                SessionHandle {
                    index: 1,
                    generation: 1,
                },
                1,
                CompositionLimits::default(),
            )
            .expect("composition");
            let window = composition.create_window().expect("window");
            let view = composition.create_view(window).expect("view");
            let surface = composition
                .attach_surface(SurfaceDescriptor {
                    view,
                    kind: SurfaceKind::Game,
                    z_order: 0,
                    input: SurfaceInputPolicy::Interactive,
                    accepts_text: true,
                    geometry: SurfaceGeometry::default(),
                })
                .expect("surface");
            composition
                .set_focus(view, Some(surface), composition.revision())
                .expect("focus");
            Self {
                composition,
                router: PlatformInputRouter::new(max_active_bindings).expect("router"),
                window,
                view,
                surface,
            }
        }

        fn event(
            &self,
            device: InputDeviceId,
            device_kind: InputDeviceKind,
            sequence: u64,
            timestamp_micros: u64,
            payload: PlatformInputPayload,
        ) -> PlatformInputEvent {
            PlatformInputEvent {
                header: PlatformInputHeader {
                    sequence,
                    timestamp_micros,
                    metrics_revision: 1,
                    window: self.window,
                    view: self.view,
                    device,
                    device_kind,
                    modifiers: InputModifiers::default(),
                },
                payload,
            }
        }

        fn route(
            &mut self,
            event: PlatformInputEvent,
        ) -> Result<PlatformInputRoutingReport, PlatformInputError> {
            self.router.route(&mut self.composition, event)
        }
    }

    #[test]
    fn focus_loss_synthesizes_pointer_key_and_gamepad_releases() {
        let mut fixture = Fixture::new(8);
        let pointer_device = InputDeviceId {
            value: 1,
            generation: 1,
        };
        let key_device = InputDeviceId {
            value: 2,
            generation: 1,
        };
        let gamepad_device = InputDeviceId {
            value: 3,
            generation: 1,
        };

        let pointer = fixture.event(
            pointer_device,
            InputDeviceKind::Mouse,
            1,
            1,
            PlatformInputPayload::Pointer {
                contact: 7,
                phase: PointerPhase::Down,
                x_milli: 10,
                y_milli: 20,
                delta_x_milli: 0,
                delta_y_milli: 0,
                pressure_q15: 16_384,
                tilt_x_degrees: 0,
                tilt_y_degrees: 0,
                buttons: 1,
                changed_button: Some(0),
            },
        );
        assert_eq!(
            fixture
                .route(pointer)
                .expect("pointer")
                .arbitration
                .unwrap()
                .consumed_by,
            Some(fixture.surface)
        );
        let key = fixture.event(
            key_device,
            InputDeviceKind::Keyboard,
            1,
            2,
            PlatformInputPayload::Key {
                phase: KeyPhase::Down,
                physical_key: 4,
                logical_key: String::from("a"),
                repeat: false,
            },
        );
        fixture.route(key).expect("key");
        let gamepad = fixture.event(
            gamepad_device,
            InputDeviceKind::Gamepad,
            1,
            3,
            PlatformInputPayload::GamepadSnapshot {
                connected: true,
                mapping: GamepadMapping::Standard,
                axes_q15: vec![16_384],
                buttons: vec![GamepadButton {
                    value_q15: u16::MAX,
                    pressed: true,
                    touched: true,
                }],
            },
        );
        fixture.route(gamepad).expect("gamepad");
        assert_eq!(fixture.router.active_binding_count(), 3);

        let focus_loss = fixture.event(
            InputDeviceId {
                value: 4,
                generation: 1,
            },
            InputDeviceKind::Keyboard,
            1,
            4,
            PlatformInputPayload::FocusChanged { focused: false },
        );
        let report = fixture.route(focus_loss).expect("focus loss");
        assert_eq!(
            report.synthesized_releases,
            vec![
                SynthesizedInputRelease::PointerCancel {
                    pointer: CompositionPointerId {
                        device: pointer_device.value,
                        device_generation: pointer_device.generation,
                        contact: 7,
                    },
                    surface: fixture.surface,
                },
                SynthesizedInputRelease::KeyUp {
                    device: key_device,
                    physical_key: 4,
                    surface: fixture.surface,
                },
                SynthesizedInputRelease::GamepadDisconnected {
                    device: gamepad_device,
                    surface: fixture.surface,
                },
            ]
        );
        assert!(report.view_release.is_some());
        assert_eq!(fixture.router.active_binding_count(), 0);
    }

    #[test]
    fn rejected_event_does_not_consume_sequence_or_binding_capacity() {
        let mut fixture = Fixture::new(1);
        let device = InputDeviceId {
            value: 11,
            generation: 2,
        };
        let down = fixture.event(
            device,
            InputDeviceKind::Keyboard,
            1,
            10,
            PlatformInputPayload::Key {
                phase: KeyPhase::Down,
                physical_key: 40,
                logical_key: String::from("enter"),
                repeat: false,
            },
        );
        fixture.route(down).expect("key down");
        assert_eq!(fixture.router.active_binding_count(), 1);

        let rejected = fixture.event(
            device,
            InputDeviceKind::Keyboard,
            2,
            11,
            PlatformInputPayload::Key {
                phase: KeyPhase::Down,
                physical_key: 41,
                logical_key: String::from("space"),
                repeat: false,
            },
        );
        assert_eq!(fixture.route(rejected), Err(PlatformInputError::Capacity));
        assert_eq!(fixture.router.active_binding_count(), 1);

        let up = fixture.event(
            device,
            InputDeviceKind::Keyboard,
            2,
            11,
            PlatformInputPayload::Key {
                phase: KeyPhase::Up,
                physical_key: 40,
                logical_key: String::from("enter"),
                repeat: false,
            },
        );
        fixture.route(up).expect("sequence remains available");
        assert_eq!(fixture.router.active_binding_count(), 0);
    }
}
