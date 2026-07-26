use vo_app_runtime::{
    AppSession, CompositionPhase, InputDeviceId, InputDeviceKind, InputModifiers, KeyPhase,
    PlatformInputEvent, PlatformInputHeader, PlatformInputPayload, PointerPhase,
    ViewInputReleaseReport, ViewMetricsUpdate, WheelUnit,
};

use crate::{NativeInputEvent, NativeInputKind, NativeModifiers, NativePointerButton};

const POINTER_DEVICE_NAMESPACE: u64 = 0x1000_0000_0000_0000;
const KEYBOARD_DEVICE_NAMESPACE: u64 = 0x2000_0000_0000_0000;
const LIFECYCLE_DEVICE: u64 = 0x3000_0000_0000_0001;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeInputRoute {
    pub source_sequence: u64,
    pub arbitration: Option<vo_app_runtime::ArbitrationResult>,
    pub released: Option<ViewInputReleaseReport>,
    pub synthesized_release_count: usize,
    pub final_composition_revision: u64,
    pub metrics_revision: u64,
}

pub fn route_native_input(
    session: &AppSession,
    event: &NativeInputEvent,
) -> Result<NativeInputRoute, String> {
    if let NativeInputKind::Resized {
        width_milli,
        height_milli,
        scale_milli,
    } = event.kind
    {
        let current = session.host_view_metrics(event.view)?;
        let scale_q16 = u32::try_from(
            u64::from(scale_milli)
                .saturating_mul(65_536)
                .saturating_div(1_000),
        )
        .unwrap_or(u32::MAX);
        let framebuffer_width = scaled_pixels(width_milli, scale_milli);
        let framebuffer_height = scaled_pixels(height_milli, scale_milli);
        let metrics = session.update_host_view_metrics(
            event.view,
            ViewMetricsUpdate {
                origin_x_milli: current.origin_x_milli,
                origin_y_milli: current.origin_y_milli,
                width_milli,
                height_milli,
                framebuffer_width,
                framebuffer_height,
                scale_q16,
                safe_area: current.safe_area,
                visibility: current.visibility,
            },
            current.revision,
        )?;
        return Ok(NativeInputRoute {
            source_sequence: event.sequence,
            arbitration: None,
            released: None,
            synthesized_release_count: 0,
            final_composition_revision: session.host_composition_revision()?,
            metrics_revision: metrics.revision,
        });
    }

    if matches!(event.kind, NativeInputKind::CloseRequested) {
        let metrics = session.host_view_metrics(event.view)?;
        let report = session.route_host_platform_input(PlatformInputEvent {
            header: input_header(
                event,
                metrics.revision,
                LIFECYCLE_DEVICE,
                InputDeviceKind::Keyboard,
                InputModifiers::default(),
            ),
            payload: PlatformInputPayload::VisibilityChanged { visible: false },
        })?;
        return Ok(route_report(event, metrics.revision, report));
    }

    let metrics = session.host_view_metrics(event.view)?;
    let Some((device, device_kind, modifiers, payload)) = normalize_input(event)? else {
        return Ok(NativeInputRoute {
            source_sequence: event.sequence,
            arbitration: None,
            released: None,
            synthesized_release_count: 0,
            final_composition_revision: session.host_composition_revision()?,
            metrics_revision: metrics.revision,
        });
    };
    let report = session.route_host_platform_input(PlatformInputEvent {
        header: input_header(event, metrics.revision, device, device_kind, modifiers),
        payload,
    })?;
    Ok(route_report(event, metrics.revision, report))
}

fn normalize_input(
    event: &NativeInputEvent,
) -> Result<Option<(u64, InputDeviceKind, InputModifiers, PlatformInputPayload)>, String> {
    let normalized = match &event.kind {
        NativeInputKind::PointerMoved {
            device,
            x_milli,
            y_milli,
            delta_x_milli,
            delta_y_milli,
            pressure_milli,
        } => (
            pointer_device(*device),
            InputDeviceKind::Mouse,
            InputModifiers::default(),
            PlatformInputPayload::Pointer {
                contact: 0,
                phase: PointerPhase::Move,
                x_milli: *x_milli,
                y_milli: *y_milli,
                delta_x_milli: *delta_x_milli,
                delta_y_milli: *delta_y_milli,
                pressure_q15: q15_from_milli(*pressure_milli),
                tilt_x_degrees: 0,
                tilt_y_degrees: 0,
                buttons: 0,
                changed_button: None,
            },
        ),
        NativeInputKind::PointerButton {
            device,
            button,
            pressed,
            x_milli,
            y_milli,
            ..
        } => {
            let button = pointer_button(*button)?;
            (
                pointer_device(*device),
                InputDeviceKind::Mouse,
                InputModifiers::default(),
                PlatformInputPayload::Pointer {
                    contact: 0,
                    phase: if *pressed {
                        PointerPhase::Down
                    } else {
                        PointerPhase::Up
                    },
                    x_milli: *x_milli,
                    y_milli: *y_milli,
                    delta_x_milli: 0,
                    delta_y_milli: 0,
                    pressure_q15: if *pressed { u16::MAX } else { 0 },
                    tilt_x_degrees: 0,
                    tilt_y_degrees: 0,
                    buttons: if *pressed { 1_u32 << button } else { 0 },
                    changed_button: Some(button),
                },
            )
        }
        NativeInputKind::Wheel {
            device,
            x_milli,
            y_milli,
            delta_x_milli,
            delta_y_milli,
            unit,
        } => (
            pointer_device(*device),
            InputDeviceKind::Mouse,
            InputModifiers::default(),
            PlatformInputPayload::Wheel {
                contact: 0,
                x_milli: *x_milli,
                y_milli: *y_milli,
                delta_x_milli: *delta_x_milli,
                delta_y_milli: *delta_y_milli,
                unit: match unit {
                    crate::NativeScrollUnit::Pixel => WheelUnit::Pixel,
                    crate::NativeScrollUnit::Line => WheelUnit::Line,
                    crate::NativeScrollUnit::Page => WheelUnit::Page,
                },
            },
        ),
        NativeInputKind::GamepadSnapshot {
            device,
            connected,
            standard_mapping,
            axes_q15,
            buttons,
        } => (
            *device,
            InputDeviceKind::Gamepad,
            InputModifiers::default(),
            PlatformInputPayload::GamepadSnapshot {
                connected: *connected,
                mapping: if *standard_mapping {
                    vo_app_runtime::GamepadMapping::Standard
                } else {
                    vo_app_runtime::GamepadMapping::Raw
                },
                axes_q15: axes_q15.clone(),
                buttons: buttons.clone(),
            },
        ),
        NativeInputKind::DeviceDisconnected { device, kind } => (
            match kind {
                InputDeviceKind::Mouse | InputDeviceKind::Touch | InputDeviceKind::Pen => {
                    pointer_device(*device)
                }
                InputDeviceKind::Keyboard => keyboard_device(*device),
                InputDeviceKind::Gamepad => *device,
            },
            *kind,
            InputModifiers::default(),
            PlatformInputPayload::DeviceDisconnected,
        ),
        NativeInputKind::Key {
            device,
            physical_key,
            logical_key,
            pressed,
            repeat,
            modifiers,
        } => (
            keyboard_device(*device),
            InputDeviceKind::Keyboard,
            input_modifiers(*modifiers),
            PlatformInputPayload::Key {
                phase: if *pressed {
                    KeyPhase::Down
                } else {
                    KeyPhase::Up
                },
                physical_key: *physical_key,
                logical_key: logical_key.clone(),
                repeat: *repeat,
            },
        ),
        NativeInputKind::Text(text) => (
            keyboard_device(1),
            InputDeviceKind::Keyboard,
            InputModifiers::default(),
            PlatformInputPayload::Text { text: text.clone() },
        ),
        NativeInputKind::ImeStarted => (
            keyboard_device(1),
            InputDeviceKind::Keyboard,
            InputModifiers::default(),
            PlatformInputPayload::Composition {
                phase: CompositionPhase::Start,
                text: String::new(),
                selection_start: 0,
                selection_end: 0,
            },
        ),
        NativeInputKind::ImeUpdated {
            text,
            selection_start_utf16,
            selection_len_utf16,
        } => (
            keyboard_device(1),
            InputDeviceKind::Keyboard,
            InputModifiers::default(),
            PlatformInputPayload::Composition {
                phase: CompositionPhase::Update,
                text: text.clone(),
                selection_start: *selection_start_utf16,
                selection_end: selection_start_utf16.saturating_add(*selection_len_utf16),
            },
        ),
        NativeInputKind::ImeCommitted(text) => (
            keyboard_device(1),
            InputDeviceKind::Keyboard,
            InputModifiers::default(),
            PlatformInputPayload::Composition {
                phase: CompositionPhase::End,
                text: text.clone(),
                selection_start: u32::try_from(text.encode_utf16().count()).unwrap_or(u32::MAX),
                selection_end: u32::try_from(text.encode_utf16().count()).unwrap_or(u32::MAX),
            },
        ),
        NativeInputKind::ImeCancelled => (
            keyboard_device(1),
            InputDeviceKind::Keyboard,
            InputModifiers::default(),
            PlatformInputPayload::Composition {
                phase: CompositionPhase::Cancel,
                text: String::new(),
                selection_start: 0,
                selection_end: 0,
            },
        ),
        NativeInputKind::FocusChanged(focused) => (
            LIFECYCLE_DEVICE,
            InputDeviceKind::Keyboard,
            InputModifiers::default(),
            PlatformInputPayload::FocusChanged { focused: *focused },
        ),
        NativeInputKind::VisibilityChanged(visible) => (
            LIFECYCLE_DEVICE,
            InputDeviceKind::Keyboard,
            InputModifiers::default(),
            PlatformInputPayload::VisibilityChanged { visible: *visible },
        ),
        NativeInputKind::ModifiersChanged(_) | NativeInputKind::Resized { .. } => return Ok(None),
        NativeInputKind::CloseRequested => unreachable!(),
    };
    Ok(Some(normalized))
}

fn input_header(
    event: &NativeInputEvent,
    metrics_revision: u64,
    device: u64,
    device_kind: InputDeviceKind,
    modifiers: InputModifiers,
) -> PlatformInputHeader {
    PlatformInputHeader {
        sequence: event.sequence,
        timestamp_micros: event.timestamp_micros,
        metrics_revision,
        window: event.window,
        view: event.view,
        device: InputDeviceId {
            value: device,
            generation: 1,
        },
        device_kind,
        modifiers,
    }
}

fn route_report(
    event: &NativeInputEvent,
    metrics_revision: u64,
    report: vo_app_runtime::PlatformInputRoutingReport,
) -> NativeInputRoute {
    NativeInputRoute {
        source_sequence: event.sequence,
        arbitration: report.arbitration,
        released: report.view_release,
        synthesized_release_count: report.synthesized_releases.len(),
        final_composition_revision: report.composition_revision,
        metrics_revision,
    }
}

const fn pointer_device(device: u64) -> u64 {
    POINTER_DEVICE_NAMESPACE | device
}

const fn keyboard_device(device: u64) -> u64 {
    KEYBOARD_DEVICE_NAMESPACE | device
}

fn pointer_button(button: NativePointerButton) -> Result<u8, String> {
    match button {
        NativePointerButton::Primary => Ok(0),
        NativePointerButton::Secondary => Ok(1),
        NativePointerButton::Middle => Ok(2),
        NativePointerButton::Auxiliary(index) => u8::try_from(index)
            .ok()
            .filter(|index| *index < 32)
            .ok_or_else(|| String::from("native pointer button exceeds platform input mask")),
    }
}

fn input_modifiers(value: NativeModifiers) -> InputModifiers {
    InputModifiers {
        shift: value.shift,
        control: value.control,
        alt: value.alt,
        meta: value.meta,
        caps_lock: value.caps_lock,
        num_lock: false,
    }
}

fn q15_from_milli(value: u16) -> u16 {
    u16::try_from(u32::from(value).saturating_mul(u32::from(u16::MAX)) / 1_000).unwrap_or(u16::MAX)
}

fn scaled_pixels(logical_milli: u32, scale_milli: u32) -> u32 {
    u32::try_from(
        u64::from(logical_milli)
            .saturating_mul(u64::from(scale_milli))
            .saturating_add(999_999)
            / 1_000_000,
    )
    .unwrap_or(u32::MAX)
}
