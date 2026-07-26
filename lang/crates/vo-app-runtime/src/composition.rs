use alloc::collections::{BTreeMap, VecDeque};
use alloc::vec;
use alloc::vec::Vec;

use vo_app_protocol::{GenerationalHandle, SessionHandle, SurfaceHandle, ViewHandle, WindowHandle};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CompositionLimits {
    pub max_windows: usize,
    pub max_views: usize,
    pub max_surfaces: usize,
    pub max_surfaces_per_view: usize,
    pub max_trace_entries: usize,
}

impl Default for CompositionLimits {
    fn default() -> Self {
        Self {
            max_windows: 16,
            max_views: 64,
            max_surfaces: 256,
            max_surfaces_per_view: 32,
            max_trace_entries: 1024,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SurfaceKind {
    Game,
    Ui,
    Diagnostics,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SurfaceInputPolicy {
    Observe,
    Passthrough,
    Interactive,
    Exclusive,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ShortcutScope {
    View,
    Window,
    Session,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SurfaceShortcutRegistration {
    pub class_mask: u64,
    pub scope: ShortcutScope,
    pub priority: i16,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SurfaceRect {
    pub x_milli: i32,
    pub y_milli: i32,
    pub width_milli: u32,
    pub height_milli: u32,
}

impl SurfaceRect {
    pub fn contains(self, x_milli: i32, y_milli: i32) -> bool {
        let right = i64::from(self.x_milli) + i64::from(self.width_milli);
        let bottom = i64::from(self.y_milli) + i64::from(self.height_milli);
        i64::from(x_milli) >= i64::from(self.x_milli)
            && i64::from(y_milli) >= i64::from(self.y_milli)
            && i64::from(x_milli) < right
            && i64::from(y_milli) < bottom
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SurfaceTransform {
    pub m11_q16: i32,
    pub m12_q16: i32,
    pub m21_q16: i32,
    pub m22_q16: i32,
    pub translate_x_milli: i32,
    pub translate_y_milli: i32,
}

impl Default for SurfaceTransform {
    fn default() -> Self {
        Self {
            m11_q16: 1 << 16,
            m12_q16: 0,
            m21_q16: 0,
            m22_q16: 1 << 16,
            translate_x_milli: 0,
            translate_y_milli: 0,
        }
    }
}

impl SurfaceTransform {
    fn inverse_point(self, x_milli: i32, y_milli: i32) -> Option<(i32, i32)> {
        let m11 = f64::from(self.m11_q16) / 65_536.0;
        let m12 = f64::from(self.m12_q16) / 65_536.0;
        let m21 = f64::from(self.m21_q16) / 65_536.0;
        let m22 = f64::from(self.m22_q16) / 65_536.0;
        let determinant = m11 * m22 - m12 * m21;
        if !determinant.is_finite() || determinant.abs() < f64::EPSILON {
            return None;
        }
        let x = f64::from(x_milli) - f64::from(self.translate_x_milli);
        let y = f64::from(y_milli) - f64::from(self.translate_y_milli);
        let local_x = (m22 * x - m21 * y) / determinant;
        let local_y = (-m12 * x + m11 * y) / determinant;
        Some((round_i32(local_x)?, round_i32(local_y)?))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SurfaceGeometry {
    /// `None` fills the owning View.
    pub bounds: Option<SurfaceRect>,
    /// Optional local-space clip applied after the inverse transform.
    pub clip: Option<SurfaceRect>,
    pub transform: SurfaceTransform,
    pub opacity_q16: u16,
    pub hit_test_enabled: bool,
}

impl Default for SurfaceGeometry {
    fn default() -> Self {
        Self {
            bounds: None,
            clip: None,
            transform: SurfaceTransform::default(),
            opacity_q16: u16::MAX,
            hit_test_enabled: true,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SurfaceDescriptor {
    pub view: ViewHandle,
    pub kind: SurfaceKind,
    pub z_order: i32,
    pub input: SurfaceInputPolicy,
    pub accepts_text: bool,
    pub geometry: SurfaceGeometry,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SurfaceRuntimeState {
    Active,
    Suspended,
    Lost,
    Recovering,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SurfacePresentationOutcome {
    Presented,
    DeadlineMissed,
    ZeroSized,
    Suspended,
    TimedOut,
    SurfaceLost,
    DeviceLost,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SurfaceStatus {
    pub surface: SurfaceHandle,
    pub generation: u64,
    pub state: SurfaceRuntimeState,
    pub last_outcome: Option<SurfacePresentationOutcome>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SurfaceRecoveryTicket {
    pub surface: SurfaceHandle,
    pub old_generation: u64,
    pub new_generation: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ViewVisibility {
    Visible,
    Hidden,
    Suspended,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ViewInsets {
    pub top_milli: u32,
    pub right_milli: u32,
    pub bottom_milli: u32,
    pub left_milli: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ViewMetrics {
    pub revision: u64,
    pub origin_x_milli: i32,
    pub origin_y_milli: i32,
    pub width_milli: u32,
    pub height_milli: u32,
    pub framebuffer_width: u32,
    pub framebuffer_height: u32,
    pub scale_q16: u32,
    pub safe_area: ViewInsets,
    pub visibility: ViewVisibility,
}

impl Default for ViewMetrics {
    fn default() -> Self {
        Self {
            revision: 1,
            origin_x_milli: 0,
            origin_y_milli: 0,
            width_milli: 0,
            height_milli: 0,
            framebuffer_width: 0,
            framebuffer_height: 0,
            scale_q16: 1 << 16,
            safe_area: ViewInsets {
                top_milli: 0,
                right_milli: 0,
                bottom_milli: 0,
                left_milli: 0,
            },
            visibility: ViewVisibility::Hidden,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ViewMetricsUpdate {
    pub origin_x_milli: i32,
    pub origin_y_milli: i32,
    pub width_milli: u32,
    pub height_milli: u32,
    pub framebuffer_width: u32,
    pub framebuffer_height: u32,
    pub scale_q16: u32,
    pub safe_area: ViewInsets,
    pub visibility: ViewVisibility,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArbitrationEvent {
    Pointer {
        hit: Option<SurfaceHandle>,
    },
    PointerStack {
        hits: Vec<SurfaceHandle>,
    },
    PointerFor {
        pointer: CompositionPointerId,
        hit: Option<SurfaceHandle>,
    },
    PointerStackFor {
        pointer: CompositionPointerId,
        hits: Vec<SurfaceHandle>,
    },
    Keyboard,
    KeyboardFor {
        target: Option<SurfaceHandle>,
    },
    Gamepad,
    GamepadFor {
        target: Option<SurfaceHandle>,
    },
    Text,
    Shortcut,
    SystemShortcut {
        class_mask: u64,
    },
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct CompositionPointerId {
    pub device: u64,
    pub device_generation: u32,
    pub contact: u32,
}

impl CompositionPointerId {
    pub const PRIMARY: Self = Self {
        device: 1,
        device_generation: 1,
        contact: 0,
    };

    pub const fn is_valid(self) -> bool {
        self.device != 0 && self.device_generation != 0 && self.contact != u32::MAX
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SurfaceLayer {
    pub surface: SurfaceHandle,
    pub descriptor: SurfaceDescriptor,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InputDelivery {
    pub surface: SurfaceHandle,
    pub observed: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ArbitrationResult {
    pub revision: u64,
    pub deliveries: Vec<InputDelivery>,
    pub consumed_by: Option<SurfaceHandle>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SurfaceRelease {
    Focus,
    PointerCapture,
    ImeComposition,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SurfaceCloseReport {
    pub surface: SurfaceHandle,
    pub releases: Vec<SurfaceRelease>,
    pub restored_focus: Option<SurfaceHandle>,
    pub revision: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ViewInputState {
    pub focused: Option<SurfaceHandle>,
    pub pointer_capture: Option<SurfaceHandle>,
    pub ime: Option<SurfaceHandle>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ViewInputReleaseReport {
    pub view: ViewHandle,
    pub released: ViewInputState,
    pub revision: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompositionShutdownReport {
    pub closed_surfaces: Vec<SurfaceCloseReport>,
    pub closed_views: Vec<ViewHandle>,
    pub closed_windows: Vec<WindowHandle>,
    pub final_revision: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompositionTrace {
    pub revision: u64,
    pub view: ViewHandle,
    pub event: ArbitrationEvent,
    pub deliveries: Vec<InputDelivery>,
    pub consumed_by: Option<SurfaceHandle>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CompositionError {
    InvalidConfig,
    InvalidOwner,
    Capacity,
    PerViewCapacity,
    InvalidWindow,
    StaleWindow,
    WindowInUse,
    InvalidView,
    StaleView,
    ViewInUse,
    InvalidSurface,
    StaleSurface,
    InvalidGeometry,
    InvalidMetrics,
    InvalidSurfaceState,
    StaleSurfaceGeneration,
    WrongView,
    NotInteractive,
    TextUnsupported,
    RevisionConflict,
    RevisionExhausted,
    GenerationExhausted,
}

struct WindowRecord;

struct ViewRecord {
    window: WindowHandle,
    metrics: ViewMetrics,
    surfaces: Vec<SurfaceHandle>,
    focus: Option<SurfaceHandle>,
    pointer_capture: Option<SurfaceHandle>,
    pointer_captures: BTreeMap<CompositionPointerId, SurfaceHandle>,
    ime: Option<SurfaceHandle>,
}

struct SurfaceRecord {
    descriptor: SurfaceDescriptor,
    system_shortcuts: Vec<SurfaceShortcutRegistration>,
    runtime_generation: u64,
    runtime_state: SurfaceRuntimeState,
    last_outcome: Option<SurfacePresentationOutcome>,
}

struct Slot<T> {
    generation: u32,
    value: Option<T>,
}

pub struct CompositionRegistry {
    session: SessionHandle,
    session_epoch: u64,
    limits: CompositionLimits,
    revision: u64,
    windows: Vec<Slot<WindowRecord>>,
    free_windows: Vec<u32>,
    live_windows: usize,
    views: Vec<Slot<ViewRecord>>,
    free_views: Vec<u32>,
    live_views: usize,
    surfaces: Vec<Slot<SurfaceRecord>>,
    free_surfaces: Vec<u32>,
    live_surfaces: usize,
    traces: VecDeque<CompositionTrace>,
}

impl CompositionRegistry {
    pub fn new(
        session: SessionHandle,
        session_epoch: u64,
        limits: CompositionLimits,
    ) -> Result<Self, CompositionError> {
        if !session.is_valid() || session_epoch == 0 {
            return Err(CompositionError::InvalidOwner);
        }
        if limits.max_windows == 0
            || limits.max_views == 0
            || limits.max_surfaces == 0
            || limits.max_surfaces_per_view == 0
            || limits.max_trace_entries == 0
            || limits.max_windows > u32::MAX as usize
            || limits.max_views > u32::MAX as usize
            || limits.max_surfaces > u32::MAX as usize
        {
            return Err(CompositionError::InvalidConfig);
        }
        Ok(Self {
            session,
            session_epoch,
            limits,
            revision: 0,
            windows: Vec::new(),
            free_windows: Vec::new(),
            live_windows: 0,
            views: Vec::new(),
            free_views: Vec::new(),
            live_views: 0,
            surfaces: Vec::new(),
            free_surfaces: Vec::new(),
            live_surfaces: 0,
            traces: VecDeque::new(),
        })
    }

    pub const fn session(&self) -> SessionHandle {
        self.session
    }

    pub const fn session_epoch(&self) -> u64 {
        self.session_epoch
    }

    pub const fn revision(&self) -> u64 {
        self.revision
    }

    pub const fn live_window_count(&self) -> usize {
        self.live_windows
    }

    pub const fn live_view_count(&self) -> usize {
        self.live_views
    }

    pub const fn live_surface_count(&self) -> usize {
        self.live_surfaces
    }

    pub fn view_window(&self, view: ViewHandle) -> Result<WindowHandle, CompositionError> {
        Ok(self.view(view)?.window)
    }

    pub fn view_metrics(&self, view: ViewHandle) -> Result<ViewMetrics, CompositionError> {
        Ok(self.view(view)?.metrics)
    }

    pub fn view_input_state(&self, view: ViewHandle) -> Result<ViewInputState, CompositionError> {
        let view = self.view(view)?;
        Ok(ViewInputState {
            focused: view.focus,
            pointer_capture: view.pointer_capture,
            ime: view.ime,
        })
    }

    pub fn view_pointer_captures(
        &self,
        view: ViewHandle,
    ) -> Result<Vec<(CompositionPointerId, SurfaceHandle)>, CompositionError> {
        Ok(self
            .view(view)?
            .pointer_captures
            .iter()
            .map(|(pointer, surface)| (*pointer, *surface))
            .collect())
    }

    pub fn surface_descriptor(
        &self,
        surface: SurfaceHandle,
    ) -> Result<SurfaceDescriptor, CompositionError> {
        Ok(self.surface(surface)?.descriptor)
    }

    pub fn view_layers(&self, view: ViewHandle) -> Result<Vec<SurfaceLayer>, CompositionError> {
        let view = self.view(view)?;
        view.surfaces
            .iter()
            .map(|surface| {
                Ok(SurfaceLayer {
                    surface: *surface,
                    descriptor: self.surface(*surface)?.descriptor,
                })
            })
            .collect()
    }

    pub fn hit_test_stack(
        &self,
        view: ViewHandle,
        x_milli: i32,
        y_milli: i32,
    ) -> Result<Vec<SurfaceHandle>, CompositionError> {
        let view = self.view(view)?;
        let mut hits = Vec::new();
        for surface in view.surfaces.iter().rev() {
            let descriptor = self.surface(*surface)?.descriptor;
            if !descriptor.geometry.hit_test_enabled {
                continue;
            }
            let Some((local_x, local_y)) = descriptor
                .geometry
                .transform
                .inverse_point(x_milli, y_milli)
            else {
                continue;
            };
            if descriptor
                .geometry
                .bounds
                .is_some_and(|bounds| !bounds.contains(local_x, local_y))
                || descriptor
                    .geometry
                    .clip
                    .is_some_and(|clip| !clip.contains(local_x, local_y))
            {
                continue;
            }
            hits.push(*surface);
        }
        Ok(hits)
    }

    pub fn traces(&self) -> impl Iterator<Item = &CompositionTrace> {
        self.traces.iter()
    }

    pub fn create_window(&mut self) -> Result<WindowHandle, CompositionError> {
        if self.live_windows == self.limits.max_windows {
            return Err(CompositionError::Capacity);
        }
        let handle = allocate(&mut self.windows, &mut self.free_windows, WindowRecord)?;
        self.live_windows += 1;
        self.advance_revision()?;
        Ok(handle)
    }

    pub fn close_window(&mut self, window: WindowHandle) -> Result<(), CompositionError> {
        self.window(window)?;
        if self.views.iter().any(|slot| {
            slot.value
                .as_ref()
                .is_some_and(|view| view.window == window)
        }) {
            return Err(CompositionError::WindowInUse);
        }
        release(&mut self.windows, &mut self.free_windows, window).map_err(map_window_error)?;
        self.live_windows -= 1;
        self.advance_revision()
    }

    pub fn create_view(&mut self, window: WindowHandle) -> Result<ViewHandle, CompositionError> {
        self.window(window)?;
        if self.live_views == self.limits.max_views {
            return Err(CompositionError::Capacity);
        }
        let handle = allocate(
            &mut self.views,
            &mut self.free_views,
            ViewRecord {
                window,
                metrics: ViewMetrics::default(),
                surfaces: Vec::new(),
                focus: None,
                pointer_capture: None,
                pointer_captures: BTreeMap::new(),
                ime: None,
            },
        )?;
        self.live_views += 1;
        self.advance_revision()?;
        Ok(handle)
    }

    pub fn update_view_metrics(
        &mut self,
        view: ViewHandle,
        update: ViewMetricsUpdate,
        expected_metrics_revision: u64,
    ) -> Result<ViewMetrics, CompositionError> {
        validate_view_metrics(update)?;
        let current = self.view(view)?.metrics;
        if current.revision != expected_metrics_revision {
            return Err(CompositionError::RevisionConflict);
        }
        let revision = current
            .revision
            .checked_add(1)
            .ok_or(CompositionError::RevisionExhausted)?;
        let metrics = ViewMetrics {
            revision,
            origin_x_milli: update.origin_x_milli,
            origin_y_milli: update.origin_y_milli,
            width_milli: update.width_milli,
            height_milli: update.height_milli,
            framebuffer_width: update.framebuffer_width,
            framebuffer_height: update.framebuffer_height,
            scale_q16: update.scale_q16,
            safe_area: update.safe_area,
            visibility: update.visibility,
        };
        self.view_mut(view)?.metrics = metrics;
        self.advance_revision()?;
        Ok(metrics)
    }

    pub fn close_view(&mut self, view: ViewHandle) -> Result<(), CompositionError> {
        let record = self.view(view)?;
        if !record.surfaces.is_empty() {
            return Err(CompositionError::ViewInUse);
        }
        release(&mut self.views, &mut self.free_views, view).map_err(map_view_error)?;
        self.live_views -= 1;
        self.advance_revision()
    }

    pub fn attach_surface(
        &mut self,
        descriptor: SurfaceDescriptor,
    ) -> Result<SurfaceHandle, CompositionError> {
        validate_geometry(descriptor.geometry)?;
        let view = self.view(descriptor.view)?;
        if view.surfaces.len() == self.limits.max_surfaces_per_view {
            return Err(CompositionError::PerViewCapacity);
        }
        if self.live_surfaces == self.limits.max_surfaces {
            return Err(CompositionError::Capacity);
        }
        let handle = allocate(
            &mut self.surfaces,
            &mut self.free_surfaces,
            SurfaceRecord {
                descriptor,
                system_shortcuts: Vec::new(),
                runtime_generation: 1,
                runtime_state: SurfaceRuntimeState::Active,
                last_outcome: None,
            },
        )?;
        self.live_surfaces += 1;
        let mut surfaces = self.view(descriptor.view)?.surfaces.clone();
        surfaces.push(handle);
        sort_layers(&self.surfaces, &mut surfaces);
        self.view_mut(descriptor.view)?.surfaces = surfaces;
        self.advance_revision()?;
        Ok(handle)
    }

    pub fn update_surface_geometry(
        &mut self,
        surface: SurfaceHandle,
        geometry: SurfaceGeometry,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        validate_geometry(geometry)?;
        self.surface_mut(surface)?.descriptor.geometry = geometry;
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn surface_status(
        &self,
        surface: SurfaceHandle,
    ) -> Result<SurfaceStatus, CompositionError> {
        let record = self.surface(surface)?;
        Ok(SurfaceStatus {
            surface,
            generation: record.runtime_generation,
            state: record.runtime_state,
            last_outcome: record.last_outcome,
        })
    }

    pub fn report_surface_outcome(
        &mut self,
        surface: SurfaceHandle,
        surface_generation: u64,
        outcome: SurfacePresentationOutcome,
    ) -> Result<SurfaceStatus, CompositionError> {
        let record = self.surface_mut(surface)?;
        if record.runtime_generation != surface_generation {
            return Err(CompositionError::StaleSurfaceGeneration);
        }
        if record.runtime_state == SurfaceRuntimeState::Recovering {
            return Err(CompositionError::InvalidSurfaceState);
        }
        record.runtime_state = match outcome {
            SurfacePresentationOutcome::Presented
            | SurfacePresentationOutcome::DeadlineMissed
            | SurfacePresentationOutcome::TimedOut => SurfaceRuntimeState::Active,
            SurfacePresentationOutcome::ZeroSized | SurfacePresentationOutcome::Suspended => {
                SurfaceRuntimeState::Suspended
            }
            SurfacePresentationOutcome::SurfaceLost | SurfacePresentationOutcome::DeviceLost => {
                SurfaceRuntimeState::Lost
            }
        };
        record.last_outcome = Some(outcome);
        self.advance_revision()?;
        self.surface_status(surface)
    }

    pub fn begin_surface_recovery(
        &mut self,
        surface: SurfaceHandle,
        expected_generation: u64,
    ) -> Result<SurfaceRecoveryTicket, CompositionError> {
        let record = self.surface_mut(surface)?;
        if record.runtime_generation != expected_generation {
            return Err(CompositionError::StaleSurfaceGeneration);
        }
        if record.runtime_state != SurfaceRuntimeState::Lost {
            return Err(CompositionError::InvalidSurfaceState);
        }
        let new_generation = expected_generation
            .checked_add(1)
            .ok_or(CompositionError::GenerationExhausted)?;
        record.runtime_state = SurfaceRuntimeState::Recovering;
        self.advance_revision()?;
        Ok(SurfaceRecoveryTicket {
            surface,
            old_generation: expected_generation,
            new_generation,
        })
    }

    pub fn complete_surface_recovery(
        &mut self,
        ticket: SurfaceRecoveryTicket,
        suspended: bool,
    ) -> Result<SurfaceStatus, CompositionError> {
        let record = self.surface_mut(ticket.surface)?;
        if record.runtime_state != SurfaceRuntimeState::Recovering
            || record.runtime_generation != ticket.old_generation
            || ticket.new_generation
                != ticket
                    .old_generation
                    .checked_add(1)
                    .ok_or(CompositionError::GenerationExhausted)?
        {
            return Err(CompositionError::StaleSurfaceGeneration);
        }
        record.runtime_generation = ticket.new_generation;
        record.runtime_state = if suspended {
            SurfaceRuntimeState::Suspended
        } else {
            SurfaceRuntimeState::Active
        };
        record.last_outcome = None;
        self.advance_revision()?;
        self.surface_status(ticket.surface)
    }

    pub fn update_surface_input_policy(
        &mut self,
        surface: SurfaceHandle,
        input: SurfaceInputPolicy,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        self.surface_mut(surface)?.descriptor.input = input;
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn set_surface_system_shortcuts(
        &mut self,
        surface: SurfaceHandle,
        class_mask: u64,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.register_surface_system_shortcuts(
            surface,
            (class_mask != 0).then_some(SurfaceShortcutRegistration {
                class_mask,
                scope: ShortcutScope::View,
                priority: 0,
            }),
            expected_revision,
        )
    }

    pub fn register_surface_system_shortcuts(
        &mut self,
        surface: SurfaceHandle,
        registration: Option<SurfaceShortcutRegistration>,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.register_surface_system_shortcut_set(
            surface,
            registration.into_iter().collect(),
            expected_revision,
        )
    }

    pub fn register_surface_system_shortcut_set(
        &mut self,
        surface: SurfaceHandle,
        registrations: Vec<SurfaceShortcutRegistration>,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        let descriptor = self.surface(surface)?.descriptor;
        if registrations.len() > 64 {
            return Err(CompositionError::Capacity);
        }
        if registrations.iter().any(|registration| {
            registration.class_mask == 0
                || !matches!(
                    descriptor.input,
                    SurfaceInputPolicy::Interactive | SurfaceInputPolicy::Exclusive
                )
        }) {
            return Err(CompositionError::NotInteractive);
        }
        self.surface_mut(surface)?.system_shortcuts = registrations;
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn set_focus(
        &mut self,
        view: ViewHandle,
        surface: Option<SurfaceHandle>,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        if let Some(surface) = surface {
            let descriptor = self.surface(surface)?.descriptor;
            if descriptor.view != view {
                return Err(CompositionError::WrongView);
            }
            if !matches!(
                descriptor.input,
                SurfaceInputPolicy::Interactive | SurfaceInputPolicy::Exclusive
            ) {
                return Err(CompositionError::NotInteractive);
            }
        }
        self.view_mut(view)?.focus = surface;
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn capture_pointer(
        &mut self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        let descriptor = self.surface(surface)?.descriptor;
        if !matches!(
            descriptor.input,
            SurfaceInputPolicy::Interactive | SurfaceInputPolicy::Exclusive
        ) {
            return Err(CompositionError::NotInteractive);
        }
        self.view_mut(descriptor.view)?.pointer_capture = Some(surface);
        self.view_mut(descriptor.view)?
            .pointer_captures
            .insert(CompositionPointerId::PRIMARY, surface);
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn capture_pointer_for(
        &mut self,
        pointer: CompositionPointerId,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        if !pointer.is_valid() {
            return Err(CompositionError::NotInteractive);
        }
        let descriptor = self.surface(surface)?.descriptor;
        if !matches!(
            descriptor.input,
            SurfaceInputPolicy::Interactive | SurfaceInputPolicy::Exclusive
        ) {
            return Err(CompositionError::NotInteractive);
        }
        let view = self.view_mut(descriptor.view)?;
        view.pointer_captures.insert(pointer, surface);
        if pointer == CompositionPointerId::PRIMARY {
            view.pointer_capture = Some(surface);
        }
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn release_pointer(
        &mut self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        let descriptor = self.surface(surface)?.descriptor;
        let view = self.view_mut(descriptor.view)?;
        if view.pointer_capture != Some(surface) {
            return Err(CompositionError::NotInteractive);
        }
        view.pointer_capture = None;
        view.pointer_captures.remove(&CompositionPointerId::PRIMARY);
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn release_pointer_for(
        &mut self,
        pointer: CompositionPointerId,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        let descriptor = self.surface(surface)?.descriptor;
        let view = self.view_mut(descriptor.view)?;
        if view.pointer_captures.get(&pointer).copied() != Some(surface) {
            return Err(CompositionError::NotInteractive);
        }
        view.pointer_captures.remove(&pointer);
        if pointer == CompositionPointerId::PRIMARY {
            view.pointer_capture = None;
        }
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn release_device_pointer_captures(
        &mut self,
        device: u64,
        device_generation: u32,
    ) -> Result<Vec<(CompositionPointerId, SurfaceHandle)>, CompositionError> {
        if device == 0 || device_generation == 0 {
            return Err(CompositionError::NotInteractive);
        }
        let mut released = Vec::new();
        for slot in &mut self.views {
            let Some(view) = slot.value.as_mut() else {
                continue;
            };
            let pointers = view
                .pointer_captures
                .iter()
                .filter_map(|(pointer, surface)| {
                    (pointer.device == device && pointer.device_generation == device_generation)
                        .then_some((*pointer, *surface))
                })
                .collect::<Vec<_>>();
            for (pointer, surface) in pointers {
                view.pointer_captures.remove(&pointer);
                if pointer == CompositionPointerId::PRIMARY {
                    view.pointer_capture = None;
                }
                released.push((pointer, surface));
            }
        }
        if !released.is_empty() {
            self.advance_revision()?;
        }
        Ok(released)
    }

    pub fn begin_ime(
        &mut self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        let descriptor = self.surface(surface)?.descriptor;
        if !descriptor.accepts_text {
            return Err(CompositionError::TextUnsupported);
        }
        let view = self.view(descriptor.view)?;
        if view.focus != Some(surface) {
            return Err(CompositionError::NotInteractive);
        }
        self.view_mut(descriptor.view)?.ime = Some(surface);
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn end_ime(
        &mut self,
        surface: SurfaceHandle,
        expected_revision: u64,
    ) -> Result<u64, CompositionError> {
        self.expect_revision(expected_revision)?;
        let descriptor = self.surface(surface)?.descriptor;
        let view = self.view_mut(descriptor.view)?;
        if view.ime != Some(surface) {
            return Err(CompositionError::NotInteractive);
        }
        view.ime = None;
        self.advance_revision()?;
        Ok(self.revision)
    }

    pub fn suspend_view_input(
        &mut self,
        view: ViewHandle,
        expected_revision: u64,
    ) -> Result<ViewInputReleaseReport, CompositionError> {
        self.expect_revision(expected_revision)?;
        let state = self.view_input_state(view)?;
        let record = self.view_mut(view)?;
        record.focus = None;
        record.pointer_capture = None;
        record.pointer_captures.clear();
        record.ime = None;
        self.advance_revision()?;
        Ok(ViewInputReleaseReport {
            view,
            released: state,
            revision: self.revision,
        })
    }

    pub fn arbitrate(
        &mut self,
        view: ViewHandle,
        event: ArbitrationEvent,
    ) -> Result<ArbitrationResult, CompositionError> {
        self.view(view)?;
        if let ArbitrationEvent::SystemShortcut { class_mask } = &event {
            if let Some(surface) = self.resolve_system_shortcut(view, *class_mask)? {
                self.advance_revision()?;
                let result = ArbitrationResult {
                    revision: self.revision,
                    deliveries: vec![InputDelivery {
                        surface,
                        observed: false,
                    }],
                    consumed_by: Some(surface),
                };
                self.push_trace(CompositionTrace {
                    revision: result.revision,
                    view,
                    event: ArbitrationEvent::SystemShortcut {
                        class_mask: *class_mask,
                    },
                    deliveries: result.deliveries.clone(),
                    consumed_by: result.consumed_by,
                });
                return Ok(result);
            }
        }
        let view_record = self.view(view)?;
        let layers = view_record.surfaces.clone();
        let focus = view_record.focus;
        let capture = view_record.pointer_capture;
        let pointer_captures = view_record.pointer_captures.clone();
        let mut deliveries = Vec::new();
        let mut consumed_by = None;
        for surface in layers.into_iter().rev() {
            let record = self.surface(surface)?;
            let descriptor = record.descriptor;
            let targeted = match &event {
                ArbitrationEvent::Pointer { hit } => {
                    capture.map_or(*hit == Some(surface), |owner| owner == surface)
                }
                ArbitrationEvent::PointerStack { hits } => {
                    capture.map_or(hits.contains(&surface), |owner| owner == surface)
                }
                ArbitrationEvent::PointerFor { pointer, hit } => pointer_captures
                    .get(pointer)
                    .copied()
                    .map_or(*hit == Some(surface), |owner| owner == surface),
                ArbitrationEvent::PointerStackFor { pointer, hits } => pointer_captures
                    .get(pointer)
                    .copied()
                    .map_or(hits.contains(&surface), |owner| owner == surface),
                ArbitrationEvent::Keyboard
                | ArbitrationEvent::Gamepad
                | ArbitrationEvent::Shortcut => focus == Some(surface),
                ArbitrationEvent::KeyboardFor { target }
                | ArbitrationEvent::GamepadFor { target } => *target == Some(surface),
                ArbitrationEvent::SystemShortcut { .. } => focus == Some(surface),
                ArbitrationEvent::Text => focus == Some(surface) && descriptor.accepts_text,
            };
            match descriptor.input {
                SurfaceInputPolicy::Observe => deliveries.push(InputDelivery {
                    surface,
                    observed: true,
                }),
                SurfaceInputPolicy::Passthrough if targeted => deliveries.push(InputDelivery {
                    surface,
                    observed: false,
                }),
                SurfaceInputPolicy::Interactive if targeted => {
                    deliveries.push(InputDelivery {
                        surface,
                        observed: false,
                    });
                    consumed_by = Some(surface);
                    break;
                }
                SurfaceInputPolicy::Exclusive => {
                    deliveries.push(InputDelivery {
                        surface,
                        observed: false,
                    });
                    consumed_by = Some(surface);
                    break;
                }
                _ => {}
            }
        }
        self.advance_revision()?;
        let result = ArbitrationResult {
            revision: self.revision,
            deliveries,
            consumed_by,
        };
        self.push_trace(CompositionTrace {
            revision: result.revision,
            view,
            event,
            deliveries: result.deliveries.clone(),
            consumed_by,
        });
        Ok(result)
    }

    pub fn close_surface(
        &mut self,
        surface: SurfaceHandle,
    ) -> Result<SurfaceCloseReport, CompositionError> {
        let descriptor = self.surface(surface)?.descriptor;
        let view = self.view(descriptor.view)?;
        let had_focus = view.focus == Some(surface);
        let captured_pointers = view
            .pointer_captures
            .iter()
            .filter_map(|(pointer, owner)| (*owner == surface).then_some(*pointer))
            .collect::<Vec<_>>();
        let had_capture = view.pointer_capture == Some(surface) || !captured_pointers.is_empty();
        let had_ime = view.ime == Some(surface);
        let mut remaining = view.surfaces.clone();
        remaining.retain(|candidate| *candidate != surface);
        let restored_focus = had_focus
            .then(|| {
                remaining.iter().rev().copied().find(|candidate| {
                    self.surface(*candidate).is_ok_and(|record| {
                        matches!(
                            record.descriptor.input,
                            SurfaceInputPolicy::Interactive | SurfaceInputPolicy::Exclusive
                        )
                    })
                })
            })
            .flatten();
        let view = self.view_mut(descriptor.view)?;
        view.surfaces = remaining;
        if had_focus {
            view.focus = restored_focus;
        }
        if had_capture {
            view.pointer_capture = None;
            for pointer in captured_pointers {
                view.pointer_captures.remove(&pointer);
            }
        }
        if had_ime {
            view.ime = None;
        }
        release(&mut self.surfaces, &mut self.free_surfaces, surface).map_err(map_surface_error)?;
        self.live_surfaces -= 1;
        self.advance_revision()?;
        let mut releases = Vec::new();
        if had_ime {
            releases.push(SurfaceRelease::ImeComposition);
        }
        if had_capture {
            releases.push(SurfaceRelease::PointerCapture);
        }
        if had_focus {
            releases.push(SurfaceRelease::Focus);
        }
        Ok(SurfaceCloseReport {
            surface,
            releases,
            restored_focus,
            revision: self.revision,
        })
    }

    pub fn shutdown(&mut self) -> Result<CompositionShutdownReport, CompositionError> {
        let mutation_count = self
            .live_surfaces
            .checked_add(self.live_views)
            .and_then(|count| count.checked_add(self.live_windows))
            .ok_or(CompositionError::RevisionExhausted)?;
        self.revision
            .checked_add(mutation_count as u64)
            .ok_or(CompositionError::RevisionExhausted)?;
        preflight_release(&self.windows)?;
        preflight_release(&self.views)?;
        preflight_release(&self.surfaces)?;

        let view_handles = live_handles(&self.views);
        let mut closed_surfaces = Vec::with_capacity(self.live_surfaces);
        let mut closed_views = Vec::with_capacity(self.live_views);
        for view in view_handles {
            let surfaces = self.view(view)?.surfaces.clone();
            for surface in surfaces.into_iter().rev() {
                closed_surfaces.push(self.close_surface(surface)?);
            }
            self.close_view(view)?;
            closed_views.push(view);
        }
        let window_handles = live_handles(&self.windows);
        let mut closed_windows = Vec::with_capacity(self.live_windows);
        for window in window_handles {
            self.close_window(window)?;
            closed_windows.push(window);
        }
        Ok(CompositionShutdownReport {
            closed_surfaces,
            closed_views,
            closed_windows,
            final_revision: self.revision,
        })
    }

    fn expect_revision(&self, expected: u64) -> Result<(), CompositionError> {
        if expected == self.revision {
            Ok(())
        } else {
            Err(CompositionError::RevisionConflict)
        }
    }

    fn advance_revision(&mut self) -> Result<(), CompositionError> {
        self.revision = self
            .revision
            .checked_add(1)
            .ok_or(CompositionError::RevisionExhausted)?;
        Ok(())
    }

    fn push_trace(&mut self, trace: CompositionTrace) {
        if self.traces.len() == self.limits.max_trace_entries {
            self.traces.pop_front();
        }
        self.traces.push_back(trace);
    }

    fn window(&self, handle: WindowHandle) -> Result<&WindowRecord, CompositionError> {
        get(&self.windows, handle).map_err(map_window_error)
    }

    fn view(&self, handle: ViewHandle) -> Result<&ViewRecord, CompositionError> {
        get(&self.views, handle).map_err(map_view_error)
    }

    fn view_mut(&mut self, handle: ViewHandle) -> Result<&mut ViewRecord, CompositionError> {
        get_mut(&mut self.views, handle).map_err(map_view_error)
    }

    fn surface(&self, handle: SurfaceHandle) -> Result<&SurfaceRecord, CompositionError> {
        get(&self.surfaces, handle).map_err(map_surface_error)
    }

    fn surface_mut(
        &mut self,
        handle: SurfaceHandle,
    ) -> Result<&mut SurfaceRecord, CompositionError> {
        get_mut(&mut self.surfaces, handle).map_err(map_surface_error)
    }

    fn resolve_system_shortcut(
        &self,
        origin_view: ViewHandle,
        class_mask: u64,
    ) -> Result<Option<SurfaceHandle>, CompositionError> {
        if class_mask == 0 {
            return Ok(None);
        }
        let origin_window = self.view(origin_view)?.window;
        let mut candidates = Vec::new();
        for (index, slot) in self.surfaces.iter().enumerate() {
            let Some(record) = slot.value.as_ref() else {
                continue;
            };
            for registration in &record.system_shortcuts {
                if registration.class_mask & class_mask == 0 {
                    continue;
                }
                let surface_view = record.descriptor.view;
                let in_scope = match registration.scope {
                    ShortcutScope::View => surface_view == origin_view,
                    ShortcutScope::Window => self.view(surface_view)?.window == origin_window,
                    ShortcutScope::Session => true,
                };
                if !in_scope {
                    continue;
                }
                let scope_rank = match registration.scope {
                    ShortcutScope::View => 3_i8,
                    ShortcutScope::Window => 2,
                    ShortcutScope::Session => 1,
                };
                candidates.push((
                    registration.priority,
                    scope_rank,
                    i32::from(surface_view == origin_view),
                    record.descriptor.z_order,
                    SurfaceHandle {
                        index: index as u32,
                        generation: slot.generation,
                    },
                ));
            }
        }
        candidates.sort();
        Ok(candidates.pop().map(|candidate| candidate.4))
    }
}

pub(crate) fn validate_geometry(geometry: SurfaceGeometry) -> Result<(), CompositionError> {
    if geometry
        .bounds
        .is_some_and(|rect| rect.width_milli == 0 || rect.height_milli == 0)
        || geometry
            .clip
            .is_some_and(|rect| rect.width_milli == 0 || rect.height_milli == 0)
    {
        return Err(CompositionError::InvalidGeometry);
    }
    let transform = geometry.transform;
    let determinant = i64::from(transform.m11_q16) * i64::from(transform.m22_q16)
        - i64::from(transform.m12_q16) * i64::from(transform.m21_q16);
    if determinant == 0 {
        return Err(CompositionError::InvalidGeometry);
    }
    Ok(())
}

fn validate_view_metrics(update: ViewMetricsUpdate) -> Result<(), CompositionError> {
    if update.scale_q16 == 0
        || update
            .safe_area
            .left_milli
            .checked_add(update.safe_area.right_milli)
            .is_none_or(|sum| sum > update.width_milli)
        || update
            .safe_area
            .top_milli
            .checked_add(update.safe_area.bottom_milli)
            .is_none_or(|sum| sum > update.height_milli)
        || ((update.width_milli == 0 || update.height_milli == 0)
            && (update.framebuffer_width != 0 || update.framebuffer_height != 0))
        || ((update.width_milli != 0 && update.height_milli != 0)
            && (update.framebuffer_width == 0 || update.framebuffer_height == 0))
    {
        return Err(CompositionError::InvalidMetrics);
    }
    Ok(())
}

fn round_i32(value: f64) -> Option<i32> {
    if !value.is_finite() || value < f64::from(i32::MIN) || value > f64::from(i32::MAX) {
        None
    } else {
        Some(value.round() as i32)
    }
}

#[derive(Clone, Copy)]
enum ArenaError {
    Invalid,
    Stale,
    GenerationExhausted,
}

fn allocate<T>(
    slots: &mut Vec<Slot<T>>,
    free: &mut Vec<u32>,
    value: T,
) -> Result<GenerationalHandle, CompositionError> {
    if let Some(index) = free.pop() {
        let slot = slots
            .get_mut(index as usize)
            .ok_or(CompositionError::InvalidConfig)?;
        slot.value = Some(value);
        return Ok(GenerationalHandle {
            index,
            generation: slot.generation,
        });
    }
    let index = u32::try_from(slots.len()).map_err(|_| CompositionError::Capacity)?;
    slots.push(Slot {
        generation: 1,
        value: Some(value),
    });
    Ok(GenerationalHandle {
        index,
        generation: 1,
    })
}

fn get<T>(slots: &[Slot<T>], handle: GenerationalHandle) -> Result<&T, ArenaError> {
    if !handle.is_valid() {
        return Err(ArenaError::Invalid);
    }
    let slot = slots
        .get(handle.index as usize)
        .ok_or(ArenaError::Invalid)?;
    if slot.generation != handle.generation || slot.value.is_none() {
        return Err(ArenaError::Stale);
    }
    slot.value.as_ref().ok_or(ArenaError::Stale)
}

fn get_mut<T>(slots: &mut [Slot<T>], handle: GenerationalHandle) -> Result<&mut T, ArenaError> {
    if !handle.is_valid() {
        return Err(ArenaError::Invalid);
    }
    let slot = slots
        .get_mut(handle.index as usize)
        .ok_or(ArenaError::Invalid)?;
    if slot.generation != handle.generation || slot.value.is_none() {
        return Err(ArenaError::Stale);
    }
    slot.value.as_mut().ok_or(ArenaError::Stale)
}

fn release<T>(
    slots: &mut [Slot<T>],
    free: &mut Vec<u32>,
    handle: GenerationalHandle,
) -> Result<(), ArenaError> {
    let slot = slots
        .get_mut(handle.index as usize)
        .ok_or(ArenaError::Invalid)?;
    if !handle.is_valid() || slot.generation != handle.generation || slot.value.is_none() {
        return Err(ArenaError::Stale);
    }
    let generation = slot
        .generation
        .checked_add(1)
        .ok_or(ArenaError::GenerationExhausted)?;
    slot.value = None;
    slot.generation = generation;
    free.push(handle.index);
    Ok(())
}

fn preflight_release<T>(slots: &[Slot<T>]) -> Result<(), CompositionError> {
    if slots
        .iter()
        .any(|slot| slot.value.is_some() && slot.generation.checked_add(1).is_none())
    {
        return Err(CompositionError::GenerationExhausted);
    }
    Ok(())
}

fn live_handles<T>(slots: &[Slot<T>]) -> Vec<GenerationalHandle> {
    slots
        .iter()
        .enumerate()
        .filter_map(|(index, slot)| {
            slot.value.as_ref().map(|_| GenerationalHandle {
                index: index as u32,
                generation: slot.generation,
            })
        })
        .collect()
}

fn sort_layers(slots: &[Slot<SurfaceRecord>], surfaces: &mut [SurfaceHandle]) {
    surfaces.sort_by_key(|handle| {
        let z = get(slots, *handle)
            .map(|record| record.descriptor.z_order)
            .unwrap_or(i32::MIN);
        (z, handle.index, handle.generation)
    });
}

fn map_window_error(error: ArenaError) -> CompositionError {
    match error {
        ArenaError::Invalid => CompositionError::InvalidWindow,
        ArenaError::Stale => CompositionError::StaleWindow,
        ArenaError::GenerationExhausted => CompositionError::GenerationExhausted,
    }
}

fn map_view_error(error: ArenaError) -> CompositionError {
    match error {
        ArenaError::Invalid => CompositionError::InvalidView,
        ArenaError::Stale => CompositionError::StaleView,
        ArenaError::GenerationExhausted => CompositionError::GenerationExhausted,
    }
}

fn map_surface_error(error: ArenaError) -> CompositionError {
    match error {
        ArenaError::Invalid => CompositionError::InvalidSurface,
        ArenaError::Stale => CompositionError::StaleSurface,
        ArenaError::GenerationExhausted => CompositionError::GenerationExhausted,
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;

    fn registry() -> CompositionRegistry {
        CompositionRegistry::new(
            SessionHandle {
                index: 1,
                generation: 1,
            },
            7,
            CompositionLimits::default(),
        )
        .expect("registry")
    }

    fn surface(
        registry: &mut CompositionRegistry,
        view: ViewHandle,
        kind: SurfaceKind,
        z_order: i32,
        input: SurfaceInputPolicy,
        accepts_text: bool,
    ) -> SurfaceHandle {
        registry
            .attach_surface(SurfaceDescriptor {
                view,
                kind,
                z_order,
                input,
                accepts_text,
                geometry: SurfaceGeometry::default(),
            })
            .expect("surface")
    }

    #[test]
    fn ui_text_focus_blocks_game_keyboard_and_records_trace() {
        let mut registry = registry();
        let window = registry.create_window().expect("window");
        let view = registry.create_view(window).expect("view");
        let game = surface(
            &mut registry,
            view,
            SurfaceKind::Game,
            0,
            SurfaceInputPolicy::Interactive,
            false,
        );
        let ui = surface(
            &mut registry,
            view,
            SurfaceKind::Ui,
            10,
            SurfaceInputPolicy::Interactive,
            true,
        );
        let revision = registry.revision();
        registry.set_focus(view, Some(ui), revision).expect("focus");
        let result = registry
            .arbitrate(view, ArbitrationEvent::Keyboard)
            .expect("arbitrate");
        assert_eq!(result.consumed_by, Some(ui));
        assert_eq!(result.deliveries[0].surface, ui);
        assert!(!result
            .deliveries
            .iter()
            .any(|delivery| delivery.surface == game));
        assert_eq!(
            registry.traces().last().expect("trace").consumed_by,
            Some(ui)
        );
    }

    #[test]
    fn transparent_overlay_observes_pointer_and_game_receives_hit() {
        let mut registry = registry();
        let window = registry.create_window().expect("window");
        let view = registry.create_view(window).expect("view");
        let game = surface(
            &mut registry,
            view,
            SurfaceKind::Game,
            0,
            SurfaceInputPolicy::Interactive,
            false,
        );
        let hud = surface(
            &mut registry,
            view,
            SurfaceKind::Ui,
            10,
            SurfaceInputPolicy::Observe,
            false,
        );
        let result = registry
            .arbitrate(view, ArbitrationEvent::Pointer { hit: Some(game) })
            .expect("arbitrate");
        assert_eq!(result.consumed_by, Some(game));
        assert_eq!(
            result.deliveries[0],
            InputDelivery {
                surface: hud,
                observed: true
            }
        );
        assert_eq!(result.deliveries[1].surface, game);
    }

    #[test]
    fn close_surface_synthesizes_releases_and_restores_lower_focus() {
        let mut registry = registry();
        let window = registry.create_window().expect("window");
        let view = registry.create_view(window).expect("view");
        let game = surface(
            &mut registry,
            view,
            SurfaceKind::Game,
            0,
            SurfaceInputPolicy::Interactive,
            false,
        );
        let ui = surface(
            &mut registry,
            view,
            SurfaceKind::Ui,
            10,
            SurfaceInputPolicy::Interactive,
            true,
        );
        registry
            .set_focus(view, Some(ui), registry.revision())
            .expect("focus");
        registry
            .capture_pointer(ui, registry.revision())
            .expect("capture");
        registry.begin_ime(ui, registry.revision()).expect("ime");
        let report = registry.close_surface(ui).expect("close");
        assert_eq!(report.restored_focus, Some(game));
        assert_eq!(
            report.releases,
            vec![
                SurfaceRelease::ImeComposition,
                SurfaceRelease::PointerCapture,
                SurfaceRelease::Focus,
            ]
        );
        assert_eq!(
            registry.close_surface(ui),
            Err(CompositionError::StaleSurface)
        );
    }

    #[test]
    fn quotas_revision_conflict_and_generation_reuse_are_fail_closed() {
        let mut registry = CompositionRegistry::new(
            SessionHandle {
                index: 1,
                generation: 1,
            },
            1,
            CompositionLimits {
                max_windows: 1,
                max_views: 1,
                max_surfaces: 1,
                max_surfaces_per_view: 1,
                max_trace_entries: 1,
            },
        )
        .expect("registry");
        let window = registry.create_window().expect("window");
        assert_eq!(registry.create_window(), Err(CompositionError::Capacity));
        let view = registry.create_view(window).expect("view");
        let first = surface(
            &mut registry,
            view,
            SurfaceKind::Game,
            0,
            SurfaceInputPolicy::Interactive,
            false,
        );
        assert_eq!(
            registry.set_focus(view, Some(first), registry.revision() - 1),
            Err(CompositionError::RevisionConflict)
        );
        registry.close_surface(first).expect("close");
        let second = surface(
            &mut registry,
            view,
            SurfaceKind::Ui,
            1,
            SurfaceInputPolicy::Interactive,
            true,
        );
        assert_eq!(first.index, second.index);
        assert!(second.generation > first.generation);
        assert_eq!(
            registry.capture_pointer(first, registry.revision()),
            Err(CompositionError::StaleSurface)
        );
    }

    #[test]
    fn shutdown_closes_layers_views_and_windows_with_zero_live_resources() {
        let mut registry = registry();
        let first_window = registry.create_window().expect("first window");
        let second_window = registry.create_window().expect("second window");
        let first_view = registry.create_view(first_window).expect("first view");
        let second_view = registry.create_view(second_window).expect("second view");
        let game = surface(
            &mut registry,
            first_view,
            SurfaceKind::Game,
            0,
            SurfaceInputPolicy::Interactive,
            false,
        );
        let ui = surface(
            &mut registry,
            first_view,
            SurfaceKind::Ui,
            10,
            SurfaceInputPolicy::Interactive,
            true,
        );
        let diagnostics = surface(
            &mut registry,
            second_view,
            SurfaceKind::Diagnostics,
            20,
            SurfaceInputPolicy::Observe,
            false,
        );
        registry
            .set_focus(first_view, Some(ui), registry.revision())
            .expect("focus");
        registry
            .capture_pointer(ui, registry.revision())
            .expect("capture");
        registry.begin_ime(ui, registry.revision()).expect("ime");

        let report = registry.shutdown().expect("shutdown");
        assert_eq!(report.closed_surfaces.len(), 3);
        assert_eq!(report.closed_views, vec![first_view, second_view]);
        assert_eq!(report.closed_windows, vec![first_window, second_window]);
        assert_eq!(registry.live_surface_count(), 0);
        assert_eq!(registry.live_view_count(), 0);
        assert_eq!(registry.live_window_count(), 0);
        assert_eq!(
            registry.close_surface(game),
            Err(CompositionError::StaleSurface)
        );
        assert_eq!(
            registry.close_surface(ui),
            Err(CompositionError::StaleSurface)
        );
        assert_eq!(
            registry.close_surface(diagnostics),
            Err(CompositionError::StaleSurface)
        );
    }
}
