use std::fmt;
use std::num::NonZeroU64;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant};

use vo_app_host_native::{
    NativeInputEvent, NativeInputKind, NativeModifiers, NativePointerButton, NativeScrollUnit,
    WgpuCompositorAdapter, WgpuCompositorConfig,
};
use vo_app_protocol::{GenerationalHandle, SurfaceHandle, ViewHandle, WindowHandle};
use vo_app_runtime::{
    NativeCompositionFrame, NativeCompositionOutcome, NativeCompositorConfig,
    NativeCompositorError, NativeCompositorOwner, SurfaceInputPolicy,
};
use vo_engine::PreparedNativeUiReload;
use vo_ui_layout::{LayoutLimits, Size};
use vo_ui_paint::PaintLimits;
use vo_ui_present_wgpu::{WgpuScenePresenter, WgpuScenePresenterConfig, WgpuScenePresenterError};
#[cfg(any(target_os = "macos", target_os = "windows"))]
use vo_ui_system::{FileDragMode, FileDragRequest};
use vo_ui_system_native::{DesktopSystemBackend, HostInvocationHandler};
use vo_ui_text_native::{NativeTextConfig, NativeTextSystem};
use vo_vm::vm::Vm;
use winit::application::ApplicationHandler;
use winit::dpi::{LogicalPosition, LogicalSize, PhysicalPosition, PhysicalSize};
use winit::event::{ElementState, Ime, MouseButton, MouseScrollDelta, WindowEvent};
use winit::event_loop::{ActiveEventLoop, ControlFlow, EventLoop};
use winit::keyboard::{Key, PhysicalKey};
use winit::window::{Window, WindowAttributes, WindowId};

#[cfg(any(target_os = "macos", target_os = "windows"))]
use winit::raw_window_handle::{HasWindowHandle, RawWindowHandle};

use crate::{NativeUiRuntime, NativeUiRuntimeConfig};

const WINDOW_HANDLE: WindowHandle = GenerationalHandle {
    index: 1,
    generation: 1,
};
const VIEW_HANDLE: ViewHandle = GenerationalHandle {
    index: 1,
    generation: 1,
};
const UI_SURFACE_HANDLE: SurfaceHandle = GenerationalHandle {
    index: 1,
    generation: 1,
};
const DEVICE_GENERATION: u64 = 1;
const MAX_AUTOMATION_STEPS: usize = 1_024;
const MAX_AUTOMATION_SETTLE_TURNS: usize = 600;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeDesktopAutomation {
    pub clicks: Vec<String>,
    pub expected_text: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct NativeDesktopConfig {
    pub title: String,
    pub width_points: f64,
    pub height_points: f64,
    pub min_width_points: f64,
    pub min_height_points: f64,
    pub runtime: NativeUiRuntimeConfig,
    pub layout: LayoutLimits,
    pub paint: PaintLimits,
    pub text: NativeTextConfig,
    pub compositor: WgpuCompositorConfig,
    pub frame_interval: Duration,
    pub max_input_events_per_frame: usize,
    pub max_accessibility_actions_per_frame: usize,
    /// Ends the event loop after this many frames have reached the platform
    /// presentation backend. This is intended for automated window smoke tests.
    pub exit_after_presented_frames: Option<NonZeroU64>,
    /// Optional semantic interaction script used by packaged-application
    /// certification. Presentation waits until every click and final text
    /// assertion has succeeded.
    pub automation: Option<NativeDesktopAutomation>,
}

impl Default for NativeDesktopConfig {
    fn default() -> Self {
        Self {
            title: "Volang".to_string(),
            width_points: 960.0,
            height_points: 640.0,
            min_width_points: 320.0,
            min_height_points: 240.0,
            runtime: NativeUiRuntimeConfig::default(),
            layout: LayoutLimits::default(),
            paint: PaintLimits::default(),
            text: NativeTextConfig::default(),
            compositor: WgpuCompositorConfig::default(),
            frame_interval: Duration::from_micros(16_667),
            max_input_events_per_frame: 4_096,
            max_accessibility_actions_per_frame: 1_024,
            exit_after_presented_frames: None,
            automation: None,
        }
    }
}

#[derive(Debug)]
pub enum NativeDesktopError {
    InvalidConfig,
    EventLoop(String),
    Window(String),
    Graphics(String),
    Runtime(String),
    Presentation(String),
}

pub type NativeDesktopReloadPoll =
    Box<dyn FnMut() -> Option<Result<PreparedNativeUiReload, String>>>;

impl fmt::Display for NativeDesktopError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidConfig => formatter.write_str("native desktop configuration is invalid"),
            Self::EventLoop(error) => {
                write!(formatter, "native desktop event loop failed: {error}")
            }
            Self::Window(error) => write!(formatter, "native desktop window failed: {error}"),
            Self::Graphics(error) => write!(formatter, "native desktop graphics failed: {error}"),
            Self::Runtime(error) => write!(formatter, "native desktop runtime failed: {error}"),
            Self::Presentation(error) => {
                write!(formatter, "native desktop presentation failed: {error}")
            }
        }
    }
}

impl std::error::Error for NativeDesktopError {}

/// Runs one official desktop window until the user closes it. The same VM can
/// contain interpreter or JIT functions; native AOT installs its function
/// table before entering this shell.
pub fn run_desktop(vm: Vm, config: NativeDesktopConfig) -> Result<(), NativeDesktopError> {
    run_desktop_inner(vm, config, None, None)
}

/// Runs one desktop window with an application-specific typed protocol bridge.
/// The handler is installed before committed component effects can reach it.
pub fn run_desktop_with_host_invocation(
    vm: Vm,
    config: NativeDesktopConfig,
    host_invocation: HostInvocationHandler,
) -> Result<(), NativeDesktopError> {
    run_desktop_inner(vm, config, None, Some(host_invocation))
}

/// Runs the desktop shell with a development reload poll. A rejected build
/// leaves the mounted session interactive and publishes a window diagnostic.
pub fn run_desktop_with_reload(
    vm: Vm,
    config: NativeDesktopConfig,
    reload: NativeDesktopReloadPoll,
) -> Result<(), NativeDesktopError> {
    run_desktop_inner(vm, config, Some(reload), None)
}

/// Runs the development desktop shell while retaining the application service
/// handler across verified VM reloads.
pub fn run_desktop_with_reload_and_host_invocation(
    vm: Vm,
    config: NativeDesktopConfig,
    reload: NativeDesktopReloadPoll,
    host_invocation: HostInvocationHandler,
) -> Result<(), NativeDesktopError> {
    run_desktop_inner(vm, config, Some(reload), Some(host_invocation))
}

fn run_desktop_inner(
    vm: Vm,
    config: NativeDesktopConfig,
    reload: Option<NativeDesktopReloadPoll>,
    host_invocation: Option<HostInvocationHandler>,
) -> Result<(), NativeDesktopError> {
    validate_config(&config)?;
    let mut event_loop_builder = EventLoop::builder();
    #[cfg(target_os = "macos")]
    {
        use winit::platform::macos::{ActivationPolicy, EventLoopBuilderExtMacOS};
        event_loop_builder
            .with_activation_policy(ActivationPolicy::Regular)
            .with_activate_ignoring_other_apps(true);
    }
    let event_loop = event_loop_builder
        .build()
        .map_err(|error| NativeDesktopError::EventLoop(error.to_string()))?;
    event_loop.set_control_flow(ControlFlow::Wait);
    let mut app = DesktopApplication::new(vm, config, reload, host_invocation);
    event_loop
        .run_app(&mut app)
        .map_err(|error| NativeDesktopError::EventLoop(error.to_string()))?;
    app.error.map_or(Ok(()), Err)
}

fn validate_config(config: &NativeDesktopConfig) -> Result<(), NativeDesktopError> {
    let sizes = [
        config.width_points,
        config.height_points,
        config.min_width_points,
        config.min_height_points,
    ];
    if sizes
        .iter()
        .any(|value| !value.is_finite() || *value <= 0.0)
        || config.frame_interval.is_zero()
        || config.max_input_events_per_frame == 0
        || config.max_accessibility_actions_per_frame == 0
        || config.automation.as_ref().is_some_and(|automation| {
            automation.clicks.is_empty()
                || automation.clicks.len() > MAX_AUTOMATION_STEPS
                || automation.expected_text.is_empty()
                || automation.expected_text.len() > MAX_AUTOMATION_STEPS
                || automation.clicks.iter().any(String::is_empty)
                || automation.expected_text.iter().any(String::is_empty)
        })
    {
        return Err(NativeDesktopError::InvalidConfig);
    }
    Ok(())
}

struct DesktopApplication {
    vm: Option<Vm>,
    config: NativeDesktopConfig,
    reload: Option<NativeDesktopReloadPoll>,
    host_invocation: Option<HostInvocationHandler>,
    state: Option<DesktopState>,
    error: Option<NativeDesktopError>,
}

impl DesktopApplication {
    fn new(
        vm: Vm,
        config: NativeDesktopConfig,
        reload: Option<NativeDesktopReloadPoll>,
        host_invocation: Option<HostInvocationHandler>,
    ) -> Self {
        Self {
            vm: Some(vm),
            config,
            reload,
            host_invocation,
            state: None,
            error: None,
        }
    }

    fn fail(&mut self, event_loop: &ActiveEventLoop, error: NativeDesktopError) {
        if self.error.is_none() {
            self.error = Some(error);
        }
        event_loop.exit();
    }
}

impl ApplicationHandler for DesktopApplication {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.state.is_some() {
            return;
        }
        let Some(vm) = self.vm.take() else {
            self.fail(
                event_loop,
                NativeDesktopError::Runtime("desktop VM was already consumed".to_string()),
            );
            return;
        };
        match DesktopState::new(
            event_loop,
            vm,
            self.config.clone(),
            self.reload.take(),
            self.host_invocation.take(),
        ) {
            Ok(state) => self.state = Some(state),
            Err(error) => self.fail(event_loop, error),
        }
    }

    fn window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        window_id: WindowId,
        event: WindowEvent,
    ) {
        let Some(state) = self.state.as_mut() else {
            return;
        };
        if state.window.id() != window_id {
            return;
        }
        if let Err(error) = state.handle_window_event(event_loop, event) {
            self.fail(event_loop, error);
        }
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) {
        let Some(state) = self.state.as_mut() else {
            return;
        };
        if let Err(error) = state.about_to_wait(event_loop) {
            self.fail(event_loop, error);
        }
    }
}

struct DesktopState {
    window: Arc<Window>,
    gpu_adapter: Arc<wgpu::Adapter>,
    runtime: NativeUiRuntime<DesktopSystemBackend>,
    text: NativeTextSystem,
    compositor: NativeCompositorOwner<WgpuCompositorAdapter<'static>>,
    presenter: WgpuScenePresenter,
    config: NativeDesktopConfig,
    started: Instant,
    last_frame: Instant,
    next_input_sequence: u64,
    input_events_since_pump: usize,
    next_pulse: u64,
    presented_frames: u64,
    device_generation: u64,
    pointer: PhysicalPosition<f64>,
    modifiers: NativeModifiers,
    hovered_files: Vec<PathBuf>,
    pending_dropped_files: Vec<PathBuf>,
    physical_size: PhysicalSize<u32>,
    scale_factor: f64,
    visible: bool,
    dirty: bool,
    accessibility: PlatformAccessibility,
    reload: Option<NativeDesktopReloadPoll>,
    reload_diagnostic: Option<String>,
    automation_cursor: usize,
    automation_settle_turns: usize,
    automation_validated: bool,
    #[cfg(target_os = "windows")]
    attached_menu_revision: u64,
}

impl DesktopState {
    fn new(
        event_loop: &ActiveEventLoop,
        vm: Vm,
        config: NativeDesktopConfig,
        reload: Option<NativeDesktopReloadPoll>,
        host_invocation: Option<HostInvocationHandler>,
    ) -> Result<Self, NativeDesktopError> {
        automation_log("creating native window and GPU surface");
        let attributes = WindowAttributes::default()
            .with_title(config.title.clone())
            .with_visible(false)
            .with_inner_size(LogicalSize::new(config.width_points, config.height_points))
            .with_min_inner_size(LogicalSize::new(
                config.min_width_points,
                config.min_height_points,
            ));
        let window = Arc::new(
            event_loop
                .create_window(attributes)
                .map_err(|error| NativeDesktopError::Window(error.to_string()))?,
        );
        let physical_size = non_zero_size(window.inner_size());
        let scale_factor = window.scale_factor();

        let instance = wgpu::Instance::default();
        let surface = instance
            .create_surface(Arc::clone(&window))
            .map_err(|error| NativeDesktopError::Graphics(error.to_string()))?;
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::HighPerformance,
            force_fallback_adapter: false,
            compatible_surface: Some(&surface),
        }))
        .ok_or_else(|| NativeDesktopError::Graphics("no compatible GPU adapter".to_string()))?;
        let (device, queue) =
            pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor::default(), None))
                .map_err(|error| NativeDesktopError::Graphics(error.to_string()))?;
        let adapter = Arc::new(adapter);
        let device = Arc::new(device);
        let queue = Arc::new(queue);
        let mut gpu = WgpuCompositorAdapter::new(
            config.compositor,
            Arc::clone(&adapter),
            device,
            queue,
            DEVICE_GENERATION,
        )
        .map_err(|error| NativeDesktopError::Graphics(format!("{error:?}")))?;
        gpu.register_view_surface(
            VIEW_HANDLE,
            surface,
            physical_size.width,
            physical_size.height,
        )
        .map_err(|error| NativeDesktopError::Graphics(format!("{error:?}")))?;
        let mut compositor = NativeCompositorOwner::new(NativeCompositorConfig::default(), gpu)
            .map_err(|error| NativeDesktopError::Graphics(format!("{error:?}")))?;
        compositor
            .attach_view(VIEW_HANDLE, DEVICE_GENERATION)
            .map_err(|error| NativeDesktopError::Graphics(format!("{error:?}")))?;
        let presenter = WgpuScenePresenter::new(WgpuScenePresenterConfig {
            surface: UI_SURFACE_HANDLE,
            z_order: 0,
            input: SurfaceInputPolicy::Exclusive,
        })
        .map_err(|error| NativeDesktopError::Presentation(error.to_string()))?;
        let now = Instant::now();
        let backend = desktop_system_backend(&window);
        let logical = physical_size.to_logical::<f64>(scale_factor);
        vo_ui_vm::set_platform_viewport(logical.width, logical.height, scale_factor, false)
            .map_err(|error| NativeDesktopError::Runtime(error.to_string()))?;
        automation_log("starting UI VM session");
        let (runtime, _) = match host_invocation {
            Some(handler) => NativeUiRuntime::start_with_host_invocation(
                vm,
                backend,
                WINDOW_HANDLE,
                VIEW_HANDLE,
                config.runtime,
                now,
                handler,
            ),
            None => {
                NativeUiRuntime::start(vm, backend, WINDOW_HANDLE, VIEW_HANDLE, config.runtime, now)
            }
        }
        .map_err(|error| NativeDesktopError::Runtime(error.to_string()))?;
        automation_log("UI VM session published its initial revision");
        let text = NativeTextSystem::new(config.text)
            .map_err(|error| NativeDesktopError::Presentation(error.to_string()))?;
        let mut text = text;
        let initial = runtime.session().renderer().host().revision();
        let mut runtime = runtime;
        let initial_frame = runtime
            .session_mut()
            .renderer_mut()
            .host_mut()
            .prepare_frame(
                Size::new(logical.width, logical.height),
                scale_factor as f32,
                config.layout,
                config.paint,
                &mut text,
            )
            .map_err(|error| NativeDesktopError::Presentation(error.to_string()))?;
        debug_assert_eq!(initial_frame.presentation.revision, initial);
        let mut accessibility = PlatformAccessibility::install(
            &window,
            initial_frame.accesskit_full,
            config.max_accessibility_actions_per_frame,
        )?;
        accessibility.set_focus(true);
        accessibility.update_bounds(&window)?;
        window.set_visible(true);
        window.request_redraw();
        let automation_validated = config.automation.is_none();
        Ok(Self {
            window,
            gpu_adapter: adapter,
            runtime,
            text,
            compositor,
            presenter,
            config,
            started: now,
            last_frame: now,
            next_input_sequence: 1,
            input_events_since_pump: 0,
            next_pulse: 1,
            presented_frames: 0,
            device_generation: DEVICE_GENERATION,
            pointer: PhysicalPosition::new(0.0, 0.0),
            modifiers: NativeModifiers::default(),
            hovered_files: Vec::new(),
            pending_dropped_files: Vec::new(),
            physical_size,
            scale_factor,
            visible: true,
            dirty: true,
            accessibility,
            reload,
            reload_diagnostic: None,
            automation_cursor: 0,
            automation_settle_turns: 0,
            automation_validated,
            #[cfg(target_os = "windows")]
            attached_menu_revision: 0,
        })
    }

    fn handle_window_event(
        &mut self,
        event_loop: &ActiveEventLoop,
        event: WindowEvent,
    ) -> Result<(), NativeDesktopError> {
        match event {
            WindowEvent::CloseRequested => {
                self.route(NativeInputKind::CloseRequested)?;
                event_loop.exit();
            }
            WindowEvent::RedrawRequested => {
                if self.automation_validated && self.draw()? {
                    event_loop.exit();
                }
            }
            WindowEvent::Resized(size) => self.resize(size)?,
            WindowEvent::ScaleFactorChanged { scale_factor, .. } => {
                self.scale_factor = scale_factor;
                self.resize(self.window.inner_size())?;
            }
            WindowEvent::Focused(focused) => {
                self.accessibility.set_focus(focused);
                self.route(NativeInputKind::FocusChanged(focused))?;
            }
            WindowEvent::Occluded(occluded) => {
                self.visible = !occluded;
                self.route(NativeInputKind::VisibilityChanged(!occluded))?;
            }
            WindowEvent::CursorMoved { position, .. } => {
                let old = self.pointer;
                self.pointer = position;
                let (x_milli, y_milli) = self.pointer_milli(position);
                let (old_x, old_y) = self.pointer_milli(old);
                self.route(NativeInputKind::PointerMoved {
                    device: 1,
                    x_milli,
                    y_milli,
                    delta_x_milli: x_milli.saturating_sub(old_x),
                    delta_y_milli: y_milli.saturating_sub(old_y),
                    pressure_milli: 0,
                })?;
            }
            WindowEvent::MouseInput { state, button, .. } => {
                let (x_milli, y_milli) = self.pointer_milli(self.pointer);
                self.route(NativeInputKind::PointerButton {
                    device: 1,
                    button: pointer_button(button),
                    pressed: state == ElementState::Pressed,
                    click_count: 1,
                    x_milli,
                    y_milli,
                })?;
            }
            WindowEvent::MouseWheel { delta, .. } => {
                let (x_milli, y_milli) = self.pointer_milli(self.pointer);
                let (delta_x_milli, delta_y_milli, unit) = match delta {
                    MouseScrollDelta::LineDelta(x, y) => (
                        milli(f64::from(x)),
                        milli(f64::from(y)),
                        NativeScrollUnit::Line,
                    ),
                    MouseScrollDelta::PixelDelta(position) => (
                        milli(position.x / self.scale_factor),
                        milli(position.y / self.scale_factor),
                        NativeScrollUnit::Pixel,
                    ),
                };
                self.route(NativeInputKind::Wheel {
                    device: 1,
                    x_milli,
                    y_milli,
                    delta_x_milli,
                    delta_y_milli,
                    unit,
                })?;
            }
            WindowEvent::ModifiersChanged(modifiers) => {
                self.modifiers = NativeModifiers {
                    shift: modifiers.state().shift_key(),
                    control: modifiers.state().control_key(),
                    alt: modifiers.state().alt_key(),
                    meta: modifiers.state().super_key(),
                    caps_lock: false,
                    function: false,
                };
                self.route(NativeInputKind::ModifiersChanged(self.modifiers))?;
            }
            WindowEvent::KeyboardInput { event, .. } => {
                let pressed = event.state == ElementState::Pressed;
                let physical_key = physical_key(&event.physical_key);
                let logical_key = logical_key(&event.logical_key);
                self.route(NativeInputKind::Key {
                    device: 1,
                    physical_key,
                    logical_key,
                    pressed,
                    repeat: event.repeat,
                    modifiers: self.modifiers,
                })?;
                if pressed {
                    if let Some(text) = event.text.filter(|text| !text.is_empty()) {
                        self.route(NativeInputKind::Text(text.to_string()))?;
                    }
                }
            }
            WindowEvent::Ime(ime) => match ime {
                Ime::Enabled => self.route(NativeInputKind::ImeStarted)?,
                Ime::Preedit(text, selection) => {
                    let (start, length) = selection
                        .map(|(start, end)| (start, end.saturating_sub(start)))
                        .unwrap_or((0, 0));
                    self.route(NativeInputKind::ImeUpdated {
                        text,
                        selection_start_utf16: u32::try_from(start).unwrap_or(u32::MAX),
                        selection_len_utf16: u32::try_from(length).unwrap_or(u32::MAX),
                    })?;
                }
                Ime::Commit(text) => self.route(NativeInputKind::ImeCommitted(text))?,
                Ime::Disabled => self.route(NativeInputKind::ImeCancelled)?,
            },
            WindowEvent::HoveredFile(path) => {
                push_unique_path(&mut self.hovered_files, path);
                let (x_milli, y_milli) = self.pointer_milli(self.pointer);
                self.route(NativeInputKind::FileDragEntered {
                    x_milli,
                    y_milli,
                    paths: display_paths(&self.hovered_files),
                })?;
            }
            WindowEvent::HoveredFileCancelled => {
                self.hovered_files.clear();
                if self.pending_dropped_files.is_empty() {
                    self.route(NativeInputKind::FileDragLeft)?;
                }
            }
            WindowEvent::DroppedFile(path) => {
                for hovered in self.hovered_files.drain(..) {
                    push_unique_path(&mut self.pending_dropped_files, hovered);
                }
                push_unique_path(&mut self.pending_dropped_files, path);
            }
            _ => {}
        }
        Ok(())
    }

    fn about_to_wait(&mut self, event_loop: &ActiveEventLoop) -> Result<(), NativeDesktopError> {
        let now = Instant::now();
        self.poll_reload(now);
        self.flush_file_drop_batch()?;
        for action in self
            .accessibility
            .drain_actions(self.config.max_accessibility_actions_per_frame)?
        {
            if self
                .runtime
                .session_mut()
                .renderer_mut()
                .host_mut()
                .route_accessibility_action(&action)
                .map_err(|error| NativeDesktopError::Runtime(error.to_string()))?
            {
                self.dirty = true;
            }
        }
        let elapsed = now.saturating_duration_since(self.last_frame);
        if self
            .runtime
            .session_mut()
            .renderer_mut()
            .host_mut()
            .advance_scroll_physics(elapsed)
        {
            self.dirty = true;
        }
        let report = self
            .runtime
            .pump(now)
            .map_err(|error| NativeDesktopError::Runtime(error.to_string()))?;
        self.input_events_since_pump = 0;
        self.dirty |= report.session.applied_frames > 0
            || report.session.delivered_events > 0
            || report.session.woken_timers > 0
            || report.session.completed_system_requests > 0;
        self.attach_latest_menu()?;
        self.advance_automation()?;
        if self.visible && self.dirty {
            if !self.automation_validated {
                let next_frame = now + self.config.frame_interval;
                event_loop.set_control_flow(ControlFlow::WaitUntil(next_frame));
                return Ok(());
            }
            if self.config.exit_after_presented_frames.is_some() {
                if self.draw()? {
                    event_loop.exit();
                    return Ok(());
                }
                // Automation may request more than one certified frame. Keep
                // the frame cadence active until the exact present count is
                // reached even when application state stays unchanged.
                self.dirty = true;
            } else {
                self.window.request_redraw();
            }
        }
        let next_frame = now + self.config.frame_interval;
        let wake = self
            .runtime
            .session()
            .next_timer_deadline()
            .map_or(next_frame, |deadline| deadline.min(next_frame));
        event_loop.set_control_flow(ControlFlow::WaitUntil(wake));
        Ok(())
    }

    fn advance_automation(&mut self) -> Result<(), NativeDesktopError> {
        if self.automation_validated {
            return Ok(());
        }
        let automation = self
            .config
            .automation
            .as_ref()
            .expect("pending automation has a script");
        if let Some(name) = automation.clicks.get(self.automation_cursor).cloned() {
            let node = self
                .runtime
                .session()
                .renderer()
                .host()
                .tree()
                .nodes()
                .find(|node| {
                    (node.listeners.contains_key(&vo_ui_core::EventType::CLICK)
                        || node.listeners.contains_key(&vo_ui_core::EventType::CHANGE))
                        && node
                            .properties
                            .get(&vo_ui_core::PropertyId::ACCESSIBLE_NAME)
                            == Some(&vo_ui_core::Value::Text(name.clone()))
                })
                .map(|node| node.id)
                .ok_or_else(|| {
                    NativeDesktopError::Runtime(format!(
                        "automation click target {name:?} is missing"
                    ))
                })?;
            let invoked = self
                .runtime
                .session_mut()
                .renderer_mut()
                .host_mut()
                .route_semantic_invoke(node)
                .map_err(|error| NativeDesktopError::Runtime(error.to_string()))?;
            if !invoked {
                return Err(NativeDesktopError::Runtime(format!(
                    "automation click target {name:?} is not invokable"
                )));
            }
            self.automation_cursor += 1;
            self.automation_settle_turns = 0;
            automation_log(&format!("completed semantic click {name:?}"));
            self.dirty = true;
            return Ok(());
        }
        let missing = automation
            .expected_text
            .iter()
            .find(|expected| {
                !self
                    .runtime
                    .session()
                    .renderer()
                    .host()
                    .tree()
                    .nodes()
                    .any(|node| node.text.as_str() == expected.as_str())
            })
            .cloned();
        if let Some(expected) = missing {
            self.automation_settle_turns = self.automation_settle_turns.saturating_add(1);
            if self.automation_settle_turns > MAX_AUTOMATION_SETTLE_TURNS {
                let visible = self
                    .runtime
                    .session()
                    .renderer()
                    .host()
                    .tree()
                    .nodes()
                    .filter(|node| !node.text.is_empty())
                    .take(96)
                    .map(|node| node.text.clone())
                    .collect::<Vec<_>>();
                return Err(NativeDesktopError::Runtime(format!(
                    "automation expected text {expected:?} is missing after {MAX_AUTOMATION_SETTLE_TURNS} settle turns; visible text: {visible:?}"
                )));
            }
            let logical = self.physical_size.to_logical::<f64>(self.scale_factor);
            let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
            self.runtime
                .session_mut()
                .renderer_mut()
                .host_mut()
                .compute_and_set_layout(
                    Size::new(logical.width, logical.height),
                    self.config.layout,
                    &mut measurer,
                )
                .map_err(|error| NativeDesktopError::Runtime(error.to_string()))?;
            // Effects and other structured workers publish through a later
            // host-event turn. Layout observation also needs a post-update
            // layout pass before its feedback event can enter that turn.
            self.dirty = true;
            return Ok(());
        }
        self.automation_validated = true;
        automation_log("semantic interaction script and final assertions passed");
        self.dirty = true;
        Ok(())
    }

    fn flush_file_drop_batch(&mut self) -> Result<(), NativeDesktopError> {
        if self.pending_dropped_files.is_empty() {
            return Ok(());
        }
        let (x_milli, y_milli) = self.pointer_milli(self.pointer);
        let paths = display_paths(&self.pending_dropped_files);
        self.pending_dropped_files.clear();
        self.route(NativeInputKind::FileDropped {
            x_milli,
            y_milli,
            paths,
        })
    }

    fn poll_reload(&mut self, now: Instant) {
        let Some(result) = self.reload.as_mut().and_then(|reload| reload()) else {
            return;
        };
        match result {
            Ok(prepared) => match self.runtime.reload(prepared, now) {
                Ok(report) => {
                    self.reload_diagnostic = None;
                    self.window.set_title(&self.config.title);
                    self.dirty = true;
                    self.dirty |= report.session.applied_frames > 0;
                    eprintln!("Volang UI desktop reload succeeded");
                }
                Err(error) => self.publish_reload_error(error.to_string()),
            },
            Err(error) => self.publish_reload_error(error),
        }
    }

    fn publish_reload_error(&mut self, error: String) {
        if self.reload_diagnostic.as_deref() == Some(error.as_str()) {
            return;
        }
        self.window
            .set_title(&format!("{} [build error]", self.config.title));
        eprintln!("Volang UI desktop reload failed: {error}");
        self.reload_diagnostic = Some(error);
    }

    fn route(&mut self, kind: NativeInputKind) -> Result<(), NativeDesktopError> {
        if self.input_events_since_pump == self.config.max_input_events_per_frame {
            return Err(NativeDesktopError::Runtime(
                "native input exceeded the bounded per-pump limit".to_string(),
            ));
        }
        self.input_events_since_pump += 1;
        let sequence = self.next_input_sequence;
        self.next_input_sequence = sequence
            .checked_add(1)
            .ok_or_else(|| NativeDesktopError::Runtime("input sequence exhausted".to_string()))?;
        let micros = self.started.elapsed().as_micros().min(u128::from(u64::MAX)) as u64;
        let report = self
            .runtime
            .route_input(&NativeInputEvent {
                sequence,
                timestamp_micros: micros,
                window: WINDOW_HANDLE,
                view: VIEW_HANDLE,
                kind,
            })
            .map_err(|error| NativeDesktopError::Runtime(error.to_string()))?;
        self.dirty |= report.routed_input_events > 0 || report.routed_system_events > 0;
        Ok(())
    }

    fn resize(&mut self, size: PhysicalSize<u32>) -> Result<(), NativeDesktopError> {
        self.physical_size = size;
        if size.width == 0 || size.height == 0 {
            self.dirty = false;
            return Ok(());
        }
        self.compositor
            .adapter_mut()
            .and_then(|adapter| {
                adapter.resize_view(
                    VIEW_HANDLE,
                    self.physical_size.width,
                    self.physical_size.height,
                )
            })
            .map_err(|error| NativeDesktopError::Graphics(format!("{error:?}")))?;
        let logical = self.physical_size.to_logical::<f64>(self.scale_factor);
        vo_ui_vm::set_platform_viewport(logical.width, logical.height, self.scale_factor, true)
            .map_err(|error| NativeDesktopError::Runtime(error.to_string()))?;
        self.route(NativeInputKind::Resized {
            width_milli: unsigned_milli(logical.width),
            height_milli: unsigned_milli(logical.height),
            scale_milli: unsigned_milli(self.scale_factor),
        })?;
        self.accessibility.update_bounds(&self.window)?;
        self.dirty = true;
        Ok(())
    }

    fn draw(&mut self) -> Result<bool, NativeDesktopError> {
        automation_log("preparing certified native frame");
        if !self.visible || self.physical_size.width == 0 || self.physical_size.height == 0 {
            return Ok(false);
        }
        let now = Instant::now();
        let logical = self.physical_size.to_logical::<f64>(self.scale_factor);
        let prepared = self
            .runtime
            .session_mut()
            .renderer_mut()
            .host_mut()
            .prepare_frame(
                Size::new(logical.width, logical.height),
                self.scale_factor as f32,
                self.config.layout,
                self.config.paint,
                &mut self.text,
            )
            .map_err(|error| NativeDesktopError::Presentation(error.to_string()))?;
        let focused = self.runtime.session().renderer().host().focused_node();
        let text_input = focused.is_some_and(|node| {
            self.runtime
                .session()
                .renderer()
                .host()
                .tree()
                .node(node)
                .is_some_and(|node| {
                    matches!(
                        node.kind,
                        vo_ui_protocol::NodeKind::Element(
                            vo_ui_core::Primitive::TextInput | vo_ui_core::Primitive::TextArea
                        )
                    )
                })
        });
        self.window.set_ime_allowed(text_input);
        if let Some(layout) = focused
            .filter(|_| text_input)
            .and_then(|node| prepared.layout.get(node))
        {
            self.window.set_ime_cursor_area(
                LogicalPosition::new(layout.content.x, layout.content.y + layout.content.height),
                LogicalSize::new(1.0, layout.content.height.max(1.0)),
            );
        }
        self.accessibility
            .update(prepared.accesskit, prepared.accesskit_full)?;
        let pulse = self.next_pulse;
        let next_pulse = pulse
            .checked_add(1)
            .ok_or_else(|| NativeDesktopError::Presentation("frame pulse exhausted".to_string()))?;
        let mut recovered_device = false;
        loop {
            let layer = {
                let adapter = self.compositor.adapter_mut().map_err(graphics_error)?;
                match self.presenter.upload(adapter, &prepared.presentation) {
                    Ok(layer) => layer,
                    Err(WgpuScenePresenterError::Upload(NativeCompositorError::DeviceLost))
                        if !recovered_device =>
                    {
                        self.recover_device()?;
                        recovered_device = true;
                        continue;
                    }
                    Err(error) => {
                        return Err(NativeDesktopError::Presentation(error.to_string()));
                    }
                }
            };
            let frame = NativeCompositionFrame {
                view: VIEW_HANDLE,
                pulse_id: pulse,
                device_generation: self.device_generation,
                viewport_width_milli: unsigned_milli(logical.width),
                viewport_height_milli: unsigned_milli(logical.height),
                layers: vec![layer],
            };
            let fence = match self.compositor.submit(frame.clone()) {
                Ok(fence) => fence,
                Err(NativeCompositorError::SurfaceLost) => {
                    self.recover_surface()?;
                    match self.compositor.submit(frame) {
                        Ok(fence) => fence,
                        Err(NativeCompositorError::DeviceLost) if !recovered_device => {
                            self.recover_device()?;
                            recovered_device = true;
                            continue;
                        }
                        Err(error) => return Err(graphics_error(error)),
                    }
                }
                Err(NativeCompositorError::DeviceLost) if !recovered_device => {
                    self.recover_device()?;
                    recovered_device = true;
                    continue;
                }
                Err(error) => return Err(graphics_error(error)),
            };
            let now_micros = self.started.elapsed().as_micros().min(u128::from(u64::MAX)) as u64;
            let deadline_micros = now_micros.saturating_add(
                self.config
                    .frame_interval
                    .as_micros()
                    .min(u128::from(u64::MAX)) as u64,
            );
            let outcome = self
                .compositor
                .present(fence, now_micros, deadline_micros)
                .map_err(graphics_error)?;
            if !matches!(
                outcome,
                NativeCompositionOutcome::Presented | NativeCompositionOutcome::DeadlineMissed
            ) {
                return Err(NativeDesktopError::Graphics(format!(
                    "unexpected composition outcome {outcome:?}"
                )));
            }
            break;
        }
        self.next_pulse = next_pulse;
        self.presented_frames = self.presented_frames.saturating_add(1);
        automation_log("certified native frame reached the presentation boundary");
        self.last_frame = now;
        self.dirty = false;
        Ok(presentation_limit_reached(
            self.config.exit_after_presented_frames,
            self.presented_frames,
        ))
    }

    fn recover_surface(&mut self) -> Result<(), NativeDesktopError> {
        self.compositor
            .adapter_mut()
            .and_then(|adapter| {
                adapter.resize_view(
                    VIEW_HANDLE,
                    self.physical_size.width,
                    self.physical_size.height,
                )
            })
            .map_err(graphics_error)
    }

    fn recover_device(&mut self) -> Result<(), NativeDesktopError> {
        let generation = self.device_generation.checked_add(1).ok_or_else(|| {
            NativeDesktopError::Graphics("GPU device generation exhausted".to_string())
        })?;
        let (device, queue) = pollster::block_on(
            self.gpu_adapter
                .request_device(&wgpu::DeviceDescriptor::default(), None),
        )
        .map_err(|error| NativeDesktopError::Graphics(error.to_string()))?;
        self.compositor
            .adapter_mut()
            .and_then(|adapter| adapter.stage_device(Arc::new(device), Arc::new(queue), generation))
            .map_err(graphics_error)?;
        self.compositor
            .rebind_device(generation)
            .map_err(graphics_error)?;
        self.presenter.reset_after_device_loss();
        self.device_generation = generation;
        Ok(())
    }

    fn pointer_milli(&self, position: PhysicalPosition<f64>) -> (i32, i32) {
        (
            milli(position.x / self.scale_factor),
            milli(position.y / self.scale_factor),
        )
    }

    #[cfg(target_os = "windows")]
    fn attach_latest_menu(&mut self) -> Result<(), NativeDesktopError> {
        let revision = self.runtime.system().installed_menu_revision();
        if revision == 0 || revision == self.attached_menu_revision {
            return Ok(());
        }
        let handle = self
            .window
            .window_handle()
            .map_err(|error| NativeDesktopError::Window(error.to_string()))?;
        let RawWindowHandle::Win32(handle) = handle.as_raw() else {
            return Err(NativeDesktopError::Window(
                "winit did not expose a Win32 HWND".to_string(),
            ));
        };
        // SAFETY: this event-loop thread owns the live winit HWND, and the
        // backend retains the installed menu for at least the same revision.
        unsafe {
            self.runtime
                .system()
                .backend()
                .attach_menu_to_hwnd(handle.hwnd.get())
        }
        .map_err(|error| NativeDesktopError::Window(error.to_string()))?;
        self.attached_menu_revision = revision;
        Ok(())
    }

    #[cfg(not(target_os = "windows"))]
    fn attach_latest_menu(&mut self) -> Result<(), NativeDesktopError> {
        Ok(())
    }
}

fn graphics_error(error: NativeCompositorError) -> NativeDesktopError {
    NativeDesktopError::Graphics(format!("{error:?}"))
}

#[cfg(target_os = "macos")]
struct PlatformAccessibility(vo_ui_accesskit::MacOsAccessKitAdapter);

#[cfg(target_os = "windows")]
struct PlatformAccessibility(vo_ui_accesskit::WindowsAccessKitAdapter);

#[cfg(all(unix, not(target_vendor = "apple")))]
struct PlatformAccessibility(vo_ui_accesskit::UnixAccessKitAdapter);

#[cfg(target_os = "macos")]
impl PlatformAccessibility {
    fn install(
        window: &Window,
        initial: accesskit::TreeUpdate,
        max_actions: usize,
    ) -> Result<Self, NativeDesktopError> {
        let handle = window
            .window_handle()
            .map_err(|error| NativeDesktopError::Window(error.to_string()))?;
        let RawWindowHandle::AppKit(handle) = handle.as_raw() else {
            return Err(NativeDesktopError::Window(
                "winit did not expose an AppKit view".to_string(),
            ));
        };
        // SAFETY: winit owns this hidden NSView for the full adapter lifetime,
        // and installation runs on the AppKit event-loop thread before show.
        let adapter = unsafe {
            vo_ui_accesskit::MacOsAccessKitAdapter::install(
                handle.ns_view.as_ptr(),
                initial,
                max_actions,
            )
        }
        .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))?;
        Ok(Self(adapter))
    }

    fn update(
        &mut self,
        incremental: accesskit::TreeUpdate,
        full: accesskit::TreeUpdate,
    ) -> Result<(), NativeDesktopError> {
        self.0
            .update(incremental, full)
            .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))
    }

    fn set_focus(&mut self, focused: bool) {
        self.0.update_view_focus_state(focused);
    }

    fn update_bounds(&mut self, _window: &Window) -> Result<(), NativeDesktopError> {
        Ok(())
    }

    fn drain_actions(
        &mut self,
        max: usize,
    ) -> Result<Vec<accesskit::ActionRequest>, NativeDesktopError> {
        self.0
            .drain_actions(max)
            .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))
    }
}

#[cfg(target_os = "windows")]
impl PlatformAccessibility {
    fn install(
        window: &Window,
        initial: accesskit::TreeUpdate,
        max_actions: usize,
    ) -> Result<Self, NativeDesktopError> {
        let handle = window
            .window_handle()
            .map_err(|error| NativeDesktopError::Window(error.to_string()))?;
        let RawWindowHandle::Win32(handle) = handle.as_raw() else {
            return Err(NativeDesktopError::Window(
                "winit did not expose a Win32 HWND".to_string(),
            ));
        };
        // SAFETY: winit owns this hidden HWND for the full adapter lifetime,
        // and installation runs on its window thread before show.
        let adapter = unsafe {
            vo_ui_accesskit::WindowsAccessKitAdapter::install(
                handle.hwnd.get() as *mut std::ffi::c_void,
                initial,
                max_actions,
            )
        }
        .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))?;
        Ok(Self(adapter))
    }

    fn update(
        &mut self,
        incremental: accesskit::TreeUpdate,
        full: accesskit::TreeUpdate,
    ) -> Result<(), NativeDesktopError> {
        self.0
            .update(incremental, full)
            .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))
    }

    fn set_focus(&mut self, _focused: bool) {}

    fn update_bounds(&mut self, _window: &Window) -> Result<(), NativeDesktopError> {
        Ok(())
    }

    fn drain_actions(
        &mut self,
        max: usize,
    ) -> Result<Vec<accesskit::ActionRequest>, NativeDesktopError> {
        self.0
            .drain_actions(max)
            .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))
    }
}

#[cfg(all(unix, not(target_vendor = "apple")))]
impl PlatformAccessibility {
    fn install(
        _window: &Window,
        initial: accesskit::TreeUpdate,
        max_actions: usize,
    ) -> Result<Self, NativeDesktopError> {
        vo_ui_accesskit::UnixAccessKitAdapter::new(initial, max_actions)
            .map(Self)
            .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))
    }

    fn update(
        &mut self,
        incremental: accesskit::TreeUpdate,
        full: accesskit::TreeUpdate,
    ) -> Result<(), NativeDesktopError> {
        self.0
            .update(incremental, full)
            .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))
    }

    fn set_focus(&mut self, focused: bool) {
        self.0.update_window_focus_state(focused);
    }

    fn update_bounds(&mut self, window: &Window) -> Result<(), NativeDesktopError> {
        let outer_position = window
            .outer_position()
            .map_err(|error| NativeDesktopError::Window(error.to_string()))?;
        let inner_position = window
            .inner_position()
            .map_err(|error| NativeDesktopError::Window(error.to_string()))?;
        let outer_size = window.outer_size();
        let inner_size = window.inner_size();
        self.0
            .set_root_window_bounds(
                accesskit::Rect {
                    x0: f64::from(outer_position.x),
                    y0: f64::from(outer_position.y),
                    x1: f64::from(outer_position.x) + f64::from(outer_size.width),
                    y1: f64::from(outer_position.y) + f64::from(outer_size.height),
                },
                accesskit::Rect {
                    x0: f64::from(inner_position.x),
                    y0: f64::from(inner_position.y),
                    x1: f64::from(inner_position.x) + f64::from(inner_size.width),
                    y1: f64::from(inner_position.y) + f64::from(inner_size.height),
                },
            )
            .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))
    }

    fn drain_actions(
        &mut self,
        max: usize,
    ) -> Result<Vec<accesskit::ActionRequest>, NativeDesktopError> {
        self.0
            .drain_actions(max)
            .map_err(|error| NativeDesktopError::Window(format!("{error:?}")))
    }
}

fn pointer_button(button: MouseButton) -> NativePointerButton {
    match button {
        MouseButton::Left => NativePointerButton::Primary,
        MouseButton::Right => NativePointerButton::Secondary,
        MouseButton::Middle => NativePointerButton::Middle,
        MouseButton::Back => NativePointerButton::Auxiliary(4),
        MouseButton::Forward => NativePointerButton::Auxiliary(5),
        MouseButton::Other(value) => NativePointerButton::Auxiliary(value),
    }
}

fn physical_key(key: &PhysicalKey) -> u32 {
    match key {
        PhysicalKey::Code(code) => *code as u32,
        PhysicalKey::Unidentified(_) => 0,
    }
}

fn logical_key(key: &Key) -> String {
    match key {
        Key::Character(value) => value.to_string(),
        Key::Named(value) => format!("{value:?}"),
        Key::Dead(value) => value.map_or_else(|| "Dead".to_string(), |value| value.to_string()),
        Key::Unidentified(_) => "Unidentified".to_string(),
    }
}

fn display_paths(paths: &[PathBuf]) -> Vec<String> {
    paths
        .iter()
        .map(|path| path.to_string_lossy().into_owned())
        .collect()
}

fn desktop_system_backend(window: &Arc<Window>) -> DesktopSystemBackend {
    let mut backend = DesktopSystemBackend::new();
    #[cfg(any(target_os = "macos", target_os = "windows"))]
    {
        let window = Arc::clone(window);
        backend.set_file_drag_starter(Box::new(move |request| {
            start_native_file_drag(window.as_ref(), request)
        }));
    }
    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    let _ = window;
    backend
}

#[cfg(any(target_os = "macos", target_os = "windows"))]
fn start_native_file_drag(window: &Window, request: &FileDragRequest) -> Result<(), String> {
    let paths = request
        .paths
        .iter()
        .map(|path| {
            PathBuf::from(path)
                .canonicalize()
                .map_err(|error| format!("cannot resolve dragged file {path}: {error}"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let preview = request
        .preview
        .as_ref()
        .map(|path| {
            PathBuf::from(path)
                .canonicalize()
                .map_err(|error| format!("cannot resolve file drag preview {path}: {error}"))
        })
        .transpose()?
        .unwrap_or_else(|| paths[0].clone());
    let options = drag::Options {
        mode: match request.mode {
            FileDragMode::Copy => drag::DragMode::Copy,
            FileDragMode::Move => drag::DragMode::Move,
        },
        ..drag::Options::default()
    };
    drag::start_drag(
        window,
        drag::DragItem::Files(paths),
        drag::Image::File(preview),
        |_result, _position| {},
        options,
    )
    .map_err(|error| error.to_string())
}

fn push_unique_path(paths: &mut Vec<PathBuf>, path: PathBuf) {
    if !paths.contains(&path) {
        paths.push(path);
    }
}

fn presentation_limit_reached(limit: Option<NonZeroU64>, presented_frames: u64) -> bool {
    limit.is_some_and(|limit| presented_frames >= limit.get())
}

fn automation_log(message: &str) {
    if std::env::var_os("VO_UI_AUTOMATION_EXIT_AFTER_FRAMES").is_some() {
        eprintln!("[VO:UI:CERTIFY] {message}");
    }
}

fn non_zero_size(size: PhysicalSize<u32>) -> PhysicalSize<u32> {
    PhysicalSize::new(size.width.max(1), size.height.max(1))
}

fn milli(value: f64) -> i32 {
    if !value.is_finite() {
        return 0;
    }
    (value * 1_000.0)
        .round()
        .clamp(f64::from(i32::MIN), f64::from(i32::MAX)) as i32
}

fn unsigned_milli(value: f64) -> u32 {
    if !value.is_finite() || value <= 0.0 {
        return 1;
    }
    (value * 1_000.0).round().clamp(1.0, f64::from(u32::MAX)) as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn native_desktop_config_rejects_zero_frame_interval() {
        let config = NativeDesktopConfig {
            frame_interval: Duration::ZERO,
            ..NativeDesktopConfig::default()
        };
        assert!(matches!(
            validate_config(&config),
            Err(NativeDesktopError::InvalidConfig)
        ));
    }

    #[test]
    fn coordinate_conversion_is_bounded() {
        assert_eq!(milli(1.25), 1_250);
        assert_eq!(unsigned_milli(1.5), 1_500);
        assert_eq!(unsigned_milli(0.0), 1);
        assert_eq!(milli(f64::INFINITY), 0);
    }

    #[test]
    fn file_drop_batch_preserves_first_seen_order_and_deduplicates() {
        let mut paths = Vec::new();
        push_unique_path(&mut paths, PathBuf::from("alpha.vo"));
        push_unique_path(&mut paths, PathBuf::from("beta.vo"));
        push_unique_path(&mut paths, PathBuf::from("alpha.vo"));

        assert_eq!(
            display_paths(&paths),
            vec!["alpha.vo".to_string(), "beta.vo".to_string()]
        );
    }

    #[test]
    fn automated_window_smoke_exits_only_after_the_configured_present() {
        let one = NonZeroU64::new(1).unwrap();
        let three = NonZeroU64::new(3).unwrap();

        assert!(!presentation_limit_reached(None, u64::MAX));
        assert!(!presentation_limit_reached(Some(one), 0));
        assert!(presentation_limit_reached(Some(one), 1));
        assert!(!presentation_limit_reached(Some(three), 2));
        assert!(presentation_limit_reached(Some(three), 3));
    }
}
