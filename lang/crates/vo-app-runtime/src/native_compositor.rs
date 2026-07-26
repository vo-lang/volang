use alloc::vec::Vec;

use vo_app_protocol::{SurfaceHandle, ViewHandle};

use crate::{composition::validate_geometry, SurfaceGeometry, SurfaceInputPolicy, SurfaceKind};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeLayerSubmission {
    pub surface: SurfaceHandle,
    pub kind: SurfaceKind,
    pub z_order: i32,
    pub input: SurfaceInputPolicy,
    pub content_revision: u64,
    pub texture_token: u64,
    pub device_generation: u64,
    pub geometry: SurfaceGeometry,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NativeCompositionFrame {
    pub view: ViewHandle,
    pub pulse_id: u64,
    pub device_generation: u64,
    pub viewport_width_milli: u32,
    pub viewport_height_milli: u32,
    pub layers: Vec<NativeLayerSubmission>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeCompositionFence {
    pub view: ViewHandle,
    pub pulse_id: u64,
    pub device_generation: u64,
    pub fence_value: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NativeCompositionOutcome {
    Presented,
    DeadlineMissed,
    Suspended,
    SurfaceLost,
    DeviceLost,
}

impl From<NativeCompositionOutcome> for crate::SurfacePresentationOutcome {
    fn from(outcome: NativeCompositionOutcome) -> Self {
        match outcome {
            NativeCompositionOutcome::Presented => Self::Presented,
            NativeCompositionOutcome::DeadlineMissed => Self::DeadlineMissed,
            NativeCompositionOutcome::Suspended => Self::Suspended,
            NativeCompositionOutcome::SurfaceLost => Self::SurfaceLost,
            NativeCompositionOutcome::DeviceLost => Self::DeviceLost,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NativeCompositorError {
    InvalidConfig,
    InvalidFrame,
    ViewCapacity,
    LayerCapacity,
    DuplicateView,
    UnknownView,
    DuplicateSurface,
    PendingFrame,
    FenceMismatch,
    StaleDevice,
    SurfaceLost,
    DeviceLost,
    OutcomeUnknown,
    Closed,
}

pub trait NativeCompositorAdapter {
    fn attach_view(
        &mut self,
        view: ViewHandle,
        device_generation: u64,
    ) -> Result<(), NativeCompositorError>;
    fn submit(&mut self, frame: &NativeCompositionFrame) -> Result<u64, NativeCompositorError>;
    fn present(
        &mut self,
        fence: NativeCompositionFence,
        now_micros: u64,
        deadline_micros: u64,
    ) -> Result<NativeCompositionOutcome, NativeCompositorError>;
    fn rebind_view(
        &mut self,
        view: ViewHandle,
        old_device_generation: u64,
        new_device_generation: u64,
    ) -> Result<(), NativeCompositorError>;
    fn rebind_device(
        &mut self,
        views: &[ViewHandle],
        old_device_generation: u64,
        new_device_generation: u64,
    ) -> Result<(), NativeCompositorError>;
    fn detach_view(
        &mut self,
        view: ViewHandle,
        device_generation: u64,
    ) -> Result<(), NativeCompositorError>;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeCompositorConfig {
    pub max_views: usize,
    pub max_layers_per_view: usize,
}

impl Default for NativeCompositorConfig {
    fn default() -> Self {
        Self {
            max_views: 16,
            max_layers_per_view: 32,
        }
    }
}

struct ViewState {
    view: ViewHandle,
    device_generation: u64,
    last_pulse_id: u64,
    pending: Option<NativeCompositionFence>,
}

pub struct NativeCompositorOwner<A> {
    config: NativeCompositorConfig,
    adapter: A,
    views: Vec<ViewState>,
    closed: bool,
}

impl<A: NativeCompositorAdapter> NativeCompositorOwner<A> {
    pub fn new(config: NativeCompositorConfig, adapter: A) -> Result<Self, NativeCompositorError> {
        if config.max_views == 0 || config.max_layers_per_view == 0 {
            return Err(NativeCompositorError::InvalidConfig);
        }
        Ok(Self {
            config,
            adapter,
            views: Vec::new(),
            closed: false,
        })
    }

    pub fn attach_view(
        &mut self,
        view: ViewHandle,
        device_generation: u64,
    ) -> Result<(), NativeCompositorError> {
        self.require_open()?;
        if !view.is_valid() || device_generation == 0 {
            return Err(NativeCompositorError::InvalidFrame);
        }
        if self.views.iter().any(|state| state.view == view) {
            return Err(NativeCompositorError::DuplicateView);
        }
        if self.views.len() == self.config.max_views {
            return Err(NativeCompositorError::ViewCapacity);
        }
        self.adapter.attach_view(view, device_generation)?;
        self.views.push(ViewState {
            view,
            device_generation,
            last_pulse_id: 0,
            pending: None,
        });
        Ok(())
    }

    pub fn submit(
        &mut self,
        frame: NativeCompositionFrame,
    ) -> Result<NativeCompositionFence, NativeCompositorError> {
        self.require_open()?;
        validate_frame(&frame, self.config.max_layers_per_view)?;
        let state = self.view(frame.view)?;
        if state.pending.is_some() {
            return Err(NativeCompositorError::PendingFrame);
        }
        if frame.device_generation != state.device_generation {
            return Err(NativeCompositorError::StaleDevice);
        }
        if frame.pulse_id <= state.last_pulse_id {
            return Err(NativeCompositorError::InvalidFrame);
        }
        let fence_value = self.adapter.submit(&frame)?;
        if fence_value == 0 {
            return Err(NativeCompositorError::OutcomeUnknown);
        }
        let fence = NativeCompositionFence {
            view: frame.view,
            pulse_id: frame.pulse_id,
            device_generation: frame.device_generation,
            fence_value,
        };
        let state = self.view_mut(frame.view)?;
        state.last_pulse_id = frame.pulse_id;
        state.pending = Some(fence);
        Ok(fence)
    }

    pub fn present(
        &mut self,
        fence: NativeCompositionFence,
        now_micros: u64,
        deadline_micros: u64,
    ) -> Result<NativeCompositionOutcome, NativeCompositorError> {
        self.require_open()?;
        let state = self.view(fence.view)?;
        if state.pending != Some(fence) {
            return Err(NativeCompositorError::FenceMismatch);
        }
        if state.device_generation != fence.device_generation {
            return Err(NativeCompositorError::StaleDevice);
        }
        let outcome = self.adapter.present(fence, now_micros, deadline_micros)?;
        self.view_mut(fence.view)?.pending = None;
        Ok(outcome)
    }

    pub fn rebind_view(
        &mut self,
        view: ViewHandle,
        new_device_generation: u64,
    ) -> Result<(), NativeCompositorError> {
        self.require_open()?;
        let state = self.view(view)?;
        if state.pending.is_some() {
            return Err(NativeCompositorError::PendingFrame);
        }
        if new_device_generation == 0 || new_device_generation <= state.device_generation {
            return Err(NativeCompositorError::StaleDevice);
        }
        let old = state.device_generation;
        self.adapter.rebind_view(view, old, new_device_generation)?;
        let state = self.view_mut(view)?;
        state.device_generation = new_device_generation;
        state.last_pulse_id = 0;
        Ok(())
    }

    pub fn rebind_device(
        &mut self,
        new_device_generation: u64,
    ) -> Result<(), NativeCompositorError> {
        self.require_open()?;
        if self.views.is_empty() || new_device_generation == 0 {
            return Err(NativeCompositorError::InvalidFrame);
        }
        if self.views.iter().any(|state| state.pending.is_some()) {
            return Err(NativeCompositorError::PendingFrame);
        }
        let old_device_generation = self.views[0].device_generation;
        if new_device_generation <= old_device_generation
            || self
                .views
                .iter()
                .any(|state| state.device_generation != old_device_generation)
        {
            return Err(NativeCompositorError::StaleDevice);
        }
        let views = self
            .views
            .iter()
            .map(|state| state.view)
            .collect::<Vec<_>>();
        self.adapter
            .rebind_device(&views, old_device_generation, new_device_generation)?;
        for state in &mut self.views {
            state.device_generation = new_device_generation;
            state.last_pulse_id = 0;
        }
        Ok(())
    }

    pub fn detach_view(&mut self, view: ViewHandle) -> Result<(), NativeCompositorError> {
        self.require_open()?;
        let index = self
            .views
            .iter()
            .position(|state| state.view == view)
            .ok_or(NativeCompositorError::UnknownView)?;
        let state = &self.views[index];
        if state.pending.is_some() {
            return Err(NativeCompositorError::PendingFrame);
        }
        self.adapter.detach_view(view, state.device_generation)?;
        self.views.remove(index);
        Ok(())
    }

    pub fn close(&mut self) -> Result<(), NativeCompositorError> {
        self.require_open()?;
        if self.views.iter().any(|state| state.pending.is_some()) {
            return Err(NativeCompositorError::PendingFrame);
        }
        while let Some(state) = self.views.pop() {
            self.adapter
                .detach_view(state.view, state.device_generation)?;
        }
        self.closed = true;
        Ok(())
    }

    pub fn adapter(&self) -> &A {
        &self.adapter
    }

    pub fn adapter_mut(&mut self) -> Result<&mut A, NativeCompositorError> {
        self.require_open()?;
        if self.views.iter().any(|state| state.pending.is_some()) {
            return Err(NativeCompositorError::PendingFrame);
        }
        Ok(&mut self.adapter)
    }

    fn require_open(&self) -> Result<(), NativeCompositorError> {
        if self.closed {
            Err(NativeCompositorError::Closed)
        } else {
            Ok(())
        }
    }

    fn view(&self, view: ViewHandle) -> Result<&ViewState, NativeCompositorError> {
        self.views
            .iter()
            .find(|state| state.view == view)
            .ok_or(NativeCompositorError::UnknownView)
    }

    fn view_mut(&mut self, view: ViewHandle) -> Result<&mut ViewState, NativeCompositorError> {
        self.views
            .iter_mut()
            .find(|state| state.view == view)
            .ok_or(NativeCompositorError::UnknownView)
    }
}

fn validate_frame(
    frame: &NativeCompositionFrame,
    max_layers: usize,
) -> Result<(), NativeCompositorError> {
    if !frame.view.is_valid()
        || frame.pulse_id == 0
        || frame.device_generation == 0
        || frame.viewport_width_milli == 0
        || frame.viewport_height_milli == 0
        || frame.layers.is_empty()
    {
        return Err(NativeCompositorError::InvalidFrame);
    }
    if frame.layers.len() > max_layers {
        return Err(NativeCompositorError::LayerCapacity);
    }
    let mut previous = None;
    for (index, layer) in frame.layers.iter().enumerate() {
        if !layer.surface.is_valid()
            || layer.content_revision == 0
            || layer.texture_token == 0
            || layer.device_generation != frame.device_generation
            || validate_geometry(layer.geometry).is_err()
        {
            return Err(NativeCompositorError::InvalidFrame);
        }
        if frame.layers[..index]
            .iter()
            .any(|candidate| candidate.surface == layer.surface)
        {
            return Err(NativeCompositorError::DuplicateSurface);
        }
        if previous.is_some_and(|z_order| z_order > layer.z_order) {
            return Err(NativeCompositorError::InvalidFrame);
        }
        previous = Some(layer.z_order);
    }
    Ok(())
}
