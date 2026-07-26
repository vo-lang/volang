use std::{collections::BTreeMap, sync::Arc};

use vo_app_protocol::{SurfaceHandle, ViewHandle};
use vo_app_runtime::{
    NativeCompositionFence, NativeCompositionFrame, NativeCompositionOutcome,
    NativeCompositorAdapter, NativeCompositorError,
};
use wgpu::util::DeviceExt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct WgpuCompositorConfig {
    pub max_views: usize,
    pub max_registered_layers: usize,
    pub desired_maximum_frame_latency: u32,
    pub clear_color_q16: [u16; 4],
}

impl Default for WgpuCompositorConfig {
    fn default() -> Self {
        Self {
            max_views: 16,
            max_registered_layers: 128,
            desired_maximum_frame_latency: 2,
            clear_color_q16: [0, 0, 0, u16::MAX],
        }
    }
}

struct RegisteredLayer {
    texture_token: u64,
    device_generation: u64,
    texture: wgpu::Texture,
}

struct WgpuView<'window> {
    surface: wgpu::Surface<'window>,
    config: wgpu::SurfaceConfiguration,
    attached: bool,
    pending: Option<(u64, wgpu::SurfaceTexture)>,
}

struct StagedDevice {
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
    generation: u64,
}

pub struct WgpuCompositorAdapter<'window> {
    config: WgpuCompositorConfig,
    adapter: Arc<wgpu::Adapter>,
    device: Arc<wgpu::Device>,
    queue: Arc<wgpu::Queue>,
    device_generation: u64,
    pipeline: wgpu::RenderPipeline,
    bind_group_layout: wgpu::BindGroupLayout,
    sampler: wgpu::Sampler,
    views: BTreeMap<ViewHandle, WgpuView<'window>>,
    layers: BTreeMap<SurfaceHandle, RegisteredLayer>,
    next_fence: u64,
    staged_device: Option<StagedDevice>,
}

impl<'window> WgpuCompositorAdapter<'window> {
    pub fn new(
        config: WgpuCompositorConfig,
        adapter: Arc<wgpu::Adapter>,
        device: Arc<wgpu::Device>,
        queue: Arc<wgpu::Queue>,
        device_generation: u64,
    ) -> Result<Self, NativeCompositorError> {
        if config.max_views == 0
            || config.max_registered_layers == 0
            || config.desired_maximum_frame_latency == 0
            || device_generation == 0
        {
            return Err(NativeCompositorError::InvalidConfig);
        }
        let (bind_group_layout, pipeline, sampler) = create_pipeline(&device);
        Ok(Self {
            config,
            adapter,
            device,
            queue,
            device_generation,
            pipeline,
            bind_group_layout,
            sampler,
            views: BTreeMap::new(),
            layers: BTreeMap::new(),
            next_fence: 1,
            staged_device: None,
        })
    }

    pub fn register_view_surface(
        &mut self,
        view: ViewHandle,
        surface: wgpu::Surface<'window>,
        width: u32,
        height: u32,
    ) -> Result<(), NativeCompositorError> {
        if !view.is_valid()
            || width == 0
            || height == 0
            || self.views.contains_key(&view)
            || self.views.len() == self.config.max_views
        {
            return Err(NativeCompositorError::InvalidFrame);
        }
        let capabilities = surface.get_capabilities(&self.adapter);
        let format = capabilities
            .formats
            .contains(&wgpu::TextureFormat::Bgra8UnormSrgb)
            .then_some(wgpu::TextureFormat::Bgra8UnormSrgb)
            .ok_or(NativeCompositorError::SurfaceLost)?;
        let present_mode = capabilities
            .present_modes
            .iter()
            .copied()
            .find(|mode| *mode == wgpu::PresentMode::Fifo)
            .or_else(|| capabilities.present_modes.first().copied())
            .ok_or(NativeCompositorError::SurfaceLost)?;
        let alpha_mode = capabilities
            .alpha_modes
            .first()
            .copied()
            .ok_or(NativeCompositorError::SurfaceLost)?;
        self.views.insert(
            view,
            WgpuView {
                surface,
                config: wgpu::SurfaceConfiguration {
                    usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
                    format,
                    width,
                    height,
                    present_mode,
                    desired_maximum_frame_latency: self.config.desired_maximum_frame_latency,
                    alpha_mode,
                    view_formats: vec![format],
                },
                attached: false,
                pending: None,
            },
        );
        Ok(())
    }

    pub fn resize_view(
        &mut self,
        view: ViewHandle,
        width: u32,
        height: u32,
    ) -> Result<(), NativeCompositorError> {
        if width == 0 || height == 0 {
            return Err(NativeCompositorError::InvalidFrame);
        }
        let state = self
            .views
            .get_mut(&view)
            .ok_or(NativeCompositorError::UnknownView)?;
        if state.pending.is_some() {
            return Err(NativeCompositorError::PendingFrame);
        }
        state.config.width = width;
        state.config.height = height;
        if state.attached {
            state.surface.configure(&self.device, &state.config);
        }
        Ok(())
    }

    pub fn register_layer_texture(
        &mut self,
        surface: SurfaceHandle,
        texture_token: u64,
        device_generation: u64,
        texture: wgpu::Texture,
    ) -> Result<(), NativeCompositorError> {
        if !surface.is_valid() || texture_token == 0 || device_generation != self.device_generation
        {
            return Err(NativeCompositorError::StaleDevice);
        }
        if !self.layers.contains_key(&surface)
            && self.layers.len() == self.config.max_registered_layers
        {
            return Err(NativeCompositorError::LayerCapacity);
        }
        self.layers.insert(
            surface,
            RegisteredLayer {
                texture_token,
                device_generation,
                texture,
            },
        );
        Ok(())
    }

    pub fn unregister_layer_texture(
        &mut self,
        surface: SurfaceHandle,
        texture_token: u64,
    ) -> Result<(), NativeCompositorError> {
        let layer = self
            .layers
            .get(&surface)
            .ok_or(NativeCompositorError::UnknownView)?;
        if layer.texture_token != texture_token {
            return Err(NativeCompositorError::InvalidFrame);
        }
        self.layers.remove(&surface);
        Ok(())
    }

    pub fn stage_device(
        &mut self,
        device: Arc<wgpu::Device>,
        queue: Arc<wgpu::Queue>,
        generation: u64,
    ) -> Result<(), NativeCompositorError> {
        if generation <= self.device_generation || self.staged_device.is_some() {
            return Err(NativeCompositorError::StaleDevice);
        }
        if self.views.values().any(|view| view.pending.is_some()) {
            return Err(NativeCompositorError::PendingFrame);
        }
        self.staged_device = Some(StagedDevice {
            device,
            queue,
            generation,
        });
        Ok(())
    }

    pub const fn device_generation(&self) -> u64 {
        self.device_generation
    }

    pub fn device(&self) -> Arc<wgpu::Device> {
        Arc::clone(&self.device)
    }

    pub fn queue(&self) -> Arc<wgpu::Queue> {
        Arc::clone(&self.queue)
    }
}

impl NativeCompositorAdapter for WgpuCompositorAdapter<'_> {
    fn attach_view(
        &mut self,
        view: ViewHandle,
        device_generation: u64,
    ) -> Result<(), NativeCompositorError> {
        if device_generation != self.device_generation {
            return Err(NativeCompositorError::StaleDevice);
        }
        let state = self
            .views
            .get_mut(&view)
            .ok_or(NativeCompositorError::UnknownView)?;
        if state.attached {
            return Err(NativeCompositorError::DuplicateView);
        }
        state.surface.configure(&self.device, &state.config);
        state.attached = true;
        Ok(())
    }

    fn submit(&mut self, frame: &NativeCompositionFrame) -> Result<u64, NativeCompositorError> {
        if frame.device_generation != self.device_generation {
            return Err(NativeCompositorError::StaleDevice);
        }
        let state = self
            .views
            .get_mut(&frame.view)
            .ok_or(NativeCompositorError::UnknownView)?;
        if !state.attached {
            return Err(NativeCompositorError::UnknownView);
        }
        if state.pending.is_some() {
            return Err(NativeCompositorError::PendingFrame);
        }
        let output = state
            .surface
            .get_current_texture()
            .map_err(map_surface_error)?;
        let target = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("vo-app-native-compositor"),
            });
        if frame.layers.is_empty() {
            let _clear = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("vo-app-native-clear-pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &target,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: wgpu::LoadOp::Clear(clear_color(self.config.clear_color_q16)),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
        }
        for (index, submission) in frame.layers.iter().enumerate() {
            let layer = self
                .layers
                .get(&submission.surface)
                .ok_or(NativeCompositorError::InvalidFrame)?;
            if layer.texture_token != submission.texture_token
                || layer.device_generation != submission.device_generation
            {
                return Err(NativeCompositorError::StaleDevice);
            }
            let source = layer
                .texture
                .create_view(&wgpu::TextureViewDescriptor::default());
            let layer_bytes = layer_uniform_bytes(frame, submission.geometry);
            let uniform = self
                .device
                .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("vo-app-native-layer-uniform"),
                    contents: &layer_bytes,
                    usage: wgpu::BufferUsages::UNIFORM,
                });
            let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                label: Some("vo-app-native-layer"),
                layout: &self.bind_group_layout,
                entries: &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(&source),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(&self.sampler),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: uniform.as_entire_binding(),
                    },
                ],
            });
            let mut pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("vo-app-native-layer-pass"),
                color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                    view: &target,
                    resolve_target: None,
                    ops: wgpu::Operations {
                        load: if index == 0 {
                            wgpu::LoadOp::Clear(clear_color(self.config.clear_color_q16))
                        } else {
                            wgpu::LoadOp::Load
                        },
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });
            pass.set_pipeline(&self.pipeline);
            pass.set_bind_group(0, &bind_group, &[]);
            pass.draw(0..6, 0..1);
        }
        self.queue.submit([encoder.finish()]);
        let fence = self.next_fence;
        self.next_fence = self
            .next_fence
            .checked_add(1)
            .ok_or(NativeCompositorError::OutcomeUnknown)?;
        state.pending = Some((fence, output));
        Ok(fence)
    }

    fn present(
        &mut self,
        fence: NativeCompositionFence,
        now_micros: u64,
        deadline_micros: u64,
    ) -> Result<NativeCompositionOutcome, NativeCompositorError> {
        if fence.device_generation != self.device_generation {
            return Err(NativeCompositorError::StaleDevice);
        }
        let state = self
            .views
            .get_mut(&fence.view)
            .ok_or(NativeCompositorError::UnknownView)?;
        let (pending_fence, output) = state
            .pending
            .take()
            .ok_or(NativeCompositorError::FenceMismatch)?;
        if pending_fence != fence.fence_value {
            state.pending = Some((pending_fence, output));
            return Err(NativeCompositorError::FenceMismatch);
        }
        output.present();
        Ok(if now_micros > deadline_micros {
            NativeCompositionOutcome::DeadlineMissed
        } else {
            NativeCompositionOutcome::Presented
        })
    }

    fn rebind_view(
        &mut self,
        view: ViewHandle,
        old_device_generation: u64,
        new_device_generation: u64,
    ) -> Result<(), NativeCompositorError> {
        if self.views.len() != 1 || !self.views.contains_key(&view) {
            return Err(NativeCompositorError::InvalidFrame);
        }
        self.rebind_device(&[view], old_device_generation, new_device_generation)
    }

    fn rebind_device(
        &mut self,
        views: &[ViewHandle],
        old_device_generation: u64,
        new_device_generation: u64,
    ) -> Result<(), NativeCompositorError> {
        if old_device_generation != self.device_generation {
            return Err(NativeCompositorError::StaleDevice);
        }
        if views.len() != self.views.len()
            || views.iter().any(|view| !self.views.contains_key(view))
            || self.views.values().any(|state| state.pending.is_some())
        {
            return Err(NativeCompositorError::InvalidFrame);
        }
        let staged = self
            .staged_device
            .take()
            .ok_or(NativeCompositorError::StaleDevice)?;
        if staged.generation != new_device_generation {
            self.staged_device = Some(staged);
            return Err(NativeCompositorError::StaleDevice);
        }
        self.device = staged.device;
        self.queue = staged.queue;
        self.device_generation = staged.generation;
        let (layout, pipeline, sampler) = create_pipeline(&self.device);
        self.bind_group_layout = layout;
        self.pipeline = pipeline;
        self.sampler = sampler;
        self.layers.clear();
        self.next_fence = 1;
        for state in self.views.values_mut() {
            if state.attached {
                state.surface.configure(&self.device, &state.config);
            }
        }
        Ok(())
    }

    fn detach_view(
        &mut self,
        view: ViewHandle,
        device_generation: u64,
    ) -> Result<(), NativeCompositorError> {
        if device_generation != self.device_generation {
            return Err(NativeCompositorError::StaleDevice);
        }
        let state = self
            .views
            .get(&view)
            .ok_or(NativeCompositorError::UnknownView)?;
        if state.pending.is_some() {
            return Err(NativeCompositorError::PendingFrame);
        }
        self.views.remove(&view);
        Ok(())
    }
}

fn clear_color(value: [u16; 4]) -> wgpu::Color {
    let scale = 1.0 / f64::from(u16::MAX);
    wgpu::Color {
        r: f64::from(value[0]) * scale,
        g: f64::from(value[1]) * scale,
        b: f64::from(value[2]) * scale,
        a: f64::from(value[3]) * scale,
    }
}

fn create_pipeline(
    device: &wgpu::Device,
) -> (wgpu::BindGroupLayout, wgpu::RenderPipeline, wgpu::Sampler) {
    let layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
        label: Some("vo-app-native-layer-layout"),
        entries: &[
            wgpu::BindGroupLayoutEntry {
                binding: 0,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Texture {
                    sample_type: wgpu::TextureSampleType::Float { filterable: true },
                    view_dimension: wgpu::TextureViewDimension::D2,
                    multisampled: false,
                },
                count: None,
            },
            wgpu::BindGroupLayoutEntry {
                binding: 1,
                visibility: wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                count: None,
            },
            wgpu::BindGroupLayoutEntry {
                binding: 2,
                visibility: wgpu::ShaderStages::VERTEX | wgpu::ShaderStages::FRAGMENT,
                ty: wgpu::BindingType::Buffer {
                    ty: wgpu::BufferBindingType::Uniform,
                    has_dynamic_offset: false,
                    min_binding_size: None,
                },
                count: None,
            },
        ],
    });
    let pipeline_layout = device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
        label: Some("vo-app-native-compositor-layout"),
        bind_group_layouts: &[&layout],
        push_constant_ranges: &[],
    });
    let shader = device.create_shader_module(wgpu::ShaderModuleDescriptor {
        label: Some("vo-app-native-compositor-shader"),
        source: wgpu::ShaderSource::Wgsl(COMPOSITOR_SHADER.into()),
    });
    let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
        label: Some("vo-app-native-compositor-pipeline"),
        layout: Some(&pipeline_layout),
        vertex: wgpu::VertexState {
            module: &shader,
            entry_point: Some("vs_main"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            buffers: &[],
        },
        primitive: wgpu::PrimitiveState::default(),
        depth_stencil: None,
        multisample: wgpu::MultisampleState::default(),
        fragment: Some(wgpu::FragmentState {
            module: &shader,
            entry_point: Some("fs_main"),
            compilation_options: wgpu::PipelineCompilationOptions::default(),
            targets: &[Some(wgpu::ColorTargetState {
                format: wgpu::TextureFormat::Bgra8UnormSrgb,
                blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                write_mask: wgpu::ColorWrites::ALL,
            })],
        }),
        multiview: None,
        cache: None,
    });
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        label: Some("vo-app-native-layer-sampler"),
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        ..Default::default()
    });
    (layout, pipeline, sampler)
}

fn layer_uniform_bytes(
    frame: &NativeCompositionFrame,
    geometry: vo_app_runtime::SurfaceGeometry,
) -> [u8; 80] {
    let viewport_width = frame.viewport_width_milli as f32;
    let viewport_height = frame.viewport_height_milli as f32;
    let bounds = geometry.bounds.unwrap_or(vo_app_runtime::SurfaceRect {
        x_milli: 0,
        y_milli: 0,
        width_milli: frame.viewport_width_milli,
        height_milli: frame.viewport_height_milli,
    });
    let clip = geometry.clip.unwrap_or(bounds);
    let values = [
        geometry.transform.m11_q16 as f32 / 65_536.0,
        geometry.transform.m12_q16 as f32 / 65_536.0,
        geometry.transform.m21_q16 as f32 / 65_536.0,
        geometry.transform.m22_q16 as f32 / 65_536.0,
        geometry.transform.translate_x_milli as f32,
        geometry.transform.translate_y_milli as f32,
        viewport_width,
        viewport_height,
        bounds.x_milli as f32,
        bounds.y_milli as f32,
        bounds.width_milli as f32,
        bounds.height_milli as f32,
        clip.x_milli as f32,
        clip.y_milli as f32,
        clip.width_milli as f32,
        clip.height_milli as f32,
        f32::from(geometry.opacity_q16) / f32::from(u16::MAX),
        0.0,
        0.0,
        0.0,
    ];
    let mut bytes = [0_u8; 80];
    for (index, value) in values.into_iter().enumerate() {
        bytes[index * 4..index * 4 + 4].copy_from_slice(&value.to_le_bytes());
    }
    bytes
}

fn map_surface_error(error: wgpu::SurfaceError) -> NativeCompositorError {
    match error {
        wgpu::SurfaceError::Lost | wgpu::SurfaceError::Outdated => {
            NativeCompositorError::SurfaceLost
        }
        wgpu::SurfaceError::OutOfMemory => NativeCompositorError::DeviceLost,
        wgpu::SurfaceError::Timeout | wgpu::SurfaceError::Other => {
            NativeCompositorError::OutcomeUnknown
        }
    }
}

const COMPOSITOR_SHADER: &str = r#"
struct LayerUniform {
    matrix: vec4<f32>,
    translation_viewport: vec4<f32>,
    bounds: vec4<f32>,
    clip: vec4<f32>,
    opacity: vec4<f32>,
}

@group(0) @binding(0) var layer_texture: texture_2d<f32>;
@group(0) @binding(1) var layer_sampler: sampler;
@group(0) @binding(2) var<uniform> layer: LayerUniform;

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) local: vec2<f32>,
}

@vertex
fn vs_main(@builtin(vertex_index) index: u32) -> VertexOutput {
    var positions = array<vec2<f32>, 6>(
        vec2<f32>(0.0, 0.0),
        vec2<f32>(1.0, 0.0),
        vec2<f32>(0.0, 1.0),
        vec2<f32>(0.0, 1.0),
        vec2<f32>(1.0, 0.0),
        vec2<f32>(1.0, 1.0),
    );
    let unit = positions[index];
    let local = layer.bounds.xy + unit * layer.bounds.zw;
    let transformed = vec2<f32>(
        layer.matrix.x * local.x + layer.matrix.z * local.y,
        layer.matrix.y * local.x + layer.matrix.w * local.y,
    ) + layer.translation_viewport.xy;
    let viewport = layer.translation_viewport.zw;
    var output: VertexOutput;
    output.position = vec4<f32>(
        transformed.x / viewport.x * 2.0 - 1.0,
        1.0 - transformed.y / viewport.y * 2.0,
        0.0,
        1.0,
    );
    output.uv = unit;
    output.local = local;
    return output;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    let clip_max = layer.clip.xy + layer.clip.zw;
    if any(input.local < layer.clip.xy) || any(input.local >= clip_max) {
        discard;
    }
    let color = textureSample(layer_texture, layer_sampler, input.uv);
    return vec4<f32>(color.rgb, color.a * layer.opacity.x);
}
"#;
