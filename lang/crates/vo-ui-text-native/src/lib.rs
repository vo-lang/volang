//! Native text shaping, system-font fallback, and deterministic software
//! presentation for Volang UI.
//!
//! The crate lives at the platform boundary. Renderer-neutral layout and paint
//! crates stay `no_std`, while this adapter owns the operating-system font
//! database and glyph raster cache.

use std::collections::BTreeSet;
use std::fmt;

use cosmic_text::{Attrs, Buffer, Color, FontSystem, Metrics, Shaping, SwashCache, Weight, Wrap};
use vo_ui_core::NodeId;
use vo_ui_layout::{IntrinsicMeasurer, Rect, Size};
use vo_ui_paint::{DrawCommand, PaintScene};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NativeTextConfig {
    pub max_text_bytes: usize,
    pub max_glyphs: usize,
    pub max_surface_pixels: usize,
    pub line_height_ratio: f32,
}

impl Default for NativeTextConfig {
    fn default() -> Self {
        Self {
            max_text_bytes: 16 * 1024 * 1024,
            max_glyphs: 1_000_000,
            max_surface_pixels: 67_108_864,
            line_height_ratio: 1.25,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NativeTextError {
    InvalidConfig,
    InvalidMetrics,
    InvalidScale,
    InvalidViewport,
    TextLimitExceeded,
    GlyphLimitExceeded,
    SurfaceLimitExceeded,
    SurfaceSizeOverflow,
}

impl fmt::Display for NativeTextError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "native UI text error: {self:?}")
    }
}

impl std::error::Error for NativeTextError {}

#[derive(Clone, Debug, PartialEq)]
pub struct ShapedGlyph {
    pub line_index: usize,
    pub source_start: usize,
    pub source_end: usize,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
    pub rtl: bool,
    pub font_id: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShapedLine {
    pub line_index: usize,
    pub y: f32,
    pub height: f32,
    pub width: f32,
    pub rtl: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ShapedText {
    pub size: Size,
    pub lines: Vec<ShapedLine>,
    pub glyphs: Vec<ShapedGlyph>,
    pub font_ids: Vec<String>,
    pub has_rtl: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SceneTextDiagnostics {
    pub text_runs: usize,
    pub glyphs: usize,
    pub rtl_runs: usize,
    pub selection_rects: usize,
    pub carets: usize,
    pub font_ids: Vec<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SoftwareSurface {
    width: u32,
    height: u32,
    pixels: Vec<u8>,
}

impl SoftwareSurface {
    fn new(width: u32, height: u32) -> Result<Self, NativeTextError> {
        let len = usize::try_from(width)
            .ok()
            .and_then(|width| {
                usize::try_from(height)
                    .ok()
                    .and_then(|height| width.checked_mul(height))
            })
            .and_then(|pixels| pixels.checked_mul(4))
            .ok_or(NativeTextError::SurfaceSizeOverflow)?;
        Ok(Self {
            width,
            height,
            pixels: vec![0; len],
        })
    }

    pub fn from_rgba8(width: u32, height: u32, pixels: Vec<u8>) -> Result<Self, NativeTextError> {
        let expected = usize::try_from(width)
            .ok()
            .and_then(|width| {
                usize::try_from(height)
                    .ok()
                    .and_then(|height| width.checked_mul(height))
            })
            .and_then(|pixels| pixels.checked_mul(4))
            .ok_or(NativeTextError::SurfaceSizeOverflow)?;
        if pixels.len() != expected {
            return Err(NativeTextError::SurfaceSizeOverflow);
        }
        Ok(Self {
            width,
            height,
            pixels,
        })
    }

    pub const fn width(&self) -> u32 {
        self.width
    }

    pub const fn height(&self) -> u32 {
        self.height
    }

    pub fn pixels_rgba8(&self) -> &[u8] {
        &self.pixels
    }

    pub fn into_pixels_rgba8(self) -> Vec<u8> {
        self.pixels
    }

    pub fn non_transparent_pixel_count(&self) -> usize {
        self.pixels
            .chunks_exact(4)
            .filter(|pixel| pixel[3] != 0)
            .count()
    }

    fn blend_rect(&mut self, x: i32, y: i32, width: u32, height: u32, color: u32, clip: PixelClip) {
        let right = i64::from(x).saturating_add(i64::from(width));
        let bottom = i64::from(y).saturating_add(i64::from(height));
        let left = i64::from(x).max(i64::from(clip.left)).max(0);
        let top = i64::from(y).max(i64::from(clip.top)).max(0);
        let right = right.min(i64::from(clip.right)).min(i64::from(self.width));
        let bottom = bottom
            .min(i64::from(clip.bottom))
            .min(i64::from(self.height));
        if left >= right || top >= bottom {
            return;
        }
        for pixel_y in top..bottom {
            for pixel_x in left..right {
                self.blend_pixel(pixel_x as u32, pixel_y as u32, color);
            }
        }
    }

    fn blend_rounded_rect(&mut self, rect: PixelRect, radius: f32, color: u32, clip: PixelClip) {
        if radius <= 0.0 || !radius.is_finite() {
            self.blend_rect(rect.x, rect.y, rect.width, rect.height, color, clip);
            return;
        }
        self.blend_rounded_shape(rect, radius, 0.0, color, clip);
    }

    fn blend_rounded_border(
        &mut self,
        rect: PixelRect,
        radius: f32,
        stroke_width: f32,
        color: u32,
        clip: PixelClip,
    ) {
        if stroke_width <= 0.0 || !stroke_width.is_finite() {
            return;
        }
        self.blend_rounded_shape(rect, radius.max(0.0), stroke_width, color, clip);
    }

    fn blend_rounded_shape(
        &mut self,
        rect: PixelRect,
        radius: f32,
        inset: f32,
        color: u32,
        clip: PixelClip,
    ) {
        let right = i64::from(rect.x).saturating_add(i64::from(rect.width));
        let bottom = i64::from(rect.y).saturating_add(i64::from(rect.height));
        let left = i64::from(rect.x).max(i64::from(clip.left)).max(0);
        let top = i64::from(rect.y).max(i64::from(clip.top)).max(0);
        let right = right.min(i64::from(clip.right)).min(i64::from(self.width));
        let bottom = bottom
            .min(i64::from(clip.bottom))
            .min(i64::from(self.height));
        if left >= right || top >= bottom {
            return;
        }
        let width_f = rect.width as f32;
        let height_f = rect.height as f32;
        let outer_radius = radius.min(width_f * 0.5).min(height_f * 0.5);
        let inner_width = (width_f - inset * 2.0).max(0.0);
        let inner_height = (height_f - inset * 2.0).max(0.0);
        let inner_radius = (outer_radius - inset).max(0.0);
        for pixel_y in top..bottom {
            for pixel_x in left..right {
                let local_x = pixel_x as f32 + 0.5 - rect.x as f32;
                let local_y = pixel_y as f32 + 0.5 - rect.y as f32;
                if !inside_rounded_rect(local_x, local_y, width_f, height_f, outer_radius) {
                    continue;
                }
                if inset > 0.0
                    && inner_width > 0.0
                    && inner_height > 0.0
                    && inside_rounded_rect(
                        local_x - inset,
                        local_y - inset,
                        inner_width,
                        inner_height,
                        inner_radius,
                    )
                {
                    continue;
                }
                self.blend_pixel(pixel_x as u32, pixel_y as u32, color);
            }
        }
    }

    fn blend_pixel(&mut self, x: u32, y: u32, color: u32) {
        let Some(index) = usize::try_from(y)
            .ok()
            .and_then(|y| usize::try_from(self.width).ok().map(|width| y * width))
            .and_then(|row| usize::try_from(x).ok().map(|x| row + x))
            .and_then(|pixel| pixel.checked_mul(4))
        else {
            return;
        };
        let source_alpha = (color >> 24) & 0xff;
        if source_alpha == 0 {
            return;
        }
        let source = [(color >> 16) & 0xff, (color >> 8) & 0xff, color & 0xff];
        let destination_alpha = u32::from(self.pixels[index + 3]);
        let inverse = 255 - source_alpha;
        let output_alpha = source_alpha + (destination_alpha * inverse + 127) / 255;
        for (channel, source_channel) in source.into_iter().enumerate() {
            let destination = u32::from(self.pixels[index + channel]);
            let premultiplied = source_channel * source_alpha
                + (destination * destination_alpha * inverse + 127) / 255;
            self.pixels[index + channel] = if output_alpha == 0 {
                0
            } else {
                ((premultiplied + output_alpha / 2) / output_alpha).min(255) as u8
            };
        }
        self.pixels[index + 3] = output_alpha.min(255) as u8;
    }
}

fn inside_rounded_rect(x: f32, y: f32, width: f32, height: f32, radius: f32) -> bool {
    if x < 0.0 || y < 0.0 || x >= width || y >= height {
        return false;
    }
    if radius <= 0.0 {
        return true;
    }
    let nearest_x = x.clamp(radius, width - radius);
    let nearest_y = y.clamp(radius, height - radius);
    let delta_x = x - nearest_x;
    let delta_y = y - nearest_y;
    delta_x * delta_x + delta_y * delta_y <= radius * radius
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PreparedPaintScene {
    pub revision: u64,
    pub frame_id: u64,
    pub scale_bits: u32,
    pub surface: SoftwareSurface,
    pub text: SceneTextDiagnostics,
}

impl PreparedPaintScene {
    pub fn scale(&self) -> f32 {
        f32::from_bits(self.scale_bits)
    }
}

#[derive(Clone, Copy)]
struct PixelRect {
    x: i32,
    y: i32,
    width: u32,
    height: u32,
}

#[derive(Clone, Copy)]
struct PixelClip {
    left: i32,
    top: i32,
    right: i32,
    bottom: i32,
}

#[derive(Clone, Copy)]
struct EditorSelection<'a> {
    source: &'a str,
    start_utf16: u32,
    length_utf16: u32,
}

#[derive(Default)]
struct RenderedText {
    text_runs: usize,
    glyphs: usize,
    rtl_runs: usize,
    selection_rects: usize,
    carets: usize,
    font_ids: BTreeSet<String>,
}

impl PixelClip {
    fn surface(width: u32, height: u32) -> Self {
        Self {
            left: 0,
            top: 0,
            right: i32::try_from(width).unwrap_or(i32::MAX),
            bottom: i32::try_from(height).unwrap_or(i32::MAX),
        }
    }

    fn from_rect(rect: Rect, scale: f32, surface: Self) -> Self {
        let left = (rect.x as f32 * scale).floor();
        let top = (rect.y as f32 * scale).floor();
        let right = ((rect.x + rect.width) as f32 * scale).ceil();
        let bottom = ((rect.y + rect.height) as f32 * scale).ceil();
        Self {
            left: finite_i32(left).max(surface.left),
            top: finite_i32(top).max(surface.top),
            right: finite_i32(right).min(surface.right),
            bottom: finite_i32(bottom).min(surface.bottom),
        }
    }
}

pub struct NativeTextSystem {
    config: NativeTextConfig,
    font_system: FontSystem,
    raster_cache: SwashCache,
    last_measure_error: Option<NativeTextError>,
}

impl NativeTextSystem {
    pub fn new(config: NativeTextConfig) -> Result<Self, NativeTextError> {
        if config.max_text_bytes == 0
            || config.max_glyphs == 0
            || config.max_surface_pixels == 0
            || !config.line_height_ratio.is_finite()
            || config.line_height_ratio <= 0.0
        {
            return Err(NativeTextError::InvalidConfig);
        }
        Ok(Self {
            config,
            font_system: FontSystem::new(),
            raster_cache: SwashCache::new(),
            last_measure_error: None,
        })
    }

    pub const fn config(&self) -> NativeTextConfig {
        self.config
    }

    pub fn take_measure_error(&mut self) -> Option<NativeTextError> {
        self.last_measure_error.take()
    }

    pub fn shape_text(
        &mut self,
        text: &str,
        font_size: f64,
        font_weight: i64,
        max_width: f64,
    ) -> Result<ShapedText, NativeTextError> {
        self.validate_text(text)?;
        let font_size = checked_positive_f32(font_size).ok_or(NativeTextError::InvalidMetrics)?;
        let max_width = checked_optional_dimension(max_width)?;
        let buffer = self.make_buffer(text, font_size, font_weight, max_width, None);
        let mut lines = Vec::new();
        let mut glyphs = Vec::new();
        let mut font_ids = BTreeSet::new();
        let mut width = 0.0_f32;
        let mut height = font_size * self.config.line_height_ratio;
        let mut has_rtl = false;
        for run in buffer.layout_runs() {
            width = width.max(run.line_w);
            height = height.max(run.line_top + run.line_height);
            has_rtl |= run.rtl;
            lines.push(ShapedLine {
                line_index: run.line_i,
                y: run.line_top,
                height: run.line_height,
                width: run.line_w,
                rtl: run.rtl,
            });
            for glyph in run.glyphs {
                if glyphs.len() >= self.config.max_glyphs {
                    return Err(NativeTextError::GlyphLimitExceeded);
                }
                let font_id = glyph.font_id.to_string();
                font_ids.insert(font_id.clone());
                let rtl = glyph.level.is_rtl();
                has_rtl |= rtl;
                glyphs.push(ShapedGlyph {
                    line_index: run.line_i,
                    source_start: glyph.start,
                    source_end: glyph.end,
                    x: glyph.x,
                    y: run.line_top + glyph.y,
                    width: glyph.w,
                    height: run.line_height,
                    rtl,
                    font_id,
                });
            }
        }
        Ok(ShapedText {
            size: Size::new(f64::from(width), f64::from(height)),
            lines,
            glyphs,
            font_ids: font_ids.into_iter().collect(),
            has_rtl,
        })
    }

    pub fn prepare_scene(
        &mut self,
        scene: &PaintScene,
        scale: f32,
    ) -> Result<PreparedPaintScene, NativeTextError> {
        self.prepare_scene_with_focus(scene, scale, None)
    }

    pub fn prepare_scene_with_focus(
        &mut self,
        scene: &PaintScene,
        scale: f32,
        focused: Option<NodeId>,
    ) -> Result<PreparedPaintScene, NativeTextError> {
        if !scale.is_finite() || scale <= 0.0 {
            return Err(NativeTextError::InvalidScale);
        }
        if !scene.viewport.width.is_finite()
            || !scene.viewport.height.is_finite()
            || scene.viewport.width < 0.0
            || scene.viewport.height < 0.0
        {
            return Err(NativeTextError::InvalidViewport);
        }
        let width = checked_surface_dimension(scene.viewport.width, scale)?;
        let height = checked_surface_dimension(scene.viewport.height, scale)?;
        let pixels = usize::try_from(width)
            .ok()
            .and_then(|width| {
                usize::try_from(height)
                    .ok()
                    .and_then(|height| width.checked_mul(height))
            })
            .ok_or(NativeTextError::SurfaceSizeOverflow)?;
        if pixels > self.config.max_surface_pixels {
            return Err(NativeTextError::SurfaceLimitExceeded);
        }
        let mut surface = SoftwareSurface::new(width, height)?;
        let surface_clip = PixelClip::surface(width, height);
        let mut text_runs = 0_usize;
        let mut glyph_count = 0_usize;
        let mut rtl_runs = 0_usize;
        let mut selection_rects = 0_usize;
        let mut carets = 0_usize;
        let mut font_ids = BTreeSet::new();
        let mut text_bytes = 0_usize;
        for command in scene.commands() {
            match command {
                DrawCommand::FillRect {
                    rect,
                    clip,
                    color,
                    radius,
                    ..
                } => {
                    let clip = clip
                        .map(|clip| PixelClip::from_rect(clip, scale, surface_clip))
                        .unwrap_or(surface_clip);
                    let x = finite_i32((rect.x as f32 * scale).floor());
                    let y = finite_i32((rect.y as f32 * scale).floor());
                    let right = finite_i32(((rect.x + rect.width) as f32 * scale).ceil());
                    let bottom = finite_i32(((rect.y + rect.height) as f32 * scale).ceil());
                    let width = u32::try_from(right.saturating_sub(x)).unwrap_or(0);
                    let height = u32::try_from(bottom.saturating_sub(y)).unwrap_or(0);
                    surface.blend_rounded_rect(
                        PixelRect {
                            x,
                            y,
                            width,
                            height,
                        },
                        *radius as f32 * scale,
                        *color,
                        clip,
                    );
                }
                DrawCommand::StrokeRect {
                    rect,
                    clip,
                    color,
                    radius,
                    width: stroke_width,
                    ..
                } => {
                    let clip = clip
                        .map(|clip| PixelClip::from_rect(clip, scale, surface_clip))
                        .unwrap_or(surface_clip);
                    let x = finite_i32((rect.x as f32 * scale).floor());
                    let y = finite_i32((rect.y as f32 * scale).floor());
                    let right = finite_i32(((rect.x + rect.width) as f32 * scale).ceil());
                    let bottom = finite_i32(((rect.y + rect.height) as f32 * scale).ceil());
                    let width = u32::try_from(right.saturating_sub(x)).unwrap_or(0);
                    let height = u32::try_from(bottom.saturating_sub(y)).unwrap_or(0);
                    surface.blend_rounded_border(
                        PixelRect {
                            x,
                            y,
                            width,
                            height,
                        },
                        *radius as f32 * scale,
                        *stroke_width as f32 * scale,
                        *color,
                        clip,
                    );
                }
                DrawCommand::Scrollbar {
                    track,
                    thumb,
                    color,
                    ..
                } => {
                    let track = PixelClip::from_rect(*track, scale, surface_clip);
                    let x = finite_i32((thumb.x as f32 * scale).floor());
                    let y = finite_i32((thumb.y as f32 * scale).floor());
                    let right = finite_i32(((thumb.x + thumb.width) as f32 * scale).ceil());
                    let bottom = finite_i32(((thumb.y + thumb.height) as f32 * scale).ceil());
                    surface.blend_rect(
                        x,
                        y,
                        u32::try_from(right.saturating_sub(x)).unwrap_or(0),
                        u32::try_from(bottom.saturating_sub(y)).unwrap_or(0),
                        *color,
                        track,
                    );
                }
                DrawCommand::Text {
                    rect,
                    clip,
                    color,
                    font_size,
                    font_weight,
                    value,
                    ..
                } => {
                    text_bytes = text_bytes
                        .checked_add(value.len())
                        .ok_or(NativeTextError::TextLimitExceeded)?;
                    if text_bytes > self.config.max_text_bytes {
                        return Err(NativeTextError::TextLimitExceeded);
                    }
                    let rendered = self.render_text(
                        &mut surface,
                        value,
                        *font_size,
                        *font_weight,
                        *rect,
                        *clip,
                        *color,
                        scale,
                        surface_clip,
                        None,
                    );
                    merge_rendered_text(
                        rendered?,
                        &mut text_runs,
                        &mut glyph_count,
                        &mut rtl_runs,
                        &mut selection_rects,
                        &mut carets,
                        &mut font_ids,
                        self.config.max_glyphs,
                    )?;
                }
                DrawCommand::TextEditor {
                    node,
                    rect,
                    clip,
                    color,
                    font_size,
                    font_weight,
                    value,
                    placeholder,
                    selection_start_utf16,
                    selection_length_utf16,
                } => {
                    text_bytes = text_bytes
                        .checked_add(value.len())
                        .and_then(|bytes| bytes.checked_add(placeholder.len()))
                        .ok_or(NativeTextError::TextLimitExceeded)?;
                    if text_bytes > self.config.max_text_bytes {
                        return Err(NativeTextError::TextLimitExceeded);
                    }
                    let displayed = if value.is_empty() { placeholder } else { value };
                    let displayed_color = if value.is_empty() {
                        with_alpha(*color, ((*color >> 24) as u8) / 2)
                    } else {
                        *color
                    };
                    let selection = (focused == Some(*node)).then_some(EditorSelection {
                        source: value,
                        start_utf16: *selection_start_utf16,
                        length_utf16: *selection_length_utf16,
                    });
                    let rendered = self.render_text(
                        &mut surface,
                        displayed,
                        *font_size,
                        *font_weight,
                        *rect,
                        *clip,
                        displayed_color,
                        scale,
                        surface_clip,
                        selection,
                    );
                    merge_rendered_text(
                        rendered?,
                        &mut text_runs,
                        &mut glyph_count,
                        &mut rtl_runs,
                        &mut selection_rects,
                        &mut carets,
                        &mut font_ids,
                        self.config.max_glyphs,
                    )?;
                }
            }
        }
        Ok(PreparedPaintScene {
            revision: scene.revision,
            frame_id: scene.revision,
            scale_bits: scale.to_bits(),
            surface,
            text: SceneTextDiagnostics {
                text_runs,
                glyphs: glyph_count,
                rtl_runs,
                selection_rects,
                carets,
                font_ids: font_ids.into_iter().collect(),
            },
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn render_text(
        &mut self,
        surface: &mut SoftwareSurface,
        value: &str,
        font_size: f64,
        font_weight: i64,
        rect: Rect,
        clip: Option<Rect>,
        color: u32,
        scale: f32,
        surface_clip: PixelClip,
        selection: Option<EditorSelection<'_>>,
    ) -> Result<RenderedText, NativeTextError> {
        let scaled_font =
            checked_positive_f32(font_size).ok_or(NativeTextError::InvalidMetrics)? * scale;
        if !scaled_font.is_finite() || scaled_font <= 0.0 {
            return Err(NativeTextError::InvalidMetrics);
        }
        let buffer_width = checked_optional_dimension(rect.width * f64::from(scale))?;
        let buffer_height = checked_optional_dimension(rect.height * f64::from(scale))?;
        let mut buffer =
            self.make_buffer(value, scaled_font, font_weight, buffer_width, buffer_height);
        let clip = clip
            .map(|clip| PixelClip::from_rect(clip, scale, surface_clip))
            .unwrap_or(surface_clip);
        let rect_clip = PixelClip::from_rect(rect, scale, surface_clip);
        let clip = PixelClip {
            left: clip.left.max(rect_clip.left),
            top: clip.top.max(rect_clip.top),
            right: clip.right.min(rect_clip.right),
            bottom: clip.bottom.min(rect_clip.bottom),
        };
        let origin_x = finite_i32((rect.x as f32 * scale).round());
        let origin_y = finite_i32((rect.y as f32 * scale).round());
        let mut rendered = RenderedText::default();
        let line_offsets = buffer_line_offsets(&buffer);
        let selection_range = selection.map(|selection| {
            let start = utf16_offset_to_byte(selection.source, selection.start_utf16);
            let end_utf16 = selection.start_utf16.saturating_add(selection.length_utf16);
            let end = utf16_offset_to_byte(selection.source, end_utf16);
            (start, end.max(start))
        });
        let mut caret_drawn = false;
        for run in buffer.layout_runs() {
            rendered.text_runs += 1;
            rendered.rtl_runs += usize::from(run.rtl);
            rendered.glyphs += run.glyphs.len();
            rendered
                .font_ids
                .extend(run.glyphs.iter().map(|glyph| glyph.font_id.to_string()));
            let line_offset = line_offsets.get(run.line_i).copied().unwrap_or(0);
            if let Some((start, end)) = selection_range {
                if start < end {
                    for glyph in run.glyphs {
                        let glyph_start = line_offset.saturating_add(glyph.start);
                        let glyph_end = line_offset.saturating_add(glyph.end);
                        if glyph_start < end && glyph_end > start {
                            surface.blend_rect(
                                origin_x.saturating_add(finite_i32(glyph.x.floor())),
                                origin_y.saturating_add(finite_i32(run.line_top.floor())),
                                glyph.w.ceil().max(1.0) as u32,
                                run.line_height.ceil().max(1.0) as u32,
                                0x663b82f6,
                                clip,
                            );
                            rendered.selection_rects += 1;
                        }
                    }
                } else if !caret_drawn {
                    let line_end = line_offset.saturating_add(run.text.len());
                    if start >= line_offset && start <= line_end {
                        if let Some(caret_x) = caret_x(&run, line_offset, start) {
                            surface.blend_rect(
                                origin_x.saturating_add(finite_i32(caret_x.round())),
                                origin_y.saturating_add(finite_i32(run.line_top.floor())),
                                scale.ceil().max(1.0) as u32,
                                run.line_height.ceil().max(1.0) as u32,
                                0xff1f6feb,
                                clip,
                            );
                            rendered.carets += 1;
                            caret_drawn = true;
                        }
                    }
                }
            }
        }
        if selection_range.is_some_and(|(start, end)| start == end) && !caret_drawn {
            surface.blend_rect(
                origin_x,
                origin_y,
                scale.ceil().max(1.0) as u32,
                scaled_font
                    .mul_add(self.config.line_height_ratio, 0.0)
                    .ceil() as u32,
                0xff1f6feb,
                clip,
            );
            rendered.carets = 1;
        }
        let font_system = &mut self.font_system;
        let raster_cache = &mut self.raster_cache;
        buffer.draw(
            font_system,
            raster_cache,
            Color(color),
            |x, y, width, height, color| {
                surface.blend_rect(
                    origin_x.saturating_add(x),
                    origin_y.saturating_add(y),
                    width,
                    height,
                    color.0,
                    clip,
                );
            },
        );
        Ok(rendered)
    }

    fn validate_text(&self, text: &str) -> Result<(), NativeTextError> {
        if text.len() > self.config.max_text_bytes {
            return Err(NativeTextError::TextLimitExceeded);
        }
        Ok(())
    }

    fn make_buffer(
        &mut self,
        text: &str,
        font_size: f32,
        font_weight: i64,
        width: Option<f32>,
        height: Option<f32>,
    ) -> Buffer {
        let metrics = Metrics::relative(font_size, self.config.line_height_ratio);
        let mut buffer = Buffer::new(&mut self.font_system, metrics);
        buffer.set_wrap(Wrap::WordOrGlyph);
        buffer.set_size(width, height);
        let weight = Weight(font_weight.clamp(1, 1_000) as u16);
        buffer.set_text(text, &Attrs::new().weight(weight), Shaping::Advanced, None);
        buffer.shape_until_scroll(&mut self.font_system, false);
        buffer
    }
}

impl Default for NativeTextSystem {
    fn default() -> Self {
        Self::new(NativeTextConfig::default()).expect("default native text config is valid")
    }
}

impl IntrinsicMeasurer for NativeTextSystem {
    fn measure_text(&mut self, _node: NodeId, text: &str, font_size: f64, max_width: f64) -> Size {
        match self.shape_text(text, font_size, 400, max_width) {
            Ok(shaped) => {
                self.last_measure_error = None;
                shaped.size
            }
            Err(error) => {
                self.last_measure_error = Some(error);
                Size::default()
            }
        }
    }
}

fn checked_positive_f32(value: f64) -> Option<f32> {
    let value = value as f32;
    (value.is_finite() && value > 0.0).then_some(value)
}

fn checked_optional_dimension(value: f64) -> Result<Option<f32>, NativeTextError> {
    if !value.is_finite() || value < 0.0 {
        return Err(NativeTextError::InvalidMetrics);
    }
    let value = value as f32;
    if !value.is_finite() {
        return Err(NativeTextError::InvalidMetrics);
    }
    Ok((value > 0.0).then_some(value))
}

fn checked_surface_dimension(value: f64, scale: f32) -> Result<u32, NativeTextError> {
    let scaled = value * f64::from(scale);
    if !scaled.is_finite() || scaled < 0.0 || scaled > f64::from(u32::MAX) {
        return Err(NativeTextError::SurfaceSizeOverflow);
    }
    Ok(scaled.ceil() as u32)
}

fn finite_i32(value: f32) -> i32 {
    if value.is_nan() {
        0
    } else if value <= i32::MIN as f32 {
        i32::MIN
    } else if value >= i32::MAX as f32 {
        i32::MAX
    } else {
        value as i32
    }
}

#[allow(clippy::too_many_arguments)]
fn merge_rendered_text(
    rendered: RenderedText,
    text_runs: &mut usize,
    glyphs: &mut usize,
    rtl_runs: &mut usize,
    selection_rects: &mut usize,
    carets: &mut usize,
    font_ids: &mut BTreeSet<String>,
    max_glyphs: usize,
) -> Result<(), NativeTextError> {
    *text_runs = text_runs
        .checked_add(rendered.text_runs)
        .ok_or(NativeTextError::GlyphLimitExceeded)?;
    *glyphs = glyphs
        .checked_add(rendered.glyphs)
        .ok_or(NativeTextError::GlyphLimitExceeded)?;
    if *glyphs > max_glyphs {
        return Err(NativeTextError::GlyphLimitExceeded);
    }
    *rtl_runs = rtl_runs
        .checked_add(rendered.rtl_runs)
        .ok_or(NativeTextError::GlyphLimitExceeded)?;
    *selection_rects = selection_rects
        .checked_add(rendered.selection_rects)
        .ok_or(NativeTextError::GlyphLimitExceeded)?;
    *carets = carets
        .checked_add(rendered.carets)
        .ok_or(NativeTextError::GlyphLimitExceeded)?;
    font_ids.extend(rendered.font_ids);
    Ok(())
}

fn buffer_line_offsets(buffer: &Buffer) -> Vec<usize> {
    let mut offsets = Vec::with_capacity(buffer.lines.len());
    let mut offset = 0_usize;
    for line in &buffer.lines {
        offsets.push(offset);
        offset = offset
            .saturating_add(line.text().len())
            .saturating_add(line.ending().as_str().len());
    }
    offsets
}

fn utf16_offset_to_byte(text: &str, requested: u32) -> usize {
    let mut utf16_offset = 0_u32;
    for (byte_offset, character) in text.char_indices() {
        if utf16_offset >= requested {
            return byte_offset;
        }
        let next = utf16_offset.saturating_add(character.len_utf16() as u32);
        if next > requested {
            return byte_offset;
        }
        utf16_offset = next;
    }
    text.len()
}

fn caret_x(run: &cosmic_text::LayoutRun<'_>, line_offset: usize, cursor: usize) -> Option<f32> {
    if run.glyphs.is_empty() {
        return Some(0.0);
    }
    for glyph in run.glyphs {
        let start = line_offset.saturating_add(glyph.start);
        let end = line_offset.saturating_add(glyph.end);
        if cursor <= start {
            return Some(if glyph.level.is_rtl() {
                glyph.x + glyph.w
            } else {
                glyph.x
            });
        }
        if cursor <= end {
            return Some(if glyph.level.is_rtl() {
                glyph.x
            } else {
                glyph.x + glyph.w
            });
        }
    }
    run.glyphs.last().map(|glyph| {
        if glyph.level.is_rtl() {
            glyph.x
        } else {
            glyph.x + glyph.w
        }
    })
}

const fn with_alpha(color: u32, alpha: u8) -> u32 {
    (color & 0x00ff_ffff) | ((alpha as u32) << 24)
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_ui_core::{Primitive, Property, PropertyId, Value};
    use vo_ui_layout::{compute_layout, LayoutLimits};
    use vo_ui_paint::{build_paint_scene, PaintLimits};
    use vo_ui_protocol::{Mutation, MutationBatch, NodeKind, ProtocolLimits, TreeMirror};

    #[test]
    fn advanced_shaping_reports_clusters_and_rtl_runs() {
        let mut system = NativeTextSystem::default();
        let latin = system
            .shape_text("office", 18.0, 400, 500.0)
            .expect("shape latin");
        assert!(latin.size.width > 0.0);
        assert!(!latin.glyphs.is_empty());
        assert!(latin.glyphs.len() <= "office".chars().count());

        let rtl = system
            .shape_text("مرحبا بالعالم", 18.0, 400, 500.0)
            .expect("shape Arabic");
        assert!(rtl.has_rtl);
        assert!(rtl.lines.iter().any(|line| line.rtl));
        assert!(rtl.glyphs.iter().any(|glyph| glyph.rtl));
    }

    #[test]
    fn wraps_and_uses_the_same_metrics_for_intrinsic_measurement() {
        let mut system = NativeTextSystem::default();
        let shaped = system
            .shape_text("Volang native text wraps across lines", 16.0, 400, 90.0)
            .expect("shape wrapped text");
        let measured = system.measure_text(
            NodeId::new(1, 1),
            "Volang native text wraps across lines",
            16.0,
            90.0,
        );
        assert!(shaped.lines.len() >= 2);
        assert_eq!(measured, shaped.size);
        assert!(measured.width <= 90.0);
    }

    #[test]
    fn configured_limits_fail_without_a_partial_scene() {
        let mut system = NativeTextSystem::new(NativeTextConfig {
            max_text_bytes: 4,
            ..NativeTextConfig::default()
        })
        .expect("config");
        assert_eq!(
            system.shape_text("too long", 14.0, 400, 100.0),
            Err(NativeTextError::TextLimitExceeded)
        );
        let measured = system.measure_text(NodeId::new(1, 1), "too long", 14.0, 100.0);
        assert_eq!(measured, Size::default());
        assert_eq!(
            system.take_measure_error(),
            Some(NativeTextError::TextLimitExceeded)
        );
    }

    #[test]
    fn rounded_fill_and_border_preserve_corner_and_center_geometry() {
        let clip = PixelClip::surface(12, 12);
        let mut fill = SoftwareSurface::new(12, 12).unwrap();
        let rect = PixelRect {
            x: 0,
            y: 0,
            width: 12,
            height: 12,
        };
        fill.blend_rounded_rect(rect, 4.0, 0xffffffff, clip);
        assert_eq!(fill.pixels_rgba8()[3], 0);
        assert_ne!(fill.pixels_rgba8()[(6 * 12 + 6) * 4 + 3], 0);

        let mut border = SoftwareSurface::new(12, 12).unwrap();
        border.blend_rounded_border(rect, 4.0, 2.0, 0xffffffff, clip);
        assert_eq!(border.pixels_rgba8()[3], 0);
        assert_ne!(border.pixels_rgba8()[(12 + 6) * 4 + 3], 0);
        assert_eq!(border.pixels_rgba8()[(6 * 12 + 6) * 4 + 3], 0);
    }

    #[test]
    fn paint_scene_rasterizes_background_and_text_with_clipping() {
        let root = NodeId::new(0, 1);
        let text = NodeId::new(1, 1);
        let mut tree = TreeMirror::new(9, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            9,
            1,
            vec![
                Mutation::SetProperty {
                    id: root,
                    property: Property::new(PropertyId::BACKGROUND, Value::Color(0xff102030)),
                },
                Mutation::Create {
                    id: text,
                    kind: NodeKind::Text,
                },
                Mutation::SetText {
                    id: text,
                    text: "Volang UI".into(),
                },
                Mutation::SetProperty {
                    id: root,
                    property: Property::new(PropertyId::FOREGROUND, Value::Color(0xffffffff)),
                },
                Mutation::SetProperty {
                    id: root,
                    property: Property::new(PropertyId::FONT_SIZE, Value::I64(20)),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: text,
                    before: None,
                },
            ],
        ))
        .expect("tree");
        let mut system = NativeTextSystem::default();
        let layout = compute_layout(
            &tree,
            Size::new(240.0, 80.0),
            LayoutLimits::default(),
            &mut system,
        )
        .expect("layout");
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).expect("paint");
        let prepared = system.prepare_scene(&scene, 2.0).expect("prepare");
        assert_eq!(prepared.revision, 1);
        assert_eq!(
            (prepared.surface.width(), prepared.surface.height()),
            (480, 160)
        );
        assert!(prepared.surface.non_transparent_pixel_count() > 60_000);
        assert!(prepared.text.glyphs > 0);
        assert!(!prepared.text.font_ids.is_empty());
    }

    #[test]
    fn surface_pixel_budget_is_checked_before_allocation() {
        let root = NodeId::new(0, 1);
        let tree = TreeMirror::new(2, root, ProtocolLimits::default());
        let system = NativeTextSystem::new(NativeTextConfig {
            max_surface_pixels: 10,
            ..NativeTextConfig::default()
        })
        .expect("config");
        let mut measurer = system;
        let layout = compute_layout(
            &tree,
            Size::new(4.0, 4.0),
            LayoutLimits::default(),
            &mut measurer,
        )
        .expect("layout");
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).expect("paint");
        assert_eq!(
            measurer.prepare_scene(&scene, 1.0),
            Err(NativeTextError::SurfaceLimitExceeded)
        );
    }

    #[test]
    fn focused_text_editor_renders_utf16_selection_and_caret() {
        let root = NodeId::new(0, 1);
        let input = NodeId::new(1, 1);
        let mut tree = TreeMirror::new(4, root, ProtocolLimits::default());
        tree.apply(&MutationBatch::new(
            4,
            1,
            vec![
                Mutation::SetProperty {
                    id: root,
                    property: Property::new(PropertyId::FONT_SIZE, Value::I64(18)),
                },
                Mutation::Create {
                    id: input,
                    kind: NodeKind::Element(Primitive::TextInput),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::VALUE, Value::Text("A😀B".into())),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::SELECTION_START_UTF16, Value::I64(1)),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::SELECTION_LENGTH_UTF16, Value::I64(2)),
                },
                Mutation::InsertBefore {
                    parent: root,
                    child: input,
                    before: None,
                },
            ],
        ))
        .unwrap();
        let mut system = NativeTextSystem::default();
        let layout = compute_layout(
            &tree,
            Size::new(200.0, 60.0),
            LayoutLimits::default(),
            &mut system,
        )
        .unwrap();
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).unwrap();
        assert!(matches!(
            scene.commands().last(),
            Some(DrawCommand::TextEditor { node, .. }) if *node == input
        ));
        let selected = system
            .prepare_scene_with_focus(&scene, 1.0, Some(input))
            .unwrap();
        assert!(selected.text.selection_rects > 0);
        assert_eq!(selected.text.carets, 0);

        tree.apply(&MutationBatch::new(
            4,
            2,
            vec![
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::SELECTION_START_UTF16, Value::I64(3)),
                },
                Mutation::SetProperty {
                    id: input,
                    property: Property::new(PropertyId::SELECTION_LENGTH_UTF16, Value::I64(0)),
                },
            ],
        ))
        .unwrap();
        let layout = compute_layout(
            &tree,
            Size::new(200.0, 60.0),
            LayoutLimits::default(),
            &mut system,
        )
        .unwrap();
        let scene = build_paint_scene(&tree, &layout, PaintLimits::default()).unwrap();
        let caret = system
            .prepare_scene_with_focus(&scene, 1.0, Some(input))
            .unwrap();
        assert_eq!(caret.text.selection_rects, 0);
        assert_eq!(caret.text.carets, 1);
        let blurred = system.prepare_scene(&scene, 1.0).unwrap();
        assert_eq!(blurred.text.carets, 0);
    }

    #[test]
    fn replaced_primitives_keep_layout_trait_defaults() {
        let mut system = NativeTextSystem::default();
        assert_eq!(
            system.measure_replaced(
                NodeId::new(1, 1),
                Primitive::TextInput,
                Size::new(300.0, 200.0)
            ),
            Size::new(160.0, 32.0)
        );
    }
}
