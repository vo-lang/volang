//! WGPU upload and composition boundary for prepared Volang UI scenes.

use std::fmt;

use vo_app_host_native::WgpuCompositorAdapter;
use vo_app_protocol::SurfaceHandle;
use vo_app_runtime::{
    NativeCompositorError, NativeLayerSubmission, SurfaceGeometry, SurfaceInputPolicy, SurfaceKind,
};
use vo_ui_text_native::PreparedPaintScene;

pub trait Rgba8LayerUploader {
    fn device_generation(&self) -> u64;

    fn upload_rgba8(
        &mut self,
        surface: SurfaceHandle,
        texture_token: u64,
        device_generation: u64,
        width: u32,
        height: u32,
        pixels: &[u8],
    ) -> Result<(), NativeCompositorError>;
}

impl Rgba8LayerUploader for WgpuCompositorAdapter<'_> {
    fn device_generation(&self) -> u64 {
        self.device_generation()
    }

    fn upload_rgba8(
        &mut self,
        surface: SurfaceHandle,
        texture_token: u64,
        device_generation: u64,
        width: u32,
        height: u32,
        pixels: &[u8],
    ) -> Result<(), NativeCompositorError> {
        self.upload_rgba8_layer_texture(
            surface,
            texture_token,
            device_generation,
            width,
            height,
            pixels,
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct WgpuScenePresenterConfig {
    pub surface: SurfaceHandle,
    pub z_order: i32,
    pub input: SurfaceInputPolicy,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum WgpuScenePresenterError {
    InvalidConfig,
    EmptySurface,
    StaleRevision { current: u64, candidate: u64 },
    TextureTokenExhausted,
    Upload(NativeCompositorError),
}

impl fmt::Display for WgpuScenePresenterError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "native UI WGPU presenter error: {self:?}")
    }
}

impl std::error::Error for WgpuScenePresenterError {}

/// Turns a revision-bound RGBA scene into a retained compositor layer.
/// Texture tokens and revisions advance only after a successful GPU upload.
pub struct WgpuScenePresenter {
    config: WgpuScenePresenterConfig,
    last_revision: u64,
    last_frame_id: u64,
    next_texture_token: u64,
}

impl WgpuScenePresenter {
    pub fn new(config: WgpuScenePresenterConfig) -> Result<Self, WgpuScenePresenterError> {
        if !config.surface.is_valid() {
            return Err(WgpuScenePresenterError::InvalidConfig);
        }
        Ok(Self {
            config,
            last_revision: 0,
            last_frame_id: 0,
            next_texture_token: 1,
        })
    }

    pub const fn last_revision(&self) -> u64 {
        self.last_revision
    }

    pub const fn next_texture_token(&self) -> u64 {
        self.next_texture_token
    }

    pub const fn last_frame_id(&self) -> u64 {
        self.last_frame_id
    }

    /// Drops revision affinity after the compositor publishes a fresh device
    /// generation. Texture tokens stay monotonic, while the current prepared
    /// frame may be uploaded again because every old GPU layer was discarded.
    pub fn reset_after_device_loss(&mut self) {
        self.last_revision = 0;
        self.last_frame_id = 0;
    }

    pub fn upload<U: Rgba8LayerUploader>(
        &mut self,
        uploader: &mut U,
        frame: &PreparedPaintScene,
    ) -> Result<NativeLayerSubmission, WgpuScenePresenterError> {
        if frame.surface.width() == 0 || frame.surface.height() == 0 {
            return Err(WgpuScenePresenterError::EmptySurface);
        }
        if frame.revision < self.last_revision || frame.frame_id <= self.last_frame_id {
            return Err(WgpuScenePresenterError::StaleRevision {
                current: self.last_revision,
                candidate: frame.revision,
            });
        }
        let texture_token = self.next_texture_token;
        let next_texture_token = texture_token
            .checked_add(1)
            .ok_or(WgpuScenePresenterError::TextureTokenExhausted)?;
        let device_generation = uploader.device_generation();
        uploader
            .upload_rgba8(
                self.config.surface,
                texture_token,
                device_generation,
                frame.surface.width(),
                frame.surface.height(),
                frame.surface.pixels_rgba8(),
            )
            .map_err(WgpuScenePresenterError::Upload)?;
        self.last_revision = frame.revision;
        self.last_frame_id = frame.frame_id;
        self.next_texture_token = next_texture_token;
        Ok(NativeLayerSubmission {
            surface: self.config.surface,
            kind: SurfaceKind::Ui,
            z_order: self.config.z_order,
            input: self.config.input,
            content_revision: frame.revision,
            texture_token,
            device_generation,
            geometry: SurfaceGeometry::default(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_app_protocol::GenerationalHandle;
    use vo_ui_text_native::{SceneTextDiagnostics, SoftwareSurface};

    #[derive(Default)]
    struct FakeUploader {
        generation: u64,
        fail: Option<NativeCompositorError>,
        uploads: Vec<(SurfaceHandle, u64, u64, u32, u32, usize)>,
    }

    impl Rgba8LayerUploader for FakeUploader {
        fn device_generation(&self) -> u64 {
            self.generation
        }

        fn upload_rgba8(
            &mut self,
            surface: SurfaceHandle,
            texture_token: u64,
            device_generation: u64,
            width: u32,
            height: u32,
            pixels: &[u8],
        ) -> Result<(), NativeCompositorError> {
            if let Some(error) = self.fail {
                return Err(error);
            }
            self.uploads.push((
                surface,
                texture_token,
                device_generation,
                width,
                height,
                pixels.len(),
            ));
            Ok(())
        }
    }

    fn surface() -> SurfaceHandle {
        GenerationalHandle {
            index: 3,
            generation: 1,
        }
    }

    fn frame(revision: u64) -> PreparedPaintScene {
        PreparedPaintScene {
            revision,
            frame_id: revision,
            scale_bits: 1.0_f32.to_bits(),
            surface: SoftwareSurface::from_rgba8(2, 3, vec![255; 24]).unwrap(),
            text: SceneTextDiagnostics {
                text_runs: 0,
                glyphs: 0,
                rtl_runs: 0,
                selection_rects: 0,
                carets: 0,
                font_ids: Vec::new(),
            },
        }
    }

    #[test]
    fn successful_upload_returns_a_revision_bound_ui_layer() {
        let mut presenter = WgpuScenePresenter::new(WgpuScenePresenterConfig {
            surface: surface(),
            z_order: 10,
            input: SurfaceInputPolicy::Interactive,
        })
        .unwrap();
        let mut uploader = FakeUploader {
            generation: 7,
            ..FakeUploader::default()
        };
        let submission = presenter.upload(&mut uploader, &frame(5)).unwrap();
        assert_eq!(submission.surface, surface());
        assert_eq!(submission.kind, SurfaceKind::Ui);
        assert_eq!(submission.content_revision, 5);
        assert_eq!(submission.texture_token, 1);
        assert_eq!(submission.device_generation, 7);
        assert_eq!(presenter.last_revision(), 5);
        assert_eq!(presenter.next_texture_token(), 2);
        assert_eq!(
            (
                uploader.uploads[0].3,
                uploader.uploads[0].4,
                uploader.uploads[0].5
            ),
            (2, 3, 24)
        );
    }

    #[test]
    fn failed_upload_keeps_revision_and_token_unchanged() {
        let mut presenter = WgpuScenePresenter::new(WgpuScenePresenterConfig {
            surface: surface(),
            z_order: 0,
            input: SurfaceInputPolicy::Interactive,
        })
        .unwrap();
        let mut uploader = FakeUploader {
            generation: 7,
            fail: Some(NativeCompositorError::DeviceLost),
            uploads: Vec::new(),
        };
        assert_eq!(
            presenter.upload(&mut uploader, &frame(1)),
            Err(WgpuScenePresenterError::Upload(
                NativeCompositorError::DeviceLost
            ))
        );
        assert_eq!(presenter.last_revision(), 0);
        assert_eq!(presenter.next_texture_token(), 1);
    }

    #[test]
    fn duplicate_or_older_revisions_are_rejected_before_upload() {
        let mut presenter = WgpuScenePresenter::new(WgpuScenePresenterConfig {
            surface: surface(),
            z_order: 0,
            input: SurfaceInputPolicy::Interactive,
        })
        .unwrap();
        let mut uploader = FakeUploader {
            generation: 7,
            ..FakeUploader::default()
        };
        presenter.upload(&mut uploader, &frame(4)).unwrap();
        assert_eq!(
            presenter.upload(&mut uploader, &frame(4)),
            Err(WgpuScenePresenterError::StaleRevision {
                current: 4,
                candidate: 4
            })
        );
        assert_eq!(uploader.uploads.len(), 1);
    }

    #[test]
    fn device_loss_reset_allows_the_current_frame_to_be_reuploaded() {
        let mut presenter = WgpuScenePresenter::new(WgpuScenePresenterConfig {
            surface: surface(),
            z_order: 0,
            input: SurfaceInputPolicy::Exclusive,
        })
        .unwrap();
        let mut uploader = FakeUploader {
            generation: 1,
            ..FakeUploader::default()
        };
        let current = frame(7);
        presenter.upload(&mut uploader, &current).unwrap();
        uploader.generation = 2;
        presenter.reset_after_device_loss();
        let recovered = presenter.upload(&mut uploader, &current).unwrap();
        assert_eq!(recovered.device_generation, 2);
        assert_eq!(uploader.uploads.len(), 2);
        assert_ne!(uploader.uploads[0].1, uploader.uploads[1].1);
    }

    #[test]
    fn a_new_native_frame_can_present_the_same_ui_revision() {
        let mut presenter = WgpuScenePresenter::new(WgpuScenePresenterConfig {
            surface: surface(),
            z_order: 0,
            input: SurfaceInputPolicy::Interactive,
        })
        .unwrap();
        let mut uploader = FakeUploader {
            generation: 7,
            ..FakeUploader::default()
        };
        let first = frame(8);
        presenter.upload(&mut uploader, &first).unwrap();
        let mut local_scroll_frame = frame(8);
        local_scroll_frame.frame_id = 9;
        let submission = presenter
            .upload(&mut uploader, &local_scroll_frame)
            .unwrap();
        assert_eq!(submission.content_revision, 8);
        assert_eq!(submission.texture_token, 2);
        assert_eq!(presenter.last_frame_id(), 9);
    }
}
