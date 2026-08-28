#![cfg_attr(not(test), no_std)]

//! Renderer-neutral contracts for clipboard, dialogs, menus, and drag/drop.

extern crate alloc;

mod codec;

pub use codec::{
    decode_system_request, decode_system_response, encode_system_request, encode_system_response,
    SystemCodecError, SystemFailure, SystemFailureKind, SystemRequest, SystemRequestEnvelope,
    SystemResponse, SystemResponseEnvelope,
};

use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::vec::Vec;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SystemLimits {
    pub max_text_bytes: usize,
    pub max_image_pixels: usize,
    pub max_paths: usize,
    pub max_path_bytes: usize,
    pub max_filters: usize,
    pub max_extensions_per_filter: usize,
    pub max_menu_items: usize,
    pub max_menu_depth: usize,
}

impl Default for SystemLimits {
    fn default() -> Self {
        Self {
            max_text_bytes: 16 * 1_024 * 1_024,
            max_image_pixels: 16_777_216,
            max_paths: 1_024,
            max_path_bytes: 32 * 1_024,
            max_filters: 64,
            max_extensions_per_filter: 64,
            max_menu_items: 4_096,
            max_menu_depth: 16,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SystemContractError {
    InvalidLimits,
    TextLimitExceeded,
    ImageLimitExceeded,
    InvalidImage,
    PathLimitExceeded,
    InvalidPath,
    FilterLimitExceeded,
    InvalidFilter,
    MenuLimitExceeded,
    MenuDepthExceeded,
    InvalidMenuIdentity,
    DuplicateMenuIdentity,
    EmptyMenuLabel,
    InvalidDragRequest,
    InvalidHostInvocation,
    PayloadLimitExceeded,
}

// HostInvocation is the versioned escape hatch for application-owned service
// protocols. The UI runtime transports opaque bytes while capability names,
// authorization, schemas, and semantics remain owned by the application host.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HostInvocation {
    pub service: String,
    pub operation: String,
    pub payload: Vec<u8>,
}

impl HostInvocation {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        if self.service.is_empty()
            || self.operation.is_empty()
            || self.service.len() > 255
            || self.operation.len() > 255
            || self
                .service
                .bytes()
                .any(|value| !(0x21..=0x7e).contains(&value))
            || self
                .operation
                .bytes()
                .any(|value| !(0x21..=0x7e).contains(&value))
        {
            return Err(SystemContractError::InvalidHostInvocation);
        }
        if self.payload.len() > limits.max_text_bytes {
            return Err(SystemContractError::PayloadLimitExceeded);
        }
        Ok(())
    }
}

impl SystemLimits {
    pub const fn is_valid(self) -> bool {
        self.max_text_bytes > 0
            && self.max_image_pixels > 0
            && self.max_paths > 0
            && self.max_path_bytes > 0
            && self.max_filters > 0
            && self.max_extensions_per_filter > 0
            && self.max_menu_items > 0
            && self.max_menu_depth > 0
    }

    pub fn validate_text(self, value: &str) -> Result<(), SystemContractError> {
        if !self.is_valid() {
            return Err(SystemContractError::InvalidLimits);
        }
        if value.len() > self.max_text_bytes {
            return Err(SystemContractError::TextLimitExceeded);
        }
        Ok(())
    }

    pub fn validate_paths(self, paths: &[String]) -> Result<(), SystemContractError> {
        if !self.is_valid() {
            return Err(SystemContractError::InvalidLimits);
        }
        if paths.len() > self.max_paths {
            return Err(SystemContractError::PathLimitExceeded);
        }
        for path in paths {
            if path.is_empty() || path.len() > self.max_path_bytes || path.contains('\0') {
                return Err(SystemContractError::InvalidPath);
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ClipboardContent {
    Text(String),
    Html { html: String, plain_text: String },
    Rgba8(ClipboardImage),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ClipboardFormat {
    Text,
    Html,
    Rgba8,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ClipboardImage {
    pub width: u32,
    pub height: u32,
    pub pixels: Vec<u8>,
}

impl ClipboardContent {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        match self {
            Self::Text(text) => limits.validate_text(text),
            Self::Html { html, plain_text } => {
                limits.validate_text(html)?;
                limits.validate_text(plain_text)
            }
            Self::Rgba8(image) => image.validate(limits),
        }
    }
}

impl ClipboardImage {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        if !limits.is_valid() {
            return Err(SystemContractError::InvalidLimits);
        }
        if self.width == 0 || self.height == 0 {
            return Err(SystemContractError::InvalidImage);
        }
        let pixels = (self.width as usize)
            .checked_mul(self.height as usize)
            .ok_or(SystemContractError::ImageLimitExceeded)?;
        if pixels > limits.max_image_pixels {
            return Err(SystemContractError::ImageLimitExceeded);
        }
        let bytes = pixels
            .checked_mul(4)
            .ok_or(SystemContractError::ImageLimitExceeded)?;
        if self.pixels.len() != bytes {
            return Err(SystemContractError::InvalidImage);
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FileDialogKind {
    OpenFile,
    OpenFiles,
    OpenFolder,
    OpenFolders,
    SaveFile,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileDialogFilter {
    pub name: String,
    pub extensions: Vec<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileDialogRequest {
    pub kind: FileDialogKind,
    pub title: String,
    pub initial_directory: Option<String>,
    pub initial_file_name: Option<String>,
    pub filters: Vec<FileDialogFilter>,
    pub can_create_directories: bool,
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct FileDialogResult {
    pub paths: Vec<String>,
}

impl FileDialogRequest {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        limits.validate_text(&self.title)?;
        if let Some(directory) = &self.initial_directory {
            limits.validate_paths(core::slice::from_ref(directory))?;
        }
        if let Some(file_name) = &self.initial_file_name {
            if file_name.is_empty()
                || file_name.len() > limits.max_path_bytes
                || file_name.contains('\0')
            {
                return Err(SystemContractError::InvalidPath);
            }
        }
        if self.filters.len() > limits.max_filters {
            return Err(SystemContractError::FilterLimitExceeded);
        }
        for filter in &self.filters {
            limits.validate_text(&filter.name)?;
            if filter.name.is_empty()
                || filter.extensions.is_empty()
                || filter.extensions.len() > limits.max_extensions_per_filter
            {
                return Err(SystemContractError::InvalidFilter);
            }
            for extension in &filter.extensions {
                if extension.is_empty()
                    || extension.len() > limits.max_path_bytes
                    || extension.starts_with('.')
                    || extension.contains('/')
                    || extension.contains('\\')
                    || extension.contains('\0')
                {
                    return Err(SystemContractError::InvalidFilter);
                }
            }
        }
        Ok(())
    }
}

impl FileDialogResult {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        limits.validate_paths(&self.paths)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FileDragMode {
    Copy,
    Move,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileDragRequest {
    pub paths: Vec<String>,
    pub preview: Option<String>,
    pub mode: FileDragMode,
}

impl FileDragRequest {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        if self.paths.is_empty() {
            return Err(SystemContractError::InvalidDragRequest);
        }
        limits.validate_paths(&self.paths)?;
        if let Some(preview) = &self.preview {
            limits.validate_paths(core::slice::from_ref(preview))?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MessageDialogLevel {
    Info,
    Warning,
    Error,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MessageDialogButtons {
    Ok,
    OkCancel,
    YesNo,
    YesNoCancel,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MessageDialogRequest {
    pub level: MessageDialogLevel,
    pub buttons: MessageDialogButtons,
    pub title: String,
    pub description: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MessageDialogResult {
    Ok,
    Cancel,
    Yes,
    No,
}

impl MessageDialogRequest {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        limits.validate_text(&self.title)?;
        limits.validate_text(&self.description)
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct MenuItemId {
    pub index: u32,
    pub generation: u32,
}

impl MenuItemId {
    pub const INVALID: Self = Self {
        index: 0,
        generation: 0,
    };

    pub const fn new(index: u32, generation: u32) -> Self {
        Self { index, generation }
    }

    pub const fn is_valid(self) -> bool {
        self.generation != 0
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MenuNode {
    Command {
        id: MenuItemId,
        label: String,
        enabled: bool,
        shortcut: Option<String>,
    },
    Check {
        id: MenuItemId,
        label: String,
        enabled: bool,
        checked: bool,
        shortcut: Option<String>,
    },
    Submenu {
        id: MenuItemId,
        label: String,
        enabled: bool,
        children: Vec<MenuNode>,
    },
    Separator {
        id: MenuItemId,
    },
}

impl MenuNode {
    pub const fn id(&self) -> MenuItemId {
        match self {
            Self::Command { id, .. }
            | Self::Check { id, .. }
            | Self::Submenu { id, .. }
            | Self::Separator { id } => *id,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MenuModel {
    pub revision: u64,
    pub roots: Vec<MenuNode>,
}

impl MenuModel {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        if !limits.is_valid() {
            return Err(SystemContractError::InvalidLimits);
        }
        let mut ids = BTreeSet::new();
        let mut count = 0_usize;
        let mut stack: Vec<(&MenuNode, usize)> =
            self.roots.iter().rev().map(|node| (node, 1)).collect();
        while let Some((node, depth)) = stack.pop() {
            if depth > limits.max_menu_depth {
                return Err(SystemContractError::MenuDepthExceeded);
            }
            count = count
                .checked_add(1)
                .ok_or(SystemContractError::MenuLimitExceeded)?;
            if count > limits.max_menu_items {
                return Err(SystemContractError::MenuLimitExceeded);
            }
            let id = node.id();
            if !id.is_valid() {
                return Err(SystemContractError::InvalidMenuIdentity);
            }
            if !ids.insert(id) {
                return Err(SystemContractError::DuplicateMenuIdentity);
            }
            match node {
                MenuNode::Command {
                    label, shortcut, ..
                }
                | MenuNode::Check {
                    label, shortcut, ..
                } => {
                    validate_label(label, limits)?;
                    if let Some(shortcut) = shortcut {
                        validate_label(shortcut, limits)?;
                    }
                }
                MenuNode::Submenu {
                    label, children, ..
                } => {
                    validate_label(label, limits)?;
                    stack.extend(children.iter().rev().map(|child| (child, depth + 1)));
                }
                MenuNode::Separator { .. } => {}
            }
        }
        Ok(())
    }
}

fn validate_label(value: &str, limits: SystemLimits) -> Result<(), SystemContractError> {
    if value.is_empty() {
        return Err(SystemContractError::EmptyMenuLabel);
    }
    limits.validate_text(value)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DragDropPhase {
    Entered,
    Moved,
    Left,
    Dropped,
}

#[derive(Clone, Debug, PartialEq)]
pub struct DragDropEvent {
    pub sequence: u64,
    pub phase: DragDropPhase,
    pub x: f64,
    pub y: f64,
    pub paths: Vec<String>,
}

impl DragDropEvent {
    pub fn validate(&self, limits: SystemLimits) -> Result<(), SystemContractError> {
        if self.sequence == 0 || !self.x.is_finite() || !self.y.is_finite() {
            return Err(SystemContractError::InvalidPath);
        }
        limits.validate_paths(&self.paths)?;
        if matches!(self.phase, DragDropPhase::Entered | DragDropPhase::Dropped)
            && self.paths.is_empty()
        {
            return Err(SystemContractError::InvalidPath);
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum SystemEvent {
    MenuActivated { sequence: u64, item: MenuItemId },
    DragDrop(DragDropEvent),
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{string::ToString, vec};

    #[test]
    fn clipboard_images_require_exact_bounded_rgba_storage() {
        let limits = SystemLimits {
            max_image_pixels: 4,
            ..SystemLimits::default()
        };
        let image = ClipboardImage {
            width: 2,
            height: 2,
            pixels: vec![255; 16],
        };
        assert_eq!(image.validate(limits), Ok(()));
        let short = ClipboardImage {
            pixels: vec![0; 15],
            ..image
        };
        assert_eq!(
            short.validate(limits),
            Err(SystemContractError::InvalidImage)
        );
    }

    #[test]
    fn file_dialog_filters_are_bounded_and_platform_neutral() {
        let request = FileDialogRequest {
            kind: FileDialogKind::OpenFiles,
            title: "Open sources".to_string(),
            initial_directory: Some("/workspace".to_string()),
            initial_file_name: None,
            filters: vec![FileDialogFilter {
                name: "Volang".to_string(),
                extensions: vec!["vo".to_string()],
            }],
            can_create_directories: false,
        };
        assert_eq!(request.validate(SystemLimits::default()), Ok(()));
    }

    #[test]
    fn file_drag_requires_a_bounded_non_empty_path_set() {
        let request = FileDragRequest {
            paths: vec!["/tmp/demo.vo".to_string()],
            preview: None,
            mode: FileDragMode::Copy,
        };
        assert_eq!(request.validate(SystemLimits::default()), Ok(()));
        assert_eq!(
            FileDragRequest {
                paths: Vec::new(),
                ..request
            }
            .validate(SystemLimits::default()),
            Err(SystemContractError::InvalidDragRequest)
        );
    }

    #[test]
    fn menu_identity_depth_and_count_are_checked_without_recursion() {
        let leaf = MenuNode::Command {
            id: MenuItemId::new(2, 1),
            label: "Save".to_string(),
            enabled: true,
            shortcut: Some("CmdOrCtrl+S".to_string()),
        };
        let model = MenuModel {
            revision: 7,
            roots: vec![MenuNode::Submenu {
                id: MenuItemId::new(1, 1),
                label: "File".to_string(),
                enabled: true,
                children: vec![leaf],
            }],
        };
        assert_eq!(model.validate(SystemLimits::default()), Ok(()));
        assert_eq!(
            model.validate(SystemLimits {
                max_menu_depth: 1,
                ..SystemLimits::default()
            }),
            Err(SystemContractError::MenuDepthExceeded)
        );
    }

    #[test]
    fn dropped_files_need_a_monotonic_identity_and_payload() {
        let event = DragDropEvent {
            sequence: 1,
            phase: DragDropPhase::Dropped,
            x: 10.0,
            y: 20.0,
            paths: vec!["/tmp/demo.vo".to_string()],
        };
        assert_eq!(event.validate(SystemLimits::default()), Ok(()));
    }
}
