use alloc::string::String;
use alloc::vec::Vec;

use crate::{
    ClipboardContent, ClipboardFormat, ClipboardImage, DragDropEvent, DragDropPhase,
    FileDialogFilter, FileDialogKind, FileDialogRequest, FileDialogResult, FileDragMode,
    FileDragRequest, HostInvocation, MenuItemId, MenuModel, MenuNode, MessageDialogButtons,
    MessageDialogLevel, MessageDialogRequest, MessageDialogResult, SystemContractError,
    SystemEvent, SystemLimits,
};

const MAGIC: &[u8; 4] = b"VUS1";
const REQUEST_FRAME: u8 = 1;
const RESPONSE_FRAME: u8 = 2;

#[derive(Clone, Debug, PartialEq)]
pub enum SystemRequest {
    ReadClipboard(ClipboardFormat),
    WriteClipboard(ClipboardContent),
    ShowFileDialog(FileDialogRequest),
    ShowMessageDialog(MessageDialogRequest),
    InstallMenu(MenuModel),
    WaitEvent,
    BeginFileDrag(FileDragRequest),
    InvokeHost(HostInvocation),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SystemRequestEnvelope {
    pub request_id: u64,
    pub request: SystemRequest,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SystemFailureKind {
    Denied,
    Unsupported,
    Cancelled,
    Failed,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SystemFailure {
    pub kind: SystemFailureKind,
    pub message: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum SystemResponse {
    Complete,
    Clipboard(Option<ClipboardContent>),
    FileDialog(FileDialogResult),
    MessageDialog(MessageDialogResult),
    MenuInstalled { revision: u64 },
    Event(SystemEvent),
    HostPayload(Vec<u8>),
    Failure(SystemFailure),
}

#[derive(Clone, Debug, PartialEq)]
pub struct SystemResponseEnvelope {
    pub request_id: u64,
    pub response: SystemResponse,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SystemCodecError {
    InvalidMagic,
    InvalidFrameKind,
    InvalidTag,
    InvalidIdentity,
    Truncated,
    TrailingBytes,
    IntegerOverflow,
    InvalidUtf8,
    Contract(SystemContractError),
}

pub fn encode_system_request(
    envelope: &SystemRequestEnvelope,
    limits: SystemLimits,
) -> Result<Vec<u8>, SystemCodecError> {
    validate_request(envelope, limits)?;
    let mut writer = Writer::new(
        REQUEST_FRAME,
        request_tag(&envelope.request),
        envelope.request_id,
    );
    match &envelope.request {
        SystemRequest::ReadClipboard(format) => writer.u8(clipboard_format_tag(*format)),
        SystemRequest::WriteClipboard(content) => encode_clipboard(&mut writer, content)?,
        SystemRequest::ShowFileDialog(request) => encode_file_dialog(&mut writer, request)?,
        SystemRequest::ShowMessageDialog(request) => encode_message_dialog(&mut writer, request)?,
        SystemRequest::InstallMenu(model) => encode_menu(&mut writer, model)?,
        SystemRequest::WaitEvent => {}
        SystemRequest::BeginFileDrag(request) => encode_file_drag(&mut writer, request)?,
        SystemRequest::InvokeHost(request) => {
            writer.string(&request.service)?;
            writer.string(&request.operation)?;
            writer.bytes(&request.payload)?;
        }
    }
    Ok(writer.finish())
}

pub fn decode_system_request(
    bytes: &[u8],
    limits: SystemLimits,
) -> Result<SystemRequestEnvelope, SystemCodecError> {
    let (mut reader, tag, request_id) = Reader::frame(bytes, REQUEST_FRAME)?;
    let request = match tag {
        1 => SystemRequest::ReadClipboard(decode_clipboard_format(reader.u8()?)?),
        2 => SystemRequest::WriteClipboard(decode_clipboard(&mut reader, limits)?),
        3 => SystemRequest::ShowFileDialog(decode_file_dialog(&mut reader, limits)?),
        4 => SystemRequest::ShowMessageDialog(decode_message_dialog(&mut reader, limits)?),
        5 => SystemRequest::InstallMenu(decode_menu(&mut reader, limits)?),
        6 => SystemRequest::WaitEvent,
        7 => SystemRequest::BeginFileDrag(decode_file_drag(&mut reader, limits)?),
        8 => SystemRequest::InvokeHost(HostInvocation {
            service: reader.string(255)?,
            operation: reader.string(255)?,
            payload: reader.bytes(limits.max_text_bytes)?,
        }),
        _ => return Err(SystemCodecError::InvalidTag),
    };
    reader.finish()?;
    let envelope = SystemRequestEnvelope {
        request_id,
        request,
    };
    validate_request(&envelope, limits)?;
    Ok(envelope)
}

pub fn encode_system_response(
    envelope: &SystemResponseEnvelope,
    limits: SystemLimits,
) -> Result<Vec<u8>, SystemCodecError> {
    validate_response(envelope, limits)?;
    let mut writer = Writer::new(
        RESPONSE_FRAME,
        response_tag(&envelope.response),
        envelope.request_id,
    );
    match &envelope.response {
        SystemResponse::Complete => {}
        SystemResponse::Clipboard(content) => match content {
            Some(content) => {
                writer.u8(1);
                encode_clipboard(&mut writer, content)?;
            }
            None => writer.u8(0),
        },
        SystemResponse::FileDialog(result) => writer.strings(&result.paths)?,
        SystemResponse::MessageDialog(result) => writer.u8(message_result_tag(*result)),
        SystemResponse::MenuInstalled { revision } => writer.u64(*revision),
        SystemResponse::Event(event) => encode_event(&mut writer, event)?,
        SystemResponse::HostPayload(payload) => writer.bytes(payload)?,
        SystemResponse::Failure(failure) => {
            writer.u8(failure_kind_tag(failure.kind));
            writer.string(&failure.message)?;
        }
    }
    Ok(writer.finish())
}

pub fn decode_system_response(
    bytes: &[u8],
    limits: SystemLimits,
) -> Result<SystemResponseEnvelope, SystemCodecError> {
    let (mut reader, tag, request_id) = Reader::frame(bytes, RESPONSE_FRAME)?;
    let response = match tag {
        1 => SystemResponse::Complete,
        2 => {
            let content = match reader.u8()? {
                0 => None,
                1 => Some(decode_clipboard(&mut reader, limits)?),
                _ => return Err(SystemCodecError::InvalidTag),
            };
            SystemResponse::Clipboard(content)
        }
        3 => SystemResponse::FileDialog(FileDialogResult {
            paths: reader.strings(limits.max_paths, limits.max_path_bytes)?,
        }),
        4 => SystemResponse::MessageDialog(decode_message_result(reader.u8()?)?),
        5 => SystemResponse::MenuInstalled {
            revision: reader.u64()?,
        },
        6 => SystemResponse::Event(decode_event(&mut reader, limits)?),
        7 => SystemResponse::HostPayload(reader.bytes(limits.max_text_bytes)?),
        0x80 => SystemResponse::Failure(SystemFailure {
            kind: decode_failure_kind(reader.u8()?)?,
            message: reader.string(limits.max_text_bytes)?,
        }),
        _ => return Err(SystemCodecError::InvalidTag),
    };
    reader.finish()?;
    let envelope = SystemResponseEnvelope {
        request_id,
        response,
    };
    validate_response(&envelope, limits)?;
    Ok(envelope)
}

fn validate_request(
    envelope: &SystemRequestEnvelope,
    limits: SystemLimits,
) -> Result<(), SystemCodecError> {
    if envelope.request_id == 0 {
        return Err(SystemCodecError::InvalidIdentity);
    }
    match &envelope.request {
        SystemRequest::ReadClipboard(_) | SystemRequest::WaitEvent => Ok(()),
        SystemRequest::WriteClipboard(content) => content.validate(limits),
        SystemRequest::ShowFileDialog(request) => request.validate(limits),
        SystemRequest::ShowMessageDialog(request) => request.validate(limits),
        SystemRequest::InstallMenu(model) => model.validate(limits),
        SystemRequest::BeginFileDrag(request) => request.validate(limits),
        SystemRequest::InvokeHost(request) => request.validate(limits),
    }
    .map_err(SystemCodecError::Contract)
}

fn validate_response(
    envelope: &SystemResponseEnvelope,
    limits: SystemLimits,
) -> Result<(), SystemCodecError> {
    if envelope.request_id == 0 {
        return Err(SystemCodecError::InvalidIdentity);
    }
    match &envelope.response {
        SystemResponse::Complete | SystemResponse::MessageDialog(_) => Ok(()),
        SystemResponse::HostPayload(payload) => {
            if payload.len() <= limits.max_text_bytes {
                Ok(())
            } else {
                Err(SystemContractError::PayloadLimitExceeded)
            }
        }
        SystemResponse::Clipboard(content) => content
            .as_ref()
            .map_or(Ok(()), |content| content.validate(limits)),
        SystemResponse::FileDialog(result) => result.validate(limits),
        SystemResponse::MenuInstalled { revision } => {
            if *revision == 0 {
                Err(SystemContractError::InvalidMenuIdentity)
            } else {
                Ok(())
            }
        }
        SystemResponse::Event(SystemEvent::MenuActivated { sequence, item }) => {
            if *sequence == 0 || !item.is_valid() {
                Err(SystemContractError::InvalidMenuIdentity)
            } else {
                Ok(())
            }
        }
        SystemResponse::Event(SystemEvent::DragDrop(event)) => event.validate(limits),
        SystemResponse::Failure(failure) => limits.validate_text(&failure.message),
    }
    .map_err(SystemCodecError::Contract)
}

fn request_tag(request: &SystemRequest) -> u8 {
    match request {
        SystemRequest::ReadClipboard(_) => 1,
        SystemRequest::WriteClipboard(_) => 2,
        SystemRequest::ShowFileDialog(_) => 3,
        SystemRequest::ShowMessageDialog(_) => 4,
        SystemRequest::InstallMenu(_) => 5,
        SystemRequest::WaitEvent => 6,
        SystemRequest::BeginFileDrag(_) => 7,
        SystemRequest::InvokeHost(_) => 8,
    }
}

fn response_tag(response: &SystemResponse) -> u8 {
    match response {
        SystemResponse::Complete => 1,
        SystemResponse::Clipboard(_) => 2,
        SystemResponse::FileDialog(_) => 3,
        SystemResponse::MessageDialog(_) => 4,
        SystemResponse::MenuInstalled { .. } => 5,
        SystemResponse::Event(_) => 6,
        SystemResponse::HostPayload(_) => 7,
        SystemResponse::Failure(_) => 0x80,
    }
}

fn clipboard_format_tag(format: ClipboardFormat) -> u8 {
    match format {
        ClipboardFormat::Text => 1,
        ClipboardFormat::Html => 2,
        ClipboardFormat::Rgba8 => 3,
    }
}

fn decode_clipboard_format(tag: u8) -> Result<ClipboardFormat, SystemCodecError> {
    match tag {
        1 => Ok(ClipboardFormat::Text),
        2 => Ok(ClipboardFormat::Html),
        3 => Ok(ClipboardFormat::Rgba8),
        _ => Err(SystemCodecError::InvalidTag),
    }
}

fn encode_clipboard(
    writer: &mut Writer,
    content: &ClipboardContent,
) -> Result<(), SystemCodecError> {
    match content {
        ClipboardContent::Text(text) => {
            writer.u8(1);
            writer.string(text)?;
        }
        ClipboardContent::Html { html, plain_text } => {
            writer.u8(2);
            writer.string(html)?;
            writer.string(plain_text)?;
        }
        ClipboardContent::Rgba8(image) => {
            writer.u8(3);
            writer.u32(image.width);
            writer.u32(image.height);
            writer.bytes(&image.pixels)?;
        }
    }
    Ok(())
}

fn decode_clipboard(
    reader: &mut Reader<'_>,
    limits: SystemLimits,
) -> Result<ClipboardContent, SystemCodecError> {
    let content = match reader.u8()? {
        1 => ClipboardContent::Text(reader.string(limits.max_text_bytes)?),
        2 => ClipboardContent::Html {
            html: reader.string(limits.max_text_bytes)?,
            plain_text: reader.string(limits.max_text_bytes)?,
        },
        3 => {
            let width = reader.u32()?;
            let height = reader.u32()?;
            let max_bytes = limits
                .max_image_pixels
                .checked_mul(4)
                .ok_or(SystemCodecError::IntegerOverflow)?;
            ClipboardContent::Rgba8(ClipboardImage {
                width,
                height,
                pixels: reader.bytes(max_bytes)?,
            })
        }
        _ => return Err(SystemCodecError::InvalidTag),
    };
    content
        .validate(limits)
        .map_err(SystemCodecError::Contract)?;
    Ok(content)
}

fn encode_file_dialog(
    writer: &mut Writer,
    request: &FileDialogRequest,
) -> Result<(), SystemCodecError> {
    writer.u8(file_dialog_kind_tag(request.kind));
    writer.string(&request.title)?;
    writer.optional_string(request.initial_directory.as_deref())?;
    writer.optional_string(request.initial_file_name.as_deref())?;
    writer.u8(u8::from(request.can_create_directories));
    writer.len(request.filters.len())?;
    for filter in &request.filters {
        writer.string(&filter.name)?;
        writer.strings(&filter.extensions)?;
    }
    Ok(())
}

fn decode_file_dialog(
    reader: &mut Reader<'_>,
    limits: SystemLimits,
) -> Result<FileDialogRequest, SystemCodecError> {
    let kind = decode_file_dialog_kind(reader.u8()?)?;
    let title = reader.string(limits.max_text_bytes)?;
    let initial_directory = reader.optional_string(limits.max_path_bytes)?;
    let initial_file_name = reader.optional_string(limits.max_path_bytes)?;
    let can_create_directories = reader.bool()?;
    let filter_count = reader.len(limits.max_filters)?;
    let mut filters = Vec::with_capacity(filter_count);
    for _ in 0..filter_count {
        filters.push(FileDialogFilter {
            name: reader.string(limits.max_text_bytes)?,
            extensions: reader.strings(limits.max_extensions_per_filter, limits.max_path_bytes)?,
        });
    }
    let request = FileDialogRequest {
        kind,
        title,
        initial_directory,
        initial_file_name,
        filters,
        can_create_directories,
    };
    request
        .validate(limits)
        .map_err(SystemCodecError::Contract)?;
    Ok(request)
}

fn file_dialog_kind_tag(kind: FileDialogKind) -> u8 {
    match kind {
        FileDialogKind::OpenFile => 1,
        FileDialogKind::OpenFiles => 2,
        FileDialogKind::OpenFolder => 3,
        FileDialogKind::OpenFolders => 4,
        FileDialogKind::SaveFile => 5,
    }
}

fn decode_file_dialog_kind(tag: u8) -> Result<FileDialogKind, SystemCodecError> {
    match tag {
        1 => Ok(FileDialogKind::OpenFile),
        2 => Ok(FileDialogKind::OpenFiles),
        3 => Ok(FileDialogKind::OpenFolder),
        4 => Ok(FileDialogKind::OpenFolders),
        5 => Ok(FileDialogKind::SaveFile),
        _ => Err(SystemCodecError::InvalidTag),
    }
}

fn encode_file_drag(
    writer: &mut Writer,
    request: &FileDragRequest,
) -> Result<(), SystemCodecError> {
    writer.u8(match request.mode {
        FileDragMode::Copy => 1,
        FileDragMode::Move => 2,
    });
    writer.strings(&request.paths)?;
    writer.optional_string(request.preview.as_deref())
}

fn decode_file_drag(
    reader: &mut Reader<'_>,
    limits: SystemLimits,
) -> Result<FileDragRequest, SystemCodecError> {
    let mode = match reader.u8()? {
        1 => FileDragMode::Copy,
        2 => FileDragMode::Move,
        _ => return Err(SystemCodecError::InvalidTag),
    };
    let request = FileDragRequest {
        paths: reader.strings(limits.max_paths, limits.max_path_bytes)?,
        preview: reader.optional_string(limits.max_path_bytes)?,
        mode,
    };
    request
        .validate(limits)
        .map_err(SystemCodecError::Contract)?;
    Ok(request)
}

fn encode_message_dialog(
    writer: &mut Writer,
    request: &MessageDialogRequest,
) -> Result<(), SystemCodecError> {
    writer.u8(match request.level {
        MessageDialogLevel::Info => 1,
        MessageDialogLevel::Warning => 2,
        MessageDialogLevel::Error => 3,
    });
    writer.u8(match request.buttons {
        MessageDialogButtons::Ok => 1,
        MessageDialogButtons::OkCancel => 2,
        MessageDialogButtons::YesNo => 3,
        MessageDialogButtons::YesNoCancel => 4,
    });
    writer.string(&request.title)?;
    writer.string(&request.description)
}

fn decode_message_dialog(
    reader: &mut Reader<'_>,
    limits: SystemLimits,
) -> Result<MessageDialogRequest, SystemCodecError> {
    let level = match reader.u8()? {
        1 => MessageDialogLevel::Info,
        2 => MessageDialogLevel::Warning,
        3 => MessageDialogLevel::Error,
        _ => return Err(SystemCodecError::InvalidTag),
    };
    let buttons = match reader.u8()? {
        1 => MessageDialogButtons::Ok,
        2 => MessageDialogButtons::OkCancel,
        3 => MessageDialogButtons::YesNo,
        4 => MessageDialogButtons::YesNoCancel,
        _ => return Err(SystemCodecError::InvalidTag),
    };
    let request = MessageDialogRequest {
        level,
        buttons,
        title: reader.string(limits.max_text_bytes)?,
        description: reader.string(limits.max_text_bytes)?,
    };
    request
        .validate(limits)
        .map_err(SystemCodecError::Contract)?;
    Ok(request)
}

fn message_result_tag(result: MessageDialogResult) -> u8 {
    match result {
        MessageDialogResult::Ok => 1,
        MessageDialogResult::Cancel => 2,
        MessageDialogResult::Yes => 3,
        MessageDialogResult::No => 4,
    }
}

fn decode_message_result(tag: u8) -> Result<MessageDialogResult, SystemCodecError> {
    match tag {
        1 => Ok(MessageDialogResult::Ok),
        2 => Ok(MessageDialogResult::Cancel),
        3 => Ok(MessageDialogResult::Yes),
        4 => Ok(MessageDialogResult::No),
        _ => Err(SystemCodecError::InvalidTag),
    }
}

fn encode_menu(writer: &mut Writer, model: &MenuModel) -> Result<(), SystemCodecError> {
    writer.u64(model.revision);
    writer.len(model.roots.len())?;
    for root in &model.roots {
        encode_menu_node(writer, root)?;
    }
    Ok(())
}

fn encode_menu_node(writer: &mut Writer, node: &MenuNode) -> Result<(), SystemCodecError> {
    match node {
        MenuNode::Command {
            id,
            label,
            enabled,
            shortcut,
        } => {
            writer.u8(1);
            writer.menu_id(*id);
            writer.string(label)?;
            writer.u8(u8::from(*enabled));
            writer.optional_string(shortcut.as_deref())?;
        }
        MenuNode::Check {
            id,
            label,
            enabled,
            checked,
            shortcut,
        } => {
            writer.u8(2);
            writer.menu_id(*id);
            writer.string(label)?;
            writer.u8(u8::from(*enabled));
            writer.u8(u8::from(*checked));
            writer.optional_string(shortcut.as_deref())?;
        }
        MenuNode::Submenu {
            id,
            label,
            enabled,
            children,
        } => {
            writer.u8(3);
            writer.menu_id(*id);
            writer.string(label)?;
            writer.u8(u8::from(*enabled));
            writer.len(children.len())?;
            for child in children {
                encode_menu_node(writer, child)?;
            }
        }
        MenuNode::Separator { id } => {
            writer.u8(4);
            writer.menu_id(*id);
        }
    }
    Ok(())
}

fn decode_menu(
    reader: &mut Reader<'_>,
    limits: SystemLimits,
) -> Result<MenuModel, SystemCodecError> {
    let revision = reader.u64()?;
    let roots = reader.len(limits.max_menu_items)?;
    let mut count = 0_usize;
    let mut nodes = Vec::with_capacity(roots);
    for _ in 0..roots {
        nodes.push(decode_menu_node(reader, limits, 1, &mut count)?);
    }
    let model = MenuModel {
        revision,
        roots: nodes,
    };
    model.validate(limits).map_err(SystemCodecError::Contract)?;
    Ok(model)
}

fn decode_menu_node(
    reader: &mut Reader<'_>,
    limits: SystemLimits,
    depth: usize,
    count: &mut usize,
) -> Result<MenuNode, SystemCodecError> {
    if depth > limits.max_menu_depth {
        return Err(SystemCodecError::Contract(
            SystemContractError::MenuDepthExceeded,
        ));
    }
    *count = count
        .checked_add(1)
        .ok_or(SystemCodecError::IntegerOverflow)?;
    if *count > limits.max_menu_items {
        return Err(SystemCodecError::Contract(
            SystemContractError::MenuLimitExceeded,
        ));
    }
    let tag = reader.u8()?;
    let id = reader.menu_id()?;
    match tag {
        1 => Ok(MenuNode::Command {
            id,
            label: reader.string(limits.max_text_bytes)?,
            enabled: reader.bool()?,
            shortcut: reader.optional_string(limits.max_text_bytes)?,
        }),
        2 => Ok(MenuNode::Check {
            id,
            label: reader.string(limits.max_text_bytes)?,
            enabled: reader.bool()?,
            checked: reader.bool()?,
            shortcut: reader.optional_string(limits.max_text_bytes)?,
        }),
        3 => {
            let label = reader.string(limits.max_text_bytes)?;
            let enabled = reader.bool()?;
            let child_count = reader.len(limits.max_menu_items.saturating_sub(*count))?;
            let mut children = Vec::with_capacity(child_count);
            for _ in 0..child_count {
                children.push(decode_menu_node(reader, limits, depth + 1, count)?);
            }
            Ok(MenuNode::Submenu {
                id,
                label,
                enabled,
                children,
            })
        }
        4 => Ok(MenuNode::Separator { id }),
        _ => Err(SystemCodecError::InvalidTag),
    }
}

fn encode_event(writer: &mut Writer, event: &SystemEvent) -> Result<(), SystemCodecError> {
    match event {
        SystemEvent::MenuActivated { sequence, item } => {
            writer.u8(1);
            writer.u64(*sequence);
            writer.menu_id(*item);
        }
        SystemEvent::DragDrop(event) => {
            writer.u8(2);
            writer.u64(event.sequence);
            writer.u8(match event.phase {
                DragDropPhase::Entered => 1,
                DragDropPhase::Moved => 2,
                DragDropPhase::Left => 3,
                DragDropPhase::Dropped => 4,
            });
            writer.u64(event.x.to_bits());
            writer.u64(event.y.to_bits());
            writer.strings(&event.paths)?;
        }
    }
    Ok(())
}

fn decode_event(
    reader: &mut Reader<'_>,
    limits: SystemLimits,
) -> Result<SystemEvent, SystemCodecError> {
    match reader.u8()? {
        1 => Ok(SystemEvent::MenuActivated {
            sequence: reader.u64()?,
            item: reader.menu_id()?,
        }),
        2 => {
            let sequence = reader.u64()?;
            let phase = match reader.u8()? {
                1 => DragDropPhase::Entered,
                2 => DragDropPhase::Moved,
                3 => DragDropPhase::Left,
                4 => DragDropPhase::Dropped,
                _ => return Err(SystemCodecError::InvalidTag),
            };
            Ok(SystemEvent::DragDrop(DragDropEvent {
                sequence,
                phase,
                x: f64::from_bits(reader.u64()?),
                y: f64::from_bits(reader.u64()?),
                paths: reader.strings(limits.max_paths, limits.max_path_bytes)?,
            }))
        }
        _ => Err(SystemCodecError::InvalidTag),
    }
}

fn failure_kind_tag(kind: SystemFailureKind) -> u8 {
    match kind {
        SystemFailureKind::Denied => 1,
        SystemFailureKind::Unsupported => 2,
        SystemFailureKind::Cancelled => 3,
        SystemFailureKind::Failed => 4,
    }
}

fn decode_failure_kind(tag: u8) -> Result<SystemFailureKind, SystemCodecError> {
    match tag {
        1 => Ok(SystemFailureKind::Denied),
        2 => Ok(SystemFailureKind::Unsupported),
        3 => Ok(SystemFailureKind::Cancelled),
        4 => Ok(SystemFailureKind::Failed),
        _ => Err(SystemCodecError::InvalidTag),
    }
}

struct Writer {
    bytes: Vec<u8>,
}

impl Writer {
    fn new(frame_kind: u8, tag: u8, request_id: u64) -> Self {
        let mut bytes = Vec::with_capacity(32);
        bytes.extend_from_slice(MAGIC);
        bytes.push(frame_kind);
        bytes.push(tag);
        bytes.extend_from_slice(&0_u16.to_le_bytes());
        bytes.extend_from_slice(&request_id.to_le_bytes());
        Self { bytes }
    }

    fn finish(self) -> Vec<u8> {
        self.bytes
    }

    fn u8(&mut self, value: u8) {
        self.bytes.push(value);
    }

    fn u32(&mut self, value: u32) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn u64(&mut self, value: u64) {
        self.bytes.extend_from_slice(&value.to_le_bytes());
    }

    fn len(&mut self, value: usize) -> Result<(), SystemCodecError> {
        self.u32(u32::try_from(value).map_err(|_| SystemCodecError::IntegerOverflow)?);
        Ok(())
    }

    fn bytes(&mut self, value: &[u8]) -> Result<(), SystemCodecError> {
        self.len(value.len())?;
        self.bytes.extend_from_slice(value);
        Ok(())
    }

    fn string(&mut self, value: &str) -> Result<(), SystemCodecError> {
        self.bytes(value.as_bytes())
    }

    fn optional_string(&mut self, value: Option<&str>) -> Result<(), SystemCodecError> {
        match value {
            Some(value) => {
                self.u8(1);
                self.string(value)
            }
            None => {
                self.u8(0);
                Ok(())
            }
        }
    }

    fn strings(&mut self, values: &[String]) -> Result<(), SystemCodecError> {
        self.len(values.len())?;
        for value in values {
            self.string(value)?;
        }
        Ok(())
    }

    fn menu_id(&mut self, id: MenuItemId) {
        self.u32(id.index);
        self.u32(id.generation);
    }
}

struct Reader<'a> {
    bytes: &'a [u8],
    position: usize,
}

impl<'a> Reader<'a> {
    fn frame(bytes: &'a [u8], expected_kind: u8) -> Result<(Self, u8, u64), SystemCodecError> {
        let mut reader = Self { bytes, position: 0 };
        if reader.take(4)? != MAGIC {
            return Err(SystemCodecError::InvalidMagic);
        }
        if reader.u8()? != expected_kind {
            return Err(SystemCodecError::InvalidFrameKind);
        }
        let tag = reader.u8()?;
        if reader.u16()? != 0 {
            return Err(SystemCodecError::InvalidTag);
        }
        let request_id = reader.u64()?;
        if request_id == 0 {
            return Err(SystemCodecError::InvalidIdentity);
        }
        Ok((reader, tag, request_id))
    }

    fn finish(self) -> Result<(), SystemCodecError> {
        if self.position == self.bytes.len() {
            Ok(())
        } else {
            Err(SystemCodecError::TrailingBytes)
        }
    }

    fn take(&mut self, count: usize) -> Result<&'a [u8], SystemCodecError> {
        let end = self
            .position
            .checked_add(count)
            .ok_or(SystemCodecError::IntegerOverflow)?;
        let value = self
            .bytes
            .get(self.position..end)
            .ok_or(SystemCodecError::Truncated)?;
        self.position = end;
        Ok(value)
    }

    fn u8(&mut self) -> Result<u8, SystemCodecError> {
        Ok(self.take(1)?[0])
    }

    fn bool(&mut self) -> Result<bool, SystemCodecError> {
        match self.u8()? {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(SystemCodecError::InvalidTag),
        }
    }

    fn u16(&mut self) -> Result<u16, SystemCodecError> {
        let mut bytes = [0_u8; 2];
        bytes.copy_from_slice(self.take(2)?);
        Ok(u16::from_le_bytes(bytes))
    }

    fn u32(&mut self) -> Result<u32, SystemCodecError> {
        let mut bytes = [0_u8; 4];
        bytes.copy_from_slice(self.take(4)?);
        Ok(u32::from_le_bytes(bytes))
    }

    fn u64(&mut self) -> Result<u64, SystemCodecError> {
        let mut bytes = [0_u8; 8];
        bytes.copy_from_slice(self.take(8)?);
        Ok(u64::from_le_bytes(bytes))
    }

    fn len(&mut self, max: usize) -> Result<usize, SystemCodecError> {
        let value = usize::try_from(self.u32()?).map_err(|_| SystemCodecError::IntegerOverflow)?;
        if value > max {
            return Err(SystemCodecError::IntegerOverflow);
        }
        Ok(value)
    }

    fn bytes(&mut self, max: usize) -> Result<Vec<u8>, SystemCodecError> {
        let len = self.len(max)?;
        Ok(self.take(len)?.to_vec())
    }

    fn string(&mut self, max: usize) -> Result<String, SystemCodecError> {
        String::from_utf8(self.bytes(max)?).map_err(|_| SystemCodecError::InvalidUtf8)
    }

    fn optional_string(&mut self, max: usize) -> Result<Option<String>, SystemCodecError> {
        match self.u8()? {
            0 => Ok(None),
            1 => Ok(Some(self.string(max)?)),
            _ => Err(SystemCodecError::InvalidTag),
        }
    }

    fn strings(
        &mut self,
        max_count: usize,
        max_bytes: usize,
    ) -> Result<Vec<String>, SystemCodecError> {
        let count = self.len(max_count)?;
        let mut values = Vec::with_capacity(count);
        for _ in 0..count {
            values.push(self.string(max_bytes)?);
        }
        Ok(values)
    }

    fn menu_id(&mut self) -> Result<MenuItemId, SystemCodecError> {
        Ok(MenuItemId::new(self.u32()?, self.u32()?))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::{string::ToString, vec};

    #[test]
    fn request_round_trip_covers_nested_menu() {
        let request = SystemRequestEnvelope {
            request_id: 7,
            request: SystemRequest::InstallMenu(MenuModel {
                revision: 3,
                roots: vec![MenuNode::Submenu {
                    id: MenuItemId::new(1, 1),
                    label: "File".to_string(),
                    enabled: true,
                    children: vec![MenuNode::Command {
                        id: MenuItemId::new(2, 1),
                        label: "Open".to_string(),
                        enabled: true,
                        shortcut: Some("CmdOrCtrl+O".to_string()),
                    }],
                }],
            }),
        };
        let limits = SystemLimits::default();
        let encoded = encode_system_request(&request, limits).unwrap();
        assert_eq!(decode_system_request(&encoded, limits), Ok(request));
    }

    #[test]
    fn response_round_trip_covers_drag_drop() {
        let response = SystemResponseEnvelope {
            request_id: 9,
            response: SystemResponse::Event(SystemEvent::DragDrop(DragDropEvent {
                sequence: 4,
                phase: DragDropPhase::Dropped,
                x: 12.5,
                y: 20.25,
                paths: vec!["/tmp/main.vo".to_string()],
            })),
        };
        let limits = SystemLimits::default();
        let encoded = encode_system_response(&response, limits).unwrap();
        assert_eq!(decode_system_response(&encoded, limits), Ok(response));
    }

    #[test]
    fn host_invocation_round_trips_opaque_bounded_payloads() {
        let limits = SystemLimits::default();
        let request = SystemRequestEnvelope {
            request_id: 12,
            request: SystemRequest::InvokeHost(HostInvocation {
                service: "volang.studio.host.v1".to_string(),
                operation: "projects.list".to_string(),
                payload: vec![0, 1, 2, 255],
            }),
        };
        let encoded = encode_system_request(&request, limits).unwrap();
        assert_eq!(decode_system_request(&encoded, limits), Ok(request));
        let response = SystemResponseEnvelope {
            request_id: 12,
            response: SystemResponse::HostPayload(vec![3, 4, 5]),
        };
        let encoded = encode_system_response(&response, limits).unwrap();
        assert_eq!(decode_system_response(&encoded, limits), Ok(response));
    }

    #[test]
    fn request_round_trip_covers_native_file_drag() {
        let request = SystemRequestEnvelope {
            request_id: 11,
            request: SystemRequest::BeginFileDrag(FileDragRequest {
                paths: vec!["/tmp/alpha.vo".to_string(), "/tmp/beta.vo".to_string()],
                preview: Some("/tmp/preview.png".to_string()),
                mode: FileDragMode::Move,
            }),
        };
        let limits = SystemLimits::default();
        let encoded = encode_system_request(&request, limits).unwrap();
        assert_eq!(decode_system_request(&encoded, limits), Ok(request));
    }

    #[test]
    fn decoder_rejects_trailing_and_over_limit_data() {
        let limits = SystemLimits {
            max_text_bytes: 3,
            ..SystemLimits::default()
        };
        let request = SystemRequestEnvelope {
            request_id: 1,
            request: SystemRequest::WriteClipboard(ClipboardContent::Text("four".to_string())),
        };
        assert_eq!(
            encode_system_request(&request, limits),
            Err(SystemCodecError::Contract(
                SystemContractError::TextLimitExceeded
            ))
        );

        let mut valid = encode_system_request(
            &SystemRequestEnvelope {
                request_id: 2,
                request: SystemRequest::WaitEvent,
            },
            limits,
        )
        .unwrap();
        valid.push(0);
        assert_eq!(
            decode_system_request(&valid, limits),
            Err(SystemCodecError::TrailingBytes)
        );
    }

    #[test]
    fn failure_messages_are_bounded_and_round_trip() {
        let response = SystemResponseEnvelope {
            request_id: 11,
            response: SystemResponse::Failure(SystemFailure {
                kind: SystemFailureKind::Unsupported,
                message: "native menu unavailable".to_string(),
            }),
        };
        let limits = SystemLimits::default();
        let bytes = encode_system_response(&response, limits).unwrap();
        assert_eq!(decode_system_response(&bytes, limits), Ok(response));
    }
}
