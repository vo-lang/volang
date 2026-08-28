//! Native system-service adapter for Volang UI.

use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt;
use std::sync::{mpsc, Arc};

use vo_app_host_native::{NativeInputEvent, NativeInputKind};
use vo_ui_system::{
    decode_system_request, encode_system_response, ClipboardContent, ClipboardFormat,
    DragDropEvent, DragDropPhase, FileDialogRequest, FileDialogResult, FileDragRequest,
    HostInvocation, MenuItemId, MenuModel, MessageDialogRequest, MessageDialogResult,
    SystemCodecError, SystemContractError, SystemEvent, SystemFailure, SystemFailureKind,
    SystemLimits, SystemRequest, SystemResponse, SystemResponseEnvelope,
};

pub trait NativeSystemBackend {
    type Error;

    fn classify_error(&self, _error: &Self::Error) -> SystemFailureKind {
        SystemFailureKind::Failed
    }

    fn read_clipboard(
        &mut self,
        format: ClipboardFormat,
    ) -> Result<Option<ClipboardContent>, Self::Error>;

    fn write_clipboard(&mut self, content: &ClipboardContent) -> Result<(), Self::Error>;

    fn show_file_dialog(
        &mut self,
        request: &FileDialogRequest,
    ) -> Result<FileDialogResult, Self::Error>;

    fn show_message_dialog(
        &mut self,
        request: &MessageDialogRequest,
    ) -> Result<MessageDialogResult, Self::Error>;

    fn install_menu(&mut self, model: &MenuModel) -> Result<(), Self::Error>;

    fn poll_menu_activation(&mut self) -> Result<Option<MenuItemId>, Self::Error>;

    fn begin_file_drag(&mut self, request: &FileDragRequest) -> Result<(), Self::Error>;
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct NativeSystemHostConfig {
    pub limits: SystemLimits,
    pub max_pending_events: usize,
}

impl Default for NativeSystemHostConfig {
    fn default() -> Self {
        Self {
            limits: SystemLimits::default(),
            max_pending_events: 4_096,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NativeSystemHostError<E> {
    InvalidConfig,
    Contract(SystemContractError),
    Backend(E),
    EventQueueFull,
    EventSequenceExhausted,
    UnknownMenuItem(MenuItemId),
    HostInvocationQueueFull,
}

impl<E: fmt::Debug> fmt::Display for NativeSystemHostError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "native UI system host error: {self:?}")
    }
}

/// Main-thread coordinator for platform services. Guest goroutines suspend at
/// the App Runtime request boundary; this owner validates all data before an OS
/// call and publishes menu/drop completions through one bounded FIFO.
pub struct NativeSystemHost<B: NativeSystemBackend> {
    backend: B,
    config: NativeSystemHostConfig,
    installed_menu_revision: u64,
    menu_items: BTreeSet<MenuItemId>,
    events: VecDeque<SystemEvent>,
    next_event_sequence: u64,
    host_invocation: Option<HostInvocationHandler>,
    pending_host_invocations: BTreeMap<u64, PendingHostInvocation>,
}

pub type HostInvocationHandler =
    Arc<dyn Fn(&HostInvocation) -> Result<Vec<u8>, SystemFailure> + Send + Sync>;

struct PendingHostInvocation {
    request: HostInvocation,
    completion: mpsc::Receiver<Result<Vec<u8>, SystemFailure>>,
}

impl<B: NativeSystemBackend> NativeSystemHost<B> {
    pub fn new(
        backend: B,
        config: NativeSystemHostConfig,
    ) -> Result<Self, NativeSystemHostError<B::Error>> {
        if !config.limits.is_valid() || config.max_pending_events == 0 {
            return Err(NativeSystemHostError::InvalidConfig);
        }
        Ok(Self {
            backend,
            config,
            installed_menu_revision: 0,
            menu_items: BTreeSet::new(),
            events: VecDeque::with_capacity(config.max_pending_events),
            next_event_sequence: 1,
            host_invocation: None,
            pending_host_invocations: BTreeMap::new(),
        })
    }

    pub const fn installed_menu_revision(&self) -> u64 {
        self.installed_menu_revision
    }

    pub fn pending_event_count(&self) -> usize {
        self.events.len()
    }

    pub fn backend(&self) -> &B {
        &self.backend
    }

    pub fn backend_mut(&mut self) -> &mut B {
        &mut self.backend
    }

    pub fn set_host_invocation_handler(&mut self, handler: HostInvocationHandler) {
        self.host_invocation = Some(handler);
    }

    pub fn pending_host_invocation_count(&self) -> usize {
        self.pending_host_invocations.len()
    }

    /// Forgets replies owned by a replaced guest session. Worker threads may
    /// finish naturally, while their disconnected one-shot senders cannot
    /// publish into the new request identity space.
    pub fn reset_host_invocations(&mut self) {
        self.pending_host_invocations.clear();
    }

    pub fn read_clipboard(
        &mut self,
        format: ClipboardFormat,
    ) -> Result<Option<ClipboardContent>, NativeSystemHostError<B::Error>> {
        let content = self
            .backend
            .read_clipboard(format)
            .map_err(NativeSystemHostError::Backend)?;
        if let Some(content) = &content {
            content
                .validate(self.config.limits)
                .map_err(NativeSystemHostError::Contract)?;
        }
        Ok(content)
    }

    pub fn write_clipboard(
        &mut self,
        content: &ClipboardContent,
    ) -> Result<(), NativeSystemHostError<B::Error>> {
        content
            .validate(self.config.limits)
            .map_err(NativeSystemHostError::Contract)?;
        self.backend
            .write_clipboard(content)
            .map_err(NativeSystemHostError::Backend)
    }

    pub fn show_file_dialog(
        &mut self,
        request: &FileDialogRequest,
    ) -> Result<FileDialogResult, NativeSystemHostError<B::Error>> {
        request
            .validate(self.config.limits)
            .map_err(NativeSystemHostError::Contract)?;
        let result = self
            .backend
            .show_file_dialog(request)
            .map_err(NativeSystemHostError::Backend)?;
        result
            .validate(self.config.limits)
            .map_err(NativeSystemHostError::Contract)?;
        Ok(result)
    }

    pub fn show_message_dialog(
        &mut self,
        request: &MessageDialogRequest,
    ) -> Result<MessageDialogResult, NativeSystemHostError<B::Error>> {
        request
            .validate(self.config.limits)
            .map_err(NativeSystemHostError::Contract)?;
        self.backend
            .show_message_dialog(request)
            .map_err(NativeSystemHostError::Backend)
    }

    pub fn begin_file_drag(
        &mut self,
        request: &FileDragRequest,
    ) -> Result<(), NativeSystemHostError<B::Error>> {
        request
            .validate(self.config.limits)
            .map_err(NativeSystemHostError::Contract)?;
        self.backend
            .begin_file_drag(request)
            .map_err(NativeSystemHostError::Backend)
    }

    pub fn install_menu(
        &mut self,
        model: &MenuModel,
    ) -> Result<bool, NativeSystemHostError<B::Error>> {
        model
            .validate(self.config.limits)
            .map_err(NativeSystemHostError::Contract)?;
        if model.revision < self.installed_menu_revision {
            return Err(NativeSystemHostError::Contract(
                SystemContractError::InvalidMenuIdentity,
            ));
        }
        if model.revision == self.installed_menu_revision && self.installed_menu_revision != 0 {
            return Ok(false);
        }
        self.backend
            .install_menu(model)
            .map_err(NativeSystemHostError::Backend)?;
        self.menu_items.clear();
        collect_menu_ids(&model.roots, &mut self.menu_items);
        self.installed_menu_revision = model.revision;
        Ok(true)
    }

    pub fn pump_menu_events(
        &mut self,
        max: usize,
    ) -> Result<usize, NativeSystemHostError<B::Error>> {
        let mut queued = 0;
        while queued < max {
            let Some(item) = self
                .backend
                .poll_menu_activation()
                .map_err(NativeSystemHostError::Backend)?
            else {
                break;
            };
            if !self.menu_items.contains(&item) {
                return Err(NativeSystemHostError::UnknownMenuItem(item));
            }
            let sequence = self.take_event_sequence()?;
            self.push_event(SystemEvent::MenuActivated { sequence, item })?;
            queued += 1;
        }
        Ok(queued)
    }

    pub fn push_drag_drop(
        &mut self,
        phase: DragDropPhase,
        x: f64,
        y: f64,
        paths: Vec<String>,
    ) -> Result<u64, NativeSystemHostError<B::Error>> {
        let sequence = self.take_event_sequence()?;
        let event = DragDropEvent {
            sequence,
            phase,
            x,
            y,
            paths,
        };
        event
            .validate(self.config.limits)
            .map_err(NativeSystemHostError::Contract)?;
        self.push_event(SystemEvent::DragDrop(event))?;
        Ok(sequence)
    }

    pub fn route_native_input(
        &mut self,
        event: &NativeInputEvent,
    ) -> Result<bool, NativeSystemHostError<B::Error>> {
        let routed = match &event.kind {
            NativeInputKind::FileDragEntered {
                x_milli,
                y_milli,
                paths,
            } => Some((DragDropPhase::Entered, *x_milli, *y_milli, paths.clone())),
            NativeInputKind::FileDragMoved { x_milli, y_milli } => {
                Some((DragDropPhase::Moved, *x_milli, *y_milli, Vec::new()))
            }
            NativeInputKind::FileDragLeft => Some((DragDropPhase::Left, 0, 0, Vec::new())),
            NativeInputKind::FileDropped {
                x_milli,
                y_milli,
                paths,
            } => Some((DragDropPhase::Dropped, *x_milli, *y_milli, paths.clone())),
            _ => None,
        };
        let Some((phase, x_milli, y_milli, paths)) = routed else {
            return Ok(false);
        };
        self.push_drag_drop(
            phase,
            f64::from(x_milli) / 1_000.0,
            f64::from(y_milli) / 1_000.0,
            paths,
        )?;
        Ok(true)
    }

    pub fn drain_events(&mut self, max: usize) -> Vec<SystemEvent> {
        let count = max.min(self.events.len());
        self.events.drain(..count).collect()
    }

    /// Executes one already-decoded VUS1 request on the native UI thread.
    /// Event waits remain pending until a menu or drag/drop event reaches the
    /// shared FIFO; all other requests complete exactly once.
    pub fn execute_request(
        &mut self,
        request: &SystemRequest,
    ) -> Result<Option<SystemResponse>, NativeSystemHostError<B::Error>> {
        let response = match request {
            SystemRequest::ReadClipboard(format) => {
                SystemResponse::Clipboard(self.read_clipboard(*format)?)
            }
            SystemRequest::WriteClipboard(content) => {
                self.write_clipboard(content)?;
                SystemResponse::Complete
            }
            SystemRequest::ShowFileDialog(request) => {
                SystemResponse::FileDialog(self.show_file_dialog(request)?)
            }
            SystemRequest::ShowMessageDialog(request) => {
                SystemResponse::MessageDialog(self.show_message_dialog(request)?)
            }
            SystemRequest::InstallMenu(model) => {
                self.install_menu(model)?;
                SystemResponse::MenuInstalled {
                    revision: model.revision,
                }
            }
            SystemRequest::BeginFileDrag(request) => {
                self.begin_file_drag(request)?;
                SystemResponse::Complete
            }
            SystemRequest::WaitEvent => {
                return Ok(self.events.pop_front().map(SystemResponse::Event));
            }
            SystemRequest::InvokeHost(request) => {
                let Some(handler) = self.host_invocation.as_mut() else {
                    return Ok(Some(SystemResponse::Failure(SystemFailure {
                        kind: SystemFailureKind::Unsupported,
                        message: "application host invocation is unavailable".to_string(),
                    })));
                };
                match handler(request) {
                    Ok(payload) => SystemResponse::HostPayload(payload),
                    Err(failure) => SystemResponse::Failure(failure),
                }
            }
        };
        Ok(Some(response))
    }

    /// Decodes, validates, and executes one VUS1 request frame. Backend
    /// failures become typed guest-visible responses; malformed protocol data
    /// remains a host error. A pending event wait returns `None` until the
    /// native event FIFO has data.
    pub fn execute_request_frame(
        &mut self,
        frame: &[u8],
    ) -> Result<Option<Vec<u8>>, SystemCodecError>
    where
        B::Error: fmt::Debug,
    {
        let request = decode_system_request(frame, self.config.limits)?;
        let response = if let SystemRequest::InvokeHost(invocation) = &request.request {
            match self.poll_host_invocation(request.request_id, invocation) {
                Ok(Some(response)) => response,
                Ok(None) => return Ok(None),
                Err(error) => SystemResponse::Failure(SystemFailure {
                    kind: SystemFailureKind::Failed,
                    message: error.to_string(),
                }),
            }
        } else {
            match self.execute_request(&request.request) {
                Ok(Some(response)) => response,
                Ok(None) => return Ok(None),
                Err(error) => SystemResponse::Failure(SystemFailure {
                    kind: match &error {
                        NativeSystemHostError::Backend(error) => self.backend.classify_error(error),
                        _ => SystemFailureKind::Failed,
                    },
                    message: error.to_string(),
                }),
            }
        };
        encode_system_response(
            &SystemResponseEnvelope {
                request_id: request.request_id,
                response,
            },
            self.config.limits,
        )
        .map(Some)
    }

    fn poll_host_invocation(
        &mut self,
        request_id: u64,
        request: &HostInvocation,
    ) -> Result<Option<SystemResponse>, NativeSystemHostError<B::Error>> {
        if let Some(pending) = self.pending_host_invocations.get(&request_id) {
            if pending.request != *request {
                return Ok(Some(SystemResponse::Failure(SystemFailure {
                    kind: SystemFailureKind::Failed,
                    message: "application host request identity was reused with different data"
                        .to_string(),
                })));
            }
            return match pending.completion.try_recv() {
                Ok(result) => {
                    self.pending_host_invocations.remove(&request_id);
                    Ok(Some(match result {
                        Ok(payload) => SystemResponse::HostPayload(payload),
                        Err(failure) => SystemResponse::Failure(failure),
                    }))
                }
                Err(mpsc::TryRecvError::Empty) => Ok(None),
                Err(mpsc::TryRecvError::Disconnected) => {
                    self.pending_host_invocations.remove(&request_id);
                    Ok(Some(SystemResponse::Failure(SystemFailure {
                        kind: SystemFailureKind::Failed,
                        message: "application host worker stopped before replying".to_string(),
                    })))
                }
            };
        }

        let Some(handler) = self.host_invocation.clone() else {
            return Ok(Some(SystemResponse::Failure(SystemFailure {
                kind: SystemFailureKind::Unsupported,
                message: "application host invocation is unavailable".to_string(),
            })));
        };
        if self.pending_host_invocations.len() == self.config.max_pending_events {
            return Err(NativeSystemHostError::HostInvocationQueueFull);
        }
        let invocation = request.clone();
        let worker_request = invocation.clone();
        let (sender, completion) = mpsc::sync_channel(1);
        std::thread::Builder::new()
            .name(format!("vo-host-{request_id}"))
            .spawn(move || {
                let _ = sender.send(handler(&worker_request));
            })
            .map_err(|_| NativeSystemHostError::HostInvocationQueueFull)?;
        self.pending_host_invocations.insert(
            request_id,
            PendingHostInvocation {
                request: invocation,
                completion,
            },
        );
        Ok(None)
    }

    fn take_event_sequence(&mut self) -> Result<u64, NativeSystemHostError<B::Error>> {
        let current = self.next_event_sequence;
        self.next_event_sequence = current
            .checked_add(1)
            .ok_or(NativeSystemHostError::EventSequenceExhausted)?;
        Ok(current)
    }

    fn push_event(&mut self, event: SystemEvent) -> Result<(), NativeSystemHostError<B::Error>> {
        if self.events.len() == self.config.max_pending_events {
            return Err(NativeSystemHostError::EventQueueFull);
        }
        self.events.push_back(event);
        Ok(())
    }
}

fn collect_menu_ids(nodes: &[vo_ui_system::MenuNode], ids: &mut BTreeSet<MenuItemId>) {
    let mut stack: Vec<_> = nodes.iter().collect();
    while let Some(node) = stack.pop() {
        ids.insert(node.id());
        if let vo_ui_system::MenuNode::Submenu { children, .. } = node {
            stack.extend(children.iter());
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DesktopSystemError {
    Unsupported,
    Clipboard(String),
    Menu(String),
    Drag(String),
    InvalidMenuRoot,
    InvalidAccelerator(String),
}

impl fmt::Display for DesktopSystemError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "native desktop system backend error: {self:?}")
    }
}

impl std::error::Error for DesktopSystemError {}

/// Production cross-platform backend. Clipboard ownership remains serialized
/// in this value, and menu objects stay on the native UI thread.
#[derive(Default)]
pub struct DesktopSystemBackend {
    #[cfg(feature = "clipboard")]
    clipboard: Option<arboard::Clipboard>,
    #[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
    menu: Option<DesktopMenu>,
    file_drag_starter: Option<FileDragStarter>,
}

pub type FileDragStarter = Box<dyn FnMut(&FileDragRequest) -> Result<(), String>>;

impl DesktopSystemBackend {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_file_drag_starter(&mut self, starter: FileDragStarter) {
        self.file_drag_starter = Some(starter);
    }

    #[cfg(feature = "clipboard")]
    fn clipboard(&mut self) -> Result<&mut arboard::Clipboard, DesktopSystemError> {
        if self.clipboard.is_none() {
            self.clipboard = Some(
                arboard::Clipboard::new()
                    .map_err(|error| DesktopSystemError::Clipboard(error.to_string()))?,
            );
        }
        self.clipboard
            .as_mut()
            .ok_or(DesktopSystemError::Unsupported)
    }

    /// Attaches the latest menu to a live Win32 HWND.
    ///
    /// # Safety
    ///
    /// `hwnd` must be a valid window owned by the calling UI thread and must
    /// outlive the installed menu.
    #[cfg(all(feature = "menus", target_os = "windows"))]
    pub unsafe fn attach_menu_to_hwnd(&self, hwnd: isize) -> Result<(), DesktopSystemError> {
        let menu = self.menu.as_ref().ok_or(DesktopSystemError::Unsupported)?;
        unsafe { menu.menu.init_for_hwnd(hwnd) }
            .map_err(|error| DesktopSystemError::Menu(error.to_string()))
    }
}

impl NativeSystemBackend for DesktopSystemBackend {
    type Error = DesktopSystemError;

    fn classify_error(&self, error: &Self::Error) -> SystemFailureKind {
        match error {
            DesktopSystemError::Unsupported => SystemFailureKind::Unsupported,
            DesktopSystemError::Clipboard(_)
            | DesktopSystemError::Menu(_)
            | DesktopSystemError::Drag(_)
            | DesktopSystemError::InvalidMenuRoot
            | DesktopSystemError::InvalidAccelerator(_) => SystemFailureKind::Failed,
        }
    }

    fn read_clipboard(
        &mut self,
        format: ClipboardFormat,
    ) -> Result<Option<ClipboardContent>, Self::Error> {
        #[cfg(feature = "clipboard")]
        {
            let clipboard = self.clipboard()?;
            let result = match format {
                ClipboardFormat::Text => clipboard.get_text().map(ClipboardContent::Text),
                ClipboardFormat::Html => {
                    let plain_text = clipboard.get_text().unwrap_or_default();
                    clipboard
                        .get()
                        .html()
                        .map(|html| ClipboardContent::Html { html, plain_text })
                }
                ClipboardFormat::Rgba8 => clipboard.get_image().and_then(|image| {
                    let width = u32::try_from(image.width)
                        .map_err(|_| arboard::Error::ConversionFailure)?;
                    let height = u32::try_from(image.height)
                        .map_err(|_| arboard::Error::ConversionFailure)?;
                    Ok(ClipboardContent::Rgba8(vo_ui_system::ClipboardImage {
                        width,
                        height,
                        pixels: image.bytes.into_owned(),
                    }))
                }),
            };
            match result {
                Ok(content) => Ok(Some(content)),
                Err(arboard::Error::ContentNotAvailable) => Ok(None),
                Err(error) => Err(DesktopSystemError::Clipboard(error.to_string())),
            }
        }
        #[cfg(not(feature = "clipboard"))]
        {
            let _ = format;
            Err(DesktopSystemError::Unsupported)
        }
    }

    fn write_clipboard(&mut self, content: &ClipboardContent) -> Result<(), Self::Error> {
        #[cfg(feature = "clipboard")]
        {
            use std::borrow::Cow;

            let clipboard = self.clipboard()?;
            match content {
                ClipboardContent::Text(text) => clipboard
                    .set_text(text)
                    .map_err(|error| DesktopSystemError::Clipboard(error.to_string())),
                ClipboardContent::Html { html, plain_text } => clipboard
                    .set_html(html, Some(plain_text))
                    .map_err(|error| DesktopSystemError::Clipboard(error.to_string())),
                ClipboardContent::Rgba8(image) => clipboard
                    .set_image(arboard::ImageData {
                        width: image.width as usize,
                        height: image.height as usize,
                        bytes: Cow::Borrowed(&image.pixels),
                    })
                    .map_err(|error| DesktopSystemError::Clipboard(error.to_string())),
            }
        }
        #[cfg(not(feature = "clipboard"))]
        {
            let _ = content;
            Err(DesktopSystemError::Unsupported)
        }
    }

    fn show_file_dialog(
        &mut self,
        request: &FileDialogRequest,
    ) -> Result<FileDialogResult, Self::Error> {
        #[cfg(feature = "dialogs")]
        {
            use vo_ui_system::FileDialogKind;

            let mut dialog = rfd::FileDialog::new()
                .set_title(&request.title)
                .set_can_create_directories(request.can_create_directories);
            if let Some(directory) = &request.initial_directory {
                dialog = dialog.set_directory(directory);
            }
            if let Some(file_name) = &request.initial_file_name {
                dialog = dialog.set_file_name(file_name);
            }
            for filter in &request.filters {
                dialog = dialog.add_filter(&filter.name, &filter.extensions);
            }
            let paths = match request.kind {
                FileDialogKind::OpenFile => dialog.pick_file().into_iter().collect(),
                FileDialogKind::OpenFiles => dialog.pick_files().unwrap_or_default(),
                FileDialogKind::OpenFolder => dialog.pick_folder().into_iter().collect(),
                FileDialogKind::OpenFolders => dialog.pick_folders().unwrap_or_default(),
                FileDialogKind::SaveFile => dialog.save_file().into_iter().collect(),
            };
            Ok(FileDialogResult {
                paths: paths
                    .into_iter()
                    .map(|path| path.to_string_lossy().into_owned())
                    .collect(),
            })
        }
        #[cfg(not(feature = "dialogs"))]
        {
            let _ = request;
            Err(DesktopSystemError::Unsupported)
        }
    }

    fn show_message_dialog(
        &mut self,
        request: &MessageDialogRequest,
    ) -> Result<MessageDialogResult, Self::Error> {
        #[cfg(feature = "dialogs")]
        {
            use rfd::{
                MessageButtons, MessageDialog, MessageDialogResult as RfdResult, MessageLevel,
            };
            use vo_ui_system::{MessageDialogButtons, MessageDialogLevel};

            let level = match request.level {
                MessageDialogLevel::Info => MessageLevel::Info,
                MessageDialogLevel::Warning => MessageLevel::Warning,
                MessageDialogLevel::Error => MessageLevel::Error,
            };
            let buttons = match request.buttons {
                MessageDialogButtons::Ok => MessageButtons::Ok,
                MessageDialogButtons::OkCancel => MessageButtons::OkCancel,
                MessageDialogButtons::YesNo => MessageButtons::YesNo,
                MessageDialogButtons::YesNoCancel => MessageButtons::YesNoCancel,
            };
            let result = MessageDialog::new()
                .set_level(level)
                .set_title(&request.title)
                .set_description(&request.description)
                .set_buttons(buttons)
                .show();
            Ok(match result {
                RfdResult::Ok => MessageDialogResult::Ok,
                RfdResult::Cancel | RfdResult::Custom(_) => MessageDialogResult::Cancel,
                RfdResult::Yes => MessageDialogResult::Yes,
                RfdResult::No => MessageDialogResult::No,
            })
        }
        #[cfg(not(feature = "dialogs"))]
        {
            let _ = request;
            Err(DesktopSystemError::Unsupported)
        }
    }

    fn install_menu(&mut self, model: &MenuModel) -> Result<(), Self::Error> {
        #[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
        {
            let menu = DesktopMenu::build(model)?;
            #[cfg(target_os = "macos")]
            menu.menu.init_for_nsapp();
            self.menu = Some(menu);
            Ok(())
        }
        #[cfg(not(all(feature = "menus", any(target_os = "macos", target_os = "windows"))))]
        {
            let _ = model;
            Err(DesktopSystemError::Unsupported)
        }
    }

    fn poll_menu_activation(&mut self) -> Result<Option<MenuItemId>, Self::Error> {
        #[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
        {
            let Some(menu) = &self.menu else {
                return Ok(None);
            };
            while let Ok(event) = muda::MenuEvent::receiver().try_recv() {
                if let Some(item) = menu.ids.get(&event.id.0) {
                    return Ok(Some(*item));
                }
            }
            Ok(None)
        }
        #[cfg(not(all(feature = "menus", any(target_os = "macos", target_os = "windows"))))]
        {
            Ok(None)
        }
    }

    fn begin_file_drag(&mut self, request: &FileDragRequest) -> Result<(), Self::Error> {
        let starter = self
            .file_drag_starter
            .as_mut()
            .ok_or(DesktopSystemError::Unsupported)?;
        starter(request).map_err(DesktopSystemError::Drag)
    }
}

#[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
struct DesktopMenu {
    menu: muda::Menu,
    ids: BTreeMap<String, MenuItemId>,
}

#[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
impl DesktopMenu {
    fn build(model: &MenuModel) -> Result<Self, DesktopSystemError> {
        let menu = muda::Menu::new();
        let mut ids = BTreeMap::new();
        for node in &model.roots {
            let item = build_menu_item(node, &mut ids)?;
            #[cfg(target_os = "macos")]
            if !matches!(item, muda::MenuItemKind::Submenu(_)) {
                return Err(DesktopSystemError::InvalidMenuRoot);
            }
            append_root(&menu, &item)?;
        }
        Ok(Self { menu, ids })
    }
}

#[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
fn build_menu_item(
    node: &vo_ui_system::MenuNode,
    ids: &mut BTreeMap<String, MenuItemId>,
) -> Result<muda::MenuItemKind, DesktopSystemError> {
    use muda::{CheckMenuItem, MenuItem, MenuItemKind, PredefinedMenuItem, Submenu};
    use vo_ui_system::MenuNode;

    let id = node.id();
    let native_id = format!("vui:{}:{}", id.generation, id.index);
    ids.insert(native_id.clone(), id);
    match node {
        MenuNode::Command {
            label,
            enabled,
            shortcut,
            ..
        } => Ok(MenuItemKind::MenuItem(MenuItem::with_id(
            native_id,
            label,
            *enabled,
            parse_accelerator(shortcut.as_deref())?,
        ))),
        MenuNode::Check {
            label,
            enabled,
            checked,
            shortcut,
            ..
        } => Ok(MenuItemKind::Check(CheckMenuItem::with_id(
            native_id,
            label,
            *enabled,
            *checked,
            parse_accelerator(shortcut.as_deref())?,
        ))),
        MenuNode::Submenu {
            label,
            enabled,
            children,
            ..
        } => {
            let submenu = Submenu::with_id(native_id, label, *enabled);
            for child in children {
                let child = build_menu_item(child, ids)?;
                append_submenu(&submenu, &child)?;
            }
            Ok(MenuItemKind::Submenu(submenu))
        }
        MenuNode::Separator { .. } => Ok(MenuItemKind::Predefined(PredefinedMenuItem::separator())),
    }
}

#[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
fn parse_accelerator(
    value: Option<&str>,
) -> Result<Option<muda::accelerator::Accelerator>, DesktopSystemError> {
    value
        .map(|value| {
            value
                .parse()
                .map_err(|_| DesktopSystemError::InvalidAccelerator(value.to_owned()))
        })
        .transpose()
}

#[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
fn append_root(menu: &muda::Menu, item: &muda::MenuItemKind) -> Result<(), DesktopSystemError> {
    with_menu_item(item, |item| menu.append(item))
        .map_err(|error| DesktopSystemError::Menu(error.to_string()))
}

#[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
fn append_submenu(
    menu: &muda::Submenu,
    item: &muda::MenuItemKind,
) -> Result<(), DesktopSystemError> {
    with_menu_item(item, |item| menu.append(item))
        .map_err(|error| DesktopSystemError::Menu(error.to_string()))
}

#[cfg(all(feature = "menus", any(target_os = "macos", target_os = "windows")))]
fn with_menu_item<T>(
    item: &muda::MenuItemKind,
    apply: impl FnOnce(&dyn muda::IsMenuItem) -> T,
) -> T {
    match item {
        muda::MenuItemKind::MenuItem(item) => apply(item),
        muda::MenuItemKind::Submenu(item) => apply(item),
        muda::MenuItemKind::Predefined(item) => apply(item),
        muda::MenuItemKind::Check(item) => apply(item),
        muda::MenuItemKind::Icon(item) => apply(item),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::VecDeque;
    use vo_ui_system::{
        decode_system_response, encode_system_request, FileDialogFilter, FileDialogKind,
        FileDragMode, MenuNode, MessageDialogButtons, MessageDialogLevel, SystemRequestEnvelope,
    };

    #[derive(Default)]
    struct FakeBackend {
        clipboard: Option<ClipboardContent>,
        dialog: FileDialogResult,
        installed: Vec<u64>,
        menu_events: VecDeque<MenuItemId>,
        drags: Vec<FileDragRequest>,
    }

    impl NativeSystemBackend for FakeBackend {
        type Error = &'static str;

        fn read_clipboard(
            &mut self,
            _format: ClipboardFormat,
        ) -> Result<Option<ClipboardContent>, Self::Error> {
            Ok(self.clipboard.clone())
        }

        fn write_clipboard(&mut self, content: &ClipboardContent) -> Result<(), Self::Error> {
            self.clipboard = Some(content.clone());
            Ok(())
        }

        fn show_file_dialog(
            &mut self,
            _request: &FileDialogRequest,
        ) -> Result<FileDialogResult, Self::Error> {
            Ok(self.dialog.clone())
        }

        fn show_message_dialog(
            &mut self,
            _request: &MessageDialogRequest,
        ) -> Result<MessageDialogResult, Self::Error> {
            Ok(MessageDialogResult::Ok)
        }

        fn install_menu(&mut self, model: &MenuModel) -> Result<(), Self::Error> {
            self.installed.push(model.revision);
            Ok(())
        }

        fn poll_menu_activation(&mut self) -> Result<Option<MenuItemId>, Self::Error> {
            Ok(self.menu_events.pop_front())
        }

        fn begin_file_drag(&mut self, request: &FileDragRequest) -> Result<(), Self::Error> {
            self.drags.push(request.clone());
            Ok(())
        }
    }

    fn menu() -> (MenuModel, MenuItemId) {
        let file = MenuItemId::new(1, 1);
        let save = MenuItemId::new(2, 1);
        (
            MenuModel {
                revision: 4,
                roots: vec![MenuNode::Submenu {
                    id: file,
                    label: String::from("File"),
                    enabled: true,
                    children: vec![MenuNode::Command {
                        id: save,
                        label: String::from("Save"),
                        enabled: true,
                        shortcut: Some(String::from("CmdOrCtrl+S")),
                    }],
                }],
            },
            save,
        )
    }

    #[test]
    fn validation_happens_before_clipboard_and_dialog_backends() {
        let mut host = NativeSystemHost::new(
            FakeBackend::default(),
            NativeSystemHostConfig {
                limits: SystemLimits {
                    max_text_bytes: 4,
                    ..SystemLimits::default()
                },
                ..NativeSystemHostConfig::default()
            },
        )
        .unwrap();
        assert_eq!(
            host.write_clipboard(&ClipboardContent::Text(String::from("longer"))),
            Err(NativeSystemHostError::Contract(
                SystemContractError::TextLimitExceeded
            ))
        );
        let request = FileDialogRequest {
            kind: FileDialogKind::OpenFile,
            title: String::from("long title"),
            initial_directory: None,
            initial_file_name: None,
            filters: vec![FileDialogFilter {
                name: String::from("vo"),
                extensions: vec![String::from("vo")],
            }],
            can_create_directories: false,
        };
        assert!(matches!(
            host.show_file_dialog(&request),
            Err(NativeSystemHostError::Contract(
                SystemContractError::TextLimitExceeded
            ))
        ));
    }

    #[test]
    fn menu_and_drag_events_share_one_bounded_monotonic_fifo() {
        let (model, save) = menu();
        let mut backend = FakeBackend::default();
        backend.menu_events.push_back(save);
        let mut host = NativeSystemHost::new(backend, NativeSystemHostConfig::default()).unwrap();
        assert_eq!(host.install_menu(&model), Ok(true));
        assert_eq!(host.install_menu(&model), Ok(false));
        assert_eq!(host.pump_menu_events(1), Ok(1));
        assert_eq!(
            host.push_drag_drop(
                DragDropPhase::Dropped,
                5.0,
                9.0,
                vec![String::from("/tmp/demo.vo")]
            ),
            Ok(2)
        );
        let events = host.drain_events(8);
        assert_eq!(
            events[0],
            SystemEvent::MenuActivated {
                sequence: 1,
                item: save
            }
        );
        assert!(matches!(
            &events[1],
            SystemEvent::DragDrop(DragDropEvent { sequence: 2, .. })
        ));
    }

    #[test]
    fn unknown_native_menu_identity_fails_closed() {
        let (model, _) = menu();
        let unknown = MenuItemId::new(99, 1);
        let mut backend = FakeBackend::default();
        backend.menu_events.push_back(unknown);
        let mut host = NativeSystemHost::new(backend, NativeSystemHostConfig::default()).unwrap();
        host.install_menu(&model).unwrap();
        assert_eq!(
            host.pump_menu_events(1),
            Err(NativeSystemHostError::UnknownMenuItem(unknown))
        );
        assert_eq!(host.pending_event_count(), 0);
    }

    #[test]
    fn message_dialog_contract_reaches_backend() {
        let mut host =
            NativeSystemHost::new(FakeBackend::default(), NativeSystemHostConfig::default())
                .unwrap();
        assert_eq!(
            host.show_message_dialog(&MessageDialogRequest {
                level: MessageDialogLevel::Warning,
                buttons: MessageDialogButtons::OkCancel,
                title: String::from("Unsaved changes"),
                description: String::from("Close this window?"),
            }),
            Ok(MessageDialogResult::Ok)
        );
    }

    #[test]
    fn file_drag_request_is_validated_then_started_once() {
        let mut host =
            NativeSystemHost::new(FakeBackend::default(), NativeSystemHostConfig::default())
                .unwrap();
        let request = FileDragRequest {
            paths: vec![String::from("/tmp/demo.vo")],
            preview: Some(String::from("/tmp/demo.png")),
            mode: FileDragMode::Copy,
        };
        assert_eq!(host.begin_file_drag(&request), Ok(()));
        assert_eq!(host.backend().drags, vec![request]);
    }

    #[test]
    fn vus1_frames_execute_without_entering_the_render_protocol() {
        let mut host =
            NativeSystemHost::new(FakeBackend::default(), NativeSystemHostConfig::default())
                .unwrap();
        let request = encode_system_request(
            &SystemRequestEnvelope {
                request_id: 17,
                request: SystemRequest::WriteClipboard(ClipboardContent::Text(String::from(
                    "copied",
                ))),
            },
            SystemLimits::default(),
        )
        .unwrap();
        let response = host.execute_request_frame(&request).unwrap().unwrap();
        let response = decode_system_response(&response, SystemLimits::default()).unwrap();
        assert_eq!(response.request_id, 17);
        assert_eq!(response.response, SystemResponse::Complete);
        assert_eq!(
            host.backend().clipboard,
            Some(ClipboardContent::Text(String::from("copied")))
        );
    }

    #[test]
    fn application_host_invocation_is_explicit_and_replaceable() {
        let mut host =
            NativeSystemHost::new(FakeBackend::default(), NativeSystemHostConfig::default())
                .unwrap();
        let request = HostInvocation {
            service: String::from("volang.studio.host.v1"),
            operation: String::from("health"),
            payload: vec![7],
        };
        assert!(matches!(
            host.execute_request(&SystemRequest::InvokeHost(request.clone()))
                .unwrap(),
            Some(SystemResponse::Failure(SystemFailure {
                kind: SystemFailureKind::Unsupported,
                ..
            }))
        ));
        host.set_host_invocation_handler(Arc::new(|request| {
            let mut result = request.payload.clone();
            result.push(8);
            Ok(result)
        }));
        assert_eq!(
            host.execute_request(&SystemRequest::InvokeHost(request))
                .unwrap(),
            Some(SystemResponse::HostPayload(vec![7, 8]))
        );
    }

    #[test]
    fn framed_application_host_invocation_completes_off_thread() {
        let mut host =
            NativeSystemHost::new(FakeBackend::default(), NativeSystemHostConfig::default())
                .unwrap();
        host.set_host_invocation_handler(Arc::new(|request| {
            let mut result = request.payload.clone();
            result.push(9);
            Ok(result)
        }));
        let request = encode_system_request(
            &SystemRequestEnvelope {
                request_id: 41,
                request: SystemRequest::InvokeHost(HostInvocation {
                    service: String::from("studio"),
                    operation: String::from("health"),
                    payload: vec![1, 2],
                }),
            },
            SystemLimits::default(),
        )
        .unwrap();
        assert!(host.execute_request_frame(&request).unwrap().is_none());
        let response = loop {
            if let Some(response) = host.execute_request_frame(&request).unwrap() {
                break response;
            }
            std::thread::yield_now();
        };
        assert_eq!(host.pending_host_invocation_count(), 0);
        assert_eq!(
            decode_system_response(&response, SystemLimits::default())
                .unwrap()
                .response,
            SystemResponse::HostPayload(vec![1, 2, 9])
        );
    }
}
