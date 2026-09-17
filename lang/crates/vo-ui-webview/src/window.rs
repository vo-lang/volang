use crate::{
    ipc::{Action, Bridge, MAX_IPC_BYTES},
    local_url, ApplicationId, Assets, ORIGIN,
};
use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc, Arc,
    },
    time::{Duration, Instant},
};
use tao::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoopBuilder},
    platform::run_return::EventLoopExtRunReturn,
    window::WindowBuilder,
};
use vo_ui_native::{
    executor::{Completion, Config, Event as ExecutionEvent, Executor},
    ExchangeId,
};
use vo_vm::vm::Vm;
use wry::{WebView, WebViewBuilder};

mod external;
mod protocol;

#[cfg(target_os = "macos")]
mod menu;

/// Receives normalized HTTP(S) URLs on the window's browser-launch worker.
pub type ExternalLinkHandler = fn(&str) -> Result<(), String>;

#[derive(Clone, Debug)]
pub struct WindowOptions {
    /// Stable persistent profile. None gives development/embedding probes an
    /// ephemeral WebView, without sharing another application's browser data.
    pub application_id: Option<ApplicationId>,
    pub title: String,
    pub width: u32,
    pub height: u32,
    pub startup_timeout: Duration,
    pub close_timeout: Duration,
    /// Embedders and automated probes may return errors immediately. Ordinary
    /// applications keep diagnostics visible until the user closes the window.
    pub exit_on_failure: bool,
    /// HTTP(S) links are handed to this platform handler outside the GUI thread.
    /// Set to None to keep external navigation disabled for an embedded window.
    pub open_external: Option<ExternalLinkHandler>,
    pub execution: Config,
}

impl Default for WindowOptions {
    fn default() -> Self {
        Self {
            application_id: None,
            title: "Volang".into(),
            width: 1080,
            height: 800,
            startup_timeout: Duration::from_secs(30),
            close_timeout: Duration::from_secs(3),
            exit_on_failure: false,
            open_external: Some(external::open_browser),
            execution: Config::default(),
        }
    }
}

#[derive(Debug)]
enum Wake {
    Ipc,
    Execution,
    #[cfg(target_os = "macos")]
    HostTerminated,
    #[cfg(target_os = "macos")]
    Close,
}

/// Run one application window on the main thread. All VM work, including the
/// supplied verified-module factory, runs on its dedicated owner thread.
/// The returning event loop permits native AOT entrypoints to release the shell;
/// it is never used as a polling loop or to run guest work outside GUI callbacks.
pub fn run(
    options: WindowOptions,
    assets: Assets,
    create: impl FnOnce() -> Result<Vm, String> + Send + 'static,
) -> Result<Completion, String> {
    if options.width == 0
        || options.height == 0
        || options.width > 16384
        || options.height > 16384
        || options.startup_timeout.is_zero()
        || options.close_timeout.is_zero()
        || options.execution.io_poll_interval.is_zero()
    {
        return Err("invalid desktop window dimensions or scheduling configuration".into());
    }
    let mut random = [0; 16];
    getrandom::fill(&mut random).map_err(|error| error.to_string())?;
    let token: String = random.iter().map(|byte| format!("{byte:02x}")).collect();
    let assets = assets.with_token(&token)?;
    #[cfg(target_os = "macos")]
    if options.application_id.is_some()
        && objc2_foundation::NSProcessInfo::processInfo()
            .operatingSystemVersion()
            .majorVersion
            < 14
    {
        return Err("persistent desktop UI requires macOS 14 or newer".into());
    }
    #[cfg(not(target_os = "macos"))]
    let directory = options
        .application_id
        .as_ref()
        .map(|id| {
            let root = dirs::data_local_dir()
                .ok_or("cannot locate the user's application data directory")?;
            let directory = id.directory_in(&root);
            std::fs::create_dir_all(&directory)
                .map_err(|error| format!("desktop browser storage: {error}"))?;
            Ok::<_, String>(directory)
        })
        .transpose()?;
    #[cfg(target_os = "macos")]
    let directory = None;
    let mut bridge = Bridge::new(token);
    let mut event_loop = EventLoopBuilder::<Wake>::with_user_event().build();
    let proxy = event_loop.create_proxy();
    let window = WindowBuilder::new()
        .with_title(&options.title)
        .with_inner_size(tao::dpi::LogicalSize::new(options.width, options.height))
        .build(&event_loop)
        .map_err(|error| error.to_string())?;
    let (ipc_tx, ipc_rx) = mpsc::sync_channel(1);
    let overflow = Arc::new(AtomicBool::new(false));
    let posted = Arc::new(AtomicBool::new(false));
    let ipc_overflow = overflow.clone();
    let ipc_posted = posted.clone();
    let ipc_proxy = proxy.clone();
    let external = options
        .open_external
        .map(external::ExternalLinks::new)
        .transpose()?
        .map(std::rc::Rc::new);
    let navigation_links = external.clone();
    let popup_links = external.clone();
    let mut web_context = wry::WebContext::new(directory);
    let builder = WebViewBuilder::new_with_web_context(&mut web_context)
        .with_initialization_script(include_str!("window/locale.js"))
        .with_incognito(options.application_id.is_none())
        .with_clipboard(true)
        .with_devtools(cfg!(debug_assertions))
        .with_custom_protocol("volang".into(), move |_, request| {
            protocol::respond(&assets, &request)
        })
        .with_navigation_handler(move |url| {
            if local_url(&url) {
                return true;
            }
            if let Some(links) = &navigation_links {
                links.open(&url);
            }
            false
        })
        .with_new_window_req_handler(move |url, _| {
            if !local_url(&url) {
                if let Some(links) = &popup_links {
                    links.open(&url);
                }
            }
            wry::NewWindowResponse::Deny
        })
        .with_ipc_handler(move |request| {
            if request.body().len() > MAX_IPC_BYTES || ipc_tx.try_send(request.into_body()).is_err()
            {
                ipc_overflow.store(true, Ordering::Relaxed);
            }
            if !ipc_posted.swap(true, Ordering::Relaxed) {
                let _ = ipc_proxy.send_event(Wake::Ipc);
            }
        })
        .with_url(format!("{ORIGIN}/index.html"));
    #[cfg(target_os = "macos")]
    let builder = {
        use wry::WebViewBuilderExtDarwin;
        let proxy = proxy.clone();
        let builder = if let Some(id) = &options.application_id {
            builder.with_data_store_identifier(id.store_id())
        } else {
            builder
        };
        builder.with_on_web_content_process_terminate_handler(move || {
            let _ = proxy.send_event(Wake::HostTerminated);
        })
    };
    #[cfg(not(target_os = "linux"))]
    let webview = builder.build(&window).map_err(|error| error.to_string())?;
    #[cfg(target_os = "linux")]
    let webview = {
        use tao::platform::unix::WindowExtUnix;
        use wry::WebViewBuilderExtUnix;
        builder
            .build_gtk(
                window
                    .default_vbox()
                    .ok_or("missing desktop window container")?,
            )
            .map_err(|error| error.to_string())?
    };

    #[cfg(target_os = "macos")]
    let _menu = menu::Menu::install(&options.title, proxy.clone())?;
    let mut create = Some(create);
    let mut executor: Option<Executor> = None;
    let mut pending: Option<ExchangeId> = None;
    let mut deadline = Some(Instant::now() + options.startup_timeout);
    let mut closing = false;
    let mut terminal = false;
    let mut result = None;
    let code = event_loop.run_return(|event, _, flow| {
        *flow = deadline.map_or(ControlFlow::Wait, ControlFlow::WaitUntil);
        let action: Result<(), String> = (|| {
            if deadline.is_some_and(|deadline| Instant::now() >= deadline) {
                if closing {
                    *flow = ControlFlow::Exit;
                }
                return Err(if closing {
                    "desktop close timed out; interruption requested"
                } else {
                    "desktop host did not start before its deadline"
                }
                .into());
            }
            match event {
                #[cfg(target_os = "macos")]
                Event::UserEvent(Wake::HostTerminated) => {
                    // The rendering process cannot display recovery controls.
                    // Close the shell and release the native execution owner.
                    *flow = ControlFlow::Exit;
                    return Err(
                        "desktop rendering process terminated; reopen the application".into(),
                    );
                }
                Event::UserEvent(Wake::Ipc) => {
                    posted.store(false, Ordering::Relaxed);
                    if terminal {
                        while ipc_rx.try_recv().is_ok() {}
                        return Ok(());
                    }
                    if overflow.swap(false, Ordering::Relaxed) {
                        return Err("desktop IPC mailbox limit exceeded".into());
                    }
                    while let Ok(text) = ipc_rx.try_recv() {
                        match bridge.receive(&text)? {
                            Action::Start => {
                                let create =
                                    create.take().ok_or("desktop session already started")?;
                                let proxy = proxy.clone();
                                executor = Some(
                                    Executor::spawn(options.execution, create, move || {
                                        proxy.send_event(Wake::Execution).is_ok()
                                    })
                                    .map_err(|error| error.to_string())?,
                                );
                            }
                            Action::Reply(bytes) => {
                                let id = pending
                                    .take()
                                    .ok_or("desktop reply has no native exchange")?;
                                executor
                                    .as_ref()
                                    .ok_or("desktop execution is absent")?
                                    .respond(&id, bytes)
                                    .map_err(|error| error.to_string())?;
                            }
                            Action::Failure(message) => return Err(message),
                        }
                    }
                }
                Event::UserEvent(Wake::Execution) => {
                    let Some(executor) = &executor else {
                        return Err("unexpected native executor wake".into());
                    };
                    while let Ok(event) = executor.try_recv() {
                        if terminal {
                            continue;
                        }
                        match event {
                            ExecutionEvent::Exchange(exchange) => {
                                if !closing {
                                    deadline = None;
                                }
                                if pending.is_some() {
                                    return Err("overlapping native desktop exchanges".into());
                                }
                                let script = bridge.exchange(&exchange.bytes)?;
                                pending = Some(exchange.id);
                                webview
                                    .evaluate_script(&script)
                                    .map_err(|error| error.to_string())?;
                            }
                            ExecutionEvent::Finished(completion) => {
                                terminal = true;
                                pending = None;
                                deadline = None;
                                if let Err(error) = &completion.result {
                                    show_failure(&webview, &error.to_string());
                                    if options.exit_on_failure || closing {
                                        *flow = ControlFlow::Exit;
                                    }
                                } else {
                                    let _ =
                                        webview.evaluate_script("window.__volangDesktop.dispose()");
                                    *flow = ControlFlow::Exit;
                                }
                                result = Some(Ok(*completion));
                            }
                        }
                    }
                }
                #[cfg(target_os = "macos")]
                Event::UserEvent(Wake::Close) => {
                    request_close(
                        &webview,
                        executor.is_some() && !terminal,
                        &mut closing,
                        &mut deadline,
                        options.close_timeout,
                        flow,
                    )?;
                }
                Event::WindowEvent {
                    event: WindowEvent::CloseRequested,
                    ..
                } => {
                    request_close(
                        &webview,
                        executor.is_some() && !terminal,
                        &mut closing,
                        &mut deadline,
                        options.close_timeout,
                        flow,
                    )?;
                }
                _ => {}
            }
            Ok(())
        })();
        if let Err(error) = action {
            if let Some(executor) = &executor {
                executor.stop();
            }
            pending = None;
            terminal = true;
            deadline = None;
            show_failure(&webview, &error);
            result = Some(Err(error));
            if options.exit_on_failure || closing {
                *flow = ControlFlow::Exit;
            }
        }
        if !matches!(flow, ControlFlow::ExitWithCode(_)) {
            *flow = deadline.map_or(ControlFlow::Wait, ControlFlow::WaitUntil);
        }
    });
    if let Some(executor) = executor {
        executor.stop();
        if executor.is_finished() {
            executor
                .join()
                .map_err(|_| "desktop worker teardown failed")?;
        }
        // Drop remains nonblocking if a foreign call has not cooperated with
        // interruption. It retains only worker-owned data, never this WebView.
    }
    drop(webview);
    drop(window);
    if code != 0 {
        return Err(format!("desktop event loop exited with code {code}"));
    }
    result.unwrap_or_else(|| {
        Ok(Completion {
            result: Ok(vo_ui_native::Exit::Stopped),
            stats: Default::default(),
        })
    })
}

fn show_failure(webview: &WebView, message: &str) {
    let message = serde_json::to_string(message).expect("diagnostic string serializes");
    let _ = webview.evaluate_script(&format!("if(window.__volangDesktop){{window.__volangDesktop.failHost({message})}}else{{document.body.textContent={message}}}"));
}

fn request_close(
    webview: &WebView,
    running: bool,
    closing: &mut bool,
    deadline: &mut Option<Instant>,
    timeout: Duration,
    flow: &mut ControlFlow,
) -> Result<(), String> {
    if !running {
        *flow = ControlFlow::Exit;
    } else if !*closing {
        *closing = true;
        *deadline = Some(Instant::now() + timeout);
        webview
            .evaluate_script("window.__volangDesktop.close()")
            .map_err(|error| error.to_string())?;
    }
    Ok(())
}
