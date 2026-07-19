//! HTTP package native function implementations.
//!
//! Provides the `nativeHttpsRequest` extern backed by ureq running on a
//! background OS thread.  A pipe signals completion to the I/O reactor so
//! the VM scheduler is never blocked — other goroutines (including
//! in-process HTTP servers) keep running.

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(all(feature = "std", unix))]
use std::io;
#[cfg(feature = "std")]
use std::io::Read;
#[cfg(all(feature = "std", unix))]
use std::os::fd::{AsFd, AsRawFd, FromRawFd, OwnedFd};
#[cfg(feature = "std")]
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
#[cfg(feature = "std")]
use std::sync::mpsc::{SyncSender, TrySendError};
#[cfg(feature = "std")]
use std::sync::{Arc, Mutex, OnceLock};

#[cfg(feature = "std")]
use vo_ffi_macro::{vostd_errors, vostd_fn};
#[cfg(feature = "std")]
use vo_runtime::builtins::error_helper::{write_error_to, write_nil_error};
use vo_runtime::ffi::ExternRegistry;
#[cfg(feature = "std")]
use vo_runtime::ffi::{ExternCallContext, ExternResult, StdlibEntry};
#[cfg(all(feature = "std", unix))]
use vo_runtime::io::{IoCancellation, IoLease};
#[cfg(feature = "std")]
use vo_runtime::io::{IoResourceToken, IoRuntime};

// ── Sentinel errors ──────────────────────────────────────────────────

#[cfg(feature = "std")]
vostd_errors! {
    "http" for "net/http" => {
        Timeout => "request timeout",
    }
}

// ── Background HTTP result storage ───────────────────────────────────

#[cfg(feature = "std")]
struct HttpResponse {
    status_code: i64,
    status: String,
    proto: String,
    headers: Vec<String>,
    body: Vec<u8>,
    error: Option<String>,
    is_timeout: bool,
}

#[cfg(feature = "std")]
impl HttpResponse {
    fn transport_error(message: impl Into<String>) -> Self {
        Self {
            status_code: 0,
            status: String::new(),
            proto: String::new(),
            headers: Vec::new(),
            body: Vec::new(),
            error: Some(message.into()),
            is_timeout: false,
        }
    }

    fn canceled() -> Self {
        Self::transport_error("request canceled")
    }
}

#[cfg(feature = "std")]
type RequestHeaders = Vec<(String, String)>;

#[cfg(feature = "std")]
type RequestText = (String, String, RequestHeaders);

#[cfg(feature = "std")]
struct HttpRequestInner {
    response: Option<HttpResponse>,
    completed: bool,
    started: bool,
    #[cfg(unix)]
    wake: Option<HttpWake>,
}

#[cfg(feature = "std")]
struct HttpRequestControl {
    canceled: AtomicBool,
    inner: Mutex<HttpRequestInner>,
}

#[cfg(feature = "std")]
impl HttpRequestControl {
    fn new() -> Self {
        Self {
            canceled: AtomicBool::new(false),
            inner: Mutex::new(HttpRequestInner {
                response: None,
                completed: false,
                started: false,
                #[cfg(unix)]
                wake: None,
            }),
        }
    }

    fn is_canceled(&self) -> bool {
        self.canceled.load(Ordering::Acquire)
    }

    fn cancel_and_was_started(&self) -> bool {
        self.canceled.store(true, Ordering::Release);
        self.complete(HttpResponse::canceled());
        self.inner
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .started
    }

    fn begin(&self) -> Result<(), &'static str> {
        let mut inner = self
            .inner
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if inner.completed {
            return Err("HTTP request was canceled before transport registration");
        }
        if inner.started {
            return Err("HTTP request handle was already started");
        }
        inner.started = true;
        Ok(())
    }

    fn complete_worker(&self, response: HttpResponse) {
        if self.is_canceled() {
            self.complete(HttpResponse::canceled());
        } else {
            self.complete(response);
        }
    }

    fn complete(&self, response: HttpResponse) {
        #[cfg(unix)]
        let wake = {
            let mut inner = self
                .inner
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if inner.completed {
                return;
            }
            inner.completed = true;
            inner.response = Some(response);
            inner.wake.take()
        };

        #[cfg(not(unix))]
        {
            let mut inner = self
                .inner
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if !inner.completed {
                inner.completed = true;
                inner.response = Some(response);
            }
        }

        #[cfg(unix)]
        if let Some(wake) = wake {
            let _ = wake.signal();
        }
    }

    fn take_response(&self) -> Option<HttpResponse> {
        self.inner
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .response
            .take()
    }

    #[cfg(unix)]
    fn register_wake(&self, wake: HttpWake) -> Result<(), &'static str> {
        let wake_now = {
            let mut inner = self
                .inner
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner);
            if inner.wake.is_some() {
                return Err("HTTP request already has a wake registration");
            }
            if inner.completed {
                Some(wake)
            } else {
                inner.wake = Some(wake);
                None
            }
        };
        if let Some(wake) = wake_now {
            let _ = wake.signal();
        }
        Ok(())
    }
}

#[cfg(all(feature = "std", unix))]
struct HttpWake {
    write_fd: OwnedFd,
    // Keeps one reader alive until the single wake write. This prevents a
    // late worker or cancellation from delivering SIGPIPE after VM teardown.
    read_guard: OwnedFd,
}

#[cfg(all(feature = "std", unix))]
impl HttpWake {
    fn signal(self) -> io::Result<()> {
        let result = signal_http_worker(&self.write_fd);
        drop(self.read_guard);
        result
    }
}

#[cfg(feature = "std")]
struct HttpRequestEntry {
    cleanup_token: IoResourceToken,
    control: Arc<HttpRequestControl>,
}

#[cfg(feature = "std")]
#[derive(Clone)]
struct HttpTokenState {
    request_id: i64,
    cleanup_token: IoResourceToken,
    control: Arc<HttpRequestControl>,
}

#[cfg(feature = "std")]
static NEXT_HTTP_REQUEST_ID: AtomicU64 = AtomicU64::new(1);

#[cfg(feature = "std")]
fn http_requests() -> &'static Mutex<HashMap<i64, HttpRequestEntry>> {
    static REQUESTS: OnceLock<Mutex<HashMap<i64, HttpRequestEntry>>> = OnceLock::new();
    REQUESTS.get_or_init(|| Mutex::new(HashMap::new()))
}

#[cfg(feature = "std")]
fn allocate_http_request_id(counter: &AtomicU64) -> Option<i64> {
    counter
        .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |current| {
            if current == 0 || current > i64::MAX as u64 {
                None
            } else if current == i64::MAX as u64 {
                Some(0)
            } else {
                Some(current + 1)
            }
        })
        .ok()
        .and_then(|id| i64::try_from(id).ok())
}

#[cfg(feature = "std")]
fn allocate_http_request(io: &mut IoRuntime) -> Result<i64, String> {
    let id = allocate_http_request_id(&NEXT_HTTP_REQUEST_ID)
        .ok_or_else(|| "HTTP request handle space exhausted".to_string())?;
    let cleanup_token = io
        .register_resource_cleanup(move |token| move || cleanup_http_request(id, token))
        .map_err(|error| format!("failed to register HTTP request cleanup: {error}"))?;
    let entry = HttpRequestEntry {
        cleanup_token,
        control: Arc::new(HttpRequestControl::new()),
    };
    let mut requests = http_requests()
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    if requests.insert(id, entry).is_some() {
        drop(requests);
        io.disarm_resource_cleanup(cleanup_token);
        return Err(format!(
            "HTTP request handle allocator produced duplicate handle {id}"
        ));
    }
    Ok(id)
}

#[cfg(feature = "std")]
fn cleanup_http_request(id: i64, cleanup_token: IoResourceToken) {
    let removed = {
        let mut requests = http_requests()
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if requests
            .get(&id)
            .is_some_and(|entry| entry.cleanup_token == cleanup_token)
        {
            requests.remove(&id)
        } else {
            None
        }
    };
    if let Some(entry) = removed {
        entry.control.cancel_and_was_started();
    }
}

#[cfg(feature = "std")]
fn request_state(id: i64) -> Option<HttpTokenState> {
    http_requests()
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(&id)
        .map(|entry| HttpTokenState {
            request_id: id,
            cleanup_token: entry.cleanup_token,
            control: entry.control.clone(),
        })
}

#[cfg(feature = "std")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum HttpCancelResult {
    Missing,
    PreTransport,
    InFlight,
}

#[cfg(feature = "std")]
fn cancel_http_request(id: i64) -> HttpCancelResult {
    let control = http_requests()
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
        .get(&id)
        .map(|entry| entry.control.clone());
    if let Some(control) = control {
        if control.cancel_and_was_started() {
            HttpCancelResult::InFlight
        } else {
            HttpCancelResult::PreTransport
        }
    } else {
        HttpCancelResult::Missing
    }
}

#[cfg(feature = "std")]
fn release_http_request(
    io: &mut IoRuntime,
    id: i64,
    expected_cleanup: Option<IoResourceToken>,
) -> bool {
    let removed = {
        let mut requests = http_requests()
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        let matches = requests.get(&id).is_some_and(|entry| {
            expected_cleanup.is_none_or(|expected| entry.cleanup_token == expected)
        });
        matches.then(|| requests.remove(&id)).flatten()
    };
    if let Some(entry) = removed {
        io.disarm_resource_cleanup(entry.cleanup_token);
        true
    } else {
        false
    }
}

#[cfg(feature = "std")]
struct HttpJob {
    method: String,
    url: String,
    headers: RequestHeaders,
    body: Vec<u8>,
    timeout_ns: i64,
    control: Arc<HttpRequestControl>,
}

#[cfg(feature = "std")]
struct HttpWorkerPool {
    sender: SyncSender<HttpJob>,
}

#[cfg(feature = "std")]
impl HttpWorkerPool {
    fn new() -> Result<Self, String> {
        const QUEUE_CAPACITY: usize = 256;
        let (sender, receiver) = std::sync::mpsc::sync_channel::<HttpJob>(QUEUE_CAPACITY);
        let receiver = Arc::new(Mutex::new(receiver));
        let worker_count = std::thread::available_parallelism()
            .map(|count| count.get().clamp(2, 8))
            .unwrap_or(4);
        let mut started = 0usize;
        for index in 0..worker_count {
            let receiver = receiver.clone();
            if std::thread::Builder::new()
                .name(format!("vo-http-worker-{index}"))
                .spawn(move || loop {
                    let job = receiver
                        .lock()
                        .unwrap_or_else(std::sync::PoisonError::into_inner)
                        .recv();
                    let Ok(job) = job else {
                        break;
                    };
                    if job.control.is_canceled() {
                        job.control.cancel_and_was_started();
                        continue;
                    }
                    let response = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                        run_ureq(job.method, job.url, job.headers, job.body, job.timeout_ns)
                    }))
                    .unwrap_or_else(|_| HttpResponse::transport_error("HTTP worker panicked"));
                    job.control.complete_worker(response);
                })
                .is_ok()
            {
                started += 1;
            }
        }
        if started == 0 {
            return Err("failed to start HTTP worker pool".to_string());
        }
        Ok(Self { sender })
    }

    fn submit(&self, job: HttpJob) -> Result<(), String> {
        self.sender.try_send(job).map_err(|error| match error {
            TrySendError::Full(_) => "HTTP worker queue is full".to_string(),
            TrySendError::Disconnected(_) => "HTTP worker pool is unavailable".to_string(),
        })
    }
}

#[cfg(feature = "std")]
fn submit_http_job(job: HttpJob) -> Result<(), String> {
    static POOL: OnceLock<Result<HttpWorkerPool, String>> = OnceLock::new();
    match POOL.get_or_init(HttpWorkerPool::new) {
        Ok(pool) => pool.submit(job),
        Err(error) => Err(error.clone()),
    }
}

// ── ureq execution (runs on background thread) ──────────────────────

#[cfg(feature = "std")]
fn run_ureq(
    method: String,
    url: String,
    headers: RequestHeaders,
    body: Vec<u8>,
    timeout_ns: i64,
) -> HttpResponse {
    // ureq does not expose an in-flight socket abort handle. A fixed upper
    // bound keeps the bounded worker pool live when a peer never produces
    // headers or a body. Logical cancellation wakes and releases the Vo
    // request immediately through HttpRequestControl.
    const MAX_REQUEST_TIME: std::time::Duration = std::time::Duration::from_secs(300);
    let requested = if timeout_ns > 0 {
        std::time::Duration::from_nanos(timeout_ns as u64)
    } else {
        MAX_REQUEST_TIME
    };
    let builder = ureq::AgentBuilder::new()
        .timeout(requested.min(MAX_REQUEST_TIME))
        .max_idle_connections(0);
    let agent = builder.build();

    let mut request = agent.request(&method, &url);
    for (key, value) in &headers {
        request = request.set(key, value);
    }

    let response_result = if body.is_empty() {
        request.call()
    } else {
        request.send_bytes(&body)
    };

    let response = match response_result {
        Ok(resp) => resp,
        Err(ureq::Error::Status(_, resp)) => resp,
        Err(ureq::Error::Transport(err)) => {
            let msg = err.to_string();
            let lower = msg.to_ascii_lowercase();
            return HttpResponse {
                status_code: 0,
                status: String::new(),
                proto: String::new(),
                headers: Vec::new(),
                body: Vec::new(),
                is_timeout: lower.contains("timeout") || lower.contains("timed out"),
                error: Some(msg),
            };
        }
    };

    let status_code = response.status() as i64;
    let proto = "HTTP/1.1".to_string();
    let status = format!("{} {} {}", proto, status_code, response.status_text());

    let mut resp_headers = Vec::new();
    for name in response.headers_names() {
        for value in response.all(&name) {
            resp_headers.push(format!("{}: {}", name, value));
        }
    }

    let mut resp_body = Vec::new();
    if let Err(e) = response.into_reader().read_to_end(&mut resp_body) {
        return HttpResponse {
            status_code: 0,
            status: String::new(),
            proto: String::new(),
            headers: Vec::new(),
            body: Vec::new(),
            error: Some(e.to_string()),
            is_timeout: false,
        };
    }

    HttpResponse {
        status_code,
        status,
        proto,
        headers: resp_headers,
        body: resp_body,
        error: None,
        is_timeout: false,
    }
}

// ── Extern implementation ────────────────────────────────────────────

#[cfg(all(feature = "std", unix))]
fn create_http_wake_pipe() -> io::Result<(OwnedFd, OwnedFd)> {
    let mut fds = [-1; 2];

    #[cfg(any(target_os = "linux", target_os = "android"))]
    let result = unsafe { libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC) };

    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    let result = unsafe { libc::pipe(fds.as_mut_ptr()) };

    if result != 0 {
        return Err(io::Error::last_os_error());
    }

    let read_fd = unsafe { OwnedFd::from_raw_fd(fds[0]) };
    let write_fd = unsafe { OwnedFd::from_raw_fd(fds[1]) };

    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    for fd in [&read_fd, &write_fd] {
        let flags = unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_GETFD) };
        if flags == -1 {
            return Err(io::Error::last_os_error());
        }
        if unsafe { libc::fcntl(fd.as_raw_fd(), libc::F_SETFD, flags | libc::FD_CLOEXEC) } == -1 {
            return Err(io::Error::last_os_error());
        }
    }

    let flags = unsafe { libc::fcntl(read_fd.as_raw_fd(), libc::F_GETFL) };
    if flags == -1 {
        return Err(io::Error::last_os_error());
    }
    if unsafe { libc::fcntl(read_fd.as_raw_fd(), libc::F_SETFL, flags | libc::O_NONBLOCK) } == -1 {
        return Err(io::Error::last_os_error());
    }

    Ok((read_fd, write_fd))
}

#[cfg(all(feature = "std", unix))]
fn signal_http_worker(write_fd: &OwnedFd) -> io::Result<()> {
    let byte = 1u8;
    loop {
        let written = unsafe {
            libc::write(
                write_fd.as_raw_fd(),
                std::ptr::from_ref(&byte).cast::<libc::c_void>(),
                1,
            )
        };
        if written == 1 {
            return Ok(());
        }
        if written >= 0 {
            return Err(io::Error::new(
                io::ErrorKind::WriteZero,
                "HTTP wake pipe accepted zero bytes",
            ));
        }
        let error = io::Error::last_os_error();
        if error.kind() != io::ErrorKind::Interrupted {
            return Err(error);
        }
    }
}

#[cfg(feature = "std")]
#[vostd_fn("net/http", "nativeNewClientRequest", std)]
fn http_native_new_client_request(call: &mut ExternCallContext) -> ExternResult {
    match allocate_http_request(call.io_mut()) {
        Ok(id) => {
            call.ret_i64(0, id);
            write_nil_error(call, 1);
        }
        Err(error) => {
            call.ret_i64(0, 0);
            write_error_to(call, 1, &error);
        }
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
#[vostd_fn("net/http", "nativeCancelClientRequest", std)]
fn http_native_cancel_client_request(call: &mut ExternCallContext) -> ExternResult {
    let id = call.arg_i64(0);
    if id > 0 && cancel_http_request(id) == HttpCancelResult::PreTransport {
        release_http_request(call.io_mut(), id, None);
    }
    ExternResult::Ok
}

#[cfg(feature = "std")]
#[vostd_fn("net/http", "nativeReleaseClientRequest", std)]
fn http_native_release_client_request(call: &mut ExternCallContext) -> ExternResult {
    let id = call.arg_i64(0);
    if id > 0 {
        release_http_request(call.io_mut(), id, None);
    }
    ExternResult::Ok
}

#[cfg(all(feature = "std", unix))]
#[vostd_fn("net/http", "nativeHttpsRequest", std)]
fn http_native_https_request(call: &mut ExternCallContext) -> ExternResult {
    if let Some(token) = call.take_resume_io_token() {
        let state = call.io_mut().take_token_attachment::<HttpTokenState>(token);
        let completion = call.io_mut().take_completion(token);
        let Some(state) = state else {
            return write_transport_error(call, "HTTP worker response state is missing");
        };
        release_http_request(call.io_mut(), state.request_id, Some(state.cleanup_token));
        if let Err(error) = completion.result {
            return write_transport_error(call, &format!("HTTP worker wake failed: {error}"));
        }
        let Some(response) = state.control.take_response() else {
            return write_transport_error(call, "HTTP worker completed without a response");
        };
        return write_http_response(call, response);
    }

    let request_id = call.arg_i64(0);
    let Some(state) = request_state(request_id) else {
        return write_transport_error(call, "invalid or released HTTP request handle");
    };
    if let Some(response) = state.control.take_response() {
        release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
        return write_http_response(call, response);
    }
    if let Err(error) = state.control.begin() {
        release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
        return write_transport_error(call, error);
    }
    let (method, url, headers) = match read_request_text(call) {
        Ok(values) => values,
        Err(error) => {
            release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
            return write_transport_error(call, &error);
        }
    };
    let body = call.arg_bytes_owned(4);
    let timeout_ns = call.arg_i64(5);

    let (read_fd, write_fd) = match create_http_wake_pipe() {
        Ok(pipe) => pipe,
        Err(error) => {
            release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
            return write_transport_error(
                call,
                &format!("failed to create pipe for async HTTP: {error}"),
            );
        }
    };
    let read_guard = match read_fd.as_fd().try_clone_to_owned() {
        Ok(guard) => guard,
        Err(error) => {
            release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
            return write_transport_error(
                call,
                &format!("failed to duplicate HTTP wake pipe: {error}"),
            );
        }
    };
    let cancellation = match IoCancellation::new() {
        Ok(cancellation) => cancellation,
        Err(error) => {
            release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
            return write_transport_error(
                call,
                &format!("failed to allocate HTTP wake cancellation key: {error}"),
            );
        }
    };
    let cancel_key = cancellation.cancel_key();
    let lease = match IoLease::from_owned(read_fd, cancellation.clone()) {
        Ok(lease) => lease,
        Err(error) => {
            release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
            return write_transport_error(call, &format!("failed to own HTTP wake pipe: {error}"));
        }
    };
    let token = match call.io_mut().try_submit_lease_owned_read(lease, 1) {
        Ok(token) => token,
        Err(error) => {
            release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
            return write_transport_error(
                call,
                &format!("failed to allocate HTTP wake token: {error}"),
            );
        }
    };
    if let Some(completion) = call.io_mut().try_take_completion(token) {
        release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
        let message = match completion.result {
            Ok(_) => "HTTP wake pipe completed before registration".to_string(),
            Err(error) => format!("failed to register HTTP wake pipe: {error}"),
        };
        return write_transport_error(call, &message);
    }
    if call.io_mut().attach_to_token(token, state.clone()).is_err() {
        cancellation.cancel();
        call.io_mut().cancel(cancel_key);
        let _ = call.io_mut().try_take_completion(token);
        release_http_request(call.io_mut(), request_id, None);
        return write_transport_error(call, "HTTP wake token already has provider state");
    }
    if let Err(error) = state.control.register_wake(HttpWake {
        write_fd,
        read_guard,
    }) {
        cancellation.cancel();
        call.io_mut().cancel(cancel_key);
        let _ = call.io_mut().try_take_completion(token);
        let _ = call.io_mut().take_token_attachment::<HttpTokenState>(token);
        release_http_request(call.io_mut(), state.request_id, Some(state.cleanup_token));
        return write_transport_error(call, error);
    }
    let job = HttpJob {
        method,
        url,
        headers,
        body,
        timeout_ns,
        control: state.control.clone(),
    };
    if let Err(error) = submit_http_job(job) {
        state.control.complete(HttpResponse::transport_error(error));
    }
    ExternResult::WaitIo { token }
}

#[cfg(all(feature = "std", not(unix)))]
#[vostd_fn("net/http", "nativeHttpsRequest", std)]
fn http_native_https_request(call: &mut ExternCallContext) -> ExternResult {
    if let Some(token) = call.take_resume_io_token() {
        let state = call.io_mut().take_token_attachment::<HttpTokenState>(token);
        let completion = call.io_mut().take_completion(token);
        let Some(state) = state else {
            return write_transport_error(call, "HTTP worker response state is missing");
        };
        if let Err(error) = completion.result {
            release_http_request(call.io_mut(), state.request_id, Some(state.cleanup_token));
            return write_transport_error(call, &format!("HTTP worker timer failed: {error}"));
        }
        if let Some(response) = state.control.take_response() {
            release_http_request(call.io_mut(), state.request_id, Some(state.cleanup_token));
            return write_http_response(call, response);
        }
        return poll_http_worker(call, state);
    }

    let request_id = call.arg_i64(0);
    let Some(state) = request_state(request_id) else {
        return write_transport_error(call, "invalid or released HTTP request handle");
    };
    if let Some(response) = state.control.take_response() {
        release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
        return write_http_response(call, response);
    }
    if let Err(error) = state.control.begin() {
        release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
        return write_transport_error(call, error);
    }
    let (method, url, headers) = match read_request_text(call) {
        Ok(values) => values,
        Err(error) => {
            release_http_request(call.io_mut(), request_id, Some(state.cleanup_token));
            return write_transport_error(call, &error);
        }
    };
    let job = HttpJob {
        method,
        url,
        headers,
        body: call.arg_bytes_owned(4),
        timeout_ns: call.arg_i64(5),
        control: state.control.clone(),
    };
    if let Err(error) = submit_http_job(job) {
        state.control.complete(HttpResponse::transport_error(error));
    }
    poll_http_worker(call, state)
}

#[cfg(all(feature = "std", not(unix)))]
fn poll_http_worker(call: &mut ExternCallContext, state: HttpTokenState) -> ExternResult {
    const HTTP_POLL_NS: i64 = 1_000_000;
    let token = match call.io_mut().try_submit_timer(HTTP_POLL_NS) {
        Ok(token) => token,
        Err(error) => {
            release_http_request(call.io_mut(), state.request_id, Some(state.cleanup_token));
            return write_transport_error(
                call,
                &format!("failed to allocate HTTP worker timer: {error}"),
            );
        }
    };
    if let Some(completion) = call.io_mut().try_take_completion(token) {
        release_http_request(call.io_mut(), state.request_id, Some(state.cleanup_token));
        let message = match completion.result {
            Ok(_) => "HTTP worker timer completed during submission".to_string(),
            Err(error) => format!("failed to register HTTP worker timer: {error}"),
        };
        return write_transport_error(call, &message);
    }
    if let Err(state) = call.io_mut().attach_to_token(token, state) {
        let _ = call.io_mut().try_take_completion(token);
        release_http_request(call.io_mut(), state.request_id, Some(state.cleanup_token));
        return write_transport_error(call, "HTTP worker timer already has provider state");
    }
    ExternResult::WaitIo { token }
}

// ── Response writing helpers ─────────────────────────────────────────

#[cfg(feature = "std")]
fn write_http_response(call: &mut ExternCallContext, resp: HttpResponse) -> ExternResult {
    if let Some(err_msg) = resp.error {
        return if resp.is_timeout {
            write_timeout_error(call)
        } else {
            write_transport_error(call, &err_msg)
        };
    }
    call.ret_i64(0, resp.status_code);
    call.ret_str(1, &resp.status);
    call.ret_str(2, &resp.proto);
    call.ret_string_slice(3, &resp.headers);
    call.ret_bytes(4, &resp.body);
    write_nil_error(call, 5);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_transport_error(call: &mut ExternCallContext, msg: &str) -> ExternResult {
    call.ret_i64(0, 0);
    call.ret_str(1, "");
    call.ret_str(2, "");
    call.ret_nil(3);
    call.ret_nil(4);
    write_error_to(call, 5, msg);
    ExternResult::Ok
}

#[cfg(feature = "std")]
fn write_timeout_error(call: &mut ExternCallContext) -> ExternResult {
    call.ret_i64(0, 0);
    call.ret_str(1, "");
    call.ret_str(2, "");
    call.ret_nil(3);
    call.ret_nil(4);
    let (slot0, slot1) = http_sentinel_error(call, HttpErrorKind::Timeout);
    call.ret_u64(5, slot0);
    call.ret_u64(6, slot1);
    ExternResult::Ok
}

// ── Helpers ──────────────────────────────────────────────────────────

#[cfg(feature = "std")]
fn read_request_text(call: &ExternCallContext) -> Result<RequestText, String> {
    let method = crate::host_bytes::utf8_arg(call, 1, "HTTP method")?;
    validate_method(&method)?;
    let url = crate::host_bytes::utf8_arg(call, 2, "HTTP URL")?;
    let headers = crate::host_bytes::read_string_slice_bytes(call.arg_ref(3))
        .into_iter()
        .enumerate()
        .map(|(index, bytes)| {
            let line = crate::host_bytes::utf8_bytes(bytes, &format!("HTTP header {index}"))?;
            parse_header_line(&line)
                .map(|(name, value)| (name.to_owned(), value.to_owned()))
                .map_err(|error| format!("HTTP header {index}: {error}"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok((method, url, headers))
}

#[cfg(feature = "std")]
fn validate_method(method: &str) -> Result<(), String> {
    if method.is_empty() || !method.bytes().all(is_http_token_byte) {
        return Err("HTTP method must be a non-empty ASCII token".to_string());
    }
    Ok(())
}

#[cfg(feature = "std")]
fn parse_header_line(line: &str) -> Result<(&str, &str), String> {
    let Some(index) = line.find(':') else {
        return Err("missing ':' separator".to_string());
    };
    let name = &line[..index];
    if name.is_empty() || !name.bytes().all(is_http_token_byte) {
        return Err("field name must be a non-empty ASCII token".to_string());
    }
    let value = line[index + 1..].trim_matches([' ', '\t']);
    if !value.bytes().all(is_http_field_value_byte) {
        return Err("field value contains a non-ASCII or control byte".to_string());
    }
    Ok((name, value))
}

#[cfg(feature = "std")]
fn is_http_token_byte(byte: u8) -> bool {
    matches!(
        byte,
        b'!' | b'#'
            | b'$'
            | b'%'
            | b'&'
            | b'\''
            | b'*'
            | b'+'
            | b'-'
            | b'.'
            | b'^'
            | b'_'
            | b'`'
            | b'|'
            | b'~'
    ) || byte.is_ascii_alphanumeric()
}

#[cfg(feature = "std")]
fn is_http_field_value_byte(byte: u8) -> bool {
    matches!(byte, b'\t' | 0x20..=0x7e | 0x80..=0xff)
}

#[cfg(all(test, feature = "std"))]
mod tests {
    use super::*;

    #[test]
    fn request_text_protocol_rejects_malformed_methods_and_headers() {
        assert!(validate_method("GET").is_ok());
        assert!(validate_method("").is_err());
        assert!(validate_method("G ET").is_err());
        assert!(validate_method("GÉT").is_err());

        assert_eq!(
            parse_header_line("Content-Type: text/plain").unwrap(),
            ("Content-Type", "text/plain")
        );
        assert_eq!(parse_header_line("X-Empty:\t ").unwrap(), ("X-Empty", ""));
        assert!(parse_header_line("missing-separator").is_err());
        assert!(parse_header_line(": value").is_err());
        assert!(parse_header_line(" Bad: value").is_err());
        assert!(parse_header_line("Bad : value").is_err());
        assert!(parse_header_line("X-Test: line\r\nnext").is_err());
        assert_eq!(
            parse_header_line("X-Test: café").unwrap(),
            ("X-Test", "café")
        );
    }

    #[test]
    fn request_ids_exhaust_without_wrapping_or_reuse() {
        let counter = AtomicU64::new(i64::MAX as u64 - 1);
        assert_eq!(allocate_http_request_id(&counter), Some(i64::MAX - 1));
        assert_eq!(allocate_http_request_id(&counter), Some(i64::MAX));
        assert_eq!(allocate_http_request_id(&counter), None);
        assert_eq!(counter.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn vm_drop_cancels_and_removes_allocated_http_request() {
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let id = allocate_http_request(&mut runtime).expect("HTTP request handle");
        let control = request_state(id).expect("registered HTTP request").control;

        drop(runtime);

        assert!(request_state(id).is_none());
        assert!(control.is_canceled());
        let response = control.take_response().expect("cancellation response");
        assert_eq!(response.error.as_deref(), Some("request canceled"));
    }

    #[test]
    fn pre_transport_cancel_can_unregister_without_wait_state() {
        let mut runtime = IoRuntime::new().expect("test I/O runtime");
        let id = allocate_http_request(&mut runtime).expect("HTTP request handle");

        assert_eq!(cancel_http_request(id), HttpCancelResult::PreTransport);
        assert!(release_http_request(&mut runtime, id, None));
        assert!(request_state(id).is_none());
    }

    #[test]
    fn cancellation_and_worker_completion_publish_exactly_one_response() {
        let control = Arc::new(HttpRequestControl::new());
        let cancel = control.clone();
        let complete = control.clone();
        let canceler = std::thread::spawn(move || cancel.cancel_and_was_started());
        let worker = std::thread::spawn(move || {
            complete.complete_worker(HttpResponse {
                status_code: 200,
                status: String::from("HTTP/1.1 200 OK"),
                proto: String::from("HTTP/1.1"),
                headers: Vec::new(),
                body: b"ok".to_vec(),
                error: None,
                is_timeout: false,
            });
        });
        canceler.join().expect("cancel request");
        worker.join().expect("complete request");

        let response = control.take_response().expect("one terminal response");
        assert!(
            response.error.as_deref() == Some("request canceled") || response.status_code == 200
        );
        assert!(control.take_response().is_none());
    }

    #[test]
    fn consuming_a_cancellation_does_not_reopen_the_terminal_state() {
        let control = HttpRequestControl::new();
        control.cancel_and_was_started();
        assert_eq!(
            control.take_response().and_then(|response| response.error),
            Some(String::from("request canceled"))
        );

        control.complete_worker(HttpResponse {
            status_code: 200,
            status: String::from("HTTP/1.1 200 OK"),
            proto: String::from("HTTP/1.1"),
            headers: Vec::new(),
            body: b"late".to_vec(),
            error: None,
            is_timeout: false,
        });

        assert!(control.take_response().is_none());
    }

    #[cfg(unix)]
    #[test]
    fn late_worker_response_is_released_after_runtime_drop_without_sigpipe() {
        let (read_fd, write_fd) = create_http_wake_pipe().expect("HTTP wake pipe");
        let high_fd = unsafe { libc::fcntl(read_fd.as_raw_fd(), libc::F_DUPFD_CLOEXEC, 10_000) };
        assert!(
            high_fd >= 10_000,
            "failed to reserve high wake fd: {}",
            io::Error::last_os_error()
        );
        drop(read_fd);
        let read_fd = unsafe { OwnedFd::from_raw_fd(high_fd) };
        let worker_read_guard = read_fd
            .as_fd()
            .try_clone_to_owned()
            .expect("worker read guard");
        let cancellation = IoCancellation::new().expect("HTTP wake cancellation key");
        let lease = IoLease::from_owned(read_fd, cancellation).expect("owned wake lease");
        let mut runtime = vo_runtime::io::IoRuntime::new().expect("test I/O runtime");
        let token = runtime.submit_lease_owned_read(lease, 1);
        assert!(!runtime.has_completion(token));

        let response = Arc::new(HttpRequestControl::new());
        let weak_response = Arc::downgrade(&response);
        response
            .register_wake(HttpWake {
                write_fd,
                read_guard: worker_read_guard,
            })
            .expect("register HTTP wake");
        assert!(
            runtime.attach_to_token(token, response.clone()).is_ok(),
            "attach HTTP response state"
        );
        let worker_response = response.clone();
        drop(response);

        let (release_tx, release_rx) = std::sync::mpsc::channel();
        let worker = std::thread::spawn(move || {
            release_rx.recv().expect("release late worker");
            worker_response.complete_worker(HttpResponse {
                status_code: 200,
                status: String::from("HTTP/1.1 200 OK"),
                proto: String::from("HTTP/1.1"),
                headers: Vec::new(),
                body: b"late".to_vec(),
                error: None,
                is_timeout: false,
            });
        });

        drop(runtime);
        assert_eq!(
            unsafe { libc::fcntl(high_fd, libc::F_GETFD) },
            -1,
            "runtime drop must close its owned read fd"
        );
        release_tx.send(()).expect("release worker");
        worker.join().expect("late HTTP worker");
        assert!(
            weak_response.upgrade().is_none(),
            "late HTTP response state outlived both runtime and worker"
        );
    }
}

// ── Registration ─────────────────────────────────────────────────────

#[cfg(feature = "std")]
#[doc(hidden)]
pub const REGISTERED_EXTERNS: &[StdlibEntry] = &[
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net/http", "getHttpErrors"),
        func: get_http_errors,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net/http", "nativeNewClientRequest"),
        func: http_native_new_client_request,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net/http", "nativeCancelClientRequest"),
        func: http_native_cancel_client_request,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net/http", "nativeReleaseClientRequest"),
        func: http_native_release_client_request,
        effects: vo_runtime::bytecode::ExternEffects::NONE,
    },
    StdlibEntry {
        name: vo_runtime::vo_extern_name!("net/http", "nativeHttpsRequest"),
        func: http_native_https_request,
        effects: vo_runtime::bytecode::ExternEffects::MAY_WAIT_IO_REPLAY,
    },
];

#[cfg(feature = "std")]
pub fn register_externs(
    registry: &mut ExternRegistry,
    externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    for (id, def) in externs.iter().enumerate() {
        for entry in REGISTERED_EXTERNS {
            if def.name == entry.name() {
                entry.try_register(registry, id as u32)?;
                break;
            }
        }
    }
    Ok(())
}

#[cfg(not(feature = "std"))]
pub fn register_externs(
    _registry: &mut ExternRegistry,
    _externs: &[vo_runtime::bytecode::ExternDef],
) -> Result<(), vo_runtime::ffi::ExternContractError> {
    // No-op
    Ok(())
}
