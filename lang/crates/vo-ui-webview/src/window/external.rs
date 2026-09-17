use std::sync::{
    atomic::{AtomicBool, Ordering},
    mpsc, Arc,
};

const MAX_URL_BYTES: usize = 8192;
const QUEUED_LINKS: usize = 8;

pub(super) fn external_url(value: &str) -> Option<String> {
    if value.len() > MAX_URL_BYTES || value.chars().any(char::is_control) {
        return None;
    }
    let url = url::Url::parse(value).ok()?;
    if !matches!(url.scheme(), "http" | "https") || url.host_str().is_none() {
        return None;
    }
    let canonical = url.to_string();
    (canonical.len() <= MAX_URL_BYTES).then_some(canonical)
}

/// One bounded launcher per window. Browser startup never blocks the GUI, and
/// closing the window discards requests that have not begun. The worker waits
/// for Unix launcher children so repeated links do not leave zombie processes.
pub(super) struct ExternalLinks {
    sender: mpsc::SyncSender<String>,
    closed: Arc<AtomicBool>,
}

impl ExternalLinks {
    pub fn new(
        handler: impl FnMut(&str) -> Result<(), String> + Send + 'static,
    ) -> Result<Self, String> {
        let (links, worker) = Self::prepare(handler);
        std::thread::Builder::new()
            .name("vo-ui-browser".into())
            .spawn(worker)
            .map_err(|error| error.to_string())?;
        Ok(links)
    }

    fn prepare(
        mut handler: impl FnMut(&str) -> Result<(), String> + Send + 'static,
    ) -> (Self, impl FnOnce() + Send + 'static) {
        let (sender, receiver) = mpsc::sync_channel::<String>(QUEUED_LINKS);
        let closed = Arc::new(AtomicBool::new(false));
        let cancelled = Arc::clone(&closed);
        let worker = move || {
            while let Ok(url) = receiver.recv() {
                if cancelled.load(Ordering::Acquire) {
                    break;
                }
                if let Err(error) = handler(&url) {
                    eprintln!("Cannot open external link: {error}");
                }
            }
        };
        (Self { sender, closed }, worker)
    }

    pub fn open(&self, value: &str) {
        if let Some(url) = external_url(value) {
            if self.sender.try_send(url).is_err() {
                eprintln!("External browser request queue is full or closed");
            }
        }
    }
}

impl Drop for ExternalLinks {
    fn drop(&mut self) {
        self.closed.store(true, Ordering::Release);
    }
}

pub(super) fn open_browser(url: &str) -> Result<(), String> {
    #[cfg(target_os = "windows")]
    {
        use std::os::windows::ffi::OsStrExt;
        use windows_sys::Win32::System::Com::{
            CoInitializeEx, CoUninitialize, COINIT_APARTMENTTHREADED, COINIT_DISABLE_OLE1DDE,
        };
        use windows_sys::Win32::UI::Shell::ShellExecuteW;
        let url: Vec<u16> = std::ffi::OsStr::new(url)
            .encode_wide()
            .chain(Some(0))
            .collect();
        // Shell extensions can require an STA. This worker owns its COM
        // initialization; successful calls (including S_FALSE) are balanced.
        let initialized = unsafe {
            CoInitializeEx(
                std::ptr::null(),
                (COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE) as u32,
            )
        };
        if initialized < 0 {
            return Err(format!(
                "system browser COM initialization failed ({initialized:#x})"
            ));
        }
        // The URL is one null-terminated argument; no command shell interprets it.
        let result = unsafe {
            let result = ShellExecuteW(
                std::ptr::null_mut(),
                std::ptr::null(),
                url.as_ptr(),
                std::ptr::null(),
                std::ptr::null(),
                1,
            ) as isize;
            CoUninitialize();
            result
        };
        if result <= 32 {
            return Err(format!("system browser launch failed ({result})"));
        }
        Ok(())
    }
    #[cfg(not(target_os = "windows"))]
    {
        use std::process::{Command, Stdio};
        let launcher = if cfg!(target_os = "macos") {
            "open"
        } else {
            "xdg-open"
        };
        let result = Command::new(launcher)
            .arg(url)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map_err(|error| format!("{launcher}: {error}"))?;
        if !result.success() {
            return Err(format!("{launcher} exited with {result}"));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_bounded_web_urls_reach_the_platform() {
        assert_eq!(
            external_url("https://example.com/a?text=中文&x=1#two"),
            Some("https://example.com/a?text=%E4%B8%AD%E6%96%87&x=1#two".into())
        );
        for url in [
            "file:///tmp/a",
            "javascript:alert(1)",
            "volang://localhost/",
            "--help",
            "/local",
            "https://example.com/\nnext",
            "mailto:person@example.com",
        ] {
            assert_eq!(external_url(url), None, "{url}");
        }
        assert!(external_url(&format!(
            "https://example.com/{}",
            "x".repeat(MAX_URL_BYTES)
        ))
        .is_none());
        // Percent encoding must not expand an accepted input past the launcher
        // budget. Unicode links within that budget are preserved above.
        assert!(external_url(&format!("https://example.com/{}", "文".repeat(1000))).is_none());
    }

    #[test]
    fn queued_links_are_bounded_ordered_and_cancelled_with_the_window() {
        let (sent, received) = mpsc::channel();
        let (links, worker) = ExternalLinks::prepare(move |url| {
            sent.send(url.to_owned()).unwrap();
            Ok(())
        });
        for index in 0..QUEUED_LINKS {
            links
                .sender
                .try_send(format!("https://example.com/{index}"))
                .unwrap();
        }
        assert!(matches!(
            links.sender.try_send("overflow".into()),
            Err(mpsc::TrySendError::Full(_))
        ));
        let thread = std::thread::spawn(worker);
        for index in 0..QUEUED_LINKS {
            assert_eq!(
                received
                    .recv_timeout(std::time::Duration::from_secs(5))
                    .unwrap(),
                format!("https://example.com/{index}")
            );
        }
        drop(links);
        thread.join().unwrap();
        let (links, worker) = ExternalLinks::prepare(|_| panic!("closed window launched a link"));
        links.open("https://example.com/");
        drop(links);
        worker();
    }
}
