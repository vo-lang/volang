use alloc::vec::Vec;

/// A keep-latest buffer for render output bytes.
///
/// Each `push` overwrites any previously buffered frame. `poll` returns and
/// clears the latest frame, giving the consumer the most recent render state
/// without accumulating stale frames.
#[derive(Debug, Default, Clone)]
pub struct RenderBuffer {
    latest: Option<Vec<u8>>,
}

impl RenderBuffer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, bytes: Vec<u8>) {
        if !bytes.is_empty() {
            self.latest = Some(bytes);
        }
    }

    pub fn poll(&mut self) -> Option<Vec<u8>> {
        self.latest.take()
    }

    pub fn has_pending(&self) -> bool {
        self.latest.is_some()
    }
}

/// Thread-safe variant of [`RenderBuffer`] for cross-thread render output.
///
/// Uses a `Mutex` internally, suitable for sharing between a guest VM thread
/// and a host render thread.
#[cfg(feature = "std")]
pub struct SyncRenderBuffer {
    inner: std::sync::Mutex<RenderBuffer>,
}

#[cfg(feature = "std")]
impl Default for SyncRenderBuffer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "std")]
impl SyncRenderBuffer {
    pub fn new() -> Self {
        Self {
            inner: std::sync::Mutex::new(RenderBuffer::new()),
        }
    }

    pub fn push(&self, bytes: Vec<u8>) {
        self.lock().push(bytes);
    }

    pub fn poll(&self) -> Option<Vec<u8>> {
        self.lock().poll()
    }

    pub fn has_pending(&self) -> bool {
        self.lock().has_pending()
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, RenderBuffer> {
        match self.inner.lock() {
            Ok(guard) => guard,
            Err(poisoned) => {
                self.inner.clear_poison();
                poisoned.into_inner()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::RenderBuffer;
    #[cfg(feature = "std")]
    use super::SyncRenderBuffer;

    #[test]
    fn push_overwrites_previous_frame() {
        let mut buf = RenderBuffer::new();

        buf.push(vec![1, 2]);
        buf.push(vec![3, 4, 5]);

        assert_eq!(buf.poll(), Some(vec![3, 4, 5]));
        assert_eq!(buf.poll(), None);
    }

    #[test]
    fn push_ignores_empty_bytes() {
        let mut buf = RenderBuffer::new();

        buf.push(vec![1]);
        buf.push(vec![]);

        assert_eq!(buf.poll(), Some(vec![1]));
    }

    #[test]
    fn poll_returns_none_when_empty() {
        let mut buf = RenderBuffer::new();

        assert_eq!(buf.poll(), None);
        assert!(!buf.has_pending());
    }

    #[cfg(feature = "std")]
    #[test]
    fn sync_buffer_recovers_after_a_writer_panics_while_holding_the_lock() {
        let buffer = SyncRenderBuffer::new();
        let result = std::panic::catch_unwind(|| {
            let _guard = buffer.inner.lock().expect("initial lock");
            panic!("poison render buffer lock");
        });
        assert!(result.is_err());

        buffer.push(vec![7, 8, 9]);
        assert!(!buffer.inner.is_poisoned());
        assert!(buffer.has_pending());
        assert_eq!(buffer.poll(), Some(vec![7, 8, 9]));
        assert!(!buffer.has_pending());
    }
}
