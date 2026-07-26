use alloc::collections::VecDeque;
use alloc::vec::Vec;

const MAX_GAME_RENDER_COMMANDS: usize = 4096;
const MAX_GAME_RENDER_BYTES: usize = 1024 * 1024 * 1024;

/// A keep-latest buffer for render output bytes.
///
/// Each `push` overwrites any previously buffered frame. `poll` returns and
/// clears the latest frame, giving the consumer the most recent render state
/// without accumulating stale frames.
#[derive(Debug, Default, Clone)]
pub struct RenderBuffer {
    latest: Option<Vec<u8>>,
    game: VecDeque<Vec<u8>>,
    game_bytes: usize,
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

    pub fn push_game(&mut self, bytes: Vec<u8>) -> bool {
        if bytes.is_empty() {
            return true;
        }
        let Some(next_bytes) = self.game_bytes.checked_add(bytes.len()) else {
            return false;
        };
        if self.game.len() == MAX_GAME_RENDER_COMMANDS || next_bytes > MAX_GAME_RENDER_BYTES {
            return false;
        }
        self.game_bytes = next_bytes;
        self.game.push_back(bytes);
        true
    }

    pub fn poll(&mut self) -> Option<Vec<u8>> {
        self.latest.take()
    }

    pub fn poll_game(&mut self) -> Option<Vec<u8>> {
        let command = self.game.pop_front()?;
        self.game_bytes -= command.len();
        Some(command)
    }

    pub fn has_pending(&self) -> bool {
        self.latest.is_some()
    }

    pub fn has_pending_game(&self) -> bool {
        !self.game.is_empty()
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

    pub fn push_game(&self, bytes: Vec<u8>) -> bool {
        self.lock().push_game(bytes)
    }

    pub fn poll(&self) -> Option<Vec<u8>> {
        self.lock().poll()
    }

    pub fn poll_game(&self) -> Option<Vec<u8>> {
        self.lock().poll_game()
    }

    pub fn has_pending(&self) -> bool {
        self.lock().has_pending()
    }

    pub fn has_pending_game(&self) -> bool {
        self.lock().has_pending_game()
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
