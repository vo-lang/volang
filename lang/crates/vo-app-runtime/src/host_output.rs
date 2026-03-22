use alloc::string::String;

use vo_runtime::output::CaptureSink;

pub fn take_captured_stdout(sink: &CaptureSink) -> Option<String> {
    let stdout = sink.take();
    if stdout.is_empty() {
        None
    } else {
        Some(stdout)
    }
}

#[cfg(test)]
mod tests {
    use super::take_captured_stdout;
    use vo_runtime::output::{CaptureSink, OutputSink};

    #[test]
    fn take_captured_stdout_returns_none_for_empty_capture() {
        let sink = CaptureSink::new();

        let stdout = take_captured_stdout(sink.as_ref());

        assert_eq!(stdout, None);
    }

    #[test]
    fn take_captured_stdout_returns_and_clears_capture() {
        let sink = CaptureSink::new();
        sink.write("hello");

        let stdout = take_captured_stdout(sink.as_ref());

        assert_eq!(stdout, Some("hello".to_string()));
        assert_eq!(take_captured_stdout(sink.as_ref()), None);
    }
}
