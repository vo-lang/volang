use alloc::vec::Vec;
use core::fmt;

use crate::SessionError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SessionDispatchError<E> {
    Session(SessionError),
    Host(E),
}

impl<E> SessionDispatchError<E> {
    pub fn is_not_waiting_for_events(&self) -> bool {
        matches!(self, Self::Session(SessionError::NotWaitingForEvents))
    }
}

impl<E> From<SessionError> for SessionDispatchError<E> {
    fn from(error: SessionError) -> Self {
        Self::Session(error)
    }
}

impl<E> fmt::Display for SessionDispatchError<E>
where
    E: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Session(error) => error.fmt(f),
            Self::Host(error) => error.fmt(f),
        }
    }
}

pub fn emit_outbound_frames<E, F>(frames: Vec<Vec<u8>>, mut emit: F) -> Result<(), E>
where
    F: FnMut(Vec<u8>) -> Result<(), E>,
{
    for frame in frames {
        emit(frame)?;
    }
    Ok(())
}

pub fn ignore_not_waiting_for_events<T, E>(
    result: Result<T, SessionDispatchError<E>>,
) -> Result<Option<T>, SessionDispatchError<E>> {
    match result {
        Ok(value) => Ok(Some(value)),
        Err(error) if error.is_not_waiting_for_events() => Ok(None),
        Err(error) => Err(error),
    }
}

pub fn emit_trimmed_stdout<F>(stdout: Option<&str>, emit: F)
where
    F: FnOnce(&str),
{
    let Some(stdout) = stdout else {
        return;
    };
    emit(stdout.trim_end());
}

#[cfg(test)]
mod tests {
    use alloc::string::String;
    use alloc::vec;
    use alloc::vec::Vec;

    use super::{
        emit_outbound_frames, emit_trimmed_stdout, ignore_not_waiting_for_events,
        SessionDispatchError,
    };
    use crate::SessionError;

    #[test]
    fn session_dispatch_error_reports_not_waiting_for_events() {
        let error = SessionDispatchError::<String>::Session(SessionError::NotWaitingForEvents);

        assert!(error.is_not_waiting_for_events());
    }

    #[test]
    fn emit_outbound_frames_preserves_order() {
        let mut seen = Vec::new();

        let result: Result<(), String> = emit_outbound_frames(vec![vec![1], vec![2, 3]], |frame| {
            seen.push(frame);
            Ok(())
        });

        assert_eq!(result, Ok(()));
        assert_eq!(seen, vec![vec![1], vec![2, 3]]);
    }

    #[test]
    fn emit_outbound_frames_stops_on_first_host_error() {
        let mut seen = Vec::new();

        let result: Result<(), String> =
            emit_outbound_frames(vec![vec![1], vec![2], vec![3]], |frame| {
                seen.push(frame.clone());
                if frame == vec![2] {
                    Err(String::from("boom"))
                } else {
                    Ok(())
                }
            });

        assert_eq!(result, Err(String::from("boom")));
        assert_eq!(seen, vec![vec![1], vec![2]]);
    }

    #[test]
    fn ignore_not_waiting_for_events_returns_none_for_not_waiting_error() {
        let result = ignore_not_waiting_for_events::<Vec<u8>, String>(Err(
            SessionDispatchError::Session(SessionError::NotWaitingForEvents),
        ));

        assert_eq!(result, Ok(None));
    }

    #[test]
    fn ignore_not_waiting_for_events_preserves_values_and_other_errors() {
        let ok = ignore_not_waiting_for_events::<Vec<u8>, String>(Ok(vec![1, 2, 3]));
        let err = ignore_not_waiting_for_events::<Vec<u8>, String>(Err(
            SessionDispatchError::Host(String::from("boom")),
        ));

        assert_eq!(ok, Ok(Some(vec![1, 2, 3])));
        assert_eq!(err, Err(SessionDispatchError::Host(String::from("boom"))));
    }

    #[test]
    fn emit_trimmed_stdout_ignores_missing_stdout() {
        let mut called = false;

        emit_trimmed_stdout(None, |_| {
            called = true;
        });

        assert!(!called);
    }

    #[test]
    fn emit_trimmed_stdout_trims_before_emitting() {
        let mut seen = String::new();

        emit_trimmed_stdout(Some("hello\n"), |stdout| {
            seen = stdout.into();
        });

        assert_eq!(seen, "hello");
    }
}
