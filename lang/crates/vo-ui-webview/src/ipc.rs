use base64::{engine::general_purpose::STANDARD, Engine as _};
use serde::Deserialize;

const LIMIT: usize = vo_ui_bridge::MAX_FRAME_BYTES;
pub(crate) const MAX_IPC_BYTES: usize = LIMIT.div_ceil(3) * 4 + 1024;
const MAX_SEQUENCE: u64 = 9_007_199_254_740_991;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Envelope {
    token: String,
    message: Message,
}

#[derive(Deserialize)]
#[serde(tag = "kind", rename_all = "kebab-case", deny_unknown_fields)]
enum Message {
    Start,
    Reply { id: u64, data: String },
    Failure { message: String },
}

#[derive(Debug)]
pub(crate) enum Action {
    Start,
    Reply(Vec<u8>),
    Failure(String),
}

pub(crate) struct Bridge {
    token: String,
    started: bool,
    serial: u64,
    pending: Option<u64>,
}

impl Bridge {
    pub fn new(token: String) -> Self {
        Self {
            token,
            started: false,
            serial: 0,
            pending: None,
        }
    }

    pub fn receive(&mut self, text: &str) -> Result<Action, String> {
        if text.len() > MAX_IPC_BYTES {
            return Err("desktop IPC exceeds byte limit".into());
        }
        let envelope: Envelope =
            serde_json::from_str(text).map_err(|error| format!("invalid desktop IPC: {error}"))?;
        if envelope.token != self.token {
            return Err("desktop IPC belongs to another document".into());
        }
        match envelope.message {
            Message::Start => {
                if self.started {
                    return Err("duplicate desktop start".into());
                }
                self.started = true;
                Ok(Action::Start)
            }
            Message::Reply { id, data } => {
                if self.pending != Some(id) {
                    return Err("stale desktop response".into());
                }
                let bytes = STANDARD
                    .decode(data)
                    .map_err(|_| "invalid desktop response encoding")?;
                if bytes.len() > LIMIT {
                    return Err("desktop response exceeds frame limit".into());
                }
                self.pending = None;
                Ok(Action::Reply(bytes))
            }
            Message::Failure { message } => {
                if message.len() > 16 * 1024 {
                    return Err("desktop diagnostic exceeds byte limit".into());
                }
                Ok(Action::Failure(message))
            }
        }
    }

    pub fn exchange(&mut self, bytes: &[u8]) -> Result<String, String> {
        if !self.started || self.pending.is_some() {
            return Err("unexpected desktop exchange".into());
        }
        if bytes.len() > LIMIT {
            return Err("desktop output exceeds frame limit".into());
        }
        let id = self
            .serial
            .checked_add(1)
            .filter(|id| *id <= MAX_SEQUENCE)
            .ok_or("desktop request identities exhausted")?;
        let data =
            serde_json::to_string(&STANDARD.encode(bytes)).map_err(|error| error.to_string())?;
        self.serial = id;
        self.pending = Some(id);
        Ok(format!("window.__volangDesktop.receive({id},{data})"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn message(token: &str, message: serde_json::Value) -> String {
        serde_json::json!({"token":token,"message":message}).to_string()
    }
    #[test]
    fn exact_document_and_sequence_preserve_pending_after_rejection() {
        let mut bridge = Bridge::new("window-a".into());
        assert!(bridge.exchange(&[]).is_err());
        assert!(bridge
            .receive(&message("window-b", serde_json::json!({"kind":"start"})))
            .is_err());
        assert!(matches!(
            bridge
                .receive(&message("window-a", serde_json::json!({"kind":"start"})))
                .unwrap(),
            Action::Start
        ));
        assert!(bridge
            .receive(&message("window-a", serde_json::json!({"kind":"start"})))
            .is_err());
        assert!(bridge
            .exchange(&[0, 255, 42])
            .unwrap()
            .contains("receive(1,\"AP8q\")"));
        assert!(bridge.exchange(&[]).is_err());
        for message in [
            serde_json::json!({"kind":"reply","id":2,"data":""}),
            serde_json::json!({"kind":"reply","id":1,"data":"?"}),
            serde_json::json!({"kind":"reply","id":1,"data":"","extra":1}),
        ] {
            assert!(bridge.receive(&self::message("window-a", message)).is_err());
            assert_eq!(bridge.pending, Some(1));
        }
        let reply = message(
            "window-a",
            serde_json::json!({"kind":"reply","id":1,"data":"AP8q"}),
        );
        assert!(
            matches!(bridge.receive(&reply).unwrap(), Action::Reply(bytes) if bytes == [0,255,42])
        );
        assert!(bridge.receive(&reply).is_err());
        bridge.serial = MAX_SEQUENCE;
        assert!(bridge.exchange(&[]).is_err());
    }
    #[test]
    fn oversized_envelopes_and_diagnostics_are_rejected() {
        let mut bridge = Bridge::new("window-a".into());
        assert!(bridge.receive(&" ".repeat(MAX_IPC_BYTES + 1)).is_err());
        assert!(bridge
            .receive(&message(
                "window-a",
                serde_json::json!({"kind":"failure","message":"x".repeat(16*1024+1)})
            ))
            .is_err());
        assert!(
            matches!(bridge.receive(&message("window-a",serde_json::json!({"kind":"failure","message":"中文"}))).unwrap(),Action::Failure(message) if message == "中文")
        );
    }
}
