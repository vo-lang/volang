//! Immutable bundled resources, including native media byte-range requests.
use crate::Assets;
use std::{borrow::Cow, ops::Range};
use wry::http::{Request, Response};

pub(super) fn respond(assets: &Assets, request: &Request<Vec<u8>>) -> Response<Cow<'static, [u8]>> {
    let method = request.method().as_str();
    let supported = matches!(method, "GET" | "HEAD");
    let asset = supported
        .then(|| assets.get(request.uri().path()))
        .flatten();
    let (mut status, mime, mut bytes) = match asset {
        Some(asset) => (200, asset.media_type.content_type(), asset.bytes.as_ref()),
        None => (
            if supported { 404 } else { 405 },
            "text/plain; charset=utf-8",
            b"Unavailable".as_slice(),
        ),
    };
    let mut response = Response::builder()
        .header("Content-Type", mime)
        .header("X-Content-Type-Options", "nosniff")
        .header("Cache-Control", "no-store");
    if asset.is_some() {
        response = response.header("Accept-Ranges", "bytes");
        // No validators are issued for these process-owned resources. A
        // conditional range therefore needs the complete current resource.
        if method == "GET" && !request.headers().contains_key("If-Range") {
            let mut headers = request.headers().get_all("Range").iter();
            let range = headers.next().and_then(|value| value.to_str().ok());
            if headers.next().is_none() {
                match range.map(|value| byte_range(value, bytes.len())) {
                    Some(ByteRange::Partial(range)) => {
                        status = 206;
                        response = response.header(
                            "Content-Range",
                            format!("bytes {}-{}/{}", range.start, range.end - 1, bytes.len()),
                        );
                        bytes = &bytes[range];
                    }
                    Some(ByteRange::Unsatisfiable) => {
                        status = 416;
                        response =
                            response.header("Content-Range", format!("bytes */{}", bytes.len()));
                        bytes = &[];
                    }
                    _ => {}
                }
            }
        }
    } else if !supported {
        response = response.header("Allow", "GET, HEAD");
    }
    response
        .status(status)
        .header("Content-Length", bytes.len())
        .body(Cow::Owned(if method == "HEAD" {
            vec![]
        } else {
            bytes.to_vec()
        }))
        .unwrap()
}

enum ByteRange {
    Full,
    Partial(Range<usize>),
    Unsatisfiable,
}

// RFC 9110 §14: support one byte range; ignore malformed, unknown or multipart
// ranges. Decimal saturation handles arbitrarily large offsets without overflow.
fn byte_range(header: &str, length: usize) -> ByteRange {
    let Some((unit, value)) = header.trim().split_once('=') else {
        return ByteRange::Full;
    };
    if !unit.eq_ignore_ascii_case("bytes") {
        return ByteRange::Full;
    }
    let Some((first, last)) = value.split_once('-') else {
        return ByteRange::Full;
    };
    let decimal = |value: &str| -> Option<usize> {
        if value.is_empty() || !value.bytes().all(|byte| byte.is_ascii_digit()) {
            return None;
        }
        Some(value.bytes().fold(0usize, |n, digit| {
            n.saturating_mul(10).saturating_add((digit - b'0') as usize)
        }))
    };
    if first.is_empty() {
        let Some(suffix) = decimal(last) else {
            return ByteRange::Full;
        };
        return if suffix == 0 || length == 0 {
            ByteRange::Unsatisfiable
        } else {
            ByteRange::Partial(length.saturating_sub(suffix)..length)
        };
    }
    let Some(start) = decimal(first) else {
        return ByteRange::Full;
    };
    let end = if last.is_empty() {
        usize::MAX
    } else {
        let Some(last) = decimal(last) else {
            return ByteRange::Full;
        };
        if last < start {
            return ByteRange::Full;
        }
        last.saturating_add(1)
    };
    if start >= length {
        ByteRange::Unsatisfiable
    } else {
        ByteRange::Partial(start..end.min(length))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Asset, MediaType, BOOTSTRAP_MARKER};

    fn assets() -> Assets {
        Assets::new(
            BOOTSTRAP_MARKER.into(),
            b"host".as_slice(),
            [
                (
                    "/sound.wav".into(),
                    Asset::new(MediaType::Wav, b"0123456789".as_slice()),
                ),
                (
                    "/empty".into(),
                    Asset::new(MediaType::Binary, b"".as_slice()),
                ),
            ],
        )
        .unwrap()
    }

    fn request(method: &str, path: &str, range: Option<&str>) -> Request<Vec<u8>> {
        let mut request = Request::builder().method(method).uri(path);
        if let Some(range) = range {
            request = request.header("Range", range);
        }
        request.body(vec![]).unwrap()
    }

    #[test]
    fn media_ranges_return_exact_bytes_and_total_length() {
        let assets = assets();
        for (range, expected, body) in [
            ("bytes=0-1", "bytes 0-1/10", "01"),
            ("bytes=4-6", "bytes 4-6/10", "456"),
            ("bytes=8-", "bytes 8-9/10", "89"),
            ("bytes=-3", "bytes 7-9/10", "789"),
            ("bytes=-99", "bytes 0-9/10", "0123456789"),
            ("BYTES=9-9999999999999999999999999", "bytes 9-9/10", "9"),
        ] {
            let response = respond(&assets, &request("GET", "/sound.wav", Some(range)));
            assert_eq!(response.status(), 206, "{range}");
            assert_eq!(response.headers()["Content-Range"], expected);
            assert_eq!(response.headers()["Content-Length"], body.len().to_string());
            assert_eq!(response.headers()["Accept-Ranges"], "bytes");
            assert_eq!(response.body().as_ref(), body.as_bytes());
        }
    }

    #[test]
    fn unsatisfiable_ranges_are_empty_and_identify_the_resource_length() {
        let assets = assets();
        for (path, range, length) in [
            ("/sound.wav", "bytes=10-", 10),
            ("/sound.wav", "bytes=-0", 10),
            ("/sound.wav", "bytes=999999999999999999999999-", 10),
            ("/empty", "bytes=0-", 0),
            ("/empty", "bytes=-1", 0),
        ] {
            let response = respond(&assets, &request("GET", path, Some(range)));
            assert_eq!(response.status(), 416);
            assert_eq!(
                response.headers()["Content-Range"],
                format!("bytes */{length}")
            );
            assert_eq!(response.headers()["Content-Length"], "0");
            assert!(response.body().is_empty());
        }
    }

    #[test]
    fn unsupported_ranges_keep_the_complete_resource() {
        let assets = assets();
        for range in [
            None,
            Some("items=0-1"),
            Some("bytes=3-2"),
            Some("bytes=-"),
            Some("bytes=0-1,4-5"),
            Some("bytes=+0-1"),
            Some("bytes=0-x"),
        ] {
            let response = respond(&assets, &request("GET", "/sound.wav", range));
            assert_eq!(response.status(), 200);
            assert_eq!(response.body().as_ref(), b"0123456789");
            assert!(!response.headers().contains_key("Content-Range"));
        }
        let mut conditional = request("GET", "/sound.wav", Some("bytes=0-1"));
        conditional
            .headers_mut()
            .insert("If-Range", "\"old\"".parse().unwrap());
        assert_eq!(respond(&assets, &conditional).status(), 200);
        conditional.headers_mut().remove("If-Range");
        conditional
            .headers_mut()
            .append("Range", "bytes=4-5".parse().unwrap());
        assert_eq!(respond(&assets, &conditional).status(), 200);
    }

    #[test]
    fn head_and_errors_keep_method_semantics() {
        let assets = assets();
        let response = respond(&assets, &request("HEAD", "/sound.wav", Some("bytes=0-1")));
        assert_eq!(response.status(), 200);
        assert_eq!(response.headers()["Content-Length"], "10");
        assert!(response.body().is_empty());
        assert!(!response.headers().contains_key("Content-Range"));
        let response = respond(&assets, &request("GET", "/missing", Some("bytes=0-1")));
        assert_eq!(response.status(), 404);
        assert!(!response.headers().contains_key("Accept-Ranges"));
        let response = respond(&assets, &request("POST", "/sound.wav", None));
        assert_eq!(response.status(), 405);
        assert_eq!(response.headers()["Allow"], "GET, HEAD");
    }
}
