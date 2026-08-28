use wasm_bindgen::prelude::*;

const MAX_INPUT_BYTES: usize = 64 * 1024 * 1024;
const MAX_RESPONSE_BYTES: usize = 64 * 1024 * 1024;
const MAX_RESULTS: usize = 1_000_000;
const RESPONSE_HEADER_BYTES: usize = 36;
const RESPONSE_MAGIC: &[u8; 8] = b"VOREG001";

fn push_u32(output: &mut Vec<u8>, value: usize) -> Option<()> {
    output.extend_from_slice(&u32::try_from(value).ok()?.to_le_bytes());
    Some(())
}

/// Execute one regexp operation using the exact semantic core shared with the
/// VM backend. The compact private response format is decoded by vo-web; it is
/// versioned independently from the public Volang AOT ABI.
#[wasm_bindgen(js_name = voAotRegexp)]
pub fn vo_aot_regexp(
    operation: u8,
    pattern: &[u8],
    input: &[u8],
    replacement: &[u8],
    n: i64,
) -> Result<Vec<u8>, JsValue> {
    let total = pattern
        .len()
        .checked_add(input.len())
        .and_then(|value| value.checked_add(replacement.len()))
        .ok_or_else(|| JsValue::from_str("regexp request length overflows"))?;
    if total > MAX_INPUT_BYTES {
        return Err(JsValue::from_str("regexp request exceeds 64 MiB"));
    }
    let operation = vo_stdlib::regexp::AotRegexpOperation::from_u8(operation)
        .ok_or_else(|| JsValue::from_str("unknown regexp support operation"))?;
    let response =
        vo_stdlib::regexp::aot_regexp_operation(operation, pattern, input, replacement, n);

    let bytes_payload = response.bytes.iter().try_fold(0usize, |total, item| {
        total.checked_add(4)?.checked_add(item.len())
    });
    let integers_payload = response.integers.len().checked_mul(8);
    let capacity = bytes_payload
        .and_then(|bytes| {
            integers_payload.and_then(|ints| {
                bytes
                    .checked_add(ints)
                    .and_then(|payload| RESPONSE_HEADER_BYTES.checked_add(payload))
            })
        })
        .ok_or_else(|| JsValue::from_str("regexp response length overflows"))?;
    if response.bytes.len() > MAX_RESULTS || response.integers.len() > MAX_RESULTS {
        return Err(JsValue::from_str("regexp response has too many results"));
    }
    if capacity > MAX_RESPONSE_BYTES {
        return Err(JsValue::from_str("regexp response exceeds 64 MiB"));
    }
    let mut output = Vec::with_capacity(capacity);
    output.extend_from_slice(RESPONSE_MAGIC);
    output.extend_from_slice(&u32::from(response.valid).to_le_bytes());
    output.extend_from_slice(&response.scalar0.to_le_bytes());
    output.extend_from_slice(&response.scalar1.to_le_bytes());
    push_u32(&mut output, response.bytes.len())
        .ok_or_else(|| JsValue::from_str("regexp byte result count exceeds u32"))?;
    push_u32(&mut output, response.integers.len())
        .ok_or_else(|| JsValue::from_str("regexp integer result count exceeds u32"))?;
    for item in response.bytes {
        push_u32(&mut output, item.len())
            .ok_or_else(|| JsValue::from_str("regexp byte result length exceeds u32"))?;
        output.extend_from_slice(&item);
    }
    for value in response.integers {
        output.extend_from_slice(&value.to_le_bytes());
    }
    Ok(output)
}
