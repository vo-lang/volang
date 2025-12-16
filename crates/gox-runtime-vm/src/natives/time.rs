//! Native implementations for the time package.

use gox_vm::native::{NativeCtx, NativeResult, NativeRegistry};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use std::thread;

pub fn register(registry: &mut NativeRegistry) {
    registry.register("time.Now", native_now);
    registry.register("time.Sleep", native_sleep);
    registry.register("time.Since", native_since);
    registry.register("time.Unix", native_unix);
    registry.register("time.UnixMilli", native_unix_milli);
    registry.register("time.ParseDuration", native_parse_duration);
}

fn native_now(ctx: &mut NativeCtx) -> NativeResult {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::ZERO);
    ctx.ret_i64(0, now.as_nanos() as i64);
    NativeResult::Ok(1)
}

fn native_sleep(ctx: &mut NativeCtx) -> NativeResult {
    let nanos = ctx.arg_i64(0);
    if nanos > 0 {
        thread::sleep(Duration::from_nanos(nanos as u64));
    }
    NativeResult::Ok(0)
}

fn native_since(ctx: &mut NativeCtx) -> NativeResult {
    let start = ctx.arg_i64(0);
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::ZERO);
    let now_nanos = now.as_nanos() as i64;
    ctx.ret_i64(0, now_nanos - start);
    NativeResult::Ok(1)
}

fn native_unix(ctx: &mut NativeCtx) -> NativeResult {
    let sec = ctx.arg_i64(0);
    let nsec = ctx.arg_i64(1);
    ctx.ret_i64(0, sec * 1_000_000_000 + nsec);
    NativeResult::Ok(1)
}

fn native_unix_milli(ctx: &mut NativeCtx) -> NativeResult {
    let msec = ctx.arg_i64(0);
    ctx.ret_i64(0, msec * 1_000_000);
    NativeResult::Ok(1)
}

fn native_parse_duration(ctx: &mut NativeCtx) -> NativeResult {
    let s = ctx.arg_str(0).to_string();
    
    // Parse duration string like "1h30m", "100ms", "2.5s"
    let result = parse_duration_str(&s);
    ctx.ret_i64(0, result);
    NativeResult::Ok(1)
}

fn parse_duration_str(s: &str) -> i64 {
    let s = s.trim();
    if s.is_empty() {
        return 0;
    }
    
    let mut total: i64 = 0;
    let mut num_start = 0;
    let chars: Vec<char> = s.chars().collect();
    let mut i = 0;
    
    while i < chars.len() {
        // Skip whitespace
        while i < chars.len() && chars[i].is_whitespace() {
            i += 1;
            num_start = i;
        }
        
        // Parse number (including decimal)
        let mut has_decimal = false;
        while i < chars.len() && (chars[i].is_ascii_digit() || chars[i] == '.') {
            if chars[i] == '.' {
                has_decimal = true;
            }
            i += 1;
        }
        
        if i == num_start {
            break;
        }
        
        let num_str: String = chars[num_start..i].iter().collect();
        let value: f64 = num_str.parse().unwrap_or(0.0);
        
        // Parse unit
        let unit_start = i;
        while i < chars.len() && chars[i].is_alphabetic() {
            i += 1;
        }
        
        let unit: String = chars[unit_start..i].iter().collect();
        let multiplier = match unit.as_str() {
            "ns" => 1,
            "us" | "µs" => 1_000,
            "ms" => 1_000_000,
            "s" => 1_000_000_000,
            "m" => 60 * 1_000_000_000,
            "h" => 3600 * 1_000_000_000,
            _ => 1, // Default to nanoseconds
        };
        
        if has_decimal {
            total += (value * multiplier as f64) as i64;
        } else {
            total += value as i64 * multiplier;
        }
        
        num_start = i;
    }
    
    total
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_duration() {
        assert_eq!(parse_duration_str("1s"), 1_000_000_000);
        assert_eq!(parse_duration_str("100ms"), 100_000_000);
        assert_eq!(parse_duration_str("1h30m"), 5400_000_000_000);
        assert_eq!(parse_duration_str("2.5s"), 2_500_000_000);
    }
}

