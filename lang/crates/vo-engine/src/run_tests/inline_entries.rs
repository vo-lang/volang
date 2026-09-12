use super::*;

fn assert_cached_wrappers(source: &str, interface: bool) {
    let compiled = crate::compile_string(source).expect("dynamic wrapper fixture must compile");
    for mode in ["vm", "function", "osr"] {
        let mut vm = if mode == "vm" {
            Vm::new()
        } else {
            Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: if mode == "function" { 1 } else { 8 },
                loop_threshold: if mode == "osr" { 1 } else { 1_000_000 },
                optimizing_threshold: 32,
                ..vo_vm::JitConfig::default()
            })
            .unwrap()
        };
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load(compiled.module.module().clone()).unwrap();
        vm.run().unwrap_or_else(|error| panic!("{mode}: {error:?}"));
        assert_eq!(output.take_bytes(), b"cached wrappers ok\n");
        let stats = vm.jit_execution_stats();
        if mode == "vm" {
            assert!(!stats.executed_jit_code());
            continue;
        }
        if mode == "osr" {
            assert!(stats.loop_entries > 0, "{stats:?}");
        } else {
            assert!(stats.function_entries > 0, "{stats:?}");
        }
        assert!(stats.optimizing_compilations > 0, "{stats:?}");
        assert!(stats.dynamic_ic_publications >= 2, "{stats:?}");
        let prepares = if interface {
            stats.iface_prepare_callbacks
        } else {
            stats.closure_prepare_callbacks
        };
        assert!(
            prepares > 0 && prepares < 1000,
            "{mode} must cache the 10000 dynamic calls: {stats:?}"
        );
    }
}

#[test]
fn compiled_scalar_wrappers_cache_dynamic_closure_calls_across_tier_changes() {
    assert_cached_wrappers(
        r#"package main
import "runtime/mem"
func base(x int) int { return x * 3 + 1 }
func first(x int) int { return base(x) + 7 }
func second(x int) int { return base(x) + 11 }
func invoke(operations []func(int) int, which, x int) int {
    if which < 0 { return x }
    return operations[which](x)
}
func main() {
    operations := []func(int) int{first, second}
    total, expected := 0, 0
    for i := 0; i < 10000; i++ {
        which := 0
        if i >= 5000 { which = i & 1 }
        x := i & 31
        total += invoke(operations, which, x)
        expected += x * 3 + 8 + which * 4
        if i % 997 == 0 { assert(mem.GCCollect()) }
    }
    assert(total == expected)
    println("cached wrappers ok")
}
"#,
        false,
    );
}

#[test]
fn compiled_scalar_wrappers_cache_opaque_interface_receivers_with_live_gc_roots() {
    assert_cached_wrappers(
        r#"package main
import "runtime/mem"
type Operation interface { Apply(int) int }
type First struct { marker int }
type Second struct { marker int }
func base(x int) int { return x * 3 + 1 }
func first(x int) int { return base(x) + 7 }
func second(x int) int { return base(x) + 11 }
func (p *First) Apply(x int) int { return first(x) }
func (p *Second) Apply(x int) int { return second(x) }
func invoke(operations []Operation, which, x int) int {
    if which < 0 { return x }
    return operations[which].Apply(x)
}
func main() {
    firstObject := &First{marker: 17}
    secondObject := &Second{marker: 29}
    operations := []Operation{firstObject, secondObject}
    total, expected := 0, 0
    for i := 0; i < 10000; i++ {
        which := 0
        if i >= 5000 { which = i & 1 }
        x := i & 31
        total += invoke(operations, which, x)
        expected += x * 3 + 8 + which * 4
        if i % 997 == 0 { assert(mem.GCCollect()) }
    }
    assert(total == expected)
    assert(firstObject.marker == 17 && secondObject.marker == 29)
    println("cached wrappers ok")
}
"#,
        true,
    );
}

#[test]
fn dynamic_closure_late_phases_reuse_victim_across_gc_and_tier_changes() {
    assert_cached_wrappers(
        r#"package main
import "runtime/mem"
func op0(x int) int { return x * 3 + 1 }
func op1(x int) int { return x * 3 + 2 }
func op2(x int) int { return x * 3 + 3 }
func op3(x int) int { return x * 3 + 4 }
func op4(x int) int { return x * 3 + 5 }
func op5(x int) int { return x * 3 + 6 }
func op6(x int) int { return x * 3 + 7 }
func op7(x int) int { return x * 3 + 8 }
func invoke(operations []func(int) int, which, x int) int {
    if which < 0 { return x }
    return operations[which](x)
}
func main() {
    operations := []func(int) int{op0, op1, op2, op3, op4, op5, op6, op7}
    total, expected := 0, 0
    for i := 0; i < 16000; i++ {
        which := (i / 1000) & 7
        x := i & 31
        total += invoke(operations, which, x)
        expected += x * 3 + which + 1
        if i % 997 == 0 { assert(mem.GCCollect()) }
    }
    assert(total == expected)
    println("cached wrappers ok")
}
"#,
        false,
    );
}

#[test]
fn dynamic_interface_late_phases_reuse_victim_across_gc_and_tier_changes() {
    assert_cached_wrappers(
        r#"package main
import "runtime/mem"
type Operation interface { Apply(int) int }
type Op0 struct { marker int }
func (p *Op0) Apply(x int) int { return x * 3 + 1 }
type Op1 struct { marker int }
func (p *Op1) Apply(x int) int { return x * 3 + 2 }
type Op2 struct { marker int }
func (p *Op2) Apply(x int) int { return x * 3 + 3 }
type Op3 struct { marker int }
func (p *Op3) Apply(x int) int { return x * 3 + 4 }
type Op4 struct { marker int }
func (p *Op4) Apply(x int) int { return x * 3 + 5 }
type Op5 struct { marker int }
func (p *Op5) Apply(x int) int { return x * 3 + 6 }
type Op6 struct { marker int }
func (p *Op6) Apply(x int) int { return x * 3 + 7 }
type Op7 struct { marker int }
func (p *Op7) Apply(x int) int { return x * 3 + 8 }
func invoke(operations []Operation, which, x int) int {
    if which < 0 { return x }
    return operations[which].Apply(x)
}
func main() {
    operations := []Operation{&Op0{marker: 17}, &Op1{marker: 18}, &Op2{marker: 19}, &Op3{marker: 20}, &Op4{marker: 21}, &Op5{marker: 22}, &Op6{marker: 23}, &Op7{marker: 24}}
    total, expected := 0, 0
    for i := 0; i < 16000; i++ {
        which := (i / 1000) & 7
        x := i & 31
        total += invoke(operations, which, x)
        expected += x * 3 + which + 1
        if i % 997 == 0 { assert(mem.GCCollect()) }
    }
    assert(total == expected)
    println("cached wrappers ok")
}
"#,
        true,
    );
}

#[test]
fn feedback_scalar_tier_up_removes_actual_calls_and_retains_target_and_gc_transitions() {
    let source = r#"package main
import "runtime/mem"
func first(x int) int { return x + 1 }
func second(x int) int { return x - 1 }
func invoke(operations []func(int) int, which, x int) int {
    if which < 0 { return x }
    return operations[which](x)
}
func main() {
    operations := []func(int) int{first, second}
    for i := 0; i < 32; i++ { assert(invoke(operations, -1, i) == i) }
    total, expected := 0, 0
    for i := 0; i < 10000; i++ {
        which := 0
        if i >= 8000 { which = i & 1 }
        total += invoke(operations, which, i)
        expected += i + 1 - which * 2
        if i % 997 == 0 { assert(mem.GCCollect()) }
    }
    assert(total == expected)
    println("feedback transitions ok")
}
"#;
    assert_feedback_transitions(source);
}

fn assert_feedback_transitions(source: &str) {
    let compiled = crate::compile_string(source).unwrap();
    for osr in [false, true] {
        let mut profiles = Vec::new();
        for optimizing in [false, true] {
            let mut vm = Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: if osr { 8 } else { 1 },
                loop_threshold: if osr { 1 } else { 1_000_000 },
                optimizing_threshold: if optimizing { 6000 } else { u64::MAX },
                ..vo_vm::JitConfig::default()
            })
            .unwrap();
            let output = CaptureSink::new();
            vm.set_output_sink(output.clone());
            vm.load(compiled.module.module().clone()).unwrap();
            vm.run()
                .unwrap_or_else(|error| panic!("osr={osr} optimizing={optimizing}: {error:?}"));
            assert_eq!(output.take_bytes(), b"feedback transitions ok\n");
            let stats = vm.jit_execution_stats();
            assert!(stats.function_entries > 0, "{stats:?}");
            if osr {
                assert!(stats.loop_entries > 0, "{stats:?}");
            }
            if optimizing {
                assert!(stats.optimizing_compilations > 0, "{stats:?}");
                assert!(stats.optimizing_functions_executed > 0, "{stats:?}");
            } else {
                assert_eq!(stats.optimizing_compilations, 0, "{stats:?}");
            }
            let callees: Vec<_> = compiled
                .module
                .module()
                .functions
                .iter()
                .enumerate()
                .filter(|(_, f)| f.name == "first" || f.name == "second" || f.name == "Apply$iface")
                .map(|(id, f)| (f.name.clone(), vm.jit_function_profile(id as u32).unwrap()))
                .collect();
            assert_eq!(callees.len(), 2, "{callees:?}");
            // An optimized callee would stop counting entries independently of
            // inlining. Keep both callees in their eligible baseline tier so the
            // existing training counters measure every nested native entry.
            for (_, profile) in &callees {
                assert_eq!(
                    profile.tier_up_state, 0,
                    "osr={osr} optimizing={optimizing}: {callees:?}"
                );
                assert_eq!(profile.optimizing_entered, 0, "{callees:?}");
            }
            profiles.push(callees);
        }
        let entries: Vec<u64> = profiles
            .iter()
            .map(|callees| callees.iter().map(|(_, p)| p.entries).sum())
            .collect();
        // Invoke reaches tier-up before its first target does. Guarded inlining
        // then removes over 2,000 actual native callee entries. The final phase
        // still dispatches roughly 1,000 calls to the changed target.
        assert!(entries[0] >= 9980, "osr={osr}: {profiles:?}");
        assert!(entries[1] + 2000 < entries[0], "osr={osr}: {profiles:?}");
        assert!(
            profiles[1]
                .iter()
                .any(|(_, p)| (990..=1010).contains(&p.entries)),
            "changed target must execute: osr={osr}: {profiles:?}"
        );
    }
}

#[test]
fn feedback_scalar_interface_receivers_keep_payloads_when_targets_change() {
    assert_feedback_transitions(
        r#"package main
import "runtime/mem"
type Operation interface { Apply(int) int }
type First int
type Second int
func (p First) Apply(x int) int { return int(p) + x }
func (p Second) Apply(x int) int { return int(p) - x }
func invoke(operations []Operation, which, x int) int {
    if which < 0 { return x }
    return operations[which].Apply(x)
}
func main() {
    operations := []Operation{First(3), Second(5)}
    for i := 0; i < 32; i++ { assert(invoke(operations, -1, i) == i) }
    total, expected := 0, 0
    for i := 0; i < 10000; i++ {
        which := 0
        if i >= 8000 { which = i & 1 }
        total += invoke(operations, which, i)
        if which == 0 { expected += 3 + i } else { expected += 5 - i }
        if i % 997 == 0 { assert(mem.GCCollect()) }
    }
    assert(total == expected)
    println("feedback transitions ok")
}
"#,
    );
}

#[test]
fn feedback_scalar_multiple_returns_remain_independent_of_target_changes() {
    assert_feedback_transitions(
        r#"package main
import "runtime/mem"
func first(x int) (int, int) { return x + 1, x - 1 }
func second(x int) (int, int) { return x - 1, x + 1 }
func invoke(operations []func(int) (int, int), which, x int) (int, int) {
    if which < 0 { return x, x }
    return operations[which](x)
}
func main() {
    operations := []func(int) (int, int){first, second}
    for i := 0; i < 32; i++ {
        a, b := invoke(operations, -1, i)
        assert(a == i && b == i)
    }
    total, expected := 0, 0
    for i := 0; i < 10000; i++ {
        which := 0
        if i >= 8000 { which = i & 1 }
        a, b := invoke(operations, which, i)
        total += a * 3 + b
        expected += i * 4 + 2 - which * 4
        if i % 997 == 0 { assert(mem.GCCollect()) }
    }
    assert(total == expected)
    println("feedback transitions ok")
}
"#,
    );
}
