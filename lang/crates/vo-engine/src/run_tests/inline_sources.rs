use super::*;

#[test]
fn variadic_callers_keep_exact_sources_in_vm_jit_and_osr() {
    let source = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../../tests/lang/cases/runtime/core/backend_caller_variadic_sources.vo"
    ));
    let compiled = crate::compile_string(source).expect("caller source fixture must compile");
    for mode in ["vm", "jit", "osr"] {
        let mut vm = if mode == "vm" {
            Vm::new()
        } else {
            Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: if mode == "jit" { 1 } else { 1_000_000 },
                loop_threshold: if mode == "osr" { 1 } else { 1_000_000 },
                ..vo_vm::JitConfig::default()
            })
            .unwrap()
        };
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load(compiled.module.module().clone()).unwrap();
        vm.run()
            .unwrap_or_else(|error| panic!("{mode} caller source failed: {error:?}"));
        assert_eq!(output.take_bytes(), b"caller source positions ok\n");
        let stats = vm.jit_execution_stats();
        match mode {
            "jit" => assert!(stats.function_entries > 0),
            "osr" => assert!(stats.loop_entries > 0),
            _ => assert!(!stats.executed_jit_code()),
        }
    }
}

fn assert_nil_source(source: &str) {
    let expected_line = source
        .lines()
        .position(|line| line.contains("// TRAP_SOURCE"))
        .unwrap() as u32
        + 1;
    let compiled = crate::compile_string(source).expect("inline source fixture must compile");
    let module = compiled.module.module();
    for mode in ["vm", "jit", "osr"] {
        let mut vm = if mode == "vm" {
            Vm::new()
        } else {
            Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: if mode == "jit" { 1 } else { 1_000_000 },
                loop_threshold: if mode == "osr" { 1 } else { 1_000_000 },
                ..vo_vm::JitConfig::default()
            })
            .expect("native compiler must initialize")
        };
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load(module.clone())
            .expect("fixture must verify and load");
        let error = vm.run().expect_err("fixture must dereference nil");
        let VmError::RuntimeTrap {
            kind: RuntimeTrapKind::NilPointerDereference,
            loc,
            ..
        } = error
        else {
            panic!("unexpected {mode} error: {error:?}");
        };
        let location = loc.expect("runtime trap must retain a source before unwinding");
        let actual = module
            .debug_info
            .lookup(location.func_id(), location.pc())
            .expect("trap instruction must resolve to source");
        assert_eq!(
            actual.line, expected_line,
            "{mode} must identify the dereference: {location:?}"
        );
        assert!(
            output.take_bytes().is_empty(),
            "no guest effect may follow the trap"
        );
        let stats = vm.jit_execution_stats();
        match mode {
            "jit" => assert!(stats.function_entries > 0),
            "osr" => assert!(stats.loop_entries > 0),
            _ => assert!(!stats.executed_jit_code()),
        }
    }
}

#[test]
fn static_leaf_nil_trap_retains_dereference_source_in_vm_jit_and_osr() {
    assert_nil_source(
        r#"package main

type Node struct { value int }

func leaf(p *Node) int {
    return p.value // TRAP_SOURCE
}

func driver() int {
    p := &Node{value: 7}
    total := 0
    for i := 0; i < 1000; i++ {
        if i == 999 { p = nil }
        total += leaf(p)
    }
    return total
}

func main() {
    println(driver())
}
"#,
    );
}

#[test]
fn interface_leaf_nil_trap_retains_dereference_source_in_vm_jit_and_osr() {
    assert_nil_source(
        r#"package main

type Node struct { value int }
type Reader interface { Read() int }
func (p *Node) Read() int {
    return p.value // TRAP_SOURCE
}
func driver() int {
    p := &Node{value: 7}
    var r Reader = p
    total := 0
    for i := 0; i < 1000; i++ {
        if i == 999 { p = nil; r = p }
        total += r.Read()
    }
    return total
}
func main() { println(driver()) }
"#,
    );
}

#[test]
fn closure_leaf_nil_trap_retains_dereference_source_in_vm_jit_and_osr() {
    assert_nil_source(
        r#"package main

type Node struct { value int }
func makeReader(p *Node) func() int {
    return func() int {
        return p.value // TRAP_SOURCE
    }
}
func driver() int {
    p := &Node{value: 7}
    f := makeReader(p)
    total := 0
    for i := 0; i < 1000; i++ {
        if i == 999 { f = makeReader(nil) }
        total += f()
    }
    return total
}
func main() { println(driver()) }
"#,
    );
}

#[test]
fn recovered_inline_trap_does_not_change_a_later_trap_source() {
    assert_nil_source(
        r#"package main

type Node struct { value int }
var recovered bool
func leaf(p *Node) int { return p.value }
func recoverOnce(p *Node) {
    defer func() { if recover() != nil { recovered = true } }()
    leaf(p)
}
func driver() int {
    p := &Node{value: 7}
    for i := 0; i < 1000; i++ {
        if i == 999 { p = nil }
        recoverOnce(p)
    }
    if !recovered { panic("inline trap did not recover") }
    var q *Node
    return q.value // TRAP_SOURCE
}
func main() { println(driver()) }
"#,
    );
}

#[test]
fn parallel_assignment_reports_the_failing_destination_after_rhs_evaluation() {
    assert_nil_source(
        r#"package main

type Node struct { value int }
func driver() int {
    p := &Node{value: 7}
    x := 0
    for i := 0; i < 1000; i++ {
        if i == 999 { p = nil }
        x,
            p.value = i, x // TRAP_SOURCE
    }
    return x
}
func main() { println(driver()) }
"#,
    );
}

#[test]
fn tuple_assignment_reports_the_failing_destination_after_rhs_evaluation() {
    assert_nil_source(
        r#"package main

type Node struct { value int }
func pair(i int) (int, int) { return i, i + 1 }
func driver() int {
    p := &Node{value: 7}
    x := 0
    for i := 0; i < 1000; i++ {
        if i == 999 { p = nil }
        x,
            p.value = pair(i) // TRAP_SOURCE
    }
    return x
}
func main() { println(driver()) }
"#,
    );
}

#[test]
fn native_inline_panic_chain_survives_module_release_and_preserves_error_kind() {
    const SOURCE: &str = r#"package main
type Node struct { value int }
func leaf(p *Node) int {
    return p.value // LEAF_SOURCE
}
func driver(p *Node) int {
    sum := 0
    for j := 0; j < 16; j++ {
        sum += leaf(p) // PARENT_SOURCE
    }
    return sum
}
func main() {
    p := &Node{value: 7}
    sum := 0
    for i := 0; i < 32; i++ {
        if i == 31 { p = nil }
        sum += driver(p)
    }
    println(sum)
}
"#;
    let line = |marker: &str| {
        SOURCE
            .lines()
            .position(|line| line.contains(marker))
            .unwrap() as u32
            + 1
    };
    for mode in ["vm", "baseline", "optimizing", "osr"] {
        // Both VM and compiled input leave this scope before the returned
        // structured diagnostic is inspected or formatted.
        let (error, stats) = {
            let compiled = crate::compile_string(SOURCE).unwrap();
            let mut vm = if mode == "vm" {
                Vm::new()
            } else {
                Vm::try_with_jit_config(vo_vm::JitConfig {
                    call_threshold: if mode == "osr" { 1_000_000 } else { 1 },
                    loop_threshold: if mode == "osr" { 1 } else { 1_000_000 },
                    optimizing_threshold: if mode == "optimizing" { 1 } else { u64::MAX },
                    ..Default::default()
                })
                .unwrap()
            };
            let output = CaptureSink::new();
            vm.set_output_sink(output.clone());
            vm.load_verified(compiled.module).unwrap();
            let failure = vm.run().expect_err("nil leaf must fail before printing");
            assert!(output.take_bytes().is_empty());
            let module = vm.module().unwrap();
            let error = RuntimeError::from_vm_error(&failure, module);
            if mode != "vm" {
                // Optional source files may be absent, while both identities
                // and their order remain available to diagnostics.
                let mut stripped = module.clone();
                stripped.debug_info = Default::default();
                let unknown = RuntimeError::from_vm_error(&failure, &stripped);
                assert!(unknown.location.is_none());
                assert_eq!(unknown.inline_frames.len(), 2, "{mode}: {failure:?}");
                assert!(unknown
                    .inline_frames
                    .iter()
                    .all(|frame| frame.location.is_none()));
            }
            (error, vm.jit_execution_stats())
        };
        assert_eq!(error.kind, RuntimeErrorKind::NilPointerDereference);
        assert_eq!(error.location.as_ref().unwrap().line, line("LEAF_SOURCE"));
        match mode {
            "vm" => assert!(!stats.executed_jit_code()),
            "baseline" => {
                assert!(stats.function_entries > 0);
                assert_eq!(stats.optimizing_functions_executed, 0);
            }
            "optimizing" => assert!(stats.optimizing_functions_executed > 0),
            "osr" => assert!(stats.loop_entries > 0),
            _ => unreachable!(),
        }
        if mode != "vm" {
            assert_eq!(error.inline_frames.len(), 2, "{mode}: {error:?}");
            let frames = &error.inline_frames;
            assert!(frames[0].function_name.as_ref().unwrap().ends_with("leaf"));
            assert!(frames[1]
                .function_name
                .as_ref()
                .unwrap()
                .ends_with("driver"));
            assert_eq!(
                frames[0].location.as_ref().unwrap().line,
                line("LEAF_SOURCE")
            );
            assert_eq!(
                frames[1].location.as_ref().unwrap().line,
                line("PARENT_SOURCE")
            );
            let display = RunError::Runtime(error).to_string();
            assert!(display.contains("\n  at ") && display.contains("\n  inlined in "));
        } else {
            assert!(error.inline_frames.is_empty(), "{mode}: {error:?}");
        }
    }
}
