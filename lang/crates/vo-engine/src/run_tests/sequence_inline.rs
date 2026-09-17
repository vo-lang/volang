use super::*;

const SOURCE: &str = r#"package main
import "runtime/mem"
type Holder struct { values [6]int }
var shared = [6]int{10, 11, 12, 13, 14, 15}
func retained() []int {
    owner := &Holder{values: [6]int{20, 21, 22, 23, 24, 25}}
    return owner.values[1:5]
}
func swap(s []int, i, j int) { s[i], s[j] = s[j], s[i] }
func step(s []int, i, j int) { s[i]++; s[j] = s[i] }
func catchStep(s []int, i, j int) (caught bool) {
    defer func() { if recover() != nil { caught = true } }()
    step(s, i, j)
    return
}
func main() {
    managed := retained()
    alias := managed[:]
    global := shared[1:5]
    packed := []int{30, 31, 32, 33}
    for i := 0; i < 1000; i++ {
        swap(managed, 0, 3)
        swap(global, 3, 0)
        swap(packed, 2, 2)
        if i % 7 == 0 { assert(mem.GCCollect()) }
    }
    assert(alias[0] == 21 && alias[3] == 24)
    assert(shared[1] == 11 && shared[4] == 14)
    assert(packed[0] == 30 && packed[2] == 32)
    assert(catchStep(managed, 0, 4))
    assert(alias[0] == 22 && alias[3] == 24)
    assert(catchStep(managed, -1, 0))
    assert(alias[0] == 22)
    var empty []int
    assert(catchStep(empty, 0, 0))
    assert(mem.GCCollect())
    assert(alias[0] == 22 && global[0] == 11 && packed[2] == 32)
    println("sequence leaves ok")
}
"#;

#[test]
fn sequence_leaves_preserve_owned_views_globals_gc_and_recover() {
    let compiled = crate::compile_string(SOURCE).unwrap();
    for mode in ["vm", "function", "osr"] {
        let mut vm = if mode == "vm" {
            Vm::new()
        } else {
            Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: if mode == "function" { 1 } else { 1_000_000 },
                loop_threshold: if mode == "osr" { 1 } else { 1_000_000 },
                optimizing_threshold: 16,
                ..vo_vm::JitConfig::default()
            })
            .unwrap()
        };
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load(compiled.module.module().clone()).unwrap();
        vm.run().unwrap_or_else(|error| panic!("{mode}: {error:?}"));
        assert_eq!(output.take_bytes(), b"sequence leaves ok\n");
        let stats = vm.jit_execution_stats();
        match mode {
            "function" => assert!(stats.function_entries > 0, "{stats:?}"),
            "osr" => assert!(stats.loop_entries > 0, "{stats:?}"),
            _ => assert!(!stats.executed_jit_code()),
        }
    }
}
