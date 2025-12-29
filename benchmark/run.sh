#!/bin/bash

# Vo Language Benchmark Suite
# Compares Vo (VM/JIT) vs Go vs Lua/LuaJIT vs Python

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check dependencies
check_deps() {
    local missing=()
    
    command -v hyperfine >/dev/null 2>&1 || missing+=("hyperfine")
    command -v go >/dev/null 2>&1 || missing+=("go")
    command -v python3 >/dev/null 2>&1 || missing+=("python3")
    
    # Check for lua or luajit
    if ! command -v luajit >/dev/null 2>&1 && ! command -v lua >/dev/null 2>&1; then
        missing+=("lua/luajit")
    fi
    
    if [ ${#missing[@]} -ne 0 ]; then
        echo -e "${RED}Missing dependencies: ${missing[*]}${NC}"
        echo "Install with:"
        echo "  brew install hyperfine go lua luajit python3"
        exit 1
    fi
}

# Check if C compiler is available
has_cc() {
    command -v cc >/dev/null 2>&1 || command -v gcc >/dev/null 2>&1 || command -v clang >/dev/null 2>&1
}

# Check if java is available
has_java() {
    command -v java >/dev/null 2>&1 && command -v javac >/dev/null 2>&1
}

# Check if lua is available
has_lua() {
    command -v lua >/dev/null 2>&1
}

# Check if luajit is available
has_luajit() {
    command -v luajit >/dev/null 2>&1
}

# Check if ruby is available
has_ruby() {
    command -v ruby >/dev/null 2>&1
}

# Build vo release binary
build_vo() {
    echo -e "${BLUE}Building Vo (release)...${NC}"
    cargo build --release --bin vo --manifest-path "$PROJECT_ROOT/Cargo.toml" 2>/dev/null
}

# Run a single benchmark
run_benchmark() {
    local name="$1"
    local dir="$SCRIPT_DIR/$name"
    local vo_bin="$PROJECT_ROOT/target/release/vo"
    
    echo -e "\n${GREEN}=== $name ===${NC}\n"
    
    # Find the source files
    local vo_file=$(find "$dir" -name "*.vo" | head -1)
    local go_file=$(find "$dir" -name "*.go" | head -1)
    local lua_file=$(find "$dir" -name "*.lua" | head -1)
    local py_file=$(find "$dir" -name "*.py" | head -1)
    local java_file=$(find "$dir" -name "*.java" | head -1)
    
    local cmds=()
    local names=()
    
    # Vo VM
    if [ -f "$vo_file" ]; then
        cmds+=("$vo_bin run '$vo_file'")
        names+=("Vo-VM")
        
        # Vo JIT
        cmds+=("VO_JIT_CALL_THRESHOLD=1 $vo_bin run --mode=jit '$vo_file'")
        names+=("Vo-JIT")
    fi
    
    # Go (pre-compile to avoid compilation overhead)
    if [ -f "$go_file" ]; then
        local go_bin="$dir/go_bench"
        go build -o "$go_bin" "$go_file" 2>/dev/null
        cmds+=("'$go_bin'")
        names+=("Go")
    fi
    
    # Lua VM
    if [ -f "$lua_file" ] && has_lua; then
        cmds+=("lua '$lua_file'")
        names+=("Lua")
    fi
    
    # LuaJIT
    if [ -f "$lua_file" ] && has_luajit; then
        cmds+=("luajit '$lua_file'")
        names+=("LuaJIT")
    fi
    
    # Python
    if [ -f "$py_file" ]; then
        cmds+=("python3 '$py_file'")
        names+=("Python")
    fi
    
    # Ruby
    local rb_file=$(find "$dir" -name "*.rb" | head -1)
    if [ -f "$rb_file" ] && has_ruby; then
        cmds+=("ruby '$rb_file'")
        names+=("Ruby")
    fi
    
    # Java (compile and run)
    if [ -f "$java_file" ] && has_java; then
        local java_class=$(basename "$java_file" .java)
        if javac -d "$dir" "$java_file" 2>/dev/null; then
            cmds+=("java -cp '$dir' '$java_class'")
            names+=("Java")
        fi
    fi
    
    # C (compile and run)
    local c_file=$(find "$dir" -name "*.c" | head -1)
    if [ -f "$c_file" ] && has_cc; then
        local c_bin="$dir/c_bench"
        if cc -O3 -o "$c_bin" "$c_file" 2>/dev/null; then
            cmds+=("'$c_bin'")
            names+=("C")
        fi
    fi
    
    # Build hyperfine command
    local hf_args=("--warmup" "1")
    for i in "${!cmds[@]}"; do
        hf_args+=("-n" "${names[$i]}" "${cmds[$i]}")
    done
    
    # Export results
    local export_json="$SCRIPT_DIR/results/${name}.json"
    local export_md="$SCRIPT_DIR/results/${name}.md"
    mkdir -p "$SCRIPT_DIR/results"
    hf_args+=("--export-json" "$export_json")
    hf_args+=("--export-markdown" "$export_md")
    
    hyperfine "${hf_args[@]}"
}

# List available benchmarks
list_benchmarks() {
    echo "Available benchmarks:"
    for dir in "$SCRIPT_DIR"/*/; do
        if [ -d "$dir" ] && [ "$(basename "$dir")" != "results" ]; then
            echo "  - $(basename "$dir")"
        fi
    done
}

# Run all benchmarks
run_all() {
    for dir in "$SCRIPT_DIR"/*/; do
        if [ -d "$dir" ] && [ "$(basename "$dir")" != "results" ]; then
            run_benchmark "$(basename "$dir")"
        fi
    done
}

# Generate summary report
generate_report() {
    echo -e "\n${GREEN}=== Summary Report ===${NC}\n"
    
    if [ -d "$SCRIPT_DIR/results" ]; then
        echo "Results saved in $SCRIPT_DIR/results/"
        echo ""
        for md in "$SCRIPT_DIR/results"/*.md; do
            if [ -f "$md" ]; then
                echo "### $(basename "$md" .md)"
                cat "$md"
                echo ""
            fi
        done
    fi
}

# Main
main() {
    check_deps
    build_vo
    
    case "${1:-all}" in
        list)
            list_benchmarks
            ;;
        all)
            run_all
            generate_report
            ;;
        *)
            if [ -d "$SCRIPT_DIR/$1" ]; then
                run_benchmark "$1"
            else
                echo -e "${RED}Unknown benchmark: $1${NC}"
                list_benchmarks
                exit 1
            fi
            ;;
    esac
}

main "$@"
