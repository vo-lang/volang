#!/bin/bash
# GoX Integration Test Runner
#
# Usage:
#   ./test.sh              # Run all tests (both VM and JIT modes)
#   ./test.sh vm           # Run only VM mode tests
#   ./test.sh jit          # Run only JIT mode tests
#   ./test.sh -v           # Verbose mode (show all output)
#   ./test.sh <file.gox>   # Run a single test file
#
# Configuration: test_data/tests.yaml

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

TEST_DIR="test_data"
CONFIG="$TEST_DIR/_tests.yaml"
BIN="target/debug/gox"
MODE="${1:-both}"
VERBOSE=false

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
DIM='\033[2m'
NC='\033[0m'

# Parse arguments
if [[ "$1" == "-v" || "$1" == "--verbose" ]]; then
    VERBOSE=true
    MODE="${2:-both}"
elif [[ "$1" == "-h" || "$1" == "--help" ]]; then
    echo "GoX Integration Test Runner"
    echo ""
    echo "Usage:"
    echo "  ./test.sh              # Run all tests (both VM and JIT modes)"
    echo "  ./test.sh vm           # Run only VM mode tests"
    echo "  ./test.sh jit          # Run only JIT mode tests"
    echo "  ./test.sh -v           # Verbose mode"
    echo "  ./test.sh <file.gox>   # Run a single test file"
    exit 0
elif [[ -f "$1" ]]; then
    # Single file mode
    echo "Running single test: $1"
    cargo build -q -p gox-cli || exit 1
    "$BIN" run "$1" --mode=vm
    exit $?
fi

# Build CLI
echo -e "${DIM}Building gox-cli...${NC}"
cargo build -q -p gox-cli || exit 1

# Check if config exists
if [[ ! -f "$CONFIG" ]]; then
    echo -e "${RED}Error: $CONFIG not found${NC}"
    exit 1
fi

# Counters
vm_passed=0
vm_failed=0
jit_passed=0
jit_failed=0
skipped=0

passed_list=""
failed_list=""

# Parse YAML and run tests
# Simple YAML parser using grep/sed
current_file=""
current_modes=""
current_skip=""

run_test() {
    local file="$1"
    local mode="$2"
    
    local path="$TEST_DIR/$file"
    
    # Check if path exists
    if [[ ! -e "$path" ]]; then
        echo -e "  ${YELLOW}⊘${NC} $file [not found]"
        return
    fi
    
    # Run the test
    local output
    local exit_code
    
    if $VERBOSE; then
        echo -e "${DIM}Running: $BIN run $path --mode=$mode${NC}"
    fi
    
    output=$("$BIN" run "$path" --mode="$mode" 2>&1) || true
    
    # Check output for result tags
    if echo "$output" | grep -q "\[GOX:OK\]"; then
        if [[ "$mode" == "vm" ]]; then
            vm_passed=$((vm_passed + 1))
        else
            jit_passed=$((jit_passed + 1))
        fi
        passed_list="$passed_list  ${GREEN}✓${NC} $file [$mode]\n"
    elif echo "$output" | grep -q "\[GOX:PANIC:"; then
        if [[ "$mode" == "vm" ]]; then
            vm_failed=$((vm_failed + 1))
        else
            jit_failed=$((jit_failed + 1))
        fi
        local panic_msg=$(echo "$output" | grep -o "\[GOX:PANIC:[^]]*\]" | head -1)
        failed_list="$failed_list  ${RED}✗${NC} $file [$mode] $panic_msg\n"
    elif echo "$output" | grep -q "\[GOX:ERROR:"; then
        if [[ "$mode" == "vm" ]]; then
            vm_failed=$((vm_failed + 1))
        else
            jit_failed=$((jit_failed + 1))
        fi
        local error_msg=$(echo "$output" | grep -o "\[GOX:ERROR:[^]]*\]" | head -1)
        failed_list="$failed_list  ${RED}✗${NC} $file [$mode] $error_msg\n"
    else
        # No tag found - assume success if no error
        if [[ "$mode" == "vm" ]]; then
            vm_passed=$((vm_passed + 1))
        else
            jit_passed=$((jit_passed + 1))
        fi
        passed_list="$passed_list  ${GREEN}✓${NC} $file [$mode]\n"
    fi
    
    if $VERBOSE; then
        echo "$output"
    fi
}

echo -e "${BOLD}Running GoX integration tests...${NC}\n"

# Read and parse YAML
while IFS= read -r line || [[ -n "$line" ]]; do
    # Skip comments and empty lines
    [[ "$line" =~ ^[[:space:]]*# ]] && continue
    [[ -z "${line// }" ]] && continue
    
    # Parse file: entry
    if [[ "$line" =~ ^[[:space:]]*-[[:space:]]*file:[[:space:]]*(.+)$ ]]; then
        # Process previous test if any
        if [[ -n "$current_file" && -n "$current_modes" ]]; then
            if [[ -n "$current_skip" ]]; then
                skipped=$((skipped + 1))
                if $VERBOSE; then
                    echo -e "  ${YELLOW}⊘${NC} $current_file [skipped: $current_skip]"
                fi
            else
                # Run for each mode
                if [[ "$current_modes" == *"vm"* ]] && [[ "$MODE" == "vm" || "$MODE" == "both" ]]; then
                    run_test "$current_file" "vm"
                fi
                if [[ "$current_modes" == *"jit"* ]] && [[ "$MODE" == "jit" || "$MODE" == "both" ]]; then
                    run_test "$current_file" "jit"
                fi
            fi
        fi
        
        # Start new test
        current_file="${BASH_REMATCH[1]}"
        current_modes=""
        current_skip=""
    fi
    
    # Parse mode: entry
    if [[ "$line" =~ ^[[:space:]]*mode:[[:space:]]*\[(.+)\]$ ]]; then
        current_modes="${BASH_REMATCH[1]}"
    fi
    
    # Parse skip: entry
    if [[ "$line" =~ ^[[:space:]]*skip:[[:space:]]*(.+)$ ]]; then
        current_skip="${BASH_REMATCH[1]}"
    fi
    
done < "$CONFIG"

# Process last test
if [[ -n "$current_file" && -n "$current_modes" ]]; then
    if [[ -n "$current_skip" ]]; then
        skipped=$((skipped + 1))
    else
        if [[ "$current_modes" == *"vm"* ]] && [[ "$MODE" == "vm" || "$MODE" == "both" ]]; then
            run_test "$current_file" "vm"
        fi
        if [[ "$current_modes" == *"jit"* ]] && [[ "$MODE" == "jit" || "$MODE" == "both" ]]; then
            run_test "$current_file" "jit"
        fi
    fi
fi

total_passed=$((vm_passed + jit_passed))
total_failed=$((vm_failed + jit_failed))

# Print results
echo ""

if [[ -n "$failed_list" ]]; then
    echo -e "${RED}${BOLD}Failed:${NC}"
    echo -e "$failed_list"
fi

echo -e "${CYAN}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║${NC}${BOLD}                   GoX Test Results                       ${NC}${CYAN}║${NC}"
echo -e "${CYAN}╠══════════════════════════════════════════════════════════╣${NC}"
printf "${CYAN}║${NC}  VM:  ${GREEN}%3d passed${NC}  ${RED}%3d failed${NC}                             ${CYAN}║${NC}\n" "$vm_passed" "$vm_failed"
printf "${CYAN}║${NC}  JIT: ${GREEN}%3d passed${NC}  ${RED}%3d failed${NC}                             ${CYAN}║${NC}\n" "$jit_passed" "$jit_failed"
if [[ $skipped -gt 0 ]]; then
    printf "${CYAN}║${NC}  Skipped: ${YELLOW}%3d${NC}                                            ${CYAN}║${NC}\n" "$skipped"
fi
echo -e "${CYAN}╠══════════════════════════════════════════════════════════╣${NC}"
printf "${CYAN}║${NC}  Total: ${GREEN}%3d passed${NC}  ${RED}%3d failed${NC}                           ${CYAN}║${NC}\n" "$total_passed" "$total_failed"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════╝${NC}"

[[ "$total_failed" -gt 0 ]] && exit 1 || exit 0
