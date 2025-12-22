#!/bin/bash
# GoX Integration Test Runner
# Usage: ./test.sh [vm|jit]  (default: both)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR/.."

TEST_DIR="crates/gox-tests/test_data"
BIN="target/debug/gox-tests"
MODE="${1:-both}"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
CYAN='\033[0;36m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Build once
cargo build -q -p gox-tests || exit 1

vm_passed=0
vm_failed=0
jit_passed=0
jit_failed=0
passed_list=""
failed_list=""

# Find all .gox files and proj_* directories
for test in $(find "$TEST_DIR" -name "*.gox" -o -type d -name "proj_*" | sort); do
    # Skip if inside proj_* (we handle those as directories)
    if [[ "$test" == *.gox ]] && [[ "$test" == */proj_*/* ]]; then
        continue
    fi
    
    name=$(echo "$test" | sed "s|$TEST_DIR/||")
    
    # Run VM mode
    if [[ "$MODE" == "vm" || "$MODE" == "both" ]]; then
        result=$("$BIN" --mode=vm "$test" 2>&1)
        if echo "$result" | grep -q "^OK$"; then
            vm_passed=$((vm_passed + 1))
            passed_list="$passed_list  ${GREEN}✓${NC} $name [vm]\n"
        elif echo "$result" | grep -q "not applicable"; then
            : # skip
        else
            vm_failed=$((vm_failed + 1))
            failed_list="$failed_list  ${RED}✗${NC} $name [vm]\n"
        fi
    fi
    
    # Run JIT mode
    if [[ "$MODE" == "jit" || "$MODE" == "both" ]]; then
        result=$("$BIN" --mode=jit "$test" 2>&1)
        if echo "$result" | grep -q "^OK$"; then
            jit_passed=$((jit_passed + 1))
            passed_list="$passed_list  ${GREEN}✓${NC} $name [jit]\n"
        elif echo "$result" | grep -q "not applicable"; then
            : # skip
        else
            jit_failed=$((jit_failed + 1))
            failed_list="$failed_list  ${RED}✗${NC} $name [jit]\n"
        fi
    fi
done

total_passed=$((vm_passed + jit_passed))
total_failed=$((vm_failed + jit_failed))

if [ -n "$passed_list" ]; then
    echo -e "${GREEN}${BOLD}Passed:${NC}"
    echo -e "$passed_list"
fi

if [ -n "$failed_list" ]; then
    echo -e "${RED}${BOLD}Failed:${NC}"
    echo -e "$failed_list"
fi

echo -e "${CYAN}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${CYAN}║${NC}${BOLD}                   GoX Test Results                       ${NC}${CYAN}║${NC}"
echo -e "${CYAN}╠══════════════════════════════════════════════════════════╣${NC}"
printf "${CYAN}║${NC}  VM:  ${GREEN}%3d passed${NC}  ${RED}%3d failed${NC}                             ${CYAN}║${NC}\n" "$vm_passed" "$vm_failed"
printf "${CYAN}║${NC}  JIT: ${GREEN}%3d passed${NC}  ${RED}%3d failed${NC}                             ${CYAN}║${NC}\n" "$jit_passed" "$jit_failed"
echo -e "${CYAN}╠══════════════════════════════════════════════════════════╣${NC}"
printf "${CYAN}║${NC}  Total: ${GREEN}%3d passed${NC}  ${RED}%3d failed${NC}                           ${CYAN}║${NC}\n" "$total_passed" "$total_failed"
echo -e "${CYAN}╚══════════════════════════════════════════════════════════╝${NC}"

[ "$total_failed" -gt 0 ] && exit 1 || exit 0
