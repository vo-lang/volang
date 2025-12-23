#!/bin/zsh
# Vo Project Lines of Code Statistics
# Usage: ./scripts/loc.sh [--with-tests]

set -e

SCRIPT_DIR="$(cd "$(dirname "${(%):-%x}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors
BOLD='\033[1m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

INCLUDE_TESTS=false
if [[ "$1" == "--with-tests" ]]; then
    INCLUDE_TESTS=true
fi

echo "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
echo "${BOLD}                    Vo Project Statistics                      ${NC}"
echo "${BOLD}═══════════════════════════════════════════════════════════════${NC}"
echo ""

# Collect crate stats
typeset -A CRATE_LINES
typeset -A CRATE_FILES
TOTAL_LINES=0
TOTAL_FILES=0

echo "${CYAN}Crate                          │  Files  │   Lines   │  Avg/File${NC}"
echo "───────────────────────────────┼─────────┼───────────┼──────────"

for crate_dir in "$PROJECT_ROOT"/crates/*/; do
    if [[ -d "$crate_dir/src" ]]; then
        crate_name=$(basename "$crate_dir")
        
        # Skip vo-tests crate entirely when not including tests
        if [[ "$INCLUDE_TESTS" == "false" && "$crate_name" == "vo-tests" ]]; then
            continue
        fi
        
        if [[ "$INCLUDE_TESTS" == "true" ]]; then
            lines=$(find "$crate_dir/src" -name "*.rs" -type f 2>/dev/null | xargs wc -l 2>/dev/null | tail -1 | awk '{print $1}')
            files=$(find "$crate_dir/src" -name "*.rs" -type f 2>/dev/null | wc -l | tr -d ' ')
        else
            lines=$(find "$crate_dir/src" -name "*.rs" -type f ! -name "*_test.rs" 2>/dev/null | xargs wc -l 2>/dev/null | tail -1 | awk '{print $1}')
            files=$(find "$crate_dir/src" -name "*.rs" -type f ! -name "*_test.rs" 2>/dev/null | wc -l | tr -d ' ')
        fi
        
        # Handle empty results
        lines=${lines:-0}
        files=${files:-0}
        
        if [[ "$files" -gt 0 ]]; then
            avg=$((lines / files))
        else
            avg=0
        fi
        
        CRATE_LINES[$crate_name]=$lines
        CRATE_FILES[$crate_name]=$files
        TOTAL_LINES=$((TOTAL_LINES + lines))
        TOTAL_FILES=$((TOTAL_FILES + files))
        
        printf "%-30s │ %7s │ %9s │ %8s\n" "$crate_name" "$files" "$lines" "$avg"
    fi
done

echo "───────────────────────────────┼─────────┼───────────┼──────────"
if [[ "$TOTAL_FILES" -gt 0 ]]; then
    TOTAL_AVG=$((TOTAL_LINES / TOTAL_FILES))
else
    TOTAL_AVG=0
fi
printf "${BOLD}%-30s │ %7s │ %9s │ %8s${NC}\n" "TOTAL" "$TOTAL_FILES" "$TOTAL_LINES" "$TOTAL_AVG"

echo ""

# Test files stats (if not including tests)
if [[ "$INCLUDE_TESTS" == "false" ]]; then
    echo "${YELLOW}Test Statistics (excluded from above):${NC}"
    
    # Count test data files
    if [[ -d "$PROJECT_ROOT/crates/vo-tests/test_data" ]]; then
        test_data_files=$(find "$PROJECT_ROOT/crates/vo-tests/test_data" -name "*.vo" -type f 2>/dev/null | wc -l | tr -d ' ')
        test_data_lines=$(find "$PROJECT_ROOT/crates/vo-tests/test_data" -name "*.vo" -type f 2>/dev/null | xargs wc -l 2>/dev/null | tail -1 | awk '{print $1}')
        test_data_lines=${test_data_lines:-0}
        echo "  Test data files (.vo):     $test_data_files files, $test_data_lines lines"
    fi
    
    # Count test runner
    if [[ -d "$PROJECT_ROOT/crates/vo-tests/src" ]]; then
        test_runner_lines=$(find "$PROJECT_ROOT/crates/vo-tests/src" -name "*.rs" -type f 2>/dev/null | xargs wc -l 2>/dev/null | tail -1 | awk '{print $1}')
        test_runner_lines=${test_runner_lines:-0}
        echo "  Test runner (Rust):         $test_runner_lines lines"
    fi
    echo ""
fi

# Breakdown by category
echo "${GREEN}Category Breakdown:${NC}"
echo "───────────────────────────────────────"

# Frontend (syntax, parser)
frontend_lines=0
[[ -n "${CRATE_LINES[vo-syntax]}" ]] && frontend_lines=$((frontend_lines + ${CRATE_LINES[vo-syntax]}))
[[ -n "${CRATE_LINES[vo-common]}" ]] && frontend_lines=$((frontend_lines + ${CRATE_LINES[vo-common]}))
printf "  %-25s %8s lines\n" "Frontend (syntax/common)" "$frontend_lines"

# Analysis (type checking)
analysis_lines=0
[[ -n "${CRATE_LINES[vo-analysis]}" ]] && analysis_lines=${CRATE_LINES[vo-analysis]}
printf "  %-25s %8s lines\n" "Analysis (type checker)" "$analysis_lines"

# Codegen
codegen_lines=0
[[ -n "${CRATE_LINES[vo-codegen-vm]}" ]] && codegen_lines=${CRATE_LINES[vo-codegen-vm]}
printf "  %-25s %8s lines\n" "Code Generation (VM)" "$codegen_lines"

# Runtime
runtime_lines=0
[[ -n "${CRATE_LINES[vo-vm]}" ]] && runtime_lines=$((runtime_lines + ${CRATE_LINES[vo-vm]}))
[[ -n "${CRATE_LINES[vo-runtime-vm]}" ]] && runtime_lines=$((runtime_lines + ${CRATE_LINES[vo-runtime-vm]}))
[[ -n "${CRATE_LINES[vo-runtime-core]}" ]] && runtime_lines=$((runtime_lines + ${CRATE_LINES[vo-runtime-core]}))
printf "  %-25s %8s lines\n" "Runtime (VM)" "$runtime_lines"

# Tools
tools_lines=0
[[ -n "${CRATE_LINES[vo-cli]}" ]] && tools_lines=$((tools_lines + ${CRATE_LINES[vo-cli]}))
[[ -n "${CRATE_LINES[vo-module]}" ]] && tools_lines=$((tools_lines + ${CRATE_LINES[vo-module]}))
printf "  %-25s %8s lines\n" "Tools (CLI/Module)" "$tools_lines"

echo ""
echo "${BLUE}Generated at: $(date '+%Y-%m-%d %H:%M:%S')${NC}"

if [[ "$INCLUDE_TESTS" == "false" ]]; then
    echo "${BLUE}Note: Run with --with-tests to include test files${NC}"
fi
