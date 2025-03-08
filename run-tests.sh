#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-03-08 14:43:04 (ywatanabe)"
# File: /home/ywatanabe/.emacs.d/lisp/elisp-test/run-tests.sh

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_PATH="$0.log"
touch "$LOG_PATH"

PATHS=(
    "$THIS_DIR"
    "$THIS_DIR/tests/"
)
SOURCE_FILES=("$THIS_DIR"/*.el)
TEST_FILES=("$THIS_DIR"/tests/test-*.el)
REQUIRE_PACKAGES=(org cl-lib)

# Color definitions
YELLOW='\033[1;33m'
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

usage() {
    echo "Usage: $0 [-d|--debug] [-h|--help]"
    echo "Runs tests for this project"
    echo
    echo "Options:"
    echo "  -d, --debug    Enable debug output"
    echo "  -h, --help     Display this help message"
    exit 1
}
main() {
    local debug=false
    while [[ $# -gt 0 ]]; do
        case $1 in
            -d|--debug) debug=true; shift ;;
            -h|--help) usage ;;
            *) echo "Unknown option: $1"; usage ;;
        esac
    done
    local l_args=""
    local loaded_files=()
    for file in "${SOURCE_FILES[@]}"; do
        if [[ -f "$file" && ! "$file" =~ "test-" ]]; then
            if [ "$debug" = true ]; then
                echo "Loading source: $file"
            fi
            l_args="$l_args -l $file"
            loaded_files+=("$file")
        fi
    done
    for file in "${TEST_FILES[@]}"; do
        if [[ -f "$file" ]]; then
            if [ "$debug" = true ]; then
                echo "Loading test: $file"
            fi
            l_args="$l_args -l $file"
            loaded_files+=("$file")
        fi
    done
    load_paths=""
    for path in "${PATHS[@]}"; do
        if [[ -d "$path" ]]; then
            load_paths="$load_paths(add-to-list 'load-path \"$path\") "
        fi
    done
    if [ "${#loaded_files[@]}" -eq 0 ]; then
        echo "No test files found"
        exit 1
    fi
    local require_exprs=""
    for pkg in "${REQUIRE_PACKAGES[@]}"; do
        require_exprs="$require_exprs(require '$pkg)"
    done
    emacs -batch \
          -l ert \
          -l package \
          --eval "(progn \
(package-initialize) \
$load_paths \
$require_exprs \
(require 'ert))" \
          $l_args \
          -f ert-run-tests-batch-and-exit || true
}

check_global_success () {
    # Check if all tests were successful
    if grep "Ran [0-9]\\+ tests" "$LOG_PATH" | tail -1 | grep -v "0 unexpected" > /dev/null; then
        echo -e "${RED}⨯ Tests FAILED!${NC}"
        # Only show actual test failures, not expected "Aborted" messages that are part of test cases
        grep -A 3 -B 3 "FAILED:" "$LOG_PATH" --color=always || true
    else
        echo -e "${YELLOW}✓ All tests PASSED as expected!${NC}"
        grep "Ran [0-9]\\+ tests" "$LOG_PATH" | tail -1
    fi

}

main "$@" 2>&1 | tee "$LOG_PATH"

check_global_success

# EOF