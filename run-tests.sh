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
    "$THIS_DIR/src/"
    "$THIS_DIR/src/core/"
    "$THIS_DIR/src/util/"
    "$THIS_DIR/src/ui/"
)
SOURCE_FILES=("$THIS_DIR"/*.el "$THIS_DIR"/src/*/*.el)
TEST_FILES=("$THIS_DIR"/tests/test-*.el)
REQUIRE_PACKAGES=(org cl-lib)

# Color definitions
YELLOW='\033[1;33m'
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

usage() {
    echo "Usage: $0 [-d|--debug] [-s|--single TEST_FILE] [-h|--help]"
    echo "Runs tests for this project"
    echo
    echo "Options:"
    echo "  -d, --debug    Enable debug output"
    echo "  -s, --single   Run a single test file"
    echo "  -h, --help     Display this help message"
    exit 1
}
main() {
    local debug=false
    local single_test=""
    while [[ $# -gt 0 ]]; do
        case $1 in
            -d|--debug) debug=true; shift ;;
            -s|--single) 
                if [[ -z "$2" || "$2" == -* ]]; then
                    echo "Error: -s|--single option requires a test file path"
                    usage
                fi
                single_test="$2"
                shift 2 
                ;;
            -h|--help) usage ;;
            *) echo "Unknown option: $1"; usage ;;
        esac
    done
    local l_args=""
    local loaded_files=()
    
    # Let's load everything in the right order to handle dependencies
    
    # First load the compatibility layer
    compat_file="$THIS_DIR/src/compat.el"
    if [[ -f "$compat_file" ]]; then
        if [ "$debug" = true ]; then
            echo "Loading compatibility layer: $compat_file"
        fi
        l_args="$l_args -l $compat_file"
        loaded_files+=("$compat_file")
    fi
    
    # Then load the main elisp-test.el file
    main_file="$THIS_DIR/elisp-test.el"
    if [[ -f "$main_file" ]]; then
        if [ "$debug" = true ]; then
            echo "Loading main file: $main_file"
        fi
        l_args="$l_args -l $main_file"
        loaded_files+=("$main_file")
    fi
    
    # Load tests - either a single test file or all test files
    if [[ -n "$single_test" ]]; then
        if [[ -f "$single_test" ]]; then
            if [ "$debug" = true ]; then
                echo "Loading single test: $single_test"
            fi
            l_args="$l_args -l $single_test"
            loaded_files+=("$single_test")
        else
            echo "Error: Test file not found: $single_test"
            exit 1
        fi
    else
        for file in "${TEST_FILES[@]}"; do
            if [[ -f "$file" ]]; then
                if [ "$debug" = true ]; then
                    echo "Loading test: $file"
                fi
                l_args="$l_args -l $file"
                loaded_files+=("$file")
            fi
        done
    fi
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