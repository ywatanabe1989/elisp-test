#!/bin/bash
# -*- coding: utf-8 -*-
# Common utilities for test scripts

# Get script directory
THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"

# Color definitions
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Default settings
TEST_TIMEOUT=${TEST_TIMEOUT:-10}
ELISP_TEST_PATH=${ELISP_TEST_PATH:-"$HOME/.emacs.d/lisp/elisp-test"}
TESTS_DIR=${TESTS_DIR:-"$THIS_DIR/tests"}
DEBUG_MODE=${DEBUG_MODE:-false}
SINGLE_TEST_FILE=""

# Usage information
usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  -d, --debug               Enable debug output"
    echo "  -s, --single FILE         Run a single test file"
    echo "  -t, --tests-dir DIR       Directory containing elisp test files (default: $TESTS_DIR)"
    echo "  --elisp-test PATH         Loadpath to elisp-test.el (default: $ELISP_TEST_PATH)"
    echo "  --timeout SECONDS         Timeout for tests in seconds (default: ${TEST_TIMEOUT}s)"
    echo "  -j, --jobs N              Number of parallel jobs (default: auto)"
    echo "  -h, --help                Display this help message"
    echo
    echo "Example:"
    echo "  $0 ./tests"
    echo "  $0 --tests-dir /path/to/custom/tests"
    echo "  $0 --timeout 30"
    echo "  $0 -s tests/test-et-core-main.el"
    echo "  $0 -d"
}

# Function to run a single test file
run_single_test() {
    local test_file="$1"
    local unique_log="${LOG_PATH}.$(basename $test_file)"

    # Prepare command
    local emacs_cmd="emacs -Q --batch"
    # Add load paths
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$(pwd)\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$THIS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$TESTS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$(dirname $test_file)\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$ELISP_TEST_PATH\\\")\" "
    # Load elisp-test
    emacs_cmd+=" --eval \"(require 'elisp-test)\" "
    # Set debug level if needed
    if $DEBUG_MODE; then
        emacs_cmd+=" --eval \"(setq debug-on-error t)\" "
        emacs_cmd+=" --eval \"(setq debug-on-signal t)\" "
    fi
    # Run tests
    emacs_cmd+=" --eval \"(elisp-test-run \\\"$test_file\\\" $TEST_TIMEOUT t)\" "

    # Execute the command with unique log
    if $DEBUG_MODE; then
        echo -e "${YELLOW}Running command: $emacs_cmd${NC}" | tee -a "$unique_log"
        eval $emacs_cmd | tee -a "$unique_log" "$LOG_PATH"
    else
        eval $emacs_cmd >> "$unique_log" 2>&1
    fi

    local exit_status=$?
    if [ $exit_status -eq 0 ]; then
        echo -e "${GREEN}Test passed: $test_file${NC}" | tee -a "$LOG_PATH"
    else
        echo -e "${RED}Test failed: $test_file${NC}" | tee -a "$LOG_PATH"
    fi

    return $exit_status
}

# Parse command line arguments
parse_args() {
    TESTS_DIR_ARG=""
    while [[ $# -gt 0 ]]; do
        case $1 in
            --timeout)
                TEST_TIMEOUT="$2"
                shift 2
                ;;
            --elisp-test)
                ELISP_TEST_PATH="$2"
                shift 2
                ;;
            -d|--debug)
                DEBUG_MODE=true
                shift
                ;;
            -s|--single)
                SINGLE_TEST_FILE="$2"
                shift 2
                ;;
            -t|--tests-dir)
                TESTS_DIR_ARG="$2"
                shift 2
                ;;
            -j|--jobs)
                PARALLEL_JOBS="$2"
                shift 2
                ;;
            -h|--help)
                usage
                exit 0
                ;;
            *)
                TESTS_DIR_ARG="$1"
                shift
                ;;
        esac
    done

    # Determine the target to test
    if [ -n "$SINGLE_TEST_FILE" ]; then
        # Single test file mode
        TEST_TARGET="$SINGLE_TEST_FILE"
    else
        # Directory mode
        TEST_TARGET="${TESTS_DIR_ARG:-$TESTS_DIR}"
    fi
}