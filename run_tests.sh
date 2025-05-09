#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-09 18:19:42 (ywatanabe)"
# File: ./run_tests.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
echo > "$LOG_PATH"

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color
# ---------------------------------------

# Script to run elisp tests
TEST_TIMEOUT=10
ELISP_TEST_PATH="$HOME/.emacs.d/lisp/elisp-test"
TESTS_DIR="${2:-$THIS_DIR/tests}"

usage() {
    echo "Usage: $0"
    echo
    echo "Options:"
    echo "  --tests-dir|-t TESTS_DIR  Directory containing elisp test files (default: $TESTS_DIR)"
    echo "  --elisp-test PATH         Loadpath to elisp-test.el (default: $ELISP_TEST_PATH)"
    echo "  --timeout SECONDS         Timeout for tests in seconds (default: ${TEST_TIMEOUT}s)"
    echo "  -h, --help                Display this help message"
    echo
    echo "Example:"
    echo "  $0 ./tests"
    echo "  $0 --tests-dir /path/to/custom/tests"
    echo "  $0 ./tests --timeout 30"
    echo "  $0 ./tests --elisp-test ~/.emacs.d/elisp-test"
}

# Parse command line arguments
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
        -h|--help)
            usage
            exit 0
            ;;
        -t|--tests-dir)
            echo -e "${RED}Error: Unknown option $1${NC}" >&2
            usage
            exit 1
            ;;
        *)
            TESTS_DIR_ARG="$1"
            shift
            ;;
    esac
done

# Function to run all tests in a directory
run_tests_elisp() {
    local directory="$1"

    if [ -z "$directory" ]; then
        echo -e "${RED}Error: Test directory not specified${NC}" | tee -a "$LOG_PATH"
        usage
        return 1
    fi

    if [ ! -d "$directory" ]; then
        echo -e "${RED}Error: Directory '$directory' does not exist${NC}" | tee -a "$LOG_PATH"
        return 1
    fi

    echo "Running tests in $directory..."

    # Run each test file
    emacs -Q --batch \
        --eval "(add-to-list 'load-path \"$(pwd)\")" \
        --eval "(add-to-list 'load-path \"$THIS_DIR\")" \
        --eval "(add-to-list 'load-path \"$TESTS_DIR\")" \
        --eval "(add-to-list 'load-path \"$directory\")" \
        --eval "(add-to-list 'load-path \"$ELISP_TEST_PATH\")" \
        --eval "(require 'elisp-test)" \
        --eval "(elisp-test-run \"$directory\" $TEST_TIMEOUT t)" \
        >> "$LOG_PATH" 2>&1

    local exit_status=$?

    if [ $exit_status -eq 124 ] || [ $exit_status -eq 137 ]; then
        echo -e "${RED}Test execution timed out after ${TEST_TIMEOUT}s${NC}" | tee -a "$LOG_PATH"
        return $exit_status
    fi

    # Find reports created in the last minute
    local report_file=$(find "$THIS_DIR" -maxdepth 1 -mmin -0.1 -name "*ELISP-TEST-REPORT*" | head -n 1)

    if [ -f "$report_file" ]; then
        echo -e "${GREEN}Report created: $report_file${NC}" | tee -a "$LOG_PATH"
        cat "$report_file" >> "$LOG_PATH"
        return 0
    else
        echo -e "${RED}No test report was generated. Check for errors.${NC}" | tee -a "$LOG_PATH"
        return 1
    fi
}

# Execute tests and log output
run_tests_elisp "${TESTS_DIR_ARG:-$THIS_DIR/tests}" | tee -a "$LOG_PATH"
exit_code=${PIPESTATUS[0]}

if [ $exit_code -eq 0 ]; then
    echo -e "${GREEN}Tests completed successfully with exit code: $exit_code${NC}" | tee -a "$LOG_PATH"
else
    echo -e "${RED}Tests completed with errors. Exit code: $exit_code${NC}" | tee -a "$LOG_PATH"
fi

exit $exit_code

# EOF