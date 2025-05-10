#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-10 20:44:20 (ywatanabe)"
# File: ./run_tests.sh

# Source common test functions
THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
source "$THIS_DIR/test_common.sh"

# Clear log file
echo > "$LOG_PATH"

# Parse command-line arguments
parse_args "$@"

# Function to run tests
run_tests_elisp() {
    local target="$1"
    local is_single_file=false

    if [ -z "$target" ]; then
        echo -e "${RED}Error: Test target not specified${NC}" | tee -a "$LOG_PATH"
        usage
        return 1
    fi

    # Check if target is a file or directory
    if [ -f "$target" ]; then
        echo "Running single test file: $target..."
        is_single_file=true
    elif [ -d "$target" ]; then
        echo "Running tests in directory: $target..."
    else
        echo -e "${RED}Error: Target '$target' does not exist${NC}" | tee -a "$LOG_PATH"
        return 1
    fi

    # Prepare command
    local emacs_cmd="emacs -Q --batch"

    # Add load paths
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$(pwd)\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$THIS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$TESTS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$target\\\")\" "
    emacs_cmd+=" --eval \"(dolist (path (directory-files-recursively \\\"$target\\\" \\\"^[^.].*\\\" t)) (add-to-list 'load-path path))\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$ELISP_TEST_PATH\\\")\" "

    # Load elisp-test
    emacs_cmd+=" --eval \"(require 'elisp-test)\" "

    # Set debug level if needed
    if $DEBUG_MODE; then
        emacs_cmd+=" --eval \"(setq debug-on-error t)\" "
        emacs_cmd+=" --eval \"(setq debug-on-signal t)\" "
    fi

    # Run tests
    emacs_cmd+=" --eval \"(elisp-test-run \\\"$target\\\" $TEST_TIMEOUT t)\" "

    # Execute the command
    if $DEBUG_MODE; then
        # Show command if in debug mode
        echo -e "${YELLOW}Running command: $emacs_cmd${NC}" | tee -a "$LOG_PATH"
        # Execute with output to terminal in debug mode
        eval $emacs_cmd | tee -a "$LOG_PATH"
    else
        # Execute quietly in normal mode
        eval $emacs_cmd >> "$LOG_PATH" 2>&1
    fi

    local exit_status=$?

    if [ $exit_status -eq 124 ] || [ $exit_status -eq 137 ]; then
        echo -e "${RED}Test execution timed out after ${TEST_TIMEOUT}s${NC}" | tee -a "$LOG_PATH"
        return $exit_status
    fi

    # Find reports created in the last minute
    local report_file=$(find "$THIS_DIR" -maxdepth 1 -mmin -0.1 -name "*ELISP-TEST-REPORT*" | head -n 1)

    # if [ -f "$report_file" ]; then
    #     echo -e "${GREEN}Report created: $report_file${NC}" | tee -a "$LOG_PATH"

    #     # Only display report content in debug mode
    #     if $DEBUG_MODE; then
    #         cat "$report_file" | tee -a "$LOG_PATH"
    #     else
    #         cat "$report_file" >> "$LOG_PATH"
    #     fi

    #     return 0
    # else
    #     echo -e "${RED}No test report was generated. Check for errors.${NC}" | tee -a "$LOG_PATH"
    #     return 1
    # fi
}

# Determine the target to test
if [ -n "$SINGLE_TEST_FILE" ]; then
    # Single test file mode
    TEST_TARGET="$SINGLE_TEST_FILE"
else
    # Directory mode
    TEST_TARGET="${TESTS_DIR_ARG:-$THIS_DIR/tests}"
fi

# Execute tests and log output
run_tests_elisp "$TEST_TARGET" | tee -a "$LOG_PATH"
exit_code=${PIPESTATUS[0]}

if [ $exit_code -eq 0 ]; then
    echo -e "${GREEN}Tests completed successfully with exit code: $exit_code${NC}" | tee -a "$LOG_PATH"
else
    echo -e "${RED}Tests completed with errors. Exit code: $exit_code${NC}" | tee -a "$LOG_PATH"
fi

exit $exit_code

# EOF