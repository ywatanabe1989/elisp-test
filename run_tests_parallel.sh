#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-10 18:46:16 (ywatanabe)"
# File: ./run_tests_parallel.sh

# Source common test functions
THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
source "$THIS_DIR/test_common.sh"

# Clear log file
echo > "$LOG_PATH"

# Set the number of parallel jobs
PARALLEL_JOBS=${PARALLEL_JOBS:-$(nproc)}  # Use number of available cores, or env variable if set

# Parse command-line arguments
parse_args "$@"

# Replace the run_tests_elisp function for directory handling to use parallel
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
        run_single_test "$target"
        return $?
    elif [ -d "$target" ]; then
        echo "Running tests in directory: $target using $PARALLEL_JOBS cores..."
        # Find all test files
        local test_files=$(find "$target" -name "test-*.el" -type f)
        
        # Export functions and variables for parallel execution
        export -f run_single_test
        export THIS_DIR LOG_PATH DEBUG_MODE TESTS_DIR ELISP_TEST_PATH TEST_TIMEOUT
        
        # Run tests in parallel
        echo "$test_files" | parallel -j $PARALLEL_JOBS bash -c "run_single_test {}"

        # Combine reports or summarize results
        echo -e "${GREEN}All parallel test executions completed${NC}" | tee -a "$LOG_PATH"
        return 0
    else
        echo -e "${RED}Error: Target '$target' does not exist${NC}" | tee -a "$LOG_PATH"
        return 1
    fi
}

# Add a new function to run single test file
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