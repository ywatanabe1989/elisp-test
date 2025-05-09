#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-09 19:34:10 (ywatanabe)"
# File: ./tests/test-run-tests-script.sh

# Test script for run_tests.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
PARENT_DIR="$(dirname "$THIS_DIR")"
TEST_LOG="/tmp/test-run-tests-script.log"
echo "" > "$TEST_LOG"

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Helper function to run tests
run_test() {
    local test_name="$1"
    local command="$2"
    local expected_status="$3"
    local expected_output_pattern="$4"
    
    echo "Running test: $test_name" | tee -a "$TEST_LOG"
    echo "Command: $command" | tee -a "$TEST_LOG"
    
    # Run the command
    output=$(cd "$PARENT_DIR" && eval "$command" 2>&1)
    status=$?
    
    echo "Output: $output" >> "$TEST_LOG"
    echo "Exit status: $status" >> "$TEST_LOG"
    
    # Check exit status
    if [ "$status" -eq "$expected_status" ]; then
        status_check="PASS"
    else
        status_check="FAIL"
    fi
    
    # Check output pattern if provided
    if [ -n "$expected_output_pattern" ]; then
        if echo "$output" | grep -q "$expected_output_pattern"; then
            output_check="PASS"
        else
            output_check="FAIL"
        fi
    else
        output_check="SKIP"
    fi
    
    # Final result
    if [ "$status_check" = "PASS" ] && ([ "$output_check" = "PASS" ] || [ "$output_check" = "SKIP" ]); then
        echo -e "${GREEN}PASS${NC}: $test_name" | tee -a "$TEST_LOG"
        return 0
    else
        echo -e "${RED}FAIL${NC}: $test_name" | tee -a "$TEST_LOG"
        echo "Expected status: $expected_status, got: $status" | tee -a "$TEST_LOG"
        if [ "$output_check" = "FAIL" ]; then
            echo "Expected output pattern: $expected_output_pattern" | tee -a "$TEST_LOG"
            echo "Output: $output" | tee -a "$TEST_LOG"
        fi
        return 1
    fi
}

# Test cases
tests=(
    "Help option:./run_tests.sh -h:0:Usage:"
    "Single file:./run_tests.sh -s tests/test-elisp-test-variables.el:0:Running single test file"
    "Debug mode:./run_tests.sh -d:0:Running command"
)

# Run the tests
failures=0
for test in "${tests[@]}"; do
    IFS=':' read -r name command expected_status pattern <<< "$test"
    if ! run_test "$name" "$command" "$expected_status" "$pattern"; then
        ((failures++))
    fi
done

# Final summary
echo "" | tee -a "$TEST_LOG"
if [ "$failures" -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}" | tee -a "$TEST_LOG"
    exit 0
else
    echo -e "${RED}$failures tests failed!${NC}" | tee -a "$TEST_LOG"
    exit 1
fi

# EOF