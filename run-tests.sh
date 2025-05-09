#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-09 14:41:28 (ywatanabe)"
# File: ./run-tests.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
touch "$LOG_PATH" >/dev/null 2>&1


# Script to run elisp tests
TEST_TIMEOUT=10
ELISP_TEST_PATH="$HOME/.emacs.d/lisp/elisp-test"

# Function to run all tests in a directory
run_tests_elisp() {
    local directory="$1"
    local test_num=0
    local failed=0

    echo "Running tests in $directory..."

    # Run each test file
    emacs -Q --batch \
        --eval "(add-to-list 'load-path \"$(pwd)\")" \
        --eval "(add-to-list 'load-path \"$THIS_DIR\")" \
        --eval "(add-to-list 'load-path \"$TEST_DIR\")" \
        --eval "(add-to-list 'load-path \"$directory\")" \
        --eval "(add-to-list 'load-path \"$ELISP_TEST_PATH\")" \
        --eval "(require 'elisp-test)" \
        --eval "(elisp-test-run \"$directory\" $TEST_TIMEOUT t)"
}

run_tests_elisp "$1"

# EOF