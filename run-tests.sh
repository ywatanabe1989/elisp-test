#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-09 14:02:57 (ywatanabe)"
# File: ./run-tests.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
LOG_DIR="$THIS_DIR/logs"
touch "$LOG_PATH" >/dev/null 2>&1


# (elisp-test-run "/home/ywatanabe/.dotfiles/.emacs.d/lisp/elisp-test/tests" 10 t)

# Script to run elisp tests
TEST_DIR="$THIS_DIR/tests"
TEST_TIMEOUT=10

# Create logs directory
mkdir -p "$LOG_DIR"

# Function to run all tests in a directory
run_all_tests() {
    local directory="$1"
    local test_num=0
    local failed=0

    echo "Running tests in $directory..."
    
    # Run each test file
    emacs -Q --batch \
        --eval "(add-to-list 'load-path \"$THIS_DIR\")" \
        --eval "(add-to-list 'load-path \"$TEST_DIR\")" \
        --eval "(require 'elisp-test)" \
        --eval "(elisp-test-run \"$directory\" $TEST_TIMEOUT t)" 
}

run_all_tests "$TEST_DIR"

# EOF