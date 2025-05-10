#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-11 00:28:50 (ywatanabe)"
# File: ./run_tests.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
echo > "$LOG_PATH"

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color
# ---------------------------------------

BLACK='\033[0;30m'

# Unified test runner that supports multiple execution modes:
# - sequential: Run tests one by one (original run_tests.sh behavior)
# - parallel-gnu: Run tests using GNU parallel (original run_tests_parallel.sh behavior)
# - parallel-xargs: Run tests using xargs (original run_tests_simple_parallel.sh behavior)

# Constants and initialization
LATEST_FNAME="LATEST-ELISP-TEST.org"
TEMP_DIR="$THIS_DIR/.temp_test_outputs"
REPORTS_DIR="$THIS_DIR/.reports"
mkdir -p "$TEMP_DIR" "$REPORTS_DIR"

# Default settings
TEST_TIMEOUT=10
ELISP_TEST_PATH="$HOME/.emacs.d/lisp/elisp-test"
TESTS_DIR="$THIS_DIR/tests"
SINGLE_TEST_FILE=""
DEBUG_MODE=false
NO_REPORT=false
ONLY_LOADABLE_CHECK=false
CLEAN_REPORTS=false
MODE="sequential"  # Default mode

# Auto-detect parallel jobs
AVAILABLE_CORES=$(nproc 2>/dev/null || echo "4")  # Default to 4 if nproc not available
PARALLEL_JOBS=$(( AVAILABLE_CORES / 4 ))
if [ "$PARALLEL_JOBS" -lt 1 ]; then
    PARALLEL_JOBS=1  # Ensure at least one job
fi

# Echo functions with color
echo_success() { echo -e "${GREEN}SUCCESS: $1${NC}" | tee -a "$LOG_PATH"; }
echo_warn() { echo -e "${YELLOW}WARNING: $1${NC}" | tee -a "$LOG_PATH"; }
echo_error() { echo -e "${RED}ERROR: $1${NC}" | tee -a "$LOG_PATH"; }
echo_info() { echo -e "${BLACK}INFO: $1${NC}" | tee -a "$LOG_PATH"; }

# Display usage information
usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  -m, --mode MODE          Execution mode (sequential, parallel-gnu, parallel-xargs)"
    echo "                           Default: sequential, auto-switches to parallel when -j is used"
    echo "  -j, --jobs N             Number of parallel jobs (auto-selects parallel mode)"
    echo "  -d, --debug              Enable debug output"
    echo "  -s, --single FILE        Run a single test file"
    echo "  -t, --tests-dir DIR      Directory containing elisp test files (default: $TESTS_DIR)"
    echo "  -l, --only-loadable-check  Only check if the package is loadable"
    echo "  --no-report              Skip creating report files (useful for parallel runs)"
    echo "  --elisp-test PATH        Loadpath to elisp-test.el (default: $ELISP_TEST_PATH)"
    echo "  --timeout SECONDS        Timeout for tests in seconds (default: ${TEST_TIMEOUT}s)"
    echo "  -h, --help               Display this help message"
    echo
    echo "Examples:"
    echo "  $0                       # Run all tests sequentially"
    echo "  $0 -m parallel-gnu       # Run all tests in parallel using GNU parallel"
    echo "  $0 -m parallel-xargs     # Run all tests in parallel using xargs"
    echo "  $0 -s tests/test-file.el # Run a single test file"
    echo "  $0 -j 4                  # Use 4 parallel jobs"
    echo "  $0 -d                    # Enable debug output"
    exit 1
}

# Parse command line arguments
TESTS_DIR_ARG=""
while [[ $# -gt 0 ]]; do
    case $1 in
        -m|--mode)
            MODE="$2"
            shift 2
            ;;
        -j|--jobs)
            PARALLEL_JOBS="$2"
            shift 2
            ;;
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
        -l|--only-loadable-check)
            ONLY_LOADABLE_CHECK=true
            shift
            ;;
        --no-report)
            NO_REPORT=true
            shift
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

# Auto-select parallel mode if -j is specified but no mode is given
# Only do this when running multiple tests
if [ "$PARALLEL_JOBS" -gt 1 ] && [ "$MODE" = "sequential" ] && [ -z "$SINGLE_TEST_FILE" ]; then
    # Check if GNU parallel is available
    if command -v parallel &> /dev/null; then
        MODE="parallel-gnu"
        echo_info "Auto-selecting parallel-gnu mode because -j option was provided"
    else
        MODE="parallel-xargs"
        echo_info "Auto-selecting parallel-xargs mode because -j option was provided"
    fi
fi

# Validate mode selection
if [[ ! "$MODE" =~ ^(sequential|parallel-gnu|parallel-xargs)$ ]]; then
    echo_error "Invalid mode: $MODE"
    echo_error "Valid modes: sequential, parallel-gnu, parallel-xargs"
    exit 1
fi

# Function to check if package is loadable
is_package_loadable() {
    local this_package=$(basename "$THIS_DIR")
    local emacs_cmd="emacs -Q --batch"

    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$(pwd)\\\")\""
    emacs_cmd+=" --eval \"(require '$this_package)\""

    echo_info "Checking if package is loadable..."

    if [ "$DEBUG_MODE" = "true" ]; then
        echo_info "Command: $emacs_cmd"
        eval $emacs_cmd
    else
        eval $emacs_cmd >/dev/null 2>&1
    fi

    local exit_status=$?

    if [ $exit_status -eq 0 ]; then
        echo_success "Package '$this_package' is loadable"
    else
        echo_error "Package '$this_package' is not loadable"
        exit 1
    fi

    if [ "$ONLY_LOADABLE_CHECK" = "true" ]; then
        echo_success "Only loadable check executed"
        exit 0
    fi

    return $exit_status
}

# Function to run a single test file in sequential mode
run_single_test_sequential() {
    local test_file="$1"
    local test_file_basename=$(basename "$test_file")
    local report_path=""

    if [ "$NO_REPORT" = "true" ]; then
        report_path="/dev/null"
    else
        # Store individual test reports in the hidden reports directory
        report_path="${REPORTS_DIR}/ELISP-TEST-REPORT-${test_file_basename}-$(date +%Y%m%d-%H%M%S).org"
    fi

    # Add all directories to load path
    local emacs_cmd="emacs -Q --batch"
    # Set environment variable to skip PDF generation for all runs
    emacs_cmd+=" --eval \"(setenv \\\"ELISP_TEST_SKIP_PDF\\\" \\\"1\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$THIS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$ELISP_TEST_PATH\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$(dirname "$test_file")\\\")\" "

    # Add all test subdirectories to load path for proper test loading
    emacs_cmd+=" --eval \"(dolist (path (directory-files-recursively \\\"$THIS_DIR/tests\\\" \\\"^[^.].*\\\" t)) (add-to-list 'load-path path))\" "

    # Load elisp-test
    emacs_cmd+=" --eval \"(require 'elisp-test)\" "

    # Set report path
    emacs_cmd+=" --eval \"(setq elisp-test-results-org-path \\\"$report_path\\\")\" "
    emacs_cmd+=" --eval \"(elisp-test-run \\\"$test_file\\\" $TEST_TIMEOUT t)\" "

    # Execute the command
    local unique_log="${TEMP_DIR}/${test_file_basename}.log"

    if [ "$DEBUG_MODE" = "true" ]; then
        echo_info "Running command: $emacs_cmd"
        eval $emacs_cmd 2>&1 | tee -a "$unique_log" "$LOG_PATH"
    else
        eval $emacs_cmd > "$unique_log" 2>&1
    fi

    local exit_status=$?

    if [ $exit_status -eq 0 ]; then
        echo_success "Test passed: $test_file"
    else
        echo_error "Test failed: $test_file (exit code $exit_status)"
    fi

    return $exit_status
}

# Function to run a single test file for parallel mode (stores result in temp files)
run_single_test_parallel() {
    local test_file="$1"
    local test_file_basename=$(basename "$test_file")
    local unique_log="${TEMP_DIR}/${test_file_basename}.log"

    # Suppress report generation by redirecting to /dev/null
    local report_path="/dev/null"

    # Keep track of test name for final report
    echo "Running test: $test_file_basename" > "$unique_log"

    # Add all directories to load path
    local emacs_cmd="emacs -Q --batch"
    # Set environment variable to skip PDF generation for all runs
    emacs_cmd+=" --eval \"(setenv \\\"ELISP_TEST_SKIP_PDF\\\" \\\"1\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$THIS_DIR\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$ELISP_TEST_PATH\\\")\" "
    emacs_cmd+=" --eval \"(add-to-list 'load-path \\\"$(dirname "$test_file")\\\")\" "

    # Add all test subdirectories to load path for proper test loading
    emacs_cmd+=" --eval \"(dolist (path (directory-files-recursively \\\"$THIS_DIR/tests\\\" \\\"^[^.].*\\\" t)) (add-to-list 'load-path path))\" "

    # Load elisp-test
    emacs_cmd+=" --eval \"(require 'elisp-test)\" "

    # Set report path to /dev/null to suppress individual report files
    emacs_cmd+=" --eval \"(setq elisp-test-results-org-path \\\"$report_path\\\")\" "
    emacs_cmd+=" --eval \"(elisp-test-run \\\"$test_file\\\" $TEST_TIMEOUT t)\" "

    # Execute the command
    if [ "$DEBUG_MODE" = "true" ]; then
        echo -e "${YELLOW}Running command: $emacs_cmd${NC}" >> "$unique_log"
        eval $emacs_cmd 2>&1 | tee -a "$unique_log" "$LOG_PATH"
    else
        eval $emacs_cmd >> "$unique_log" 2>&1
    fi

    local exit_status=$?

    # Extract test results from output
    if [ $exit_status -eq 0 ]; then
        echo -e "${GREEN}Test passed: $test_file${NC}" | tee -a "$LOG_PATH"
        echo "PASSED" > "${TEMP_DIR}/${test_file_basename}.result"
    else
        echo -e "${RED}Test failed: $test_file (exit code $exit_status)${NC}" | tee -a "$LOG_PATH"
        echo "FAILED" > "${TEMP_DIR}/${test_file_basename}.result"
    fi

    return $exit_status
}

# Create a unified test report (org only, no PDF generation)
create_hidden_report() {
    local timestamp=$(date +%Y%m%d-%H%M%S)

    # Calculate success rate for filename
    local passed=0
    local failed=0

    # Check if there are result files in the temp directory
    if [ -d "$TEMP_DIR" ] && [ "$(find "$TEMP_DIR" -name "*.result" 2>/dev/null | wc -l)" -gt 0 ]; then
        passed=$(grep -l "PASSED" ${TEMP_DIR}/*.result 2>/dev/null | wc -l)
        failed=$(grep -l "FAILED" ${TEMP_DIR}/*.result 2>/dev/null | wc -l)
    fi

    local total=$((passed + failed))
    local success_rate=0

    if [ "$total" -gt 0 ]; then
        success_rate=$(awk "BEGIN {printf \"%.0f\", ($passed * 100.0 / $total)}" 2>/dev/null || echo "0")
    fi

    # Create filename with stats
    local stats_suffix="${passed}-PASSED-${total}-TOTAL-${success_rate}-PERCENT"
    # local temp_report="${REPORTS_DIR}/ELISP-TEST-REPORT-${timestamp}-${stats_suffix}.org"
    local temp_report="${REPORTS_DIR}/.ELISP-TEST-REPORT-${timestamp}-${stats_suffix}.org"
    local semifinal_report="${THIS_DIR}/.ELISP-TEST-REPORT-${timestamp}-${stats_suffix}.org"

    # Set an environment variable to indicate we don't want PDFs generated
    export ELISP_TEST_SKIP_PDF=1

    # Format success rate for display (with decimal point)
    local success_rate_display="0.0"
    if [ "$total" -gt 0 ]; then
        success_rate_display=$(awk "BEGIN {printf \"%.1f\", ($passed * 100.0 / $total)}" 2>/dev/null || echo "0.0")
    fi

    # Create report header
    cat > "$temp_report" <<EOL
#+TITLE: Elisp Test Report
#+DATE: $(date)
#+AUTHOR: elisp-test unified runner (mode: $MODE)

* Test Results Summary

** Summary

- Test files processed: $total
- Passed: $passed
- Failed: $failed
- Success Rate: ${success_rate_display}%

EOL

    # List all the test files with their status
    echo "** Test Details" >> "$temp_report"
    echo "" >> "$temp_report"

    # Function to extract test function names from a file
extract_test_functions() {
    local file="$1"
    if [ -f "$file" ]; then
        # Use grep to find ert-deftest patterns and extract function names
        grep -o '(ert-deftest [[:alnum:]_-]\+' "$file" | sed 's/(ert-deftest //'
    fi
}

# Add passed tests first with test function names
    if [ $passed -gt 0 ]; then
        echo "*** Passed Tests" >> "$temp_report"
        echo "" >> "$temp_report"
        for result in $(grep -l "PASSED" ${TEMP_DIR}/*.result 2>/dev/null); do
            test_file_basename=$(basename "$result" .result)
            # Find the actual test file (search in both tests dir and the whole project)
            test_file=""
            if [[ "$test_file_basename" == *.el ]]; then
                # If the basename already has .el extension
                test_file=$(find "$THIS_DIR" -name "${test_file_basename}" -type f | head -1)
            else
                # Otherwise try with .el extension
                test_file=$(find "$THIS_DIR" -name "${test_file_basename}.el" -type f | head -1)
            fi
            echo "- ${test_file_basename}" >> "$temp_report"

            # Extract and list test functions if the file exists
            if [ -f "$test_file" ]; then
                # Get test functions
                local test_funcs=$(extract_test_functions "$test_file")

                if [ -n "$test_funcs" ]; then
                    echo "  - Functions:" >> "$temp_report"
                    while read -r func_name; do
                        if [ -n "$func_name" ]; then
                            echo "    - ${func_name}" >> "$temp_report"
                        fi
                    done <<< "$test_funcs"
                else
                    # Debug output if no functions found
                    if [ "$DEBUG_MODE" = "true" ]; then
                        echo "  - No test functions found in $test_file" >> "$temp_report"
                        echo "    File path: $test_file" >> "$temp_report"
                    fi
                fi
            elif [ "$DEBUG_MODE" = "true" ]; then
                echo "  - Test file not found: $test_file_basename" >> "$temp_report"
            fi
        done
        echo "" >> "$temp_report"
    fi

    # Add failed tests with more details
    if [ $failed -gt 0 ]; then
        echo "*** Failed Tests" >> "$temp_report"
        echo "" >> "$temp_report"
        for result in $(grep -l "FAILED" ${TEMP_DIR}/*.result 2>/dev/null); do
            test_file_basename=$(basename "$result" .result)
            # Find the actual test file (search in both tests dir and the whole project)
            test_file=""
            if [[ "$test_file_basename" == *.el ]]; then
                # If the basename already has .el extension
                test_file=$(find "$THIS_DIR" -name "${test_file_basename}" -type f | head -1)
            else
                # Otherwise try with .el extension
                test_file=$(find "$THIS_DIR" -name "${test_file_basename}.el" -type f | head -1)
            fi
            echo "- ${test_file_basename}" >> "$temp_report"

            # Extract and list test functions if the file exists
            if [ -f "$test_file" ]; then
                # Get test functions
                local test_funcs=$(extract_test_functions "$test_file")

                if [ -n "$test_funcs" ]; then
                    echo "  - Functions:" >> "$temp_report"
                    while read -r func_name; do
                        if [ -n "$func_name" ]; then
                            echo "    - ${func_name}" >> "$temp_report"
                        fi
                    done <<< "$test_funcs"
                else
                    # Debug output if no functions found
                    if [ "$DEBUG_MODE" = "true" ]; then
                        echo "  - No test functions found in $test_file" >> "$temp_report"
                        echo "    File path: $test_file" >> "$temp_report"
                    fi
                fi
            elif [ "$DEBUG_MODE" = "true" ]; then
                echo "  - Test file not found: $test_file_basename" >> "$temp_report"
            fi

            # Add error details
            if [ -f "${TEMP_DIR}/${test_file_basename}.log" ]; then
                # Extract relevant error information if available
                echo "  - Error details:" >> "$temp_report"
                grep -A 5 "ERROR" "${TEMP_DIR}/${test_file_basename}.log" | head -n 3 | sed 's/^/    /' >> "$temp_report"
            fi
        done
    fi

    # If no tests were found, add a note
    if [ $total -eq 0 ]; then
        echo "** Note" >> "$temp_report"
        echo "" >> "$temp_report"
        echo "No test results were found. This might be due to:" >> "$temp_report"
        echo "" >> "$temp_report"
        echo "1. No tests were run" >> "$temp_report"
        echo "2. Tests were run but results were not properly saved" >> "$temp_report"
        echo "3. There was an error in the test execution" >> "$temp_report"
        echo "" >> "$temp_report"
        echo "Please check the log file for more details." >> "$temp_report"
    fi

    # Copy the report from hidden directory to the main directory
    cp "$temp_report" "$semifinal_report"

    # Create symlink to latest report
    ln -sf "$semifinal_report" "$THIS_DIR/$LATEST_FNAME"
    echo_success "Created report: $semifinal_report"
    echo_success "Symlinked to: $LATEST_FNAME"

    return $((failed > 0))
}

# Run tests in sequential mode
run_tests_sequential() {
    local test_files="$1"
    local exit_status=0

    echo_info "Running tests sequentially..."

    for test_file in $test_files; do
        run_single_test_sequential "$test_file"
        local result=$?

        # Store result for reporting
        local test_file_basename=$(basename "$test_file")
        if [ $result -eq 0 ]; then
            echo "PASSED" > "${TEMP_DIR}/${test_file_basename}.result"
        else
            echo "FAILED" > "${TEMP_DIR}/${test_file_basename}.result"
            exit_status=1
        fi
    done

    return $exit_status
}

# Run tests in parallel using GNU parallel
run_tests_parallel_gnu() {
    local test_files="$1"

    # Export functions and variables for parallel
    export -f run_single_test_parallel
    export -f echo_success
    export -f echo_warn
    export -f echo_error
    export -f echo_info
    export THIS_DIR TEMP_DIR LOG_PATH DEBUG_MODE ELISP_TEST_PATH GREEN YELLOW RED BLACK NC TEST_TIMEOUT

    echo_info "Running tests in parallel using GNU parallel ($PARALLEL_JOBS jobs)..."

    # Use parallel to run tests
    echo "$test_files" | parallel -j $PARALLEL_JOBS --timeout 60 "run_single_test_parallel {}"
    local parallel_exit=$?

    return $parallel_exit
}

# Run tests in parallel using xargs
run_tests_parallel_xargs() {
    local test_files="$1"
    local exit_status=0

    echo_info "Running tests in parallel using xargs ($PARALLEL_JOBS jobs)..."

    # Create a results directory for test outcomes
    mkdir -p "$TEMP_DIR/results"

    # Function to run a single test
    run_test_xargs() {
        local test_file="$1"
        local basename=$(basename "$test_file")
        local result_file="${TEMP_DIR}/results/${basename}.result"

        # Run test with the sequential runner but suppress reports
        run_single_test_sequential "$test_file" > /dev/null 2>&1
        local status=$?

        # Record result
        if [ $status -eq 0 ]; then
            echo "PASSED" > "$result_file"
            echo "PASSED: $test_file"
        else
            echo "FAILED" > "$result_file"
            echo "FAILED: $test_file (exit code $status)"
            exit_status=1
        fi
    }

    # Export the function for xargs
    export -f run_test_xargs
    export -f run_single_test_sequential
    export -f echo_success
    export -f echo_warn
    export -f echo_error
    export -f echo_info
    export THIS_DIR TEMP_DIR LOG_PATH DEBUG_MODE ELISP_TEST_PATH GREEN YELLOW RED BLACK NC TEST_TIMEOUT NO_REPORT

    # Process each test file using xargs
    echo "$test_files" | xargs -P "$PARALLEL_JOBS" -I{} bash -c "run_test_xargs {}"

    # Count results
    local passed=$(grep -l "PASSED" ${TEMP_DIR}/results/*.result 2>/dev/null | wc -l)
    local failed=$(grep -l "FAILED" ${TEMP_DIR}/results/*.result 2>/dev/null | wc -l)

    echo_info "Tests completed. Passed: $passed, Failed: $failed"

    # Store results for reporting
    for result_file in ${TEMP_DIR}/results/*.result; do
        if [ -f "$result_file" ]; then
            cp "$result_file" "${TEMP_DIR}/$(basename "$result_file")"
        fi
    done

    return $exit_status
}

# Cleanup old report files (older than 24 hours)
cleanup_old_reports() {
    # Only clean up reports if not in debug mode
    if [ "$DEBUG_MODE" = "false" ]; then
        echo_info "Cleaning up old report files (older than 24 hours)..."

        # Keep LATEST-ELISP-TEST.org and any reports from the last 24 hours
        find "$THIS_DIR" -maxdepth 1 -name "ELISP-TEST-REPORT-*.org" -type f -mtime +1 -not -name "LATEST-ELISP-TEST.org" -delete

        # Clean up any LaTeX-related files older than 24 hours
        find "$THIS_DIR" -maxdepth 1 -name "*.pdf" -type f -mtime +1 -delete
        find "$THIS_DIR" -maxdepth 1 -name "*.aux" -type f -mtime +1 -delete
        find "$THIS_DIR" -maxdepth 1 -name "*.log" -type f -mtime +1 -delete
        find "$THIS_DIR" -maxdepth 1 -name "*.toc" -type f -mtime +1 -delete
        find "$THIS_DIR" -maxdepth 1 -name "*.fls" -type f -mtime +1 -delete
        find "$THIS_DIR" -maxdepth 1 -name "*.fdb_latexmk" -type f -mtime +1 -delete
        find "$THIS_DIR" -maxdepth 1 -name "*.out" -type f -mtime +1 -delete

        # Always clean up the hidden reports directory
        find "$REPORTS_DIR" -type f -delete
    fi
}

# # Cleanup all report files except the most recent one
# cleanup_reports_except_for_the_last_one() {
#     echo_info "Cleaning up report files, keeping only the most recent one..."

#     # Find the most recent report file by modification time
#     local latest_report=$(find "$THIS_DIR" -maxdepth 1 -name "ELISP-TEST-REPORT-*.org" -type f -not -name "*test-script*.org" | xargs ls -t 2>/dev/null | head -1)

#     if [ -n "$latest_report" ]; then
#         echo_info "Keeping latest report: $(basename "$latest_report")"

#         # Remove all report files except the latest one
#         find "$THIS_DIR" -maxdepth 1 -name "ELISP-TEST-REPORT-*.org" -type f ! -name "$(basename "$latest_report")" -delete

#         # Make sure LATEST-ELISP-TEST.org symlink points to the most recent report
#         ln -sf "$latest_report" "$THIS_DIR/$LATEST_FNAME"
#     else
#         echo_info "No report files found to keep"
#     fi

#     # Remove any LaTeX-related files
#     find "$THIS_DIR" -maxdepth 1 -name "*.aux" -delete
#     find "$THIS_DIR" -maxdepth 1 -name "*.log" -delete
#     find "$THIS_DIR" -maxdepth 1 -name "*.toc" -delete
#     find "$THIS_DIR" -maxdepth 1 -name "*.pdf" -delete
#     find "$THIS_DIR" -maxdepth 1 -name "*.fls" -delete
#     find "$THIS_DIR" -maxdepth 1 -name "*.fdb_latexmk" -delete
#     find "$THIS_DIR" -maxdepth 1 -name "*.out" -delete

#     # Clean up any temporary directories
#     rm -rf "$TEMP_DIR"

#     # Clean all files in the hidden reports directory
#     find "$REPORTS_DIR" -type f -delete

#     echo_success "Report files have been cleaned up, keeping only the most recent one."
# }


# Add this function to run_tests.sh
cleanup_all_reports_except_latest() {
    # Get the most recent report file
    local latest=$(find "$THIS_DIR" -maxdepth 1 -name "ELISP-TEST-REPORT-*.org" -type f | sort -r | head -1)

    if [ -n "$latest" ]; then
        # Remove all other report files
        find "$THIS_DIR" -maxdepth 1 -name "ELISP-TEST-REPORT-*.org" -type f ! -name "$(basename "$latest")" -delete

        # Update the symlink
        ln -sf "$latest" "$THIS_DIR/$LATEST_FNAME"

        # Remove PDF files except those related to the latest report
        local base_name=$(basename "$latest" .org)
        find "$THIS_DIR" -maxdepth 1 -name "ELISP-TEST-REPORT-*.pdf" -type f ! -name "${base_name}.pdf" -delete
    fi
}


# # Update this function to properly include metrics in the final report
# create_final_report() {
#     # Find the most recent hidden report
#     local latest_hidden=$(find "$THIS_DIR" -maxdepth 1 -name ".ELISP-TEST-REPORT-*.org" -type f | sort -r | head -1)

#     if [ -n "$latest_hidden" ]; then
#         # Get basename and remove the leading dot
#         local hidden_fname=$(basename "$latest_hidden")
#         local visible_fname="${hidden_fname#.}"

#         # Create final report with visible name in same directory
#         local final_report="$THIS_DIR/$visible_fname"

#         # Make visible by copying/renaming
#         cp "$latest_hidden" "$final_report"

#         # Update the symlink to point to the final report
#         ln -sf "$final_report" "$THIS_DIR/$LATEST_FNAME"

#         # Clean up old reports
#         find "$THIS_DIR" -maxdepth 1 -name "ELISP-TEST-REPORT-*.org" -type f -not -name "$visible_fname" -delete
#         find "$THIS_DIR" -maxdepth 1 -name ".ELISP-TEST-REPORT-*.org" -type f -delete

#         echo_success "Created report: $final_report"
#     fi
# }


# Main function
main() {
    # Clean up old reports
    cleanup_old_reports

    # Check if package is loadable
    is_package_loadable

    # Determine which tests to run
    if [ -n "$SINGLE_TEST_FILE" ]; then
        if [ -f "$SINGLE_TEST_FILE" ]; then
            TEST_FILES="$SINGLE_TEST_FILE"
        else
            echo_error "Test file not found: $SINGLE_TEST_FILE"
            exit 1
        fi
    else
        # Find all test files
        TEST_PATH="${TESTS_DIR_ARG:-$TESTS_DIR}"
        if [ -d "$TEST_PATH" ]; then
            TEST_FILES=$(find "$TEST_PATH" -name "test-*.el" -type f)
        else
            echo_error "Test directory not found: $TEST_PATH"
            exit 1
        fi
    fi

    # Count test files
    TEST_COUNT=$(echo "$TEST_FILES" | wc -w)
    echo_info "Found $TEST_COUNT test files"

    if [ "$TEST_COUNT" -eq 0 ]; then
        echo_error "No test files found"
        exit 1
    fi

    # Run tests based on selected mode
    if [ "$MODE" = "sequential" ]; then
        run_tests_sequential "$TEST_FILES"
    elif [ "$MODE" = "parallel-gnu" ]; then
        # Check if GNU parallel is installed
        if ! command -v parallel &> /dev/null; then
            echo_error "GNU parallel is not installed. Please install it or use another mode."
            exit 1
        fi
        run_tests_parallel_gnu "$TEST_FILES"
    elif [ "$MODE" = "parallel-xargs" ]; then
        run_tests_parallel_xargs "$TEST_FILES"
    fi

    local exit_status=$?

    # Create unified report if not in single test mode or no-report mode
    if [ "$NO_REPORT" = "false" ]; then
        create_hidden_report
        # sleep 0.5
        # create_final_report
    fi

    # Clean up temp directories if not in debug mode
    if [ "$DEBUG_MODE" = "false" ]; then
        rm -rf "$TEMP_DIR"
        find "$REPORTS_DIR" -type f -delete
    fi

    # # Handle clean reports option
    # if [ "$CLEAN_REPORTS" = "true" ]; then
    #     cleanup_reports_except_for_the_last_one
    #     exit 0
    # fi


    # create_final_report

    # Display final result
    if [ $exit_status -eq 0 ]; then
        echo_success "All tests completed successfully"
        # sleep 5
        # cleanup_reports_except_for_the_last_one
        exit 0
    else
        echo_error "Tests completed with some failures"
        # sleep 5
        # cleanup_reports_except_for_the_last_one
        exit 1
    fi
}

# Execute main function
main

# EOF