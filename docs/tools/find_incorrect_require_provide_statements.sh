#!/bin/bash
# -*- coding: utf-8 -*-
# Timestamp: "2025-05-11 17:08:45 (ywatanabe)"
# File: ./.claude/tools/find_incorrect_require_provide_statements.sh

THIS_DIR="$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)"
LOG_PATH="$THIS_DIR/.$(basename $0).log"
echo > "$LOG_PATH"

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color
# ---------------------------------------

NC='\033[0m' # No Color

# ---------------------------------------
# Script to find incorrect require and provide statements in Emacs Lisp files

# Function to log messages
log_message() {
    local level=$1
    local message=$2
    local color=$NC

    case $level in
        "INFO") color=$GREEN ;;
        "WARNING") color=$YELLOW ;;
        "ERROR") color=$RED ;;
    esac

    echo -e "${color}[$level] $message${NC}"
    echo "[$level] $message" >> "$LOG_PATH"
}

# Function to check if provide statement matches filename
check_provide_statement() {
    local file_path=$1
    local filename=$(basename "$file_path" .el)
    local provide_statements=$(grep -o "(provide '[^)]*)" "$file_path" | sed "s/(provide '//g" | sed "s/)//g")

    if [ -z "$provide_statements" ]; then
        log_message "ERROR" "No provide statement in $file_path"
        return 1
    fi

    local main_provide=$(echo "$provide_statements" | head -1)
    if [[ "$main_provide" != "$filename" ]]; then
        log_message "ERROR" "Provide statement '$main_provide' doesn't match filename '$filename' in $file_path"
        return 1
    fi

    # Check for provides with slashes
    if echo "$provide_statements" | grep -q "/"; then
        log_message "WARNING" "Provide statement contains slash in $file_path: $(echo "$provide_statements" | grep "/")"
    fi

    return 0
}

# Function to check require statements
check_require_statements() {
    local file_path=$1
    local require_statements=$(grep -o "(require '[^)]*)" "$file_path" | sed "s/(require '//g" | sed "s/)//g")

    if [ -z "$require_statements" ]; then
        return 0
    fi

    # Check for requires with slashes
    if echo "$require_statements" | grep -q "/"; then
        log_message "WARNING" "Require statement contains slash in $file_path: $(echo "$require_statements" | grep "/")"
    fi

    return 0
}

# Main function
main() {
    local elisp_files

    # Find all Emacs Lisp files in the repository
    elisp_files=$(find "$THIS_DIR/.." -name "*.el" -type f -not -path "*/\.*")

    log_message "INFO" "Checking $(echo "$elisp_files" | wc -l) Emacs Lisp files"

    local errors=0
    local warnings=0

    for file in $elisp_files; do
        check_provide_statement "$file" || ((errors++))
        check_require_statements "$file" || ((warnings++))
    done

    if [ $errors -eq 0 ] && [ $warnings -eq 0 ]; then
        log_message "INFO" "All files passed validation"
    else
        log_message "INFO" "Found $errors errors and $warnings warnings. See $LOG_PATH for details."
    fi
}

# Execute main function
main "$@"

# EOF