# Improvements to elisp-test Framework

## Summary of Changes

We've enhanced the elisp-test framework to provide better handling of test execution, improved reporting, and more flexibility in how tests are run.

### Key Improvements

1. **Consolidated Test Reporting**
   - All test results are now combined into a single comprehensive report
   - Reports include test file paths, test names, and detailed error information
   - New option `elisp-test-generate-per-directory-reports` to generate separate reports for each directory

2. **Enhanced Run Scenarios**
   - Better handling of tests when called in a buffer (runs tests from current buffer)
   - Improved handling of dired mode with marked files
   - Added pattern-based test selection
   - Single and directory-based test execution

3. **Command-line Integration**
   - New `elisp-test-cli.el` for running tests from command line
   - Support for `emacs -Q --batch` with command-line arguments:
     - `--test-file` - Run a specific test file
     - `--test-dir` - Run all tests in a directory
     - `--pattern` - Run tests matching a pattern
     - `--timeout` - Set test timeout in seconds

4. **Improved Shell Script**
   - Enhanced `run-tests.sh` to use the new CLI
   - Better output formatting with color coding
   - Improved error handling and reporting
   - Added timing information for test runs
   - Support for verbose and quiet modes

## Usage Examples

### Running from Shell

```bash
# Run all tests
./run-tests.sh

# Run tests with verbose output
./run-tests.sh -v

# Run a single test file
./run-tests.sh -s tests/test-et-core-variables.el

# Run tests matching a pattern
./run-tests.sh -p variables

# Run tests in a specific directory
./run-tests.sh tests/nested

# Debug mode (shows commands)
./run-tests.sh -d
```

### Running from Emacs

```elisp
;; Run tests in current buffer
M-x elisp-test-run

;; Run tests from dired with marked files
(in dired mode, mark files, then)
M-x elisp-test-run

;; Run tests programmatically
(elisp-test-run '("path/to/tests"))
(elisp-test-run "path/to/test-file.el")
```

### Running from Command Line

```bash
# Run a specific test file
emacs -Q --batch -l elisp-test-cli.el -- --test-file path/to/test-file.el

# Run all tests in a directory
emacs -Q --batch -l elisp-test-cli.el -- --test-dir path/to/tests

# Run tests matching a pattern
emacs -Q --batch -l elisp-test-cli.el -- --pattern variables

# Set a custom timeout
emacs -Q --batch -l elisp-test-cli.el -- --test-dir path/to/tests --timeout 20
```

## Future Improvements

Potential areas for future enhancement:

1. **Parallel Test Execution** - Run tests in parallel for faster execution
2. **Test Groups** - Allow defining and running groups of related tests
3. **Test Filtering** - More advanced filtering options beyond patterns
4. **HTML Reports** - Generate HTML reports in addition to Org/PDF
5. **Coverage Analysis** - Integrate with coverage tools for code coverage reporting