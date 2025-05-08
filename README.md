<!-- ---
!-- Timestamp: 2025-05-07 14:01:29
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.emacs.d/lisp/elisp-test/README.md
!-- --- -->

# Elisp Test

[![Build Status](https://github.com/ywatanabe1989/elisp-test/workflows/tests/badge.svg)](https://github.com/ywatanabe1989/elisp-test/actions)

A testing framework for Emacs Lisp projects that integrates with ERT (Emacs Lisp Regression Testing).

![Demo GIF](./docs/emacs-gif-screenshot-2025-03-05-09:13:39.gif)

## Project Structure

The project has been refactored with a modern Emacs package structure:

```
elisp-test/
├── elisp-test.el           # Main entry point
├── src/
│   ├── core/               # Core functionality
│   │   ├── variables.el    # Framework-wide variables
│   │   ├── loadpath.el     # Load path management
│   │   ├── run.el          # Test execution
│   │   └── main.el         # Main user-facing functions
│   ├── util/               # Utility functions
│   │   ├── find.el         # File finding utilities
│   │   ├── parse.el        # Test result parsing
│   │   └── plan.el         # Test planning
│   └── ui/                 # UI components
│       ├── buffer.el       # Buffer manipulation
│       └── report.el       # Report generation
└── tests/                  # Test files
```

A compatibility layer (`src/compat.el`) ensures backward compatibility with the old file structure for existing code.

## Example Test Reports
- [`./tests/ELISP-TEST-REPORT-20250509-015650-98-PERCENT.org`](./tests/ELISP-TEST-REPORT-20250509-015650-98-PERCENT.org)
- [`./tests/nested/ELISP-TEST-REPORT-20250509-015650-100-PERCENT.org`](./tests/nested/ELISP-TEST-REPORT-20250509-015650-100-PERCENT.org)

## Installation
1. Clone the repository:
```bash
git clone https://github.com/username/elisp-test.git ~/.emacs.d/lisp/elisp-test
```

2. Add to your init.el:
```elisp
;; Add main directory and src subdirectories to load path
(add-to-list 'load-path "~/.emacs.d/lisp/elisp-test")
(add-to-list 'load-path "~/.emacs.d/lisp/elisp-test/src/core")
(add-to-list 'load-path "~/.emacs.d/lisp/elisp-test/src/util")
(add-to-list 'load-path "~/.emacs.d/lisp/elisp-test/src/ui")

;; Or use a helper function to recursively add directories
(let ((base-dir "~/.emacs.d/lisp/elisp-test"))
  (add-to-list 'load-path base-dir)
  (dolist (dir (directory-files (expand-file-name "src" base-dir) t directory-files-no-dots-regexp))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

;; Load the package
(require 'elisp-test)
```

## Usage
#### Interactive Mode
1. Run tests on current buffer:
```elisp
(elisp-test-run)
```

2. Run tests on specific path (and child test files):
```elisp
(elisp-test-run) ; Run tests on current directory
(elisp-test-run "~/projects/my-elisp-project/test-example.el") ; Run tests on specific path
(elisp-test-run "~/projects/my-elisp-project/tests/") ; Run tests on child paths
;; In dired
;; Mark test files/directories with `m` -> `M-x elisp-test-run`
```

3. Running on multiple directories:
```elisp
;; Results are consolidated into a single report
(elisp-test-run '("~/path/to/tests1" "~/path/to/tests2"))

;; For dired mode: mark multiple directories, then run elisp-test-run
;; Each directory will generate its own report with consistent timestamps
```

#### Batch Mode
Create a `run-tests.el`:
```elisp
(setq ert-batch-print-level nil)
(setq ert-batch-print-length nil)

;; Add all required directories to load-path
(let ((base-dir "~/.emacs.d/lisp/elisp-test"))
  (add-to-list 'load-path base-dir)
  (add-to-list 'load-path (expand-file-name "src" base-dir))
  (dolist (dir '("core" "util" "ui"))
    (add-to-list 'load-path (expand-file-name (concat "src/" dir) base-dir))))

;; Use compatibility layer for simplified loading
(load "~/.emacs.d/lisp/elisp-test/src/compat.el")

;; Run the tests
(elisp-test-run "~/path/to/tests" nil t) ; Third parameter t means skip confirmation
```

Run from command line:
```bash
emacs -Q --batch -l run-tests.el
```

For CI environments, you'll want to skip the confirmation prompt by setting the third parameter to `t`:
```elisp
;; elisp-test-run parameters:
;; 1. Path(s) to test files or directories
;; 2. Timeout per test in seconds (default: elisp-test-timeout-sec)
;; 3. Skip confirmation prompt (default: nil)
(elisp-test-run "~/path/to/tests" 10 t) ; Skip confirmation prompt, 10s timeout
```

## Configurations
#### Example Key Bindings
``` elisp
(global-set-test-key (kbd "C-c C-t") #'elisp-test-run)
```

#### Customizable Variables
- `elisp-test-timeout-sec`: Test timeout (default: 10s)
- `elisp-test-run-file-expressions`: Test file patterns
- `elisp-test-run-file-exclude-expressions`: Exclusion patterns
- `elisp-test-results-org-path`: Results file location
- `elisp-test-results-org-path-dired`: Results file location for dired-specific reports
- `elisp-test-buffer-name`: Test buffer name

## License
Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

<!-- EOF -->