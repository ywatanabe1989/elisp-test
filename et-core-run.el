;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 21:57:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-core-run.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'et-core-variables)
(require 'et-ui-buffer)

;; Variable to store the results of the last test run

(defvar elisp-test--last-test-results nil
  "Results from the last test run.")

(defun elisp-test--parse-test-output ()
  "Parse test output from the *ert* buffer.
Returns a string describing the test result: PASSED, FAILED, SKIPPED, or the raw output."
  (with-current-buffer "*ert*"
    (let
        ((output
          (buffer-substring-no-properties (point-min) (point-max))))
      (cond
       ((string-match "Failed:\\s-*0\\b" output)
        "PASSED")
       ((string-match "Failed:\\s-*[1-9][0-9]*\\b" output)
        (concat "FAILED: " output))
       ((string-match "Skipped:\\s-*[1-9][0-9]*\\b" output)
        (concat "SKIPPED: " output))
       (t output)))))

(defun elisp-test--run-single-test (test &optional timeout)
  "Run a single TEST with TIMEOUT (defaults to 10 seconds).
TEST should be a cons cell with (file . test-name).
TIMEOUT specifies the maximum time in seconds to allow for test execution.
Returns a list of (file test-name result) where result is the test outcome."
  (let ((file (car test))
        (testname (cdr test))
        (timeout-secs (or timeout 10)))
    (with-current-buffer (elisp-test-buffer-create "*ert*")
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (condition-case err
        (progn
          ;; Modify to handle already loaded tests by suppressing redefinition errors
          (let ((ert-test-redefinition-messages nil))
            ;; locally suppress redefinition warnings
            (condition-case load-err
                (load file nil t)
              ;; If we get a redefinition error, just continue since the test is already loaded
              (error
               (when
                   (not
                    (string-match-p "redefined"
                                    (error-message-string load-err)))
                 (signal (car load-err) (cdr load-err))))))
          (let ((test-symbol (intern testname)))
            (if (ert-test-boundp test-symbol)
                (with-timeout (timeout-secs
                               (list file testname
                                     (format
                                      "TIMEOUT: Test exceeded time limit of %s"
                                      timeout-secs)))
                  (ert test-symbol)
                  (list file testname (elisp-test--parse-test-output)))
              (list file testname "NOT-FOUND: Test not found"))))
      (error
       (list file testname (format "ERROR: %S" err))))))

;; Multiple Tests Runner
;; ----------------------------------------

(defun elisp-test--run-multiple-tests
    (test-alist &optional timeout-per-test)
  "Run multiple tests from TEST-ALIST and return ((path selector results) ...).
Tests are run sequentially to avoid loading conflicts.
TEST-ALIST should be a list of (file . test-name) cons cells.
TIMEOUT-PER-TEST specifies the maximum execution time for each test.
Results are stored in `elisp-test--last-test-results` for later use."
  (interactive)
  ;; Sequential execution
  (let ((results (mapcar
                  (lambda (test)
                    (elisp-test--run-single-test test timeout-per-test))
                  test-alist)))
    ;; Store the results for later access (e.g., for exit code determination)
    (setq elisp-test--last-test-results results)
    results))

;; Run tests from command line
;; ----------------------------------------

(defun elisp-test--run-from-args ()
  "Run elisp-test based on command line arguments.
This function is intended to be called from emacs -Q --eval statements.
It handles the following arguments:
--test-file FILE   Specify a single test file to run
--test-dir DIR     Specify a directory containing test files
--pattern PATTERN  Run only tests matching the pattern
--timeout SECONDS  Set the timeout for test execution

After running tests, exits Emacs with status code 1 if any tests failed,
or 0 if all tests passed."
  (let ((test-file nil)
        (test-dir nil)
        (pattern nil)
        (args command-line-args-left)
        (timeout-per-test elisp-test-timeout-sec))

    ;; Parse command line arguments
    (while args
      (cond
       ((string= (car args) "--test-file")
        (setq args (cdr args))
        (when args
          (setq test-file (car args))
          (setq args (cdr args))))

       ((string= (car args) "--test-dir")
        (setq args (cdr args))
        (when args
          (setq test-dir (car args))
          (setq args (cdr args))))

       ((string= (car args) "--pattern")
        (setq args (cdr args))
        (when args
          (setq pattern (car args))
          (setq args (cdr args))))

       ((string= (car args) "--timeout")
        (setq args (cdr args))
        (when args
          (setq timeout-per-test (string-to-number (car args)))
          (setq args (cdr args))))

       (t (setq args (cdr args)))))

    ;; Set command-line-args-left to nil to avoid processing by the batch handler
    (setq command-line-args-left nil)

    ;; Run tests based on the parsed arguments
    (cond
     (test-file
      ;; Run a single test file
      (elisp-test-run (list test-file) timeout-per-test t))

     (pattern
      ;; Run tests matching a pattern
      (let* ((test-dir (or test-dir default-directory))
             (files (if (string= pattern "test-")
                        ;; Special case for default pattern - get all test files
                        (directory-files-recursively test-dir
                                                     "test-.*\\.el$" t)
                      ;; Regular pattern matching
                      (directory-files-recursively test-dir
                                                   (format
                                                    "test-.*%s.*\\.el$"
                                                    pattern)
                                                   t))))
        (elisp-test-run files timeout-per-test t)))

     (test-dir
      ;; Run all tests in a directory
      (elisp-test-run (list test-dir) timeout-per-test t))

     (t
      ;; No arguments, run tests in the current directory
      (elisp-test-run (list default-directory) timeout-per-test t))))

  ;; Exit with appropriate status code
  (let
      ((exit-code
        (elisp-test--determine-exit-code elisp-test--last-test-results)))
    (kill-emacs exit-code)))

(defun elisp-test--determine-exit-code (test-results)
  "Determine exit code based on TEST-RESULTS.
Returns 0 if all tests passed, 1 if any tests failed, errored, or timed out."
  (let ((has-failures nil))
    ;; Check if any tests failed
    (dolist (result test-results)
      (let ((test-status (nth 2 result)))
        (when (and test-status
                   (or (string-match-p "FAILED:" test-status)
                       (string-match-p "ERROR:" test-status)
                       (string-match-p "TIMEOUT:" test-status)))
          (setq has-failures t))))
    (if has-failures 1 0)))


(provide 'et-core-run)

(when
    (not load-file-name)
  (message "et-core-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))