;;; elisp-test-core-run.el --- Test runner for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 02:14:00>

;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:

;; Provides functions for running tests in the elisp-test framework.

;;; Code:

(require 'src/core/variables)
(require 'src/ui/buffer)

(defun elisp-test--run-single-test
    (test &optional timeout)
  "Run a single TEST with TIMEOUT (defaults to 10 seconds)."
  (let
      ((file
        (car test))
       (testname
        (cdr test))
       (timeout-secs
        (or timeout 10)))
    (with-current-buffer
        (elisp-test-buffer-create "*ert*")
      (let
          ((inhibit-read-only t))
        (erase-buffer)))
    (condition-case err
        (progn
          ;; Modify to handle already loaded tests by suppressing redefinition errors
          (let ((ert-test-redefinition-messages nil)) ; locally suppress redefinition warnings
            (condition-case load-err
                (load file nil t)
              ;; If we get a redefinition error, just continue since the test is already loaded
              (error (when (not (string-match-p "redefined" (error-message-string load-err)))
                       (signal (car load-err) (cdr load-err))))))
          (let
              ((test-symbol
                (intern testname)))
            (if
                (ert-test-boundp test-symbol)
                (with-timeout
                    (timeout-secs
                     (list file testname
                           (format
                            "TIMEOUT: Test exceeded time limit of %s"
                            timeout-secs)))
                  (ert test-symbol)
                  (list file testname
                        (with-current-buffer "*ert*"
                          (let
                              ((output
                                (buffer-substring-no-properties
                                 (point-min)
                                 (point-max))))
                            (cond
                             ((string-match "Failed:\\s-*0\\b" output)
                              "PASSED")
                             ((string-match
                               "Failed:\\s-*[1-9][0-9]*\\b" output)
                              (concat "FAILED: " output))
                             ((string-match
                               "Skipped:\\s-*[1-9][0-9]*\\b" output)
                              (concat "SKIPPED: " output))
                             (t output))))))
              (list file testname "NOT-FOUND: Test not found"))))
      (error
       (list file testname
             (format "ERROR: %S" err))))))

;; Multiple Tests Runner
;; ----------------------------------------

(defun elisp-test--run-multiple-tests
    (test-alist &optional timeout-per-test)
  "Run multiple tests from TEST-ALIST and return ((path selector results) ...).
Tests are run sequentially to avoid loading conflicts."
  (interactive)
  ;; Sequential execution
  (message "Running tests sequentially")
  (mapcar
   (lambda
     (test)
     (elisp-test--run-single-test test timeout-per-test))
   test-alist))


(provide 'src/core/run)

;;; elisp-test-core-run.el ends here