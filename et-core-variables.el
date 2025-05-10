;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 17:40:27>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-core-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defgroup elisp-test
  nil
  "Emacs Lisp Testing Framework."
  :group 'tools)

;; Variables
;; ----------------------------------------

(defvar elisp-test-loadpath
  '()
  "List of load paths for tests.")

(defvar elisp-test-report-base-name
  "ELISP-TEST-REPORT"
  "Base name for test report files.")


(defvar elisp-test-results-org-path ""
  "Path to save test results in org format.")

;; Then define the path for dired, which depends on the base path

(defvar elisp-test-results-org-path-dired ""
  "Filename for test results when running from dired.")

(defvar elisp-test-results-org-path-final
  nil
  "Filepath for test results")

;; Define these with defconst to make sure they're always available

(defconst elisp-test-buffer-name
  "*elisp-test*"
  "Name of the buffer used for elisp test results.")

(defconst elisp-test-timeout-sec
  10
  "Default timeout in seconds for running a single test.")

(defconst elisp-test-run-file-expressions
  '("^test-.*\\.el$")
  "List of regular expressions to match test files.")

(defvar elisp-test-run-file-exclude-expressions
  '("/\\.[^/]*/"        ; Hidden directories
    "/\\.[^/]*$"        ; Hidden files
    "/_[^/]*/"          ; Underscore directories
    "/_[^/]*$")         ; Underscore files
  "List of regular expressions to exclude test files.")


(defvar elisp-test-plan-buffer-name
  "*ELISP-TEST-PLAN*"
  "Buffer name for test planning")

(defvar elisp-test-generate-per-directory-reports
  nil
  "When non-nil, generate separate test reports for each directory.
In addition to the consolidated report, each directory will get its own
report with only the tests from that directory.")


(provide 'et-core-variables)

(when
    (not load-file-name)
  (message "et-core-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))