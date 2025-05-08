;;; elisp-test-core-variables.el --- Core variables for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 02:10:00>

;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:

;; This module defines variables used across the elisp-test framework.

;;; Code:

(defgroup elisp-test
  nil
  "Emacs Lisp Testing Framework."
  :group 'tools)

;; Variables
;; ----------------------------------------

(defvar elisp-test-loadpath
  '()
  "List of load paths for tests.")

(defvar elisp-test-buffer-name
  "*elisp-test*"
  "Name of the buffer used for elisp test results.")

(defcustom elisp-test-timeout-sec
  10
  "Default timeout in seconds for running a single test."
  :type 'integer
  :group 'elisp-test)

(defcustom elisp-test-run-file-expressions
  '("^test-.*\\.el$")
  "List of regular expressions to match test files."
  :type
  '(repeat string)
  :group 'elisp-test)

(defcustom elisp-test-run-file-exclude-expressions
  '("/\\.[^/]*/"        ; Hidden directories
    "/\\.[^/]*$"        ; Hidden files
    "/_[^/]*/"          ; Underscore directories
    "/_[^/]*$")
                                        ; Underscore files
  "List of regular expressions to exclude test files."
  :type
  '(repeat string)
  :group 'elisp-test)

(defcustom elisp-test-results-org-path
  "~/ELISP-TEST-REPORT.org"
  "File path where test results will be saved."
  :type 'file
  :group 'elisp-test)

(defcustom elisp-test-results-org-path-dired
  (file-name-nondirectory elisp-test-results-org-path)
  "Filename for test results when running from dired."
  :type 'string
  :group 'elisp-test)

(defvar elisp-test-results-org-path-switched
  nil
  "Filepath for test results")

(defvar elisp-test-plan-buffer-name
  "*ELISP-TEST-PLAN*"
  "Buffer name for test planning")


(provide 'elisp-test-core-variables)

;;; elisp-test-core-variables.el ends here