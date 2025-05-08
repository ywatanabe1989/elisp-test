;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 01:11:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/elisp-test-variables.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; This module defines variables used across the framework.

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


(provide 'elisp-test-variables)

(when
    (not load-file-name)
  (message "elisp-test-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))