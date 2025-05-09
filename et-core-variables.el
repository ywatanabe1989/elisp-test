;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:28:46>
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
  nil
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

(defcustom elisp-test-generate-per-directory-reports
  nil
  "When non-nil, generate separate test reports for each directory.
In addition to the consolidated report, each directory will get its own
report with only the tests from that directory."
  :type 'boolean
  :group 'elisp-test)


(provide 'et-core-variables)

(when
    (not load-file-name)
  (message "et-core-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))