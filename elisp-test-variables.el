;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-03 10:33:19>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/elisp-test-variables.el

;; This module defines variables used across the framework.

(defgroup elisp-test
  nil
  "Emacs Lisp Testing Framework."
  :group 'tools)

;; Variables
;; ----------------------------------------

(defvar et-loadpath
  '()
  "List of load paths for tests.")

(defvar et-buffer-name
  "*elisp-test*"
  "Name of the buffer used for elisp test results.")

(defcustom et-timeout-sec
  10
  "Default timeout in seconds for running a single test."
  :type 'integer
  :group 'elisp-test)

(defcustom et-test-file-expressions
  '("^test-.*\\.el$")
  "List of regular expressions to match test files."
  :type
  '(repeat string)
  :group 'elisp-test)

(defcustom et-test-file-exclude-expressions
  '("/\\.[^/]*/"        ; Hidden directories
    "/\\.[^/]*$"        ; Hidden files
    "/_[^/]*/"          ; Underscore directories
    "/_[^/]*$")
                                        ; Underscore files
  "List of regular expressions to exclude test files."
  :type
  '(repeat string)
  :group 'elisp-test)

(defcustom et-results-org-path
  "~/ELISP-TEST-REPORT.org"
  "File path where test results will be saved."
  :type 'file
  :group 'elisp-test)

(defcustom et-results-org-path-dired
  (file-name-nondirectory et-results-org-path)
  "Filename for test results when running from dired."
  :type 'string
  :group 'elisp-test)

(defvar et-results-org-path-switched
  nil
  "Filepath for test results")

(defvar et-plan-buffer-name
  "*ELISP-TEST-PLAN*"
  "Buffer name for test planning")

(provide 'elisp-test-variables)

(when
    (not load-file-name)
  (message "elisp-test-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
