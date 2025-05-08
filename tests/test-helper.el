;;; test-helper.el --- Test helper for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:45:00>

;;; Commentary:

;; Helper module that's loaded before running tests.
;; This sets up the test environment.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Add src directories to load path
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (expand-file-name "../" default-directory))
(add-to-list 'load-path (expand-file-name "../src" default-directory))
(add-to-list 'load-path (expand-file-name "../src/core" default-directory))
(add-to-list 'load-path (expand-file-name "../src/ui" default-directory))
(add-to-list 'load-path (expand-file-name "../src/util" default-directory))

;; Load the main elisp-test file
(require 'elisp-test)

;; Export key variables for tests to use
(defconst elisp-test-buffer-name "*elisp-test*"
  "Buffer name used in tests.")

(defconst elisp-test-timeout-sec 10
  "Timeout used in tests.")

(defconst elisp-test-run-file-expressions '("^test-.*\\.el$")
  "File expressions used in tests.")

(provide 'test-helper)

;;; test-helper.el ends here