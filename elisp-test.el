;;; elisp-test.el --- Testing framework for Emacs Lisp projects -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 02:55:00>
;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-cheat/elisp-test.el
;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, testing
;; URL: https://github.com/ywatanabe1989/elisp-test

;;; Commentary:

;; A testing framework for Emacs Lisp projects that integrates with
;; ERT (Emacs Lisp Regression Testing).
;;
;; This package provides utilities for organizing and running tests,
;; generating reports, and managing test infrastructure.

;;; Code:

;; Core functionality
(require 'et-core-variables)
(require 'et-core-loadpath)
(require 'et-core-run)
(require 'et-core-main)

;; Make sure core variables are accessible
(eval-and-compile
  (defvar elisp-test-buffer-name)
  (defvar elisp-test-timeout-sec)
  (defvar elisp-test-run-file-expressions))

;; UI components
(require 'et-ui-buffer)
(require 'et-ui-report)

;; Utility functions
(require 'et-utils-find)
(require 'et-utils-parse)
(require 'et-utils-plan)

;; No need for alias since function is already defined in et-core-main.el

;; Avoid loading this file twice
(provide 'elisp-test)

;;; elisp-test.el ends here
