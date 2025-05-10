;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 21:53:19>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/elisp-test.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Core functionality
(require 'et-core-variables)
(require 'et-core-loadpath)
(require 'et-core-run)
(require 'et-core-main)

;; Make sure core variables are accessible
(eval-and-compile
  (defvar elisp-test-buffer-name)
  (defvar elisp-test-timeout-sec)
  (defvar elisp-test-run-file-expressions)
  (defvar elisp-test-results-org-path ""))

;; UI components
(require 'et-ui-buffer)
(require 'et-ui-report)

;; Utility functions
(require 'et-utils-find)
(require 'et-utils-parse)
(require 'et-utils-plan)
(require 'et-ui-suppress-message)


(provide 'elisp-test)

(when
    (not load-file-name)
  (message "elisp-test.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))