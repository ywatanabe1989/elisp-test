;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-02 21:48:11>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/elisp-test.el

;; Main entry point, requiring all other modules and providing the `elisp-test` feature.

;;; elisp-test.el --- Main entry point for the Emacs Lisp testing framework -*- lexical-binding: t; -*-

(require 'elisp-test-variables)
(require 'elisp-test-loadpath)
(require 'elisp-test-buffer)
(require 'elisp-test-find)
(require 'elisp-test-run)
(require 'elisp-test-parse)
(require 'elisp-test-report)
(require 'elisp-test-plan)
(require 'elisp-test-main)

(provide 'elisp-test)

(when
    (not load-file-name)
  (message "elisp-test.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))