;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:18:07>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/elisp-test-cli.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; elisp-test-cli.el --- Command-line interface for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 15:15:30>

;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:

;; This file provides a command-line interface for running elisp-test.
;; It is designed to be called from the shell via:
;;
;; emacs -Q --batch -l elisp-test-cli.el -- [--test-file FILE] [--test-dir DIR] [--pattern PATTERN] [--timeout SECONDS]

;;; Code:

;; Load the testing framework
(let ((current-file-directory (or
                               (and load-file-name
                                    (file-name-directory
                                     load-file-name))
                               (and buffer-file-name
                                    (file-name-directory
                                     buffer-file-name))
                               default-directory)))
  (add-to-list 'load-path current-file-directory))

(require 'elisp-test)

;; Run the test handler if we're in batch mode
(when noninteractive
  (elisp-test--run-from-args))

;;; elisp-test-cli.el ends here


(provide 'elisp-test-cli)

(when
    (not load-file-name)
  (message "elisp-test-cli.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))