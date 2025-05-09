;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:28:44>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/elisp-test-cli.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


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


(provide 'elisp-test-cli)

(when
    (not load-file-name)
  (message "elisp-test-cli.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))