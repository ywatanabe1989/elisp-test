;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:50:59>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/elisp-test.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'elisp-test-variables)
(require 'elisp-test-loadpath)
(require 'elisp-test-buffer)
(require 'elisp-test-find)
(require 'elisp-test-parse)
(require 'elisp-test-report)
(require 'elisp-test-plan)
(require 'elisp-test-run)
(require 'elisp-test-main)

(provide 'elisp-test)

;;;###autoload
(add-to-list 'auto-mode-alist '("/test-.*\\.el\\'" . emacs-lisp-mode))


(when
    (not load-file-name)
  (message "elisp-test.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
