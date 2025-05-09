;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:38:10>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-ui-suppress-message.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;; Suppress ERT messages during test execution

(defun elisp-test--suppress-messages (orig-fun &rest args)
  "Advice to suppress messages from ERT during test execution."
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply orig-fun args)))

;; Add advice to ert functions that output messages
(advice-add 'ert-run-tests-batch-and-exit :around
            #'elisp-test--suppress-messages)
(advice-add 'ert-run-test-interactively :around
            #'elisp-test--suppress-messages)
(advice-add 'ert--pp-with-indentation-and-newline :around
            #'elisp-test--suppress-messages)


(provide 'et-ui-suppress-message)

(when
    (not load-file-name)
  (message "et-ui-suppress-message.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))