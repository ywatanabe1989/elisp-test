;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 01:40:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/-test-elisp-test-variables.el

(require 'ert)
(require 'elisp-test-variables)

(ert-deftest test-elisp-test-buffer-name-defined
    ()
  (should
   (boundp 'elisp-test-buffer-name))
  (should
   (stringp elisp-test-buffer-name))
  (should
   (string= elisp-test-buffer-name "*elisp-test*")))

(ert-deftest test-elisp-test-loadpath-defined
    ()
  (should
   (boundp 'elisp-test-loadpath))
  (should
   (listp elisp-test-loadpath)))

(ert-deftest test-elisp-test-timeout-sec-defined
    ()
  (should
   (boundp 'elisp-test-timeout-sec))
  (should
   (integerp elisp-test-timeout-sec))
  (should
   (= elisp-test-timeout-sec 10)))

(ert-deftest test-elisp-test-run-file-expressions-defined
    ()
  (should
   (boundp 'elisp-test-run-file-expressions))
  (should
   (listp elisp-test-run-file-expressions))
  (should
   (string=
    (car elisp-test-run-file-expressions)
    "^test-.*\\.el$")))

(ert-deftest test-elisp-test-run-file-exclude-expressions-defined
    ()
  (should
   (boundp 'elisp-test-run-file-exclude-expressions))
  (should
   (listp elisp-test-run-file-exclude-expressions))
  (should
   (=
    (length elisp-test-run-file-exclude-expressions)
    4)))

(provide '-test-elisp-test-variables)

(when
    (not load-file-name)
  (message "-test-elisp-test-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))