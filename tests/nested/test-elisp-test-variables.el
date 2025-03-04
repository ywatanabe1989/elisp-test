;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 01:40:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/-test-elisp-test-variables.el

(require 'ert)
(require 'elisp-test-variables)

(ert-deftest test-et-buffer-name-defined
    ()
  (should
   (boundp 'et-buffer-name))
  (should
   (stringp et-buffer-name))
  (should
   (string= et-buffer-name "*elisp-test*")))

(ert-deftest test-et-loadpath-defined
    ()
  (should
   (boundp 'et-loadpath))
  (should
   (listp et-loadpath)))

(ert-deftest test-et-timeout-sec-defined
    ()
  (should
   (boundp 'et-timeout-sec))
  (should
   (integerp et-timeout-sec))
  (should
   (= et-timeout-sec 10)))

(ert-deftest test-et-test-file-expressions-defined
    ()
  (should
   (boundp 'et-test-file-expressions))
  (should
   (listp et-test-file-expressions))
  (should
   (string=
    (car et-test-file-expressions)
    "^test-.*\\.el$")))

(ert-deftest test-et-test-file-exclude-expressions-defined
    ()
  (should
   (boundp 'et-test-file-exclude-expressions))
  (should
   (listp et-test-file-exclude-expressions))
  (should
   (=
    (length et-test-file-exclude-expressions)
    4)))

(provide '-test-elisp-test-variables)

(when
    (not load-file-name)
  (message "-test-elisp-test-variables.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))