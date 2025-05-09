;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:21:54>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/tests/test-et-ui-buffer.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; test-et-ui-buffer.el --- Test buffer UI module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 14:50:00>

;;; Commentary:

;; Tests for the et-ui-buffer module

;;; Code:

(require 'ert)

(ert-deftest test-et-ui-buffer-module-loads ()
  "Test that the UI buffer module loads properly."
  (require 'et-ui-buffer)
  (should (featurep 'et-ui-buffer)))

;;; test-et-ui-buffer.el ends here


(provide 'test-et-ui-buffer)

(when
    (not load-file-name)
  (message "test-et-ui-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))