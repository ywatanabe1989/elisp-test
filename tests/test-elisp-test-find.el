;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:21:50>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/tests/test-elisp-test-find.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; test-elisp-test-find.el --- Test find module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:30:00>

;;; Commentary:

;; Tests for the elisp-test find module

;;; Code:

(require 'ert)

(ert-deftest test-elisp-test-find-module-loads ()
  "Test that the find module loads properly."
  (require 'et-utils-find)
  (should (featurep 'et-utils-find)))

;;; test-elisp-test-find.el ends here


(provide 'test-elisp-test-find)

(when
    (not load-file-name)
  (message "test-elisp-test-find.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))