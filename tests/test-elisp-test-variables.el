;;; test-elisp-test-variables.el --- Test variables module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:30:00>

;;; Commentary:

;; Tests for the elisp-test-variables module

;;; Code:

(require 'ert)
(require 'elisp-test)

(ert-deftest test-elisp-test-variables-exist ()
  "Test that the core variables are defined."
  (should (boundp 'elisp-test-buffer-name))
  (should (boundp 'elisp-test-timeout-sec))
  (should (boundp 'elisp-test-run-file-expressions)))

(provide 'test-elisp-test-variables)

;;; test-elisp-test-variables.el ends here