;;; test-elisp-test-variables.el --- Test variables module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:30:00>

;;; Commentary:

;; Tests for the elisp-test-variables module

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest test-elisp-test-variables-exist ()
  "Test that the core variables are defined."
  (should (stringp elisp-test-buffer-name))
  (should (integerp elisp-test-timeout-sec))
  (should (listp elisp-test-run-file-expressions)))

(provide 'test-elisp-test-variables)

;;; test-elisp-test-variables.el ends here