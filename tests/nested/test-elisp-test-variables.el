;;; test-elisp-test-variables.el --- Test variables module (nested) -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:30:00>

;;; Commentary:

;; Tests for the elisp-test-variables module (nested directory)

;;; Code:

(require 'ert)

(require 'et-core-variables)

(ert-deftest test-elisp-test-variables-nested-buffer-name ()
  "Test that the buffer name variable is defined correctly (nested)."
  (should (stringp elisp-test-buffer-name))
  (should (string= "*elisp-test*" elisp-test-buffer-name)))

(provide 'test-elisp-test-variables-nested)

;;; test-elisp-test-variables.el ends here