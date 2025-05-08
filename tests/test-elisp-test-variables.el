;;; test-elisp-test-variables.el --- Test variables module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:30:00>

;;; Commentary:

;; Tests for the elisp-test-variables module

;;; Code:

(require 'ert)
(require 'src/core/variables)

(ert-deftest test-elisp-test-variables-buffer-name ()
  "Test that the buffer name variable is defined correctly."
  (should (stringp elisp-test-buffer-name))
  (should (string= "*elisp-test*" elisp-test-buffer-name)))

(ert-deftest test-elisp-test-variables-timeout ()
  "Test that the timeout variable is defined correctly."
  (should (integerp elisp-test-timeout-sec))
  (should (= 10 elisp-test-timeout-sec)))

(ert-deftest test-elisp-test-variables-file-expressions ()
  "Test that the file expressions are defined correctly."
  (should (listp elisp-test-run-file-expressions))
  (should (member "^test-.*\\.el$" elisp-test-run-file-expressions)))

(provide 'test-elisp-test-variables)

;;; test-elisp-test-variables.el ends here