;;; test-elisp-test-find.el --- Test find module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:30:00>

;;; Commentary:

;; Tests for the elisp-test find module

;;; Code:

(require 'ert)
(require 'src/util/find)

(ert-deftest test-elisp-test-find-deftest ()
  "Test that the deftest finder works correctly."
  (should (functionp '--elisp-test-find-deftest))
  (should (functionp '--elisp-test-find-deftest-file)))

(provide 'test-elisp-test-find)

;;; test-elisp-test-find.el ends here