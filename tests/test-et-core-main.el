;;; test-et-core-main.el --- Test main module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 14:50:00>

;;; Commentary:

;; Tests for the et-core-main module

;;; Code:

(require 'ert)


(ert-deftest test-et-core-main-module-loads ()
  "Test that the core main module loads properly."
  (require 'et-core-main)
  (should (featurep 'et-core-main)))

(provide 'test-et-core-main)

;;; test-et-core-main.el ends here