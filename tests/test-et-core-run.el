;;; test-et-core-run.el --- Test run module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 14:50:00>

;;; Commentary:

;; Tests for the et-core-run module

;;; Code:

(require 'ert)


(ert-deftest test-et-core-run-module-loads ()
  "Test that the core run module loads properly."
  (require 'et-core-run)
  (should (featurep 'et-core-run)))

(provide 'test-et-core-run)

;;; test-et-core-run.el ends here