;;; test-et-core-loadpath.el --- Test loadpath module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 14:50:00>

;;; Commentary:

;; Tests for the et-core-loadpath module

;;; Code:

(require 'ert)


(ert-deftest test-et-core-loadpath-module-loads ()
  "Test that the core loadpath module loads properly."
  (require 'et-core-loadpath)
  (should (featurep 'et-core-loadpath)))

(provide 'test-et-core-loadpath)

;;; test-et-core-loadpath.el ends here