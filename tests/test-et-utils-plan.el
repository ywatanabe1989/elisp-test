;;; test-et-utils-plan.el --- Test plan module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 14:50:00>

;;; Commentary:

;; Tests for the et-utils-plan module

;;; Code:

(require 'ert)


(ert-deftest test-et-utils-plan-module-loads ()
  "Test that the utils plan module loads properly."
  (require 'et-utils-plan)
  (should (featurep 'et-utils-plan)))

(provide 'test-et-utils-plan)

;;; test-et-utils-plan.el ends here