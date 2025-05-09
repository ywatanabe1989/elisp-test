;;; test-et-ui-report.el --- Test report UI module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 14:50:00>

;;; Commentary:

;; Tests for the et-ui-report module

;;; Code:

(require 'ert)


(ert-deftest test-et-ui-report-module-loads ()
  "Test that the UI report module loads properly."
  (require 'et-ui-report)
  (should (featurep 'et-ui-report)))

(provide 'test-et-ui-report)

;;; test-et-ui-report.el ends here