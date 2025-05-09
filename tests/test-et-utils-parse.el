;;; test-et-utils-parse.el --- Test parse module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 14:50:00>

;;; Commentary:

;; Tests for the et-utils-parse module

;;; Code:

(require 'ert)


(ert-deftest test-et-utils-parse-module-loads ()
  "Test that the utils parse module loads properly."
  (require 'et-utils-parse)
  (should (featurep 'et-utils-parse)))

(provide 'test-et-utils-parse)

;;; test-et-utils-parse.el ends here