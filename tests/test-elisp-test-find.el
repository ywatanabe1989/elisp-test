;;; test-elisp-test-find.el --- Test find module -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:30:00>

;;; Commentary:

;; Tests for the elisp-test find module

;;; Code:

(require 'ert)
(require 'test-helper)

(ert-deftest test-elisp-test-find-module-loads ()
  "Test that the find module loads properly."
  (should (featurep 'src/util/find)))

(provide 'test-elisp-test-find)

;;; test-elisp-test-find.el ends here