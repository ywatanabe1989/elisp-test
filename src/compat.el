;;; compat.el --- Compatibility layer for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:00:00>

;;; Commentary:

;; This file provides compatibility for different module naming schemes
;; to ensure the refactored codebase works as expected.

;;; Code:

;; Define feature aliases for compatibility - both new namespace and old names

;; Core
(provide 'src/core/variables)
(provide 'elisp-test-variables)
(provide 'elisp-test-core-variables)

(provide 'src/core/loadpath)
(provide 'elisp-test-loadpath)
(provide 'elisp-test-core-loadpath)

(provide 'src/core/run)
(provide 'elisp-test-run)
(provide 'elisp-test-core-run)

(provide 'src/core/main)
(provide 'elisp-test-main)
(provide 'elisp-test-core-main)

;; UI
(provide 'src/ui/buffer)
(provide 'elisp-test-buffer)
(provide 'elisp-test-ui-buffer)

(provide 'src/ui/report)
(provide 'elisp-test-report)
(provide 'elisp-test-ui-report)

;; Util
(provide 'src/util/find)
(provide 'elisp-test-find)
(provide 'elisp-test-util-find)

(provide 'src/util/parse)
(provide 'elisp-test-parse) 
(provide 'elisp-test-util-parse)

(provide 'src/util/plan)
(provide 'elisp-test-plan)
(provide 'elisp-test-util-plan)

;; Provide this module
(provide 'compat)

;;; compat.el ends here