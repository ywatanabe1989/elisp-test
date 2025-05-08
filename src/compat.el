;;; compat.el --- Compatibility layer for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:00:00>

;;; Commentary:

;; This file provides compatibility for different module naming schemes
;; to ensure the refactored codebase works as expected.

;;; Code:

;; Define feature aliases for compatibility
(provide 'src/core/variables)
(provide 'src/core/loadpath)
(provide 'src/core/run)
(provide 'src/core/main)
(provide 'src/ui/buffer)
(provide 'src/ui/report)
(provide 'src/util/find)
(provide 'src/util/parse)
(provide 'src/util/plan)

(provide 'compat)

;;; compat.el ends here