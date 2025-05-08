;;; compat.el --- Backwards compatibility for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 03:00:00>

;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:

;; This file provides backward compatibility with the old file structure.
;; It defines the old module names to point to the new ones.

;;; Code:

;; Setup backward compatibility by creating empty features
;; that will be filled later by loading the actual files
(provide 'elisp-test)
(provide 'elisp-test-variables)
(provide 'elisp-test-loadpath)
(provide 'elisp-test-buffer)
(provide 'elisp-test-find)
(provide 'elisp-test-run)
(provide 'elisp-test-parse)
(provide 'elisp-test-plan)
(provide 'elisp-test-report)
(provide 'elisp-test-main)

;; Load all files directly
(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  ;; Load core modules
  (load (expand-file-name "core/variables.el" base-dir) nil t)
  (load (expand-file-name "core/loadpath.el" base-dir) nil t)
  
  ;; Load UI modules
  (load (expand-file-name "ui/buffer.el" base-dir) nil t)
  
  ;; Load utility modules
  (load (expand-file-name "util/find.el" base-dir) nil t)
  (load (expand-file-name "util/parse.el" base-dir) nil t)
  
  ;; Load remaining modules
  (load (expand-file-name "core/run.el" base-dir) nil t)
  (load (expand-file-name "util/plan.el" base-dir) nil t)
  (load (expand-file-name "ui/report.el" base-dir) nil t)
  (load (expand-file-name "core/main.el" base-dir) nil t))

;;; compat.el ends here