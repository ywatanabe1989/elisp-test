;;; elisp-test-core-loadpath.el --- Load path handling for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 02:12:00>

;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:

;; Handles load paths for the elisp-test framework.

;;; Code:

(require 'elisp-test-core-variables)

;;;###autoload
(defun elisp-test-add-load-paths
    (paths)
  "Add PATHS to `elisp-test-loadpath` and `load-path`.
  PATHS should be a list of directory paths to include."
  (dolist
      (path paths)
    (let
        ((full-path
          (expand-file-name path)))
      (add-to-list 'elisp-test-loadpath full-path)
      (add-to-list 'load-path full-path))))


(provide 'elisp-test-core-loadpath)

;;; elisp-test-core-loadpath.el ends here
