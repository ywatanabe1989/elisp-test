;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 13:07:56>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-core-loadpath.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; elisp-test-core-loadpath.el --- Load path handling for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 02:12:00>

;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:

;; Handles load paths for the elisp-test framework.

;;; Code:

(require 'et-core-variables)

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

;;; elisp-test-core-loadpath.el ends here


(provide 'et-core-loadpath)

(when
    (not load-file-name)
  (message "et-core-loadpath.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
