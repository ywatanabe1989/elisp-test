;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:28:45>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-core-loadpath.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


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


(provide 'et-core-loadpath)

(when
    (not load-file-name)
  (message "et-core-loadpath.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
