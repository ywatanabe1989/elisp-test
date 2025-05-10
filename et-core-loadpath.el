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

;; (defun et--create-load-path-list (parent)
;;   "Return original load path list for testing purpose.
;; PARENT is the directory to start from."
;;   (let ((result '())
;;         (candidates (directory-files parent t "\\`[^.]")))
;;     (dolist (dir candidates)
;;       (when (file-directory-p dir)
;;         (push dir result)))
;;     result))

(defun elisp-test--add-package-load-paths (test-files)
  "Add package load paths based on TEST-FILES.
This detects package root and adds all relevant directories to load-path."
  (let ((package-roots (make-hash-table :test 'equal)))
    ;; Step 1: Find potential package roots for all test files
    (dolist (test-file test-files)
      (let* ((test-dir (file-name-directory test-file))
             (parent-dir (directory-file-name (file-name-directory test-dir)))
             (tests-dir-name (file-name-nondirectory (directory-file-name test-dir))))
        ;; Check if this file is in a "tests" directory (or similar)
        (when (or (string= tests-dir-name "tests")
                  (string= tests-dir-name "test")
                  (string-match-p "\\`test[s]?\\'" tests-dir-name))
          ;; Assume the parent dir is the package root
          (puthash parent-dir t package-roots))))
    
    ;; Step 2: Add each package root and its subdirs to load-path
    (maphash 
     (lambda (root _)
       (add-to-list 'load-path root)
       (message "Added package root to load-path: %s" root)
       
       ;; Add direct subdirectories except tests dirs
       (dolist (subdir (directory-files root t "\\`[^.]"))
         (when (and (file-directory-p subdir)
                    (not (string-match-p "/tests?/?\\'" subdir))
                    (not (string-match-p "/test[s]?/?\\'" subdir)))
           (add-to-list 'load-path subdir)
           (message "Added subdir to load-path: %s" subdir))))
     package-roots)))


(provide 'et-core-loadpath)

(when
    (not load-file-name)
  (message "et-core-loadpath.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
