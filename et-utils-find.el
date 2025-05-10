;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:28:48>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-utils-find.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'et-core-variables)

;;;###autoload
(defun elisp-test-find-test-files-multiple
    (root-paths &optional include-hidden)
  "Find test files in multiple ROOT-PATHS.
ROOT-PATHS can be a single path string or a list of path strings.
When INCLUDE-HIDDEN is non-nil, include files in hidden directories.
Returns a list of file paths matching the test file patterns."
  (when root-paths
    (let*
        ((paths-list
          (if
              (stringp root-paths)
              (list root-paths)
            root-paths))
         (result
          (mapcan
           (lambda
             (path)
             (elisp-test--find-test-files-single path include-hidden))
           paths-list)))
      result)))

(defun elisp-test-find-test-files-multiple-dired
    (&optional include-hidden)
  "Find test files based on selected paths"
  (interactive)
  (when
      (eq major-mode 'dired-mode)
    (let*
        ((root-paths
          (elisp-test--find-list-marked-paths-dired))
         (test-files
          (when root-paths
            (mapcan
             (lambda
               (path)
               (elisp-test--find-test-files-single path
                                                    include-hidden))
             root-paths))))
      (when test-files
        (with-current-buffer
            (elisp-test-buffer-create elisp-test-buffer-name)
          (erase-buffer)
          (insert
           (mapconcat 'identity test-files "\n"))
          (display-buffer
           (current-buffer)))
        (progn
          (message "Found test files: %S" test-files)
          test-files)))))

;; From Single Root
;; ----------------------------------------

(defun elisp-test--find-matching-files
    (root-path patterns)
  "Find files in ROOT-PATH matching any of the given PATTERNS."
  (let
      ((file-list
        '()))
    (if
        (file-directory-p root-path)
        (dolist
            (pattern patterns)
          (let
              ((matching-files
                (directory-files-recursively root-path pattern t)))
            (setq file-list
                  (append file-list matching-files))))
      (let
          ((filename
            (file-name-nondirectory root-path)))
        (dolist
            (pattern patterns)
          (when
              (string-match-p pattern filename)
            (push root-path file-list)))))
    file-list))

(defun elisp-test--find-test-files-single
    (root-path &optional include-hidden)
  "Find all test files in ROOT-PATH matching `elisp-test-run-file-expressions`.
ROOT-PATH is used for calculating relative paths for exclusion patterns."
  (interactive "fSelect file or directory: ")
  (let*
      ((root-path-full-path
        (expand-file-name root-path))
       (file-list
        (elisp-test--find-matching-files root-path-full-path
                                          elisp-test-run-file-expressions)))
    (setq file-list
          (elisp-test--filter-excluded-files file-list
                                              root-path-full-path
                                              elisp-test-run-file-exclude-expressions))
    (setq file-list
          (elisp-test--filter-hidden-files file-list
                                            root-path-full-path
                                            include-hidden))
    file-list))

(defun elisp-test--filter-excluded-files
    (file-list root-path exclude-patterns)
  "Filter FILE-LIST removing files matching EXCLUDE-PATTERNS relative to ROOT-PATH."
  (if exclude-patterns
      (let
          ((original-count
            (length file-list)))
        (setq file-list
              (seq-remove
               (lambda
                 (file)
                 (let*
                     ((rel-path
                       (file-relative-name file root-path)))
                   (let
                       ((should-exclude
                         (cl-some
                          (lambda
                            (pattern)
                            (let
                                ((match
                                  (string-match-p pattern rel-path)))
                              match))
                          exclude-patterns)))
                     should-exclude)))
               file-list)))
    )
  file-list)

(defun elisp-test--filter-hidden-files
    (file-list root-path include-hidden)
  "Filter FILE-LIST to exclude hidden files unless INCLUDE-HIDDEN is non-nil.
Only considers files with hidden components after ROOT-PATH."
  (if include-hidden
      file-list
    (let
        ((original-count
          (length file-list)))
      (setq file-list
            (seq-filter
             (lambda
               (file)
               (let*
                   ((rel-path
                     (file-relative-name file root-path))
                    (components
                     (split-string rel-path "/" t))
                    (has-hidden-component nil))
                 (if
                     (string= rel-path ".")
                     t
                   (progn
                     (dolist
                         (component components)
                       (when
                           (string-match-p "^\\." component)
                         (setq has-hidden-component t)))
                     (let
                         ((result
                           (not has-hidden-component)))
                       result)))))
             file-list))
      file-list)))

;; Helper
;; ----------------------------------------

(defun elisp-test--find-list-marked-paths-dired
    ()
  "Get list of marked files/directories in dired."
  (interactive)
  (when
      (eq major-mode 'dired-mode)
    (let
        ((found
          (dired-get-marked-files)))
      (progn
        found))))

;; Deftest Finder
;; ----------------------------------------

(defun elisp-test--find-deftest-file
    (file)
  "Extract ert-deftest names from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char
     (point-min))
    (let
        (tests)
      (while
          (re-search-forward "(ert-deftest\\s-+\\([^[:space:]\n]+\\)"
                             nil t)
        (push
         (cons file
               (match-string 1))
         tests))
      tests)))

;; (defun --elisp-test-find-deftest
;;     (&optional paths)
;;   "Find all ert-deftest forms in provided PATHS.
;; PATHS can be a single path or a list of paths."
;;   (let* ((path-list (cond 
;;                      ((null paths) (list default-directory)) ;; Use current directory if nil
;;                      ((listp paths) paths)
;;                      (t (list paths))))
;;          (tests '()))
;;     (dolist (path path-list tests)
;;       (if (file-directory-p path)
;;           ;; If path is a directory, find test files first
;;           (let ((test-files (--elisp-test-find-test-files-single path)))
;;             (dolist (file test-files)
;;               (setq tests (append tests (--elisp-test-find-deftest-file file)))))
;;         ;; If path is a file, search directly
;;         (when (file-exists-p path)
;;           (setq tests (append tests (--elisp-test-find-deftest-file path))))))))
(defun elisp-test--find-deftest (&optional paths)
  "Find all ert-deftest forms in provided PATHS.
PATHS can be a single path or a list of paths."
  (let* ((path-list (cond 
                     ((null paths) (list default-directory)) 
                     ((listp paths) paths)
                     (t (list paths))))
         (tests '()))
    (dolist (path path-list tests)
      (when path  ;; Skip nil paths
        (if (file-directory-p path)
            ;; If path is a directory, find test files first
            (let ((test-files (elisp-test--find-test-files-single path)))
              (dolist (file test-files)
                (setq tests (append tests (elisp-test--find-deftest-file file)))))
          ;; If path is a file, process it directly
          (when (file-exists-p path)
            (setq tests (append tests (elisp-test--find-deftest-file path)))))))))

(provide 'et-utils-find)

(when
    (not load-file-name)
  (message "et-utils-find.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
