;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-03 10:54:13>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-test/elisp-test-find.el

(defun et-find-test-files-multiple
    (root-paths &optional include-hidden)
  "Find test files in multiple ROOT-PATHS."
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
             (--et-find-test-files-single path include-hidden))
           paths-list)))
      ;; (message "DEBUG: Final result: %S" result)
      result)))

(defun et-find-test-files-multiple-dired
    (&optional include-hidden)
  "Find test files based on selected paths"
  (interactive)
  (when
      (eq major-mode 'dired-mode)
    (let*
        ((root-paths
          (--et-find-list-marked-paths-dired))
         (test-files
          (when root-paths
            (mapcan
             (lambda
               (path)
               (--et-find-test-files-single path include-hidden))
             root-paths))))
      ;; nil
      (when test-files
        (with-current-buffer
            (get-buffer-create et-buffer-name)
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

(defun --et-find-matching-files
    (root-path patterns)
  "Find files in ROOT-PATH matching any of the given PATTERNS."
  ;; (message "DEBUG: Finding matching files in %s with patterns %S" root-path patterns)
  (let
      ((file-list
        '()))
    (if
        (file-directory-p root-path)
        (dolist
            (pattern patterns)
          ;; (message "DEBUG: Searching with pattern: %s" pattern)
          (let
              ((matching-files
                (directory-files-recursively root-path pattern t)))
            ;; (message "DEBUG: Pattern %s found %d files" pattern
            ;;          (length matching-files))
            (setq file-list
                  (append file-list matching-files))))
      (let
          ((filename
            (file-name-nondirectory root-path)))
        ;; (message "DEBUG: Checking single file: %s" filename)
        (dolist
            (pattern patterns)
          (when
              (string-match-p pattern filename)
            ;; (message "DEBUG: File matches pattern %s" pattern)
            (push root-path file-list)))))
    ;; (message "DEBUG: Total files matched: %d"
    ;;          (length file-list))
    file-list))

(defun --et-find-test-files-single
    (root-path &optional include-hidden)
  "Find all test files in ROOT-PATH matching `et-test-file-expressions`.
ROOT-PATH is used for calculating relative paths for exclusion patterns."
  (interactive "fSelect file or directory: ")
  ;; (message "DEBUG: Starting --et-find-test-files-single with root-path: %s" root-path)
  (let*
      ((root-path-full-path
        (expand-file-name root-path))
       (file-list
        (--et-find-matching-files root-path-full-path et-test-file-expressions)))
    ;; (message "DEBUG: After matching: Found %d files with list: %S"
    ;;          (length file-list)
    ;;          file-list)
    ;; (message "DEBUG: et-test-file-exclude-expressions: %S" et-test-file-exclude-expressions)
    (setq file-list
          (--et-filter-excluded-files file-list root-path-full-path et-test-file-exclude-expressions))
    ;; (message "DEBUG: After exclusion: %d files remaining with list: %S"
    ;;          (length file-list)
    ;;          file-list)
    (setq file-list
          (--et-filter-hidden-files file-list root-path-full-path include-hidden))
    ;; (message "DEBUG: After hidden filtering: %d files remaining with list: %S"
    ;;          (length file-list)
    ;;          file-list)
    file-list))

(defun --et-filter-excluded-files
    (file-list root-path exclude-patterns)
  "Filter FILE-LIST removing files matching EXCLUDE-PATTERNS relative to ROOT-PATH."
  ;; (message "DEBUG: Filtering excluded files, patterns: %S" exclude-patterns)
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
                   ;; (message "DEBUG: Checking if %s should be excluded" rel-path)
                   (let
                       ((should-exclude
                         (cl-some
                          (lambda
                            (pattern)
                            (let
                                ((match
                                  (string-match-p pattern rel-path)))
                              (when match
                                ;; (message "DEBUG: Excluding %s - matches pattern %s" rel-path pattern)
                                )
                              match))
                          exclude-patterns)))
                     should-exclude)))
               file-list))
        ;; (message "DEBUG: Excluded %d files"
        ;;          (- original-count
        ;;             (length file-list)))
        )
    ;; (message "DEBUG: No exclusion patterns, keeping all files")
    )
  file-list)

(defun --et-filter-hidden-files
    (file-list root-path include-hidden)
  "Filter FILE-LIST to exclude hidden files unless INCLUDE-HIDDEN is non-nil.
Only considers files with hidden components after ROOT-PATH."
  ;; (message "DEBUG: Filtering hidden files, include-hidden: %s" include-hidden)
  (if include-hidden
      (progn
        ;; (message "DEBUG: Including hidden files, no filtering needed")
        file-list)
    ;; Return the full list when include-hidden is true
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
                 ;; Skip checking if rel-path is just "." (current directory)
                 (if
                     (string= rel-path ".")
                     t  ;; Always include current directory
                   ;; Otherwise check components
                   (progn
                     (dolist
                         (component components)
                       (when
                           (string-match-p "^\\." component)
                         (setq has-hidden-component t)
                         ;; (message "DEBUG: Found hidden component: %s in relative path %s"
                         ;;          component rel-path)
                         ))
                     (let
                         ((result
                           (not has-hidden-component)))
                       (unless result
                         ;; (message "DEBUG: Excluding hidden file: %s" rel-path)
                         )
                       result)))))
             file-list))
      ;; (message "DEBUG: Excluded %d hidden files"
      ;;          (- original-count
      ;;             (length file-list)))
      file-list)))

;; Helper
;; ----------------------------------------

(defun --et-find-list-marked-paths-dired
    ()
  "Get list of marked files/directories in dired."
  (interactive)
  (when
      (eq major-mode 'dired-mode)
    (let
        ((found
          (dired-get-marked-files)))
      (progn
        ;; (message "Marked Files: %S" found)
        found))))

;; Deftest Finder
;; ----------------------------------------

(defun --et-find-deftest-file
    (file)
  "Extract ert-deftest names from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char
     (point-min))
    (let
        (tests)
      (while
          (re-search-forward "^(ert-deftest\\s-+\\([^[:space:]\n]+\\)" nil t)
        (push
         (cons file
               (match-string 1))
         tests))
      tests)))

(defun --et-find-deftest
    (&optional root-path)
  "Find all ert-deftest forms in ROOT-PATH."
  (let*
      ((test-files
        (--et-find-test-files-single root-path))
       (tests
        '()))
    (dolist
        (file test-files tests)
      (setq tests
            (append tests
                    (--et-find-deftest-file file))))))

(provide 'elisp-test-find)

(when
    (not load-file-name)
  (message "elisp-test-find.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
