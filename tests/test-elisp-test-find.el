;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-03 10:28:16>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/test-elisp-test-find.el

(require 'ert)

(ert-deftest test-elisp-test-find-test-files-multiple
    ()
  "Tests finding test files across multiple root paths."
  (let
      ((test-paths
        '("/tmp/test-dir1" "/tmp/test-dir2"))
       (expected-files
        '("/tmp/test-dir1/test-sample.el" "/tmp/test-dir2/test-another.el")))
    (cl-letf
        (((symbol-function '--elisp-test-find-test-files-single)
          (lambda
            (path &optional _)
            (if
                (string= path "/tmp/test-dir1")
                '("/tmp/test-dir1/test-sample.el")
              '("/tmp/test-dir2/test-another.el")))))
      (should
       (equal
        (elisp-test-find-test-files-multiple test-paths)
        expected-files)))))

(ert-deftest test-elisp-test-find-test-files-multiple-with-nil
    ()
  "Tests that nil root paths returns nil."
  (should
   (eq
    (elisp-test-find-test-files-multiple nil)
    nil)))

(ert-deftest test-elisp-test--find-matching-files-directory
    ()
  "Tests finding matching files in a directory."
  (let
      ((root-path "/tmp/test-dir")
       (patterns
        '("\\.el$"))
       (expected-files
        '("/tmp/test-dir/file1.el" "/tmp/test-dir/file2.el")))
    (cl-letf
        (((symbol-function 'file-directory-p)
          (lambda
            (_)
            t))
         ((symbol-function 'directory-files-recursively)
          (lambda
            (dir pattern &optional _)
            (when
                (and
                 (string= dir "/tmp/test-dir")
                 (string= pattern "\\.el$"))
              expected-files))))
      (should
       (equal
        (--elisp-test-find-matching-files root-path patterns)
        expected-files)))))

(ert-deftest test-elisp-test--find-matching-files-single-file
    ()
  "Tests finding matching files for a single file path."
  (let
      ((root-path "/tmp/test-file.el")
       (patterns
        '("\\.el$")))
    (cl-letf
        (((symbol-function 'file-directory-p)
          (lambda
            (_)
            nil))
         ((symbol-function 'file-name-nondirectory)
          (lambda
            (_)
            "test-file.el"))
         ((symbol-function 'string-match-p)
          (lambda
            (pattern _)
            (string= pattern "\\.el$"))))
      (should
       (equal
        (--elisp-test-find-matching-files root-path patterns)
        '("/tmp/test-file.el"))))))

(ert-deftest test-elisp-test--filter-excluded-files-no-patterns
    ()
  "Tests filtering excluded files with no exclude patterns."
  (let
      ((file-list
        '("/tmp/test-dir/file1.el" "/tmp/test-dir/file2.el"))
       (root-path "/tmp/test-dir")
       (exclude-patterns nil))
    (should
     (equal
      (--elisp-test-filter-excluded-files file-list root-path exclude-patterns)
      file-list))))

(ert-deftest test-elisp-test--filter-hidden-files
    ()
  "Tests filtering hidden files."
  (let
      ((file-list
        '("/tmp/test-dir/file1.el" "/tmp/test-dir/.hidden/file2.el"))
       (root-path "/tmp/test-dir")
       (include-hidden nil))
    (should
     (equal
      (--elisp-test-filter-hidden-files file-list root-path include-hidden)
      '("/tmp/test-dir/file1.el")))))

(ert-deftest test-elisp-test--filter-hidden-files-include-hidden
    ()
  "Tests including hidden files when requested."
  (let
      ((file-list
        '("/tmp/test-dir/file1.el" "/tmp/test-dir/.hidden/file2.el"))
       (root-path "/tmp/test-dir")
       (include-hidden t))
    (should
     (equal
      (--elisp-test-filter-hidden-files file-list root-path include-hidden)
      file-list))))

(ert-deftest test-elisp-test--find-list-marked-paths-dired
    ()
  "Tests getting marked files in dired mode."
  (when
      (fboundp 'dired-elisp-test-marked-files)
    (let
        ((major-mode 'dired-mode))
      (cl-letf
          (((symbol-function 'dired-elisp-test-marked-files)
            (lambda
              ()
              '("/tmp/test-dir/file1.el" "/tmp/test-dir/file2.el"))))
        (should
         (equal
          (--elisp-test-find-list-marked-paths-dired)
          '("/tmp/test-dir/file1.el" "/tmp/test-dir/file2.el")))))))

(ert-deftest test-elisp-test--find-deftest-file
    ()
  "Tests extracting deftest names from a file."
  (let
      ((temp-file
        (make-temp-file "test-elisp-test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(ert-deftest test-function1 ()\n  (should t))\n\n"
                    "(ert-deftest test-function2 ()\n  (should nil))"))
          (let
              ((result
                (--elisp-test-find-deftest-file temp-file)))
            (should
             (=
              (length result)
              2))
            (should
             (equal
              (cdr
               (nth 0 result))
              "test-function2"))
            (should
             (equal
              (cdr
               (nth 1 result))
              "test-function1"))))
      (delete-file temp-file))))

(ert-deftest test-elisp-test--find-deftest
    ()
  "Tests finding all deftest forms in files."
  (cl-letf
      (((symbol-function '--elisp-test-find-test-files-single)
        (lambda
          (path)
          (if path
              '("/tmp/test-dir/test-file1.el" "/tmp/test-dir/test-file2.el")
            '("/tmp/test-dir/test-file1.el"))))
       ((symbol-function '--elisp-test-find-deftest-file)
        (lambda
          (file)
          (if
              (string= file "/tmp/test-dir/test-file1.el")
              (list
               (cons file "test-func1"))
            (list
             (cons file "test-func1"))))))
    (let
        ((result
          (--elisp-test-find-deftest "/tmp/test-dir")))
      (should
       (=
        (length result)
        2))
      (should
       (equal
        (cdr
         (nth 0 result))
        "test-func1"))
      (should
       (equal
        (cdr
         (nth 1 result))
        "test-func1")))))

(ert-deftest test-elisp-test--find-deftest-no-path
    ()
  "Tests finding all deftest forms without specifying a path."
  (cl-letf
      (((symbol-function '--elisp-test-find-test-files-single)
        (lambda
          (&optional path)
          '("/tmp/test-file1.el")))
       ((symbol-function '--elisp-test-find-deftest-file)
        (lambda
          (file)
          (list
           (cons file "test-func1")))))
    (let
        ((result
          (--elisp-test-find-deftest)))
      (should
       (=
        (length result)
        1))
      (should
       (equal
        (cdr
         (car result))
        "test-func1")))))

(provide 'test-elisp-test-find)

(provide 'test-elisp-test-find)

(when
    (not load-file-name)
  (message "test-elisp-test-find.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))