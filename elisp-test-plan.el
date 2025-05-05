;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:54:53>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/elisp-test-plan.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun elisp-test--prepare-test-plan
    (paths)
  "Create test execution plan from PATHS and return list of tests."
  (let*
      ((tests
        (mapcan #'--elisp-test-find-deftest paths))
       (test-names
        (mapcar #'cdr tests))
       (duplicate-names
        (seq-filter
         (lambda
           (name)
           (>
            (seq-count
             (lambda
               (n)
               (equal n name))
             test-names)
            1))
         (seq-uniq test-names))))
    (when tests
      (with-current-buffer
          (elisp-test-buffer-create elisp-test-plan-buffer-name)
        (when buffer-read-only
          (read-only-mode -1))
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: Elisp Test Plan\n")
        (insert "#+STARTUP: overview\n\n")
        (insert "* Found Tests\n\n")
        (dolist
            (test tests)
          (let
              ((file-path
                (car test))
               (test-name
                (cdr test)))
            (insert
             (format "- [[file:%s][%s]] :: %s\n"
                     file-path
                     (file-name-nondirectory file-path)
                     test-name))))
        ;; Add a section for duplicate test names if any exist
        (when duplicate-names
          (insert "\n* Duplicate Test Names\n\n")
          (dolist
              (dup-name duplicate-names)
            (insert
             (format "** %s\n" dup-name))
            (dolist
                (test tests)
              (when
                  (equal
                   (cdr test)
                   dup-name)
                (let
                    ((file-path
                      (car test)))
                  (insert
                   (format "- [[file:%s][%s]]\n"
                           file-path
                           (file-name-nondirectory file-path))))))))
        (org-mode)
        ;; Re-enable org-mode to ensure links are processed
        (read-only-mode 1)
        ;; Make the buffer read-only
        (local-set-key
         (kbd "q")
         'kill-this-buffer)
        ;; Allow quitting with 'q'
        (goto-char
         (point-min))
        (org-fold-show-all)
        (display-buffer
         (current-buffer)))

      ;; Cleanup
      (when
          (elisp-test-buffer elisp-test-plan-buffer-name)
        (kill-buffer elisp-test-plan-buffer-name))

      tests)))


(provide 'elisp-test-plan)

(when
    (not load-file-name)
  (message "elisp-test-plan.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))