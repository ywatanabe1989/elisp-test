;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:28:49>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-utils-plan.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'et-core-variables)
(require 'et-ui-buffer)
(require 'et-utils-find)

(defun elisp-test--prepare-test-plan (paths)
  "Create test execution plan from PATHS and return list of tests."
  (let* ((tests (mapcan #'--elisp-test-find-deftest paths))
         (test-names (mapcar #'cdr tests))
         (duplicate-names
          (seq-filter
           (lambda (name)
             (> (seq-count
                 (lambda (n) (equal n name))
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
        (dolist (test tests)
          (let ((file-path (car test))
                (test-name (cdr test)))
            (insert
             (format "- [[file:%s][%s]] :: %s\n"
                     file-path
                     (file-name-nondirectory file-path)
                     test-name))))
        ;; Add a section for duplicate test names if any exist
        (when duplicate-names
          (insert "\n* Duplicate Test Names\n\n")
          (dolist (dup-name duplicate-names)
            (insert (format "** %s\n" dup-name))
            (dolist (test tests)
              (when (equal (cdr test) dup-name)
                (let ((file-path (car test)))
                  (insert
                   (format "- [[file:%s][%s]]\n"
                           file-path
                           (file-name-nondirectory file-path))))))))
        ;; Re-enable org-mode to ensure links are processed
        (org-mode)
        ;; Make the buffer read-only
        (read-only-mode 1)
        ;; Allow quitting with 'q'
        (local-set-key (kbd "q") 'kill-this-buffer)
        (goto-char (point-min))
        (org-fold-show-all)
        (display-buffer (current-buffer)))

      ;; Cleanup
      (when (elisp-test-buffer elisp-test-plan-buffer-name)
        (kill-buffer elisp-test-plan-buffer-name))

      tests)))

;;; elisp-test-util-plan.el ends here


(provide 'et-utils-plan)

(when
    (not load-file-name)
  (message "et-utils-plan.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))