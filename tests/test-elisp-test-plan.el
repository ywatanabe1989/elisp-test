;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 07:30:36>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/test-elisp-test-plan.el

(require 'ert)
(require 'elisp-test-plan)

(ert-deftest test-et--prepare-test-plan-empty-paths
    ()
  (should
   (null
    (et--prepare-test-plan
     '()))))

(ert-deftest test-et--prepare-test-plan-buffer-mode
    ()
  (let
      ((mock-tests
        '(("/path/test.el" . "test-function"))))
    (cl-letf
        (((symbol-function '--et-find-deftest)
          (lambda
            (_)
            mock-tests))
         ((symbol-function 'org-mode)
          (lambda
            ()
            (setq major-mode 'org-mode))))
      (unwind-protect
          (progn
            (et--prepare-test-plan
             '("/path/test.el")))))))

(ert-deftest test-et--prepare-test-plan-returns-tests
    ()
  (let
      ((mock-tests
        '(("/path/test.el" . "test-function"))))
    (cl-letf
        (((symbol-function '--et-find-deftest)
          (lambda
            (_)
            mock-tests)))
      (should
       (equal
        (et--prepare-test-plan
         '("/path/test.el"))
        mock-tests)))))

(provide 'test-elisp-test-plan)

(when
    (not load-file-name)
  (message "test-elisp-test-plan.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
