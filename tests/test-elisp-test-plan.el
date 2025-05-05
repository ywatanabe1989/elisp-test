;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-08 14:25:50>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/tests/test-elisp-test-plan.el

(require 'ert)
(require 'elisp-test-plan)

(ert-deftest test-elisp-test--prepare-test-plan-empty-paths
    ()
  (should
   (null
    (elisp-test--prepare-test-plan
     '()))))

;; (ert-deftest test-elisp-test--prepare-test-plan-buffer-mode
;;     ()
;;   (let
;;       ((mock-tests
;;         '(("/path/test.el" . "test-function"))))
;;     (cl-letf
;;         (((symbol-function '--elisp-test-find-deftest)
;;           (lambda
;;             (_)
;;             mock-tests))
;;          ((symbol-function 'org-mode)
;;           (lambda
;;             ()
;;             (setq major-mode 'org-mode))))
;;       (unwind-protect
;;           (should
;;            (equal (elisp-test--prepare-test-plan '("/path/test.el"))
;;                   mock-tests))))))

(ert-deftest test-elisp-test--prepare-test-plan-buffer-mode
    ()
  (let
      ((mock-tests
        '(("/path/test.el" . "test-function"))))
    (cl-letf
        (((symbol-function '--elisp-test-find-deftest)
          (lambda
            (_)
            mock-tests))
         ((symbol-function 'org-mode)
          (lambda
            ()
            (setq major-mode 'org-mode)))
         ;; Mock org-element-type function that's being used
         ((symbol-function 'org-element-type)
          (lambda (element) nil)))
      (unwind-protect
          (should
           (equal (elisp-test--prepare-test-plan '("/path/test.el"))
                  mock-tests))))))

(ert-deftest test-elisp-test--prepare-test-plan-returns-tests
    ()
  (let
      ((mock-tests
        '(("/path/test.el" . "test-function"))))
    (cl-letf
        (((symbol-function '--elisp-test-find-deftest)
          (lambda
            (_)
            mock-tests)))
      (should
       (equal
        (elisp-test--prepare-test-plan
         '("/path/test.el"))
        mock-tests)))))

(provide 'test-elisp-test-plan)

(when
    (not load-file-name)
  (message "test-elisp-test-plan.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))