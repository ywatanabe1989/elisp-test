;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 05:29:24>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/test-elisp-test-run.el

(require 'ert)
(require 'elisp-test-run)

(ert-deftest test-et--run-single-test-not-found
    ()
  (let
      ((test-path
        (expand-file-name "nonexistent.el")))
    (let
        ((result
          (et--run-single-test
           (cons test-path "nonexistent-test"))))
      (should
       (equal
        (nth 0 result)
        test-path))
      (should
       (equal
        (nth 1 result)
        "nonexistent-test"))
      (should
       (string-match-p "Test not found\\|Cannot open"
                       (nth 2 result))))))

(ert-deftest test-et--run-single-test-error
    ()
  (let
      ((temp-file
        (make-temp-file "test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(ert-deftest test-error () (error \"Intentional error\"))"))
          (should
           (string-match-p
            "ERROR: .*Intentional error"
            (nth 2
                 (et--run-single-test
                  (cons temp-file "test-error"))))))
      (delete-file temp-file))))

(ert-deftest test-et--run-single-test-success
    ()
  (let
      ((temp-file
        (make-temp-file "test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(ert-deftest test-success () (should t))"))
          (should
           (string-match-p
            "PASSED"
            (nth 2
                 (et--run-single-test
                  (cons temp-file "test-success"))))))
      (delete-file temp-file))))

(ert-deftest test-et--run-single-test-timeout
    ()
  (let
      ((temp-file
        (make-temp-file "test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(ert-deftest test-timeout () (sleep-for 2))"))
          (should
           (string-match-p
            "TIMEOUT"
            (nth 2
                 (et--run-single-test
                  (cons temp-file "test-timeout")
                  1)))))
      (delete-file temp-file))))

(ert-deftest test-et--run-multiple-tests-basic
    ()
  (let
      ((temp-file
        (make-temp-file "test" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "(ert-deftest test-multi1 () (should t))
                    (ert-deftest test-multi2 () (should t))"))
          (should
           (= 2
              (length
               (et--run-multiple-tests
                (list
                 (cons temp-file "test-multi1")
                 (cons temp-file "test-multi2")))))))
      (delete-file temp-file))))

(provide 'test-elisp-test-run)

(when
    (not load-file-name)
  (message "test-elisp-test-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))