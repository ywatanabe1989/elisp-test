;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-03 00:56:27>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/test-elisp-test-report.el

(require 'ert)
(require 'elisp-test-report)

(ert-deftest test-et--count-results-empty
    ()
  (let
      ((test-names
        (make-hash-table :test 'equal)))
    (should
     (equal
      (et--count-results
       '()
       test-names)
      '(0 0 0 0)))))

(ert-deftest test-et--count-results-passed
    ()
  (let
      ((test-names
        (make-hash-table :test 'equal))
       (results
        '(("file.el" "test1" "PASSED"))))
    (should
     (equal
      (et--count-results results test-names)
      '(1 0 0 0)))))

(ert-deftest test-et--count-results-failed
    ()
  (let
      ((test-names
        (make-hash-table :test 'equal))
       (results
        '(("file.el" "test1" "FAILED: reason"))))
    (should
     (equal
      (et--count-results results test-names)
      '(0 1 0 0)))))

(ert-deftest test-et--count-duplicates-none
    ()
  (let
      ((test-names
        (make-hash-table :test 'equal)))
    (puthash "test1" 1 test-names)
    (puthash "test2" 1 test-names)
    (should
     (=
      (et--count-duplicates test-names)
      0))))

(ert-deftest test-et--count-duplicates-one
    ()
  (let
      ((test-names
        (make-hash-table :test 'equal)))
    (puthash "test1" 2 test-names)
    (should
     (=
      (et--count-duplicates test-names)
      1))))

(ert-deftest test-et--insert-summary-basic
    ()
  (let
      ((buf
        (generate-new-buffer "*test*")))
    (unwind-protect
        (progn
          (et--insert-summary buf
                              '(1 0 0 0)
                              0)
          (with-current-buffer buf
            (should
             (string-match-p "Success Rate: 100.0%"
                             (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest test-et--insert-test-section-basic
    ()
  (let
      ((buf
        (generate-new-buffer "*test*"))
       (test-names
        (make-hash-table :test 'equal))
       (report-dir nil)
       (results
        '(("file.el" "test1" "PASSED"))))
    (unwind-protect
        (progn
          (et--insert-test-section buf results test-names report-dir
                                   (lambda
                                     (str)
                                     (string-match-p "PASSED" str))
                                   "Passed Tests")
          (with-current-buffer buf
            (should
             (string-match-p "test1"
                             (buffer-string)))))
      (kill-buffer buf))))

;; (ert-deftest test-et--insert-test-section-basic
;;     ()
;;   (let
;;       ((buf
;;         (generate-new-buffer "*test*"))
;;        (test-names
;;         (make-hash-table :test 'equal))
;;        (results
;;         '(("file.el" "test1" "PASSED"))))
;;     (unwind-protect
;;         (progn
;;           (et--insert-test-section buf results test-names
;;                                    (lambda
;;                                      (str)
;;                                      (string-match-p "PASSED" str))
;;                                    "Passed Tests")
;;           (with-current-buffer buf
;;             (should
;;              (string-match-p "test1"
;;                              (buffer-string)))))
;;       (kill-buffer buf))))

(provide 'test-elisp-test-report)

(when
    (not load-file-name)
  (message "test-elisp-test-report.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))