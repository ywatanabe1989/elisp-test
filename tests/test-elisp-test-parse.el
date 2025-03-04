;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 07:19:51>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/elisp-test-parse.el

(require 'ert)
(require 'elisp-test-parse)

(ert-deftest test-et--parse-test-result-timeout
    ()
  (should
   (equal
    (et--parse-test-result "TIMEOUT: Test timed out")
    '(0 0 0 1))))

(ert-deftest test-et--parse-test-result-error
    ()
  (should
   (equal
    (et--parse-test-result "ERROR: Test failed with error")
    '(0 1 0 0))))

(ert-deftest test-et--parse-test-result-not-found
    ()
  (should
   (equal
    (et--parse-test-result "NOT-FOUND: Test not found")
    '(0 0 1 0))))

(ert-deftest test-et--parse-test-result-passed
    ()
  (should
   (equal
    (et--parse-test-result "Passed: 1")
    '(1 0 0 0))))

(ert-deftest test-et--parse-test-result-failed
    ()
  (should
   (equal
    (et--parse-test-result "Failed: 1")
    '(0 1 0 0))))

(ert-deftest test-et--parse-test-result-skipped
    ()
  (should
   (equal
    (et--parse-test-result "Skipped: 1")
    '(0 0 1 0))))

(ert-deftest test-et--parse-test-result-mixed
    ()
  (should
   (equal
    (et--parse-test-result "Passed: 2\nFailed: 1\nSkipped: 3")
    '(2 1 3 0))))

(provide 'elisp-test-parse)

(when
    (not load-file-name)
  (message "elisp-test-parse.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))