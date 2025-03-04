;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-05 07:27:38>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-test/elisp-test-run.el

;; Single Test Runner
;; ----------------------------------------

(defun et--run-single-test
    (test &optional timeout)
  "Run a single TEST with TIMEOUT (defaults to 10 seconds)."
  (let
      ((file
        (car test))
       (testname
        (cdr test))
       (timeout-secs
        (or timeout 10)))
    (with-current-buffer
        (get-buffer-create "*ert*")
      (let
          ((inhibit-read-only t))
        (erase-buffer)))
    (condition-case err
        (progn
          (load file nil t)
          (let
              ((test-symbol
                (intern testname)))
            (if
                (ert-test-boundp test-symbol)
                (with-timeout
                    (timeout-secs
                     (list file testname
                           (format "TIMEOUT: Test exceeded time limit of %s" timeout-secs)))
                  (ert test-symbol)
                  (list file testname
                        (with-current-buffer "*ert*"
                          (let
                              ((output
                                (buffer-substring-no-properties
                                 (point-min)
                                 (point-max))))
                            (cond
                             ((string-match "Failed:\\s-*0\\b" output)
                              "PASSED")
                             ((string-match "Failed:\\s-*[1-9][0-9]*\\b" output)
                              (concat "FAILED: " output))
                             ((string-match "Skipped:\\s-*[1-9][0-9]*\\b" output)
                              (concat "SKIPPED: " output))
                             (t output))))))
              (list file testname "NOT-FOUND: Test not found"))))
      (error
       (list file testname
             (format "ERROR: %S" err))))))

;; Multiple Tests Runner
;; ----------------------------------------

(defun et--run-multiple-tests
    (test-alist &optional timeout-per-test)
  "Run multiple tests from TEST-ALIST and return ((path selector results) ...).
Tests are run sequentially to avoid loading conflicts."
  ;; Sequential execution
  (message "Running tests sequentially")
  (mapcar
   (lambda
     (test)
     (et--run-single-test test timeout-per-test))
   test-alist))

(provide 'elisp-test-run)

(when
    (not load-file-name)
  (message "elisp-test-run.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))