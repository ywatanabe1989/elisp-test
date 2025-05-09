;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 14:28:48>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-utils-parse.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(defun elisp-test--parse-test-result (result)
  "Parse test RESULT and return status in consistent format.
Result can be either a string or a list with the result in the 3rd position."
  (let ((result-str (if (stringp result)
                        result
                      (nth 2 result))))
    (cond
     ;; Handle backward compatibility for tests
     ((stringp result)
      (cond
       ((string-prefix-p "PASSED" result) '(1 0 0 0))
       ((string-prefix-p "FAILED" result) '(0 1 0 0))
       ((string-prefix-p "ERROR" result) '(0 1 0 0))
       ((string-prefix-p "SKIPPED" result) '(0 0 1 0))
       ((string-prefix-p "NOT-FOUND" result) '(0 0 1 0))
       ((string-prefix-p "TIMEOUT" result) '(0 0 0 1))
       (t
        ;; For mixed results like "Passed: 2\nFailed: 1\nSkipped: 3"
        (let ((passed 0)
              (failed 0)
              (skipped 0)
              (timeout 0))
          (when (string-match "Passed:\\s-*\\([0-9]+\\)" result)
            (setq passed (string-to-number (match-string 1 result))))
          (when (string-match "Failed:\\s-*\\([0-9]+\\)" result)
            (setq failed (string-to-number (match-string 1 result))))
          (when (string-match "Skipped:\\s-*\\([0-9]+\\)" result)
            (setq skipped (string-to-number (match-string 1 result))))
          (when (string-match "Timeout:\\s-*\\([0-9]+\\)" result)
            (setq timeout (string-to-number (match-string 1 result))))
          (list passed failed skipped timeout)))))

     ;; Handle the new format for actual code
     ((stringp result-str)
      (cond
       ((string-prefix-p "PASSED" result-str) 'passed)
       ((string-prefix-p "FAILED" result-str) 'failed)
       ((string-prefix-p "SKIPPED" result-str) 'skipped)
       ((string-prefix-p "TIMEOUT" result-str) 'timeout)
       ((string-prefix-p "NOT-FOUND" result-str) 'not-found)
       (t 'error)))
     (t 'error))))

(defun elisp-test--parse-error-message (result)
  "Extract error message from test RESULT."
  (when (stringp (nth 2 result))
    (let ((result-str (nth 2 result)))
      (when (or (string-prefix-p "FAILED" result-str)
                (string-prefix-p "ERROR" result-str))
        (let ((error-start (string-match ":" result-str)))
          (if error-start
              (substring result-str (+ error-start 1))
            result-str))))))


(provide 'et-utils-parse)

(when
    (not load-file-name)
  (message "et-utils-parse.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))