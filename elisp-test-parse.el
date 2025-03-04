;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 06:29:57>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/elisp-test-parse.el

;; Handles parsing and summarizing test results.

;; Parser
;; ----------------------------------------

(defun et--parse-test-result
    (result-str)
  "Parse test result string to extract pass/fail counts."
  (let
      ((passed 0)
       (failed 0)
       (skipped 0)
       (timeout 0))
    (cond
     ((string-prefix-p "TIMEOUT:" result-str)
      (cl-incf timeout))
     ((string-prefix-p "ERROR:" result-str)
      (cl-incf failed))
     ((string-prefix-p "NOT-FOUND:" result-str)
      (cl-incf skipped))
     (t
      (when
          (string-match "Passed:\\s-*\\([0-9]+\\)" result-str)
        (setq passed
              (string-to-number
               (match-string 1 result-str))))
      (when
          (string-match "Failed:\\s-*\\([0-9]+\\)" result-str)
        (setq failed
              (+ failed
                 (string-to-number
                  (match-string 1 result-str)))))
      (when
          (string-match "Skipped:\\s-*\\([0-9]+\\)" result-str)
        (setq skipped
              (string-to-number
               (match-string 1 result-str))))))
    (list passed failed skipped timeout)))

(provide 'elisp-test-parse)

(when
    (not load-file-name)
  (message "elisp-test-parse.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))