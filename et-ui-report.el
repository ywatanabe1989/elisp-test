;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-09 13:07:58>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-ui-report.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


;;; elisp-test-ui-report.el --- Test report generation for elisp-test -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-09 02:35:00>

;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;; Commentary:

;; Provides test report generation functions for the elisp-test framework.
;; This module handles creating and exporting test reports in Org format.

;;; Code:

(require 'et-core-variables)
(require 'et-ui-buffer)

(defun elisp-test--report-results
    (buffer test-results &optional timeout-per-test total-time-spent
            timestamp)
  "Save results from BUFFER using TEST-RESULTS to file if needed."
  (let* ((test-names (make-hash-table :test 'equal))
         (report-dir
          (file-name-directory elisp-test-results-org-path-switched))
         (old-dir (expand-file-name ".old" report-dir))
         ;; Use provided timestamp or generate a new one
         (timestamp
          (or timestamp (format-time-string "%Y%m%d-%H%M%S")))
         (totals (elisp-test--count-results test-results test-names))
         (duplicates (elisp-test--count-duplicates test-names))
         (total-passed (nth 0 totals))
         (total-failed (nth 1 totals))
         (total-skipped (nth 2 totals))
         (total-timeout (nth 3 totals))
         (total
          (+ total-passed total-failed total-skipped total-timeout))
         (success-rate
          (if (> total 0)
              (* 100.0 (/ (float total-passed) total))
            0.0))
         ;; Create success rate formatted filename
         (success-rate-str (format "%d-PERCENT" (round success-rate)))
         (org-file-with-rate
          (replace-regexp-in-string
           "\\.org$"
           (format "-%s-%s.org" timestamp success-rate-str)
           elisp-test-results-org-path-switched))
         (pdf-file-with-rate
          (replace-regexp-in-string
           "\\.pdf$"
           (format "-%s-%s.pdf" timestamp success-rate-str)
           (concat (file-name-sans-extension
                    elisp-test-results-org-path-switched)
                   ".pdf"))))

    ;; Create .old directory if it doesn't exist
    (unless (file-exists-p old-dir)
      (make-directory old-dir t))

    ;; Move old reports to .old directory (only in the report directory)
    (dolist (file (directory-files report-dir t
                                   "ELISP-TEST-REPORT.*\\.\\(org\\|pdf\\)$"))
      (when (and (file-regular-p file)
                 (not (string-equal
                       (file-name-nondirectory file)
                       (file-name-nondirectory org-file-with-rate)))
                 (not (string-equal
                       (file-name-nondirectory file)
                       (file-name-nondirectory pdf-file-with-rate))))
        (rename-file file
                     (expand-file-name
                      (file-name-nondirectory file)
                      old-dir)
                     t)))

    ;; Create the report content
    (with-temp-buffer
      (org-mode)
      ;; Insert summary
      (elisp-test--insert-summary
       (current-buffer)
       totals
       duplicates
       timeout-per-test
       total-time-spent)

      ;; Insert test sections
      ;; --------------------
      ;; Passed
      (elisp-test--insert-test-section
       (current-buffer)
       test-results test-names report-dir
       (lambda (str) (string-match-p "PASSED" str))
       "Passed Tests")
      ;; Failed
      (elisp-test--insert-test-section
       (current-buffer)
       test-results test-names report-dir
       (lambda (str) (string-match-p "\\(ERROR\\|FAILED\\)" str))
       "Failed Tests")
      ;; Timeout
      (elisp-test--insert-test-section
       (current-buffer)
       test-results test-names report-dir
       (lambda (str) (string-match-p "TIMEOUT:" str))
       (format "Timeout Tests (= %s s)" timeout-per-test))
      ;; Not Found
      (elisp-test--insert-test-section
       (current-buffer)
       test-results test-names report-dir
       (lambda (str) (string-match-p "NOT-FOUND:" str))
       "Not Found Tests")
      ;; Write the buffer to a file with success rate
      (write-region (point-min) (point-max) org-file-with-rate))

    ;; Generate PDF if possible
    (let ((latex-deps '("pdflatex" "latex")))
      (when (and (require 'ox-latex nil t)
                 (cl-every #'executable-find latex-deps))
        (let ((buf (elisp-test-file-buffer org-file-with-rate)))
          (unless buf
            (setq buf (find-file-noselect org-file-with-rate)))
          (when buf
            (with-current-buffer buf
              (org-fold-show-all)
              (org-latex-export-to-pdf)
              (let ((tex-file
                     (concat
                      (file-name-sans-extension org-file-with-rate)
                      ".tex")))
                (when (file-exists-p tex-file)
                  (delete-file tex-file)))))
          (when (elisp-test-buffer "*Org PDF LaTeX Output*")
            (kill-buffer "*Org PDF LaTeX Output*")))))))

;; Helpers
;; ----------------------------------------

(defun elisp-test--count-results (results test-names)
  "Count totals from test RESULTS, updating TEST-NAMES hash table."
  (let ((total-passed 0)
        (total-failed 0)
        (total-skipped 0)
        (total-timeout 0))
    (dolist (result results)
      (let* ((test-name (cadr result))
             (output (nth 2 result)))
        (puthash test-name
                 (1+ (gethash test-name test-names 0))
                 test-names)
        (cond
         ((equal output "PASSED")
          (cl-incf total-passed))
         ((string-prefix-p "FAILED:" output)
          (cl-incf total-failed))
         ((string-prefix-p "SKIPPED:" output)
          (cl-incf total-skipped))
         ((string-prefix-p "TIMEOUT:" output)
          (cl-incf total-timeout))
         ((string-prefix-p "NOT-FOUND:" output)
          (cl-incf total-skipped))
         ((string-prefix-p "ERROR:" output)
          (cl-incf total-failed)))))
    (list total-passed total-failed total-skipped total-timeout)))

(defun elisp-test--count-duplicates (test-names)
  "Count duplicate test names in TEST-NAMES hash table."
  (let ((total-duplicates 0))
    (maphash
     (lambda (k v)
       (when (> v 1)
         (cl-incf total-duplicates)))
     test-names)
    total-duplicates))

(defun elisp-test--insert-summary
    (buffer totals duplicates &optional timeout-per-test
            total-time-spent)
  "Insert summary section into BUFFER using TOTALS and DUPLICATES count."
  (with-current-buffer buffer
    (let* ((total-passed (nth 0 totals))
           (total-failed (nth 1 totals))
           (total-skipped (nth 2 totals))
           (total-timeout (nth 3 totals))
           (total
            (+ total-passed total-failed total-skipped total-timeout))
           (success-rate
            (if (> total 0)
                (* 100.0 (/ (float total-passed) total))
              0.0))
           (timeout-str (format "%s" timeout-per-test)))
      (insert "#+TITLE: Elisp Test Report\n")
      (insert "#+AUTHOR: ywatanabe\n")
      (insert "#+DATE: ")
      (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
      (insert
       " Created by https://github.com/ywatanabe1989/emacs-test\n\n")
      (insert "* Test Results Summary\n\n")
      (insert (format "- Passed: %d\n" total-passed))
      (insert (format "- Failed: %d\n" total-failed))
      (insert (format "- Skipped: %d\n" total-skipped))
      (insert
       (format "- Timeout (= %s s): %d\n" timeout-str total-timeout))
      (insert (format "- Duplicates: %d\n" duplicates))
      (insert (format "- Total: %d\n" total))
      (when total-time-spent
        (insert
         (format "- Total Time: %.2f seconds\n" total-time-spent)))
      (insert (format "- Success Rate: %.1f%%\n\n" success-rate)))))

(defun elisp-test--insert-test-section
    (buffer results test-names report-dir condition section-title)
  "Insert test section into BUFFER for tests matching CONDITION."
  (with-current-buffer buffer
    (let ((matching-tests
           (seq-filter
            (lambda (result)
              (and (nth 2 result)
                   (funcall condition (nth 2 result))))
            results)))
      ;; Only insert section if there are matching tests
      (when matching-tests
        (insert (format "* %s (%d)\n" section-title
                        (length matching-tests)))
        ;; Group results by file
        (let ((files-hash (make-hash-table :test 'equal)))
          ;; First group by files
          (dolist (result matching-tests)
            (let ((file (car result)))
              (push result
                    (gethash file files-hash '()))))
          ;; Then output by file groups
          (maphash
           (lambda (file file-results)
             (let ((file-name (file-name-nondirectory file))
                   (rel-path
                    (if report-dir
                        (file-relative-name file report-dir)
                      file)))
               (insert (format "** %s (%d tests)\n" file-name
                               (length file-results)))
               (dolist (result file-results)
                 (let* ((test-name (cadr result))
                        (output (nth 2 result))
                        (duplicate-tag
                         (if (> (gethash test-name test-names 0) 1)
                             " [DUPLICATE]"
                           "")))
                   (insert (format "- [[file:%s::%s][%s]]%s\n"
                                   rel-path
                                   test-name
                                   test-name
                                   duplicate-tag))
                   ;; Add complete error details for failed tests
                   (when
                       (string-match-p "\\(ERROR\\|FAILED\\)" output)
                     (insert "  + Error details:\n")
                     ;; Extract the entire error content
                     (let ((error-content output))
                       ;; Remove just the initial ERROR/FAILED: prefix if present
                       (when (string-match
                              "^\\(ERROR\\|FAILED\\):\\s-*"
                              error-content)
                         (setq error-content
                               (substring error-content
                                          (match-end 0))))
                       ;; Format and insert the entire error content
                       (dolist
                           (line (split-string error-content "\n"))
                         (insert (format "    %s\n" line)))))))))
           files-hash))))))

;;; elisp-test-ui-report.el ends here


(provide 'et-ui-report)

(when
    (not load-file-name)
  (message "et-ui-report.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))