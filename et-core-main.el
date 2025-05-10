;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-10 20:42:26>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/et-core-main.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)


(require 'et-core-variables)
(require 'et-core-run)
(require 'et-utils-find)
(require 'et-utils-plan)
(require 'et-ui-buffer)
(require 'et-ui-report)

;; Functions to run tests from a buffer

(defun elisp-test--run-buffer (file-path)
  "Run tests from FILE-PATH buffer."
  (let* ((tests (--elisp-test-find-deftest-file file-path))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (timeout-per-test-confirmed elisp-test-timeout-sec)
         (start-time (current-time))
         (test-results
          (elisp-test--run-multiple-tests tests
                                          timeout-per-test-confirmed))
         (total-time-spent
          (float-time (time-subtract (current-time) start-time))))

    ;; Generate the report for this file
    (elisp-test--report-results
     (elisp-test-buffer-create "*elisp-test-results*")
     test-results
     timeout-per-test-confirmed
     total-time-spent
     timestamp)))

;; Helper function to determine which paths to test based on context

(defun elisp-test--determine-test-paths (root-paths)
  "Determine which paths to test based on context and ROOT-PATHS."
  (cond
   ;; Case 1: Called from dired with marked files
   ((eq major-mode 'dired-mode)
    (--elisp-test-find-list-marked-paths-dired))

   ;; Case 2: Called with explicit root paths
   (root-paths
    ;; Handle both string and list arguments
    (if (stringp root-paths)
        (elisp-test-find-test-files-multiple (list root-paths))
      (elisp-test-find-test-files-multiple root-paths)))

   ;; Case 3: No specific paths, use current directory
   (t
    (elisp-test-find-test-files-multiple (list default-directory)))))

(defun elisp-test--determine-report-path ()
  "Determine where test reports should be saved based on context."
  (let ((default-org-path
         (or elisp-test-results-org-path
             (expand-file-name
              (format "%s.org" elisp-test-report-base-name)
              (or default-directory "~/")))))
    (if (eq major-mode 'dired-mode)
        ;; In dired mode - use the current directory for report
        (expand-file-name
         (or elisp-test-results-org-path-dired
             (format "%s.org" elisp-test-report-base-name))
         default-directory)
      ;; Otherwise use the default
      default-org-path)))

;; Helper function to confirm test execution

(defun elisp-test--confirm-and-get-timeout
    (test-alist no-confirm timeout-per-test)
  "Confirm running tests in TEST-ALIST and get timeout.
If NO-CONFIRM is non-nil, skip confirmation.
TIMEOUT-PER-TEST is used as the default timeout if provided."
  (when (and test-alist
             (or no-confirm
                 (yes-or-no-p
                  (format "Proceed with running these %s tests? "
                          (length test-alist)))))
    ;; Return the timeout to use
    (or timeout-per-test
        (if no-confirm
            elisp-test-timeout-sec
          (read-number "Timeout [s]: " elisp-test-timeout-sec)))))

;; Helper function to run tests and time execution

(defun elisp-test--execute-tests (test-alist timeout-per-test)
  "Run tests in TEST-ALIST with TIMEOUT-PER-TEST and return results with timing info."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (start-time (current-time))
         (test-results (elisp-test--run-multiple-tests
                        test-alist timeout-per-test))
         (total-time-spent (float-time
                            (time-subtract (current-time) start-time))))
    (list :timestamp timestamp
          :results test-results
          :total-time total-time-spent)))

;; Helper function to generate per-directory reports

(defun elisp-test--generate-per-directory-reports
    (test-results timeout total-time timestamp)
  "Generate per-directory reports for TEST-RESULTS.
Uses TIMEOUT, TOTAL-TIME and TIMESTAMP for report generation."
  (when elisp-test-generate-per-directory-reports
    (let ((directories
           (seq-uniq
            (mapcar
             (lambda (result)
               (file-name-directory (car result)))
             test-results))))
      ;; For each directory, create a report with only its tests
      (dolist (dir directories)
        ;; Set the report path to be in this directory
        (let ((elisp-test-results-org-path-final
               (expand-file-name elisp-test-results-org-path-dired dir))
              ;; Filter test results to only those in this directory
              (filtered-results
               (seq-filter
                (lambda (result)
                  (string-prefix-p dir (car result)))
                test-results)))
          ;; Generate report for this directory with shared timestamp
          (elisp-test--report-results
           (elisp-test-buffer-create "*elisp-test-results*")
           filtered-results
           timeout
           total-time
           timestamp))))))

(defun elisp-test-run
    (&optional root-paths timeout-per-test no-confirm)
  "Run tests from ROOT-PATHS, marked files in dired, or current buffer."
  (interactive)
  ;; Ensure we have a valid report path
  (setq elisp-test-results-org-path-final
        (elisp-test--determine-report-path))
  ;; Handle different scenarios
  (cond
   ;; In dired mode with marked files
   ((and (eq major-mode 'dired-mode)
         (--elisp-test-find-list-marked-paths-dired))
    (let* ((dired-paths (--elisp-test-find-list-marked-paths-dired)))
      ;; Add package paths
      (elisp-test--add-package-load-paths dired-paths)
      (let* ((test-alist (--elisp-test-find-deftest dired-paths))
             (timeout-confirmed
              (elisp-test--confirm-and-get-timeout
               test-alist no-confirm timeout-per-test)))
        (when timeout-confirmed
          (let* ((execution-data
                  (elisp-test--execute-tests test-alist
                                             timeout-confirmed))
                 (timestamp (plist-get execution-data :timestamp))
                 (test-results (plist-get execution-data :results))
                 (total-time-spent
                  (plist-get execution-data :total-time)))
            (elisp-test--report-results
             (elisp-test-buffer-create "*elisp-test-results*")
             test-results
             timeout-confirmed
             total-time-spent
             timestamp))))))
   ;; In a file buffer
   ((and buffer-file-name (not root-paths))
    ;; Add package path for single file
    (elisp-test--add-package-load-paths (list buffer-file-name))
    (elisp-test--run-buffer buffer-file-name))
   ;; Other cases
   (t
    (let* ((paths (elisp-test--determine-test-paths root-paths)))
      ;; Add package paths
      (elisp-test--add-package-load-paths paths)
      (let*
          ((test-alist (and paths (--elisp-test-find-deftest paths)))
           (timeout-confirmed
            (elisp-test--confirm-and-get-timeout
             test-alist no-confirm timeout-per-test)))
        (when timeout-confirmed
          (let* ((execution-data
                  (elisp-test--execute-tests test-alist
                                             timeout-confirmed))
                 (timestamp (plist-get execution-data :timestamp))
                 (test-results (plist-get execution-data :results))
                 (total-time-spent
                  (plist-get execution-data :total-time)))
            (elisp-test--report-results
             (elisp-test-buffer-create "*elisp-test-results*")
             test-results
             timeout-confirmed
             total-time-spent
             timestamp)))))))
  ;; Clean up
  (when (elisp-test-buffer "*ert*")
    (kill-buffer "*ert*")))

;; Helper function to prepare test plan

(defun elisp-test--prepare-test-plan (paths)
  "Create a test plan from PATHS by extracting ert-deftest definitions."
  (when paths
    ;; (message "Preparing test plan from %d files/directories"
    ;;          (length paths))
    (--elisp-test-find-deftest paths)))

;;; elisp-test-core-main.el ends here


(provide 'et-core-main)

(when
    (not load-file-name)
  (message "et-core-main.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))