;;; test-elisp-test-dired-run.el --- Test running tests from dired -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-10 21:55:00>

;;; Commentary:

;; Tests for running elisp-test from a dired context

;;; Code:

(require 'ert)
(require 'elisp-test)
(require 'dired)

;; Test helpers for dired simulation
(defun setup-test-dired-env ()
  "Set up a test environment with dired buffer and test files."
  (let* ((test-dir (make-temp-file "elisp-test-dired-" t))
         ;; Create test files
         (test-file1 (expand-file-name "test-file1.el" test-dir))
         (test-file2 (expand-file-name "test-file2.el" test-dir))
         (non-test-file (expand-file-name "non-test.el" test-dir))
         (dired-buffer nil))
    
    ;; Create file content
    (with-temp-file test-file1
      (insert "(require 'ert)
(ert-deftest test-file1-passing () 
  \"A passing test in file1.\"
  (should t))
(provide 'test-file1)"))
    
    (with-temp-file test-file2
      (insert "(require 'ert)
(ert-deftest test-file2-passing () 
  \"A passing test in file2.\"
  (should t))
(ert-deftest test-file2-failing () 
  \"A failing test in file2.\"
  (should nil))
(provide 'test-file2)"))
    
    (with-temp-file non-test-file
      (insert "(provide 'non-test)"))
    
    ;; Create dired buffer
    (setq dired-buffer (dired-noselect test-dir))
    
    ;; Return the setup information
    (list :dir test-dir
          :test-file1 test-file1
          :test-file2 test-file2
          :non-test-file non-test-file
          :dired-buffer dired-buffer)))

(defun cleanup-test-dired-env (env)
  "Clean up the test environment ENV."
  (when (buffer-live-p (plist-get env :dired-buffer))
    (kill-buffer (plist-get env :dired-buffer)))
  
  (let ((dir (plist-get env :dir)))
    (when (and dir (file-exists-p dir))
      (delete-directory dir t))))

(defun simulate-dired-mark-files (dired-buffer files)
  "Simulate marking FILES in DIRED-BUFFER."
  (with-current-buffer dired-buffer
    (dired-unmark-all-marks)
    (dolist (file files)
      (when (dired-goto-file file)
        (dired-mark 1)))))

(ert-deftest test-elisp-test-dired-run-single-file ()
  "Test running a single test file from dired."
  (let* ((env (setup-test-dired-env))
         (test-file1 (plist-get env :test-file1))
         (dired-buffer (plist-get env :dired-buffer))
         (result nil)
         (elisp-test-results-org-path nil) ;; Prevent writing output files
         (elisp-test-generate-per-directory-reports nil))
    
    (unwind-protect
        (progn
          ;; Mark test-file1
          (simulate-dired-mark-files dired-buffer (list test-file1))
          
          ;; Mock the test execution
          (cl-letf (((symbol-function 'elisp-test--report-results)
                     (lambda (buf test-results timeout time timestamp)
                       (setq result test-results)
                       buf))
                    ;; Ensure we don't try to actually run tests in dired marked files
                    ((symbol-function 'elisp-test--confirm-and-get-timeout)
                     (lambda (test-alist no-confirm timeout-per-test)
                       timeout-per-test)))
            
            ;; Run tests from dired with marked files
            (with-current-buffer dired-buffer
              (call-interactively 'elisp-test-run))
            
            ;; Check results were collected
            (should result)))
      
      (cleanup-test-dired-env env))))

(ert-deftest test-elisp-test-dired-run-multiple-files ()
  "Test running multiple test files from dired."
  (let* ((env (setup-test-dired-env))
         (test-file1 (plist-get env :test-file1))
         (test-file2 (plist-get env :test-file2))
         (dired-buffer (plist-get env :dired-buffer))
         (result nil)
         (elisp-test-results-org-path nil) ;; Prevent writing output files
         (elisp-test-generate-per-directory-reports nil))
    
    (unwind-protect
        (progn
          ;; Mark test files
          (simulate-dired-mark-files dired-buffer (list test-file1 test-file2))
          
          ;; Mock the test execution
          (cl-letf (((symbol-function 'elisp-test--report-results)
                     (lambda (buf test-results timeout time timestamp)
                       (setq result test-results)
                       buf))
                    ;; Ensure we don't try to actually run tests in dired marked files
                    ((symbol-function 'elisp-test--confirm-and-get-timeout)
                     (lambda (test-alist no-confirm timeout-per-test)
                       timeout-per-test)))
            
            ;; Run tests from dired with marked files
            (with-current-buffer dired-buffer
              (call-interactively 'elisp-test-run))
            
            ;; Check results
            (should result)))
      
      (cleanup-test-dired-env env))))

;; Testing the list-marked-paths functionality
(ert-deftest test-elisp-test-dired-list-marked-paths ()
  "Test the function to get marked files from dired."
  (let* ((env (setup-test-dired-env))
         (test-file1 (plist-get env :test-file1))
         (test-file2 (plist-get env :test-file2))
         (dired-buffer (plist-get env :dired-buffer))
         (marked-paths nil))
    
    (unwind-protect
        (progn
          ;; Mark test files
          (simulate-dired-mark-files dired-buffer (list test-file1 test-file2))
          
          ;; Get marked paths
          (with-current-buffer dired-buffer
            (setq marked-paths (elisp-test--find-list-marked-paths-dired)))
          
          ;; Check paths
          (should (= 2 (length marked-paths)))
          (should (member test-file1 marked-paths))
          (should (member test-file2 marked-paths)))
      
      (cleanup-test-dired-env env))))

(provide 'test-elisp-test-dired-run)

;;; test-elisp-test-dired-run.el ends here