;;; test-elisp-test-reporting.el --- Test the reporting functionality -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-10 22:25:00>

;;; Commentary:

;; Tests for the test results reporting functionality

;;; Code:

(require 'ert)
(require 'elisp-test)

(ert-deftest test-elisp-test-report-results ()
  "Test generation of test results report."
  (let* ((test-results 
          '(("/path/to/test1.el" "test1" "PASSED")
            ("/path/to/test2.el" "test2" "FAILED: Some error message")
            ("/path/to/test3.el" "test3" "PASSED")
            ("/path/to/test4.el" "test4" "SKIPPED: Skipped test")))
         (timeout 10)
         (total-time 0.5)
         (timestamp "20250510-222500")
         (buffer (get-buffer-create "*test-report-buffer*"))
         (report-content nil)
         (elisp-test-results-org-path nil) ;; Prevent writing output files
         (elisp-test-generate-per-directory-reports nil))
    
    (unwind-protect
        (progn
          ;; Generate report
          (elisp-test--report-results buffer test-results timeout total-time timestamp)
          
          ;; Capture report content
          (with-current-buffer buffer
            (setq report-content (buffer-string)))
          
          ;; Check results
          (should (string-match-p "ELISP-TEST Report" report-content))
          (should (string-match-p "Timestamp: 20250510-222500" report-content))
          (should (string-match-p "Timeout: 10 seconds" report-content))
          (should (string-match-p "Total Time: 0.5" report-content))
          
          ;; Check summary statistics
          (should (string-match-p "Total Tests: 4" report-content))
          (should (string-match-p "Passed: 2" report-content))
          (should (string-match-p "Failed: 1" report-content))
          (should (string-match-p "Skipped: 1" report-content))
          (should (string-match-p "Success Rate: 50.0%" report-content))
          
          ;; Check individual test results
          (should (string-match-p "test1.*PASSED" report-content))
          (should (string-match-p "test2.*FAILED" report-content))
          (should (string-match-p "test3.*PASSED" report-content))
          (should (string-match-p "test4.*SKIPPED" report-content)))
      
      ;; Cleanup
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-elisp-test-per-directory-reports ()
  "Test generation of per-directory test reports."
  (let* ((test-results 
          '(("/path/to/dir1/test1.el" "test1" "PASSED")
            ("/path/to/dir1/test2.el" "test2" "FAILED: Some error")
            ("/path/to/dir2/test3.el" "test3" "PASSED")
            ("/path/to/dir2/test4.el" "test4" "SKIPPED: Skipped")))
         (timeout 10)
         (total-time 0.5)
         (timestamp "20250510-222600")
         (buffer (get-buffer-create "*test-report-buffer*"))
         (elisp-test-results-org-path nil)
         (elisp-test-results-org-path-dired "dir-report.org")
         (elisp-test-generate-per-directory-reports t)
         (dir-reports-generated nil))
    
    (unwind-protect
        (progn
          ;; Mock directory report generation
          (cl-letf (((symbol-function 'expand-file-name)
                     (lambda (name &optional dir)
                       (concat (or dir "") "/" name)))
                    ((symbol-function 'elisp-test--report-results)
                     (lambda (buf results timeout time ts)
                       ;; Count report generation calls
                       (when (eq buf buffer)
                         ;; This is the original call - just record it happened
                         (setq dir-reports-generated (cons :main dir-reports-generated)))
                       ;; Record directory-specific reports
                       (unless (eq buf buffer)
                         (setq dir-reports-generated 
                               (cons (list :dir-report (length results)) dir-reports-generated))))))
            
            ;; Generate original report with per-directory enabled
            (elisp-test--generate-per-directory-reports test-results timeout total-time timestamp)
            
            ;; Check results - we should have 3 reports (main + 2 dirs)
            (should (= 2 (length dir-reports-generated)))
            
            ;; Check the directory reports were split correctly
            (let ((dir-report-counts (mapcar (lambda (r) 
                                             (when (eq (car r) :dir-report)
                                               (cadr r)))
                                           dir-reports-generated)))
              ;; Remove nil values
              (setq dir-report-counts (delq nil dir-report-counts))
              ;; Should have one report with 2 tests per directory
              (should (equal '(2 2) dir-report-counts)))))
      
      ;; Cleanup
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest test-elisp-test-determine-report-path ()
  "Test determining the report path based on context."
  ;; Test with default settings (no customization)
  (let ((elisp-test-results-org-path nil)
        (elisp-test-results-org-path-dired nil)
        (elisp-test-report-base-name "TEST-REPORT")
        (default-directory "/tmp/test/"))
    
    ;; Test in normal mode (not dired)
    (should (string= "/tmp/test/TEST-REPORT.org" 
                     (elisp-test--determine-report-path)))
    
    ;; Test in dired mode
    (with-temp-buffer
      (setq major-mode 'dired-mode)
      (should (string= "/tmp/test/TEST-REPORT.org" 
                       (elisp-test--determine-report-path)))))
  
  ;; Test with custom paths
  (let ((elisp-test-results-org-path "/custom/path/report.org")
        (elisp-test-results-org-path-dired "dired-report.org")
        (default-directory "/tmp/test/"))
    
    ;; Test in normal mode with custom path
    (should (string= "/custom/path/report.org" 
                     (elisp-test--determine-report-path)))
    
    ;; Test in dired mode with custom dired path
    (with-temp-buffer
      (setq major-mode 'dired-mode)
      (should (string= "/tmp/test/dired-report.org" 
                       (elisp-test--determine-report-path))))))

(provide 'test-elisp-test-reporting)

;;; test-elisp-test-reporting.el ends here