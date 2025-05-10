;;; test-elisp-test-shell-run.el --- Test running tests from shell -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-10 22:05:00>

;;; Commentary:

;; Tests for running elisp-test from shell scripts

;;; Code:

(require 'ert)
(require 'elisp-test)

;; Helper for setting up a temporary test directory
(defun setup-shell-test-env ()
  "Set up a test environment with test files for shell tests."
  (let* ((test-dir (make-temp-file "elisp-test-shell-" t))
         ;; Create test files
         (test-file1 (expand-file-name "test-sample1.el" test-dir))
         (test-file2 (expand-file-name "test-sample2.el" test-dir)))
    
    ;; Create file content
    (with-temp-file test-file1
      (insert "(require 'ert)
(ert-deftest shell-test-sample1 () 
  \"A passing test in sample1.\"
  (should t))
(provide 'test-sample1)"))
    
    (with-temp-file test-file2
      (insert "(require 'ert)
(ert-deftest shell-test-sample2-pass () 
  \"A passing test in sample2.\"
  (should t))
(ert-deftest shell-test-sample2-fail () 
  \"A failing test in sample2.\"
  (should nil))
(provide 'test-sample2)"))
    
    ;; Return the setup information
    (list :dir test-dir
          :test-file1 test-file1
          :test-file2 test-file2)))

(defun cleanup-shell-test-env (env)
  "Clean up the shell test environment ENV."
  (let ((dir (plist-get env :dir)))
    (when (and dir (file-exists-p dir))
      (delete-directory dir t))))

(ert-deftest test-elisp-test-run-from-args-single-file ()
  "Test the function for running tests from command line arguments with a single file."
  (let* ((env (setup-shell-test-env))
         (test-file1 (plist-get env :test-file1))
         (command-line-args-left (list "--test-file" test-file1 "--timeout" "5"))
         (exit-called nil)
         (exit-code nil)
         (result nil))
    
    (unwind-protect
        (progn
          ;; Mock functions to avoid actual exits and capture results
          (cl-letf (((symbol-function 'kill-emacs)
                     (lambda (code)
                       (setq exit-called t)
                       (setq exit-code code)))
                    ((symbol-function 'elisp-test-run)
                     (lambda (paths timeout no-confirm)
                       (setq result (list :paths paths :timeout timeout :no-confirm no-confirm))
                       nil)))
            
            ;; Run the command-line handler
            (elisp-test--run-from-args)
            
            ;; Check results
            (should exit-called)
            (should (= 0 exit-code)) ;; Success by default since we mocked elisp-test-run
            (should result)
            (should (equal (list test-file1) (plist-get result :paths)))
            (should (= 5 (plist-get result :timeout)))
            (should (plist-get result :no-confirm))))
      
      (cleanup-shell-test-env env))))

(ert-deftest test-elisp-test-run-from-args-directory ()
  "Test the function for running tests from command line arguments with a directory."
  (let* ((env (setup-shell-test-env))
         (test-dir (plist-get env :dir))
         (command-line-args-left (list "--test-dir" test-dir))
         (exit-called nil)
         (exit-code nil)
         (result nil))
    
    (unwind-protect
        (progn
          ;; Mock functions to avoid actual exits and capture results
          (cl-letf (((symbol-function 'kill-emacs)
                     (lambda (code)
                       (setq exit-called t)
                       (setq exit-code code)))
                    ((symbol-function 'elisp-test-run)
                     (lambda (paths timeout no-confirm)
                       (setq result (list :paths paths :timeout timeout :no-confirm no-confirm))
                       nil)))
            
            ;; Run the command-line handler
            (elisp-test--run-from-args)
            
            ;; Check results
            (should exit-called)
            (should (= 0 exit-code)) ;; Success by default since we mocked elisp-test-run
            (should result)
            (should (equal (list test-dir) (plist-get result :paths)))
            (should (plist-get result :no-confirm))))
      
      (cleanup-shell-test-env env))))

(ert-deftest test-elisp-test-run-from-args-pattern ()
  "Test the function for running tests from command line arguments with a pattern."
  (let* ((env (setup-shell-test-env))
         (test-dir (plist-get env :dir))
         (command-line-args-left (list "--pattern" "sample" "--test-dir" test-dir))
         (exit-called nil)
         (files-found nil)
         (result nil))
    
    (unwind-protect
        (progn
          ;; Mock functions to avoid actual exits and capture results
          (cl-letf (((symbol-function 'kill-emacs)
                     (lambda (code)
                       (setq exit-called t)))
                    ((symbol-function 'directory-files-recursively)
                     (lambda (dir pattern &optional include-dirs)
                       (setq files-found (list :dir dir :pattern pattern))
                       (list (plist-get env :test-file1) (plist-get env :test-file2))))
                    ((symbol-function 'elisp-test-run)
                     (lambda (paths timeout no-confirm)
                       (setq result (list :paths paths :timeout timeout :no-confirm no-confirm))
                       nil)))
            
            ;; Run the command-line handler
            (elisp-test--run-from-args)
            
            ;; Check results
            (should exit-called)
            (should files-found)
            (should (equal test-dir (plist-get files-found :dir)))
            (should (string-match-p "test-.*sample.*\\.el" (plist-get files-found :pattern)))
            (should result)
            (should (= 2 (length (plist-get result :paths))))
            (should (plist-get result :no-confirm))))
      
      (cleanup-shell-test-env env))))

(ert-deftest test-elisp-test-determine-exit-code ()
  "Test determining exit code based on test results."
  ;; Test with all passing tests
  (let ((all-passing-results 
         '(("/path/to/test1.el" "test1" "PASSED")
           ("/path/to/test2.el" "test2" "PASSED"))))
    (should (= 0 (elisp-test--determine-exit-code all-passing-results))))
  
  ;; Test with a failing test
  (let ((with-failing-results 
         '(("/path/to/test1.el" "test1" "PASSED")
           ("/path/to/test2.el" "test2" "FAILED: some error"))))
    (should (= 1 (elisp-test--determine-exit-code with-failing-results))))
  
  ;; Test with an error
  (let ((with-error-results 
         '(("/path/to/test1.el" "test1" "PASSED")
           ("/path/to/test2.el" "test2" "ERROR: some error"))))
    (should (= 1 (elisp-test--determine-exit-code with-error-results))))
  
  ;; Test with a timeout
  (let ((with-timeout-results 
         '(("/path/to/test1.el" "test1" "PASSED")
           ("/path/to/test2.el" "test2" "TIMEOUT: Test exceeded time limit of 10"))))
    (should (= 1 (elisp-test--determine-exit-code with-timeout-results)))))

(ert-deftest test-elisp-test-parse-test-output ()
  "Test the function for parsing test output."
  ;; We need to mock the ert output buffer
  (let ((temp-buffer (get-buffer-create "*ert*")))
    (unwind-protect
        (progn
          ;; Test passing case
          (with-current-buffer temp-buffer
            (erase-buffer)
            (insert "Ran 1 tests, 0 failed, 0 skipped, completed in 0.123 sec.
 
Failed:     0
Skipped:    0
Passed:     1
Ran:        1")
            (should (string= "PASSED" (elisp-test--parse-test-output))))
          
          ;; Test failing case
          (with-current-buffer temp-buffer
            (erase-buffer)
            (insert "Ran 2 tests, 1 failed, 0 skipped, completed in 0.123 sec.
 
Failed:     1
Skipped:    0
Passed:     1
Ran:        2")
            (should (string-match-p "FAILED:" (elisp-test--parse-test-output))))
          
          ;; Test skipped case
          (with-current-buffer temp-buffer
            (erase-buffer)
            (insert "Ran 2 tests, 0 failed, 1 skipped, completed in 0.123 sec.
 
Failed:     0
Skipped:    1
Passed:     1
Ran:        2")
            (should (string-match-p "SKIPPED:" (elisp-test--parse-test-output)))))
      
      ;; Cleanup
      (kill-buffer temp-buffer))))

(provide 'test-elisp-test-shell-run)

;;; test-elisp-test-shell-run.el ends here