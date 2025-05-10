;;; test-run-scripts-integration.el --- Integration tests for shell scripts -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-10 22:15:00>

;;; Commentary:

;; Integration tests for the shell scripts (run_tests.sh and run_tests_parallel.sh)

;;; Code:

(require 'ert)

(defun setup-script-test-env ()
  "Set up a test environment for script integration tests."
  (let* ((test-dir (make-temp-file "elisp-test-scripts-" t))
         ;; Create test files
         (test-file1 (expand-file-name "test-script1.el" test-dir))
         (test-file2 (expand-file-name "test-script2.el" test-dir))
         (script-file (expand-file-name "run_test_integration.sh" test-dir)))
    
    ;; Create file content
    (with-temp-file test-file1
      (insert "(require 'ert)
(ert-deftest script-test1 () 
  \"A passing test for script integration.\"
  (should t))
(provide 'test-script1)"))
    
    (with-temp-file test-file2
      (insert "(require 'ert)
(ert-deftest script-test2-pass () 
  \"A passing test in script test 2.\"
  (should t))
(ert-deftest script-test2-fail () 
  \"A failing test in script test 2.\"
  (should nil))
(provide 'test-script2)"))
    
    ;; Create a simple wrapper script that calls our main scripts
    (with-temp-file script-file
      (insert "#!/bin/bash
# Simple wrapper to call our test scripts
THIS_DIR=\"$(cd $(dirname ${BASH_SOURCE[0]}) && pwd)\"
ELISP_TEST_DIR=\"/home/ywatanabe/.dotfiles/.emacs.d/lisp/elisp-test\"

# Run with normal script
function run_normal() {
    $ELISP_TEST_DIR/run_tests.sh \"$@\"
    return $?
}

# Run with parallel script
function run_parallel() {
    $ELISP_TEST_DIR/run_tests_parallel.sh \"$@\"
    return $?
}

# Run the specified version
if [ \"$1\" = \"--parallel\" ]; then
    shift
    run_parallel \"$@\"
    exit $?
else
    run_normal \"$@\"
    exit $?
fi
"))
    
    ;; Make the script executable
    (shell-command (format "chmod +x %s" script-file))
    
    ;; Return the setup information
    (list :dir test-dir
          :test-file1 test-file1
          :test-file2 test-file2
          :script-file script-file)))

(defun cleanup-script-test-env (env)
  "Clean up the script test environment ENV."
  (let ((dir (plist-get env :dir)))
    (when (and dir (file-exists-p dir))
      (delete-directory dir t))))

(ert-deftest test-run-tests-script-single-file ()
  "Test running a single test file with run_tests.sh script."
  (let* ((env (setup-script-test-env))
         (test-file1 (plist-get env :test-file1))
         (script-file (plist-get env :script-file))
         (result nil)
         (status nil))
    
    (unwind-protect
        (progn
          ;; Run the script with single file
          (with-temp-buffer
            (setq status (call-process script-file nil t nil 
                                       "-s" test-file1 "--debug"))
            (setq result (buffer-string)))
          
          ;; Check result
          (should (= 0 status)) ;; Should exit with 0 (success)
          (should (string-match-p "Test passed: .*test-script1.el" result))
          (should (string-match-p "Tests completed successfully" result)))
      
      (cleanup-script-test-env env))))

(ert-deftest test-run-tests-script-directory ()
  "Test running tests in a directory with run_tests.sh script."
  (let* ((env (setup-script-test-env))
         (test-dir (plist-get env :dir))
         (script-file (plist-get env :script-file))
         (result nil)
         (status nil))
    
    (unwind-protect
        (progn
          ;; Run the script with directory
          (with-temp-buffer
            (setq status (call-process script-file nil t nil 
                                      "--debug" test-dir))
            (setq result (buffer-string)))
          
          ;; Check result - should fail because of the failing test
          (should (= 1 status)) ;; Should exit with 1 (failure)
          (should (string-match-p "Tests completed with errors" result)))
      
      (cleanup-script-test-env env))))

(ert-deftest test-run-tests-parallel-script ()
  "Test running tests with run_tests_parallel.sh script."
  (let* ((env (setup-script-test-env))
         (test-dir (plist-get env :dir))
         (script-file (plist-get env :script-file))
         (result nil)
         (status nil))
    
    (unwind-protect
        (progn
          ;; Run the parallel script with directory
          (with-temp-buffer
            (setq status (call-process script-file nil t nil 
                                      "--parallel" "--debug" test-dir))
            (setq result (buffer-string)))
          
          ;; Check result - should have run in parallel mode
          (should (string-match-p "Running tests in directory:.* using.*cores" result)))
      
      (cleanup-script-test-env env))))

(ert-deftest test-run-tests-script-with-timeout ()
  "Test running tests with a custom timeout value."
  (let* ((env (setup-script-test-env))
         (test-file1 (plist-get env :test-file1))
         (script-file (plist-get env :script-file))
         (result nil)
         (status nil))
    
    (unwind-protect
        (progn
          ;; Run the script with custom timeout
          (with-temp-buffer
            (setq status (call-process script-file nil t nil 
                                      "-s" test-file1 "--timeout" "20" "--debug"))
            (setq result (buffer-string)))
          
          ;; Check result - should include the timeout value in the command
          (should (= 0 status))
          (should (string-match-p "elisp-test-run.*20" result)))
      
      (cleanup-script-test-env env))))

(provide 'test-run-scripts-integration)

;;; test-run-scripts-integration.el ends here