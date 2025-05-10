;;; test-elisp-test-buffer-run.el --- Test running tests from buffer -*- lexical-binding: t -*-

;; Author: ywatanabe
;; Timestamp: <2025-05-10 21:45:00>

;;; Commentary:

;; Tests for running elisp-test from a buffer context

;;; Code:

(require 'ert)
(require 'elisp-test)

;; Test helper to simulate a test buffer
(defun setup-test-buffer (content)
  "Set up a test buffer with CONTENT."
  (let ((buffer (generate-new-buffer "*test-buffer*")))
    (with-current-buffer buffer
      (insert content)
      (emacs-lisp-mode)
      (set-visited-file-name (make-temp-file "test-" nil ".el"))
      (save-buffer))
    buffer))

(defun cleanup-test-buffer (buffer)
  "Clean up test BUFFER and its associated file."
  (when (buffer-live-p buffer)
    (let ((filename (buffer-file-name buffer)))
      (kill-buffer buffer)
      (when (and filename (file-exists-p filename))
        (delete-file filename)))))

(ert-deftest test-elisp-test-run-buffer ()
  "Test running tests from a buffer."
  (let* ((test-content "(require 'ert)
(ert-deftest sample-passing-test () 
  \"A simple passing test for testing the buffer runner.\"
  (should t))
(provide 'sample-test)")
         (buffer (setup-test-buffer test-content))
         (elisp-test-results-org-path nil) ;; Prevent writing output files
         (elisp-test-generate-per-directory-reports nil)
         (result))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Mock elisp-test--report-results to capture the results
            (cl-letf (((symbol-function 'elisp-test--report-results)
                       (lambda (buf test-results timeout time timestamp)
                         (setq result test-results)
                         buf)))
              ;; Run the test from buffer
              (call-interactively 'elisp-test-run)
              ;; Check results
              (should result)
              (should (= 1 (length result)))
              (should (string= "PASSED" (nth 2 (car result)))))))
      (cleanup-test-buffer buffer))))

(ert-deftest test-elisp-test-run-buffer-with-failing-test ()
  "Test running a failing test from a buffer."
  (let* ((test-content "(require 'ert)
(ert-deftest sample-failing-test () 
  \"A simple failing test for testing the buffer runner.\"
  (should nil))
(provide 'sample-test)")
         (buffer (setup-test-buffer test-content))
         (elisp-test-results-org-path nil) ;; Prevent writing output files
         (elisp-test-generate-per-directory-reports nil)
         (result))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Mock elisp-test--report-results to capture the results
            (cl-letf (((symbol-function 'elisp-test--report-results)
                       (lambda (buf test-results timeout time timestamp)
                         (setq result test-results)
                         buf)))
              ;; Run the test from buffer
              (call-interactively 'elisp-test-run)
              ;; Check results
              (should result)
              (should (= 1 (length result)))
              (should (string-match-p "FAILED:" (nth 2 (car result)))))))
      (cleanup-test-buffer buffer))))

(ert-deftest test-elisp-test-run-buffer-with-multiple-tests ()
  "Test running multiple tests from a buffer."
  (let* ((test-content "(require 'ert)
(ert-deftest sample-passing-test-1 () 
  \"First passing test.\"
  (should t))
(ert-deftest sample-passing-test-2 () 
  \"Second passing test.\"
  (should (= 2 (+ 1 1))))
(ert-deftest sample-failing-test () 
  \"A failing test.\"
  (should nil))
(provide 'sample-test)")
         (buffer (setup-test-buffer test-content))
         (elisp-test-results-org-path nil) ;; Prevent writing output files
         (elisp-test-generate-per-directory-reports nil)
         (result))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Mock elisp-test--report-results to capture the results
            (cl-letf (((symbol-function 'elisp-test--report-results)
                       (lambda (buf test-results timeout time timestamp)
                         (setq result test-results)
                         buf)))
              ;; Run the test from buffer
              (call-interactively 'elisp-test-run)
              ;; Check results
              (should result)
              (should (= 3 (length result)))
              (let* ((test-names (mapcar (lambda (r) (nth 1 r)) result))
                     (pass-count (length (seq-filter (lambda (r) (string= "PASSED" (nth 2 r))) result)))
                     (fail-count (length (seq-filter (lambda (r) (string-match-p "FAILED:" (nth 2 r))) result))))
                (should (= 2 pass-count))
                (should (= 1 fail-count))
                (should (member "sample-passing-test-1" test-names))
                (should (member "sample-passing-test-2" test-names))
                (should (member "sample-failing-test" test-names))))))
      (cleanup-test-buffer buffer))))

(provide 'test-elisp-test-buffer-run)

;;; test-elisp-test-buffer-run.el ends here