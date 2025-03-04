;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-03 04:55:53>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/elisp-test-buffer.el

(defun et--run-buffer
    (&optional file-path buffer-name)
  "Run all tests in FILE-PATH and store results in BUFFER-NAME."
  (interactive)
  (let*
      ((current-file
        (if file-path
            (expand-file-name file-path)
          (buffer-file-name)))
       (test-buffer
        (find-file-noselect current-file))
       (buffer
        (get-buffer-create
         (or buffer-name "*elisp-test-results*"))))
    (with-current-buffer test-buffer
      (eval-buffer)
      (save-excursion
        (goto-char
         (point-min))
        (let
            ((tests
              '())
             (results
              '())
             (total-passed 0)
             (total-failed 0)
             (total-aborted 0))
          (while
              (re-search-forward "^(ert-deftest\\s-+\\(\\sw\\(?:\\sw\\|-\\)*\\)" nil t)
            (push
             (intern
              (match-string-no-properties 1))
             tests))
          (when tests
            (with-current-buffer buffer
              (let
                  ((inhibit-read-only t))
                (erase-buffer)
                (insert
                 (format "File: %s\n\n" current-file))
                (setq tests
                      (nreverse tests))
                (dolist
                    (test tests)
                  (condition-case err
                      (let*
                          ((test-obj
                            (ert-get-test test))
                           (result
                            (ert-run-test test-obj))
                           (status
                            (if
                                (ert-test-passed-p result)
                                (progn
                                  (cl-incf total-passed)
                                  "PASSED")
                              (progn
                                (cl-incf total-failed)
                                "FAILED"))))
                        (push
                         (cons test
                               (cons status result))
                         results)
                        (insert
                         (format "Test: %s\nStatus: %s\n" test status))
                        (when
                            (ert-test-failed-p result)
                          (insert
                           (format "Error: %S\n"
                                   (ert-test-result-with-condition-condition result))))
                        (insert "\n"))
                    (error
                     (cl-incf total-aborted)
                     (push
                      (cons test
                            (cons "ABORTED" err))
                      results)
                     (insert
                      (format "Test: %s\nStatus: ABORTED\nError: %S\n\n"
                              test err)))))
                ;; Add summary section with statistics
                (goto-char
                 (point-min))
                (let
                    ((total
                      (+ total-passed total-failed total-aborted))
                     (success-rate
                      (if
                          (>
                           (+ total-passed total-failed total-aborted)
                           0)
                          (* 100.0
                             (/
                              (float total-passed)
                              (+ total-passed total-failed total-aborted)))
                        0.0)))
                  (insert "* Test Results Summary\n\n")
                  (insert
                   (format "- Total Tests: %d\n" total))
                  (insert
                   (format "- Passed: %d\n" total-passed))
                  (insert
                   (format "- Failed: %d\n" total-failed))
                  (insert
                   (format "- Aborted: %d\n" total-aborted))
                  (insert
                   (format "- Success Rate: %.1f%%\n\n" success-rate))
                  (insert "* Test Details\n\n"))
                ;; Add syntax highlighting
                (font-lock-mode 1)
                (font-lock-ensure)
                ;; Make test status more visible
                (goto-char
                 (point-min))
                (while
                    (re-search-forward "Status: \\(PASSED\\|FAILED\\|ABORTED\\)" nil t)
                  (let
                      ((status
                        (match-string 1)))
                    (put-text-property
                     (match-beginning 1)
                     (match-end 1)
                     'face
                     (cond
                      ((string= status "PASSED")
                       '(:foreground "green" :weight bold))
                      ((string= status "FAILED")
                       '(:foreground "red" :weight bold))
                      ((string= status "ABORTED")
                       '(:foreground "orange" :weight bold)))))))))))
      (pop-to-buffer buffer)
      (pop-to-buffer test-buffer)
      buffer)))

(provide 'elisp-test-buffer)

(when
    (not load-file-name)
  (message "elisp-test-buffer.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))