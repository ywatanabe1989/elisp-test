;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-02 21:57:10>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/test-elisp-test-summarize.el

(require 'ert)
(require 'elisp-test-report)
(require 'elisp-test-variables)

(ert-deftest test-elisp-test--report-results-saves-file
    ()
  (let
      ((elisp-test-results-org-path-switched
        (make-temp-file "elisp-test-run-"))
       (temp-buffer
        (generate-new-buffer "*test*"))
       (test-results
        '(("1" "test1" "PASSED")
          ("2" "test2" "FAILED: reason"))))
    (unwind-protect
        (progn
          (elisp-test--report-results temp-buffer test-results)
          (should
           (file-exists-p elisp-test-results-org-path-switched)))
      (kill-buffer temp-buffer)
      (when
          (file-exists-p elisp-test-results-org-path-switched)
        (delete-file elisp-test-results-org-path-switched)))))

(provide 'test-elisp-test-summarize)

(when
    (not load-file-name)
  (message "test-elisp-test-summarize.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))