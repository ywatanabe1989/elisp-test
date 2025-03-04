;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-05 07:33:03>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-test/tests/test-elisp-test-main.el

;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-05 07:31:31>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-test/tests/test-elisp-test-main.el
(require 'ert)
(require 'elisp-test-main)

(ert-deftest test-et-test-function
    ()
  "Test the `et-test` function."
  ;; Since `et-test` is interactive and prompts the user, we'll need to simulate
  ;; user input. We can use `cl-letf` to override `yes-or-no-p`.
  (let
      ((results-org-path-orig
        et-results-org-path-switched)
       (test-dir
        (make-temp-file "et-test-dir" t))
       (test-file
        (make-temp-file "et-test-file" nil ".el"))
       (test-code "(ert-deftest test-sample-test () (should t))")
       (timeout-per-test 10)
       ;; Save current kill-buffer function to restore later
       (original-kill-buffer
        (symbol-function 'kill-buffer)))
    (setq et-results-org-path-switched et-results-org-path)
    (unwind-protect
        (progn
          ;; Write test code to temp file
          (with-temp-file test-file
            (insert test-code))
          ;; ;; Move test file to test directory
          ;; (rename-file test-file
          ;;              (expand-file-name
          ;;               (file-name-nondirectory test-file)
          ;;               test-dir))

          ;; Temporarily modify kill-buffer to skip killing *ert* during testing
          (cl-letf
              (((symbol-function 'kill-buffer)
                (lambda
                  (buffer-or-name)
                  (unless
                      (string=
                       (if
                           (bufferp buffer-or-name)
                           (buffer-name buffer-or-name)
                         buffer-or-name)
                       "*ert*")
                    (funcall original-kill-buffer buffer-or-name))))
               ((symbol-function 'yes-or-no-p)
                (lambda
                  (&rest args)
                  t)))
            (et-test test-dir timeout-per-test)))

      ;; Cleanup
      (setq et-results-org-path-switched results-org-path-orig)
      (delete-directory test-dir t))))

(provide 'test-elisp-test-main)
(when
    (not load-file-name)
  (message "test-elisp-test-main.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))

;; ;;; -*- coding: utf-8; lexical-binding: t -*-
;; ;;; Author: ywatanabe
;; ;;; Timestamp: <2025-03-05 07:31:31>
;; ;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-test/tests/test-elisp-test-main.el

;; (require 'ert)
;; (require 'elisp-test-main)

;; (ert-deftest test-et-test-function
;;     ()
;;   "Test the `et-test` function."
;;   ;; Since `et-test` is interactive and prompts the user, we'll need to simulate
;;   ;; user input. We can use `cl-letf` to override `yes-or-no-p`.

;;   (let
;;       ((results-org-path-orig
;;         et-results-org-path-switched)
;;        (test-dir
;;         (make-temp-file "et-test-dir" t))
;;        (test-file
;;         (make-temp-file "et-test-file" nil ".el"))
;;        (test-code "(ert-deftest test-sample-test () (should t))")
;;        (timeout-per-test 10))

;;     (setq et-results-org-path-switched et-results-org-path)

;;     (unwind-protect
;;         (progn
;;           ;; Write test code to temp file
;;           (with-temp-file test-file
;;             (insert test-code))

;;           ;; ;; Move test file to test directory
;;           ;; (rename-file test-file
;;           ;;              (expand-file-name
;;           ;;               (file-name-nondirectory test-file)
;;           ;;               test-dir))

;;           (cl-letf
;;               (((symbol-function 'yes-or-no-p)
;;                 (lambda
;;                   (&rest args)
;;                   t)))
;;             (et-test
;;              test-dir
;;              timeout-per-test)))

;;       ;; Cleanup
;;       (setq et-results-org-path-switched results-org-path-orig)
;;       (delete-directory test-dir t))))

;; (provide 'test-elisp-test-main)

;; (when
;;     (not load-file-name)
;;   (message "test-elisp-test-main.el loaded."
;;            (file-name-nondirectory
;;             (or load-file-name buffer-file-name))))

(provide 'test-elisp-test-main)

(when
    (not load-file-name)
  (message "test-elisp-test-main.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))