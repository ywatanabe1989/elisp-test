;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-05-06 01:50:52>
;;; File: /home/ywatanabe/.emacs.d/lisp/elisp-test/elisp-test-main.el

;;; Copyright (C) 2025 Yusuke Watanabe (ywatanabe@alumni.u-tokyo.ac.jp)

;;;###autoload
(defun elisp-test-run
    (&optional root-paths timeout-per-test no-confirm)
  "Run tests from specified ROOT-PATHS, marked files in dired, or current directory.
When run in a buffer with a file, only run tests from that file.
With NO-CONFIRM non-nil, skip confirmation prompt."
  (interactive)

  ;; Saving path switcher based on how and where to be called
  (if
      (eq major-mode 'dired-mode)
      ;; In dired mode - use the current directory for report
      (setq elisp-test-results-org-path-switched
            (expand-file-name elisp-test-results-org-path-dired
                              default-directory))
    ;; Otherwise use the default
    (setq elisp-test-results-org-path-switched
          elisp-test-results-org-path))
  ;; Check if called in a buffer with a file
  (if buffer-file-name
      (elisp-test--run-buffer buffer-file-name)
    ;; Define test paths and create test-alist (test name and file path)
    (let*
        ((paths-defined-by-a-method
          (cond
           ((eq major-mode 'dired-mode)
            (--elisp-test-find-list-marked-paths-dired))
           (root-paths
            (elisp-test-find-test-files-multiple root-paths))
           (t
            (list default-directory))))
         (test-alist
          (elisp-test--prepare-test-plan paths-defined-by-a-method)))
      ;; Confirmation with displaying detected tests
      (when
          (and test-alist
               (or no-confirm
                   (yes-or-no-p
                    (format "Proceed with running these %s tests? "
                            (length test-alist)))))
        ;; Run tests
        (let*
            ((timestamp
              (format-time-string "%Y%m%d-%H%M%S"))
             (timeout-per-test-confirmed
              (or timeout-per-test
                  (if no-confirm
                      elisp-test-timeout-sec
                    (read-number "Timeout [s]: " elisp-test-timeout-sec))))
             (start-time
              (current-time))
             (test-results
              (elisp-test--run-multiple-tests test-alist
                                              timeout-per-test-confirmed)))
          (let
              ((total-time-spent
                (float-time
                 (time-subtract
                  (current-time)
                  start-time))))
            ;; Generate a report for each directory involved
            (let
                ((directories
                  (seq-uniq
                   (mapcar
                    (lambda
                      (result)
                      (file-name-directory
                       (car result)))
                    test-results))))
              ;; For each directory, create a report with only its tests
              (dolist
                  (dir directories)
                ;; Set the report path to be in this directory
                (let
                    ((elisp-test-results-org-path-switched
                      (expand-file-name
                       elisp-test-results-org-path-dired dir))
                     ;; Filter test results to only those in this directory
                     (filtered-results
                      (seq-filter
                       (lambda
                         (result)
                         (string-prefix-p dir
                                          (car result)))
                       test-results)))
                  ;; Generate report for this directory with shared timestamp
                  (elisp-test--report-results
                   (elisp-test-buffer-create "*elisp-test-results*")
                   filtered-results
                   timeout-per-test-confirmed
                   total-time-spent
                   timestamp))))
            ;; Also generate a full report at the original location
            (elisp-test--report-results
             (elisp-test-buffer-create "*elisp-test-results*")
             test-results
             timeout-per-test-confirmed
             total-time-spent
             timestamp))))))
  (when (elisp-test-buffer "*ert*")
    (kill-buffer "*ert*")))

;; ;;;###autoload
;; (defun elisp-test-run
;;     (&optional root-paths timeout-per-test)
;;   "Run tests from specified ROOT-PATHS, marked files in dired, or current directory.
;; When run in a buffer with a file, only run tests from that file."
;;   (interactive)
;;   ;; Saving path switcher based on how and where to be called
;;   (if
;;       (eq major-mode 'dired-mode)
;;       ;; In dired mode - use the current directory for report
;;       (setq elisp-test-results-org-path-switched
;;             (expand-file-name elisp-test-results-org-path-dired
;;                               default-directory))
;;     ;; Otherwise use the default
;;     (setq elisp-test-results-org-path-switched
;;           elisp-test-results-org-path))

;;   ;; Check if called in a buffer with a file
;;   (if buffer-file-name
;;       (elisp-test--run-buffer buffer-file-name)
;;     ;; Define test paths and create test-alist (test name and file path)
;;     (let*
;;         ((paths-defined-by-a-method
;;           (cond
;;            ((eq major-mode 'dired-mode)
;;             (--elisp-test-find-list-marked-paths-dired))
;;            (root-paths
;;             (elisp-test-find-test-files-multiple root-paths))
;;            (t
;;             (list default-directory))))
;;          (test-alist
;;           (elisp-test--prepare-test-plan paths-defined-by-a-method)))

;;       ;; Confirmation with displaying detected tests
;;       (when
;;           (and test-alist
;;                (yes-or-no-p
;;                 (format "Proceed with running these %s tests? "
;;                         (length test-alist))))

;;         ;; Run tests
;;         (let*
;;             ((timestamp
;;               (format-time-string "%Y%m%d-%H%M%S"))
;;              (timeout-per-test-confirmed
;;               (or timeout-per-test
;;                   (read-number "Timeout [s]: " elisp-test-timeout-sec)))
;;              (start-time
;;               (current-time))
;;              (test-results
;;               (elisp-test--run-multiple-tests test-alist
;;                                               timeout-per-test-confirmed)))

;;           (let
;;               ((total-time-spent
;;                 (float-time
;;                  (time-subtract
;;                   (current-time)
;;                   start-time))))

;;             ;; Generate a report for each directory involved
;;             (let
;;                 ((directories
;;                   (seq-uniq
;;                    (mapcar
;;                     (lambda
;;                       (result)
;;                       (file-name-directory
;;                        (car result)))
;;                     test-results))))

;;               ;; For each directory, create a report with only its tests
;;               (dolist
;;                   (dir directories)
;;                 ;; Set the report path to be in this directory
;;                 (let
;;                     ((elisp-test-results-org-path-switched
;;                       (expand-file-name
;;                        elisp-test-results-org-path-dired dir))
;;                      ;; Filter test results to only those in this directory
;;                      (filtered-results
;;                       (seq-filter
;;                        (lambda
;;                          (result)
;;                          (string-prefix-p dir
;;                                           (car result)))
;;                        test-results)))

;;                   ;; Generate report for this directory with shared timestamp
;;                   (elisp-test--report-results
;;                    (elisp-test-buffer-create "*elisp-test-results*")
;;                    filtered-results
;;                    timeout-per-test-confirmed
;;                    total-time-spent
;;                    timestamp))))

;;             ;; Also generate a full report at the original location
;;             (elisp-test--report-results
;;              (elisp-test-buffer-create "*elisp-test-results*")
;;              test-results
;;              timeout-per-test-confirmed
;;              total-time-spent
;;              timestamp))))))

;;   (when (elisp-test-buffer "*ert*")
;;     (kill-buffer "*ert*")))

;; ;; Key Binding
;; (global-set-test-key (kbd "C-c C-t") #'elisp-test-run)


(provide 'elisp-test-main)

(when
    (not load-file-name)
  (message "elisp-test-main.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))
