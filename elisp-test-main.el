;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-03-05 07:29:00>
;;; File: /home/ywatanabe/.emacs.d/lisp/emacs-test/elisp-test-main.el

;; Contains the main function to run tests.

(defun et-test
    (&optional root-paths timeout-per-test)
  "Run tests from specified ROOT-PATHS, marked files in dired, or current directory.
When run in a buffer with a file, only run tests from that file."
  (interactive)
  ;; Saving path switcher based on how and where to be called
  (if
      (eq major-mode 'dired-mode)
      ;; In dired mode - use the current directory for report
      (setq et-results-org-path-switched
            (expand-file-name et-results-org-path-dired default-directory))
    ;; Otherwise use the default
    (setq et-results-org-path-switched et-results-org-path))

  ;; Check if called in a buffer with a file
  (if buffer-file-name
      (et--run-buffer buffer-file-name)
    ;; Define test paths and create test-alist (test name and file path)
    (let*
        ((paths-defined-by-a-method
          (cond
           ((eq major-mode 'dired-mode)
            (--et-find-list-marked-paths-dired))
           (root-paths
            (et-find-test-files-multiple root-paths))
           (t
            (list default-directory))))
         (test-alist
          (et--prepare-test-plan paths-defined-by-a-method)))

      ;; Confirmation with displaying detected tests
      (when
          (and test-alist
               (yes-or-no-p
                (format "Proceed with running these %s tests? "
                        (length test-alist))))

        ;; Run tests
        (let*
            ((timestamp
              (format-time-string "%Y%m%d-%H%M%S"))
             (timeout-per-test-confirmed
              (or timeout-per-test
                  (read-number "Timeout [s]: " et-timeout-sec)))
             (start-time
              (current-time))
             (test-results
              (et--run-multiple-tests test-alist timeout-per-test-confirmed)))

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
                    ((et-results-org-path-switched
                      (expand-file-name et-results-org-path-dired dir))
                     ;; Filter test results to only those in this directory
                     (filtered-results
                      (seq-filter
                       (lambda
                         (result)
                         (string-prefix-p dir
                                          (car result)))
                       test-results)))

                  ;; Generate report for this directory with shared timestamp
                  (et--report-results
                   (get-buffer-create "*elisp-test-results*")
                   filtered-results
                   timeout-per-test-confirmed
                   total-time-spent
                   timestamp))))

            ;; Also generate a full report at the original location
            (et--report-results
             (get-buffer-create "*elisp-test-results*")
             test-results
             timeout-per-test-confirmed
             total-time-spent
             timestamp))))))
  (kill-buffer "*ert*"))

;; ;; Key Binding
;; (global-set-key (kbd "C-c C-t") #'et-test)

(provide 'elisp-test-main)

(when
    (not load-file-name)
  (message "elisp-test-main.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))