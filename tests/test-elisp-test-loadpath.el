;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 07:22:47>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/elisp-test-loadpath.el

(require 'ert)
(require 'elisp-test-loadpath)

(ert-deftest test-elisp-test-add-load-paths-adds-to-elisp-test-loadpath
    ()
  (unwind-protect
      (let
          ((original-elisp-test-loadpath
            (copy-sequence elisp-test-loadpath))
           (test-path "/tmp/test/path"))
        (elisp-test-add-load-paths
         (list test-path))
        (should
         (member
          (expand-file-name test-path)
          elisp-test-loadpath))
        (setq elisp-test-loadpath original-elisp-test-loadpath))))

(ert-deftest test-elisp-test-add-load-paths-adds-to-load-path
    ()
  (unwind-protect
      (let
          ((original-load-path
            (copy-sequence load-path))
           (test-path "/tmp/test/path"))
        (elisp-test-add-load-paths
         (list test-path))
        (should
         (member
          (expand-file-name test-path)
          load-path))
        (setq load-path original-load-path))))

(ert-deftest test-elisp-test-add-load-paths-handles-multiple-paths
    ()
  (unwind-protect
      (let
          ((original-elisp-test-loadpath
            (copy-sequence elisp-test-loadpath))
           (original-load-path
            (copy-sequence load-path))
           (test-paths
            '("/tmp/test/path1" "/tmp/test/path2")))
        (elisp-test-add-load-paths test-paths)
        (should
         (member
          (expand-file-name
           (car test-paths))
          elisp-test-loadpath))
        (should
         (member
          (expand-file-name
           (car test-paths))
          load-path))
        (setq elisp-test-loadpath original-elisp-test-loadpath
              load-path original-load-path))))

(provide 'test-elisp-test-loadpath)

(provide 'elisp-test-loadpath)

(when
    (not load-file-name)
  (message "elisp-test-loadpath.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))