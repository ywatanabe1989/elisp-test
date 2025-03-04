;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 07:22:47>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/elisp-test-loadpath.el

(require 'ert)
(require 'elisp-test-loadpath)

(ert-deftest test-et-add-load-paths-adds-to-et-loadpath
    ()
  (unwind-protect
      (let
          ((original-et-loadpath
            (copy-sequence et-loadpath))
           (test-path "/tmp/test/path"))
        (et-add-load-paths
         (list test-path))
        (should
         (member
          (expand-file-name test-path)
          et-loadpath))
        (setq et-loadpath original-et-loadpath))))

(ert-deftest test-et-add-load-paths-adds-to-load-path
    ()
  (unwind-protect
      (let
          ((original-load-path
            (copy-sequence load-path))
           (test-path "/tmp/test/path"))
        (et-add-load-paths
         (list test-path))
        (should
         (member
          (expand-file-name test-path)
          load-path))
        (setq load-path original-load-path))))

(ert-deftest test-et-add-load-paths-handles-multiple-paths
    ()
  (unwind-protect
      (let
          ((original-et-loadpath
            (copy-sequence et-loadpath))
           (original-load-path
            (copy-sequence load-path))
           (test-paths
            '("/tmp/test/path1" "/tmp/test/path2")))
        (et-add-load-paths test-paths)
        (should
         (member
          (expand-file-name
           (car test-paths))
          et-loadpath))
        (should
         (member
          (expand-file-name
           (car test-paths))
          load-path))
        (setq et-loadpath original-et-loadpath
              load-path original-load-path))))

(provide 'test-elisp-test-loadpath)

(provide 'elisp-test-loadpath)

(when
    (not load-file-name)
  (message "elisp-test-loadpath.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))