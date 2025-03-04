;;; -*- coding: utf-8; lexical-binding: t -*-
;;; Author: ywatanabe
;;; Timestamp: <2025-02-25 02:50:02>
;;; File: /home/ywatanabe/.dotfiles/.emacs.d/lisp/emacs-test/tests/test-elisp-test-path.el

(require 'ert)
(require 'elisp-test-loadpath)

(ert-deftest test-et-add-load-paths-single
    ()
  (let
      ((original-loadpath
        (copy-sequence et-loadpath))
       (original-load-path
        (copy-sequence load-path))
       (test-path "/tmp/test-path"))
    (unwind-protect
        (progn
          (et-add-load-paths
           (list test-path))
          (should
           (member
            (expand-file-name test-path)
            et-loadpath))
          (should
           (member
            (expand-file-name test-path)
            load-path)))
      (setq et-loadpath original-loadpath
            load-path original-load-path))))

(ert-deftest test-et-add-load-paths-multiple
    ()
  (let
      ((original-loadpath
        (copy-sequence et-loadpath))
       (original-load-path
        (copy-sequence load-path))
       (test-paths
        '("/tmp/test-path1" "/tmp/test-path2")))
    (unwind-protect
        (progn
          (et-add-load-paths test-paths)
          (dolist
              (path test-paths)
            (should
             (member
              (expand-file-name path)
              et-loadpath))
            (should
             (member
              (expand-file-name path)
              load-path))))
      (setq et-loadpath original-loadpath
            load-path original-load-path))))

(provide 'test-elisp-test-path)

(when
    (not load-file-name)
  (message "test-elisp-test-path.el loaded."
           (file-name-nondirectory
            (or load-file-name buffer-file-name))))