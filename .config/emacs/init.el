;;; init.el --- My configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Demis Balbach
;;
;; Author: Demis Balbach <db@minikn.xyz>
;; Maintainer: Demis Balbach <db@minikn.xyz>
;; Created: Februar 09, 2021
;; Modified: Februar 09, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/minikN/dotArch
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

;; Measuring startup time.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(defun db/edit ()
  "Edit the user configuration."
  (interactive)
  (find-file (concat (file-name-directory user-emacs-directory) "config.org")))

(defun db/reload ()
  "Reload the user configuration. This does not reload the core config."
  (interactive)
  (org-babel-load-file (expand-file-name (concat (file-name-directory user-emacs-directory) "config.org"))))

;; We use `straigh.el' as our package manager.
;; `straight.el' supports a minimum version of Emacs 24.5, and works on macOS,
;; Windows, and most flavors of Linux. You must install `git' in order to
;; use straight.el.

;; Further information:
;; https://github.com/raxod502/straight.el
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; We still want to use `use-package's syntax. By setting `straight-use-package-by-default'
;; to non-nil we allow it to use straight in the background. This is the
;; only time we have to call `straight-use-package'.
(straight-use-package 'use-package)

;; Let's make use of DOOM's core libraries for convenience.
(load (concat (file-name-directory load-file-name) "core/core-lib") nil t)
(load (concat (file-name-directory load-file-name) "core/core-buffers") nil t)
(load (concat (file-name-directory load-file-name) "core/core-ui") nil t)

;; Load user configuration
(when (file-readable-p (concat (file-name-directory load-file-name) "config.org"))
  (org-babel-load-file (expand-file-name (concat (file-name-directory load-file-name) "config.org"))))
;;; init.el ends here
