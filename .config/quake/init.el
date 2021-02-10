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

;; TEMP
(setq user-emacs-directory (concat (file-name-directory load-file-name) "emacs"))

;; use-package should use straight.el under the hood.
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

;; Install use-package
(straight-use-package 'use-package)

;; Load core configuration
(load (concat (file-name-directory load-file-name) "core")
        nil t)

;; Load user configuration
(when (file-readable-p (concat (file-name-directory load-file-name) "config.org"))
  (org-babel-load-file (expand-file-name (concat (file-name-directory load-file-name) "config.org"))))
;;; init.el ends here
