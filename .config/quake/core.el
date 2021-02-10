;;; core.el --- Core Configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  Core configuration that makes up the look and feel of Emacs.
;;
;;; Code:

(defun db/edit ()
  "Edit the configuration."
  (interactive)
  (find-file (concat (file-name-directory user-emacs-directory) "config.org")))

(defun db/reload ()
  "Reload the configuration."
  (interactive)
  (org-babel-load-file (expand-file-name (concat (file-name-directory user-emacs-directory) "config.org"))))

;;; Let's clean up Emacs' user interface. It's pretty cluttered
;;; by default. Let's make it more minimal and give some
;;; breathing room.
(setq inhibit-startup-message t)                ;; Disable the welcome screen.
(scroll-bar-mode -1)                            ;; Disable the visibility of scroll bars.
(tool-bar-mode -1)                              ;; We don't need a toolbar, do we?
(tooltip-mode -1)                               ;; Line above, but /bar/tip/.
(fringe-mode '(15 . 0))                         ;; We want a bit of fringe on the left,
                                                ;; but not the right.
(setq visible-bell t)                           ;; Let's test this.
;;; core.el ends here
