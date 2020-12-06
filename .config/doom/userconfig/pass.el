;;; ../../.local/share/git/dotArch/.config/doom/userconfig/pass.el -*- lexical-binding: t; -*-

(defun db/pass-push ()
  "Push passwords to git"
  (interactive)
  (message "push"))

(defun db/pass-pull ()
  "Pull passwords to git"
  (interactive)
  (message "pull"))

(after! pass
  (define-key! pass-mode-map
    "p" #'db/pass-push
    "P" #'db/pass-pull))
