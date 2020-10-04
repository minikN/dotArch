;;; ~/git/dotArch/.config/doom/userconfig/env.el -*- lexical-binding: t; -*-

; Determine which OS we are on.
(if (string-match "Microsoft" (car (split-string (shell-command-to-string "uname -r"))))
    (setq ENV "wsl")
  (setq ENV "linux"))
