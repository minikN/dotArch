;;; ~/git/dotArch/.config/doom/userconfig/env.el -*- lexical-binding: t; -*-

; Determine which OS we are on.
(if (string-match "Microsoft" (car (split-string (shell-command-to-string "uname -r"))))
    (setq ENV "wsl")
  (setq ENV "linux"))

; Determine which device we are on.
(if (equal (x-display-pixel-height) 1440)
    (setq ENV_WSL "desktop")
  (setq ENV_WSL "surface"))
