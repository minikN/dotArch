;;; ~/git/dotArch/.config/doom/userconfig/env.el -*- lexical-binding: t; -*-

; Determine which OS we are on.
(if (string-match "-[Mm]icrosoft" operating-system-release)
    (setq ENV "wsl")
  (setq ENV "linux"))
