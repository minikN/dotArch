;;; ~/.doom.d/userconfig/theme-colors.el -*- lexical-binding: t; -*-

(defun db/extract-theme-colors (&optional theme)
  "Extract colors from the current theme."
  (interactive)
  (setq THEME_BG (face-background 'default))
  (setq THEME_FG (face-foreground 'default))
  (setq THEME_CONTRAST (face-background 'tooltip))
  (setq THEME_YELLOW (face-foreground 'font-lock-string-face))
  (setenv "THEME_BG" THEME_BG)
  (setenv "THEME_FG" THEME_FG)
  (setenv "THEME_CONTRAST" THEME_CONTRAST)
  (setenv "THEME_YELLOW" THEME_YELLOW))

(advice-add 'enable-theme :after #'db/extract-theme-colors)
