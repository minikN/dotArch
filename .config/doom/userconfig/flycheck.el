;;; ../../.local/share/git/dotArch/.config/doom/userconfig/flycheck/flycheck.el -*- lexical-binding: t; -*-

(after! flycheck
  (custom-set-faces!
    `(flycheck-error :underline nil :box (:line-width 1 :color ,THEME_RED :style nil))
    `(flycheck-warning :underline nil :box (:line-width 1 :color ,THEME_YELLOW :style nil))
    `(flycheck-info :underline nil :box (:line-width 1 :color ,THEME_GREEN :style nil))))
