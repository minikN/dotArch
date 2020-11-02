;;; ../../.local/share/git/dotArch/.config/doom/userconfig/treemacs.el -*- lexical-binding: t; -*-

(after! treemacs
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (setq treemacs-show-hidden-files t
        treemacs-follow-after-init t
        treemacs-silent-filewatch t
        treemacs-silent-refresh t
        treemacs-recenter-after-file-follow 'always
        treemacs-file-name-transformer (lambda (it) (concat "" it))
        treemacs-directory-name-transformer (lambda (it) (concat "" it))))

;; (after! lsp-treemacs
;;   (lsp-treemacs-sync-mode 1))