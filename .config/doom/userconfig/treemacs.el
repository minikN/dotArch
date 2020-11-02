;;; ../../.local/share/git/dotArch/.config/doom/userconfig/treemacs.el -*- lexical-binding: t; -*-

;; treemacs
(after! treemacs
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (setq treemacs-show-hidden-files t
        treemacs-follow-after-init t
        treemacs-silent-filewatch t
        treemacs-silent-refresh t
        treemacs-recenter-after-file-follow 'always))

;; lsp-treemacs
(with-eval-after-load 'lsp-treemacs
  (lsp-treemacs-sync-mode 1)
  (setq lsp-treemacs-symbols-position-params
        `((side . right)
          (slot . 1)
          (window-width . ,treemacs-width))))

;; Toggle lsp-treemacs
(defun db/lsp-treemacs-symbols-toggle ()
  "Toggle the lsp-treemacs-symbols buffer."
  (interactive)
  (if (get-buffer "*LSP Symbols List*")
      (kill-buffer "*LSP Symbols List*")
    (progn (lsp-treemacs-symbols)
           (other-window -1))))

;; bind lsp-treemacs toggle
(map! :leader
      :desc "Toggle Symbols" "c S" #'db/lsp-treemacs-symbols-toggle)
