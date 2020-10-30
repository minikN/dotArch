;;; ../../.local/share/git/dotArch/.config/doom/userconfig/theme.el -*- lexical-binding: t; -*-

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)

(use-package! doom-themes
  :defer t
  :init
  (setq doom-theme 'doom-monokai-spectrum
        doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme "doom-colors-extended"
        lsp-treemacs-theme "doom-colors-extended")
  ;; improve integration w/ org-mode
  (add-hook 'doom-load-theme-hook #'doom-themes-org-config)
  ;; more Atom-esque file icons for neotree/treemacs
  (when (featurep! :ui neotree)
    (add-hook 'doom-load-theme-hook #'doom-themes-neotree-config)
    (setq doom-themes-neotree-enable-variable-pitch t
          doom-themes-neotree-file-icons 'simple
          doom-themes-neotree-line-spacing 2))
  (when (featurep! :ui treemacs)
    (add-hook 'doom-load-theme-hook #'doom-themes-treemacs-config)))

(use-package! solaire-mode
  :when (or (daemonp) (display-graphic-p))
  :hook (doom-load-theme . solaire-global-mode)
  :config
  (when (daemonp)
    (add-hook! '(doom-switch-frame-hook after-make-frame-functions)
      (defun +doom-disable-solaire-mode-maybe-h (&optional frame)
        (if (display-graphic-p frame)
            (unless solaire-global-mode
              (solaire-global-mode +1))
          (when solaire-global-mode
            (solaire-global-mode -1)))))))
