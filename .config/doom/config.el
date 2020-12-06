;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Determine the environment we are running on.
(load! "userconfig/env")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Demis Balbach"
      user-mail-address "db@minikn.xyz")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(if (equal ENV "linux")
    ;(setq doom-font (font-spec :family "Cozette" :size 12))
    (setq doom-font (font-spec :family "Cozette" :size 16))
  (setq doom-font (font-spec :family "Cascadia Mono" :size 14)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-monokai-spectrum)
;(setq doom-themes-treemacs-theme "doom-colors")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k')
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Userconfig starts here

;; make ivy/switch-workspace-buffer list all buffers
(after! persp-mode
  (remove-hook 'persp-add-buffer-on-after-change-major-mode-filter-functions #'doom-unreal-buffer-p))

;; Setup icons, theme and colors
(load! "userconfig/icons")
(load! "userconfig/theme")
(load! "userconfig/theme-colors")

;; Linux specific config
(if (equal ENV "linux")
    (progn
      (load! "userconfig/acpi")
      (load! "userconfig/exwm")
      (load! "userconfig/edwina")
      (load! "userconfig/exwm-randr")
      (load! "userconfig/exwm-polybar")
      (load! "userconfig/screenshot")))

(load! "userconfig/projectile")
(load! "userconfig/lsp")
(load! "userconfig/treemacs")
(load! "userconfig/flycheck")
(load! "userconfig/irc")
(load! "userconfig/pinentry")
(load! "userconfig/org-mode")
(load! "userconfig/mail")
(load! "userconfig/shell")
(load! "userconfig/browser")
(load! "userconfig/magit")
(load! "userconfig/pass")
;; ivy-posframe
;(load! "userconfig/ivy-posframe")
