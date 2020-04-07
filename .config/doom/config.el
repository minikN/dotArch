;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Demis Balbach"
      user-mail-address "demisbalbach@googlemail.com")

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
(setq doom-font (font-spec :family "Cozette" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-molokai)
(setq doom-theme 'doom-monokai-spectrum)

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

;; Set theme colors for ENV
(load! "userconfig/theme-colors")

;; Power management
(load! "userconfig/acpi")

;; Load EXWM and related configs
(load! "userconfig/exwm")
(load! "userconfig/exwm-randr")
(load! "userconfig/exwm-polybar")

;; Configure windows
(load! "userconfig/windows")

;; Configure lsp and lsp-ui
(load! "userconfig/lsp")

;; IRC
;(load! "userconfig/irc")

;; Setup Pinentry for GPG and SSH
(load! "userconfig/pinentry")

;; Setup org mode
(load! "userconfig/org-mode")

;; (defvar *image-directory* "/home/demis/screenshots")

;; (defun screenshot (&optional crop)
;;   "Takes screenshots and opens the folder in Dired"
;;   (interactive "p")
;;   (let* ((image-capture-program (if (= current-prefix-arg 0) "maim -s" "maim"))
;;      (sleep-seconds (if (and current-prefix-arg (not (zerop current-prefix-arg)))
;;             current-prefix-arg 0))
;;      (image-type ".png")
;;      (year (format-time-string "%Y"))
;;      (month (format-time-string "%m"))
;;      (date (format-time-string "%Y-%d-%m"))
;;      (directory (concat *image-directory* "/" year "/" month "/" date))
;;      (filename (concat directory "/" (format-time-string "%Y-%d-%m-%T") image-type))
;;      (command (concat image-capture-program " \"" filename "\"")))
;;     (make-directory directory t)
;;     (sleep-for sleep-seconds)
;;     (shell-command command)
;;     (find-file-other-window directory)
;;     (revert-buffer)))

;; (global-set-key (kbd "<print>") 'screenshot)
