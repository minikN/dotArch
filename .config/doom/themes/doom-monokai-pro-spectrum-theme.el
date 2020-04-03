;; doom-monokai-pro-spectrum-theme.el --- inspired by Textmate's Monokai -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-monokai-pro-spectrum-theme nil
  "Options for doom-molokai."
  :group 'doom-themes)

(defcustom doom-monokai-pro-spectrum-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-monokai-pro-spectrum-theme
  :type 'boolean)

(defcustom doom-monokai-pro-spectrum-comment-bg doom-monokai-pro-spectrum-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-monokai-pro-spectrum-theme
  :type 'boolean)

(defcustom doom-monokai-pro-spectrum-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-monokai-pro-spectrum-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-monokai-pro-spectrum
  "A dark, vibrant theme inspired by Textmate's Monokai."

  ;; name        gui       256       16
  ((bg         '("#222222" nil       nil            ))
   (bg-alt     '("#191919" nil       nil            ))
   (base0      '("#131313" "#121212" "black"        ))
   (base1      '("#191919" "#1c1c1c" "black"        ))
   (base2      '("#2d2c2d" "#262626" "brightblack"  ))
   (base3      '("#363537" "#3a3a3a" "brightblack"  ))
   (base4      '("#525053" "#585858" "brightblack"  ))
   (base5      '("#585858" "#585858" "brightblack"  ))
   (base6      '("#69676c" "#6c6c6c" "brightblack"  ))
   (base7      '("#8b888f" "#8a8a8a" "brightblack"  ))
   (base8      '("#bab6c0" "#bcbcbc" "white"        ))
   (fg         '("#f7f1ff" "#ffffff" "brightwhite"  ))
   (fg-alt     '("#c6c6c6" "#c6c6c6" "white"        ))

   (grey       base4)
   (red        '("#fc618d" "#ff69bf" "red"          ))
   (orange     '("#fd9353" "#ff7f50" "brightred"    ))
   (green      '("#7bd88f" "#90ee90" "green"        ))
   (yellow     '("#fce566" "#f0e68c" "yellow"       ))
   (violet     '("#948ae3" "#9370db" "magenta"      ))
   (cyan       '("#5ad4e6" "#40e0d0" "brightcyan"   ))
   (magenta     cyan)
   (blue        cyan)
   (dark-blue   cyan)
   (teal        cyan)
   (dark-cyan   cyan)

   ;; face categories
   (highlight      yellow)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      base2)
   (builtin        violet)
   (comments       (if doom-monokai-pro-spectrum-brighter-comments violet base5))
   (doc-comments   (if doom-monokai-pro-spectrum-brighter-comments (doom-lighten violet 0.1) (doom-lighten base5 0.25)))
   (constants      violet)
   (functions      green)
   (keywords       magenta)
   (methods        green)
   (operators      red)
   (type           cyan)
   (strings        yellow)
   (variables      fg)
   (numbers        violet)
   (region         base2)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-monokai-pro-spectrum-padded-modeline
      (if (integerp doom-monokai-pro-spectrum-padded-modeline) doom-monokai-pro-spectrum-padded-modeline 4)))

   (modeline-fg nil)
   (modeline-fg-alt base4)

   (modeline-bg base1)
   (modeline-bg-inactive (doom-darken base2 0.2))

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))


  ;; --- extra faces ------------------------
  ((cursor                                       :background fg)
   (default                                      :foreground fg :background bg)

   ;;; I-search
   (match                                        :foreground fg :background base4)
   (isearch                                      :inherit 'match :box `(:line-width 2 :color ,yellow))
   (lazy-highlight                               :inherit 'match)
   (isearch-fail                                 :foreground red)

   ;;; mode-line
   (mode-line                                    :background modeline-bg :foreground modeline-fg
                                                 :box (if -modeline-pad `(:line-width ,-modeline-pad :color modeline-bg)))
   (mode-line-inactive                           :background modeline-bg-inactive :foreground modeline-fg-alt
                                                 :box (if -modeline-pad `(:line-width ,-modeline-pad :color modeline-bg-inactive)))

   ;; Centaur tabs
   (centaur-tabs-selected-modified               :inherit 'centaur-tabs-selected :foreground yellow)
   (centaur-tabs-unselected-modified             :inherit 'centaur-tabs-unselected :foreground yellow)
   (centaur-tabs-active-bar-face                 :background yellow)
   (centaur-tabs-modified-marker-selected        :inherit 'centaur-tabs-selected :foreground fg)
   (centaur-tabs-modified-marker-unselected      :inherit 'centaur-tabs-unselected :foreground fg)

   ;; Doom modeline
   (doom-modeline-bar                            :background yellow)
   (doom-modeline-buffer-file                    :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path                    :inherit 'bold :foreground green)
   (doom-modeline-buffer-project-root            :foreground green :weight 'bold)
   (doom-modeline-buffer-modified                :inherit 'bold :foreground orange)

   ((line-number &override)                      :foreground base5 :distant-foreground nil)
   ((line-number-current-line &override)         :foreground base7 :distant-foreground nil)

   ;; ediff
   (ediff-fine-diff-A                            :background (doom-blend magenta bg 0.3) :weight 'bold)

   ;; evil-mode
   (evil-search-highlight-persist-highlight-face :background violet)

   ;; evil-snipe
   (evil-snipe-first-match-face                  :foreground base0 :background green)
   (evil-snipe-matches-face                      :foreground green :underline t)

   ;; flycheck
   (flycheck-error                               :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning                             :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info                                :underline `(:style wave :color ,green)  :background base3)

   ;;; git-gutter
   ;(git-gutter:added                             :inherit vc-added)
   ;(git-gutter:deleted                           :inherit vc-deleted)
   ;(git-gutter:modfied                           :inherit vc-modified)
   ;(git-gutter:separator                         :foreground cyan)
   ;(git-gutter:unchanged                         :foreground yellow)
   ;(git-gutter-fr:added                          :inherit git-gutter:added)
   ;(git-gutter-fr:deleted                        :inherit git-gutter:deleted)
   ;(git-gutter-fr:modfied                        :interit git-gutter:modified)
   
   ;; helm
   (helm-swoop-target-line-face                  :foreground magenta :inverse-video t)

   ;; ivy
   (ivy-current-match                            :background base3)
   (ivy-minibuffer-match-face-1                  :background base1 :foreground base4)

   ;; neotree
   (neo-dir-link-face                            :foreground cyan)
   (neo-expand-btn-face                          :foreground magenta)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face              :foreground magenta)
   (rainbow-delimiters-depth-2-face              :foreground orange)
   (rainbow-delimiters-depth-3-face              :foreground green)
   (rainbow-delimiters-depth-4-face              :foreground cyan)
   (rainbow-delimiters-depth-5-face              :foreground magenta)
   (rainbow-delimiters-depth-6-face              :foreground orange)
   (rainbow-delimiters-depth-7-face              :foreground green)


   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property                     :foreground keywords)

   ;; markdown-mode
   (markdown-blockquote-face                     :inherit 'italic :foreground dark-blue)
   (markdown-list-face                           :foreground magenta)
   (markdown-pre-face                            :foreground cyan)
   (markdown-link-face                           :inherit 'bold :foreground blue)
   ((markdown-code-face &override)               :background (doom-lighten base2 0.045))

   ;; org-mode
   ((outline-1 &override)                        :foreground magenta)
   ((outline-2 &override)                        :foreground orange)
   (org-ellipsis                                 :foreground orange)
   (org-tag                                      :foreground yellow :bold nil)
   ((org-quote &override)                        :inherit 'italic :foreground base7 :background org-quote)
   (org-todo                                     :foreground yellow :bold 'inherit)
   (org-list-dt                                  :foreground yellow))


  ;; --- extra variables --------------------
  ;; ()
  )

;;; doom-monokai-pro-spectrum-theme.el ends here
